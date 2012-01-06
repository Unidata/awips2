/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.cloudheight;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.satellite.units.ir.IRPixel;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.cloudheight.data.CloudHeightData;
import com.raytheon.uf.viz.cloudheight.data.SoundingSource;
import com.raytheon.uf.viz.cloudheight.data.SoundingSource.SourceType;
import com.raytheon.uf.viz.cloudheight.impl.CloudHeightCalculatorPorted;
import com.raytheon.uf.viz.cloudheight.impl.CloudHeightCalculatorPorted.CloudHeightResult;
import com.raytheon.uf.viz.cloudheight.impl.ModelCloudHeightSourceImplementation;
import com.raytheon.uf.viz.cloudheight.impl.RaobCloudHeightSourceImplementation;
import com.raytheon.uf.viz.cloudheight.rsc.PopupSkewTResource;
import com.raytheon.uf.viz.cloudheight.ui.CloudHeightRightClickAction;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.d2d.core.sampling.CloudHeightResourceData.ICloudHeightAlgorithm;
import com.raytheon.viz.core.map.GeoUtil;
import com.raytheon.viz.skewt.ui.PopupSkewTDialog;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A1 ported cloud height algorithm
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CloudHeightAlgorithm implements ICloudHeightAlgorithm {

    // TODO: Common place for string
    public static final String RAW_VALUE = "rawValue";

    public static final VerticalSounding LOADING = new VerticalSounding();

    private static final CloudHeightData algorithmData = CloudHeightData
            .getCloudHeightData();

    private static final float MISSING = -999999.0f;

    // TODO: Look into purging data (forwarding remove(DataTime))?
    public static interface ICloudHeightSourceImplementation {
        public DataTime[] getDataTimes();

        public VerticalSounding createSounding(Coordinate latLon,
                DataTime rscTime);
    }

    /** Sounding source implementation map */
    private Map<SoundingSource, ICloudHeightSourceImplementation> implMap = new HashMap<SoundingSource, CloudHeightAlgorithm.ICloudHeightSourceImplementation>();

    /** Currently selected source */
    private SoundingSource currentSource;

    /** Whether skewT is selected or not */
    private boolean skewT = false;

    /** Pop up skewT dialog */
    private PopupSkewTDialog dialog;

    /**
     * If we have sampled and they selected skewT in menu, we should open the
     * dialog (A1 behavior)
     */
    private boolean hasSampled = false;

    /**
     * 
     */
    public CloudHeightAlgorithm() {
        // Create an implementation for each sounding source
        for (SoundingSource source : algorithmData.getSources()) {
            ICloudHeightSourceImplementation impl = null;
            if (source.getType() == SourceType.MODEL) {
                impl = new ModelCloudHeightSourceImplementation(source);
            } else if (source.getType() == SourceType.RAOB) {
                impl = new RaobCloudHeightSourceImplementation(source);
            }
            implMap.put(source, impl);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.d2d.core.sampling.CloudHeightResourceData.
     * ICloudHeightAlgorithm
     * #addContextMenuItems(com.raytheon.uf.viz.core.drawables.IDescriptor,
     * org.eclipse.jface.action.IMenuManager, int, int)
     */
    @Override
    public void addContextMenuItems(IDescriptor descriptor,
            IMenuManager manager, int x, int y) {
        manager.add(new CloudHeightRightClickAction(descriptor, this));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.d2d.core.sampling.CloudHeightResourceData.
     * ICloudHeightAlgorithm
     * #inspect(com.raytheon.uf.viz.core.drawables.IDescriptor,
     * com.raytheon.uf.common.time.DataTime,
     * com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public String inspect(AbstractVizResource<?, ?> reqRsc, Coordinate latLon) {
        IDescriptor descriptor = reqRsc.getDescriptor();
        ICloudHeightSourceImplementation impl = getCurrentImplementation();
        if (impl == null) {
            // No implementation for the source
            return null;
        }

        hasSampled = true;
        if (skewT) {
            showDialog();
        }
        FramesInfo currInfo = descriptor.getFramesInfo();

        // Get all valid contributors to cloud height that provide temp
        List<AbstractVizResource<?, ?>> resources = getValidContributors(
                new ArrayList<AbstractVizResource<?, ?>>(),
                descriptor.getResourceList());
        float[] temps = null;
        AbstractVizResource<?, ?> usedRsc = null;
        for (AbstractVizResource<?, ?> rsc : resources) {
            DataTime timeForRsc = currInfo.getTimeForResource(rsc);
            if (timeForRsc != null) {
                try {
                    Map<String, Object> interMap = rsc
                            .interrogate(new ReferencedCoordinate(latLon));
                    if (interMap != null
                            && interMap.get(RAW_VALUE) instanceof Double
                            && interMap.containsKey(ISpatialObject.class
                                    .toString())) {
                        temps = extractTemps(latLon, rsc, interMap, rsc
                                .getCapability(ColorMapCapability.class)
                                .getColorMapParameters());
                        boolean good = true;
                        for (int i = 0; i < temps.length; ++i) {
                            if (temps[i] == MISSING) {
                                good = false;
                            }
                        }
                        if (good) {
                            usedRsc = rsc;
                            break;
                        }
                    }
                } catch (VizException e) {
                    UFStatus.getHandler().handle(Priority.PROBLEM,
                            "Error interrogating resource", e);
                }
            }
        }

        if (temps != null && temps[0] != MISSING) {
            float cloudTemp = temps[0];
            float coldestCloudTemp = temps[1];
            float predCloudTemp = temps[2];
            float warmestCloudTemp = temps[3];

            Float otherTempToUse = null;
            switch (algorithmData.getDisplayOption()) {
            case LOW: {
                otherTempToUse = warmestCloudTemp;
                break;
            }
            case PEAK: {
                otherTempToUse = coldestCloudTemp;
                break;
            }
            case PREDOMINANT: {
                otherTempToUse = predCloudTemp;
                break;
            }
            }

            String status = "";
            float height = -1, otherHeight = -1;
            VerticalSounding sounding = impl.createSounding(latLon,
                    descriptor.getTimeForResource(usedRsc));
            if (sounding == null) {
                if (currentSource.getType() == SourceType.RAOB) {
                    return "NO RAOB DATA";
                } else if (currentSource.getType() == SourceType.MODEL) {
                    sounding = new VerticalSounding();
                    sounding.setStationId("CLIMO");
                    sounding.setName(GeoUtil.formatCoordinate(latLon));

                    int day = Calendar.getInstance().get(Calendar.DAY_OF_YEAR);

                    // This will populate the sounding
                    height = CloudHeightCalculatorPorted
                            .getCloudHeightClimo(cloudTemp + 273.0f,
                                    (float) latLon.y, day, sounding);

                    if (otherTempToUse != null) {
                        // Here we just want the height
                        otherHeight = CloudHeightCalculatorPorted
                                .getCloudHeightClimo(otherTempToUse + 273.0f,
                                        (float) latLon.y, day, null);
                    }
                }
            } else if (sounding != LOADING) {
                float[] muParcelTrajectory = PopupSkewTDialog
                        .derivemuParcelTrajectory(sounding);

                CloudHeightResult result = CloudHeightCalculatorPorted
                        .getCloudHeightGrid((float) (cloudTemp + 273.0),
                                sounding, currentSource, muParcelTrajectory);
                status = result.status;
                height = result.value;

                if (otherTempToUse != null) {
                    result = CloudHeightCalculatorPorted.getCloudHeightGrid(
                            otherTempToUse + 273.0f, sounding, currentSource,
                            muParcelTrajectory);
                    otherHeight = result.value;
                }
            } else {
                reqRsc.issueRefresh();
                return null;
            }

            if (height < 0 || Float.isNaN(height)) {
                if (currentSource.getType() == SourceType.RAOB) {
                    return "NO RAOB DATA";
                } else if (currentSource.getType() == SourceType.MODEL) {
                    return "NO CLIMO DATA";
                }
            }

            int heightInHundredsFt = ((int) (height * 3.28 / 100 + .5) * 100);
            String rval = "" + heightInHundredsFt;
            if (otherHeight >= 0) {
                int otherHeightInHundredsFt = ((int) (otherHeight * 3.28 / 100 + .5) * 100);
                rval += "/" + otherHeightInHundredsFt;
            }

            if (dialog != null) {
                setSounding(sounding);
                plotHeight(height, cloudTemp);
            }

            return rval
                    + String.format(" feet (%s %s)", sounding.getStationId(),
                            status);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.d2d.core.sampling.CloudHeightResourceData.
     * ICloudHeightAlgorithm
     * #isEnabled(com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public boolean isEnabled(IDescriptor descriptor) {
        return getValidContributors(new ArrayList<AbstractVizResource<?, ?>>(),
                descriptor.getResourceList()).size() > 0
                || descriptor.getResourceList()
                        .getResourcesByType(PopupSkewTResource.class).size() > 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.d2d.core.sampling.CloudHeightResourceData.
     * ICloudHeightAlgorithm#getDataTimes()
     */
    @Override
    public DataTime[] getDataTimes() {
        ICloudHeightSourceImplementation impl = implMap.get(currentSource);
        if (impl != null) {
            return impl.getDataTimes();
        }
        return new DataTime[0];
    }

    /**
     * Returns the first valid ICloudHeightContributor on the descriptor
     * 
     * @param descriptor
     * @return
     */
    private List<AbstractVizResource<?, ?>> getValidContributors(
            List<AbstractVizResource<?, ?>> resources, ResourceList list) {
        for (ResourcePair rp : list) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc != null && rp.getProperties().isVisible()
                    && rsc.hasCapability(ColorMapCapability.class)) {
                ColorMapParameters params = rsc.getCapability(
                        ColorMapCapability.class).getColorMapParameters();
                if (params != null
                        && SI.CELSIUS.equals(params.getDisplayUnit())
                        && params.getDataUnit() instanceof IRPixel) {
                    resources.add(rsc);
                }
            }
        }

        List<AbstractVizResource<?, ?>> groups = list
                .getResourcesByType(IResourceGroup.class);
        for (AbstractVizResource<?, ?> group : groups) {
            if (group.getProperties().isVisible()) {
                getValidContributors(resources,
                        ((IResourceGroup) group).getResourceList());
            }
        }
        return resources;
    }

    public ICloudHeightSourceImplementation getCurrentImplementation() {
        return implMap.get(currentSource);
    }

    public SoundingSource getCurrentSource() {
        return currentSource;
    }

    public void setCurrentSource(SoundingSource currentSource) {
        this.currentSource = currentSource;
    }

    public boolean isSkewT() {
        return skewT;
    }

    public void setSkewT(boolean skewT) {
        this.skewT = skewT;
        if (skewT) {
            showDialog();
        } else {
            hideDialog();
        }
    }

    public synchronized void hideDialog() {
        if (dialog != null) {
            dialog.close();
            dialog = null;
        }
    }

    public synchronized void showDialog() {
        if (dialog == null) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            dialog = new PopupSkewTDialog(shell);
            dialog.addListener(SWT.Close, new Listener() {
                @Override
                public void handleEvent(Event event) {
                    skewT = false;
                }
            });
        }

        if (dialog.isOpen() == false && hasSampled) {
            dialog.open();
        }
    }

    public void setSounding(VerticalSounding sounding) {
        if (dialog != null) {
            dialog.setSounding(sounding);
        }
    }

    public void plotHeight(float height, float temp) {
        if (dialog != null) {
            dialog.plotHeight(height, temp);
        }
    }

    private float[] extractTemps(Coordinate latLon,
            AbstractVizResource<?, ?> rsc, Map<String, Object> interMap,
            ColorMapParameters params) {
        // Method loosely ported from SatPVImageDepict.C::interogate

        ISpatialObject so = (ISpatialObject) interMap.get(ISpatialObject.class
                .toString());

        UnitConverter dataToDisplay = params.getDataToDisplayConverter();
        UnitConverter displayToData = params.getDisplayToDataConverter();

        float[] temps = new float[] { MISSING, MISSING, MISSING, MISSING };

        Coordinate c = MapUtil.latLonToGridCoordinate(latLon,
                PixelOrientation.CENTER, so);

        int x = (int) Math.round(c.x);
        int y = (int) Math.round(c.y);
        int nx = so.getNx();
        int ny = so.getNy();
        if (x < 0 || x >= nx || y < 0 || y >= ny) {
            return temps;
        }

        Double displayVal = (Double) interMap.get(RAW_VALUE);
        byte tmp = (byte) displayToData.convert(displayVal);
        if (tmp == 0) {
            return temps;
        }

        temps[0] = displayVal.floatValue();

        byte[] elements = new byte[6400];
        int numElements = 0;
        int i, j, ii, jj;
        int yStart = -(int) (algorithmData.getNy() / 2);
        int yEnd = (int) algorithmData.getNy() / 2;

        int xStart = -(int) (algorithmData.getNx() / 2);
        int xEnd = (int) algorithmData.getNx() / 2;

        for (j = yStart; j < yEnd; j++) {
            jj = y + j;
            if (jj < 0 || jj >= ny) {
                continue;
            }
            for (i = xStart; i < xEnd; i++) {
                ii = x + i;
                if (ii >= 0 && ii < nx) {
                    latLon = MapUtil.gridCoordinateToLatLon(new Coordinate(ii,
                            jj), PixelOrientation.CENTER, so);
                    try {
                        interMap = rsc.interrogate(new ReferencedCoordinate(
                                latLon));
                        if (interMap.get(RAW_VALUE) instanceof Double) {
                            elements[numElements++] = (byte) displayToData
                                    .convert((Double) interMap.get(RAW_VALUE));
                        }
                    } catch (VizException e) {
                        // ignore
                    }
                }
            }
        }

        int[] calculated = new int[3];
        CloudHeightCalculatorPorted.findHighPredLowBrightness(elements,
                numElements, calculated);
        temps[1] = (float) dataToDisplay.convert(calculated[0]);
        temps[2] = (float) dataToDisplay.convert(calculated[1]);
        temps[3] = (float) dataToDisplay.convert(calculated[2]);

        return temps;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.d2d.core.sampling.CloudHeightResourceData.
     * ICloudHeightAlgorithm#dispose()
     */
    @Override
    public void dispose() {
        hideDialog();
    }
}
