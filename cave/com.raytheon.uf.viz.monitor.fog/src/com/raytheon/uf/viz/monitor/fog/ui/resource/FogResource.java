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
package com.raytheon.uf.viz.monitor.fog.ui.resource;

import java.nio.FloatBuffer;
import java.util.Calendar;
import java.util.Date;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.monitor.fog.FogMonitor;
import com.raytheon.uf.viz.monitor.fog.listeners.IFogResourceListener;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay.GriddedImagePaintProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * FogResource
 * 
 * Implements Grid Image display for Fog data
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    11Nov2009    2037        dhladky    Initial Creation.
 *    16Jun2012    14386       zhao       Fixed a bug causing auto update to fail; 
 *                                        also modified to keep only latest Fog record for each frame.
 *    29Jul2014    3465        mapeters   Updated deprecated drawString() calls.
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FogResource extends
        AbstractVizResource<FogResourceData, MapDescriptor> implements
        IResourceDataChanged, IFogResourceListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FogResource.class);

    public String icao;

    public String fieldName;

    public String fieldUnitString;

    public FogRecord record;

    public DataTime displayedDataTime;

    public Date previousDataTime;

    private ColorMap colorMap = null;

    private String colormapfile = null;

    private static String[] fogLabels = new String[] { "LOW", "MODERATE",
            "HIGH" };

    private static float[] fogValues = new float[] { 0, 15.0f, 60.0f, 110.0f,
            220.0f };

    private static float[] fogLabelValues = new float[] { 60.0f, 110.0f, 220.0f };

    private GriddedImageDisplay gridDisplay = null;
    
    private Date refHour;

    /* The font used */
    public IFont font = null;

    private FogMonitor monitor = null;

    private boolean needsUpdate = false;

    public FogResource(FogResourceData data, LoadProperties props) {
        super(data, props);

        data.addChangeListener(this);
        monitor = getResourceData().getFogMonitor();
        monitor.addFogResourceListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        // FogRecord rec = resourceData.dataObjectMap.get(refHour);

        if (record == null) {
            return "No Fog Data Available";
        }
        return "Fog Threat Level "
                + record.getDataTime().getLegendString();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject pdo : pdos) {
                if (pdo instanceof FogRecord) {
                    addRecord((FogRecord) pdo);
                }
            }
        }
        issueRefresh();
    }

    /**
     * @param pdo
     */
    private void addRecord(FogRecord pdo) {
        Calendar rh = pdo.getRefHour();
        if (!resourceData.dataObjectMap.containsKey(rh)) {
            try {
                record = resourceData.populateRecord(pdo);
                resourceData.dataObjectMap.put(record.getRefHour().getTime(), record);
                resourceData.gridImageMap.put(record.getRefHour().getTime(), null);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            }
        } else if ( resourceData.dataObjectMap.containsKey(rh) ) {
        	try {
        		record = resourceData.populateRecord(pdo);
        		resourceData.dataObjectMap.remove(record.getRefHour().getTime());
                resourceData.dataObjectMap.put(record.getRefHour().getTime(), record);
                resourceData.gridImageMap.remove(record.getRefHour().getTime());
                resourceData.gridImageMap.put(record.getRefHour().getTime(), null);
        	} catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            }
        }
    }

    @Override
    protected void disposeInternal() {

        if (resourceData.gridImageMap != null) {
            for (Date time : resourceData.gridImageMap.keySet()) {
                if (resourceData.gridImageMap.get(time) != null) {
                    resourceData.gridImageMap.get(time).dispose();
                }
            }
        }
        resourceData.getFogMonitor().removeFogResourceListener(this);
        resourceData.getFogMonitor().closeDialog();

        if (font != null) {
            font.dispose();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (this.font == null) {
            this.font = target.initializeFont("Dialog", 11, null);
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        this.displayedDataTime = paintProps.getDataTime();
        FramesInfo info = paintProps.getFramesInfo();
        int currentFrame = info.getFrameIndex();
        if (info.getFrameTimes() != null) {
            refHour = info.getFrameTimes()[currentFrame].getRefTime();
        } else {
            return;
        }
        this.record = resourceData.dataObjectMap.get(refHour);
        if (record != null) {
            if(record.getThreats()== null) needsUpdate=true;
            if (resourceData.gridImageMap.get(refHour) != null && needsUpdate==false) {
                gridDisplay = resourceData.gridImageMap.get(refHour);
            } else {
                if (needsUpdate) {
                    // new image
                    record = resourceData.getFogThreat().getFogThreat(record);
                }
                gridDisplay = new GriddedImageDisplay(FloatBuffer.wrap(record
                        .getThreats()), descriptor, record.getGridGeometry());
                resourceData.gridImageMap.put(refHour, gridDisplay);
                needsUpdate=false;
            }
        }
        if (!refHour.equals(previousDataTime)) {
            this.previousDataTime = refHour;
            updateDialogTime(refHour);
        }
        if (record == null) {
            return;
        }

        ImagingCapability imagingCap = getCapability(ImagingCapability.class);
        GriddedImagePaintProperties giProps = new GriddedImagePaintProperties(
                paintProps, imagingCap.getBrightness(),
                imagingCap.getContrast(), imagingCap.isInterpolationState());

        ColorMapParameters colorMapParameters = getCapability(
                ColorMapCapability.class).getColorMapParameters();

        this.colormapfile = colorMapParameters.getColorMapName();
        colorMapParameters.setColorMap(getColorMap());
        colorMapParameters.setColorMapMax(255);
        colorMapParameters.setColorMapMin(0);
        colorMapParameters.setDataMax(255.0f);
        colorMapParameters.setDataMin(0.0f);
        colorMapParameters.setColorBarIntervals(fogValues);
        // colorMapParameters.manualAddLabels(fogLabelValues, fogLabels);

        gridDisplay.setColorMapParameters(colorMapParameters);
        gridDisplay.paint(target, giProps);

        paintProductString(target, paintProps);

    }

    /**
     * Gets the color map
     * 
     * @return
     * @throws VizException
     */
    private ColorMap getColorMap() throws VizException {

        if (colorMap == null) {
            if (colormapfile != null) {
                IColorMap cxml = ColorMapLoader.loadColorMap(colormapfile);
                colorMap = new ColorMap(colormapfile, (ColorMap) cxml);
            }
        }
        return colorMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IInspectableResource#inspect(com
     * .vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate latLon) throws VizException {
        String inspect = null;
        if (record != null && record.getVisArray() != null
                && record.getIR_3_9Array() != null
                && record.getIR_10_7Array() != null) {
            Coordinate coor = null;
            try {
                coor = latLon.asGridCell(record.getGridGeometry(),
                        PixelInCell.CELL_CENTER);
            } catch (FactoryException e) {
                e.printStackTrace();
            } catch (org.opengis.referencing.operation.TransformException e) {
                e.printStackTrace();
            }
            int select = (int) ((record.getNx() * (int) coor.y) + (int) coor.x);

            if (select < record.getNx() * record.getNy() && select > 0
                    && coor.x > 0 && coor.x < record.getNx()) {
                inspect = "VIS value: "
                        + record.getVisArray()[(int) (record.getNx() * (int) coor.y)
                                + (int) coor.x]
                        + "\n"
                        + " IR 3.9 value: "
                        + record.getIR_3_9Array()[(int) (record.getNx() * (int) coor.y)
                                + (int) coor.x]
                        + "\n"
                        + " IR 10.7 value: "
                        + record.getIR_10_7Array()[(int) (record.getNx() * (int) coor.y)
                                + (int) coor.x] + "\n";
            } else {
                inspect = "NO DATA";
            }

        }

        return inspect;
    }

    /**
     * Draws the field text string
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void paintProductString(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        double[] pixel1 = paintProps.getView().getDisplayCoords(
                new double[] { 80, 35 }, target);
        double[] pixel2 = paintProps.getView().getDisplayCoords(
                new double[] { 200, 35 }, target);
        double[] pixel3 = paintProps.getView().getDisplayCoords(
                new double[] { 400, 35 }, target);

        target.clearClippingPlane();

        RGB color = getCapability(ColorableCapability.class).getColor();
        DrawableString[] strings = new DrawableString[3];
        strings[0] = new DrawableString("LOW", color);
        strings[0].font = font;
        strings[0].setCoordinates(pixel1[0], pixel1[1]);
        strings[0].addTextStyle(TextStyle.BLANKED);
        strings[0].verticallAlignment = VerticalAlignment.MIDDLE;

        strings[1] = new DrawableString("MODERATE", color);
        strings[1].font = font;
        strings[1].setCoordinates(pixel2[0], pixel2[1]);
        strings[1].addTextStyle(TextStyle.BLANKED);
        strings[1].verticallAlignment = VerticalAlignment.MIDDLE;

        strings[2] = new DrawableString("HIGH", color);
        strings[2].font = font;
        strings[2].setCoordinates(pixel3[0], pixel3[1]);
        strings[2].addTextStyle(TextStyle.BLANKED);
        strings[2].verticallAlignment = VerticalAlignment.MIDDLE;

        target.drawStrings(strings);
    }

    @Override
    public void algorithmUpdate() {
        // clears the grid image map forcing
        // a re-make of it with new algorithm settings
        resourceData.gridImageMap.clear();
        resourceData.fogThreat = null;
        issueRefresh();
    }

    @Override
    public void updateDialogTime(Date time) {
        resourceData.getFogMonitor().updateDialogTime(time);
    }

    /** grab monitor instance **/
    public FogMonitor getFogMonitor() {
        return FogMonitor.getInstance();
    }

    @Override
    public void remove(DataTime dataTime) {
        if (resourceData.gridImageMap != null) {
            GriddedImageDisplay display = resourceData.gridImageMap
                    .remove(dataTime);
            if (display != null) {
                display.dispose();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.fog.listeners.IFogResourceListener#closeDialog
     * ()
     */
    @Override
    public void closeDialog() {
        monitor.closeDialog();

    }
    
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
            if (record != null) {
                record.setGridGeometry2D(null);
                resourceData.resetGridImgMap();
                needsUpdate = false;
                issueRefresh();
            }
    }

}
