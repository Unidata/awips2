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
package com.raytheon.uf.viz.vil;

import java.nio.FloatBuffer;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.vil.VILRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * VILResource
 * 
 * Implements Grid Image display for VIL data
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    11Nov2009    2037        dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class VILResource extends
        AbstractVizResource<VILResourceData, MapDescriptor> implements
        IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VILResource.class);

    public String icao;

    public String fieldName;

    public String fieldUnitString;

    public VILRecord record;

    public DataTime displayedDataTime;

    public DataTime previousDataTime;

    private HashMap<DataTime, GriddedImageDisplay2> griddedDisplayMap;

    private String colormapfile = null;

    /* The font used */
    public IFont font = null;

    private float vilmax = 0.0f;

    private float vilmin = 0.0f;

    boolean init = true;

    /* formatter */
    private DecimalFormat df = new DecimalFormat();

    public VILResource(VILResourceData data, LoadProperties props) {
        super(data, props);

        data.addChangeListener(this);

        griddedDisplayMap = new HashMap<DataTime, GriddedImageDisplay2>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        VILRecord rec = null;
        for (VILRecord record : resourceData.dataObjectMap.values()) {
            rec = record;
            break;
        }
        if (rec == null) {
            return "No Data Available";
        }

        StringBuilder prefix = new StringBuilder();
        prefix.append(rec.getIcao());
        prefix.append(" ");
        prefix.append(rec.getParameterName());
        prefix.append(" ");
        if (displayedDataTime != null) {
            prefix.append(displayedDataTime.getLegendString());
        }

        return prefix.toString();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject pdo : pdos) {
                try {
                    VILRecord vil = (VILRecord) pdo;
                    resourceData.dataObjectMap.put(vil.getDataTime(), vil);
                    record = vil;
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error updating VIL resource", e);
                }
            }
        }
        issueRefresh();
    }

    @Override
    protected void disposeInternal() {

        for (DataTime dTime : griddedDisplayMap.keySet()) {
            GriddedImageDisplay2 gDisplay = griddedDisplayMap.get(dTime);
            if (gDisplay != null) {
                gDisplay.dispose();
            }
        }

        griddedDisplayMap.clear();

        if (font != null) {
            font.dispose();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.df = new DecimalFormat();
        df.setMinimumIntegerDigits(1);
        df.setMaximumFractionDigits(2);
        df.setMinimumFractionDigits(1);
        df.setDecimalSeparatorAlwaysShown(true);
        init = true;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        target.clearClippingPlane();
        this.displayedDataTime = paintProps.getDataTime();

        if (this.font == null) {
            this.font = target.initializeFont("Dialog", 11, null);
        }

        // Pull the record out
        this.record = resourceData.dataObjectMap.get(this.displayedDataTime);

        if (record == null) {
            // Don't have data for this frame
            return;
        }

        GriddedImageDisplay2 gridDisplay = null;

        if (record.getDataArray() == null) {
            record = resourceData.populateRecord(record);
        }

        gridDisplay = griddedDisplayMap.get(displayedDataTime);
        if (gridDisplay == null) {
            gridDisplay = new GriddedImageDisplay2(FloatBuffer.wrap(record
                    .getDataArray()), record.getGridGeometry(), this);
            this.previousDataTime = displayedDataTime;
            griddedDisplayMap.put(displayedDataTime, gridDisplay);
        }

        if (record.getDataArray() == null) {
            // this should never happen, but just to be sure
            System.err.println("record.getDataArray() returned null");
            return;
        }

        ColorMapParameters colorMapParameters = getCapability(
                ColorMapCapability.class).getColorMapParameters();

        if (record != null && init) {
            // Get default colormap
            StyleRule sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.IMAGERY, getMatchCriteria());
            this.colormapfile = ((ImagePreferences) sr.getPreferences())
                    .getDefaultColormap();

            IColorMap cxml = ColorMapLoader.loadColorMap(colormapfile);
            ColorMap colorMap = new ColorMap(colormapfile, (ColorMap) cxml);
            colorMapParameters.setColorMap(colorMap);

            colorMapParameters.setDataMapping(((ImagePreferences) sr
                    .getPreferences()).getDataMapping());

            vilmax = colorMapParameters
                    .getDataMapping()
                    .getEntries()
                    .get(colorMapParameters.getDataMapping().getEntries()
                            .size() - 1).getDisplayValue().floatValue();
            vilmin = colorMapParameters.getDataMapping().getEntries().get(0)
                    .getDisplayValue().floatValue();
            colorMapParameters.setColorMapMax(vilmax);
            colorMapParameters.setColorMapMin(vilmin);

            init = false;
        }

        gridDisplay.paint(target, paintProps);
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
        String inspect = "NO DATA";

        if (record != null) {
            if (record.getDataArray() == null) {
                record = resourceData.populateRecord(record);
            }
            if (record.getDataArray() != null) {
                Coordinate coor = null;
                try {
                    coor = latLon.asGridCell(record.getGridGeometry(),
                            PixelInCell.CELL_CENTER);
                    // System.err.println(coor.toString());

                } catch (TransformException e) {
                    e.printStackTrace();
                } catch (FactoryException e) {
                    e.printStackTrace();
                }

                int select = (int) ((record.getNy() * Math.round(coor.y)) + Math
                        .round(coor.x));

                if (select < (record.getNx() * record.getNy()) && select > 0
                        && coor.x > 0 && coor.x < record.getNx()) {
                    float vilval = record.getDataArray()[select];
                    if (vilval == 0.0) {
                        inspect = "NO DATA";
                    } else if (vilval > vilmax) {
                        inspect = record.getFieldName() + ": > "
                                + df.format(vilmax) + " "
                                + record.getParameterUnit();
                    } else {
                        inspect = record.getFieldName() + ": "
                                + df.format(record.getDataArray()[select])
                                + " " + record.getParameterUnit();
                    }
                }
            }
        }

        return inspect;
    }

    /**
     * Get and load the style rule
     * 
     * @return
     */
    public ParamLevelMatchCriteria getMatchCriteria() {
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add(record.getPluginName());
        match.setParameterName(paramList);
        return match;
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        for (DataTime dTime : griddedDisplayMap.keySet()) {
            GriddedImageDisplay2 gDisplay = griddedDisplayMap.get(dTime);
            if (gDisplay != null) {
                gDisplay.project(descriptor.getGridGeometry());
            }
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        this.dataTimes.remove(dataTime);
        GriddedImageDisplay2 display = this.griddedDisplayMap.remove(dataTime);
        if (display != null) {
            display.dispose();
        }
    }
}
