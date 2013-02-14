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
package com.raytheon.uf.viz.cwat;

import java.nio.ShortBuffer;
import java.util.ArrayList;
import java.util.HashMap;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.cwat.CWATRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.monitor.scan.SCTI;
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
 * CWATResource
 * 
 * Implements Grid Image display for CWAT data
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    16JUN2009    2037        dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class CWATResource extends
        AbstractVizResource<CWATResourceData, MapDescriptor> implements
        IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CWATResource.class);

    public String icao;

    public String fieldName;

    public String fieldUnitString;

    public CWATRecord record;

    private HashMap<DataTime, GriddedImageDisplay2> griddedDisplayMap;

    public DataTime displayedDataTime;

    public DataTime previousDataTime;

    private String colormapfile = null;

    /* The font used */
    public IFont font = null;

    private float cwatmax = 0.0f;

    private float cwatmin = 0.0f;

    boolean init = true;

    public CWATResource(CWATResourceData data, LoadProperties props) {
        super(data, props);

        data.addChangeListener(this);
        this.dataTimes = new ArrayList<DataTime>();
        griddedDisplayMap = new HashMap<DataTime, GriddedImageDisplay2>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        CWATRecord record = null;
        for (CWATRecord rec : resourceData.dataObjectMap.values()) {
            record = rec;
            break;
        }

        if (record == null) {
            return "";
        }

        StringBuilder prefix = new StringBuilder();
        prefix.append(record.getIcao());
        prefix.append(" ");
        prefix.append(record.getParameterName());

        return prefix.toString();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject pdo : pdos) {
                try {
                    CWATRecord cwat = (CWATRecord) pdo;
                    resourceData.dataObjectMap.put(cwat.getDataTime(), cwat);
                    record = cwat;
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error updating CWAT resource", e);
                }
            }

            issueRefresh();
        }
    }

    @Override
    protected void disposeInternal() {

        for (DataTime key : griddedDisplayMap.keySet()) {
            GriddedImageDisplay2 gDisplay = griddedDisplayMap.get(key);
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
        if (this.font == null) {
            this.font = target.initializeFont("Dialog", 11, null);
        }
        init = true;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        this.displayedDataTime = paintProps.getDataTime();

        // Pull the record out
        this.record = resourceData.dataObjectMap.get(this.displayedDataTime);

        if (record == null) {
            // Don't have data for this frame
            return;
        }

        GriddedImageDisplay2 gridDisplay = griddedDisplayMap
                .get(displayedDataTime);

        if (record.getDataArray() == null) {
            record = resourceData.populateRecord(record);
        }

        if (gridDisplay == null) {
            gridDisplay = new GriddedImageDisplay2(ShortBuffer.wrap(record
                    .getDataArray()), record.getGridGeometry(), this);
            this.previousDataTime = displayedDataTime;
            griddedDisplayMap.put(displayedDataTime, gridDisplay);
        }

        ColorMapParameters colorMapParameters = getCapability(
                ColorMapCapability.class).getColorMapParameters();

        if (record != null && init) {
            StyleRule sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.IMAGERY, getMatchCriteria());
            this.colormapfile = ((ImagePreferences) sr.getPreferences())
                    .getDefaultColormap();

            IColorMap cxml = ColorMapLoader.loadColorMap(colormapfile);
            ColorMap colorMap = new ColorMap(colormapfile, (ColorMap) cxml);
            colorMapParameters.setColorMap(colorMap);

            colorMapParameters.setDataMapping(((ImagePreferences) sr
                    .getPreferences()).getDataMapping());

            cwatmax = colorMapParameters
                    .getDataMapping()
                    .getEntries()
                    .get(colorMapParameters.getDataMapping().getEntries()
                            .size() - 1).getDisplayValue().floatValue();
            cwatmin = colorMapParameters.getDataMapping().getEntries().get(0)
                    .getDisplayValue().floatValue();
            colorMapParameters.setDataMax(Short.MAX_VALUE);
            colorMapParameters.setDataMin(Short.MIN_VALUE);
            colorMapParameters.setColorMapMax(cwatmax);
            colorMapParameters.setColorMapMin(cwatmin);

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

            Coordinate coor = null;
            try {
                if (record.getDataArray() != null) {
                    coor = latLon.asGridCell(record.getGridGeometry(),
                            PixelInCell.CELL_CENTER);
                    int index = (int) ((record.getNx() * Math.round(coor.y)) + Math
                            .round(coor.x));
                    int value = 0;
                    if (index < record.getDataArray().length && index > -1) {
                        value = record.getDataArray()[index];

                        if (value >= 10) {
                            inspect = value + ":  "
                                    + SCTI.getSCTImessage(value);
                        }
                    }
                }
            } catch (TransformException e) {
                e.printStackTrace();
            } catch (FactoryException e) {
                e.printStackTrace();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return inspect;
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
}
