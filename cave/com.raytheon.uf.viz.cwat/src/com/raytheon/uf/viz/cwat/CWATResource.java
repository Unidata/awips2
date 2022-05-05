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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.cwat.CWATRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.monitor.scan.SCTI;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay2;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import org.locationtech.jts.geom.Coordinate;

/**
 * CWATResource
 * 
 * Implements Grid Image display for CWAT data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------
 * Jun 16, 2009  2037     dhladky   Initial Creation.
 * Apr 17, 2013  1916     njensen   Overrode getDataTimes()
 * Nov 05, 2015  5070     randerso  Adjust font sizes for dpi scaling
 * Apr 04, 2018  6889     njensen   Use brightness from ImagePreferences if
 *                                  present but missing in ImagingCapability
 * 
 * </pre>
 * 
 * @author dhladky
 */
public class CWATResource
        extends AbstractVizResource<CWATResourceData, MapDescriptor>
        implements IResourceDataChanged {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CWATResource.class);

    protected CWATRecord record;

    private Map<DataTime, GriddedImageDisplay2> griddedDisplayMap;

    protected DataTime displayedDataTime;

    protected DataTime previousDataTime;

    /* The font used */
    protected IFont font = null;

    private boolean init = true;

    public CWATResource(CWATResourceData data, LoadProperties props) {
        super(data, props);

        data.addChangeListener(this);
        griddedDisplayMap = new HashMap<>();
    }

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
                    synchronized (resourceData.dataObjectMap) {
                        resourceData.dataObjectMap.put(cwat.getDataTime(),
                                cwat);
                    }
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
            this.font = target.initializeFont("Dialog", 9, null);
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
            gridDisplay = new GriddedImageDisplay2(
                    this.getCapability(ImagingCapability.class),
                    ShortBuffer.wrap(record.getDataArray()),
                    record.getGridGeometry(), this);
            this.previousDataTime = displayedDataTime;
            griddedDisplayMap.put(displayedDataTime, gridDisplay);
        }

        ColorMapParameters colorMapParameters = getCapability(
                ColorMapCapability.class).getColorMapParameters();

        if (record != null && init) {
            StyleRule sr;
            try {
                sr = StyleManager.getInstance().getStyleRule(
                        StyleManager.StyleType.IMAGERY, getMatchCriteria());
            } catch (StyleException e) {
                throw new VizException(e.getLocalizedMessage(), e);
            }
            ImagePreferences imgPrefs = (ImagePreferences) sr.getPreferences();
            String colormapfile = imgPrefs.getDefaultColormap();

            /*
             * If the capability already has a brightness it was most likely
             * loaded from a procedure/bundle and that should take precedent. If
             * brightness is not set, then try style rules to get a brightness.
             */
            ImagingCapability imgCap = getCapability(ImagingCapability.class);
            if (!imgCap.isBrightnessSet() && imgPrefs.getBrightness() != null) {
                imgCap.setBrightness(imgPrefs.getBrightness());
            }

            try {
                IColorMap cxml = ColorMapLoader.loadColorMap(colormapfile);
                ColorMap colorMap = new ColorMap(colormapfile, (ColorMap) cxml);
                colorMapParameters.setColorMap(colorMap);
            } catch (ColorMapException e) {
                throw new VizException(
                        "Error loading colormap file " + colormapfile, e);
            }

            colorMapParameters.setDataMapping(imgPrefs.getDataMapping());

            float cwatmax = colorMapParameters.getDataMapping().getEntries()
                    .get(colorMapParameters.getDataMapping().getEntries().size()
                            - 1)
                    .getDisplayValue().floatValue();
            float cwatmin = colorMapParameters.getDataMapping().getEntries()
                    .get(0).getDisplayValue().floatValue();
            colorMapParameters.setDataMax(Short.MAX_VALUE);
            colorMapParameters.setDataMin(Short.MIN_VALUE);
            colorMapParameters.setColorMapMax(cwatmax);
            colorMapParameters.setColorMapMin(cwatmin);

            init = false;
        }

        gridDisplay.paint(target, paintProps);

    }

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
                    int index = (int) ((record.getNx() * Math.round(coor.y))
                            + Math.round(coor.x));
                    int value = 0;
                    if (index < record.getDataArray().length && index > -1) {
                        value = record.getDataArray()[index];

                        if (value >= 10) {
                            inspect = value + ":  "
                                    + SCTI.getSCTImessage(value);
                        }
                    }
                }
            } catch (Exception e) {
                statusHandler.error("Error inspecting CWAT data", e);
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
        synchronized (resourceData.dataObjectMap) {
            resourceData.dataObjectMap.remove(dataTime);
        }
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
        ArrayList<String> paramList = new ArrayList<>();
        paramList.add(record.getPluginName());
        match.setParameterName(paramList);
        return match;
    }

    @Override
    public DataTime[] getDataTimes() {
        DataTime[] times = new DataTime[0];
        synchronized (resourceData.dataObjectMap) {
            times = this.resourceData.dataObjectMap.keySet().toArray(times);
        }
        Arrays.sort(times);
        return times;
    }
}
