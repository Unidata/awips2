/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.viz.mpe.ui.rsc;

import java.nio.FloatBuffer;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.mpe.constants.RFCQPEConstants;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay2;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.polygon.PolygonEditManager;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResourceData.MPEFieldFrame;

import systems.uom.common.USCustomary;

/**
 * 
 * RFC QPE resource
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2017 17911       wkwock     Initial creation
 *
 * </pre>
 *
 * @author wkwock
 */
public class RFCQPEResource
        extends AbstractVizResource<RFCQPEResourceData, MapDescriptor> {

    /** Grid Geometry 2D */
    private GridGeometry2D gridGeometry;

    /** color map parameters */
    private ColorMapParameters parameters;

    /** QPE data buffer */
    private FloatBuffer buf;

    /** last QPE data buffer */
    private FloatBuffer lastBuf = null;

    /** color values */
    private List<Colorvalue> colorSet;

    /** insert time. It's when data got updated. */
    private Date insertTime = null;

    /** RFC site name */
    private String rfcSite;

    /** MPE field frame */
    private MPEFieldFrame frame = null;

    public RFCQPEResource(RFCQPEResourceData resourceData,
            LoadProperties loadProperties, List<Colorvalue> colorSet,
            String rfcSite) {
        super(resourceData, loadProperties);
        this.colorSet = colorSet;
        initColorMapParams();
        this.rfcSite = rfcSite;
    }

    @Override
    protected void disposeInternal() {
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        Date displayDate = MPEDisplayManager.getCurrent()
                .getCurrentDisplayedDate();

        if (displayDate == null) {
            return;
        }

        buf = resourceData.getQPEData();
        UnitConverter dataToImage = parameters.getDisplayToColorMapConverter();
        FloatBuffer imageData = FloatBuffer.allocate(buf.limit());
        for (int i = 0; i < buf.limit(); ++i) {
            float value = buf.get(i);
            imageData.put((float) dataToImage.convert(value));
        }

        if (frame == null) {
            DisplayFieldData dt = MPEDisplayManager.getCurrent()
                    .getDisplayFieldType();
            frame = new MPEFieldFrame(displayDate, null,
                    PolygonEditManager.getPolygonEdits(dt, displayDate));
        }

        synchronized (frame) {
            if ((frame.imageDisplay == null) || (lastBuf != buf)) {
                frame.imageDisplay = new GriddedImageDisplay2(imageData,
                        gridGeometry, this);
                lastBuf = buf;
            }
            frame.imageDisplay.paint(target, paintProps);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        try {
            HRAPSubGrid subGrid = new HRAPSubGrid(resourceData.getExtent());
            gridGeometry = MapUtil.getGridGeometry(subGrid);
        } catch (Exception e) {
            throw new VizException("Error computing hrap extent coordinates");
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        // Delete the gridDisplay so it can be regenerated in the correct
        // projection
        if (frame != null) {
            frame.disposeImage();
        }
    }

    /**
     * Set inert time.
     * 
     * @param insertTime
     */
    protected void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
    }

    @Override
    public String getName() {
        if (insertTime == null) {
            return rfcSite.toUpperCase() + " no data";
        }

        SimpleDateFormat sdf = new SimpleDateFormat("E HH:mm'Z' dd-MMM-yy");
        return rfcSite.toUpperCase() + " updated " + sdf.format(insertTime);
    }

    /**
     * Initialize color map parameters
     */
    private void initColorMapParams() {
        ColorMap colorMap = new ColorMap(colorSet.size());
        DataMappingPreferences dmPref = new DataMappingPreferences();
        int i = 0;

        for (Colorvalue cv : colorSet) {
            RGB rgb = RGBColors.getRGBColor(cv.getColorname().getColorName());
            Color color = new Color(rgb.red / 255f, rgb.green / 255f,
                    rgb.blue / 255f);
            DataMappingEntry entry = new DataMappingEntry();
            entry.setDisplayValue(cv.getId().getThresholdValue());
            if ((cv.getId()
                    .getThresholdValue() == RFCQPEConstants.MISSING_VALUE)
                    || (cv.getId().getThresholdValue() == 0)) {
                color.setAlpha(0);
            } else if (cv.getId().getThresholdValue() < 0) {
                color.setAlpha(0.5f);
            } else {
                color.setAlpha(1);
            }

            entry.setPixelValue((double) i);
            dmPref.addEntry(entry);
            colorMap.setColor(i, color);
            i++;
        }
        DataMappingEntry entry = new DataMappingEntry();
        entry.setPixelValue((double) (i - 1));
        entry.setDisplayValue(Double.MAX_VALUE);
        dmPref.addEntry(entry);
        ColorMapCapability cmc = getCapability(ColorMapCapability.class);
        parameters = cmc.getColorMapParameters();
        if (parameters == null) {
            parameters = new ColorMapParameters();
            cmc.setColorMapParameters(parameters);
        }
        parameters.setColorMap(colorMap);
        parameters.setDataMapping(dmPref);
        Unit<?> displayUnit = USCustomary.INCH;
        parameters.setDisplayUnit(displayUnit);
        parameters.setColorMapUnit(dmPref.getImageUnit(displayUnit));
        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
    }
}
