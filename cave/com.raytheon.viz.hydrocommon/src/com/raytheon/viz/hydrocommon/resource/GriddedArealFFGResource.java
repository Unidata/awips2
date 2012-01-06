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
package com.raytheon.viz.hydrocommon.resource;

import java.awt.Point;
import java.awt.Rectangle;
import java.nio.FloatBuffer;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil.GeoAreaLineSegs;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Mean Areal FFG Resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GriddedArealFFGResource extends
        AbstractVizResource<GriddedArealFFGResourceData, MapDescriptor> {
    private FloatBuffer buf;

    private FloatBuffer buf2;

    /** The ColorMapParameters */
    private ColorMapParameters parameters;

    private List<Colorvalue> colorSet;

    /**
     * The Unit Converter
     */
    private UnitConverter cvt;

    private GriddedImageDisplay gridDisplay;

    private GridGeometry2D gridGeometry;

    private float brightness = 1.0f;

    private float contrast = 1.0f;

    private boolean isInterpolated;

    private Rectangle extent;

    private XmrgFile xmrg;

    private short[] data;

    private Date dataDate;

    private float minArea = 9.0f;

    /** The HRAP sub grid */
    private HRAPSubGrid subGrid;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected GriddedArealFFGResource(GriddedArealFFGResourceData resourceData,
            LoadProperties loadProperties, List<Colorvalue> colorSet) {
        super(resourceData, loadProperties);
        this.colorSet = colorSet;
        getAppsDefaults();
        loadData();
    }

    private void getAppsDefaults() {
        String s = AppsDefaults.getInstance().getToken("whfs_min_area_covered",
                "0.9");
        if ((s != null) && (s.length() > 0)) {
            this.minArea = Float.parseFloat(s);
        }
    }

    private void loadData() {
        initColorMapParams();

        xmrg = resourceData.getXmrg();

        try {
            if (xmrg != null) {
                xmrg.load();
                HydroDisplayManager.getInstance().setDataDate(
                        this.resourceData.getDataDate());
                this.dataDate = this.resourceData.getDataDate();
                data = xmrg.getData();

                buf = FloatBuffer.allocate(data.length);
                buf2 = FloatBuffer.allocate(buf.capacity());
                for (short s : data) {
                    if (s < 0) {
                        buf.put(0.0f);
                    } else {
                        float f = (float) Math.floor(cvt.convert(s));
                        buf.put(f);
                    }
                }
                buf.rewind();
                this.extent = xmrg.getHrapExtent();
            }

            if ((extent.x == 0) && (extent.y == 0)) {
                Rectangle coord = HRAPCoordinates.getHRAPCoordinates();
                if ((extent.width == coord.width)
                        && (extent.height == coord.height)) {
                    extent = coord;
                } else {
                    xmrg = null;
                    return;
                }
            }

            // Get Line Segs data
            ArrayList<GeoAreaLineSegs> meanAreaNodes = (ArrayList<GeoAreaLineSegs>) GeoUtil
                    .getInstance().getGeoAreaLinesegs("BASIN");

            for (GeoAreaLineSegs gals : meanAreaNodes) {
                /* compute average FFG value for basin and areal coverage */
                try {
                    computeMeanArealFfg(gals);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            subGrid = new HRAPSubGrid(extent);

            gridGeometry = MapUtil.getGridGeometry(subGrid);

            project(gridGeometry.getCoordinateReferenceSystem());
        } catch (Exception e) {
            xmrg = null;
            e.printStackTrace();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }
    }

    private void computeMeanArealFfg(GeoAreaLineSegs node) {
        /* compute average FFG value for basin and areal coverage */
        float totalCnt = 0;
        float valCnt = 0;
        double sum = 0.0;
        double avgVal;
        double rawVal;
        int width = this.extent.width;
        double areaCovered = 0;

        for (int i = 0; i < node.numrows; ++i) {
            totalCnt += node.end_cols[i] - node.beg_cols[i] + 1;
            int row = (int) (node.rows[i] - this.extent.y);

            /* loop on the number of columns in each row */
            for (int jcol = (int) node.beg_cols[i]; jcol <= node.end_cols[i]; ++jcol) {
                /*
                 * sum the value and increment the cnts. note that the array
                 * index method must match the method by which the grid was
                 * originally loaded
                 */
                int col = jcol - this.extent.x;

                // Grid starts outside the window
                if ((row >= extent.height) || (col >= extent.width)
                        || (row < 0) || (col < 0)) {
                    continue;
                }
                if (row * width + col >= data.length) {
                    continue;
                }

                // compute the offset into the data buffer
                int offset = (((extent.height - row) * extent.width) + col);
                if ((offset > data.length) || (offset < 0)) {
                    continue;
                }

                // value converted to real data value, from image value.
                rawVal = (float) cvt.convert(data[offset]);
                if (rawVal >= 0.00f) {
                    sum += rawVal;
                    valCnt++;
                }
            }
        }

        /*
         * compute the avg ffg value as the average of all the bins within the
         * area that have valid area_id data.
         */
        if (totalCnt <= 0) {
            areaCovered = 0;
        } else {
            areaCovered = (valCnt / totalCnt);
        }

        if (valCnt > 0) {
            avgVal = (sum / valCnt);
        } else {
            avgVal = 0;
        }

        /*
         * adjust the returned value if it is less than some minimal number;
         * this is due to the nature of the precip data, especially the radar
         * data which contains super-tiny values
         */
        if (avgVal < 0.00001) {
            avgVal = 0;
        }

        if (minArea > areaCovered) {
            return;
        }
        node.avg_val = (float) avgVal;
        node.area_covered = (float) areaCovered;

        // Calculate the mean areal value once for all cells
        float ma = node.avg_val;

        for (int i = 0; i < node.numrows; ++i) {
            int row = (int) (node.rows[i] - extent.y + 1);
            for (int jcol = (int) node.beg_cols[i]; jcol <= node.end_cols[i]; ++jcol) {
                int col = (jcol - extent.x);

                // compute the offset into the buffer to store mean areal values
                int offset = (((extent.height - row) * extent.width) + col);
                if ((offset >= buf.capacity()) || (offset < 0)) {
                    continue;
                }

                buf2.put(offset, ma);
            }
        }
    }

    private void initColorMapParams() {
        ColorMap colorMap = new ColorMap(colorSet.size());
        DataMappingPreferences dmPref = new DataMappingPreferences();
        int i = 0;
        for (Colorvalue cv : colorSet) {
            RGB rgb = RGBColors.getRGBColor(cv.getColorname().getColorName());
            colorMap.setColor(i, new Color(rgb.red / 255f, rgb.green / 255f,
                    rgb.blue / 255f));

            DataMappingEntry entry = new DataMappingEntry();
            entry.setPixelValue((double) i);
            entry.setDisplayValue(cv.getId().getThresholdValue());
            dmPref.addEntry(entry);

            i++;
        }
        DataMappingEntry entry = new DataMappingEntry();
        entry.setPixelValue((double) (i - 1));
        entry.setDisplayValue(Double.MAX_VALUE);
        dmPref.addEntry(entry);

        dmPref.getEntries().get(0).setLabel("");
        dmPref.getEntries().get(1).setLabel("");

        ColorMapCapability cmc = getCapability(ColorMapCapability.class);

        parameters = cmc.getColorMapParameters();
        if (parameters == null) {
            parameters = new ColorMapParameters();
            cmc.setColorMapParameters(parameters);
        }
        parameters.setColorMap(colorMap);
        parameters.setDataMapping(dmPref);

        Unit<?> displayUnit = NonSI.INCH;
        Unit<?> dataUnit = SI.MILLIMETER.divide(100);

        parameters.setFormatString("0.00");

        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setDataUnit(dataUnit);

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);
        cvt = parameters.getDataToImageConverter();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (buf2 == null) {
            return;
        }

        if (gridDisplay == null) {
            gridDisplay = new GriddedImageDisplay(buf2, descriptor,
                    gridGeometry);

            gridDisplay.setColorMapParameters(getCapability(
                    ColorMapCapability.class).getColorMapParameters());
        }

        GriddedImagePaintProperties giProps = new GriddedImagePaintProperties(
                paintProps, brightness, contrast, isInterpolated);

        gridDisplay.paint(target, giProps);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        String noData = "";
        if (xmrg == null) {
            noData = " No Data Available";
        }

        String name = null;
        SimpleDateFormat sdf = new SimpleDateFormat("EEE MM/dd HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        int hours = this.resourceData.getDuration()
                / HydroConstants.MILLIS_PER_HOUR;
        String hourStr = "hour";
        if (hours != 1) {
            hourStr = hourStr.concat("s");
        }

        if (dataDate == null) {
            name = "FFG No Data Available";
        } else {
            name = "FFG Gridded Basin" + " " + hours + " " + hourStr + " "
                    + sdf.format(this.dataDate) + noData;
        }

        return name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> values = interrogate(coord);

        if (values == null) {
            return "NO DATA";
        } else {
            return values.get("Value").toString();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#interrogate(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        if ((xmrg == null)) {
            return null;
        }

        Map<String, Object> values = new HashMap<String, Object>();
        try {
            Coordinate gridCell = coord.asGridCell(HRAP.getInstance()
                    .getGridGeometry(), PixelInCell.CELL_CENTER);

            Point p = new Point((int) gridCell.x, (int) gridCell.y);

            values.put("Value", "");

            Rectangle extent = subGrid.getExtent();
            if (extent.contains(p)) {
                int x = p.x - extent.x;
                int y = extent.height - 1 - (p.y - extent.y);

                float f = buf2.get(y * subGrid.getNx() + x);
                double value = parameters.getImageToDisplayConverter().convert(
                        f);

                DecimalFormat df = new DecimalFormat(
                        parameters.getFormatString());
                values.put("Value", df.format(value));
            }

        } catch (Exception e) {
            throw new VizException("Error performing interrogation", e);
        }

        return values;
    }
}
