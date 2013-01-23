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
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.DataTime;
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
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.constants.FFGConstants.ResolutionLevel;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil.GeoAreaLineSegs;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RFCGriddedBasinFFGResource extends
        AbstractVizResource<RFCGriddedBasinFFGResourceData, MapDescriptor> {

    /** The ColorMapParameters */
    private ColorMapParameters parameters;

    private List<Colorvalue> colorSet;

    /**
     * The Unit Converter
     */
    private UnitConverter cvt;

    /**
     * The data in a float[]
     */
    private float[] data;

    /** Buffer to hold the data for display */
    private FloatBuffer buf;

    /** The GriddedImageDisplay */
    private GriddedImageDisplay2 gridDisplay;

    /** The grid geometry */
    private GridGeometry2D gridGeometry;

    private Rectangle rfcExtent;

    private GridRecord gr;

    private float minArea = 9.0f;

    /** The HRAP sub grid */
    private HRAPSubGrid subGrid;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected RFCGriddedBasinFFGResource(
            RFCGriddedBasinFFGResourceData resourceData,
            LoadProperties loadProperties, List<Colorvalue> colorSet) {
        super(resourceData, loadProperties);
        this.colorSet = colorSet;
        this.gr = resourceData.getGr();
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

        gridGeometry = gr.getLocation().getGridGeometry();

        try {
            data = new float[((float[]) gr.getMessageData()).length];
            data = (float[]) gr.getMessageData();
            HydroDisplayManager.getInstance().setDataDate(
                    gr.getDataTime().getRefTime());

            buf = FloatBuffer.allocate(data.length);

            HRAP hrap;
            Coordinate org = MapUtil.gridCoordinateToLatLon(
                    new Coordinate(0, 0), PixelOrientation.CENTER,
                    gr.getLocation());

            int nx = gr.getLocation().getNx();
            int ny = gr.getLocation().getNy();

            hrap = HRAP.getInstance();

            Coordinate ulRfcNationalScale = hrap.latLonToGridCoordinate(org,
                    PixelOrientation.CENTER);

            rfcExtent = new Rectangle((int) ulRfcNationalScale.x,
                    (int) ulRfcNationalScale.y - ny, nx, ny);

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

            subGrid = new HRAPSubGrid(rfcExtent);

            gridGeometry = MapUtil.getGridGeometry(subGrid);
            project(gridGeometry.getCoordinateReferenceSystem());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void computeMeanArealFfg(GeoAreaLineSegs node) {
        /* compute average FFG value for basin and areal coverage */
        float totalCnt = 0;
        float valCnt = 0;
        double sum = 0.0;
        double avgVal;
        double rawVal;
        int width = this.rfcExtent.width;
        double areaCovered = 0;

        for (int i = 0; i < node.numrows; ++i) {
            totalCnt += node.end_cols[i] - node.beg_cols[i] + 1;
            int row = (int) (node.rows[i] - this.rfcExtent.y);

            /* loop on the number of columns in each row */
            for (int jcol = (int) node.beg_cols[i]; jcol <= node.end_cols[i]; ++jcol) {
                /*
                 * sum the value and increment the cnts. note that the array
                 * index method must match the method by which the grid was
                 * originally loaded
                 */
                int col = jcol - this.rfcExtent.x;

                // Grid starts outside the window
                if ((row >= rfcExtent.height) || (col >= rfcExtent.width)
                        || (row < 0) || (col < 0)) {
                    continue;
                }
                if (row * width + col >= data.length) {
                    continue;
                }

                // compute the offset into the data buffer
                int offset = (((rfcExtent.height - row) * rfcExtent.width) + col);
                if ((offset > data.length) || (offset < 0)) {
                    continue;
                }

                rawVal = data[offset];
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
        float ma = (float) parameters.getDataToImageConverter().convert(
                node.avg_val);

        for (int i = 0; i < node.numrows; ++i) {
            int row = (int) (node.rows[i] - rfcExtent.y + 1);
            for (int jcol = (int) node.beg_cols[i]; jcol <= node.end_cols[i]; ++jcol) {
                int col = (jcol - rfcExtent.x);

                // compute the offset into the buffer to store mean areal values
                int offset = (((rfcExtent.height - row) * rfcExtent.width) + col);
                if ((offset >= buf.capacity()) || (offset < 0)) {
                    continue;
                }

                buf.put(offset, ma);
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
        Unit<?> dataUnit = SI.MILLIMETER;

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
        if (buf == null) {
            return;
        }

        if (gridDisplay == null) {
            gridDisplay = new GriddedImageDisplay2(buf, gridGeometry, this);
        }

        gridDisplay.paint(target, paintProps);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        String noData = "";
        if ((buf == null) || (buf.array().length == 0)) {
            noData = " No Data Available";
        }

        String name = null;
        ResolutionLevel res = this.resourceData.getResolution();
        SimpleDateFormat sdf = new SimpleDateFormat("EEE MM/dd HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        int hours = this.resourceData.getDuration()
                / HydroConstants.MILLIS_PER_HOUR;
        String hourStr = "hour";
        if (hours != 1) {
            hourStr = hourStr.concat("s");
        }

        if (this.resourceData.getDataDate() == null) {
            name = "FFG No Data Available";
        } else {
            name = "FFG Gridded " + res.getResolution() + " " + hours + " "
                    + hourStr + " "
                    + sdf.format(this.resourceData.getDataDate()) + noData;
        }

        return name;
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
            return "FFG NO DATA";
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
        if (this.data == null) {
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
                float f = buf.get(y * subGrid.getNx() + x);
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // TODO Auto-generated method stub

    }

    @Override
    public void remove(DataTime dataTime) {
        if (gridDisplay != null) {
            gridDisplay.dispose();
        }
    }

    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        this.descriptor = descriptor;
    }
}
