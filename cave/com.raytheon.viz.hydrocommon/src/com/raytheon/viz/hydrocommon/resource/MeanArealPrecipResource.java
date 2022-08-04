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

import java.awt.Rectangle;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.FloatBuffer;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay2;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil.GeoAreaLineSegs;

import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 * Display the mean areal precipitation.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2009            mpduff      Initial creation
 * Jul 29, 2014 #3465      mapeters    Updated deprecated drawString() calls.
 * Jun 11, 2018 6605       tgurney     Check for existence of xmrg file before reading data
 *
 * </pre>
 *
 * @author mpduff
 */

public class MeanArealPrecipResource
        extends AbstractVizResource<AbstractResourceData, MapDescriptor> {
    private static final String MIN_COVERAGE_TOKEN = "whfs_min_area_covered";

    private List<Colorvalue> colorSet = null;

    private String areaType = null;

    private ColorMapParameters parameters;

    private UnitConverter cvt;

    private List<GeoAreaLineSegs> meanAreaNodes;

    private FloatBuffer buf;

    private FloatBuffer buf2;

    private int xor = 0;

    private int yor = 0;

    private int max_columns = 0;

    private int max_rows = 0;

    private HRAPSubGrid subGrid;

    private Rectangle extent;

    private GridGeometry2D gridGeometry;

    private short[] data;

    private GriddedImageDisplay2 gridDisplay;

    /** The ID */
    private boolean ids = false;

    /** The value */
    private boolean labels = false;

    private double scaleWidthValue = 0.0;

    private double scaleHeightValue = 0.0;

    private IFont font = null;

    public static enum ImageSize {
        VERY_SMALL(11, 20), SMALL(13, 25), MEDIUM(15, 30), LARGE(17, 35);

        private final int width;

        private final int height;

        ImageSize(int width, int height) {
            this.width = width;
            this.height = height;
        }

        public int getWidth() {
            return width;
        }

        public int getHeight() {
            return height;
        }
    }

    public MeanArealPrecipResource(AbstractResourceData resourceData,
            LoadProperties loadProperties, String boundaryType,
            List<Colorvalue> colorSet) {
        super(null, loadProperties);
        this.colorSet = colorSet;
        areaType = boundaryType;
        loadData();
    }

    private void loadData() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();
        float minCoverage = Float
                .parseFloat(appsDefaults.getToken(MIN_COVERAGE_TOKEN));
        ColorMap colorMap = new ColorMap(colorSet.size());
        colorMap.setName(displayManager.getCvUse());
        DataMappingPreferences dmPref = new DataMappingPreferences();

        ids = displayManager.isIds();
        labels = displayManager.isLabels();

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

        Unit<?> displayUnit = USCustomary.INCH;
        Unit<?> dataUnit = MetricPrefix.MILLI(SI.METRE).divide(100);

        parameters.setFormatString("0.00");

        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setDataUnit(dataUnit);

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);
        cvt = parameters.getDataToImageConverter();

        readData();
        meanAreaNodes = GeoUtil.getInstance().getGeoAreaLinesegs(areaType);
        if (extent != null) {
            compute_mean_areal_precip(buf, meanAreaNodes, xor, yor, max_columns,
                    max_rows);
            try {
                subGrid = new HRAPSubGrid(extent);
            } catch (Exception e) {
                statusHandler.warn("Failed to create HRAP subgrid from extent "
                        + extent.toString(), e);
            }

            gridGeometry = MapUtil.getGridGeometry(subGrid);

            try {
                project(gridGeometry.getCoordinateReferenceSystem());
            } catch (InvalidGridGeometryException | VizException e) {
                statusHandler.warn(
                        "Failed to project " + gridGeometry.toString(), e);
            }
        }
    }

    private void compute_mean_areal_precip(FloatBuffer fbuf,
            List<GeoAreaLineSegs> meanAreaNodes2, int xor, int yor,
            int max_columns, int max_rows) {

        float cur_max;
        float cur_min;
        int col;
        int i;
        int jcol;
        int miss_cnt;
        int row;
        int total_cnt;
        int val_cnt;
        float raw_val;
        float sum;

        if (meanAreaNodes2 == null || meanAreaNodes2.isEmpty()) {
            statusHandler.debug("\nIn routine 'compute_mean_areal_precip':\n"
                    + "No lineseg information exists.  Cannot\n"
                    + "calculate mean areal precipitation.\n");
            return;
        }

        /* Initialize the list. */
        for (GeoAreaLineSegs pNode : meanAreaNodes2) {
            /* initialize */
            miss_cnt = total_cnt = val_cnt = 0;
            cur_max = 0.0f;
            cur_min = 0.0f;
            sum = 0.0f;

            /* loop on the number of rows for this area */
            for (i = 0; i < pNode.numrows; ++i) {
                total_cnt += pNode.end_cols[i] - pNode.beg_cols[i] + 1;

                /* loop on the number of columns in each row */
                for (jcol = (int) pNode.beg_cols[i]; jcol <= pNode.end_cols[i]; ++jcol) {
                    /*
                     * sum the value and increment the counts. note that the
                     * array index method must match the method by which the
                     * grid was originally loaded
                     */

                    row = (int) pNode.rows[i] - yor;
                    col = (jcol - xor);

                    /*-----------------------------------------------*/
                    /* check that box is within site's area */
                    /* if not, return -1. */
                    /*-----------------------------------------------*/

                    if ((row >= max_rows) || (col >= max_columns) || (row < 0)
                            || (col < 0)) {
                        ++miss_cnt;
                    } else {
                        // compute the offset into the data buffer
                        int offset = (((extent.height - row) * extent.width)
                                + col);
                        if ((offset >= buf.capacity()) || (offset < 0)) {
                            continue;
                        }
                        // value converted to real data value, from image value.
                        raw_val = (float) parameters
                                .getImageToDisplayConverter()
                                .convert(buf.get(offset));

                        if (raw_val > 0.00f) {
                            sum += raw_val;
                            if (raw_val > cur_max) {
                                cur_max = raw_val;
                            }
                            if (raw_val < cur_min) {
                                cur_min = raw_val;
                            }

                            ++val_cnt;
                        } else {
                            ++miss_cnt;
                        }
                    }
                }
            }

            /*
             * compute the avg ffg value as the average of all the bins within
             * the area that have valid area_id data.
             */
            if (total_cnt <= 0) {
                pNode.area_covered = 0.0f;
            } else {
                pNode.area_covered = ((float) val_cnt / (float) total_cnt);
            }

            if (val_cnt > 0) {

                pNode.avg_val = sum / val_cnt;
                pNode.max_val = cur_max;
                pNode.min_val = cur_min;
            } else {
                // values corrected to 0.00 in.
                pNode.avg_val = 0.0f;
                pNode.max_val = 0.0f;
                pNode.min_val = 0.0f;
            }

            /*
             * adjust the returned value if it is less than some minimal number;
             * this is due to the nature of the precip data, especially the
             * radar data which contains super-tiny values
             */

            if (pNode.avg_val < .00001) {
                pNode.avg_val = 0.0f;
            }

            if (pNode.max_val < .00001) {
                pNode.max_val = 0.0f;
            }

            if (pNode.min_val < .00001) {
                pNode.min_val = 0.0f;
            }
            for (i = 0; i < pNode.numrows; ++i) {
                for (jcol = (int) pNode.beg_cols[i]; jcol <= pNode.end_cols[i]; ++jcol) {
                    row = (int) (pNode.rows[i] - yor);
                    col = (jcol - xor);
                    // compute the offset into the buffer to store mean areal
                    // values.
                    int offset = (((extent.height - row) * extent.width) + col);
                    if ((offset >= buf.capacity()) || (offset < 0)) {
                        continue;
                    }
                    // convert the mean areal value back to image values
                    float map = (float) parameters.getDisplayToImageConverter()
                            .convert(pNode.avg_val);
                    // store the value into a new float buffer to use in the
                    // areal projection
                    buf2.put(offset, map);
                }
            }
        }
        return;
    }

    /**
     * Accumulate data from specified xmrg file into the data field of this
     * class. If data is null then it will be created.
     *
     * @param fname
     */
    private void accumData(String fname) {
        XmrgFile xmrg = null;
        try {
            xmrg = new XmrgFile(fname);
            xmrg.load();
        } catch (FileNotFoundException e) {
            statusHandler.debug("xmrg file " + fname + " not found", e);
            return;
        } catch (IOException e) {
            statusHandler.warn("Failed to load xmrg file " + fname, e);
            return;
        }
        if (data == null) {
            extent = xmrg.getHrapExtent();
            int datasz = extent.height * extent.width;
            xor = extent.x;
            yor = extent.y;
            max_columns = extent.width;
            max_rows = extent.height;
            buf = FloatBuffer.allocate(datasz);
            buf2 = FloatBuffer.allocate(buf.capacity());
            // this is used to accumulate all hours
            data = new short[datasz];
        }

        short[] tempdata = xmrg.getData();
        for (int c = 0; c < tempdata.length; c++) {
            if ((data[c] < 0) && (tempdata[c] >= 0)) {
                data[c] = tempdata[c];
            } else if ((data[c] >= 0) && (tempdata[c] > 0)) {
                data[c] += tempdata[c];
            }
            c++;
        }
    }

    private void readData() {
        // this will accumulate all hours requested and display it
        // this holds current xmrg values
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();

        String dirname = AppsDefaults.getInstance()
                .getToken(HydroConstants.XMRG_DIR_TOKEN);

        Calendar dataDateCal = Calendar
                .getInstance((TimeZone.getTimeZone("GMT")));
        dataDateCal.setTime(displayManager.getDataDate());
        int accumHours = displayManager.getAccumInterval();

        try {
            for (int k = 0; k < accumHours; k++) {
                Calendar accumDateCal = Calendar
                        .getInstance((TimeZone.getTimeZone("GMT")));
                accumDateCal.setTime(dataDateCal.getTime());
                accumDateCal.add(Calendar.SECOND,
                        -k * TimeUtil.SECONDS_PER_HOUR);
                String dtform = HydroConstants.QPE_DATE_FORMAT
                        .format(accumDateCal.getTime());
                String xmrgFileName = FileUtil.join(dirname,
                        displayManager.getCvUse() + dtform + "z");
                accumData(xmrgFileName);
            }
            if (data != null) {
                for (short s : data) {
                    float f = (float) Math.floor(cvt.convert(s));
                    buf.put(f);
                }
                buf.rewind();

                if ((extent.x == 0) && (extent.y == 0)) {
                    Rectangle coord = HRAPCoordinates.getHRAPCoordinates();
                    if ((extent.width == coord.width)
                            && (extent.height == coord.height)) {
                        extent = coord;
                    } else {
                        return;
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.warn("Failed to read data from XMRG file", e);
        }
    }

    @Override
    protected void disposeInternal() {
        if (gridDisplay != null) {
            gridDisplay.dispose();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // TODO Auto-generated method stub

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if ((buf2 == null) || (gridGeometry == null)) {
            return;
        }
        // Don't display the default colorbar
        // target.setUseBuiltinColorbar(false);

        if (gridDisplay == null) {
            gridDisplay = new GriddedImageDisplay2(buf2, gridGeometry, this);
        }

        gridDisplay.paint(target, paintProps);

        if ((ids) || (labels)) {
            List<DrawableString> strings = new ArrayList<>();
            for (GeoAreaLineSegs pMeanPrecip : meanAreaNodes) {
                /* Using the MPE Lat/Lon Grid, draw the point. */
                setScaleWidth(paintProps);
                setScaleHeight(paintProps);
                Coordinate StationPoint = new Coordinate();
                double xpos = -pMeanPrecip.interiorLon;
                double ypos = pMeanPrecip.interiorLat;

                StationPoint.x = xpos;
                StationPoint.y = ypos;
                double[] centerpixels = descriptor.worldToPixel(
                        new double[] { StationPoint.x, StationPoint.y });
                Coordinate valueCoor = new Coordinate(
                        centerpixels[0] - getScaleWidth() / 2,
                        centerpixels[1] - getScaleHeight() / 2);
                Coordinate labelCoor = new Coordinate(
                        centerpixels[0] - getScaleWidth() / 2,
                        (centerpixels[1] - 15) - getScaleHeight() / 2);
                RGB txtcolor = RGBColors.getRGBColor("WHITE");
                String area_id = pMeanPrecip.name;
                float val = pMeanPrecip.avg_val;
                DecimalFormat df = new DecimalFormat(
                        parameters.getFormatString());

                String valStr = df.format(val);

                if (ids) {
                    DrawableString string = new DrawableString(area_id,
                            txtcolor);
                    string.font = font;
                    string.setCoordinates(labelCoor.x, labelCoor.y);
                    strings.add(string);
                }
                if (labels) {
                    DrawableString string = new DrawableString(valStr,
                            txtcolor);
                    string.font = font;
                    string.setCoordinates(valueCoor.x, valueCoor.y);
                    strings.add(string);
                }
            }
            target.drawStrings(strings);
        }
    }

    @Override
    public String getName() {
        HydroDisplayManager dman = HydroDisplayManager.getInstance();
        boolean noData = false;
        if ((buf2 == null) || (gridGeometry == null)) {
            noData = true;
        }

        StringBuilder sb = new StringBuilder();
        sb.append(dman.getAccumInterval()
                + " hr Accumulated Best Estimate QPE Ending "
                + HydroConstants.DISPLAY_DATE_FORMAT.format(dman.getDataDate())
                + "z (in)");

        if (noData) {
            sb.append(" No Data Available");
        }
        return sb.toString();
    }

    /**
     * Set the width scalar
     *
     * @param props
     * @return
     */
    private void setScaleWidth(PaintProperties props) {
        double screenToWorldWidthRatio = props.getCanvasBounds().width
                / props.getView().getExtent().getWidth();
        scaleWidthValue = (ImageSize.MEDIUM.getWidth() / 2.0)
                / screenToWorldWidthRatio;
    }

    /**
     * get the scale width value
     *
     * @return
     */
    private double getScaleWidth() {
        return scaleWidthValue;
    }

    /**
     * Set the height scalar
     *
     * @param props
     * @return
     */
    private void setScaleHeight(PaintProperties props) {
        double screenToWorldHeightRatio = props.getCanvasBounds().height
                / props.getView().getExtent().getHeight();
        scaleHeightValue = (ImageSize.MEDIUM.getHeight() / 2.0)
                / screenToWorldHeightRatio;
    }

    /**
     * Get the scalar height
     *
     * @return
     */
    private double getScaleHeight() {
        return scaleHeightValue;
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        if (gridDisplay != null) {
            gridDisplay.project(descriptor.getGridGeometry());
        }
    }
}
