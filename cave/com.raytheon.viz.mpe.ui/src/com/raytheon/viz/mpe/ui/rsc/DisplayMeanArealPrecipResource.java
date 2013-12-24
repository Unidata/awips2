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
package com.raytheon.viz.mpe.ui.rsc;

import java.awt.Rectangle;
import java.io.IOException;
import java.nio.FloatBuffer;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Set;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil.GeoAreaLineSegs;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.DisplayMode;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2009            snaples     Initial creation
 * Dec 23, 2013  16329     snaples     Added target assignment to paintInternal().
 * 
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class DisplayMeanArealPrecipResource extends
        AbstractVizResource<DisplayMeanArealPrecipResourceData, MapDescriptor>
        implements IMpeResource {

    MPEDisplayManager displayMgr = null;

    private String area_type = "";

    private int xor = 0;

    private int yor = 0;

    private int max_columns = 0;

    private int max_rows = 0;

    private int accumInterval;

    private DisplayFieldData displayField;

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

    public DisplayMeanArealPrecipResource(MPEDisplayManager displayMgr,
            String boundary_type, DisplayFieldData displayField,
            int accumInterval) {
        super(new DisplayMeanArealPrecipResourceData(), new LoadProperties());
        this.displayMgr = displayMgr;
        this.area_type = boundary_type;
        this.displayField = displayField;
        this.accumInterval = accumInterval;
    }

    IGraphicsTarget target;

    private GriddedImageDisplay2 gridDisplay;

    private GriddedContourDisplay contourDisplay;

    int hed;

    int time_pos;

    int display_flag;

    int first = 1;

    public static boolean ids = false;

    public static boolean vals = false;

    GeometryFactory jtsGeometryFactory;

    Rectangle extent;

    HRAPSubGrid subGrid;

    GridGeometry2D gridGeometry;

    private FloatBuffer buf;

    private FloatBuffer buf2;

    static final float DEFAULT_COVERAGE = 0.80f;

    private String min_coverage_token = "whfs_min_area_covered";

    AppsDefaults appsDefaults = AppsDefaults.getInstance();

    short[] data;

    float min_coverage = 0;

    private double scaleWidthValue = 0.0;

    private double scaleHeightValue = 0.0;

    private UnitConverter cvt;

    private static final SimpleDateFormat sds;

    private static final SimpleDateFormat sxf;

    static {
        sds = new SimpleDateFormat("yyyyMMddHH");
        sds.setTimeZone(TimeZone.getTimeZone("GMT"));
        sxf = new SimpleDateFormat("MMddyyyyHH");
        sxf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    private ArrayList<GeoAreaLineSegs> meanAreaNodes = new ArrayList<GeoAreaLineSegs>();

    private void compute_mean_areal_precip(FloatBuffer fbuf,
            ArrayList<GeoAreaLineSegs> meanAreaNodes, int xor, int yor,
            int max_columns, int max_rows) {
        ColorMapParameters parameters = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
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
        int corr = extent.height - 1;

        if (meanAreaNodes == null || meanAreaNodes.size() == 0) {
            System.out.println("\nIn routine 'compute_mean_areal_precip':\n"
                    + "No lineseg information exists.  Cannot\n"
                    + "calculate mean areal precipitation.\n");
            return;
        }

        /* Initialize the list. */
        for (GeoAreaLineSegs pNode : meanAreaNodes) {
            /* initialize */
            miss_cnt = total_cnt = val_cnt = 0;
            cur_max = 0.0f;
            cur_min = 0.0f;
            sum = 0.0f;

            /* loop on the number of rows for this area */
            for (i = 0; i < pNode.numrows; ++i) {
                total_cnt += pNode.end_cols[i] - pNode.beg_cols[i];
                row = (int) pNode.rows[i] - yor;

                /* loop on the number of columns in each row */
                for (jcol = (int) pNode.beg_cols[i]; jcol <= pNode.end_cols[i]; jcol++) {
                    /*
                     * sum the value and increment the counts. note that the
                     * array index method must match the method by which the
                     * grid was originally loaded
                     */

                    col = (jcol - xor);

                    /*-----------------------------------------------*/
                    /* check that box is within site's area */
                    /* if not, return -1. */
                    /*-----------------------------------------------*/

                    if (row > max_rows - 1 || col > max_columns - 1 || row < 0
                            || col < 0) {
                        ++miss_cnt;
                    } else {
                        // compute the offset into the data buffer
                        int offset = (((corr - row) * extent.width) + col);
                        if (offset >= buf.capacity() || offset < 0) {
                            continue;
                        }
                        // value converted to real data value, from image value.
                        raw_val = (float) parameters
                                .getImageToDisplayConverter().convert(
                                        buf.get(offset));

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
                row = (int) (pNode.rows[i] - yor);
                for (jcol = (int) pNode.beg_cols[i]; jcol <= pNode.end_cols[i]; jcol++) {
                    col = (jcol - xor);
                    /*-----------------------------------------------*/
                    /* check that box is within site's area */
                    /* if not, return -1. */
                    /*-----------------------------------------------*/

                    if (row > max_rows - 1 || col > max_columns - 1 || row < 0
                            || col < 0) {
                        continue;
                    }
                    // compute the offset into the buffer to store mean areal
                    // values.
                    int offset = (((corr - row) * extent.width) + col);
                    if (offset >= buf.capacity() || offset < 0) {
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

    @Override
    protected void paintInternal(IGraphicsTarget aTarget,
            PaintProperties paintProps) throws VizException {

        if (buf2 == null) {
            return;
        }

        Set<DisplayMode> mode = displayMgr.getDisplayMode();
        
        target = aTarget;

        if (mode.contains(DisplayMode.Image)) {
            if (gridDisplay == null) {
                gridDisplay = new GriddedImageDisplay2(buf2, gridGeometry, this);
            }

            gridDisplay.paint(target, paintProps);
        }

        if (mode.contains(DisplayMode.Contour)) {
            if (contourDisplay == null) {
                contourDisplay = new GriddedContourDisplay(descriptor,
                        gridGeometry, buf2);

                contourDisplay.setColor(ColorUtil.WHITE);
                contourDisplay.setLineStyle(LineStyle.SOLID);
                contourDisplay.setOutlineWidth(1);
            }
            contourDisplay.paint(target, paintProps);
        }

        if (ids == true || vals == true) {
            ColorMapParameters parameters = getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            for (GeoAreaLineSegs pMeanPrecip : meanAreaNodes) {

                /* Using the MPE Lat/Lon Grid, draw the point. */
                setScaleWidth(paintProps);
                setScaleHeight(paintProps);
                Coordinate StationPoint = new Coordinate();
                double xpos = -pMeanPrecip.interiorLon;
                double ypos = pMeanPrecip.interiorLat;
                IExtent screenExtent = paintProps.getView().getExtent();
                double scale = (screenExtent.getHeight() / paintProps
                        .getCanvasBounds().height);

                StationPoint.x = xpos;
                StationPoint.y = ypos;
                double[] centerpixels = descriptor.worldToPixel(new double[] {
                        StationPoint.x, StationPoint.y });
                Coordinate valueCoor = new Coordinate(centerpixels[0]
                        - getScaleWidth() / 2 * scale, centerpixels[1]
                        - getScaleHeight() / 3 * scale);
                Coordinate labelCoor = new Coordinate(centerpixels[0]
                        - getScaleWidth() / 2 * scale,
                        (centerpixels[1] - 10 * scale) - getScaleHeight() / 3
                                * scale);
                RGB txtcolor = RGBColors.getRGBColor("WHITE");
                String area_id = pMeanPrecip.name;
                float val = pMeanPrecip.avg_val;
                DecimalFormat df = new DecimalFormat(
                        parameters.getFormatString());

                String valStr = df.format(val);
                DrawableString string = new DrawableString("", txtcolor);
                string.font = font;

                if (ids == true) {
                    try {
                        string.setText(area_id, txtcolor);
                        string.setCoordinates(labelCoor.x, labelCoor.y);
                        string.horizontalAlignment = HorizontalAlignment.LEFT;
                        string.verticallAlignment = VerticalAlignment.TOP;
                        target.drawStrings(string);
                    } catch (VizException e1) {
                        // TODO Auto-generated catch block
                        e1.printStackTrace();
                    }
                }
                if (vals == true) {
                    try {
                        string.setText(valStr, txtcolor);
                        string.setCoordinates(valueCoor.x, valueCoor.y);
                        string.horizontalAlignment = HorizontalAlignment.LEFT;
                        string.verticallAlignment = VerticalAlignment.TOP;
                        target.drawStrings(string);
                    } catch (VizException e1) {
                        // TODO Auto-generated catch block
                        e1.printStackTrace();
                    }
                }

            }
        }
    }

    @Override
    protected void disposeInternal() {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }

        if (contourDisplay != null) {
            contourDisplay.dispose();
            contourDisplay = null;
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        getCapability(ColorMapCapability.class)
                .setColorMapParameters(
                        MPEDisplayManager.createColorMap(displayField
                                .getCv_use(), accumInterval,
                                MPEFieldResourceData
                                        .getDataUnitsForField(displayField),
                                MPEFieldResourceData
                                        .getDisplayUnitsForField(displayField)));

        loadData();
    }

    private void loadData() {
        min_coverage = Float.parseFloat(appsDefaults
                .getToken(min_coverage_token));
        ColorMapParameters parameters = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        cvt = parameters.getDataToImageConverter();

        this.readData();
        meanAreaNodes = (ArrayList<GeoAreaLineSegs>) GeoUtil.getInstance()
                .getGeoAreaLinesegs(area_type);
        compute_mean_areal_precip(buf, meanAreaNodes, xor, yor, max_columns,
                max_rows);
        try {
            subGrid = new HRAPSubGrid(extent);
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        gridGeometry = MapUtil.getGridGeometry(subGrid);

        try {
            project(gridGeometry.getCoordinateReferenceSystem());
        } catch (InvalidGridGeometryException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        return DisplayFieldData.multiHour.toString();
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
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }

        if (contourDisplay != null) {
            contourDisplay.dispose();
            contourDisplay = null;
        }
    }

    private void readData() {

        // this will accumulate all hours requested and display it
        // this holds current xmrg values
        short[] tempdata = null;
        String cv_use = displayMgr.getDisplayFieldType().getCv_use();
        String dirname = appsDefaults.getToken(displayMgr.getDisplayFieldType()
                .getDirToken());
        String fname = "";
        String dtform = "";
        String use = "";
        Calendar cal1 = Calendar.getInstance((TimeZone.getTimeZone("GMT")));
        cal1.setTime(displayMgr.getCurrentEditDate());

        if (cv_use.equals("XMRG")) {
            dtform = sxf.format(cal1.getTime());
            use = cv_use.toLowerCase();
        } else {
            dtform = sds.format(cal1.getTime());
            use = cv_use;
        }
        fname = FileUtil.join(dirname, use + dtform + "z");
        XmrgFile xmrg = new XmrgFile(fname);
        try {
            xmrg.load();
        } catch (IOException e1) {
            e1.printStackTrace();
        }
        extent = xmrg.getHrapExtent();
        int datasz = extent.height * extent.width;
        xor = extent.x;
        yor = extent.y;
        max_columns = extent.width;
        max_rows = extent.height;

        // this is used to accumulate all hours
        data = new short[datasz];

        xmrg = null;

        int numhours = accumInterval;
        int secsPerHr = 3600;
        buf = FloatBuffer.allocate(datasz);
        buf2 = FloatBuffer.allocate(buf.capacity());
        Calendar cal2 = Calendar.getInstance((TimeZone.getTimeZone("GMT")));
        cal2.setTime(cal1.getTime());

        try {

            for (int k = 0; k < numhours; k++) {
                cal2.setTime(cal1.getTime());
                cal2.add(Calendar.SECOND, -(k * secsPerHr));
                if (cv_use.equals("XMRG")) {
                    dtform = sxf.format(cal2.getTime());
                } else {
                    dtform = sds.format(cal2.getTime());
                }
                fname = FileUtil.join(dirname, use + dtform + "z");
                XmrgFile wmrg = null;

                try {
                    wmrg = new XmrgFile(fname);
                    wmrg.load();
                } catch (IOException io) {
                    System.out.println("XMRG file not found " + fname);
                    continue;
                }

                xmrg = wmrg;

                tempdata = xmrg.getData();

                int c = 0;
                for (@SuppressWarnings("unused")
                short s : tempdata) {
                    if (data[c] < 0 && tempdata[c] >= 0) {
                        data[c] = tempdata[c];
                    } else if (data[c] >= 0 && tempdata[c] > 0) {
                        data[c] += tempdata[c];
                    }
                    c++;
                }
            }
            // Don't convert missing data, checking to see if we are using
            // Temperature data
            float f = 0;
            String temps = "TEMP";
            for (short s : data) {
                if (s < 0) {
                    if (s == -9999 || s == -999 || s == -99
                            || (s == -9 && cv_use.indexOf(temps) == -1)) {
                        f = 0;
                    } else if (s == -8888 || s == -899) {
                        f = 1;
                    } else {
                        f = (float) Math.floor(cvt.convert(s));
                    }
                } else {
                    f = (float) Math.floor(cvt.convert(s));
                }
                buf.put(f);
            }
            buf.rewind();

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
        } catch (Exception e) {
            xmrg = null;
            System.err.println("XMRG file not found " + fname);
        }
    }
}
