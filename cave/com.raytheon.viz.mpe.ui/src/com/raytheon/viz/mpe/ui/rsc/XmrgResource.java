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

import java.awt.Point;
import java.awt.Rectangle;
import java.io.IOException;
import java.nio.FloatBuffer;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.apache.commons.lang.StringUtils;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
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
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarLoc;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.DisplayMode;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Resource to display data from XMRG file format
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2008            randerso     Initial creation
 * Sep 5, 2012    15079    snaples      Updated interrogate method to handle values without rounding errors.
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class XmrgResource extends
        AbstractVizResource<XmrgResourceData, MapDescriptor> implements
        IMpeResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(XmrgResource.class);

    private static final SimpleDateFormat sdf;

    private static final SimpleDateFormat sds;

    private static final SimpleDateFormat sdx;
    static {
        sdf = new SimpleDateFormat("MMM dd yyyy HH");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        sds = new SimpleDateFormat("yyyyMMddHH");
        sds.setTimeZone(TimeZone.getTimeZone("GMT"));
        sdx = new SimpleDateFormat("MMddyyyyHH");
        sdx.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    private static final GeometryFactory gf = new GeometryFactory();

    private static final double MILLICVT = 25.4;

    private XmrgFile xmrg;

    private HRAPSubGrid subGrid;

    private GriddedImageDisplay2 gridDisplay;

    private ColorMapParameters parameters;

    private final float brightness = 1.0f;

    private final float contrast = 1.0f;

    private final boolean isInterpolated = true;

    private final MPEDisplayManager displayMgr;

    private GriddedContourDisplay contourDisplay;

    private GridGeometry2D gridGeometry;

    private FloatBuffer buf;

    private FloatBuffer cbuf;

    private final List<Colorvalue> colorSet;

    private final DisplayFieldData dataType;

    private short[] data;

    private final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private UnitConverter cvt;

    private UnitConverter cCvt;

    private DataMappingPreferences dmPref;

    public XmrgResource(MPEDisplayManager displayMgr,
            DisplayFieldData dataType, XmrgFile xmrg, List<Colorvalue> colorSet) {
        super(new XmrgResourceData(), new LoadProperties());

        this.displayMgr = displayMgr;
        this.dataType = dataType;
        this.xmrg = xmrg;
        this.colorSet = colorSet;
        setColorMapParams();
        loadData();
    }

    public XmrgResource(XmrgResourceData xmrgResourceData,
            MPEDisplayManager displayMgr, DisplayFieldData dataType,
            XmrgFile xmrg, List<Colorvalue> colorSet) {
        super(xmrgResourceData, new LoadProperties());
        this.displayMgr = displayMgr;
        this.dataType = dataType;
        this.xmrg = xmrg;
        this.colorSet = colorSet;
        setColorMapParams();
        loadData();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        String timeSlot = "";
        Date cd = displayMgr.getCurrentDate();
        Calendar cl = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cl.setTime(cd);
        if (xmrg == null) {
            timeSlot = sdf.format(cd);
            return timeSlot + "z  site="
                    + MPEDataManager.getInstance().getRFC() + "  "
                    + "No Data Available";
        }
        // Subtract 10 days from date to make sure that file header has valid
        // time. Some data files have epoch date in header, instead of a valid
        // date.
        cl.add(Calendar.SECOND, -(10 * 86400));
        if (xmrg.getHeader().getValidDate().before(cl.getTime())) {
            timeSlot = sdf.format(cd);
        } else {
            timeSlot = sdf.format(xmrg.getHeader().getValidDate());
        }
        return timeSlot + "z  site=" + MPEDataManager.getInstance().getRFC()
                + "  " + dataType.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) {
    }

    private void loadData() {
        String cv_use = dataType.getCv_use();
        if (displayMgr.getAccum_interval() > 1) {
            // this will accumulate all hours requested and display it
            // this holds current xmrg values
            short[] tempdata = null;
            Rectangle extent = null;
            try {
                extent = HRAPCoordinates.getHRAPCoordinates();
            } catch (Exception e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

            int datasz = extent.height * extent.width;

            // this is used to accumulate all hours
            data = new short[datasz];

            String dirname = appsDefaults.getToken(dataType.getDirToken());
            String fname = "";
            int numhours = displayMgr.getAccum_interval();
            int secsPerHr = 3600;
            buf = FloatBuffer.allocate(datasz);
            cbuf = FloatBuffer.allocate(datasz);
            Calendar cal1 = Calendar.getInstance((TimeZone.getTimeZone("GMT")));
            cal1.setTime(displayMgr.getCurrentDate());
            Calendar cal2 = Calendar.getInstance((TimeZone.getTimeZone("GMT")));
            cal2.setTime(cal1.getTime());

            try {

                for (int k = 0; k < numhours; k++) {
                    cal2.setTime(cal1.getTime());
                    cal2.add(Calendar.SECOND, -(k * secsPerHr));
                    String dtform = "";

                    // If first time through skip loading file
                    // as it has been loaded already
                    // if (k != 0) {
                    if (cv_use.equals("XMRG")) {
                        dtform = sdx.format(cal2.getTime());
                        fname = FileUtil.join(dirname, cv_use.toLowerCase()
                                + dtform + "z");
                    } else {
                        dtform = sds.format(cal2.getTime());
                        fname = FileUtil.join(dirname, cv_use + dtform + "z");
                    }
                    XmrgFile wmrg = null;

                    try {
                        wmrg = new XmrgFile(fname);
                        wmrg.load();
                    } catch (IOException io) {
                        System.out.println("XMRG file not found " + fname);
                        continue;
                    }

                    xmrg = wmrg;
                    extent = xmrg.getHrapExtent();
                    // }// End if (k != 0)

                    tempdata = xmrg.getData();

                    int c = 0;
                    for (short s : tempdata) {
                        if (data[c] < 0 && s >= 0) {
                            data[c] = s;
                        } else if (data[c] >= 0 && s > 0) {
                            data[c] += s;
                        }
                        c++;
                    }
                }
                float f = 0;
                // Don't convert missing data, checking to see if we are using
                // Temperature data
                String temps = "TEMP";
                int tempsval = cv_use.indexOf(temps);
                for (short s : data) {
                    if (cv_use.equalsIgnoreCase("Locspan")
                            || cv_use.equalsIgnoreCase("Locbias")
                            || cv_use.equalsIgnoreCase("Index")) {
                        // f = s;
                        f = (float) Math.floor(cvt.convert(s));
                    } else if (s < 0) {
                        if (s == -9999 || s == -999 || s == -99
                                || (s == -9 && tempsval == -1)) {
                            f = 0;
                        } else if (s == -8888 || s == -899) {
                            f = 1;
                        } else {
                            f = (float) Math.floor(cvt.convert(s));
                        }
                    } else {
                        if (s < 30 && s > 24) {
                            s = 26;
                        } else if (s > 0 && s <= 24) {
                            s = 0;
                        }
                        f = (float) Math.floor(cvt.convert(s));
                    }
                    float g = (float) cCvt.convert(s);
                    buf.put(f);
                    if (g < 0) {
                        g = 0;
                    }
                    cbuf.put(g);
                }
                buf.rewind();
                cbuf.rewind();

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
                subGrid = new HRAPSubGrid(extent);

                gridGeometry = MapUtil.getGridGeometry(subGrid);

                project(gridGeometry.getCoordinateReferenceSystem());

            } catch (Exception e) {
                xmrg = null;
                System.err.println("XMRG file not found " + fname);
            }
        } else {
            // this loads only the current working hour requested
            try {
                xmrg.load();
                data = xmrg.getData();
                buf = FloatBuffer.allocate(data.length);
                cbuf = FloatBuffer.allocate(data.length);
                // Don't convert missing data, checking to see if we are using
                // Temperature data
                String temps = "TEMP";
                int tempsval = cv_use.indexOf(temps);
                float f = 0;
                for (short s : data) {
                    if (cv_use.equalsIgnoreCase("Locspan")
                            || cv_use.equalsIgnoreCase("Locbias")
                            || cv_use.equalsIgnoreCase("Index")) {
                        // f = s;
                        f = (float) Math.floor(cvt.convert(s));
                    } else if (s < 0) {
                        if (s == -9999 || s == -999 || s == -99
                                || (s == -9 && tempsval == -1)) {
                            f = 0;
                        } else if (s == -8888 || s == -899) {
                            f = 1;
                        } else {
                            f = (float) Math.floor(cvt.convert(s));
                        }
                    } else {
                        if (s < 30 && s > 24) {
                            s = 26;
                        } else if (s > 0 && s <= 24) {
                            s = 0;
                        }
                        f = (float) Math.floor(cvt.convert(s));
                    }
                    float g = (float) cCvt.convert(s);
                    buf.put(f);
                    if (g < 0) {
                        g = 0;
                    }
                    cbuf.put(g);
                }
                buf.rewind();
                cbuf.rewind();
                Rectangle extent = xmrg.getHrapExtent();
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
                subGrid = new HRAPSubGrid(extent);

                gridGeometry = MapUtil.getGridGeometry(subGrid);

                project(gridGeometry.getCoordinateReferenceSystem());

            } catch (Exception e) {
                xmrg = null;
                System.err.println("XMRG file not found");
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (buf == null) {
            return;
        }

        Set<DisplayMode> mode = displayMgr.getDisplayMode();

        if (mode.contains(DisplayMode.Image)) {
            if (gridDisplay == null) {
                gridDisplay = new GriddedImageDisplay2(buf, gridGeometry, this);
            }

            GriddedImagePaintProperties giProps = new GriddedImagePaintProperties(
                    paintProps, brightness, contrast, isInterpolated);

            gridDisplay.paint(target, giProps);
        }

        if (mode.contains(DisplayMode.Contour)) {
            if (contourDisplay == null) {
                contourDisplay = new GriddedContourDisplay(descriptor,
                        gridGeometry, cbuf);

                contourDisplay.setColor(ColorUtil.WHITE);
                contourDisplay.setLineStyle(LineStyle.SOLID);
                contourDisplay.setOutlineWidth(1);
            }
            contourDisplay.paint(target, paintProps);
        }
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> Values = interrogate(coord);

        if (Values == null) {
            return "NO DATA";
        } else {
            return Values.get("Value").toString();
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
        if (xmrg == null) {
            return null;
        }

        Map<String, Object> values = new HashMap<String, Object>();

        try {
            Coordinate gridCell = coord.asGridCell(HRAP.getInstance()
                    .getGridGeometry(), PixelInCell.CELL_CENTER);

            Point p = new Point((int) gridCell.x, (int) gridCell.y);
            Coordinate l = coord.asLatLon();

            values.put("X", Integer.toString(p.x));
            values.put("Y", Integer.toString(p.y));
            values.put("Lon", String.format("%5.2f", l.x));
            values.put("Lat", String.format("%4.2f", l.y));
            values.put("Value", "-----");
            values.put("County", "Not Defined");
            values.put("Basin", "Not Defined");

            Rectangle extent = subGrid.getExtent();
            if (extent.contains(p)) {
                int x = p.x - extent.x;
                int y = extent.height - 1 - (p.y - extent.y);

                short s = data[(y * subGrid.getNx()) + x];

                String cv_use = dataType.getCv_use();
                if (cv_use.equalsIgnoreCase("INDEX")) {
                    values.put("Value", parameters.getLabels().get(s + 1)
                            .getText());
                } else {
                    String tmps = "TEMP";
                    int tempsval = dataType.getCv_use().indexOf(tmps);
                    float f = 0;
                    if (s < 0) {
                        if (s == -9999 || s == -999 || s == -99
                                || (s == -9 && tempsval == -1)) {
                            f = s;
                        } else if (s == -8888 || s == -899) {
                            f = s;
                        } else {
                            f = (float) parameters.getDataToDisplayConverter()
                                    .convert(s);
                        }
                    } else {
                        if (s < 30 && s > 24) {
                            s = 26;
                        } else if (s > 0 && s <= 24) {
                            s = 0;
                        }
                        if ((cv_use.equalsIgnoreCase("Locbias")
                                || cv_use.equalsIgnoreCase("height")
                                || cv_use.equalsIgnoreCase("locspan") || tempsval == -1)) {
                            f = (float) parameters.getDataToDisplayConverter()
                                    .convert(s);
                        } else {
                            f = (float) (s / 100 / MILLICVT);
                        }
                    }
                    String da = String.format("%2.2f", f);
                    values.put("Value", da);
                }
            }

            ISpatialQuery query = SpatialQueryFactory.create();

            com.vividsolutions.jts.geom.Point point = gf.createPoint(coord
                    .asLatLon());

            SpatialQueryResult[] results = query.query("county",
                    new String[] { "countyname" }, point, null, false,
                    SearchMode.WITHIN);

            String county = null;
            if ((results != null) && (results.length > 0)) {
                county = (String) results[0].attributes.get("countyname");
            }

            if (!StringUtils.isBlank(county)) {
                values.put("County", county);
            }

            results = query.query("basins", new String[] { "name" }, point,
                    null, false, SearchMode.WITHIN);

            String basin = null;
            if ((results != null) && (results.length > 0)) {
                basin = (String) results[0].attributes.get("name");
            }

            if (!StringUtils.isBlank(basin)) {
                values.put("Basin", basin);
            }
        } catch (Exception e) {
            throw new VizException("Error performing interrogation", e);
        }

        return values;
    }

    /**
     * Update the Xmrg Display.
     * 
     * @param reload
     *            Reread the data from the file if true
     */
    public void updateXmrg(boolean reload) {
        if (xmrg == null) {
            return;
        }
        String cv_use = dataType.getCv_use();
        try {
            cvt = parameters.getDataToImageConverter();
            if (reload || (data == null)) {
                // this.recycle();
                // this.init(displayMgr.getDisplayPane().getTarget());
                data = xmrg.getData();
            }
            buf = FloatBuffer.allocate(data.length);
            // Don't convert missing data, checking to see if we are using
            // Temperature data
            String temps = "TEMP";
            int tempsval = dataType.getCv_use().indexOf(temps);
            float f = 0;
            for (short s : data) {
                if (cv_use.equalsIgnoreCase("Locspan")
                        || cv_use.equalsIgnoreCase("Locbias")
                        || cv_use.equalsIgnoreCase("Index")) {
                    // f = s;
                    f = (float) Math.floor(cvt.convert(s));
                } else if (s < 0) {
                    if (s == -9999 || s == -999 || s == -99
                            || (s == -9 && tempsval == -1)) {
                        f = 0;
                    } else if (s == -8888 || s == -899) {
                        f = 1;
                    } else {
                        f = (float) Math.floor(cvt.convert(s));
                    }
                } else {
                    if (s < 30 && s > 24) {
                        s = 26;
                    } else if (s > 0 && s <= 24) {
                        s = 0;
                    }

                    f = (float) Math.floor(cvt.convert(s));
                }
                float g = (float) cCvt.convert(s);
                buf.put(f);
                if (g < 0) {
                    g = 0;
                }
                cbuf.put(g);
            }
            buf.rewind();
            cbuf.rewind();
            Rectangle extent = xmrg.getHrapExtent();
            if (extent == null) {
                xmrg = null;
                return;
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
            subGrid = new HRAPSubGrid(extent);

            gridGeometry = MapUtil.getGridGeometry(subGrid);

            project(gridGeometry.getCoordinateReferenceSystem());

            issueRefresh();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * @return the xmrg
     */
    public XmrgFile getXmrgFile() {
        return xmrg;
    }

    /**
     * @return the data
     */
    public short[] getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(short[] dta) {
        data = null;
        data = dta;
    }

    private void setColorMapParams() {
        ColorMap colorMap = new ColorMap(colorSet.size());
        colorMap.setName(dataType.getCv_use());
        dmPref = new DataMappingPreferences();
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

        Unit<?> displayUnit = Unit.ONE;
        Unit<?> dataUnit = Unit.ONE;

        switch (dataType) {
        case Locbias:
            displayUnit = Unit.ONE;
            dataUnit = Unit.ONE.divide(100);
            break;

        case Height:
            displayUnit = NonSI.FOOT;
            dataUnit = SI.METER;
            break;

        case Index:
            int j = 2;
            for (MPERadarLoc radar : MPEDataManager.getInstance().getRadars()) {
                dmPref.getEntries().get(j++).setLabel(radar.getId());
            }
            while (j < dmPref.getEntries().size()) {
                dmPref.getEntries().get(j++).setLabel("");
            }
        case Locspan:
            dmPref.getEntries().get(1).setLabel("mis");
            displayUnit = Unit.ONE;
            dataUnit = Unit.ONE;
            break;

        case Prism:
            displayUnit = NonSI.INCH;
            dataUnit = SI.MILLIMETER;
            parameters.setFormatString("0.00");
            break;

        case mintempPrism:
        case maxtempPrism:
            displayUnit = NonSI.FAHRENHEIT;
            dataUnit = NonSI.FAHRENHEIT.divide(10);
            break;

        case srMosaic:
        case sgMosaic:
        case srgMosaic:
        case satPre:
        case lsatPre:
            displayUnit = NonSI.INCH;
            dataUnit = SI.MILLIMETER.divide(100);
            parameters.setFormatString("0.00");
            break;

        default:
            displayUnit = NonSI.INCH;
            dataUnit = SI.MILLIMETER.divide(100);
            parameters.setFormatString("0.00");

        }

        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setDataUnit(dataUnit);

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);
        cvt = parameters.getDataToImageConverter();
        cCvt = parameters.getDataToDisplayConverter();
        cmc.setColorMapParameters(parameters);

    }

    public MPEDisplayManager getDisplayMgr() {
        return displayMgr;
    }

    public DisplayFieldData getDataType() {
        return dataType;
    }
}
