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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.measure.Unit;

import org.eclipse.swt.graphics.RGB;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.index.strtree.STRtree;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEFontFactory;
import com.raytheon.viz.mpe.ui.actions.DrawDQCStations;
import com.raytheon.viz.mpe.ui.actions.OtherPrecipOptions;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.DailyQcUtils.Stn;

import systems.uom.common.USCustomary;

/**
 * MPEMultiple point resource.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 24, 2008 1748       snaples     Initial creation
 * Jul 24, 2014 3429       mapeters    Updated deprecated drawLine() calls.
 * Aug 11, 2014 3504       mapeters    Replaced deprecated IODataPreparer
 *                                      instances with IRenderedImageCallback
 *                                      and removed unused modifyStation() method.
 * Nov 26, 2014 16889      snaples     Daily QC does not display SNOTEL data.
 * Mar 08, 2017 6149       mduff       Performance fixes for painting.
 * Jun 21, 2017 6149       mduff       Fix circle size.
 * Aug 11, 2017 6148       bkowal      Cleanup.
 * May 10, 2018 7131       mduff       Changes made for DQC Dialog updates.
 * </pre>
 * 
 * @author snaples
 */

public class PointPrecipPlotResource extends
        HydroPointResource<PointPrecipResourceData> implements IMpeResource {

    private static final String[] COLOR_MAP_A = { "Aquamarine", "OrangeRed",
            "Orange", "Yellow", "VioletRed", "SpringGreen4", "Green3", "Grey",
            "White" };

    private static final String[] COLOR_MAP_N = { "Grey", "Grey", "Blue",
            "Aquamarine", "LightGreen", "DarkGreen", "Violet", "Purple", "Blue",
            "Blue", "Yellow", "Yellow", "Yellow2", "VioletRed", "Red",
            "White" };

    private static final RGB[] RGB_COLOR_MAP_A = new RGB[COLOR_MAP_A.length];

    private static final RGB[] RGB_COLOR_MAP_N = new RGB[COLOR_MAP_N.length];

    private static final RGB WHITE = new RGB(255, 255, 255);

    private static final int TEXT_WIDTH_PADDING = 9;

    private static final double VERTICAL_SPACING_FACTOR = 1.5;

    private static final RGB gageColor = WHITE;

    private Map<String, Station> dataMap = null;

    private STRtree strTree = null;

    private Coordinate selectedCoordinate;

    static {
        for (int i = 0; i < COLOR_MAP_A.length; i++) {
            RGB_COLOR_MAP_A[i] = RGBColors.getRGBColor(COLOR_MAP_A[i]);
        }

        for (int i = 0; i < COLOR_MAP_N.length; i++) {
            RGB_COLOR_MAP_N[i] = RGBColors.getRGBColor(COLOR_MAP_N[i]);
        }
    }

    private MPEFontFactory fontFactory;

    private final DecimalFormat df = new DecimalFormat();

    private double scaleWidthValue = 0.0;

    private double scaleHeightValue = 0.0;

    private Pdata pdata[];

    private int time_pos = 0;

    private Map<String, Stn> pdataMap;

    private final DailyQcUtils dqc = DailyQcUtils.getInstance();

    private List<DrawableString> drawableStrings = new ArrayList<>();

    private List<DrawableCircle> drawableCircles = new ArrayList<>();

    private DrawableLine[] lines = new DrawableLine[typename.length];

    private double circleSize;

    private RGB circleColor;

    /**
     * Constructor.
     * 
     * @param resourceData
     * @param props
     */
    public PointPrecipPlotResource(PointPrecipResourceData resourceData,
            LoadProperties props) {
        super(resourceData, props);
        pdata = DailyQcUtils.pdata;
        df.setMaximumFractionDigits(2);
        df.setMaximumIntegerDigits(4);
    }

    private void addPoints() {

        dataMap = new HashMap<>();
        pdataMap = new HashMap<>();
        strTree = new STRtree();

        if (!DailyQcUtils.precip_stations.isEmpty()) {
            int i = 0;
            for (Station gageData : DailyQcUtils.precip_stations) {
                Coordinate xy = new Coordinate(gageData.lon, gageData.lat);
                String pm = gageData.parm;
                StringBuilder kv = new StringBuilder(String.valueOf(xy.x));
                kv.append(":");
                kv.append(String.valueOf(xy.y));
                kv.append(":");
                kv.append(pm);
                dataMap.put(kv.toString(), gageData);
                pdataMap.put(kv.toString(),
                        pdata[DailyQcUtils.pcpn_day].stn[i]);

                /* Create a small envelope around the point */
                Coordinate p1 = new Coordinate(xy.x + .02, xy.y + .02);
                Coordinate p2 = new Coordinate(xy.x - .02, xy.y - .02);
                Envelope env = new Envelope(p1, p2);
                List<Object> data = new ArrayList<>();
                data.add(xy);
                data.add("STATION: " + gageData.hb5 + " VALUE: "
                        + pdata[DailyQcUtils.pcpn_day].stn[i].frain[time_pos].data);
                strTree.insert(env, data);
                i++;
            }
        }
    }

    /**
     * Gets the pixel extent of the rectangle
     * 
     * @param c
     * @return
     */
    private PixelExtent getPixelExtent(Coordinate c) {
        double[] pixels = descriptor.worldToPixel(new double[] { c.x, c.y });
        Coordinate[] coors = new Coordinate[4];
        coors[0] = new Coordinate(
                (pixels[0] - this.getScaleWidth()) - (this.getScaleWidth() / 2),
                (pixels[1] - this.getScaleHeight())
                        - (this.getScaleHeight() / 2));
        coors[1] = new Coordinate(
                (pixels[0] + this.getScaleWidth()) - (this.getScaleWidth() / 2),
                (pixels[1] - this.getScaleHeight())
                        - (this.getScaleHeight() / 2));
        coors[2] = new Coordinate(
                (pixels[0] + this.getScaleWidth()) - (this.getScaleWidth() / 2),
                (pixels[1] + this.getScaleHeight())
                        - (this.getScaleHeight() / 2));
        coors[3] = new Coordinate(
                (pixels[0] - this.getScaleWidth()) - (this.getScaleWidth() / 2),
                (pixels[1] + this.getScaleHeight())
                        - (this.getScaleHeight() / 2));
        return new PixelExtent(coors);
    }

    private void drawPlotInfo(Coordinate c, String key, Station station,
            IGraphicsTarget target, PaintProperties paintProps, IFont font)
            throws VizException {
        MPEDisplayManager displayMan = MPEDisplayManager.getCurrent();
        if ((displayMan.isQpf()) && (DailyQcUtils.points_flag == 1)
                && (displayMan.getQcPrecipDialog().isOpen())) {
            int type = DailyQcUtils.plot_view;
            if (type == 0) {
                return;
            }
            int dcmode = OtherPrecipOptions.dcmode;
            int tcmode = OtherPrecipOptions.tcmode;
            int gage_char[] = DailyQcUtils.gage_char;

            if ((station.elev >= 0)
                    && (station.elev < dqc.getPointElevationFilterValue())) {
                return;
            }

            if ((tcmode == 1) && (pdataMap.get(key).tcons == 1)) {
                return;
            }

            if ((tcmode == 0) && (pdataMap.get(key).tcons == -1)) {
                return;
            }

            if ((dcmode == 0) && (pdataMap.get(key).scons[time_pos] == -1)) {
                return;
            }

            if ((dcmode == 1) && (pdataMap.get(key).scons[time_pos] == 1)) {
                return;
            }

            if ((station.tip == 0) && (gage_char[0] == -1)) {
                return;
            }

            if ((station.tip == 1) && (gage_char[1] == -1)) {
                return;
            }

            int m = 0;
            int dmvalue = dqc.dmvalue;
            int tsmax = DailyQcUtils.tsmax;
            boolean frzlvl_flag = dqc.frzlvl_flag;
            int find_station_flag = dqc.find_station_flag;
            String mbuf = "";
            String tbuf = "";
            String val = "";

            if (MPEDisplayManager.pcpn_time_step == 0) {
                time_pos = DailyQcUtils.pcpn_time;
            } else {
                time_pos = 4;
            }

            for (m = 0; m < tsmax; m++) {
                if (station.parm.substring(3, 5)
                        .equalsIgnoreCase(DailyQcUtils.ts[m].abr)
                        && (DailyQcUtils.dflag[m + 1] == 1)) {
                    break;
                }
            }

            if (m == tsmax) {
                return;
            }

            for (m = 0; m < 9; m++) {
                if ((m == pdataMap.get(key).frain[time_pos].qual)
                        && (DailyQcUtils.qflag[m] == 1)) {
                    break;
                } else if ((m == 7) && (DailyQcUtils.qflag[7] == 1)
                        && (pdataMap.get(key).frain[time_pos].data == -99)
                        && (pdataMap.get(key).frain[time_pos].qual == -99)) {
                    break;
                }
            }

            if (m == 9) {
                return;
            }

            /* locate station in data stream */
            if (type == 4 || type == 5) {
                if (pdata[DailyQcUtils.pcpn_day].used[time_pos] == 0
                        && (pdata[DailyQcUtils.pcpn_day].level == 0)) {
                    return;
                }
                if (pdataMap.get(key).frain[time_pos].data < dqc
                        .getPrecipFilterValue()
                        && (pdataMap.get(key).frain[time_pos].data != -99)
                        && (pdataMap.get(key).frain[time_pos].qual != -99)) {
                    return;
                }

                if (pdataMap.get(key).frain[time_pos].data > dqc
                        .getPrecipReverseFilterValue()
                        && (pdataMap.get(key).frain[time_pos].data < 20.00)) {
                    return;
                }
            }

            double[] centerpixels = descriptor
                    .worldToPixel(new double[] { c.x, c.y });

            DrawableCircle circle = new DrawableCircle();
            circle.basics.color = circleColor;
            circle.setCoordinates(centerpixels[0], centerpixels[1]);
            double pixelRatio = paintProps.getView().getExtent().getWidth()
                    / paintProps.getCanvasBounds().width;
            circle.radius = circleSize * pixelRatio;
            circle.filled = true;
            drawableCircles.add(circle);

            tbuf = "";
            if (type == 1) {
                tbuf = station.hb5;
            }

            else if (type == 2) {
                tbuf = station.parm.substring(3, 5);
            }

            else if (type == 3) {
                tbuf = station.name;

            } else if (type == 4) {
                if ((pdata[DailyQcUtils.pcpn_day].used[time_pos] == 0)
                        && (pdata[DailyQcUtils.pcpn_day].level == 0)) {
                    return;

                }
                if (pdataMap.get(key).frain[time_pos].data == -2) {
                    return;

                }

                /* if point data is missing, use character 'm' */

                mbuf = "";
                if ((pdataMap.get(key).frain[time_pos].data == -99)
                        && (pdataMap.get(key).frain[time_pos].qual == -99)) {
                    mbuf = "m";

                } else {
                    mbuf = String.format("%5.2f",
                            pdataMap.get(key).frain[time_pos].data);

                }
                tbuf = mbuf;

            } else if (type == 5) {
                if ((pdata[DailyQcUtils.pcpn_day].used[time_pos] == 0)
                        && (pdata[DailyQcUtils.pcpn_day].level == 0)) {
                    return;

                }
                if (pdataMap.get(key).frain[time_pos].data == -2) {
                    return;

                }
                mbuf = String.format("%5.2f",
                        pdataMap.get(key).frain[time_pos].stddev);
                tbuf = mbuf;
            }
            if (m == 9) {
                m = 7;
            }

            RGB color = RGB_COLOR_MAP_A[m];

            int xadd = station.xadd;
            int yadd = station.yadd;

            IExtent screenExtent = paintProps.getView().getExtent();
            double scale = (screenExtent.getHeight()
                    / paintProps.getCanvasBounds().height);
            DrawableString dstr = new DrawableString("0", WHITE);
            dstr.font = font;
            double textHeight = target.getStringsBounds(dstr).getHeight()
                    * scale;
            Coordinate valCoor = new Coordinate(
                    centerpixels[0] + (this.getScaleWidth() / 3),
                    centerpixels[1] - this.getScaleHeight());

            dstr.setText(tbuf, color);
            dstr.horizontalAlignment = HorizontalAlignment.LEFT;
            dstr.verticallAlignment = VerticalAlignment.TOP;

            double textWidth = target.getStringsBounds(dstr).getWidth();
            double scaledTextWidth = (textWidth + TEXT_WIDTH_PADDING) * scale;
            double textX = valCoor.x + (xadd * scaledTextWidth);
            double textY = valCoor.y
                    + ((yadd) * VERTICAL_SPACING_FACTOR * textHeight)
                    + (textHeight / 2);

            dstr.setCoordinates(textX, textY);
            drawableStrings.add(dstr);

            if (0 == find_station_flag) {
                find_station_flag = -1;
            }

            if (pdataMap.get(key).snoflag[time_pos] != 0) {
                color = RGBColors.getRGBColor(COLOR_MAP_A[1]);
            }

            if (pdataMap.get(key).sflag[time_pos] == 1) {
                color = RGBColors.getRGBColor(COLOR_MAP_A[0]);
            }

            int mm = 0;
            if ((frzlvl_flag) && (station.tip == 0)
                    && ((pdataMap.get(key).frain[time_pos].estimate > .005)
                            || (pdataMap
                                    .get(key).frain[time_pos].data > .005))) {
                if (time_pos == 4) {
                    for (mm = 0; mm < 4; mm++) {

                        if ((pdataMap.get(key).frzlvl[mm] < -98)
                                || (station.elev < 0)) {
                            continue;
                        }

                        if ((pdataMap.get(key).frzlvl[mm]
                                - dmvalue) < station.elev) {
                            break;
                        }

                    }

                    if (mm == 4) {
                        return;
                    }
                } else {
                    if ((pdataMap.get(key).frzlvl[time_pos] < -98)
                            || (station.elev < 0)) {
                        return;
                    }

                    if ((pdataMap.get(key).frzlvl[time_pos]
                            - dmvalue) >= station.elev) {
                        return;
                    }
                }

                /* XSetForeground(display,gc,amap[1]); */
                color = RGB_COLOR_MAP_A[1];

                /* XDrawLine(display,pix,gc,xc,yc,xc+text_width,yc); */
                // mDrawLine(M_EXPOSE, map_number, xc, yc, xc + text_width, yc);
                Coordinate stageCoor = new Coordinate(
                        centerpixels[0] + (this.getScaleWidth() / 3),
                        centerpixels[1] + this.getScaleHeight());
                val = df.format(pdataMap.get(key).frain[time_pos].data);
                val = "m";

                // draw the value
                dstr = new DrawableString(val, gageColor);
                dstr.setCoordinates(stageCoor.x, stageCoor.y);
                dstr.horizontalAlignment = HorizontalAlignment.LEFT;
                drawableStrings.add(dstr);

                // draw the ID
                Coordinate idCoor = new Coordinate(
                        centerpixels[0] + (this.getScaleWidth() / 3),
                        centerpixels[1] - this.getScaleHeight());
                dstr = new DrawableString(station.hb5, gageColor);
                dstr.setCoordinates(idCoor.x, idCoor.y);

                dstr.horizontalAlignment = HorizontalAlignment.LEFT;
                drawableStrings.add(dstr);

                DrawableCircle circle2 = new DrawableCircle();
                circle2.basics.color = color;
                circle2.setCoordinates(centerpixels[0], centerpixels[1]);
                circle2.radius = circleSize;
                circle2.filled = true;
                drawableCircles.add(circle2);
            }
        }
    }

    /**
     * Get the contrasting color to specified color
     * 
     * @param RGB
     * 
     * @return RGB contrast color
     */
    public static RGB getContrast(RGB rgb) {
        RGB xc;
        int r = rgb.red;
        int g = rgb.green;
        int b = rgb.blue;

        // If color is near black set the contrast to white
        if (((r + g + b) / 3) == 0) {
            xc = WHITE;
        } else {
            if (rgb.green <= 127) {
                g = 255;
            } else {
                g = 10;
            }

            if (rgb.blue <= 127) {
                b = 255;
            } else {
                b = 10;
            }
            xc = new RGB(r, g, b);
        }
        return xc;
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
        scaleWidthValue = (IMAGE_WIDTH / 2.0) / screenToWorldWidthRatio;
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
     * Get the scalar height
     * 
     * @return
     */
    private double getScaleHeight() {
        return scaleHeightValue;
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
        scaleHeightValue = (IMAGE_HEIGHT / 2.0) / screenToWorldHeightRatio;
    }

    /**
     * Paint method called to display this resource.
     * 
     * @param target
     *            The IGraphicsTarget
     * @param paintProps
     *            The Paint Properties
     * @throws VizException
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        this.circleSize = resourceData.getDpi().x / 34;
        MPEDisplayManager displayMgr = getResourceData().getMPEDisplayManager();
        if (!displayMgr.isQpf() || DailyQcUtils.points_flag != 1) {
            return;
        }

        // Fonts are shared and cached, no need to init or dispose
        IFont font = fontFactory.getMPEFont(MPEDisplayManager.getFontId());

        drawableStrings.clear();
        drawableCircles.clear();
        setScaleWidth(paintProps);
        setScaleHeight(paintProps);

        for (Entry<String, Station> entry : dataMap.entrySet()) {
            String key = entry.getKey();
            Station station = entry.getValue();
            Coordinate coord = new Coordinate(station.lon, station.lat);

            double[] pixel = descriptor
                    .worldToPixel(new double[] { coord.x, coord.y });

            // Skip gages not visible on the map
            if (paintProps.getView().getExtent().contains(pixel)) {
                drawPlotInfo(coord, key, station, target, paintProps, font);
                if (getSelectedCoordinate() != null) {
                    Envelope env = new Envelope(getSelectedCoordinate());
                    List<?> elements = strTree.query(env);
                    if (!elements.isEmpty()) {

                        /*
                         * element 0 = Coordinate, 1 = inspectString
                         */
                        ArrayList<?> data = (ArrayList<?>) elements.get(0);
                        PixelExtent pe = this
                                .getPixelExtent((Coordinate) data.get(0));
                        target.drawRect(pe, HydroConstants.SQUARE_COLOR, 2, 1);
                    }
                }
            }
        }

        target.clearClippingPlane();
        drawQCLegend(target, paintProps, font);
        target.drawStrings(drawableStrings);
        target.drawCircle(drawableCircles.toArray(new DrawableCircle[0]));
        target.drawLine(lines);
        target.setupClippingPlane(paintProps.getClippingPane());
    }

    private void drawQCLegend(IGraphicsTarget target,
            PaintProperties paintProps, IFont font) throws VizException {
        // TODO this screen location code is borrowed from MPELegendResource...
        // should it be put into a shared class, possibly a paint
        // properties method?

        IExtent screenExtent = paintProps.getView().getExtent();
        double scale = (screenExtent.getHeight()
                / paintProps.getCanvasBounds().height);
        DrawableString drawableString = new DrawableString("0");
        drawableString.font = font;
        double textHeight = target.getStringsBounds(drawableString).getHeight()
                * scale;
        double padding = 3.2 * scale;
        double textSpace = textHeight + padding;
        double cmapHeight = textHeight * 1.25;
        double legendHeight = cmapHeight + (2.0 * textSpace) + (2.0 * padding);
        double y1 = screenExtent.getMinY() + legendHeight;
        double x1 = screenExtent.getMinX() + padding;
        int[] funct = dqc.funct;

        for (int i = 0; i < typename.length; i++) {
            RGB color = RGB_COLOR_MAP_A[funct[i]];
            lines[i] = new DrawableLine();
            lines[i].setCoordinates(x1, y1 + (i * textSpace));
            lines[i].addPoint(x1 + (2.5 * padding), y1 + (i * textSpace));
            lines[i].basics.color = color;
            lines[i].width = 35;
            color = RGB_COLOR_MAP_N[15];

            double xLoc = x1 + (4 * padding);
            double yLoc = y1 + ((i + .45) * textSpace);
            DrawableString string = new DrawableString(typename[i], color);
            string.font = font;
            string.setCoordinates(xLoc, yLoc);
            string.horizontalAlignment = HorizontalAlignment.LEFT;
            string.verticallAlignment = VerticalAlignment.BOTTOM;
            drawableStrings.add(string);
        }
    }

    /**
     * Selected coordinate
     * 
     * @return
     */
    public Coordinate getSelectedCoordinate() {
        return selectedCoordinate;
    }

    /**
     * Inspect method called when moused over while inspect is enabled
     * 
     * @param coord
     *            The coordinate of the inspection
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Envelope env = new Envelope();
        try {
            env = new Envelope(coord.asLatLon());
        } catch (TransformException | FactoryException e) {
            statusHandler.error("Error converting coordinate [" + coord + "].",
                    e);
            return null;
        }
        List<?> elements = strTree.query(env);
        if (!elements.isEmpty()) {
            Iterator<?> iter = elements.iterator();
            while (iter.hasNext()) {
                ArrayList<?> list = (ArrayList<?>) iter.next();
                if (list.get(1) instanceof String) {
                    return (String) list.get(1);
                } else {
                    return null;
                }
            }
        }
        return null;
    }

    /**
     * Interrogate method called when user clicks on a location
     * 
     * @param coord
     *            The coordinates of the mouse click
     */
    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate rcoord)
            throws VizException {

        return null;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // Fonts are shared and cached, no need to init or dispose
        fontFactory = new MPEFontFactory(target, this);
        circleColor = RGB_COLOR_MAP_N[15];

        /* Retrieve the precip colormap. */
        List<Colorvalue> colorSet = getResourceData().getColorSet();
        ColorMap colorMap = new ColorMap(colorSet.size());
        colorMap.setName("24hGRID_PRECIP");
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

        dmPref.getEntries().get(0).setLabel("");
        dmPref.getEntries().get(1).setLabel("");

        ColorMapCapability cmc = getCapability(ColorMapCapability.class);
        ColorMapParameters parameters = cmc.getColorMapParameters();
        if (parameters == null) {
            parameters = new ColorMapParameters();
            cmc.setColorMapParameters(parameters);
        }
        parameters.setColorMap(colorMap);
        parameters.setDataMapping(dmPref);

        Unit<?> displayUnit = USCustomary.INCH;
        Unit<?> dataUnit = USCustomary.INCH;
        parameters.setDataUnit(dataUnit);
        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setFormatString("0.00");

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);
        addPoints();
    }

    public int getSize() {
        return dataMap.size();

    }

    @Override
    public String getName() {
        if (DrawDQCStations.qcmode == "") {
            return "No Data Available";
        }

        return DrawDQCStations.qcmode;
    }

    /**
     * convert RGB to Color
     * 
     * @param color
     * @return
     */
    public static java.awt.Color convertR(RGB color) {
        int blue = color.blue;
        int green = color.green;
        int red = color.red;

        return new java.awt.Color(red, green, blue);
    }

    /**
     * convert Color to RGB
     * 
     * @param color
     * @return
     */
    public static RGB convertC(Color color) {
        int blue = (int) (color.getBlue() * 255f);
        int green = (int) (color.getGreen() * 255f);
        int red = (int) (color.getRed() * 255f);

        return new RGB(red, green, blue);
    }

    @Override
    protected void disposeInternal() {
        fontFactory.dispose();
    }
}
