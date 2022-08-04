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
import com.raytheon.viz.mpe.ui.dialogs.QcFreezeOptionsDialog;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.DailyQcUtils.Ztn;

import systems.uom.common.USCustomary;

/**
 * MPEMultiple point resource.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- -----------  --------------------------
 * Jul  8, 2009 2589       snaples      Initial creation
 * Mar  3, 2014 2804       mschenke     Set back up clipping pane
 * Jul 24, 2014 3429       mapeters     Updated deprecated drawLine() calls.
 * Aug 11, 2014 3504       mapeters     Replaced deprecated IODataPreparer
 *                                       instances with IRenderedImageCallback.
 *                                       and removed unused modifyStation() method.
 * Nov 26, 2014 16889      snaples      Daily QC does not display SNOTEL data.
 * Dec  3, 2014 16889      snaples      Fixed colorvalue_use warning and cleaned up some code.
 * Mar 08, 2017 6149       mduff        Performance fixes for painting.
 * Jun 21, 2017 6149       mduff        Fix circle size.
 * May 10, 2018 7131       mduff        Changes made for DQC Dialog updates.
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class PointFreezePlotResource extends
        HydroPointResource<PointFreezeResourceData> implements IMpeResource {

    private static final String[] COLOR_MAP_A = { "Cyan1", "Salmon", "Orange1",
            "Yellow1", "Magenta1", "Green1", "Green4", "Gray74", "White",
            "Cyan1" };

    private static final String[] COLOR_MAP_N = { "Grey10", "Grey", "Blue",
            "Aquamarine", "LightGreen", "DarkGreen", "Violet", "Purple", "Blue",
            "Blue", "Yellow1", "Yellow", "Yellow2", "VioletRed", "Red",
            "White" };

    private static final RGB[] RGB_COLOR_MAP_A = new RGB[COLOR_MAP_A.length];

    private static final RGB[] RGB_COLOR_MAP_N = new RGB[COLOR_MAP_N.length];

    private Map<String, Station> dataMap = null;

    private static STRtree strTree = null;

    private static Coordinate selectedCoordinate;

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

    private Map<String, Ztn> zdataMap;

    private final DailyQcUtils dqc = DailyQcUtils.getInstance();

    private List<DrawableString> drawableStrings = new ArrayList<>();

    private List<DrawableCircle> drawableCircles = new ArrayList<>();

    private List<DrawableLine> lines = new ArrayList<>(typename.length);

    private double circleSize;

    /**
     * Constructor.
     * 
     * @param resourceData
     * @param props
     */
    public PointFreezePlotResource(PointFreezeResourceData resourceData,
            LoadProperties props) {
        super(resourceData, props);
        df.setMaximumFractionDigits(2);
        df.setMaximumIntegerDigits(4);
    }

    private void addPoints() {

        dataMap = new HashMap<>();
        zdataMap = new HashMap<>();
        strTree = new STRtree();

        if (!DailyQcUtils.freezing_stations.isEmpty()) {
            int i = 0;
            for (Station gageData : DailyQcUtils.freezing_stations) {
                Coordinate xy = new Coordinate();
                xy.x = gageData.lon;
                xy.y = gageData.lat;
                String pm = gageData.parm;
                StringBuilder kv = new StringBuilder(String.valueOf(xy.x));
                kv.append(":");
                kv.append(String.valueOf(xy.y));
                kv.append(":");
                kv.append(pm);
                dataMap.put(kv.toString(), gageData);
                zdataMap.put(kv.toString(),
                        DailyQcUtils.zdata[DailyQcUtils.pcpn_day].zstn[i]);

                /* Create a small envelope around the point */
                Coordinate p1 = new Coordinate(xy.x + .02, xy.y + .02);
                Coordinate p2 = new Coordinate(xy.x - .02, xy.y - .02);
                Envelope env = new Envelope(p1, p2);
                List<Object> data = new ArrayList<>();
                data.add(xy);
                data.add("STATION: " + gageData.hb5 + " VALUE: "
                        + DailyQcUtils.zdata[DailyQcUtils.pcpn_day].zstn[i].zlevel2[DailyQcUtils.pcpn_time].data);
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
        if ((displayMan.isZflag()) && (DailyQcUtils.points_flag == 1)
                && displayMan.getQcFreezeDialog().isOpen()) {
            int type = DailyQcUtils.plot_view;
            if (type == 0) {
                return;
            }

            int i = 0;
            int m = 0;
            float freezing_reverse_filter_value = 0;
            float freezing_filter_value = 0;
            int find_station_flag = dqc.find_station_flag;
            String mbuf = "";
            String tbuf = "";

            double[] centerpixels = descriptor
                    .worldToPixel(new double[] { c.x, c.y });

            freezing_reverse_filter_value = QcFreezeOptionsDialog
                    .getPointFilterReverseValue();
            freezing_filter_value = QcFreezeOptionsDialog.getPointFilterValue();

            if ((zdataMap
                    .get(key).zlevel2[DailyQcUtils.pcpn_time].data < freezing_filter_value)
                    && (zdataMap.get(
                            key).zlevel2[DailyQcUtils.pcpn_time].data >= 0.0)) {
                return;
            }

            if ((zdataMap
                    .get(key).zlevel2[DailyQcUtils.pcpn_time].data > freezing_reverse_filter_value)
                    && (zdataMap.get(
                            key).zlevel2[DailyQcUtils.pcpn_time].data < 20.0)) {
                return;
            }

            /* locate station in data stream */
            if ((type == 4) && (zdataMap
                    .get(key).zlevel2[DailyQcUtils.pcpn_time].data == -1)) {
                return;

            }
            if ((type == 4)
                    && (DailyQcUtils.zdata[DailyQcUtils.pcpn_day].used[DailyQcUtils.pcpn_time] == 0)) {
                return;
            }

            // sets current color to White
            RGB color = RGB_COLOR_MAP_A[8];

            DrawableCircle circle = new DrawableCircle();
            circle.basics.color = color;
            circle.setCoordinates(centerpixels[0], centerpixels[1]);
            double pixelRatio = paintProps.getView().getExtent().getWidth()
                    / paintProps.getCanvasBounds().width;
            circle.radius = circleSize * pixelRatio;
            circle.filled = true;
            drawableCircles.add(circle);

            tbuf = "";
            if (type == 1) {
                tbuf = station.hb5;
            } else if (type == 2) {
                tbuf = station.parm.substring(3, 5);
            } else if (type == 3) {
                tbuf = station.name;
            } else if (type == 4) {
                if (DailyQcUtils.zdata[DailyQcUtils.pcpn_day].used[DailyQcUtils.pcpn_time] == 0) {
                    return;
                }

                mbuf = String.format("%5.2f",
                        zdataMap.get(key).zlevel2[DailyQcUtils.pcpn_time].data);

                mbuf = mbuf.trim();
                tbuf = mbuf;

            }

            m = zdataMap.get(key).zlevel2[DailyQcUtils.pcpn_time].qual;

            if ((m <= 9) && (m >= 0)) {
                color = RGB_COLOR_MAP_A[m];
            }

            int xadd = station.xadd;
            int yadd = station.yadd;
            int xc = 0;
            int yc = 0;
            IExtent screenExtent = paintProps.getView().getExtent();
            double scale = (screenExtent.getHeight()
                    / paintProps.getCanvasBounds().height);
            DrawableString string = new DrawableString(tbuf, color);
            string.font = font;
            double textHeight = target.getStringsBounds(string).getHeight()
                    * scale;
            double padding = .5 * scale;
            int text_width = (int) (tbuf.length() * .95);
            double textSpace = textHeight + (padding * scale * .5);
            Coordinate valCoor = new Coordinate(
                    centerpixels[0] + (this.getScaleWidth() / 3),
                    centerpixels[1] - this.getScaleHeight());
            if (xadd < 0) {
                xc = (int) (valCoor.x - text_width);
            } else {
                xc = (int) (valCoor.x + 1);
            }

            if (yadd < 0) {
                yc = (int) valCoor.y;
            } else {
                yc = (int) valCoor.y + 2;
            }

            double xLoc = xc + (.75 * padding) + (text_width * scale);
            double yLoc = yc + ((.75) * textSpace);
            string.setCoordinates(xLoc, yLoc);
            string.horizontalAlignment = HorizontalAlignment.LEFT;
            string.verticallAlignment = VerticalAlignment.TOP;
            drawableStrings.add(string);
            if (i == find_station_flag) {
                find_station_flag = -1;
            }
        }
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
        drawableStrings.clear();
        drawableCircles.clear();

        MPEDisplayManager displayMgr = getResourceData().getMPEDisplayManager();

        // Fonts are shared and cached, get from factory
        IFont font = fontFactory.getMPEFont(MPEDisplayManager.getFontId());

        if ((DailyQcUtils.points_flag == 1) && (displayMgr.isZflag())) {
            for (Entry<String, Station> entry : dataMap.entrySet()) {
                String key = entry.getKey();
                Station gageData = entry.getValue();
                Coordinate c = new Coordinate(gageData.lon, gageData.lat);
                double[] pixel = descriptor
                        .worldToPixel(new double[] { c.x, c.y });

                if (paintProps.getView().getExtent().contains(pixel)) {
                    setScaleWidth(paintProps);
                    setScaleHeight(paintProps);

                    drawPlotInfo(c, key, dataMap.get(key), target, paintProps,
                            font);

                    if (getSelectedCoordinate() != null) {
                        Envelope env = new Envelope(getSelectedCoordinate());
                        List<?> elements = strTree.query(env);
                        if (!elements.isEmpty()) {
                            Iterator<?> iter2 = elements.iterator();
                            /* Take the first one in the list */
                            if (iter2.hasNext()) {
                                /*
                                 * element 0 = Coordinate, 1 = inspectString
                                 */
                                ArrayList<?> data = (ArrayList<?>) iter2.next();
                                PixelExtent pe = this.getPixelExtent(
                                        (Coordinate) data.get(0));
                                target.drawRect(pe, HydroConstants.SQUARE_COLOR,
                                        2, 1);
                            }
                        }
                    }
                }
            }
        }
        target.clearClippingPlane();
        drawQCLegend(target, paintProps, font);
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
        DrawableString string = new DrawableString("0");
        string.font = font;
        double textHeight = target.getStringsBounds(string).getHeight() * scale;
        double padding = 3.2 * scale;
        double textSpace = textHeight + padding;
        double cmapHeight = textHeight * 1.25;
        double legendHeight = cmapHeight + (2.0 * textSpace) + (2.0 * padding);
        double y1 = screenExtent.getMinY() + legendHeight;
        double x1 = screenExtent.getMinX() + padding;
        int[] funct = dqc.funct;
        int temp = 0;

        lines.clear();
        for (int i = 0; i < typename.length; i++) {
            if ((i != 0) && (i != 6) && (i != 7) && (i != 3)) {
                continue;
            }
            DrawableLine dl = new DrawableLine();
            dl.setCoordinates(x1, y1 + (temp * textSpace));
            dl.addPoint(x1 + (2.5 * padding), y1 + (temp * textSpace));
            dl.basics.color = RGB_COLOR_MAP_A[funct[i]];
            dl.width = 35;
            lines.add(dl);

            double xLoc = x1 + (4 * padding);
            double yLoc = y1 + ((temp + .5) * textSpace);
            string.setText(typename[i], RGB_COLOR_MAP_N[15]);
            string.setCoordinates(xLoc, yLoc);
            string.horizontalAlignment = HorizontalAlignment.LEFT;
            string.verticallAlignment = VerticalAlignment.BOTTOM;
            target.drawStrings(string);
            temp++;
        }
        target.drawLine(lines.toArray(new DrawableLine[0]));
    }

    /**
     * Set the selected coordinate
     * 
     * @param selectedCoordinate
     */
    public static void setSelectedCoordinate(Coordinate selCoordinate) {
        selectedCoordinate = selCoordinate;
    }

    /**
     * Selected coordinate
     * 
     * @return
     */
    public static Coordinate getSelectedCoordinate() {
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
        fontFactory = new MPEFontFactory(target, this);

        /* Retrieve the freezing colormap. */
        List<Colorvalue> colorSet = getResourceData().getColorSet();
        ColorMap colorMap = new ColorMap(colorSet.size());
        colorMap.setName("6hGRID_FREEZL");
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

        Unit<?> displayUnit = USCustomary.FOOT;
        Unit<?> dataUnit = USCustomary.FOOT.divide(100);
        parameters.setDataUnit(dataUnit);
        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setFormatString("0");

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
