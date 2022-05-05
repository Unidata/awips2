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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
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
import com.raytheon.viz.mpe.ui.dialogs.EditTempStationsDialog;
import com.raytheon.viz.mpe.ui.dialogs.QcTempOptionsDialog;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.DailyQcUtils.Ts;
import com.raytheon.viz.mpe.util.DailyQcUtils.Ttn;

import systems.uom.common.USCustomary;

/**
 * MPEMultiple point resource.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2009 2524       snaples     Initial creation
 * Mar  3, 2014 2804       mschenke    Set back up clipping pane
 * Jul 24, 2014 3429       mapeters    Updated deprecated drawLine() calls.
 * Aug 11, 2014 3504       mapeters    Replaced deprecated IODataPreparer
 *                                      instances with IRenderedImageCallback
 *                                      and removed unused modifyStation() method.
 * Nov 26, 2014 16889      snaples     Daily QC does not display SNOTEL data.
 * Dec  3, 2014 16889      snaples     Fixed colorvalue_use warning and cleaned up some code.
 * Mar 08, 2017 6149       mduff       Performance fixes for painting.
 * Jun 21, 2017 6149       mduff       Fix circle size.
 * May 10, 2018 7131       mduff       Changes made for DQC Dialog updates.
 * </pre>
 * 
 * @author snaples
 */

public class PointTempPlotResource extends
        HydroPointResource<PointTempResourceData> implements IMpeResource {

    private static final String[] COLOR_MAP_A = { "Cyan1", "Salmon", "Orange1",
            "Yellow1", "Magenta1", "Green1", "Green4", "Gray74", "White",
            "Cyan1" };

    private static final String[] COLOR_MAP_N = { "Grey10", "Grey", "Blue",
            "Aquamarine", "LightGreen", "DarkGreen", "Violet", "Purple", "Blue",
            "Blue", "Yellow1", "Yellow", "Yellow2", "VioletRed", "Red",
            "White" };

    private static final RGB[] RGB_COLOR_MAP_A = new RGB[COLOR_MAP_A.length];

    private static final RGB[] RGB_COLOR_MAP_N = new RGB[COLOR_MAP_N.length];

    private static final int TEXT_WIDTH_PADDING = 9;

    private static final double VERTICAL_SPACING_FACTOR = 1.5;

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

    private int time_pos = 0;

    private Map<String, Ttn> tdataMap;

    private final DailyQcUtils dqc = DailyQcUtils.getInstance();

    private List<DrawableString> drawableStrings = new ArrayList<>();

    private List<DrawableCircle> drawableCircles = new ArrayList<>();

    private List<DrawableLine> lines = new ArrayList<>(typename.length);

    private double circleSize;

    private RGB circleColor;

    /**
     * Constructor.
     * 
     * @param resourceData
     * @param props
     */
    public PointTempPlotResource(PointTempResourceData resourceData,
            LoadProperties props) {
        super(resourceData, props);
        df.setMaximumFractionDigits(2);
        df.setMaximumIntegerDigits(4);
    }

    private void addPoints() {
        dataMap = new HashMap<>();
        tdataMap = new HashMap<>();
        strTree = new STRtree();

        if (!DailyQcUtils.temperature_stations.isEmpty()) {
            int i = 0;
            for (Station gageData : DailyQcUtils.temperature_stations) {
                Coordinate xy = new Coordinate(gageData.lon, gageData.lat);
                String pm = gageData.parm;
                StringBuilder kv = new StringBuilder(String.valueOf(xy.x));
                kv.append(":");
                kv.append(String.valueOf(xy.y));
                kv.append(":");
                kv.append(pm);
                dataMap.put(kv.toString(), gageData);
                tdataMap.put(kv.toString(),
                        DailyQcUtils.tdata[DailyQcUtils.pcpn_day].tstn[i]);

                /* Create a small envelope around the point */
                Coordinate p1 = new Coordinate(xy.x + .02, xy.y + .02);
                Coordinate p2 = new Coordinate(xy.x - .02, xy.y - .02);
                Envelope env = new Envelope(p1, p2);
                List<Object> data = new ArrayList<>();
                data.add(xy);
                data.add("STATION: " + gageData.hb5 + " VALUE: "
                        + DailyQcUtils.tdata[DailyQcUtils.pcpn_day].tstn[i].tlevel2[time_pos].data);
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
        if ((displayMan.isMaxmin()) && (DailyQcUtils.points_flag == 1)) {
            int type = DailyQcUtils.plot_view;
            if (type == 0) {
                return;
            }

            int i = 0;
            int m = 0;
            float reverse_filter_value = 0;
            int elevation_filter_value = 0;
            Ts ts[] = DailyQcUtils.ts;
            int tsmax = DailyQcUtils.tsmax;
            float temp_filter_value = 0;
            int find_station_flag = dqc.find_station_flag;
            int dflag[] = DailyQcUtils.dflag;
            int qflag[] = DailyQcUtils.qflag;
            String mbuf = "";
            String tbuf = "";

            if (MPEDisplayManager.pcpn_time_step == 1) {
                time_pos = 4;
            } else if (MPEDisplayManager.pcpn_time_step == 2) {
                time_pos = 5;
            } else {
                time_pos = DailyQcUtils.pcpn_time;
            }

            double[] centerpixels = descriptor
                    .worldToPixel(new double[] { c.x, c.y });

            if ((DailyQcUtils.points_flag == 1)
                    && (displayMan.getQcTempDialog().isOpen())
                    && (MPEDisplayManager.getCurrent().isMaxmin())) {

                reverse_filter_value = QcTempOptionsDialog
                        .getPointFilterReverseValue();
                elevation_filter_value = dqc.getPointElevationFilterValue();
                temp_filter_value = QcTempOptionsDialog.getPointFilterValue();
            } else {
                return;
            }

            if ((station.elev >= 0)
                    && (station.elev < elevation_filter_value)) {
                return;
            }

            for (m = 0; m < tsmax; m++) {
                if (station.parm.substring(3, 5).equalsIgnoreCase(ts[m].abr)
                        && (dflag[m + 1] == 1)) {
                    break;
                }
            }

            if (m == tsmax) {
                return;
            }

            for (m = 0; m < 9; m++) {
                if ((m == tdataMap.get(key).tlevel2[time_pos].qual)
                        && (qflag[m] == 1)) {
                    break;
                } else if (((m == 7) && (qflag[7] == 1)
                        && ((tdataMap.get(key).tlevel2[time_pos].data == -99)
                                && (tdataMap
                                        .get(key).tlevel2[time_pos].qual == -99)))
                        || ((tdataMap.get(key).tlevel2[time_pos].data == -99)
                                && (tdataMap.get(
                                        key).tlevel2[time_pos].qual == 0))) {
                    break;
                }
            }

            if (m == 9) {
                return;
            }

            /* locate station in data stream */
            if (type == 4 || type == 5) {
                if (DailyQcUtils.tdata[DailyQcUtils.pcpn_day].used[time_pos] == 0
                        && DailyQcUtils.tdata[DailyQcUtils.pcpn_day].level[0] == 0) {
                    return;
                }
                if (tdataMap.get(key).tlevel2[time_pos].data < temp_filter_value
                        && tdataMap.get(key).tlevel2[time_pos].data != -99) {
                    return;
                }

                if (tdataMap
                        .get(key).tlevel2[time_pos].data > reverse_filter_value
                        && tdataMap.get(key).tlevel2[time_pos].data < 125) {
                    return;
                }
            }

            if ((type == 4) && (tdataMap.get(key).tlevel2[time_pos].data == -99)
                    && (qflag[7] != 1)) {
                return;
            }

            DrawableCircle circle = new DrawableCircle();
            circle.basics.color = this.circleColor;
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
                if ((DailyQcUtils.tdata[DailyQcUtils.pcpn_day].used[time_pos] == 0)
                        && (DailyQcUtils.tdata[DailyQcUtils.pcpn_day].level[0] == 0)) {
                    return;
                }

                /* if point data is missing, use character 'm' */

                mbuf = "";
                if (((tdataMap.get(key).tlevel2[time_pos].data == -99)
                        && (tdataMap.get(key).tlevel2[time_pos].qual == -99))
                        || ((tdataMap.get(key).tlevel2[time_pos].data == -99)
                                && (tdataMap.get(
                                        key).tlevel2[time_pos].qual == 0))) {
                    if (qflag[7] == 1) {
                        mbuf = "m";
                    } else {
                        return;
                    }

                } else {
                    mbuf = String.format("%d",
                            (int) tdataMap.get(key).tlevel2[time_pos].data);
                }

                tbuf = mbuf;
            } else if (type == 5) {
                if (station.tip == 0) {
                    return;
                }

                mbuf = station.hb5;
                tbuf = mbuf;

            } else if (type == 6) {
                mbuf = String.format("%d", station.elev);
                tbuf = mbuf;
            }

            m = tdataMap.get(key).tlevel2[time_pos].qual;

            RGB color = null;
            if ((m <= 9) && (m >= 0)) {
                color = RGB_COLOR_MAP_A[m];
            }
            /* set font color for missing data */

            if ((qflag[7] == 1)
                    && (tdataMap.get(key).tlevel2[time_pos].data == -99)
                    && (tdataMap.get(key).tlevel2[time_pos].qual == -99)) {

                color = RGB_COLOR_MAP_A[7];
            }

            int xadd = station.xadd;
            int yadd = station.yadd;

            IExtent screenExtent = paintProps.getView().getExtent();
            double scale = (screenExtent.getHeight()
                    / paintProps.getCanvasBounds().height);
            DrawableString string = new DrawableString("0", color);
            string.font = font;
            double textHeight = target.getStringsBounds(string).getHeight()
                    * scale;
            Coordinate valCoor = new Coordinate(
                    centerpixels[0] + (this.getScaleWidth() / 3),
                    centerpixels[1] - this.getScaleHeight());
            double textWidth = target.getStringsBounds(string).getWidth();
            double scaledTextWidth = (textWidth + TEXT_WIDTH_PADDING) * scale;
            double textX = valCoor.x + (xadd * scaledTextWidth);
            double textY = valCoor.y
                    + ((yadd) * VERTICAL_SPACING_FACTOR * textHeight)
                    + (textHeight / 2);
            string.setText(tbuf, color);
            string.setCoordinates(textX, textY);
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

        MPEDisplayManager displayMgr = getResourceData().getMPEDisplayManager();
        // Fonts are shared and cached, no need to init or dispose
        IFont font = fontFactory.getMPEFont(MPEDisplayManager.getFontId());

        drawableStrings.clear();
        drawableCircles.clear();

        if ((DailyQcUtils.points_flag == 1) && (displayMgr.isMaxmin())) {
            for (Entry<String, Station> entry : dataMap.entrySet()) {
                String key = entry.getKey();
                Coordinate c = new Coordinate();
                String[] spKey = key.split(":", 5);
                c.x = Double.parseDouble(spKey[0]);
                c.y = Double.parseDouble(spKey[1]);
                double[] pixel = descriptor
                        .worldToPixel(new double[] { c.x, c.y });

                if (paintProps.getView().getExtent().contains(pixel)) {
                    setScaleWidth(paintProps);
                    setScaleHeight(paintProps);

                    drawPlotInfo(c, key, dataMap.get(key), target, paintProps,
                            font);

                    if (selectedCoordinate != null) {
                        Envelope env = new Envelope(selectedCoordinate);
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
            target.clearClippingPlane();
            drawQCLegend(target, paintProps, font);
            target.drawStrings(drawableStrings);
            target.drawCircle(drawableCircles.toArray(new DrawableCircle[0]));
            target.drawLine(lines.toArray(new DrawableLine[0]));
            target.setupClippingPlane(paintProps.getClippingPane());
        }
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
        RGB color = null;
        String label = "";
        int[] funct = dqc.funct;
        int temp = 0;
        lines.clear();

        for (int i = 0; i < typename.length; i++) {
            if (i == 5) {
                continue;
            }
            color = RGB_COLOR_MAP_A[funct[i]];
            DrawableLine dl = new DrawableLine();

            dl.setCoordinates(x1, y1 + (temp * textSpace));
            dl.addPoint(x1 + (2.5 * padding), y1 + (temp * textSpace));
            dl.basics.color = color;
            dl.width = 35;
            lines.add(dl);

            label = typename[i];
            color = RGB_COLOR_MAP_N[15];
            double xLoc = x1 + (4 * padding);
            double yLoc = y1 + ((temp + .5) * textSpace);
            DrawableString string = new DrawableString(label, color);
            string.font = font;
            string.setCoordinates(xLoc, yLoc);
            string.horizontalAlignment = HorizontalAlignment.LEFT;
            string.verticallAlignment = VerticalAlignment.BOTTOM;
            drawableStrings.add(string);
            temp++;
        }
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
                List<?> list = (List<?>) iter.next();
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
        Coordinate coord = null;
        try {
            coord = rcoord.asLatLon();
        } catch (TransformException | FactoryException e) {
            statusHandler.error("Error converting coordinate [" + coord + "].",
                    e);
            return null;
        }
        Envelope env = new Envelope(coord);
        List<?> elements = strTree.query(env);
        Iterator<?> iter = elements.iterator();

        /* Take the first one in the list */
        if (iter.hasNext()) {
            selectedCoordinate = coord;
            this.issueRefresh();
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            EditTempStationsDialog epd = new EditTempStationsDialog(shell,
                    new ReferencedCoordinate(coord));
            epd.open();
        }
        return null;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

        fontFactory = new MPEFontFactory(target, this);
        this.circleColor = RGB_COLOR_MAP_A[8];

        List<Colorvalue> colorSet = getResourceData().getColorSet();
        /* Retrieve the temp colormap. */
        ColorMap colorMap = new ColorMap(colorSet.size());
        colorMap.setName("maxGRID_TEMP");
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

        Unit<?> displayUnit = USCustomary.FAHRENHEIT;
        Unit<?> dataUnit = USCustomary.FAHRENHEIT.divide(100);
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
