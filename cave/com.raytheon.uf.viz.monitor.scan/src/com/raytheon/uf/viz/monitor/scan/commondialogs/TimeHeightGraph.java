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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.TimeZone;
import java.util.TreeSet;

import javax.measure.UnitConverter;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.monitor.scan.commondialogs.EllipseData.EllipseType;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;

import si.uom.NonSI;
import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 * Composite containing the Time Height Graph canvas that will draw the
 * time-height data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------
 * Mar 11, 2010           lvenable  Initial creation
 * Oct 14, 2013  2474     lvenable  Fixed font memory leak.
 * Oct 11, 2016  19405    jdynina   Account for SAILS
 * Sep 18, 2017  20312    jdynina   Account for MRLE/SAILS updates
 * Jul 16, 2018  6766     randerso  Code cleanup.
 *
 * </pre>
 *
 * @author lvenable
 */
public class TimeHeightGraph {
    /**
     * Parent composite.
     */
    private Composite parentComp;

    /**
     * Drawing canvas
     */
    private Canvas canvas;

    /**
     * Display object.
     */
    private Display display;

    /**
     * Canvas width.
     */
    private static final int CANVAS_WIDTH = 1050;

    /**
     * Canvas height.
     */
    private static final int CANVAS_HEIGHT = 550;

    /**
     * X coord of the left vertical time line.
     */
    private static final int LEFT_TIMELINE_XCOORD = 50;

    /**
     * X coord of the right vertical time line.
     */
    private static final int RIGHT_TIMELINE_XCOORD = CANVAS_WIDTH - 75;

    /**
     * Y coord of the imaginary top line across the top of the graph.
     */
    private static final int TOP_HLINE_YCOORD = 50;

    /**
     * Y coord of the bottom line across the bottom of the graph.
     */
    private static final int BOTTOM_HLINE_YCOORD = CANVAS_HEIGHT - 50;

    /**
     * Width of the graph (area where the points and lines are drawn).
     */
    private static final int GRAPH_WIDTH = RIGHT_TIMELINE_XCOORD
            - LEFT_TIMELINE_XCOORD;

    /**
     * Height of the graph (area where the points and lines are drawn).
     */
    private static final int GRAPH_HEIGHT = BOTTOM_HLINE_YCOORD
            - TOP_HLINE_YCOORD;

    /**
     * X coord of the starting time label.
     */
    private static final int LEFT_TIME_LBL_XCOORD = LEFT_TIMELINE_XCOORD + 50;

    /**
     * X coord of the ending time label.
     */
    private static final int RIGHT_TIME_LBL_XCOORD = RIGHT_TIMELINE_XCOORD - 25;

    /**
     * Kilometers to thousands of feet conversion.
     */
    private static final UnitConverter KM_TO_KFT = MetricPrefix.KILO(SI.METRE)
            .getConverterTo(MetricPrefix.KILO(USCustomary.FOOT));

    /**
     * Nautical Miles to km conversion.
     */

    private static final UnitConverter NAUTICAL_MILES_TO_KM = USCustomary.NAUTICAL_MILE
            .getConverterTo(MetricPrefix.KILO(SI.METRE));

    /**
     * Meters per second to knots conversion.
     *
     */
    private static final UnitConverter METERS_PER_SEC_TO_KTS = SI.METRE_PER_SECOND
            .getConverterTo(USCustomary.KNOT);

    /**
     * Time span covered by the graph.
     */
    private static final int GRAPH_WIDTH_SECONDS = (60 + 2) * 60;

    private static final int GRAPH_RIGHT_MARGIN_SECONDS = 1 * 60;

    /**
     * Height per pixel increment.
     */
    private double heightPixInc = Double.NaN;

    /**
     * Time label string.
     */
    private static final String timeStr = "Time (UTC)";

    /**
     * X coord offset for the time string.
     */
    private int timeStrXCoordOffset = 0;

    /**
     * Height label string.
     */
    private static final String heightStr = "Height (ARL)\n   (kft)";

    /**
     * Text font.
     */
    private Font textFont;

    /**
     * Text width.
     */
    private int textWidth = 0;

    /**
     * Text height.
     */
    private int textHeight = 0;

    /**
     * Text height Y coord offset which is 1/2 the next height.
     */
    private int textHgtYOffset = 0;

    /**
     * The maximum value of the height in kft.
     */
    private int maxHeightValueKft = 8;

    /**
     * Draw settings that determine what is drawn on the graph.
     */
    private DrawSettings drawSettings;

    /**
     * Time format.
     */
    private SimpleDateFormat sdf = new SimpleDateFormat("HHmm");

    /**
     * Number of milliseconds per pixel.
     */
    private double millisPerPixel = 0.0;

    /**
     * Starting time of graph in milliseconds.
     */
    private long startingTimeMillis = 0;

    /**
     * Pixel per height.
     */
    private double pixelPerHeight = 0.0;

    /**
     * Ellipse data that will calculate the necessary offsets
     */
    private EllipseData ellipseData;

    /**
     * Graph data that will be drawn. Volume Scan Time in millis ->
     * DMDTableDataRow
     */
    private NavigableMap<Long, DMDTableDataRow> graphData;

    /**
     * TreeSet of all the angles in the graph data.
     */
    private TreeSet<Double> allAngles;

    /**
     * Map of elevation lines and the pixel value the lines are for.
     */
    private HashMap<Double, ArrayList<Integer>> elevLinesAngMap;

    /**
     * Array of primitive int arrays used for drawing the volume scan poles.
     */
    private ArrayList<int[]> volScanLines;

    /**
     * Scan configuration.
     */
    private SCANConfig scanCfg;

    /**
     * Callback for getting time-height information.
     */
    private ITimeHeightInfo timeHeightCB;

    /**
     * Flag to determine if the canvas should be redrawn. This is used to
     * suspend drawing while the data is being updated.
     */
    private boolean redrawFlag = true;

    /** No Data Available font. */
    private Font noDataFont;

    /**
     * Constructor.
     *
     * @param parentComp
     *            Parent composite.
     * @param drawSettings
     *            Draw settings.
     * @param timeHeightCB
     *            Time-height info callback.
     */
    public TimeHeightGraph(Composite parentComp, DrawSettings drawSettings,
            ITimeHeightInfo timeHeightCB) {
        this.parentComp = parentComp;
        this.drawSettings = drawSettings;
        this.timeHeightCB = timeHeightCB;
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        display = this.parentComp.getDisplay();

        initData();
        createCanvas();
    }

    /**
     * Initialize the data.
     */
    private void initData() {
        scanCfg = SCANConfig.getInstance();

        elevLinesAngMap = new LinkedHashMap<>();
        volScanLines = new ArrayList<>();

        // Create set that will contain all of the angles that
        // are present in the data.
        allAngles = new TreeSet<>();

        // Create the ellipse data
        ellipseData = new EllipseData();

        // Create the text font
        textFont = new Font(display, "Monospace", 10, SWT.BOLD);
        noDataFont = new Font(display, "Monospace", 30, SWT.BOLD);

        // Create the time labels and make other calculations
        createTimeLabels();

        // Make text height and width calculations
        calcTextWidthHeight();

        // Calculate time string X coordinate offset for drawing the time text
        int timeStrPixLen = Math.round(timeStr.length() * textWidth);
        int graphLineMiddlePix = ((GRAPH_WIDTH) / 2) + LEFT_TIMELINE_XCOORD;
        timeStrXCoordOffset = (int) Math
                .round(graphLineMiddlePix - (timeStrPixLen / 2.0));
    }

    /**
     * Re-calculate the height for graphing.
     *
     * @param maxHeight
     *            Maximum height in the data.
     */
    private void recalcHeightVarables(double maxHeight) {
        // Calculate the max height value
        maxHeightValueKft = findMaxHeightValue(maxHeight);

        // Calculate pixel increments
        heightPixInc = (double) GRAPH_HEIGHT / (double) maxHeightValueKft;
        pixelPerHeight = (BOTTOM_HLINE_YCOORD - TOP_HLINE_YCOORD)
                / (double) maxHeightValueKft;
    }

    /**
     * Create the canvas for drawing the data.
     */
    private void createCanvas() {
        canvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        // canvas = new Canvas(shell, SWT.NO_BACKGROUND);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        canvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);

        canvas.setLayoutData(gd);
        canvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });

        canvas.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                textFont.dispose();
                noDataFont.dispose();
            }
        });
    }

    /**
     * Draw the canvas.
     *
     * @param gc
     *            Graphics context.
     */
    private void drawCanvas(GC gc) {
        // Prevent redrawing because the data is being updated.
        if (!redrawFlag) {
            return;
        }

        gc.setAntialias(SWT.ON);
        gc.setFont(textFont);

        gc.setBackground(display.getSystemColor(SWT.COLOR_BLACK));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        if (graphData == null) {
            gc.setFont(noDataFont);
            gc.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
            gc.drawString("NO DATA AVAILABLE", 100, CANVAS_HEIGHT / 2, true);

            return;
        }

        /*
         * Draw graph outline.
         */
        gc.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        gc.drawLine(LEFT_TIMELINE_XCOORD, TOP_HLINE_YCOORD,
                LEFT_TIMELINE_XCOORD, BOTTOM_HLINE_YCOORD);
        gc.drawLine(LEFT_TIMELINE_XCOORD, BOTTOM_HLINE_YCOORD,
                RIGHT_TIMELINE_XCOORD, BOTTOM_HLINE_YCOORD);
        gc.drawLine(RIGHT_TIMELINE_XCOORD, BOTTOM_HLINE_YCOORD,
                RIGHT_TIMELINE_XCOORD, TOP_HLINE_YCOORD);

        /*
         * Draw Time and Height graph labels
         */
        gc.drawString(timeStr, timeStrXCoordOffset,
                CANVAS_HEIGHT - textHeight - 2, true);
        gc.drawText(heightStr, 2, 2, true);
        gc.drawText(heightStr, RIGHT_TIMELINE_XCOORD - (12 * textWidth) / 2, 2,
                true);

        /*
         * Draw the height hash marks and labels
         */
        drawHeightHashAndLabels(gc);

        /*
         * Draw the time labels.
         */
        drawTimeLabels(gc);

        /*
         * Draw the legend.
         */
        if ((drawSettings.legend) && (drawSettings.diamOverlay)) {
            drawLegend(gc);
        }

        /*
         * Draw the Vol Scan Poles
         */
        if (drawSettings.volScanPoles) {
            gc.setForeground(display.getSystemColor(SWT.COLOR_GRAY));
            gc.setLineWidth(2);

            for (int[] array : volScanLines) {
                gc.drawPolyline(array);
            }
            gc.setLineWidth(1);
        }

        /*
         * Draw the Elevation angle lines and labels
         */
        if (drawSettings.elevAngles) {
            drawElevationLinesAndAngles(gc);
        }

        /*
         * Draw the value labels (with or without circulation)
         */
        List<Double> dataList = null;
        String attr = timeHeightCB.getCurrentAttribute();
        Calendar volScanCal = TimeUtil.newGmtCalendar();
        Iterator<Long> iter = graphData.keySet().iterator();
        while (iter.hasNext()) {
            long timeMillis = iter.next();
            DMDTableDataRow dataRow = graphData.get(timeMillis);
            List<Double> timeList = dataRow.getTimeHeightTimes(); // seconds
                                                                  // past
                                                                  // midnight
            List<Double> heightList = dataRow.getTimeHeightHeight();
            List<Double> angleList = dataRow.getTimeHeightElevationIndexes();
            List<Double> llDiamList = dataRow.getTimeHeightDiam();

            // Get the data
            if ("llgtg".equalsIgnoreCase(attr)) {
                dataList = dataRow.getTimeHeightGtgMax();
            } else if ("llShr".equalsIgnoreCase(attr)) {
                dataList = dataRow.getTimeHeightShear();
            } else if ("llVr".equalsIgnoreCase(attr)) {
                List<Double> tmp = new ArrayList<>();
                dataList = new ArrayList<>();
                tmp = dataRow.getTimeHeightRotvel();
                // Convert to kts
                for (Double d : tmp) {
                    long val = Math.round(METERS_PER_SEC_TO_KTS.convert(d).doubleValue());
                    dataList.add(new Double(val));
                }
            } else if ("llDiam".equalsIgnoreCase(attr)) {
                dataList = new ArrayList<>();
                // Convert to nautical miles
                for (Double d : llDiamList) {
                    dataList.add(NAUTICAL_MILES_TO_KM.convert(d).doubleValue());
                }
            } else if ("stRank".equalsIgnoreCase(attr)) {
                dataList = dataRow.getTimeHeightRank();
            } else {
                continue;
            }

            int idx = 0;
            int maxIdx = angleList.size();
            if (timeList.size() < angleList.size()) {
                maxIdx = timeList.size();
            }

            volScanCal.setTimeInMillis(timeMillis);
            volScanCal.set(Calendar.HOUR_OF_DAY, 0);
            volScanCal.set(Calendar.MINUTE, 0);
            volScanCal.set(Calendar.SECOND, 0);
            volScanCal.set(Calendar.MILLISECOND, 0);
            long volScanMidnight = volScanCal.getTimeInMillis();

            for (int i = 0; i < angleList.size(); i++) {
                Double angleIdx = angleList.get(i);
                // prevent falling off the end of available times
                if (idx >= maxIdx) {
                    continue;
                }
                Double seconds = timeList.get(angleIdx.intValue());

                Calendar cal = Calendar
                        .getInstance(TimeZone.getTimeZone("GMT"));

                cal.setTimeInMillis(volScanMidnight
                        + (long) (double) seconds * TimeUtil.MILLIS_PER_SECOND);
                double heightKft = KM_TO_KFT.convert(heightList.get(idx).doubleValue());
                drawCirculation(gc, cal.getTimeInMillis(), heightKft,
                        dataList.get(idx), llDiamList.get(idx));
                idx++;
            }
        }
    }

    /**
     * Draw the left and right height hashes and labels.
     *
     * @param gc
     *            Graphics context.
     */
    private void drawHeightHashAndLabels(GC gc) {
        int newYcoord = 0;

        for (int i = 0; i <= maxHeightValueKft; i += 2) {
            newYcoord = (int) Math
                    .round(BOTTOM_HLINE_YCOORD - (i * heightPixInc));
            if ((i == 0) || (i % 4 == 0)) {
                gc.drawString(String.format("%3d", i),
                        LEFT_TIMELINE_XCOORD - (3 * textWidth) - 7,
                        newYcoord - (int) Math.round(textHeight / 2.0), true);

                gc.drawString(String.format("%d", i), RIGHT_TIMELINE_XCOORD + 7,
                        newYcoord - (int) Math.round(textHeight / 2.0), true);
            }

            gc.drawLine(LEFT_TIMELINE_XCOORD, newYcoord,
                    LEFT_TIMELINE_XCOORD - 5, newYcoord);
            gc.drawLine(RIGHT_TIMELINE_XCOORD, newYcoord,
                    RIGHT_TIMELINE_XCOORD + 5, newYcoord);
        }
    }

    /**
     * Draw the time labels.
     *
     * @param gc
     *            Graphics context.
     */
    private void drawTimeLabels(GC gc) {
        int textOffset = (int) Math.round(textWidth * 4 / 2.0);
        int xCoord = 0;
        int xCoordLast = 0;
        Calendar cal = TimeUtil.newGmtCalendar();
        double timeMillisOffset = 0.0;

        for (Long timeMillis : graphData.keySet()) {
            // Invalid times have been removed by setGraphData()
            cal.setTimeInMillis(timeMillis);
            timeMillisOffset = (cal.getTimeInMillis() - startingTimeMillis)
                    / millisPerPixel;
            xCoord = (int) Math.round(timeMillisOffset + LEFT_TIME_LBL_XCOORD);

            // ensure adequate space between time labels
            if ((xCoord >= LEFT_TIMELINE_XCOORD)
                    && ((xCoord - xCoordLast) > 30)) {
                gc.drawLine(xCoord, BOTTOM_HLINE_YCOORD, xCoord,
                        BOTTOM_HLINE_YCOORD + 5);
                gc.drawString(sdf.format(cal.getTime()), xCoord - textOffset,
                        BOTTOM_HLINE_YCOORD + 7, true);
            }
            xCoordLast = xCoord;
        }
    }

    /**
     * Draw the elevation lines and labels.
     *
     * @param gc
     *            Graphics context.
     */
    private void drawElevationLinesAndAngles(GC gc) {
        gc.setForeground(display.getSystemColor(SWT.COLOR_GRAY));
        gc.setLineWidth(8);

        for (Entry<Double, ArrayList<Integer>> entry : elevLinesAngMap
                .entrySet()) {
            List<Integer> list = entry.getValue();
            if (list.isEmpty()) {
                continue;
            }
            int[] intArr = new int[list.size() + 4];
            int i = 0;
            for (i = 0; i < intArr.length - 2; i++) {
                if (i == 0) {
                    intArr[i] = list.get(i) - 40;
                    i++;
                    intArr[i] = list.get(i);
                    continue;
                }
                intArr[i] = list.get(i - 2);
            }

            int lastY = intArr[i - 1];
            intArr[i] = RIGHT_TIMELINE_XCOORD;
            i++;
            intArr[i] = lastY;
            gc.drawPolyline(intArr);
        }
        gc.setLineWidth(1);

        /*
         * Loop through the array of elevation lines. Use the first set of x,y
         * coords to draw the left side labels. Use the last one to draw the
         * right side labels.
         */
        int newTextXCoord = 0;
        int newTextYCoord = 0;
        int lastTextXCoord = 0;
        int lastTextYCoord = 0;
        Map<Double, int[]> lastMap = new HashMap<>();

        for (Entry<Double, ArrayList<Integer>> entry : elevLinesAngMap
                .entrySet()) {
            if (entry.getValue().isEmpty()) {
                continue;
            }

            List<Integer> intList = entry.getValue();

            newTextXCoord = intList.get(0) - (4 * textWidth) - 50;
            newTextYCoord = (int) Math.round(intList.get(1) - textHeight / 2.0);
            if (newTextXCoord >= LEFT_TIMELINE_XCOORD) {
                gc.drawString(String.format("%4.1f", entry.getKey()),
                        newTextXCoord, newTextYCoord, true);
            }

            lastTextXCoord = intList.get(intList.size() - 2) + (4 * textWidth)
                    + 50;
            lastTextYCoord = (int) Math
                    .round(intList.get(intList.size() - 1) - textHeight / 2.0);

            lastMap.put(entry.getKey(),
                    new int[] { lastTextXCoord, lastTextYCoord });
        }

        /*
         * Draw the elevation angles on the right side of the graph.
         */
        for (Entry<Double, int[]> entry : lastMap.entrySet()) {
            int[] pts = entry.getValue();
            gc.drawString(String.format("%4.1f", entry.getKey()),
                    RIGHT_TIMELINE_XCOORD + 30, pts[1], true);
        }
    }

    /**
     * Draw the circulation ellipse.
     *
     * @param gc
     *            Graphics context.
     * @param timeInMillis
     *            Time in milliseconds.
     * @param height
     *            Height in kft.
     * @param value
     *            Value.
     * @param diamVal
     *            Low level diameter.
     */
    private void drawCirculation(GC gc, long timeInMillis, double height,
            double value, double diamVal) {
        // Get the color for the threshold.
        Color threshColor = scanCfg.getThresholdColor(ScanTables.DMD,
                timeHeightCB.getCurrentAttribute(), value);

        // If the value ends in ".0" then remove it.
        String valStr = String.format("%01.1f", value);
        if (valStr.endsWith(".0")) {
            valStr = valStr.replaceAll("\\.0", "");
        }
        // Determine the X and Y coordinate
        Point xyCoords = calcTimeHeightToXY(timeInMillis, height);
        if (xyCoords.x >= RIGHT_TIMELINE_XCOORD) {
            return;
        }

        // invalid coordinate filter
        if ((xyCoords.x < LEFT_TIMELINE_XCOORD)
                || (xyCoords.y < TOP_HLINE_YCOORD)) {
            return;
        }

        // Check if the oval need to be drawn
        if (drawSettings.diamOverlay) {
            gc.setBackground(threshColor);
            gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

            Rectangle rec = ellipseData.getEllipseDrawData(xyCoords.x,
                    xyCoords.y, diamVal);

            gc.fillOval(rec.x, rec.y, rec.width, rec.height);

            int textXCoord = xyCoords.x
                    - (int) Math.round(textWidth * valStr.length() / 2.0);
            int textYCoord = xyCoords.y - textHgtYOffset;
            gc.drawString(valStr, textXCoord, textYCoord, true);
        } else {
            gc.setForeground(threshColor);
            int textXCoord = xyCoords.x
                    - (int) Math.round(textWidth * valStr.length() / 2.0);
            int textYCoord = xyCoords.y - textHgtYOffset;
            gc.drawString(valStr, textXCoord, textYCoord, true);
        }
    }

    /**
     * Draw the legend.
     *
     * @param gc
     *            Graphic Context
     */
    private void drawLegend(GC gc) {
        int textYCoord = TOP_HLINE_YCOORD - 10;
        int textXCoord = LEFT_TIMELINE_XCOORD + 15;
        int ellipseMiddleXCoord = textXCoord + 15
                + ellipseData.getEllipseMiddleWidth(EllipseType.E_6mn);
        int ellipseXCoord = 0;

        gc.setForeground(display.getSystemColor(SWT.COLOR_GRAY));
        gc.setBackground(display.getSystemColor(SWT.COLOR_GRAY));

        /*
         * Draw the legend header
         */
        gc.drawString("   Marcotte", textXCoord, textYCoord, true);
        gc.drawString("  Enhancement", textXCoord, textYCoord + textHeight - 3,
                true);
        gc.drawString("Diameter Legend", textXCoord,
                textYCoord + textHeight * 2, true);

        /*
         * Draw the legend ellipses
         */

        // 6 nm
        int xCoordoffset = ellipseData.getXOffsetForType(EllipseType.E_6mn);
        int ellipseYCoord = textYCoord + textHeight * 3 + 3;
        ellipseXCoord = ellipseMiddleXCoord + xCoordoffset;
        gc.fillOval(ellipseXCoord, ellipseYCoord,
                ellipseData.getEllipseWidth(EllipseType.E_6mn),
                ellipseData.getEllipseHeight());

        // 3 nm
        xCoordoffset = ellipseData.getXOffsetForType(EllipseType.E_3mn);
        ellipseYCoord += ellipseData.getEllipseHeight() + 5;
        ellipseXCoord = ellipseMiddleXCoord + xCoordoffset;
        gc.fillOval(ellipseXCoord, ellipseYCoord,
                ellipseData.getEllipseWidth(EllipseType.E_3mn),
                ellipseData.getEllipseHeight());

        // 0.5 nm
        xCoordoffset = ellipseData.getXOffsetForType(EllipseType.E_0_5);
        ellipseYCoord += ellipseData.getEllipseHeight() + 5;
        ellipseXCoord = ellipseMiddleXCoord + xCoordoffset;
        gc.fillOval(ellipseXCoord, ellipseYCoord,
                ellipseData.getEllipseWidth(EllipseType.E_0_5),
                ellipseData.getEllipseHeight());

        /*
         * Draw the ellipse labels
         */
        int labelXCoord = textXCoord + 15
                + ellipseData.getEllipseWidth(EllipseType.E_6mn) + 5;
        int labelYCoord = textYCoord + textHeight * 3 + 3;

        gc.drawString(" 6 nm", labelXCoord, labelYCoord, true);

        labelYCoord += ellipseData.getEllipseHeight() + 5;
        gc.drawString(" 3 nm", labelXCoord, labelYCoord, true);

        labelYCoord += ellipseData.getEllipseHeight() + 5;
        gc.drawString("0.5 nm", labelXCoord, labelYCoord, true);
    }

    /**
     * Calculate the X & Y coordinates based on the time and height passed in.
     * The X coordinate will be to the right of the starting time's X
     * coordinate.
     *
     * @param height
     * @param timeInMillis
     * @return
     */
    private Point calcTimeHeightToXY(long timeInMillis, double height) {
        long millis = timeInMillis - startingTimeMillis;

        int xCoord = (int) Math
                .round((millis / millisPerPixel) + LEFT_TIME_LBL_XCOORD);
        int yCoord = (int) Math
                .round(BOTTOM_HLINE_YCOORD - (pixelPerHeight * height));

        Point p = new Point(xCoord, yCoord);

        return p;
    }

    /**
     * Calculate the text width and height.
     */
    private void calcTextWidthHeight() {
        Image image = new Image(display, 100, 100);
        GC gc = new GC(image);
        gc.setFont(textFont);

        textWidth = gc.getFontMetrics().getAverageCharWidth();
        textHeight = gc.getFontMetrics().getHeight();
        textHgtYOffset = (int) Math.round(textHeight / 2.0);

        gc.dispose();
        image.dispose();
    }

    private static int getSecondsPastMidnight(Calendar cal) {
        return cal.get(Calendar.HOUR_OF_DAY) * 3600
                + cal.get(Calendar.MINUTE) * 60 + cal.get(Calendar.SECOND);
    }

    /**
     * Create the time labels at the bottom.
     */
    private void createTimeLabels() {
        Date d = timeHeightCB.getDialogTime();
        Calendar cal = TimeUtil.newGmtCalendar();
        cal.setTime(d);

        if ((graphData != null) && !graphData.isEmpty()) {
            Map.Entry<Long, DMDTableDataRow> lastEntry = graphData.lastEntry();

            Calendar volScanCal = TimeUtil.newGmtCalendar();
            volScanCal.setTimeInMillis(lastEntry.getKey());

            DMDTableDataRow dataRow = lastEntry.getValue();
            List<Double> timeList = dataRow.getTimeHeightTimes(); // seconds
                                                                  // past
                                                                  // midnight
            if ((timeList != null) && (!timeList.isEmpty())) {
                int seconds = (int) (double) Collections.max(timeList);

                if (seconds < getSecondsPastMidnight(volScanCal)) {
                    volScanCal.add(Calendar.DAY_OF_MONTH, 1);
                }
                volScanCal.set(Calendar.HOUR_OF_DAY, 0);
                volScanCal.set(Calendar.MINUTE, 0);
                volScanCal.set(Calendar.SECOND, 0);
                volScanCal.set(Calendar.MILLISECOND, 0);
                volScanCal.add(Calendar.SECOND, seconds);
                cal = volScanCal;
            }
        }

        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        cal.add(Calendar.SECOND, GRAPH_RIGHT_MARGIN_SECONDS);
        long endMillis = cal.getTimeInMillis();

        startingTimeMillis = endMillis
                - GRAPH_WIDTH_SECONDS * TimeUtil.MILLIS_PER_SECOND;
        millisPerPixel = (endMillis - startingTimeMillis)
                / (RIGHT_TIME_LBL_XCOORD - LEFT_TIME_LBL_XCOORD);
    }

    /**
     * Find the maximum height value for the graph.
     *
     * @param largestHeight
     *            Largest height from the graph data.
     * @return Maximum height value.
     */
    private int findMaxHeightValue(double largestHeight) {
        int minHeightValue = 2;
        int roundedNum = (int) Math.round(largestHeight + 0.5);

        if (roundedNum % 2 != 0) {
            ++roundedNum;
        }

        if (roundedNum < minHeightValue) {
            return minHeightValue;
        }

        return roundedNum;
    }

    /**
     * Set the draw settings.
     *
     * @param drawSettings
     */
    public void setDrawSettings(DrawSettings drawSettings) {
        this.drawSettings = drawSettings;
        canvas.redraw();
    }

    /**
     * Set the graph data and calculate all of the necessary item so the data
     * can be drawn.
     *
     * @param data
     *            Time-Height graph data.
     */
    public void setGraphData(NavigableMap<Long, DMDTableDataRow> data) {
        if (data == null) {
            canvas.redraw();
            return;
        }

        redrawFlag = false;
        graphData = data;

        allAngles.clear();
        elevLinesAngMap.clear();
        volScanLines.clear();

        /*
         * Update the times for the labels on the graph. As time moves forward
         * the time will change on the graph. This will change the starting
         * time.
         */
        createTimeLabels();

        /*
         * Verify the time in the graphData can be drawn on the graph. If the
         * time is less than the start time in milliseconds then remove the data
         * from the graphData map.
         */
        List<Long> invalidTimes = new ArrayList<>();
        double maxHeight = 2.0;
        Long lastGraph = 0L;

        for (Entry<Long, DMDTableDataRow> entry : graphData.entrySet()) {
            Long l = entry.getKey();
            if (entry.getValue() == null) {
                invalidTimes.add(l);
                continue;
            }

            /*
             * TODO: Was startingTimeMillis >
             * graphData.get(l).getTime().getValidTime().getTimeInMillis(), but
             * that is currently not the volume scan time.
             */
            if ((startingTimeMillis > l) && (!invalidTimes.contains(l))) {
                invalidTimes.add(l);
                lastGraph = l;
                continue;
            }

            DMDTableDataRow tmpData = graphData.get(l);
            List<Double> times = tmpData.getTimeHeightTimes();
            List<Double> heightList = tmpData.getTimeHeightHeight();
            List<Double> elevAnglesIndices = tmpData
                    .getTimeHeightElevationIndexes();

            if ((heightList.isEmpty() || elevAnglesIndices.isEmpty())
                    && (!invalidTimes.contains(l))) {
                invalidTimes.add(l);
                lastGraph = l;
                continue;
            }
            // Check for MRLE/SAILS scan times
            if (lastGraph != 0L) {
                DMDTableDataRow lastTmpData = graphData.get(lastGraph);
                List<Double> lastTimes = lastTmpData.getTimeHeightTimes();

                int seconds = 0;
                int secondsPrev = 0;
                int secondsLast = 0;

                if ((times != null) && (!times.isEmpty()) && (lastTimes != null)
                        && (!lastTimes.isEmpty())) {
                    for (int i = 1; i < times.size(); i++) {
                        if (i == 5) {
                            // MRLE/SAILS do not go above 1.8 degrees
                            break;
                        }
                        int tmpCur = (int) (double) times.get(i);
                        int tmpPrev = (int) (double) times.get(i - 1);

                        if ((tmpCur < tmpPrev) && (lastTimes.size() >= i)) {
                            seconds = tmpCur;
                            secondsPrev = tmpPrev;
                            secondsLast = (int) (double) lastTimes.get(i);
                            break;
                        }
                    }

                    // MRLE or SAILS found; modify times list to reflect new
                    // times
                    if ((seconds == secondsLast) && (seconds < secondsPrev)) {
                        if (!invalidTimes.contains(lastGraph)) {
                            invalidTimes.add(lastGraph);
                        }
                    }
                }
            }

            // Find the maximum height.
            for (Double height : heightList) {
                if (KM_TO_KFT.convert(height).doubleValue() > maxHeight) {
                    maxHeight = KM_TO_KFT.convert(height).doubleValue();
                }
            }

            lastGraph = l;
        }

        // Remove the times that are earlier than the start time.
        if (invalidTimes != null) {
            for (Long l : invalidTimes) {
                graphData.remove(l);
            }
        }

        recalcHeightVarables(maxHeight);

        /*
         * Loop of the data and do the following: -- Store the paths for drawing
         * the vol scan pols -- Store all of the available elevation angles --
         * Find the largest height
         */
        List<Point> volScanPoints = new ArrayList<>();
        Calendar volScanCal = TimeUtil.newGmtCalendar();
        for (Entry<Long, DMDTableDataRow> entry : graphData.entrySet()) {
            DMDTableDataRow dataRow = entry.getValue();

            List<Double> elevAngles = dataRow.getTimeHeightElevationAngles();
            List<Double> elevAnglesIndices = dataRow
                    .getTimeHeightElevationIndexes();

            // seconds past midnight
            List<Double> timeList = dataRow.getTimeHeightTimes();

            List<Double> heightList = dataRow.getTimeHeightHeight();

            Point xyCoord;
            volScanPoints.clear();
            int idx = 0;
            int maxIdx = elevAnglesIndices.size();
            if (timeList.size() < elevAnglesIndices.size()) {
                maxIdx = timeList.size();
            }

            volScanCal.setTimeInMillis(entry.getKey());
            volScanCal.set(Calendar.HOUR_OF_DAY, 0);
            volScanCal.set(Calendar.MINUTE, 0);
            volScanCal.set(Calendar.SECOND, 0);
            volScanCal.set(Calendar.MILLISECOND, 0);
            long volScanMidnight = volScanCal.getTimeInMillis();

            for (Double angleIdx : elevAnglesIndices) {
                if (idx >= maxIdx) {
                    continue;
                }
                // Calculate the time offset of each angle by adding
                // the time value to the midnight time value
                Calendar cal = Calendar
                        .getInstance(TimeZone.getTimeZone("GMT"));
                Double seconds = timeList.get(angleIdx.intValue());

                cal.setTimeInMillis(volScanMidnight
                        + (long) (double) seconds * TimeUtil.MILLIS_PER_SECOND);
                xyCoord = calcTimeHeightToXY(cal.getTimeInMillis(),
                        KM_TO_KFT.convert(heightList.get(idx).doubleValue()));
                if ((xyCoord.x < LEFT_TIMELINE_XCOORD)
                        || (xyCoord.y < TOP_HLINE_YCOORD)) {
                    break;
                }
                if (xyCoord.x < RIGHT_TIMELINE_XCOORD) {
                    volScanPoints.add(xyCoord);
                }
                double angle = elevAngles.get(angleIdx.intValue());
                if (!elevLinesAngMap.containsKey(angle)) {
                    elevLinesAngMap.put(angle, new ArrayList<Integer>());
                }
                if (!elevLinesAngMap.get(angle).contains(xyCoord.x)
                        && (xyCoord.x < RIGHT_TIMELINE_XCOORD)) {
                    elevLinesAngMap.get(angle).add(xyCoord.x);
                    elevLinesAngMap.get(angle).add(xyCoord.y);
                }
                idx++;
            }

            int[] tmpArray = new int[volScanPoints.size() * 2];

            for (int i = 0; i < volScanPoints.size(); i++) {
                if (volScanPoints.get(i).x < RIGHT_TIMELINE_XCOORD) {
                    tmpArray[i * 2] = volScanPoints.get(i).x;
                    tmpArray[i * 2 + 1] = volScanPoints.get(i).y;
                }
            }
            volScanLines.add(tmpArray);
        }

        /*
         * Redraw the graph
         */
        redrawFlag = true;
        canvas.redraw();

        // printGraphData();
    }

    /**
     * Redraw the canvas.
     */
    public void redrawCanvas() {
        canvas.redraw();
    }
}
