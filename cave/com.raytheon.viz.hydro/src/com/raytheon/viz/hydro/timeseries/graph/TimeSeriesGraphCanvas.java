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
package com.raytheon.viz.hydro.timeseries.graph;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Region;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDisplayDlg;
import com.raytheon.viz.hydro.timeseries.util.GraphData;
import com.raytheon.viz.hydro.timeseries.util.ScaleManager;
import com.raytheon.viz.hydro.timeseries.util.StageDischargeUtils;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesPoint;
import com.raytheon.viz.hydro.timeseries.util.TraceData;
import com.raytheon.viz.hydro.util.HydroUtils;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Graphing library for the Time Series Viewer.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 12, 2008  1519     mpduff    Initial creation
 * Jan 26, 2011  5557     bkowal    Finished the implementation of "Reverse
 *                                  Video" printing.
 * Feb 03, 2011  8085     mpduff    Modified the trace line to circle around the
 *                                  point.
 * Apr 18, 2011  8963     jpiatt    Removed Left Scale call to scale manager.
 * Jul 12, 2011  9709     djingtao  draw right Y axis for showPP is true. add
 *                                  new function adjust_pcymax()
 * Aug 10, 2011  10457    djingtao  allow the red rubberband box to be drawn for
 *                                  setMissing in Edit
 * Jul 24, 2012  15195    mpduff    Fix x axis scales.
 * Nov 06, 2012  15399    wkwock    Fix refine the plot algorithm and sampling
 *                                  algorithm
 * May 06, 2013  1976     mpduff    Code cleanup
 * Sep 05, 2013  2332     lvenable  Fixed memory leaks.
 * Nov 18, 2015  5073     skorolev  Fixed y2pixel.
 * Oct 24, 2016  5955     randerso  Make graph scale better with DPI
 * Dec 05, 2016  6017     randerso  Fix y offset when moving points on the graph while zoomed
 *
 * </pre>
 *
 * @author mpduff
 */

public class TimeSeriesGraphCanvas extends Canvas {
    /**
     * Font for the canvas.
     */
    protected Font canvasFont;

    /**
     * Canvas width.
     */
    protected int canvasWidth;

    /**
     * Canvas height.
     */
    protected int canvasHeight;

    /** Graph top border in pixels. */
    protected int graphBorderTop;

    /** Graph bottom border in pixels. */
    protected int graphBorderBottom;

    /** Right side graph border in pixels. */
    protected int graphBorderRight;

    /** Left side graph border in pixels. */
    protected int graphBorderLeft;

    /**
     * Graph Area Width in pixels.
     */
    protected int graphAreaWidth;

    /**
     * Graph Area Height in pixels.
     */
    protected int graphAreaHeight;

    /**
     * The horizontal span of the graph Valid values are 1-6
     */
    protected int horizontalSpan = 3;

    /**
     * The vertical span of the graph Valid values are 1-2
     */
    protected int verticalSpan = 2;

    /** The trace line width */
    protected int lineWidth;

    /** The rectangle that is the rubber band bounding box */
    protected Rectangle boundingBox;

    /** The rectangle that is delete point selection */
    protected Rectangle deleteRect = null;

    /** The rectangle that is setMissing point selection */
    protected Rectangle setMissingRect = null;

    /** The first x coordinate of the bounding box */
    protected int rubberBandX1;

    /** The first y coordinate of the bounding box */
    protected int rubberBandY1;

    /** The second x coordinate of the bounding box */
    protected int rubberBandX2;

    /** The second y coordinate of the bounding box */
    protected int rubberBandY2;

    /** The height of the font in pixels */
    protected int fontHeight = -999;

    /** The width of the average character in pixels */
    protected int fontWidth = -999;

    /** Height of font above baseline */
    protected int fontAscent = -999;

    /** The Cross Hair cursor */
    protected Cursor crossHairCursor;

    /** The Hand cursor */
    protected Cursor handCursor;

    /** The North South Arrow Cursor */
    protected Cursor northSouthCursor;

    /** Flag to display the gridlines */
    protected boolean displayGridLines = true;

    /** The rectangle that is the graph area of the canvas */
    protected Rectangle graphAreaRectangle;

    /** Flag for existence of rating curve */
    protected boolean ratingCurveExist = false;

    /* Flag for selection of 1hr PC as PP */
    protected boolean showPP = false;

    protected final List<Region> precipRegions = new ArrayList<Region>();

    protected final List<List<Region>> precipPointList = new ArrayList<List<Region>>();

    protected int currentX;

    protected int currentY;

    /**
     * Current trace color.
     */
    protected Color currentTraceColor = null;

    /** The parent composite */
    protected Composite parentComp = null;

    /** The dialog containing this object */
    private TimeSeriesDisplayDlg parentDialog = null;

    protected int bottomBorder;

    protected int rightBorder;

    protected ScaleManager scalingManager = null;

    protected double yminChange = 0;

    protected double ymaxChange = 0;

    protected double newyminChange = 0;

    protected double newymaxChange = 0;

    protected double yinc = 0;

    /**
     *
     * @param parent
     *            The parent composite
     * @param style
     *            The window style
     */
    public TimeSeriesGraphCanvas(Composite parent, int style) {
        super(parent, style);
        parentComp = parent;
        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        Display display = parentComp.getDisplay();
        Point dpi = display.getDPI();

        if (fontHeight <= 0) {
            GC gc = new GC(display);
            FontMetrics fm = gc.getFontMetrics();
            fontHeight = fm.getHeight();
            fontWidth = fm.getAverageCharWidth();
            fontAscent = fm.getAscent();
        }

        /*
         * Compute constants in inches * dpi so they scale correctly.
         */
        canvasWidth = (int) (4.7 * dpi.x);
        canvasHeight = (int) (5.6 * dpi.y);

        graphBorderTop = fontHeight * 5;
        graphBorderBottom = fontHeight * 3;
        graphBorderRight = (int) (1.0 * dpi.x);
        graphBorderLeft = (int) (0.7 * dpi.x);
        graphAreaWidth = canvasWidth - graphBorderLeft - graphBorderRight;
        graphAreaHeight = canvasHeight - graphBorderTop - graphBorderBottom;
        bottomBorder = canvasHeight - graphBorderBottom;

        this.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (currentTraceColor != null
                        && currentTraceColor.isDisposed() == false) {
                    currentTraceColor.dispose();
                }
            }
        });
    }

    /**
     * Draws the Y axis (horizontal lines)
     *
     * @param e
     *            The Paint Event
     * @param gd
     *            The Graph Data
     * @param label
     *            The label for the axis
     */
    protected void drawYAxis(GC gc, GraphData gd, String label) {
        int swtColor = SWT.COLOR_WHITE;
        if (this.parentDialog.isInverseVideo()) {
            swtColor = SWT.COLOR_BLACK;
        }

        double yMin = gd.getYmin();
        double yMax = gd.getYmax();
        double yDiff = (gd.getYmax() - gd.getYmin());

        ratingCurveExist = false;
        ScaleManager scale2Mgr = null;

        /* Is this PC data */
        showPP = gd.getShowpp();

        // Right Scale
        if (showPP) {
            // scale the right axis when showPP is true
            scale2Mgr = new ScaleManager(0, newymaxChange - newyminChange);
        } else {
            scale2Mgr = new ScaleManager(0, yMax - yMin);
        }

        /* Maximum discharge value */
        double maxDischarge = -999;

        gd.setDisplayFlowUnit(false);
        String pe = gd.getTraceData(0).getPe().toUpperCase();

        /* Does a rating table exist for this site? */
        String lid = gd.getTraces().get(0).getLid();
        if (!ratingCurveExist && (pe.startsWith("H") || pe.startsWith("Q"))) {
            ratingCurveExist = StageDischargeUtils.checkRatingTable(lid);
        }

        if (pe.startsWith("Q")) {
            maxDischarge = gd.getYmax();
        } else {
            if (ratingCurveExist) {
                maxDischarge = StageDischargeUtils.stage2discharge(lid,
                        gd.getYmax());
            }
        }

        int numberTicks = scalingManager.getMajorTickCount();
        int numberTicks2 = scale2Mgr.getMajorTickCount();

        gc.setForeground(parentComp.getDisplay().getSystemColor(swtColor));

        double data = gd.getYmin();
        double showPPData = 0;
        double inc = scalingManager.getMajorTickIncrement();
        double inc2 = scale2Mgr.getMajorTickIncrement();

        gd.setYmax(scalingManager.getMaxScaleValue());
        gd.setYmin(scalingManager.getMinScaleValue());
        gd.setYmin2(showPPData);
        gd.setYmax2(newymaxChange);

        NumberFormat dischargeFormat = new DecimalFormat("0.0");
        int dx = 5;
        int y = 0;
        int showPPDatay = 0;

        String fmt = "0";
        if (yDiff < 1.0) {
            fmt = "0.00";
        } else if (yDiff < 10.0) {
            fmt = "0.0";
        } else if ((yDiff >= 10.0) && (yDiff <= 100.0)) {
            fmt = "0.0";
        }
        NumberFormat formatter = new DecimalFormat(fmt);

        dx = fontWidth;
        for (int i = 0; i < numberTicks; i++) {
            y = y2pixel(gd, data);
            if (displayGridLines) {
                gc.setForeground(
                        parentComp.getDisplay().getSystemColor(SWT.COLOR_GRAY));
                gc.setLineStyle(SWT.LINE_DOT);
                gc.drawLine(graphBorderLeft, graphBorderTop + y, rightBorder,
                        graphBorderTop + y);

                gc.setLineStyle(SWT.LINE_SOLID);

                gc.setForeground(
                        parentComp.getDisplay().getSystemColor(swtColor));
            }

            /* Draw the tick marks and values on left axis */
            gc.drawLine(graphBorderLeft, graphBorderTop + y,
                    graphBorderLeft - dx, graphBorderTop + y);

            String dataStr = formatter.format(data);
            Point dataExtent = gc.textExtent(dataStr);
            gc.drawText(dataStr,
                    graphBorderLeft - dx - dataExtent.x - fontWidth,
                    graphBorderTop + y - fontHeight / 2);

            /*
             * Draw the tick marks and values on right axis for ratingCurveExist
             */
            if (ratingCurveExist) {
                double value = 0;
                if (pe.toUpperCase().startsWith("H")) {
                    value = StageDischargeUtils.stage2discharge(lid, data);
                    if ((value < 0)) {
                        value = 0.0;
                    } else if (maxDischarge >= 10000) {
                        value /= 1000;
                    }
                } else if (pe.toUpperCase().startsWith("Q")) {
                    double stageValue = StageDischargeUtils
                            .getStageFromDischarge(gd, data);
                    if (stageValue != HydroConstants.MISSING_VALUE) {
                        value = stageValue;
                    } else {
                        break;
                    }
                }

                gc.drawLine(rightBorder, graphBorderTop + y, rightBorder + dx,
                        graphBorderTop + y);

                gc.drawText(dischargeFormat.format(value),
                        rightBorder + dx + fontWidth,
                        graphBorderTop + y - fontHeight / 2, true);
            }

            data += inc;
        }

        /* draw right y axis when showPP is true */
        if (showPP) {
            for (int i = 0; i < numberTicks2; i++) {
                showPPDatay = secondaryY2pixel(gd, showPPData);

                gc.drawLine(rightBorder, graphBorderTop + showPPDatay,
                        rightBorder + dx, graphBorderTop + showPPDatay);

                gc.setForeground(parentComp.getDisplay()
                        .getSystemColor(SWT.COLOR_YELLOW));
                gc.drawText(dischargeFormat.format(showPPData),
                        rightBorder + fontWidth,
                        graphBorderTop + showPPDatay - 7, true);

                gc.setForeground(parentComp.getDisplay()
                        .getSystemColor(SWT.COLOR_WHITE));

                showPPData += inc2;
            }
        }

        if (ratingCurveExist) {
            labelRightAxis(gc, pe, maxDischarge);
        }

        labelLeftAxis(gc, pe, maxDischarge, label);
    }

    /**
     * adjust min/max on Y when showPP is true
     */
    protected void adjust_pcymax() {
        double dminmax;
        dminmax = ymaxChange - yminChange;

        if (dminmax <= 0.5) {
            newyminChange = 0.0;
            newymaxChange = (int) ymaxChange + 0.5;
            yinc = 0.1;
        } else if (dminmax <= 2.0) {
            newyminChange = (int) yminChange;
            newymaxChange = (int) ymaxChange + 1.0;
            yinc = 0.5;
        } else {
            newyminChange = (int) yminChange;
            newymaxChange = (int) ymaxChange + 1.0;
            yinc = 1.0;
        }

    }

    /**
     * Draws the X Axis
     *
     * @param e
     *            The Paint Event
     * @param gd
     *            The Graph Data
     */
    protected void drawXAxis(GC gc, GraphData gd) {
        int minorTicks = 1; /* Minor ticks set 1 hour default */
        int majorTicks = 6; /* Major ticks set 6 hour default */

        long ndays = (gd.getXMax().getTime() / HydroConstants.MILLIS_PER_MINUTE
                - gd.getXMin().getTime() / HydroConstants.MILLIS_PER_MINUTE)
                / HydroConstants.MINUTES_PER_DAY;

        if (ndays == 0) {
            ndays = 1;
        }

        boolean zHrDisplay = true;
        int daysCount = 1;
        int daysSkip = 1;

        if (ndays > 10) {
            zHrDisplay = false;
            daysSkip = (int) (ndays / 10);
            majorTicks = (int) (ndays / 10) * 24;
            if (majorTicks == 0) {
                majorTicks = 1;
            }
            minorTicks = majorTicks / 2;
            if (minorTicks == 0) {
                minorTicks = 1;
            }
            daysCount = daysSkip;
        }

        // Check canvas width. if small then need to skip extra days
        if (this.canvasWidth < 500) {
            daysSkip++;
        }

        int x;
        int dy;
        int dx;
        long startMillis = ((long) Math
                .ceil(gd.getXMin().getTime() / HydroConstants.MILLIS_PER_HOUR))
                * HydroConstants.MILLIS_PER_HOUR;
        long endMillis = gd.getXMax().getTime();

        TimeZone gmt = TimeZone.getTimeZone("GMT");
        Calendar c = Calendar.getInstance(gmt);

        SimpleDateFormat hourFormat = new SimpleDateFormat("HH");
        hourFormat.setTimeZone(gmt);

        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd");
        dateFormat.setTimeZone(gmt);

        for (long i = startMillis; i <= endMillis; i += HydroConstants.MILLIS_PER_HOUR) {
            x = x2pixel(gd, i + 59000); // 59 seconds for better sampling
            dy = fontHeight / 2;
            c.setTimeInMillis(i);
            String hourStr = hourFormat.format(c.getTime());
            Point hourExtent = gc.textExtent(hourStr);
            dx = hourExtent.x / 2;
            int hour = c.get(Calendar.HOUR_OF_DAY);
            if (hour == 0) {
                dy = fontHeight;
                if (daysCount++ % daysSkip == 0) {
                    gc.drawText(hourStr, x + graphBorderLeft - dx,
                            bottomBorder + fontHeight);

                    String dateStr = dateFormat.format(c.getTime());
                    Point dateExtent = gc.textExtent(dateStr);
                    gc.drawText(dateStr, x + graphBorderLeft - dateExtent.x / 2,
                            bottomBorder + fontHeight * 2);

                    if (displayGridLines) {
                        gc.setLineStyle(SWT.LINE_DOT);
                        gc.drawLine(x + graphBorderLeft, graphBorderTop,
                                x + graphBorderLeft, bottomBorder);
                        gc.setLineStyle(SWT.LINE_SOLID);
                    }
                } else if (ndays < 8) {
                    gc.drawText(hourStr, x + graphBorderLeft - dx,
                            bottomBorder + fontHeight);

                }
            } else if (hour % majorTicks == 0) {
                /*
                 * Hour annotation
                 */
                dy = (fontHeight * 3) / 4;
                if (ndays < 8 && this.canvasWidth > 450) {
                    gc.drawText(hourStr, x + graphBorderLeft - dx,
                            bottomBorder + fontHeight);
                } else if (hour == 12) {
                    gc.drawText(hourStr, x + graphBorderLeft - dx,
                            bottomBorder + fontHeight);

                }
            }

            /* ******************************** */
            /* major and minor ticks annotation */
            /* ******************************** */
            if ((c.get(Calendar.HOUR_OF_DAY) % minorTicks) == 0) {
                gc.drawLine(x + graphBorderLeft, bottomBorder,
                        x + graphBorderLeft, bottomBorder + dy);
            }
        }

        if (zHrDisplay) {
            gc.drawText("(Z)", rightBorder + 10, bottomBorder + fontHeight);
        }

        /* ********************************************* */
        /* Draw reference vertical line at present time */
        /* ********************************************* */
        Date d = SimulatedTime.getSystemTime().getTime();
        if ((d.getTime() > gd.getXMin().getTime())
                && (d.getTime() < gd.getXMax().getTime())) {
            int curTimeLoc = graphBorderLeft + x2pixel(gd, d.getTime());
            if ((curTimeLoc < (canvasWidth - graphBorderTop))
                    && (curTimeLoc > graphBorderLeft)) {
                gc.setLineStyle(SWT.LINE_DOT);
                gc.setLineWidth(3);
                gc.drawLine(curTimeLoc, graphBorderTop, curTimeLoc,
                        bottomBorder);
                gc.setLineStyle(SWT.LINE_SOLID);
                gc.setLineWidth(1);
            }
        }
    }

    /**
     * convert real Y value to pixel value.
     *
     * @param gd
     *            The Graph Data
     * @param y
     *            The y data value to convert
     * @return The y pixel value
     */
    protected int y2pixel(GraphData gd, double y) {
        if (y == HydroConstants.MISSING_VALUE) {
            return gd.getH() + graphBorderTop;
        }
        double yDiff = gd.getYmax() - gd.getYmin();
        double yValue = (graphAreaHeight / yDiff) * (y - gd.getYmin());
        if (yValue < 0) {
            return graphAreaHeight;
        } else {
            return (int) (graphAreaHeight - Math.round(yValue));
        }
    }

    /**
     * convert pixel value to real Y value
     *
     * @param gd
     *            The GraphData object
     * @param ypix
     *            The y pixel value
     * @return The y value
     */
    protected double pixel2y(GraphData gd, int ypix) {
        double yMin = gd.getYmin();
        double yMax = gd.getYmax();
        double ydiff = yMax - yMin;
        double pixPerUnit = graphAreaHeight / ydiff;

        return (yMax - ((ypix - graphBorderTop) / pixPerUnit));
    }

    /**
     * convert real X value to pixel value.
     *
     * @param gd
     *            The Graph Data
     * @param x
     *            The X value to convert
     * @return The pixel value
     */
    protected int x2pixel(GraphData gd, long x) {
        long xMin = gd.getXMin().getTime();
        long xMax = gd.getXMax().getTime();
        long xDiff = xMax - xMin;

        long millisPerPixel = xDiff / graphAreaWidth;

        float xValue = (x - xMin) / millisPerPixel;

        return Math.round(xValue);
    }

    /**
     * convert pixel value to real time value.
     *
     * @param gd
     *            The Graph Data object
     * @param xpix
     *            The x pixel value
     * @return The date value of the x pixel
     */
    protected Date pixel2x(GraphData gd, int xpix) {
        long xMin = gd.getXMin().getTime();
        long xMax = gd.getXMax().getTime();
        long xDiff = xMax - xMin;
        double millisPerPixel = xDiff / graphAreaWidth;
        long millisTime = (long) (xpix * millisPerPixel) + xMin;

        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis(millisTime);

        return cal.getTime();
    }

    /**
     * convert real Y value to pixel value for right axis.
     *
     * @param gd
     *            The Graph Data
     * @param y
     *            The y data value to convert
     * @return The y pixel value
     */
    protected int secondaryY2pixel(GraphData gd, double y) {
        if (y == HydroConstants.MISSING_VALUE) {
            return gd.getH() + graphBorderTop;
        }

        double yDiff = gd.getYmax2() - gd.getYmin2();
        double yValue = (graphAreaHeight / yDiff) * (y - gd.getYmin2());

        return (int) (graphAreaHeight - Math.round(yValue));
    }

    /**
     * Labels the left axis.
     *
     * @param e
     *            PaintEvent
     * @param pe
     *            Physical Element
     * @param maxDischarge
     *            Maximum Discharge
     * @param label
     *            Label to display
     */
    private void labelLeftAxis(GC gc, String pe, double maxDischarge,
            String label) {
        if (pe.toUpperCase().startsWith("Q")) {
            if (maxDischarge >= 10000.0) {
                label = "Total Discharge in KCFS";
            } else {
                label = "Total Discharge in CFS";
            }
        } else if (pe.toUpperCase().startsWith("H")) {
            label = "River Stage in Feet";
        } else {
            // Use the label passed in
        }

        int yoffset = ((graphAreaHeight + graphBorderTop + graphBorderBottom)
                - fontAscent * label.length()) / 2;

        for (int i = 0; i < label.length(); i++) {
            gc.drawText(Character.toString(label.charAt(i)), fontWidth,
                    fontAscent * i + yoffset, true);
        }
    }

    /**
     * Label the right axis.
     *
     * @param e
     *            PaintEvent
     * @param pe
     *            Physical Element
     * @param maxDischarge
     *            Maximum Discharge
     */
    private void labelRightAxis(GC gc, String pe, double maxDischarge) {
        int yoffset;
        String label = "";

        if (ratingCurveExist) {
            label = "River Stage in Feet";
            if (pe.toUpperCase().startsWith("H")) {
                if (maxDischarge >= 10000.0) {
                    label = "Total Discharge in KCFS";
                } else {
                    label = "Total Discharge in CFS";
                }
            }
        }

        yoffset = ((graphAreaHeight + graphBorderTop + graphBorderBottom)
                - fontAscent * label.length()) / 2;

        for (int i = 0; i < label.length(); i++) {
            gc.drawText(Character.toString(label.charAt(i)), graphAreaWidth
                    + graphBorderLeft + graphBorderRight - fontWidth * 2,
                    fontAscent * i + yoffset, true);
        }
    }

    /**
     * Resize the graph when the window size changes
     *
     * @param rect
     *            The new graph size
     */
    public void resizeGraph(Rectangle rect) {
        canvasHeight = rect.height / 2 * verticalSpan;
        canvasWidth = rect.width / 6 * horizontalSpan;

        graphAreaWidth = canvasWidth - graphBorderLeft - graphBorderRight;
        graphAreaHeight = canvasHeight - graphBorderTop - graphBorderBottom;
        bottomBorder = canvasHeight - graphBorderBottom;
        rightBorder = canvasWidth - graphBorderRight;
    }

    /**
     * Draws the "rubber band" bounding box on the canvas
     *
     * @param e
     *            PaintEvent
     * @param mouseDown
     *            Is the mouse down, true if mouse is down
     */
    protected void drawRubberBand(GC gc, boolean mouseDown) {
        gc.setForeground(parentComp.getDisplay().getSystemColor(SWT.COLOR_RED));
        gc.setLineWidth(2);
        if ((boundingBox != null) && parentDialog.isZoomAction() && mouseDown) {
            gc.drawRectangle(boundingBox);
        } else if ((deleteRect != null) && mouseDown) {
            gc.drawRectangle(deleteRect);
        } else if ((setMissingRect != null) && mouseDown) {
            gc.drawRectangle(setMissingRect);
        }
    }

    /**
     * Display the flood category lines on the graph
     *
     * @param e
     *            PaintEvent
     * @param graphData
     *            The GraphData object
     */
    protected void displayFloodCatLines(GC gc, GraphData graphData) {
        int y = 0;

        gc.setLineWidth(2);

        /* Action stage/flow */
        if ((graphData.getActionStage() >= 0)
                || (graphData.getActionFlow() >= 0)) {
            gc.setForeground(new Color(parentComp.getDisplay(),
                    HydroUtils.getColor("Yellow")));

            double value;
            if (graphData.getTraceData(0).getPe().toUpperCase()
                    .startsWith("H")) {
                value = graphData.getActionStage();
            } else {
                value = graphData.getActionFlow();
            }

            y = y2pixel(graphData, value);

            if ((y <= (graphData.getY() + graphData.getH()))
                    && (y >= graphData.getY())) {
                gc.drawLine(graphBorderLeft, y + graphBorderTop,
                        graphBorderLeft + graphAreaWidth, y + graphBorderTop);
            }
        }

        /* Flood/Flow stage */
        if ((graphData.getFloodStage() >= 0)
                || (graphData.getFloodFlow() >= 0)) {
            gc.setForeground(new Color(parentComp.getDisplay(),
                    HydroUtils.getColor("Orange")));

            double value;
            if (graphData.getTraceData(0).getPe().toUpperCase()
                    .startsWith("H")) {
                value = graphData.getFloodStage();
            } else {
                value = graphData.getFloodFlow();
            }

            y = y2pixel(graphData, value);

            if ((y <= (graphData.getY() + graphData.getH()))
                    && (y >= graphData.getY())) {
                gc.drawLine(graphBorderLeft, y + graphBorderTop,
                        graphBorderLeft + graphAreaWidth, y + graphBorderTop);
            }
        }

        /* Minor stage/flow */
        if ((graphData.getMinorStage() >= 0)
                || (graphData.getMinorFlow() >= 0)) {
            gc.setForeground(new Color(parentComp.getDisplay(),
                    HydroUtils.getColor("Orange")));

            double value;
            if (graphData.getTraceData(0).getPe().toUpperCase()
                    .startsWith("H")) {
                value = graphData.getMinorStage();
            } else {
                value = graphData.getMinorFlow();
            }

            y = y2pixel(graphData, value);

            if ((y <= (graphData.getY() + graphData.getH()))
                    && (y >= graphData.getY())) {
                gc.drawLine(graphBorderLeft, y + graphBorderTop,
                        graphBorderLeft + graphAreaWidth, y + graphBorderTop);
            }
        }

        /* Moderate stage/flow */
        if ((graphData.getModerateStage() >= 0)
                || (graphData.getModerateFlow() >= 0)) {
            gc.setForeground(new Color(parentComp.getDisplay(),
                    HydroUtils.getColor("Red")));

            double value;
            if (graphData.getTraceData(0).getPe().toUpperCase()
                    .startsWith("H")) {
                value = graphData.getModerateStage();
            } else {
                value = graphData.getModerateFlow();
            }

            y = y2pixel(graphData, value);

            if ((y <= (graphData.getY() + graphData.getH()))
                    && (y >= graphData.getY())) {
                gc.drawLine(graphBorderLeft, y + graphBorderTop,
                        graphBorderLeft + graphAreaWidth, y + graphBorderTop);
            }
        }

        /* Major stage/flow */
        if ((graphData.getMajorStage() >= 0)
                || (graphData.getMajorFlow() >= 0)) {
            gc.setForeground(new Color(parentComp.getDisplay(),
                    HydroUtils.getColor("Magenta")));

            double value;
            if (graphData.getTraceData(0).getPe().toUpperCase()
                    .startsWith("H")) {
                value = graphData.getMajorStage();
            } else {
                value = graphData.getMajorFlow();
            }

            y = y2pixel(graphData, value);

            if ((y <= (graphData.getY() + graphData.getH()))
                    && (y >= graphData.getY())) {
                gc.drawLine(graphBorderLeft, y + graphBorderTop,
                        graphBorderLeft + graphAreaWidth, y + graphBorderTop);
            }
        }

        gc.setLineWidth(1);
        gc.setForeground(currentTraceColor);
    }

    /**
     * Draw the trace on the canvas
     *
     * @param e
     *            The PaintEvent
     * @param dataPts
     *            The data points of the trace
     */
    protected void drawTrace(GC gc, int[] dataPts) {
        if (parentDialog.getPointsMI().getSelection()) {
            /* Draw circles at each data point */
            for (int i = 0; i < dataPts.length; i++) {
                gc.drawOval(dataPts[i] - 3, dataPts[++i] - 3, lineWidth + 4,
                        lineWidth + 4);
            }
            setForegroundColor(gc, SWT.COLOR_RED);
            setBackgroundColor(gc, SWT.COLOR_RED);
            for (int i = 0; i < dataPts.length; i++) {
                gc.fillOval(dataPts[i] - 2, dataPts[++i] - 2, lineWidth + 2,
                        lineWidth + 2);
            }
            setBackgroundColor(gc, SWT.COLOR_BLACK);
        } else if (parentDialog.getLinesMI().getSelection()) {
            gc.setLineWidth(lineWidth);
            gc.drawPolyline(dataPts);
            gc.setLineWidth(1);
        } else {
            gc.setLineWidth(lineWidth);
            gc.drawPolyline(dataPts);
            gc.setLineWidth(2);

            /* Draw circles at each data point */
            for (int i = 0; i < dataPts.length; i++) {
                gc.drawOval(dataPts[i] - 3, dataPts[++i] - 3, lineWidth + 4,
                        lineWidth + 4);
            }
            setForegroundColor(gc, SWT.COLOR_RED);
            setBackgroundColor(gc, SWT.COLOR_RED);
            for (int i = 0; i < dataPts.length; i++) {
                gc.fillOval(dataPts[i] - 2, dataPts[++i] - 2, lineWidth + 2,
                        lineWidth + 2);
            }
            gc.setLineWidth(1);
            setBackgroundColor(gc, SWT.COLOR_BLACK);
        }
    }

    /**
     * Draw the 1hr PC as PP bars.
     *
     * @param gc
     *            The GC
     * @param graphData
     *            The GraphData object
     * @param td
     *            The TraceData object to display as bars
     */
    protected void drawPcBars(GC gc, GraphData graphData, TraceData td) {
        int barWidth = 7;
        int barHeight = 0;
        TimeSeriesPoint[] pointArray = td.getTsData();

        // Set the color
        Color foreground = gc.getForeground();
        Color background = gc.getBackground();
        gc.setForeground(
                parentComp.getDisplay().getSystemColor(SWT.COLOR_YELLOW));
        gc.setBackground(
                parentComp.getDisplay().getSystemColor(SWT.COLOR_YELLOW));

        ArrayList<Region> ppointList = new ArrayList<Region>();

        yminChange = ymaxChange = 0;

        /* calculate the yminChange and ymaxChange */
        for (int i = 0; i < pointArray.length; i++) {
            if (pointArray[i].getY() == HydroConstants.MISSING_VALUE) {
                continue;
            }
            if ((i == pointArray.length - 1) || (pointArray[i + 1]
                    .getY() == HydroConstants.MISSING_VALUE)) {
                continue;
            }

            double change2 = pointArray[i + 1].getY() - pointArray[i].getY();

            if (change2 > ymaxChange) {
                ymaxChange = change2;
            }
        }

        // adjust the yminChange and ymaxChange to newyminChange and
        // newymaxChange
        adjust_pcymax();

        // scale graphData Ymin2/Ymax2 with the new min/max values
        graphData.setYmin2(newyminChange);
        graphData.setYmax2(newymaxChange);

        for (int i = 0; i < pointArray.length; i++) {
            if (pointArray[i].getY() == HydroConstants.MISSING_VALUE) {
                continue;
            }

            if ((i == pointArray.length - 1) || (pointArray[i + 1]
                    .getY() == HydroConstants.MISSING_VALUE)) {
                continue;
            }

            pointArray[i].setPixelX(
                    x2pixel(graphData, pointArray[i].getX().getTime()));

            int x = pointArray[i].getPixelX() + graphBorderLeft;

            if ((x < 100) || (x > graphBorderLeft + graphAreaWidth)) {
                continue;
            }

            int x2 = x2pixel(graphData,
                    pointArray[i].getX().getTime() + 3600000) + graphBorderLeft;

            double change = pointArray[i + 1].getY() - pointArray[i].getY();

            barHeight = graphAreaHeight - secondaryY2pixel(graphData, change);

            if (barHeight < 1) {
                barHeight = 2;
            }

            barWidth = x2 - x;// - 10;
            if (barWidth < 1) {
                barWidth = 1;
            }

            /* Draw the bar */
            Rectangle rect = new Rectangle(x, bottomBorder, barWidth,
                    -barHeight);
            gc.drawRectangle(rect);
            gc.fillRectangle(rect);
        }

        precipPointList.add(ppointList);

        // Reset the colors
        gc.setForeground(foreground);
        gc.setBackground(background);
    }

    /**
     * Swap the corner points of the bounding box if necessary
     *
     * @param x1
     *            Corner 1 x value
     * @param y1
     *            Corner 1 y value
     * @param x2
     *            Corner 2 x value
     * @param y2
     *            Corner 2 y value
     */
    protected void swapPoints(int x1, int y1, int x2, int y2) {
        int tmp;
        tmp = x1;
        x1 = x2;
        x2 = tmp;

        tmp = y1;
        y1 = y2;
        y2 = tmp;
    }

    /**
     * Get the parent dialog containing this canvas
     *
     * @return the dialog
     */
    public TimeSeriesDisplayDlg getDialog() {
        return parentDialog;
    }

    /**
     * Set the parent dialog containing this canvas
     *
     * @param dialog
     *            the dialog to set
     */
    public void setDialog(TimeSeriesDisplayDlg dialog) {
        parentDialog = dialog;
    }

    /**
     * Reset the background color.
     *
     * @param e
     *            PaintEvent
     */
    protected void setBackgroundColor(GC gc, int color) {
        if (parentDialog.isInverseVideo()) {
            gc.setBackground(
                    parentComp.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        } else {
            gc.setBackground(parentComp.getDisplay().getSystemColor(color));
        }
    }

    /**
     * Reset the foreground color.
     *
     * @param e
     *            PaintEvent
     */
    protected void setForegroundColor(GC gc, int color) {
        if (parentDialog.isInverseVideo()) {
            gc.setBackground(
                    parentComp.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        } else {
            gc.setForeground(parentComp.getDisplay().getSystemColor(color));
        }
    }

    protected void drawCrossHairs(GC gc) {
        setForegroundColor(gc, SWT.COLOR_WHITE);
        int lineWidth = gc.getLineWidth();
        gc.setLineWidth(1);

        // horizontal cross bar
        int x1 = graphBorderLeft;
        int y1 = currentY;
        int x2 = rightBorder;
        int y2 = currentY;
        gc.drawLine(x1, y1, x2, y2);

        // vertical cross bar
        x1 = currentX;
        y1 = graphBorderTop;
        x2 = currentX;
        y2 = bottomBorder;
        gc.drawLine(x1, y1, x2, y2);

        gc.setLineWidth(lineWidth);
    }

    /**
     * @param ratingCurveExist
     *            the ratingCurveExist to set
     */
    public void setRatingCurveExist(boolean ratingCurveExist) {
        this.ratingCurveExist = ratingCurveExist;
    }
}