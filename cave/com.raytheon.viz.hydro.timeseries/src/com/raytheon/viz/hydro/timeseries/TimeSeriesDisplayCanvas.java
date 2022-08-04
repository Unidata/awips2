package com.raytheon.viz.hydro.timeseries;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.timeseries.util.GraphData;
import com.raytheon.viz.hydro.timeseries.util.GraphData.HGData;
import com.raytheon.viz.hydro.timeseries.util.GraphInfo;
import com.raytheon.viz.hydro.timeseries.util.GraphInfo.DERIVE_PP;
import com.raytheon.viz.hydro.timeseries.util.GroupData;
import com.raytheon.viz.hydro.timeseries.util.GroupInfo.TraceMode;
import com.raytheon.viz.hydro.timeseries.util.PageData;
import com.raytheon.viz.hydro.timeseries.util.PageInfo;
import com.raytheon.viz.hydro.timeseries.util.StageDischargeUtils;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesPoint;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesPoint.MODE;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesUtil;
import com.raytheon.viz.hydro.timeseries.util.TraceData;
import com.raytheon.viz.hydro.timeseries.util.TraceInfo;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.util.HydroUtils;

/**
 * This class is the canvas where the Time Series Display data will be drawn.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Nov 29, 2007  373         lvenable     Initial creation.
 * Jul 01, 2008  1194        mpduff       Implement graph.
 * Apr 29, 2010  5016/8      mpduff       Changed so the lines and traces stay
 *                                        within the bounds of the graph.
 * Jul 02, 2010  5280        lbousaid     added river name to the graph display
 *                                        also, the flood stage value to the
 *                                        graph when no data available
 * Jul 22, 2010  5967        mpduff       Implemented toggling on/off of traces
 *                                        via right click pop-up menu.
 * Jan 19, 2011  5281        lbousaidi    fixed max,min trace values and added
 *                                        related min and max dates to graph
 *                                        display
 * Jan 24, 2011  7799        bkowal       ensured that the y-axis maximum value
 *                                        would be updated if the flood category
 *                                        y-axis max was greater than the
 *                                        current y-axis max
 * Jan 26, 2011  5557        bkowal       created a new function that would draw
 *                                        the time series canvas within
 *                                        specified boundaries.
 * Jan 31, 2011  5274        bkowal       long-running queries are now done
 *                                        asynchronously so that a status
 *                                        indicator can be displayed and so that
 *                                        the interface is not locked up.
 *                                        Extracted the data retrieval logic
 *                                        from the function that updates the
 *                                        interface with the data retrieved.
 * Feb 07, 2011  6288        lbousaid     Added validGraph flag and findGraph
 *                                        arrayList to list if the graph has
 *                                        trace or no
 * Feb 04, 2011  7758/7759                Added checkbox to pop up menu and
 *                                        added check box selection event to the
 *                                        handler
 * Apr 13, 2011  9006        jpiatt       Corrected getFcstData select statement
 *                                        to have uppercase parameters.
 * Apr 18, 2011  8963        jpiatt       Adjusted call to scaling manager for
 *                                        y-axis.
 * Apr 25, 2011  7759        djingtao     Modify getDataForGraph(),
 *                                        getFcstData() and misc to allow
 *                                        correct graph display when "show
 *                                        latest Forecast only" is on or off
 * Jun 01, 2011  9499        djingtao     update getFcstData()
 * Jun 29, 2011  9669        lbousaidi    removed page flickering during the
 *                                        graph display
 * Jul 01, 2011  9700        djingtao     show latest forecast in group mode
 * Jul 12, 2011  9709        djingtao     add draw "DERIVED PP"
 * Jul 25, 2011  10082       djingtao     modify makeRegions()
 * Aug 10, 2011  10457       djingtao     allow red rubberband box to be draw
 *                                        for setMissing in Edit
 * Mar 27, 2012  14527       wkwock       Fix incomplete time series selection
 *                                        issue
 * Apr 24, 2012  14669       wkwock       Handle invalid color name
 * May 08, 2012  14958       wkwock       Fix overcrowded TS list
 * May 30, 2012  14967       wkwock       Fix incorrect product time
 * Nov 06, 2012  15399       wkwock       Fix refine the plot algorithm and
 *                                        sampling algorithm
 * Nov 06, 2012  15459       lbousaidi    update data when page/up or page/down
 *                                        is pressed without having to click in
 *                                        graph button again.
 * Nov 06, 2012  15400       lbousaidi    Changed logic in buildPointString
 *                                        routine, added discharge2stage to
 *                                        display stage value, also added
 *                                        checking for rating curve for both
 *                                        stage and discharge.
 * Nov 13, 2012  15416       lbousaidi    added a check when the colorname is
 *                                        null and a call to getGroupModeColor
 * Jan 09, 2012  15493       lbousaidi    added code to delete data while
 *                                        zooming when you draw a box
 * Jan 16, 2013  15695       wkwock       Fix popup menu
 * Apr 24, 2013  1921        mpduff       Fix zoom reset to only reset the
 *                                        "active" graph
 * May 06, 2013  1976        mpduff       Refactored Hydro time series data
 *                                        access.
 * May 29, 2013  2016        mpduff       Fix TS Toggle Traces.
 * Sep 05, 2013  2332        lvenable     Fixed memory leaks.
 * Jan 24, 2013  15959       lbousaidi    Swap the corner points of the bounding
 *                                        box when zooming.
 * Oct 22, 2015  13736       xwei         Fixed missing data after zoom, edit, &
 *                                        reset problem
 * Oct 26, 2015  14217       jwu          Removed MAX_TRACES limitation
 * Nov 18, 2015  5073        skorolev     Fixed drawing PP time series.
 * May 06, 2016  5483        dgilling     Change parameter type for
 *                                        handleSelection.
 * Oct 24, 2016  5955        randerso     Make graph scale better with DPI
 * Dec 12, 2016  6017        randerso     Fix y offset in displayed data
 * Jul 18, 2017  18467       xwei         Fix IndexOutOfBoundsException error
 *                                        when moving points of a selected trace
 * Jan 04, 2018  6744        mduff        Don't scale Q* PEs to flood stage.
 * Jan 15, 2018  6971        mduff        Set the min/max point data for all
 *                                        points, not only when zoomed out.
 * Apr 12, 2018  6619        randerso     Keep dur as int. Code cleanup.
 * May 16, 2018  6749        randerso     Fixed graph printing and saving. Code
 *                                        cleanup
 * May 29, 2018  6748        randerso     Major rewrite to use a single canvas
 *                                        for the current page only.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class TimeSeriesDisplayCanvas extends Canvas {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TimeSeriesDisplayCanvas.class);

    private static final String NO_DATA_AVAILABLE = "NO DATA AVAILABLE";

    private static final int MAX_GRAPHS_X = 6;

    private static final int MAX_GRAPHS_Y = 2;

    private static final int EDIT_COLOR = 20;

    private GroupData groupData;

    private Date beginDate;

    private Date endDate;

    private Font canvasFont;

    private int fontHeight;

    private int fontWidth;

    private int graphBorderTop;

    private int graphBorderBottom;

    private int graphBorderRight;

    private int graphBorderLeft;

    /**
     * Constructor
     *
     * @param parent
     * @param groupData
     * @param beginDate
     * @param endDate
     */
    public TimeSeriesDisplayCanvas(Composite parent, GroupData groupData,
            Date beginDate, Date endDate) {
        super(parent, SWT.DOUBLE_BUFFERED);
        this.groupData = groupData;
        this.beginDate = beginDate;
        this.endDate = endDate;

        setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

        canvasFont = new Font(getDisplay(), "Monospace", 9, SWT.NORMAL);
        this.setFont(canvasFont);
        GC gc = new GC(this);
        FontMetrics fm = gc.getFontMetrics();
        fontHeight = fm.getHeight();
        fontWidth = gc.textExtent("0").x;

        // space for lines of text above graph
        graphBorderTop = fontHeight * 5;
        // space for lines of text below graph
        graphBorderBottom = fontHeight * 3;

        // space for text on left and right of graph
        graphBorderRight = fontWidth * 10;
        graphBorderLeft = fontWidth * 10;
        gc.dispose();

        addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                try {
                    drawCanvas(e.gc);
                } catch (Exception ex) {
                    statusHandler.error("Problem Painting Graph", ex);
                }
            }
        });

        addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (canvasFont != null && !canvasFont.isDisposed()) {
                    canvasFont.dispose();
                }
            }
        });
    }

    /**
     * @return an image of the canvas contents
     */
    public Image getImage() {
        Rectangle imageBounds = getBounds();
        Image image = new Image(getDisplay(), imageBounds.width,
                imageBounds.height);

        GC gc = new GC(image);
        gc.setFont(canvasFont);
        gc.setBackground(getBackground());
        gc.fillRectangle(image.getBounds());
        drawCanvas(gc);
        gc.dispose();

        return image;
    }

    private void drawCanvas(GC gc) {
        gc.setAntialias(SWT.OFF);

        /*
         * Draw page numbers and page title if available
         */
        PageData pageData = groupData.getCurrentPage();
        PageInfo pageInfo = pageData.getPageInfo();
        String title = pageInfo.getTitle();

        String pageStr = String.format("Page %d of %d               %s",
                groupData.getCurrentPageNum() + 1, groupData.getNumPages(),
                title);
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_CYAN));
        gc.drawText(pageStr, 3, 0, true);

        for (GraphData graphData : pageData.getGraphDataList()) {
            drawGraph(gc, graphData);
        }
    }

    private void drawGraph(GC gc, GraphData graphData) {
        Rectangle clientArea = getClientArea();

        GraphInfo graphInfo = graphData.getGraphInfo();
        int graphPos = graphInfo.getGraphPos() - 1;
        int x = clientArea.x
                + graphPos % MAX_GRAPHS_X * (clientArea.width / MAX_GRAPHS_X);
        int y = clientArea.y
                + graphPos / MAX_GRAPHS_X * (clientArea.height / MAX_GRAPHS_Y);

        int width = graphInfo.getXsize() * (clientArea.width / MAX_GRAPHS_X);
        int height = graphInfo.getYsize() * (clientArea.height / MAX_GRAPHS_Y);

        Rectangle graphArea = new Rectangle(x + graphBorderLeft,
                y + graphBorderTop, width - graphBorderLeft - graphBorderRight,
                height - graphBorderTop - graphBorderBottom);
        graphData.setGraphArea(graphArea);

        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.drawRectangle(graphArea);

        if (!graphData.isValid()) {
            TraceData traceData = graphData.getTraceData(0);
            String noDataLabel = getNoDataLabel(gc, graphData,
                    traceData.getLid(), traceData.getName());
            int tmpX = graphArea.x
                    + (graphArea.width - gc.textExtent(NO_DATA_AVAILABLE).x)
                            / 2;
            int tmpY = graphArea.y + graphArea.height / 2;
            gc.drawText(NO_DATA_AVAILABLE, tmpX, tmpY, true);
            gc.drawText(noDataLabel, graphArea.x - fontWidth,
                    graphArea.y - fontHeight * 2, true);
            return;
        }

        for (TraceData traceData : graphData.getTraces()) {
            if (traceData.getPe().equalsIgnoreCase(HydroConstants.PP)) {
                /*
                 * Displaying PP data already. No need to do PC to PP
                 * conversion.
                 */
                graphData.getGraphInfo().setDerivepp(DERIVE_PP.NO_PC_TO_PP);
            }
        }
        /* Check and Display scale to categories for current graph */
        boolean displayHgvars = displayFloodCat(graphData);

        /* ******************************************************** */
        /* If "Show PC as PP" selected then add derived PP trace to */
        /* current graph dynamically. */
        /* ******************************************************** */
        if (graphInfo.getDerivepp() != DERIVE_PP.NO_PC_TO_PP) {
            for (TraceData traceData : graphData.getTraces()) {
                if (traceData.isTraceOn()) {
                    if (HydroConstants.PC.equalsIgnoreCase(traceData.getPe())
                            && traceData.getTsData().size() > 1) {
                        boolean interpMode = false;
                        boolean distribMode = false;
                        if (graphInfo.getDerivepp()
                                .equals(DERIVE_PP.INTERPOLATE)) {
                            interpMode = true;
                            distribMode = false;
                        }
                        if (graphInfo.getDerivepp().equals(DERIVE_PP.ASSIGN)) {
                            interpMode = false;
                            distribMode = true;
                        }

                        TraceInfo pcAsppTraceInfo = new TraceInfo(
                                traceData.getTraceInfo());
                        TraceData pcAsppTraceData = new TraceData(
                                pcAsppTraceInfo);
                        TimeSeriesUtil.tsNormalize(traceData, pcAsppTraceData,
                                HydroConstants.INTERVAL_SECONDS, interpMode,
                                beginDate, endDate);
                        TimeSeriesUtil.tsAccumToInc3(pcAsppTraceData,
                                distribMode);
                        pcAsppTraceInfo.setName(traceData.getName());
                        pcAsppTraceInfo.setLid(traceData.getLid());
                        pcAsppTraceInfo.setPe(HydroConstants.PP);
                        pcAsppTraceInfo.setTs(traceData.getTs());
                        pcAsppTraceInfo.setExtremum(traceData.getExtremum());

                        pcAsppTraceData.setTraceOn(true);
                        pcAsppTraceData.setValid(true);

                        drawTracePcAsPp(gc, graphData, pcAsppTraceData);
                        break;
                    }
                }
            }
        }

        /* Start display all traces in a selected graph */

        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        String labelString;
        try {
            labelString = dataManager
                    .getShefPE(graphData.getTraceData(0).getPe());
        } catch (VizException e) {
            statusHandler.error("Unable to get units for pe="
                    + graphData.getTraceData(0).getPe(), e);
            labelString = "";
        }

        drawXaxis(gc, graphData);
        drawYaxis(gc, graphData, labelString);

        for (int n = 0; n < graphData.getNumTraces(); n++) {
            TraceData traceData = graphData.getTraceData(n);
            if (traceData.isTraceOn() && traceData.isValid()) {
                if (!groupData.getGroupInfo().isGroupSelected()) {
                    traceData.setTraceColor(HydroUtils.getColor(n));
                } else if (traceData.getNpts() == 0) {
                    traceData.setTraceColor(HydroUtils.getColor("white"));
                } else {
                    if (traceData.getColorName() != null
                            && !traceData.getColorName().isEmpty()) {
                        traceData.setTraceColor(
                                HydroUtils.getColor(traceData.getColorName()));
                    } else {
                        int color_index = pickMyColor(graphInfo, n);
                        traceData.setTraceColor(
                                HydroUtils.getColor(color_index));
                    }
                }

                if (traceData.getPe().equalsIgnoreCase(HydroConstants.PP)) {
                    drawTracePp(gc, graphData, traceData);
                } else {
                    drawTrace(gc, graphData, traceData);
                }
            }
        }

        /**********************************************************
         * Only show details of max/min for single trace in graph otherwise show
         * just Lid-PeDTsEP if more than one traces in selected graph.
         **********************************************************/

        if (graphData.getNumTraces() == 1) {
            displayTraceMinMax(gc, graphData);
        } else if (graphData.getNumTraces() > 1) {
            displayTraceNames(gc, graphData);
        }

        /* ************************ */
        /* Display Flood Categories */
        /* ************************ */

        if (displayHgvars) {
            displayFloodCatLines(gc, graphData);
        }

        /* Show active graph label */
        if (graphData == groupData.getActiveGraph()) {
            gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_GREEN));
            gc.drawText("A", graphArea.x + graphArea.width - fontWidth * 2,
                    graphArea.y + fontHeight, false);
        }
    }

    private void drawTracePcAsPp(GC gc, GraphData graphData,
            TraceData traceData) {
        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_YELLOW));

        Date xMin = graphData.getXmin();
        Date xMax = graphData.getXmax();

        double yMin = 0.0;
        double yMax = 0.0;
        List<TimeSeriesPoint> tsData = traceData.getTsData();
        for (TimeSeriesPoint tsPoint : tsData) {
            yMax = Math.max(tsPoint.getY(), yMax);
        }

        graphData.adjustPcYmax(yMin, yMax);
        yMin = graphData.getYmin2();
        yMax = graphData.getYmax2();

        Rectangle graphArea = graphData.getGraphArea();
        for (TimeSeriesPoint tsPoint : tsData) {
            if (tsPoint.getMode() == MODE.DELETE
                    || tsPoint.getMode() == MODE.SETMISSING) {
                continue;
            }

            Date xValue = tsPoint.getX();
            double yValue = tsPoint.getY();
            if (xValue.before(xMin) || xValue.after(xMax) || yValue < yMin
                    || yValue == HydroConstants.MISSING_VALUE) {
                continue;
            }

            int x2 = x2pixel(graphData, xValue);
            int x1 = x2pixel(graphData,
                    new Date(xValue.getTime() - TimeUtil.MILLIS_PER_HOUR));
            int barWidth = x2 - x1;

            int y = secondaryY2pixel(graphData, yValue);
            int barHeight = graphArea.y + graphArea.height - y;

            gc.fillRectangle(x2 - barWidth, y, barWidth, barHeight);
        }

        gc.setBackground(getBackground());

        drawPcAsPpYaxis(gc, graphData);
    }

    private void drawPcAsPpYaxis(GC gc, GraphData graphData) {
        double ymin = graphData.getYmin2();
        double ymax = graphData.getYmax2();
        double yinc = graphData.getDataInc2();

        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_YELLOW));
        gc.setLineWidth(1);

        Rectangle graphArea = graphData.getGraphArea();
        int x = graphArea.x + graphArea.width;
        gc.drawLine(x, graphArea.y, x, graphArea.y + graphArea.height);

        double ydiff = ymax - ymin;

        for (double data = ymin; data <= ymax; data += yinc) {
            String format;
            if (ydiff < 1.0) {
                format = "%4.2f";
            } else if (ydiff < 100.0) {
                format = "%4.1f";
            } else {
                format = "%.0f";
            }
            String buf = String.format(format, data);

            int y = secondaryY2pixel(graphData, data);

            gc.drawLine(x, y, x + fontWidth, y);

            gc.drawText(buf, x + fontWidth, y - fontHeight / 2, true);
        }
    }

    /**
     * Display trace data for precip data only
     *
     * @param gc
     * @param graphData
     * @param traceData
     */
    private void drawTracePp(GC gc, GraphData graphData, TraceData traceData) {
        Color color;
        if (traceData == groupData.getSelectedTrace()) {
            color = new Color(getDisplay(), HydroUtils.getColor(EDIT_COLOR));
        } else {
            color = new Color(getDisplay(), traceData.getTraceColor());
        }
        gc.setBackground(color);

        Date xMin = graphData.getXmin();
        Date xMax = graphData.getXmax();
        double yMin = graphData.getYmin();

        Rectangle graphArea = graphData.getGraphArea();
        List<TimeSeriesPoint> tsData = traceData.getTsData();
        for (TimeSeriesPoint tsPoint : tsData) {
            if (tsPoint.getMode() == MODE.DELETE
                    || tsPoint.getMode() == MODE.SETMISSING) {
                continue;
            }

            Date xValue = tsPoint.getX();
            double yValue = tsPoint.getY();
            if (xValue.before(xMin) || xValue.after(xMax) || yValue < yMin) {
                continue;
            }

            int x2 = x2pixel(graphData, xValue);
            int x1 = x2pixel(graphData,
                    new Date(xValue.getTime() - TimeUtil.MILLIS_PER_HOUR));
            int barWidth = x2 - x1;

            int y = y2pixel(graphData, yValue);
            int barHeight = graphArea.y + graphArea.height - y;

            gc.fillRectangle(x2 - barWidth, y, barWidth, barHeight);
        }

        gc.setBackground(getBackground());
        color.dispose();
    }

    /**
     * Display the flood category lines on the graph
     *
     * @param gc
     *            the graphics context
     * @param graphData
     *            The GraphData object
     */
    protected void displayFloodCatLines(GC gc, GraphData graphData) {
        Rectangle graphArea = graphData.getGraphArea();
        HGData hgData = graphData.getHgData();
        gc.setLineWidth(1);

        /* Action stage/flow */
        Color color = new Color(getDisplay(), HydroUtils.getColor("Yellow"));
        gc.setForeground(color);

        double value;
        if (graphData.getTraceData(0).getPe().toUpperCase().startsWith("H")) {
            value = hgData.getActionStage();
        } else {
            value = hgData.getActionFlow();
        }

        int y = y2pixel(graphData, value);

        if (value >= 0 && graphArea.contains(graphArea.x, y)) {
            gc.drawLine(graphArea.x, y, graphArea.x + graphArea.width, y);
        }
        color.dispose();

        /* Flood Stage/Discharge stage */
        color = new Color(getDisplay(), HydroUtils.getColor("Orange"));
        gc.setForeground(color);

        if (graphData.getTraceData(0).getPe().toUpperCase().startsWith("H")) {
            value = hgData.getFloodStage();
        } else {
            value = hgData.getFloodDischarge();
        }

        y = y2pixel(graphData, value);

        if (value >= 0 && graphArea.contains(graphArea.x, y)) {
            gc.drawLine(graphArea.x, y, graphArea.x + graphArea.width, y);
        }

        /* Minor stage */
        value = hgData.getMinor();

        y = y2pixel(graphData, value);

        if (value >= 0 && graphArea.contains(graphArea.x, y)) {
            gc.drawLine(graphArea.x, y, graphArea.x + graphArea.width, y);
        }
        color.dispose();

        /* Moderate stage/flow */
        color = new Color(getDisplay(), HydroUtils.getColor("Red"));
        gc.setForeground(color);

        value = hgData.getModerate();

        y = y2pixel(graphData, value);

        if (value >= 0 && graphArea.contains(graphArea.x, y)) {
            gc.drawLine(graphArea.x, y, graphArea.x + graphArea.width, y);
        }
        color.dispose();

        /* Major stage/flow */
        color = new Color(getDisplay(), HydroUtils.getColor("Magenta"));
        gc.setForeground(color);

        value = hgData.getMajor();

        y = y2pixel(graphData, value);

        if (value >= 0 && graphArea.contains(graphArea.x, y)) {
            gc.drawLine(graphArea.x, y, graphArea.x + graphArea.width, y);
        }
        color.dispose();
    }

    private void displayTraceNames(GC gc, GraphData graphData) {
        Rectangle graphArea = graphData.getGraphArea();
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();

        String siteLabel = null;
        try {
            String lid = graphData.getTraceData(0).getLid();
            String[] sa = dataManager.getStnRiverName(lid);
            if (sa != null && sa[0] != null && sa[1] != null) {
                if (sa[0].equalsIgnoreCase(HydroConstants.UNDEFINED)
                        && sa[1].equalsIgnoreCase(HydroConstants.UNDEFINED)) {
                    siteLabel = lid;
                } else if (!sa[0].equals(HydroConstants.UNDEFINED)
                        && !sa[1].equals(HydroConstants.UNDEFINED)) {
                    siteLabel = lid + " (" + sa[0] + " - " + sa[1] + ")";
                } else if (!sa[0].equals(HydroConstants.UNDEFINED)
                        && sa[1].equals(HydroConstants.UNDEFINED)) {
                    siteLabel = lid + " (" + sa[0] + ")";
                } else {
                    siteLabel = lid;
                }
            } else {
                siteLabel = lid;
            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving river names", e);
            siteLabel = "";
        }

        double floodStage = graphData.getHgData().getFloodStage();
        if (HydroConstants.MISSING_VALUE == floodStage) {
            siteLabel = String.format("%s fs=M", siteLabel);
        } else {
            siteLabel = String.format("%s fs=%.1f", siteLabel, floodStage);
        }

        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_CYAN));
        gc.drawText(siteLabel, graphArea.x - fontWidth,
                graphArea.y - fontHeight * 2, true);

        boolean showDerivedPP = false;
        int nameLength = gc.textExtent(siteLabel).x;
        int xpos = graphArea.x - fontWidth;
        int ypos = graphArea.y - fontHeight;
        for (TraceData traceData : graphData.getTraces()) {
            if (traceData.isTraceOn() && traceData.isValid()) {
                String buf = "";
                if (!HydroConstants.PP.equalsIgnoreCase(traceData.getPe())) {
                    buf = traceData.getPEDTSE();
                }

                if (HydroConstants.PC.equalsIgnoreCase(traceData.getPe())) {
                    showDerivedPP = graphData
                            .getDerivepp() != DERIVE_PP.NO_PC_TO_PP;
                }

                /* Display trace Name PEDTSEP */
                Color currentTraceColor = new Color(getDisplay(),
                        traceData.getTraceColor());
                gc.setForeground(currentTraceColor);
                gc.drawText(buf, xpos, ypos, true);
                currentTraceColor.dispose();

                xpos += gc.textExtent(buf).x + fontWidth;
                if (xpos > graphArea.width) {
                    xpos = graphArea.x + nameLength;
                    ypos -= fontHeight;
                }
            }
        }

        for (TraceData traceData : graphData.getTraces()) {
            if (!traceData.isValid()) {
                traceData.setTraceColor(HydroUtils.getColor(24));
                String buf = traceData.getPEDTSE() + "(NO DATA)";

                Color currentTraceColor = new Color(getDisplay(),
                        traceData.getTraceColor());
                gc.setForeground(currentTraceColor);
                gc.drawText(buf, xpos, ypos, true);
                currentTraceColor.dispose();

                xpos += gc.textExtent(buf).x + fontWidth;
                if (xpos > graphArea.width) {
                    xpos = graphArea.x + nameLength;
                    ypos -= fontHeight;
                }
            }
        }

        if (showDerivedPP) {
            gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_YELLOW));
            gc.drawText("DERIVED PP", xpos, ypos, true);
        }
    }

    private void displayTraceMinMax(GC gc, GraphData graphData) {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();

        Rectangle graphArea = graphData.getGraphArea();
        TraceData traceData = graphData.getTraceData(0);
        String siteLabel = null;
        try {
            String lid = traceData.getLid();
            String[] sa = dataManager.getStnRiverName(lid);
            if (sa != null && sa[0] != null && sa[1] != null) {
                if (sa[0].equalsIgnoreCase(HydroConstants.UNDEFINED)
                        && sa[1].equalsIgnoreCase(HydroConstants.UNDEFINED)) {
                    siteLabel = lid;
                } else if (!sa[0].equals(HydroConstants.UNDEFINED)
                        && !sa[1].equals(HydroConstants.UNDEFINED)) {
                    siteLabel = lid + " (" + sa[0] + " - " + sa[1] + ")";
                } else if (!sa[0].equals(HydroConstants.UNDEFINED)
                        && sa[1].equals(HydroConstants.UNDEFINED)) {
                    siteLabel = lid + " (" + sa[0] + ")";
                } else {
                    siteLabel = lid;
                }
            } else {
                siteLabel = lid;
            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving river names", e);
            siteLabel = "";
        }

        double floodStage = graphData.getHgData().getFloodStage();
        if (HydroConstants.MISSING_VALUE == floodStage) {
            siteLabel = String.format("%s fs=M", siteLabel);
        } else {
            siteLabel = String.format("%s fs=%.1f", siteLabel, floodStage);
        }

        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_CYAN));
        gc.drawText(siteLabel, graphArea.x - fontWidth,
                graphArea.y - fontHeight * 2, true);

        String name = traceData.getPEDTSE();
        String buf = name;

        boolean showDerivedPP = false;
        if (graphData.getDerivepp() != DERIVE_PP.NO_PC_TO_PP
                && HydroConstants.PC.equalsIgnoreCase(traceData.getPe())) {
            showDerivedPP = true;
        } else {
            SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yy HH");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

            double valueYmin = traceData.getYmin();
            double valueYmax = traceData.getYmax();

            if (traceData.getPe().toUpperCase().startsWith("Q")) {

                if (valueYmax >= 10_000) {
                    valueYmax /= 1000;
                    valueYmin /= 1000;
                }
                buf = String.format("%s min=%.2f %sz max=%.2f %sz", name,
                        valueYmin, sdf.format(traceData.getXmin()), valueYmax,
                        sdf.format(traceData.getXmax()));
            } else {
                if (groupData.getCurrentPage().getNumGraphs() >= 5) {
                    buf = String.format("%s min=%.2f max=%.2f", name, valueYmin,
                            valueYmax);
                } else {
                    buf = String.format("%s min=%.2f %sz max=%.2f %sz", name,
                            valueYmin, sdf.format(traceData.getXmin()),
                            valueYmax, sdf.format(traceData.getXmax()));
                }
            }
        }

        Color currentTraceColor = new Color(getDisplay(),
                traceData.getTraceColor());
        gc.setForeground(currentTraceColor);

        int xpos = graphArea.x - fontWidth;
        int ypos = graphArea.y - fontHeight;
        gc.drawText(buf, xpos, ypos, true);
        currentTraceColor.dispose();

        if (showDerivedPP) {
            xpos += gc.textExtent(buf).x + fontWidth;
            gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_YELLOW));
            gc.drawText("DERIVED PP", xpos, ypos, true);
        }

    }

    private int pickMyColor(GraphInfo graphInfo, int ntrace) {
        int n;
        for (n = ntrace; n < HydroUtils.NUM_COLORS; n++) {
            boolean duplicate = false;
            for (TraceInfo traceInfo : graphInfo.getTraceInfoList()) {
                RGB color = HydroUtils.getColor(traceInfo.getColorName());
                if (color.equals(HydroUtils.getColor(n))) {
                    duplicate = true;
                    break;
                }
            }
            if (!duplicate) {
                break;
            }
        }

        return n;
    }

    private void drawYaxis(GC gc, GraphData graphData, String label) {
        boolean ratingCurveExists = false;

        /* Maximum discharge value */
        double maxDischarge = -999;

        graphData.setDisplayFlowUnit(false);
        String pe = graphData.getTraceData(0).getPe().toUpperCase();

        /* Does a rating table exist for this site? */
        String lid = graphData.getTraces().get(0).getLid();
        if (!ratingCurveExists && (pe.startsWith("H") || pe.startsWith("Q"))) {
            ratingCurveExists = StageDischargeUtils.checkRatingTable(lid);
        }

        if (pe.startsWith("Q")) {
            maxDischarge = graphData.getYmax();
        } else {
            if (ratingCurveExists) {
                maxDischarge = StageDischargeUtils.stage2discharge(lid,
                        graphData.getYmax());
            }
        }

        double data = graphData.getYmin();

        double yDiff = graphData.getYmax() - graphData.getYmin();
        double inc = graphData.getDataInc();
        double tmp = yDiff / inc;
        int numberTicks = (int) tmp + 1;

        NumberFormat dischargeFormat = new DecimalFormat("0.0");
        int dx = 5;
        int y = 0;

        String fmt = "0";
        if (yDiff < 1.0) {
            fmt = "0.00";
        } else if (yDiff < 10.0) {
            fmt = "0.0";
        }

        NumberFormat formatter = new DecimalFormat(fmt);

        gc.setLineWidth(1);
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        dx = fontWidth;
        Rectangle graphArea = graphData.getGraphArea();
        for (int i = 0; i < numberTicks; i++) {
            y = y2pixel(graphData, data);
            if (groupData.getGroupInfo().isGridLines()) {
                gc.setLineStyle(SWT.LINE_DOT);
                gc.drawLine(graphArea.x, y, graphArea.x + graphArea.width, y);
                gc.setLineStyle(SWT.LINE_SOLID);
            }

            /* Draw the tick marks and values on left axis */
            gc.drawLine(graphArea.x, y, graphArea.x - dx, y);

            String dataStr = formatter.format(data);
            Point dataExtent = gc.textExtent(dataStr);
            gc.drawText(dataStr, graphArea.x - dx - dataExtent.x - fontWidth,
                    y - fontHeight / 2, true);

            /*
             * Draw the tick marks and values on right axis for ratingCurveExist
             */
            if (ratingCurveExists) {
                double value = 0;
                if (pe.toUpperCase().startsWith("H")) {
                    value = StageDischargeUtils.stage2discharge(lid, data);
                    if (value < 0) {
                        value = 0.0;
                    } else if (maxDischarge >= 10_000) {
                        value /= 1000;
                    }
                } else if (pe.toUpperCase().startsWith("Q")) {
                    double stageValue = StageDischargeUtils
                            .getStageFromDischarge(graphData, data);
                    if (stageValue != HydroConstants.MISSING_VALUE) {
                        value = stageValue;
                    } else {
                        break;
                    }
                }

                gc.drawLine(graphArea.x + graphArea.width, y,
                        graphArea.x + graphArea.width + dx, y);

                gc.drawText(dischargeFormat.format(value),
                        graphArea.x + graphArea.width + dx + fontWidth,
                        y - fontHeight / 2, true);
            }

            data += inc;
        }
        if (ratingCurveExists) {
            labelRightAxis(gc, pe, maxDischarge, ratingCurveExists, graphArea);
        }

        labelLeftAxis(gc, pe, maxDischarge, label, graphArea);
    }

    /**
     * Labels the left axis.
     *
     * @param gc
     *            the graphics context
     * @param pe
     *            Physical Element
     * @param maxDischarge
     *            Maximum Discharge
     * @param label
     *            Label to display
     * @param graphArea
     *            rectangle defining graph area
     */
    private void labelLeftAxis(GC gc, String pe, double maxDischarge,
            String label, Rectangle graphArea) {
        if (pe.toUpperCase().startsWith("Q")) {
            if (maxDischarge >= 10000.0) {
                label = "Total Discharge in KCFS";
            } else {
                label = "Total Discharge in CFS";
            }
        } else if (pe.toUpperCase().startsWith("H")) {
            label = "River Stage in Feet";
        }
        // else just use the label passed in

        int yoffset = (graphArea.height - fontHeight * label.length()) / 2;

        for (int i = 0; i < label.length(); i++) {
            gc.drawText(Character.toString(label.charAt(i)),
                    graphArea.x - graphBorderLeft + fontWidth / 2,
                    graphArea.y + yoffset + fontHeight * i, true);
        }
    }

    /**
     * Label the right axis.
     *
     * @param gc
     *            the graphics context
     * @param pe
     *            Physical Element
     * @param maxDischarge
     *            Maximum Discharge
     * @param ratingCurveExists
     *            true if rating curve exists
     * @param graphArea
     *            rectangle defining graph area
     */
    private void labelRightAxis(GC gc, String pe, double maxDischarge,
            boolean ratingCurveExists, Rectangle graphArea) {
        String label = "";

        if (ratingCurveExists) {
            label = "River Stage in Feet";
            if (pe.toUpperCase().startsWith("H")) {
                if (maxDischarge >= 10000.0) {
                    label = "Total Discharge in KCFS";
                } else {
                    label = "Total Discharge in CFS";
                }
            }
        }

        int yoffset = (graphArea.height - fontHeight * label.length()) / 2;

        for (int i = 0; i < label.length(); i++) {
            gc.drawText(Character.toString(label.charAt(i)),
                    graphArea.x + graphArea.width + graphBorderRight
                            - fontWidth * 3 / 2,
                    graphArea.y + yoffset + fontHeight * i, true);
        }
    }

    private void drawXaxis(GC gc, GraphData graphData) {
        /* Minor ticks set 1 hour default */
        int minorTicks = 1;

        /* Major ticks set 6 hour default */
        int majorTicks = 6;

        long ndays = (graphData.getXmax().getTime()
                - graphData.getXmin().getTime()) / TimeUtil.MILLIS_PER_DAY;

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

        // Check graph area width. if small then need to skip extra days
        Rectangle graphArea = graphData.getGraphArea();
        if (graphArea.width < 500) {
            daysSkip++;
        }

        TimeZone gmt = TimeZone.getTimeZone("GMT");
        SimpleDateFormat hourFormat = new SimpleDateFormat("HH");
        hourFormat.setTimeZone(gmt);

        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd");
        dateFormat.setTimeZone(gmt);

        gc.setLineWidth(1);
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        int x;
        int dy;
        int dx;
        long startMillis = (long) Math.ceil(
                graphData.getXmin().getTime() / HydroConstants.MILLIS_PER_HOUR)
                * HydroConstants.MILLIS_PER_HOUR;
        Calendar c = TimeUtil.newGmtCalendar(startMillis);
        while (!c.getTime().after(graphData.getXmax())) {
            x = x2pixel(graphData, c.getTime());
            dy = fontHeight / 2;
            String hourStr = hourFormat.format(c.getTime());
            Point hourExtent = gc.textExtent(hourStr);
            dx = hourExtent.x / 2;
            int hour = c.get(Calendar.HOUR_OF_DAY);
            if (hour == 0) {
                dy = fontHeight;
                if (daysCount % daysSkip == 0) {
                    gc.drawText(hourStr, x - dx,
                            graphArea.y + graphArea.height + fontHeight, true);

                    String dateStr = dateFormat.format(c.getTime());
                    Point dateExtent = gc.textExtent(dateStr);
                    gc.drawText(dateStr, x - dateExtent.x / 2,
                            graphArea.y + graphArea.height + fontHeight * 2,
                            true);

                    if (groupData.getGroupInfo().isGridLines()) {
                        gc.setLineStyle(SWT.LINE_DOT);
                        gc.drawLine(x, graphArea.y, x,
                                graphArea.y + graphArea.height);
                        gc.setLineStyle(SWT.LINE_SOLID);
                    }
                } else if (ndays < 8) {
                    gc.drawText(hourStr, x - dx,
                            graphArea.y + graphArea.height + fontHeight, true);

                }
                daysCount++;
            } else if (hour % majorTicks == 0) {
                /*
                 * Hour annotation
                 */
                dy = fontHeight * 3 / 4;
                if (ndays < 8 && graphArea.width > 450) {
                    gc.drawText(hourStr, x - dx,
                            graphArea.y + graphArea.height + fontHeight, true);
                } else if (hour == 12) {
                    gc.drawText(hourStr, x - dx,
                            graphArea.y + graphArea.height + fontHeight, true);

                }
            }

            /* ******************************** */
            /* major and minor ticks annotation */
            /* ******************************** */
            if (c.get(Calendar.HOUR_OF_DAY) % minorTicks == 0) {
                gc.drawLine(x, graphArea.y + graphArea.height, x,
                        graphArea.y + graphArea.height + dy);
            }

            c.add(Calendar.HOUR, 1);
        }

        if (zHrDisplay) {
            gc.drawText("(Z)", graphBorderRight + 10,
                    graphArea.y + graphArea.height + fontHeight, true);
        }

        /* ********************************************* */
        /* Draw reference vertical line at present time */
        /* ********************************************* */
        Date d = SimulatedTime.getSystemTime().getTime();
        if (d.after(graphData.getXmin()) && d.before(graphData.getXmax())) {
            int curTimeLoc = x2pixel(graphData, d);
            gc.setLineStyle(SWT.LINE_DASH);
            gc.setLineWidth(2);
            gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_CYAN));
            gc.drawLine(curTimeLoc, graphArea.y, curTimeLoc,
                    graphArea.y + graphArea.height);
            gc.setLineStyle(SWT.LINE_SOLID);
            gc.setLineWidth(1);
        }
    }

    /**
     * Draw the trace on the canvas
     *
     * @param gc
     *            The graphics context on which to draw the trace
     * @param graphData
     * @param traceData
     */
    protected void drawTrace(GC gc, GraphData graphData, TraceData traceData) {
        if (traceData.getNpts() == 0) {
            return;
        }

        // draw points and lines by default
        boolean drawLines = true;
        boolean drawPoints = true;

        if (groupData.getGroupInfo().getTraceMode().equals(TraceMode.POINTS)) {
            // Draw points but not lines
            drawLines = false;
        } else if (groupData.getGroupInfo().getTraceMode()
                .equals(TraceMode.LINES)) {
            // Draw lines but not points
            drawPoints = false;
        }

        int lineWidth = AppsDefaults.getInstance()
                .getInt(HydroConstants.TS_LINEWIDTH, 1);
        int diameter = lineWidth + 5;
        int offset = diameter / 2;

        Color currentTraceColor;
        if (traceData == groupData.getSelectedTrace()) {
            currentTraceColor = new Color(getDisplay(),
                    HydroUtils.getColor(EDIT_COLOR));
        } else {
            currentTraceColor = new Color(getDisplay(),
                    traceData.getTraceColor());
        }
        gc.setForeground(currentTraceColor);

        /* convert data points to pixels */
        List<TimeSeriesPoint> tsData = traceData.getTsData();
        Date xMin = graphData.getXmin();
        Date xMax = graphData.getXmax();

        int[] dataPts = new int[tsData.size() * 2];
        int dataIndex = 0;
        for (TimeSeriesPoint tsPoint : tsData) {
            if (tsPoint.getMode() == MODE.DELETE
                    || tsPoint.getMode() == MODE.SETMISSING) {
                continue;
            }

            Date x = tsPoint.getX();
            if (x.before(xMin) || x.after(xMax)) {
                continue;
            }

            dataPts[dataIndex] = x2pixel(graphData, x);
            dataIndex++;

            double y = tsPoint.getY();
            dataPts[dataIndex] = y2pixel(graphData, y);
            dataIndex++;
        }

        if (dataIndex < dataPts.length) {
            int[] temp = dataPts;
            dataPts = new int[dataIndex];
            System.arraycopy(temp, 0, dataPts, 0, dataIndex);
        }

        gc.setClipping(graphData.getGraphArea());
        if (drawLines) {
            gc.setLineWidth(lineWidth);
            gc.drawPolyline(dataPts);
            gc.setLineWidth(1);
        }

        if (drawPoints) {
            gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_RED));
            for (int i = 0; i < dataPts.length; i += 2) {
                gc.fillOval(dataPts[i] - offset, dataPts[i + 1] - offset,
                        diameter, diameter);
            }
            gc.setLineWidth(lineWidth);

            /* Draw circles at each data point */
            for (int i = 0; i < dataPts.length; i += 2) {
                gc.drawOval(dataPts[i] - offset, dataPts[i + 1] - offset,
                        diameter, diameter);
            }

            gc.setBackground(getBackground());
        }

        gc.setClipping((Rectangle) null);
        currentTraceColor.dispose();
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
    protected int x2pixel(GraphData gd, Date x) {
        Rectangle graphArea = gd.getGraphArea();

        long xMin = gd.getXmin().getTime();
        long xMax = gd.getXmax().getTime();

        double xDiff = xMax - xMin;
        double millisPerPixel = xDiff / graphArea.width;
        double xValue = (x.getTime() - xMin) / millisPerPixel;

        return graphArea.x + (int) Math.round(xValue);
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
        Rectangle graphArea = gd.getGraphArea();

        long xMin = gd.getXmin().getTime();
        long xMax = gd.getXmax().getTime();
        double xDiff = xMax - xMin;
        double millisPerPixel = xDiff / graphArea.width;
        long millisTime = (long) ((xpix - graphArea.x) * millisPerPixel) + xMin;

        return new Date(millisTime);
    }

    /**
     * convert real Y value to pixel value.
     *
     * @param gd
     *            The Graph Data
     * @param yValue
     *            The y data value to convert
     * @return The y pixel value
     */
    protected int y2pixel(GraphData gd, double yValue) {
        Rectangle graphArea = gd.getGraphArea();

        double y = 0;
        if (yValue != HydroConstants.MISSING_VALUE) {
            double yDiff = gd.getYmax() - gd.getYmin();
            y = graphArea.height / yDiff * (yValue - gd.getYmin());
            if (y < 0) {
                y = 0;
            }
        }
        return graphArea.y + graphArea.height - (int) Math.round(y);
    }

    /**
     * convert real Y value to pixel value for right axis.
     *
     * @param gd
     *            The Graph Data
     * @param yValue
     *            The y data value to convert
     * @return The y pixel value
     */
    protected int secondaryY2pixel(GraphData gd, double yValue) {

        Rectangle graphArea = gd.getGraphArea();

        double y = 0;
        if (yValue != HydroConstants.MISSING_VALUE) {
            double yDiff = gd.getYmax2() - gd.getYmin2();
            y = graphArea.height / yDiff * (yValue - gd.getYmin2());
            if (y < 0) {
                y = 0;
            }
        }
        return graphArea.y + graphArea.height - (int) Math.round(y);
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
        Rectangle graphArea = gd.getGraphArea();

        double yMin = gd.getYmin();
        double yMax = gd.getYmax();
        double ydiff = yMax - yMin;
        double pixPerUnit = graphArea.height / ydiff;

        return yMax - (ypix - graphArea.y) / pixPerUnit;
    }

    private boolean displayFloodCat(GraphData graphData) {
        boolean validGraph = false;
        for (TraceData traceData : graphData.getTraces()) {
            if (traceData.isTraceOn()) {
                validGraph = traceData.getPe().toUpperCase().startsWith("H")
                        || traceData.getPe().toUpperCase().startsWith("Q");
                if (validGraph) {
                    break;
                }
            }
        }

        if (!validGraph || !graphData.getHgData().isValid()) {
            return false;
        }

        /* Adjust Y axis to new max min when scale to categories selected */
        double ymin = graphData.getHgData().getYmin();
        double ymax = graphData.getHgData().getYmax();

        if (graphData.isZoomed() && graphData.getHgData().isValid()) {
            return true;
        }

        if (graphData.getHgData().isValid()) {

            if (graphData.isYscaleToData() && !graphData.isShowcat()) {
                graphData.setYmin(graphData.getOldYmin());
                graphData.setYmax(graphData.getOldYmax());
                graphData.setDataInc(graphData.getOldDataInc());
                return false;

            } else if (graphData.isYscaleToData() && graphData.isShowcat()) {
                graphData.setYmin(graphData.getOldYmin());
                graphData.setYmax(graphData.getOldYmax());
                graphData.setDataInc(graphData.getOldDataInc());
                return true;

            } else if (!graphData.isYscaleToData() && graphData.isShowcat()) {
                if (ymin >= graphData.getYmin()
                        && ymax <= graphData.getYmax()) {
                    /*
                     * may not work in one case ??? graphData.ymin =
                     * graphData.old_ymin; graphData.ymax = graphData.old_ymax;
                     * graphData.data_inc = graphData.old_data_inc;
                     */
                    return true;
                } else {

                    if (ymin > graphData.getYmin()) {
                        ymin = graphData.getYmin();
                    }

                    if (ymax < graphData.getYmax()) {
                        ymax = graphData.getYmax();
                    }

                    if (ymax == 0.0) {
                        graphData.setYmin(0.0);
                    }

                    if (ymax < graphData.getOldYmax()) {
                        ymax = graphData.getOldYmax();
                    }
                    graphData.adjustYmaxYmin(ymin, ymax);
                    return true;
                }
            }

        }

        return false;
    }

    private String getNoDataLabel(GC gc, GraphData graphData, String trace_lid,
            String trace_name) {
        String buf = trace_lid;

        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        try {
            String[] s = dataManager.getStnRiverName(trace_lid);
            String stn_name = s[0];
            String river_name = s[1];

            String tmpfs;
            if (graphData.getHgData()
                    .getFloodStage() == HydroConstants.MISSING_VALUE) {
                tmpfs = "M";
            } else {
                tmpfs = String.format("%.1f",
                        graphData.getHgData().getFloodStage());
            }

            if (HydroConstants.UNDEFINED.equalsIgnoreCase(river_name)) {
                buf = String.format("%s (%s)", trace_lid, stn_name);
            } else {
                buf = String.format("%s (%s - %s) fs=%s", trace_lid, stn_name,
                        river_name, tmpfs);
            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving river names", e);
        }

        return buf;
    }
}
