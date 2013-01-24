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
package com.raytheon.viz.hydro.timeseries;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Region;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.shef.tables.Fcstheight;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.timeseries.graph.TimeSeriesGraphCanvas;
import com.raytheon.viz.hydro.timeseries.util.GraphData;
import com.raytheon.viz.hydro.timeseries.util.GroupInfo;
import com.raytheon.viz.hydro.timeseries.util.PageInfo;
import com.raytheon.viz.hydro.timeseries.util.ScaleManager;
import com.raytheon.viz.hydro.timeseries.util.StageDischargeUtils;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesPoint;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesUtil;
import com.raytheon.viz.hydro.timeseries.util.TraceData;
import com.raytheon.viz.hydro.util.HydroUtils;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.ForecastData;
import com.raytheon.viz.hydrocommon.util.DbUtils;

/**
 * This class is the canvas where the Time Series Display data will be drawn.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation.
 * 01 Jul 2008  1194       mpduff      Implement graph.
 * 29 Apr 2010  5016/8     mpduff      Changed so the lines and 
 *      traces stay within the bounds of the graph.
 * 02 Jul 2010  5280       lbousaid    added river name to the graph display
 *      also, the flood stage value to the graph when no data available
 * 22 Jul 2010  5967       mpduff      Implemented toggling on/off of traces
 *      via right click pop-up menu. 
 *      
 * 19 Jan 2011  5281      lbousaidi   fixed max,min trace values and added 
 * 		related min and max dates to graph display
 * 24 Jan 2011  7799      bkowal      ensured that the y-axis maximum value would
 *                                    be updated if the flood category y-axis max
 *                                    was greater than the current y-axis max
 * 26 Jan 2011  5557      bkowal      created a new function that would draw the
 *                                    time series canvas within specified boundaries.
 * 31 Jan 2011  5274      bkowal      long-running queries are now done
 *                                    asynchronously so that a status indicator
 *                                    can be displayed and so that the
 *                                    interface is not locked up. Extracted
 *                                    the data retrieval logic from the function
 *                                    that updates the interface with the
 *                                    data retrieved.
 * 07 Feb 2011 6288      lbousaid     Added validGraph flag and findGraph arrayList 
 * 									  to list if the graph has trace or no
 * 04 Feb 2011 7758/7759              Added checkbox to pop up menu and added check box
 * 									  selection event to the handler
 * 13 Apr 2011 9006      jpiatt       Corrected getFcstData select statement to have 
 *                                    uppercase parameters.
 * 18 Apr 2011 8963      jpiatt       Adjusted call to scaling manager for y-axis.
 * 25 Apr 2011 7759      djingtao     Modify getDataForGraph(), getFcstData() and misc to allow correct
 *                                    graph display when "show latest Forecast only" is on or off
 * 01 June 2011 9499     djingtao     update getFcstData()        
 * 29 June 2011 9669 	 lbousaidi    removed page flickering during the graph display  
 * 01 July 2011 9700     djingtao     show latest forecast in group mode      
 * 12 July 2011 9709     djingtao     add draw "DERIVED PP"         
 * 25 July 2011 10082    djingtao     modify makeRegions()
 * 10 August 2011 10457  djingtao     allow red rubberband box to be draw for setMissing in Edit
 * 27 March 2012 14527   wkwock       Fix incomplete time series selection issue
 * 24 April 2012 14669   wkwock       Handle invalid color name
 * 08 May   2012 14958   wkwock       Fix overcrowded TS list
 * 30 May   2012 14967   wkwock       Fix incorrect product time
 * 06 Nov   2012 15399   wkwock       Fix refine the plot algorithm and sampling algorithm
 * 06 Nov   2012 15459   lbousaidi    update data when page/up or page/down is pressed without having
 * 									  to click in graph button again. 
 * 06 Nov   2012 15400   lbousaidi    Changed logic in buildPointString routine, added discharge2stage
 *                                    to display stage value, also added checking for rating curve for both 
 *                                    stage and discharge. 
 * 13 Nov   2012 15416   lbousaidi    added a check when the colorname is null and a call to 
 *                                    getGroupModeColor   
 * 09 Jan   2012 15493   lbousaidi    added code to delete data while zooming when you draw a box                         
 * @author lvenable
 * @version 1.0
 * 
 */
public class TimeSeriesDisplayCanvas extends TimeSeriesGraphCanvas implements
        IJobChangeListener {
    /** The maximum number of forecast traces. */
    private static final int MAX_FCST_TRACES = 30;

    private final String INCH = "in";

    private final String FEET = "ft";

    private final String CFS = "cfs";

    private final String KCFS = "kcfs";

    /**
     * Parent composite.
     */
    private final Composite parentComp;

    /**
     * Flag indicating that obs data are not available.
     */
    private boolean noDataAvailable = true;

    /**
     * Flag indicating that forecast data are not available.
     */
    private boolean noFcstDataAvailable = true;

    /**
     * No Data Available string.
     */
    private static final String NO_DATA_AVAILABLE = "NO DATA AVAILABLE";

    /** Location ID */
    private String lid = null;

    /** Physical Element */
    private String pe = null;

    /** Type Source */
    private String ts = null;

    /** duration */
    private String dur = "0";

    private String siteLabel = null;

    /** Starting Date of date */
    private Date beginDate = null;

    /** Ending date of date */
    private Date endDate = null;

    /** Date for min value */
    private Date minDate = null;

    /** Date for max value */
    private Date maxDate = null;

    /** List of visible traces */
    private ArrayList<TraceData> traceArray = null;

    /** Graph data object */
    private GraphData graphData = null;

    /** Flood Stage String */
    private String floodStage = null;

    /** If the request is new (from the TS Control dialog) query the db */
    private boolean newRequest = true;

    /** If getAgain, then query the DB */
    private boolean getAgain = false;

    /** Group Mode flag */
    private boolean groupMode = false;

    /** Reference to the dialog containing this class */
    private TimeSeriesDisplayDlg dialog;

    /** Mouse button pressed flag */
    private boolean mouseDown = false;

    /** String holding the value at the mouse point */
    private String pointString;

    /** 2 decimal place number format */
    private NumberFormat twoDecimalFormat = new DecimalFormat("0.00");

    /** A reference to this class */
    private Canvas tsCanvas;

    /**
     * Flag to determine if Flood Category data are loaded
     */
    private boolean floodCatDataLoaded = false;

    /**
     * List of regions for a single trace
     */
    private ArrayList<Region> regionList = new ArrayList<Region>();

    /**
     * List of Region Lists
     */
    private ArrayList<ArrayList<Region>> listRegionList = new ArrayList<ArrayList<Region>>();

    /**
     * List of regions for points for each trace
     */
    private ArrayList<ArrayList<Region>> pointList = new ArrayList<ArrayList<Region>>();

    /**
     * Is a point selected?
     */
    private boolean pointSelected = false;

    /**
     * Is the trace a selectable trace?
     */
    private boolean selectableTrace = false;

    /**
     * Is a trace selected?
     */
    private boolean traceSelected = false;

    /**
     * Index of the selection
     */
    private int selectionIndex = 0;

    /**
     * The delete index
     */
    private int deleteIndex = HydroConstants.MISSING_VALUE;

    /**
     * List of deleted indexes
     */
    private List<Integer> deleteList = new ArrayList<Integer>();

    /**
     * The index of the point set to missing
     */
    private int setMissingIndex = HydroConstants.MISSING_VALUE;

    /**
     * List of setMissing indexes
     */
    private List<Integer> setMissingList = new ArrayList<Integer>();

    /**
     * The selected X value
     */
    private int selectedX = 0;

    /**
     * The selected Y value
     */
    private int selectedY = 0;

    /**
     * The edited point values and the surrounding point values
     */
    private int[] editPts = new int[6];

    /** Left axis label string */
    private String labelString = "";

    /** The selected trace index */
    private int selectedTraceId = 0;

    /**
     * Is the PE selected a precip PE
     */
    private boolean precipPE = false;

    /**
     * The inserted point
     */
    private TimeSeriesPoint insertedPoint = null;

    /**
     * Flag to create new regions for the traces
     */
    private boolean createRegions = true;

    /** IHFS Date Format yyyy-MM-dd HH:mm:ss */
    public SimpleDateFormat dateFormat;

    /** Graph display Date Format MM/dd/yy HH 'z' */
    public SimpleDateFormat graphFormat;

    /**
     * List of graph traces that are available.
     */
    private ArrayList<Boolean> validGraph;

    /**
     * Is the graph valid
     */
    private boolean findGraph = false;

    /**
     * Show Latest Forecast flag.
     */
    private boolean latestFcstFlag = true;

    /**
     * record of the number of forecast traces and type source
     */

    private int num_of_fcstTraces = 0;

    private int num_of_fcstTs = 0;

    private Boolean inDataRetreival = Boolean.FALSE;

    private TimeSeriesDataJobManager tsDataJobManager = null;
    
    private boolean zoomed = false;

	/**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param lid
     *            Location Id
     * @param pe
     *            Physical Element
     * @param ts
     *            Type Source
     */
    public TimeSeriesDisplayCanvas(final TimeSeriesDisplayDlg dialog,
            Composite parent, GraphData graphData, Date beginDate,
            Date endDate, boolean groupMode) {
        super(parent, SWT.DOUBLE_BUFFERED);

        this.beginDate = beginDate;
        this.endDate = endDate;
        this.graphData = graphData;
        this.groupMode = groupMode;
        this.dialog = dialog;
        this.tsDataJobManager = new TimeSeriesDataJobManager();
        setDialog(dialog);

        lineWidth = AppsDefaults.getInstance().getInt(
                HydroConstants.TS_LINEWIDTH, 1);
        crossHairCursor = new Cursor(parent.getDisplay(), SWT.CURSOR_CROSS);
        arrowCursor = new Cursor(parent.getDisplay(), SWT.CURSOR_ARROW);
        northSouthCursor = new Cursor(parent.getDisplay(), SWT.CURSOR_SIZENS);
        handCursor = new Cursor(parent.getDisplay(), SWT.CURSOR_HAND);

        newRequest = true;
        parentComp = parent;
        tsCanvas = this;

        white = parentComp.getDisplay().getSystemColor(SWT.COLOR_WHITE);
        black = parentComp.getDisplay().getSystemColor(SWT.COLOR_BLACK);

        dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        setRatingCurveExist(false);

        /* Handle window resizing */
        final Shell parentShell = parentComp.getShell();
        parentShell.addListener(SWT.Resize, new Listener() {
            public void handleEvent(Event e) {
                // Set true so new regions will be created as graph is resized

                createRegions = true;
                resizeGraph(parentShell.getClientArea());
                redraw();
            }
        });
        setupCanvas();
        final TimeSeriesDisplayCanvas c = this;
        getDisplay().asyncExec(new Runnable() {
            public void run() {
                c.redraw();
            }
        });
        
        /* 
         * Add a key listener for up and down arrows
         * to move up and down through the pages of 
         * the graph
         */
        this.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_UP) {
                    newRequest=true;
                    dialog.pageUpAction();
                } else if (e.keyCode == SWT.ARROW_DOWN) {
                    newRequest=true;
                    dialog.pageDownAction();
                }
            }
        });
    }

    private void scheduleDataRetrieval() {
        inDataRetreival = Boolean.TRUE;

        tsDataJobManager.scheduleGetGraphData(this, this);
    }

    /**
     * Setup the canvas for displaying information.
     */
    private void setupCanvas() {
        canvasFont = new Font(parentComp.getDisplay(), "Monospace", 9,
                SWT.NORMAL);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = canvasHeight;
        gd.widthHint = canvasWidth;

        setLayoutData(gd);
        addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                try {
                    if (newRequest || getAgain) {
                        /* Schedule A Data Retrieval */
                        latestFcstFlag = dialog.showLatestFcst();
                        scheduleDataRetrieval();
                        if (newRequest) {
                            newRequest = false;
                        }
                        if (getAgain) {
                            getAgain = false;
                        }
                    } else {
                        /* Use The Data That We Already Have */
                        drawCanvas(e.gc);
                    }
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        });

        addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                if ((canvasFont != null) && (canvasFont.isDisposed() == false)) {
                    canvasFont.dispose();
                }
            }
        });

        addMouseListener(new MouseListener() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                // Intentionally blank
            }

            @Override
            public void mouseDown(MouseEvent e) {
                handleMouseDownEvent(e);
            }

            @Override
            public void mouseUp(MouseEvent e) {
                handleMouseUpEvent(e);
            }
        });

        addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {
                handleMouseMoveEvent(e);
            }
        });
    }

    public void updateGraphBounds(Rectangle bounds) {
        this.resizeGraph(bounds);
    }

    public void getDataForGraph() {
        try {
            /*
             * Get the data for the lid and pe values passed in. If the
             * graphData object is null, get data from IHFS else reuse the data
             */
            int start_num = 0;
            int end_num = 0;

            validGraph = new ArrayList<Boolean>();
            for (int d = 0; d < 100; d++)
                validGraph.add(false);

            ArrayList<TraceData> traceList = graphData.getOriginalTraces();
            if (traceList.isEmpty()) {
                if (groupMode && latestFcstFlag && (num_of_fcstTraces != 0)) {
                    for (int g = 0; g < graphData.getNumTraces()
                            - (num_of_fcstTraces - num_of_fcstTs); g++) {
                        traceList.add(graphData.getTraceData(g));
                    }
                } else
                    traceList = graphData.getTraces();
            }
            graphData.setTraces(traceList);

            // Clone the list here because we are adding to it in the
            // getFcstData
            @SuppressWarnings("unchecked")
            ArrayList<TraceData> clonedList = (ArrayList<TraceData>) traceList
                    .clone();

            start_num = clonedList.size();
            num_of_fcstTraces = 0;
            num_of_fcstTs = 0;

            HashSet <String> uniqueList = new HashSet <String> ();
            graphData.setTraces(new ArrayList<TraceData>());
            for (int i = 0; i < clonedList.size(); i++) {
                if (clonedList.get(i).isForecast()) {
                	TraceData td=clonedList.get(i);
                	String traceKey = td.getLid()+td.getPe()+td.getTs()+td.getDur()+td.getExtremum();
                	if (uniqueList.contains(traceKey))
                		continue;
                	else {
                		uniqueList.add(traceKey);
                	}

                    int return_fcst_num = getFcstData(clonedList.get(i));

                    if (groupMode && !latestFcstFlag && (return_fcst_num > 1)) {
                        num_of_fcstTraces = num_of_fcstTraces + return_fcst_num;
                        num_of_fcstTs++;
                    }

                    validGraph.set(i, noFcstDataAvailable);

                    if (return_fcst_num > 1) {
                        end_num = start_num + (return_fcst_num - 1);
                        for (int k = start_num; k < end_num; k++) {
                            validGraph.set(k, noFcstDataAvailable);
                        }
                        start_num = end_num;
                    }

                } else {
                	graphData.addTrace(clonedList.get(i));
                    getData(clonedList.get(i));
                    validGraph.set(i, noDataAvailable);
                }
            }

            graphData.setBeginDate(beginDate);
            graphData.setEndDate(endDate);
        } finally {
            inDataRetreival = Boolean.FALSE;
        }
        // newRequest = false;
        // getAgain = false;
    }

    /**
     * Draw the canvas
     * 
     * @param gc
     *            Graphics Context.
     */
    protected void drawCanvas(GC gc) {
        gc.setFont(canvasFont);

        fontHeight = (gc.getFontMetrics().getHeight());
        int fontAveWidth = gc.getFontMetrics().getAverageCharWidth();
        int swtColor = SWT.COLOR_BLACK;
        if (this.dialog.isInverseVideo()) {
            swtColor = SWT.COLOR_WHITE;
        }

        setBackgroundColor(gc, swtColor);
        gc.fillRectangle(0, 0, canvasWidth, canvasHeight + 2);

        if (mouseDown && pointSelected) {
            setCursor(northSouthCursor);
        } else if (mouseDown && !pointSelected) {
            setCursor(crossHairCursor);

            if (!dialog.isZoomAction()) {
                /* Draw the full graph cross hairs */
                drawCrossHairs(gc);
            }
        }

        if (dialog.isCancel()) {
            traceSelected = false;
            dialog.setInsert(false);
            insertedPoint = null;
        }

        // ---------------------------------------------
        // Draw page numbers
        // ---------------------------------------------
        setForegroundColor(gc, SWT.COLOR_CYAN);

        if (graphData.getGraph_pos() == 1) {
            if (!inDataRetreival) {
                String pageStr = String.format("Page %d of %d",
                        dialog.getCurrentPage() + 1, dialog.getTotalPages());
                gc.drawString(pageStr, 3, 3, true);

                // draw page title if available
                GroupInfo groupInfo = this.dialog.getGroupInfo();
                ArrayList<PageInfo> pageInfoList = groupInfo.getPageInfoList();

                if ((pageInfoList != null) && (pageInfoList.size() > 0)) {
                    String title = pageInfoList.get(dialog.getCurrentPage())
                            .getTitle();
                    if (title != null) {
                        gc.drawString(title, 3, fontHeight);
                    }
                }
            } else {
                gc.drawString("Waiting for data . . .", 3, 3, true);
            }
        }

        graphData.setW(graphAreaWidth);
        graphData.setH(graphAreaHeight);

        // If we are retrieving data don't go any further.
        if (inDataRetreival) {
            gc.drawRectangle(0, 0, canvasWidth, canvasHeight);
            return;
        }

        /* get the station info */
        /* Data Access Manager */
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();

        // TimeSeriesDataManager dataManagerStn = TimeSeriesDataManager
        // .getInstance();
        try {
            String[] sa = dataManager.getStnRiverName(lid);
            if ((sa != null) && (sa[0] != null) && (sa[1] != null)) {
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
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        /* Find the flood stage */

        if (floodStage == null) {
            TimeSeriesUtil.getFloodStage(lid, graphData);
            floodStage = String.valueOf(graphData.getFloodStage());
            if (floodStage.equals("-9999.0") || floodStage.equals("")
                    || floodStage.equals(HydroConstants.MISSING_VALUE)) {
                floodStage = "M";
            }
        }

        /* Get the data traces */

        traceArray = graphData.getTraces();
        traceArray.trimToSize();

        /* find valid graph with data or not */
        findGraph = false;
        for (int j = 0; j < traceArray.size(); j++) {
            if ((validGraph != null) && !validGraph.get(j)) {
                findGraph = true;
                break;
            }
        }

        /*
         * If no data are available then draw no data available information.
         */
        if (!findGraph) {
            setForegroundColor(gc, SWT.COLOR_WHITE);
            // Draws a white border around the graph area
            int[] points = { GRAPHBORDER, GRAPHBORDER,
                    graphAreaWidth + GRAPHBORDER, GRAPHBORDER,
                    graphAreaWidth + GRAPHBORDER,
                    graphAreaHeight + GRAPHBORDER, GRAPHBORDER,
                    graphAreaHeight + GRAPHBORDER };
            gc.drawPolygon(points);

            int tmpX = canvasWidth / 2 - NO_DATA_AVAILABLE.length() / 2
                    * fontAveWidth;
            int tmpY = canvasHeight / 2;

            if (this.dialog.isInverseVideo()) {
                gc.setBackground(this.white);
            }
            gc.drawString(NO_DATA_AVAILABLE, tmpX, tmpY);
            gc.drawString(siteLabel +" fs=" + floodStage, GRAPHBORDER - 15,
                    GRAPHBORDER - fontHeight * 2);

            this.dialog.getParentDialog().enableGraphButton();
            this.dialog.getParentDialog().enableBothButton();
            return;

        } else {
            findGraphMinMax(graphData);
            setForegroundColor(gc, SWT.COLOR_CYAN);

            /* Get the data traces */
            traceArray = graphData.getTraces();
            traceArray.trimToSize();

            setForegroundColor(gc, SWT.COLOR_CYAN);
            if (pointString != null) {
                gc.drawString(pointString, GRAPHBORDER_LEFT - 55, fontHeight * 2);
            }

            if (this.dialog.isInverseVideo()) {
                gc.setBackground(this.white);
            }
            gc.drawString(siteLabel + " fs=" + floodStage, GRAPHBORDER_LEFT - 10,
                    GRAPHBORDER - fontHeight * 2);

            int index = GRAPHBORDER_LEFT - 10;
            // If labels run off the right of the canvas then need to stack them
            boolean stackLabels = false;
            int stackCount = 2; // This should start as 2 because the first stack will be above a line
            int labelStartX = 0;
            int labelStartY = 0;
            
            //store the label to be plotted on the gc legend later
            ArrayList noDataLabels = new ArrayList<String>();
            
            for (int j = 0; j < traceArray.size(); j++) {
                TraceData td = traceArray.get(j);
                boolean traceValid = true;
                if (validGraph.get(j)) {
                    traceValid = false;
                }

                if (td.getPe().equalsIgnoreCase(HydroConstants.PP)) {
                    /*
                     * Displaying PP data already. No need to do PC to PP
                     * conversion.
                     */
                    graphData.setShowpp(false);
                    graphData.setDerivepp(HydroConstants.OFF);
                }

                /* Reset buttons on PC as 1 hour PP menu. Once per drawing. */
                if (j == 0) {

                    if ((graphData.getDerivepp() != null)
                            && graphData.getDerivepp().equalsIgnoreCase(
                                    HydroConstants.OFF)) {
                        dialog.setPcasPPOff(true);
                        dialog.setPcasPPAssign(false);
                        dialog.setPcasPPInterpolate(false);

                    } else if ((graphData.getDerivepp() != null)
                            && graphData.getDerivepp().equalsIgnoreCase(
                                    HydroConstants.ASSIGN)) {
                        dialog.setPcasPPOff(false);
                        dialog.setPcasPPAssign(true);
                        dialog.setPcasPPInterpolate(false);

                    } else if ((graphData.getDerivepp() != null)
                            && graphData.getDerivepp().equalsIgnoreCase(
                                    HydroConstants.INTERPOLATE)) {
                        dialog.setPcasPPOff(false);
                        dialog.setPcasPPAssign(false);
                        dialog.setPcasPPInterpolate(true);

                    }
                }

                setPointArray(td);
                TraceData pcAsppTrace = null;

                /* ******************************************************** */
                /* If "Show PC as PP" selected then add derived PP trace to */
                /* current graph dynamically. */
                /* ******************************************************** */
                if (graphData.getShowpp()) {
                    try {
                        pcAsppTrace = (TraceData) TimeSeriesUtil.objCopy(td);
                        if (pcAsppTrace.getPe().equalsIgnoreCase(
                                HydroConstants.PC)
                                && (pcAsppTrace.getLineData().length > 1)) {
                            boolean interpMode = false;
                            boolean distribMode = false;
                            if (graphData.getDerivepp().equalsIgnoreCase(
                                    HydroConstants.INTERPOLATE)) {
                                interpMode = true;
                            } else if (graphData.getDerivepp()
                                    .equalsIgnoreCase(HydroConstants.ASSIGN)) {
                                distribMode = true;
                            }

                            int npts = TimeSeriesUtil.tsNormalize(td,
                                    pcAsppTrace,
                                    HydroConstants.INTERVAL_SECONDS,
                                    interpMode, beginDate, endDate, dialog);
                            npts = TimeSeriesUtil.tsAccumToInc3(pcAsppTrace,
                                    distribMode, dialog);
                            pcAsppTrace.setNpts(npts);
                            pcAsppTrace.setName(td.getName());
                            pcAsppTrace.setLid(td.getLid());
                            pcAsppTrace.setPe(HydroConstants.PP);
                            pcAsppTrace.setTs(td.getTs());
                            pcAsppTrace.setExtremum(td.getExtremum());

                            // draw bars
                            if (graphData.getShowpp()) {
                                drawPcBars(gc, graphData, pcAsppTrace);
                            }
                        }
                    } catch (Exception ex) {
                        ex.printStackTrace();
                    }
                }

                /* Width of the bars in pixels */
                int barWidth = 3;

                /* Data String displayed on the graph */
                String dataString = getPEDTSE(td);

                setForegroundColor(td, j, gc);

                if (traceSelected && (selectedTraceId == j)
                        && !dialog.isCancel()) { // if in edit mode
                    traceArray.get(selectedTraceId).setSelected(true);
                    gc.drawString("Active Trace:  " + dataString,
                            (GRAPHBORDER_LEFT + GRAPHBORDER_RIGHT + graphAreaWidth) / 2,
                            GRAPHBORDER / 2);
                }

                /* Precipitation physical element */
                if (td.getPe().equalsIgnoreCase(HydroConstants.PP)) {
                    precipPE = true;
                    setBackgroundColor(td, j, gc);
                    int[] ia = new int[8];
                    Region r = new Region();
                    TimeSeriesPoint[] pointArray = td.getTsData();

                    ArrayList<Region> ppointList = new ArrayList<Region>();
                    for (int i = 0; i < pointArray.length; i++) {
                        pointArray[i].setPixelX(x2pixel(graphData,
                                pointArray[i].getX().getTime()));
                        if (pointArray[i].getY() != HydroConstants.MISSING_VALUE) {
                            pointArray[i].setPixely(y2pixel(graphData,
                                    pointArray[i].getY()));
                        } else {
                            pointArray[i].setPixely(canvasHeight - GRAPHBORDER
                                    - 2);
                        }

                        /* Top left point of bar */
                        int x = pointArray[i].getPixelX() + GRAPHBORDER_LEFT;// - 20;

                        if ((x < GRAPHBORDER_LEFT) || (x > GRAPHBORDER_LEFT + graphAreaWidth)) {
                            continue;
                        }

                        int x2 = x;
                        x = x2pixel(graphData, pointArray[i].getX()
                                .getTime() - 3600000)
                                + GRAPHBORDER_LEFT;// - 20;
                        int y = pointArray[i].getPixelY() + GRAPHBORDER;
                        ia[0] = x;
                        ia[1] = y;
                        barWidth = x2 - x;
                        if (barWidth < 1) {
                            barWidth = 1;
                        }
                        ia[2] = barWidth + x;
                        ia[3] = y;
                        ia[4] = barWidth + x;
                        ia[5] = GRAPHBORDER + graphAreaHeight;
                        ia[6] = x;
                        ia[7] = GRAPHBORDER + graphAreaHeight;
                        Rectangle rect = new Rectangle(x - 5, y - 5,
                                barWidth + 10, GRAPHBORDER + graphAreaHeight);
                        Rectangle pointRect = new Rectangle(x - 5, y - 5, 10,
                                10);
                        Region pr = new Region();
                        pr.add(pointRect);
                        ppointList.add(pr);
                        r.add(rect);

                        /* Draw the bar */
                        gc.drawPolygon(ia);
                        setBackgroundColor(td, j, gc);
                        gc.fillPolygon(ia);
                        setBackgroundColor(gc, SWT.COLOR_BLACK);
                    }
                    precipPointList.add(ppointList);
                    precipRegions.add(r);
                    setBackgroundColor(gc, SWT.COLOR_BLACK);
                    setForegroundColor(gc, SWT.COLOR_CYAN);
                } else {
                    precipPE = false;
                }

                if (labelString == "") {
                    try {
                        labelString = dataManager.getShefPE(pe);
                    } catch (VizException ve) {
                        ve.printStackTrace();
                        // TODO Log error here
                    }
                }

                double max = td.getValue_ymax();
                double min = td.getValue_ymin();
                Date dateMax = td.getXmax();
                Date dateMin = td.getXmin();
                graphFormat = new SimpleDateFormat("MM/dd/yy HH'z'");
                String noDataString = dataString + "(NO DATA) ";

                setForegroundColor(td, j, gc);

                if (graphData.getTraces().size() > 1) {                
                    if (td.getLineData()!=null && td.getLineData().length>0) {
                        if (td.isTraceOn()) {
                        	if (stackLabels || ((dataString.length() * fontAveWidth) + 50 + index > canvasWidth)) {
                        		int[] xy = getLabelLocation(index, dataString, stackCount);
                        		stackCount++;
                        		labelStartX = xy[0];
                        	    labelStartY = xy[1];
                        		stackLabels = true;
                        	} else {
                        		labelStartX = index;
                        		labelStartY = GRAPHBORDER - fontHeight;
                        	}
                            gc.drawString(dataString, labelStartX, labelStartY);
                            if (!stackLabels) {
                            	index += (dataString.length() + 2) * fontAveWidth;
                            }
                        }
                    } else {
                        noDataLabels.add(noDataString);

                    }
                } else {
                    if (graphData.getTraceData(0).getPe().startsWith("Q")) {

                        if (graphData.getTraceData(0).getYmax() >= 10000) {
                            if (td.isTraceOn()) {
                                gc.drawString(
                                        dataString
                                                + " min="
                                                + twoDecimalFormat
                                                        .format(min / 1000)
                                                + " "
                                                + graphFormat.format(dateMin)
                                                + " max="
                                                + twoDecimalFormat
                                                        .format(max / 1000)
                                                + " "
                                                + graphFormat.format(dateMax),
                                        GRAPHBORDER_LEFT
                                                + (dataString.length()
                                                        * fontAveWidth * j),
                                        GRAPHBORDER - fontHeight);
                            }
                        } else {
                            if (td.isTraceOn())
                                gc.drawString(dataString + " min="
                                        + twoDecimalFormat.format(min) + " "
                                        + graphFormat.format(dateMin) + " max="
                                        + twoDecimalFormat.format(max) + " "
                                        + graphFormat.format(dateMax),
                                        GRAPHBORDER_LEFT
                                                + (dataString.length()
                                                        * fontAveWidth * j),
                                        GRAPHBORDER - fontHeight);
                        }
                    } else {
                        if (td.isTraceOn()) {
                            // only draw dataString if PC && showPP
                            if (pe.equalsIgnoreCase("PC")
                                    && graphData.getShowpp()) {
                                gc.drawString(dataString, index, GRAPHBORDER
                                        - fontHeight);
                                index += (dataString.length() + 2)
                                        * fontAveWidth;

                            } else
                                gc.drawString(dataString + " min="
                                        + twoDecimalFormat.format(min) + " "
                                        + graphFormat.format(dateMin) + " max="
                                        + twoDecimalFormat.format(max) + " "
                                        + graphFormat.format(dateMax),
                                        GRAPHBORDER_LEFT
                                                + (dataString.length()
                                                        * fontAveWidth * j),
                                        GRAPHBORDER - fontHeight);
                        }

                    }
                }

                int[] dataPts = td.getLineData();

                if (!pe.equalsIgnoreCase("PP")) {

                    /* Draw the floodstage lines if needed */
                    if (groupMode) {
                        if ((pe.toUpperCase().startsWith("H") || pe
                                .toUpperCase().startsWith("Q"))
                                && (graphData.getShowcat())) {
                            displayFloodCatLines(gc, graphData);
                        }
                    } else {
                        if ((pe.toUpperCase().startsWith("H") || pe
                                .toUpperCase().startsWith("Q"))
                                && (isFloodLineDisplay())) {
                            displayFloodCatLines(gc, graphData);
                        }
                    }

                    /* Draw points and lines */
                    if (pe.equalsIgnoreCase("PP")) {
                        dialog.getPointsMI().setEnabled(false);
                        dialog.getLinesMI().setEnabled(false);
                        dialog.getBothMI().setEnabled(false);
                    } else {
                        if (td.isSelected()) {
                            setForegroundColor(gc, SWT.COLOR_WHITE);
                        }
                        if (td.isTraceOn()) {
                            drawTrace(gc, dataPts);
                        }
                    }
                }
            }
            
            //draw no data legends
            setForegroundColor(gc, SWT.COLOR_WHITE);
            for (int i=0;i<noDataLabels.size();i++) {
            	String labelString=(String)noDataLabels.get(i);
            	if (stackLabels || ((labelString.length() * fontAveWidth) + 50 + index > canvasWidth)) {
            		int[] xy = getLabelLocation(index, labelString, stackCount);
            		stackCount++;
            		labelStartX = xy[0];
            	    labelStartY = xy[1];
            		stackLabels = true;
            	} else {
            		labelStartX = index;
            		labelStartY = GRAPHBORDER - fontHeight;
            	}
            	gc.drawString(labelString,labelStartX ,labelStartY);
                if (!stackLabels) {
                	index += (labelString.length() + 2) * fontAveWidth;
                }
            }


            // draw X/Y axis
            setForegroundColor(gc, SWT.COLOR_WHITE);

            drawYAxis(gc, graphData, labelString);
            drawXAxis(gc, graphData);

            /* Graph Area Rectangle */
            graphAreaRectangle = new Rectangle(GRAPHBORDER_LEFT, GRAPHBORDER,
                    graphAreaWidth, graphAreaHeight);

            gc.drawRectangle(graphAreaRectangle);

            drawRubberBand(gc, mouseDown);
            if (pointSelected && !precipPE) {
                setForegroundColor(gc, SWT.COLOR_WHITE);
                gc.setLineStyle(SWT.LINE_DASH);
                gc.setLineWidth(2);
                gc.drawPolyline(editPts);
            }
            // draw "DERIVED PP" after dataString when precipPE is PC && showPP
            if (pe.equalsIgnoreCase("PC") && graphData.getShowpp()) {
                setForegroundColor(gc, SWT.COLOR_YELLOW);
                gc.drawString("DERIVED PP", index, GRAPHBORDER - fontHeight);
            }
            makeRegions(traceArray);
        }
        this.dialog.getParentDialog().enableGraphButton();
        this.dialog.getParentDialog().enableBothButton();
    }
    
    private int[] getLabelLocation(int index, String dataString, int stackCount) {
    	int[] xy = new int[2];
    	
		xy[0] = canvasWidth - GRAPHBORDER_RIGHT - 75;
		xy[1] = GRAPHBORDER - (stackCount * fontHeight);
		
    	return xy;
    }

    /**
     * Scales the data points.
     * 
     * @param gd
     *            The Graph Data object
     */
    private void findGraphMinMax(GraphData gd) {
        // Scale the data to match the graph area
        double yLowest = Integer.MAX_VALUE;
        double yHighest = Integer.MIN_VALUE;
//      if (!dialog.isZoomSet()) {
      if (!zoomed) {
            gd.setYmin(yLowest);
            gd.setYmax(yHighest);
        }
        if (isFloodLineDisplay()) {
            if (!floodCatDataLoaded) {
                // Need to query the floodcat table
                TimeSeriesUtil.getFloodCategories(lid, gd);
                floodCatDataLoaded = true;
            }
        }

        TraceData td = null;

        /* loop through each trace and set the min and max values */
        for (int i = 0; i < gd.getTraces().size(); i++) {
            td = gd.getTraceData(i);
            if (td != null && td.isTraceOn()) {
                TimeSeriesPoint[] points = null;
                if (zoomed) {
                	points = td.getZoomedTsData();
                } else {
                	points = td.getTsData();
                }

                if (points != null) {
                    ArrayList<TimeSeriesPoint> pointList = new ArrayList<TimeSeriesPoint>();
                    /* Delete the specified point */
                    if ((deleteList.size() > 0) && (i == selectedTraceId)) {
                        for (int j = 0; j < points.length; j++) {
                            if (!deleteList.contains(j)) {
                                pointList.add(points[j]);
                            }

                        }
                        td.setTsData(pointList
                                .toArray(new TimeSeriesPoint[pointList.size()]));
                        deleteIndex = HydroConstants.MISSING_VALUE;
                        deleteList.clear();
                    }

                    /* Set missing */
                    // if ((setMissingIndex != HydroConstants.MISSING_VALUE)
                    if ((setMissingList.size() > 0) && (i == selectedTraceId)) {
                        for (int j = 0; j < points.length; j++) {
                            // if (j != setMissingIndex) {
                            if (!setMissingList.contains(j)) {
                                pointList.add(points[j]);
                            }
                        }
                        td.setTsData(pointList
                                .toArray(new TimeSeriesPoint[pointList.size()]));
                        setMissingIndex = HydroConstants.MISSING_VALUE;
                        setMissingList.clear();
                    }

                    /* Insert the point */
                    if ((insertedPoint != null) && (i == selectedTraceId)) {
                        boolean pointAdded = false;
                        for (int j = 0; j < points.length; j++) {
                            if (pointAdded) {
                                pointList.add(points[j]);
                                continue;
                            }

                            if (insertedPoint.getX().before(points[j].getX())) {
                                pointList.add(insertedPoint);
                                pointList.add(points[j]);
                                pointAdded = true;
                            } else if (points[j].getX().before(
                                    insertedPoint.getX())) {
                                pointList.add(points[j]);
                                if ((j + 1 < points.length)
                                        && points[j + 1].getX().after(
                                                insertedPoint.getX())) {
                                    pointList.add(insertedPoint);
                                    pointAdded = true;
                                }
                            }
                        }

                        if (!pointAdded) {
                            pointList.add(insertedPoint);
                        }
                        td.setTsData(pointList
                                .toArray(new TimeSeriesPoint[pointList.size()]));
                        insertedPoint = null;
                    }
                    if (!zoomed) {
                        TimeSeriesPoint[] pointArray = td.getTsData();
                        if (pointArray != null) {
                            for (int j = 0; j < pointArray.length; j++) {
                                if (pointArray[j] != null) {
                                    if (pointArray[j].getY() < yLowest) {
                                        yLowest = pointArray[j].getY();
                                        minDate = pointArray[j].getX();

                                        // Set the lowest ymin of the graph
                                        if (gd.getYmin() > yLowest) {
                                            gd.setYmin(yLowest);
                                        }
                                    }
                                    if (pointArray[j].getY() > yHighest) {
                                        yHighest = pointArray[j].getY();
                                        maxDate = pointArray[j].getX();

                                        // Set the highest ymax of the graph
                                        if (gd.getYmax() < yHighest) {
                                            gd.setYmax(yHighest);
                                        }
                                    }
                                }
                            } // end for
                        }
                    }

                }
            }
        } // end for

        if (zoomed) {
            if (!dialog.isZoomAction() && dialog.isSelectZoom()) {
                if (rubberBandY1 > rubberBandY2) {
                    swapPoints(rubberBandX1, rubberBandX2, rubberBandY1,
                            rubberBandY2);
                }
                Date xMin = pixel2x(gd, rubberBandX1-GRAPHBORDER_LEFT);
                Date xMax = pixel2x(gd, rubberBandX2-GRAPHBORDER_LEFT);

                gd.setXMin(xMin);
                gd.setXMax(xMax);
                gd.setX(gd.getXMax().getTime() - gd.getXMin().getTime());

                double ymin = pixel2y(gd, rubberBandY2);
                double ymax = pixel2y(gd, rubberBandY1);

                if (ymax > gd.getYmax()) {
                    ymax = gd.getYmax();
                }

                if (ymin < gd.getYmin()) {
                    ymin = gd.getYmin();
                }

                gd.setYmin(ymin);
                gd.setYmax(ymax);
                gd.setY2(gd.getYmax2() - gd.getYmin2());
                dialog.setSelectZoom(false);
                dialog.setZoomAction(false);

            }
        } else {
            gd.setXMin(beginDate);
            gd.setXMax(endDate);
            gd.setX(endDate.getTime() / HydroConstants.MILLIS_PER_MINUTE
                    / -beginDate.getTime() / HydroConstants.MILLIS_PER_MINUTE);

            /* Store real max and min from trace for display purpose */
            if (td != null) {
                td.setValue_ymin(yLowest);
                td.setValue_ymax(yHighest);
                td.setXmin(minDate);
                td.setXmax(maxDate);
            }

            gd.setY2(gd.getYmax2() - gd.getYmin2());
        }

        /* if only rain data types then skip the flood stage */
        boolean useFloodStage = false;
        for (TraceData trace: gd.getTraces()) {
            if (trace.getPe().startsWith("H") || trace.getPe().startsWith("h") || 
                    trace.getPe().startsWith("Q") || trace.getPe().startsWith("q")) {
                useFloodStage = true;
                break;
            }   
        }
        
        /* Add the flood stages if selected */
        if (dialog.getBatchDataAndCategoriesMI().getSelection() && useFloodStage && !zoomed) {
            // Get the stages
            double floodCatMinor = gd.getMinorStage();
            double floodCatMajor = gd.getMajorStage();
            double floodStage = gd.getFloodStage();

            if (floodCatMinor == -9999) {
                floodCatMinor = gd.getMinorFlow();
                if (floodCatMinor != -9999) {
                    floodCatMinor = StageDischargeUtils.discharge2stage(lid,
                            floodCatMinor);
                }
            }

            if (floodCatMajor == -9999) {
                floodCatMajor = gd.getMajorFlow();
                if (floodCatMajor != -9999) {
                    floodCatMajor = StageDischargeUtils.discharge2stage(lid,
                            floodCatMajor);
                }
            }

            if (floodStage == -9999) {
                floodStage = gd.getFloodFlow();
                if (floodStage != -9999) {
                    floodStage = StageDischargeUtils.discharge2stage(lid,
                            floodStage);
                }
            }

            // Find the max, min values
            double max = -9999;
            double min = 9999;

            if ((floodCatMinor < min) && (floodCatMinor != -9999)) {
                min = floodCatMinor;
            }
            if (floodCatMinor > max) {
                max = floodCatMinor;
            }

            if ((floodCatMajor < min) && (floodCatMajor != -9999)) {
                min = floodCatMajor;
            }
            if (floodCatMajor > max) {
                max = floodCatMajor;
            }

            if ((floodStage < min) && (floodStage != -9999)) {
                min = floodStage;
            }
            if (floodStage > max) {
                max = floodStage;
            }

            if (gd.getYmin() > min) {
                gd.setYmin(Math.floor(min));
            }
            if (gd.getYmax() < max) {
                gd.setYmax(Math.ceil(max));
            }
        }

        if (this.scalingManager == null) {
            scalingManager = new ScaleManager(gd.getYmin(), gd.getYmax());
        } else {
            scalingManager.setMinDataValue(gd.getYmin());
            scalingManager.setMaxDataValue(gd.getYmax());
        }

        scalingManager.setZoomFlag(zoomed);
        gd.setYmin(scalingManager.getMinScaleValue());
        gd.setYmax(scalingManager.getMaxScaleValue());
    }

    private ForecastData createPoint(TraceData td, TimeSeriesPoint point) {
        ForecastData data = new ForecastData();
        if (!td.getDur().equals("")) {
            data.setDur(TimeSeriesUtil.convertDur2Short(td.getDur()
                    .toUpperCase().toCharArray()[0]));
        } else {
            data.setDur(0);
        }
        data.setExtremum(td.getExtremum());
        data.setLid(td.getLid());

        if (td.getTs().toUpperCase().startsWith("F")
                || td.getTs().toUpperCase().startsWith("C")) {
            data.setValidTime(point.getX());
            data.setBasisTime(td.getBasistime());
            data.setPreviousValue(point.getY());

            data.setProductTime(td.getProductTime());
            data.setProductID(td.getProductId());
        } else {
            data.setValidTime(point.getX());
            data.setBasisTime(td.getBasistime());
            data.setPreviousValue(point.getY());

            data.setObsTime(point.getX());
            data.setValidTime(td.getProductTime());
            data.setProductTime(td.getProductTime());
            data.setProductID(td.getProductId());
        }
        data.setPe(td.getPe());
        data.setTs(td.getTs());
        data.setValue(TimeSeriesUtil.round(point.getY(), 2));
        data.setPostingTime(Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                .getTime());

        return data;
    }

    /**
     * Get the data for the trace.
     * 
     * @param td
     *            The TraceData object
     */
    private void getData(TraceData td) {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        ArrayList<TimeSeriesPoint> points = new ArrayList<TimeSeriesPoint>();
        ArrayList<TimeSeriesPoint> pointsbak = new ArrayList<TimeSeriesPoint>();
        lid = td.getLid();
        ts = td.getTs().toUpperCase();
        pe = td.getPe().toUpperCase();
        dur = td.getDur();
        String ext = td.getExtremum().toUpperCase();

        String tablename = DbUtils.getTableName(pe, ts);

        /* Get the data from IHFS and store in TimeSeriesPoint object */
        try {
            List<Object[]> data = dataManager.getGraphData(tablename, lid, pe,
                    ts, dur, ext, beginDate, endDate);

            if ((data != null) && (data.size() > 0)) {
                for (int i = 0; i < data.size(); i++) {
                    TimeSeriesPoint p = new TimeSeriesPoint();
                    TimeSeriesPoint pbak = new TimeSeriesPoint();
                    Object[] oa = data.get(i);
                    Date d = (Date) oa[1];
                    p.setX(d);
                    pbak.setX(d);
                    if (!((Double) oa[2] == HydroConstants.MISSING_VALUE)) {
                        p.setY((Double) oa[2]);
                        pbak.setY((Double) oa[2]);
                        points.add(p);
                        pointsbak.add(pbak);
                    }
                }
                noDataAvailable = false;
            } else {
                noDataAvailable = true;
            }
        } catch (VizException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }

        td.setTsData(points.toArray(new TimeSeriesPoint[points.size()]));

        td.setPreviousTsData(pointsbak.toArray(new TimeSeriesPoint[pointsbak
                .size()]));
    }

    private String getFcstPEDTSE(TraceData td) {
        String fcst = getPEDTSE(td);
        if (td.getBasistime() != null) {
            fcst = fcst.concat(" " + dateFormat.format(td.getBasistime()));
        }
        return fcst;
    }

    /**
     * Build the PEDTSE String for display.
     * 
     * @param td
     *            The Trace Data
     * @return The PEDTSE String
     */
    private String getPEDTSE(TraceData td) {
        StringBuilder sb = new StringBuilder();
        if (td.getPe() != null) {
            sb.append(td.getPe() + " ");
        }
        String dur = td.getDur();
        if ((dur == null) || dur.equals("")) {
            sb.append("0 ");
        } else {
            sb.append(dur + " ");
        }
        if (td.getTs() != null) {
            sb.append(td.getTs() + " ");
        }
        if (td.getExtremum() != null) {
            sb.append(td.getExtremum() + " ");
        }
        return sb.toString().toUpperCase();
    }

    /**
     * Display the right click popup menu.
     */
    private void popupMenu() {
        List<TraceData> traceList = getTraceList();
        Menu m = new Menu(parentComp.getShell(), SWT.POP_UP);

        for (int i = 0; i < traceList.size(); i++) {
            TraceData td = traceList.get(i);
            String s = null;
            if (td.isForecast()) {
                s = getFcstPEDTSE(td);
            } else {
                s = getPEDTSE(td);
            }
            MenuItem mi = new MenuItem(m, SWT.CHECK);
            if (td.getLineData()!=null && td.getLineData().length>0) {
                if (td.isTraceOn())
                    mi.setSelection(true);
                else
                    mi.setSelection(false);
            } else {
                mi.setSelection(false);
                s = s.concat("" + "NO DATA");
            }
            mi.setText(s);
            mi.setData(td);
            mi.addListener(SWT.Selection, new Listener() {
                public void handleEvent(Event event) {
                    handleSelection(event);
                }
            });
        }
        // We need to make the menu visible
        m.setVisible(true);
    }

    private void handleSelection(Event event) {
        MenuItem item = (MenuItem) event.widget;
        TraceData trace = (TraceData) item.getData();
        trace.setTraceOn(!trace.isTraceOn());
        if (trace.isTraceOn()) {
            item.setSelection(true);
        } else {
            item.setSelection(false);
        }

        this.redraw();
    }

    /**
     * Build the point string that is displayed when clicking on the graph
     * 
     * @param e
     *            the MouseEvent
     */
    private String buildPointString(int x, int y) {
        StringBuilder sb = new StringBuilder();
        Date xValue = pixel2x(graphData, x-GRAPHBORDER_LEFT);
        SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy HH:mm'Z'");
        format.setTimeZone(TimeZone.getTimeZone("GMT"));
        sb.append(format.format(xValue));
        double yValue = pixel2y(graphData, y);
        String units = FEET;
        boolean isRiverData = true;
        boolean isStage= true;
        ArrayList<TraceData> traces = graphData.getTraces();
        for (TraceData trace : traces) {
            if (!trace.getPe().toUpperCase().startsWith("H")
                    && !trace.getPe().toUpperCase().startsWith("Q")) {
                isRiverData = false;
            }
            if (trace.getPe().toUpperCase().startsWith("Q")) {
                isStage=false;
            }
        }
        
        if (isRiverData) {        	
        	
            if (isStage) {
            	/**
                 * Convert the stage to discharge for the location and stage value passed in.
                 */
        		double q = StageDischargeUtils.stage2discharge(lid, yValue);
        		//check for rating curve
        		if (q != HydroConstants.RATING_CONVERT_FAILED) {
	                if (q > 10000) {
	                	units = KCFS;
	                	q = q / 1000;	                	
	                }else {
	                	units = CFS;
	                } 
	                	sb.append(" value=" + twoDecimalFormat.format(yValue)
        					+ " " + FEET + " ");
	                	sb.append(String.format("%8.1f", q) + " " + units);
        		}else {
        			sb.append(" value=" + twoDecimalFormat.format(yValue));
        		} 
        
            }else {  
            	/**
                 * Convert the discharge to stage for the location and discharge value passed in.
                 */
        		double q = StageDischargeUtils.discharge2stage(lid, yValue);
        		//check for rating curve
        		if (q != HydroConstants.RATING_CONVERT_FAILED) {
        			sb.append(" value=" + twoDecimalFormat.format(yValue)
        							+ " " + CFS + " ");
        			sb.append(String.format("%8.1f", q) + " " + FEET);
        		}else {
        			sb.append(" value=" + twoDecimalFormat.format(yValue));
        		}
        	}
        	
        }else {
           sb.append("  value=" + twoDecimalFormat.format(yValue)); 
        }

        return sb.toString();
    }

    /**
     * Handle the Mouse Move Event.
     * 
     * @param e
     *            The MouseEvent
     */
    private void handleMouseMoveEvent(MouseEvent e) {
        if (mouseDown && !pointSelected && !dialog.isDelete()
                && !dialog.isSetMissing()) {
            if (graphAreaRectangle != null) {
                if (!dialog.isZoomAction()) {
                    if (graphAreaRectangle.contains(e.x, e.y)) {
                        setCursor(crossHairCursor);
                        pointString = buildPointString(e.x, e.y);
                        currentX = e.x;
                        currentY = e.y;
                        redraw();
                    }
                } else {
                    if (graphAreaRectangle.contains(e.x, e.y)
                            && graphAreaRectangle.contains(rubberBandX1,
                                    rubberBandY1)) {
                        boundingBox = new Rectangle(rubberBandX1, rubberBandY1,
                                e.x - rubberBandX1, e.y - rubberBandY1);
                        redraw();
                    }
                }
            }
        } else if (dialog.isSelectMove() && traceSelected && !pointSelected) {
            // This catches the move event before point is selected
            if (!precipPE) {
                ArrayList<Region> prl = pointList.get(selectedTraceId);
                for (int i = 0; i < prl.size(); i++) {
                    if (prl.get(i).contains(e.x, e.y)) {
                        setCursor(northSouthCursor);
                        break;
                    } else {
                        pointSelected = false;
                        setCursor(arrowCursor);
                    }
                }
            } else {
                ArrayList<Region> ppl = precipPointList.get(selectedTraceId);
                for (int i = 0; i < ppl.size(); i++) {
                    if (ppl.get(i).contains(e.x, e.y)) {
                        setCursor(northSouthCursor);
                        break;
                    } else {
                        pointSelected = false;
                        setCursor(arrowCursor);
                    }
                }
            }
        } else if  ( traceSelected && dialog.isDelete()) {
            if (mouseDown) {
                int deleteX1 = selectedX;
                int deleteY1 = selectedY;
                deleteRect = new Rectangle(deleteX1, deleteY1, e.x - deleteX1,
                        e.y - deleteY1);
                redraw();
            } else {
                if (precipPE) {
                    ArrayList<Region> ppl = precipPointList
                            .get(selectedTraceId);
                    for (int i = 0; i < ppl.size(); i++) {
                        if (ppl.get(i).contains(e.x, e.y)) {
                            setCursor(northSouthCursor);
                            break;
                        } else {
                            pointSelected = false;
                            setCursor(arrowCursor);
                        }
                    }
                } else {
                    ArrayList<Region> prl = pointList.get(selectedTraceId);
                    for (int i = 0; i < prl.size(); i++) {
                        if (prl.get(i).contains(e.x, e.y)) {
                            setCursor(handCursor);
                            break;
                        } else {
                            pointSelected = false;
                            setCursor(arrowCursor);
                        }
                    }
                }
            }
        } else if (dialog.isSetMissing()) {
            if (mouseDown) {
                int setMissingX1 = selectedX;
                int setMissingY1 = selectedY;
                setMissingRect = new Rectangle(setMissingX1, setMissingY1, e.x
                        - setMissingX1, e.y - setMissingY1);
                redraw();
            } else {
                if (precipPE) {
                    ArrayList<Region> ppl = precipPointList
                            .get(selectedTraceId);
                    for (int i = 0; i < ppl.size(); i++) {
                        if (ppl.get(i).contains(e.x, e.y)) {
                            setCursor(handCursor);
                            break;
                        } else {
                            pointSelected = false;
                            setCursor(arrowCursor);
                        }
                    }
                } else {
                    ArrayList<Region> prl = pointList.get(selectedTraceId);
                    for (int i = 0; i < prl.size(); i++) {
                        if (prl.get(i).contains(e.x, e.y)) {
                            setCursor(handCursor);
                            break;
                        } else {
                            pointSelected = false;
                            setCursor(arrowCursor);
                        }
                    }
                }
            }
        } else {
            if (pointSelected && mouseDown) {
                // moving a data point
                setCursor(northSouthCursor);
                int[] dataPts = graphData.getTraceData(selectedTraceId)
                        .getLineData();
                int x1 = -999;
                int y1 = -999;
                int x2 = -999;
                int y2 = -999;

                // Selected point
                int pointx = selectedX;
                int pointy = e.y;

                if (!precipPE) {
                    // Redraw the line from point p-1 to p+1
                    int index = selectionIndex * 2 - 2;
                    if (index >= 0) {
                        // point to the left of the selected point
                        x1 = dataPts[selectionIndex * 2 - 2];
                        y1 = dataPts[selectionIndex * 2 - 1];
                    }

                    index = selectionIndex * 2 + 3;
                    if (index < dataPts.length) {
                        // point to the right of the selected point
                        x2 = dataPts[selectionIndex * 2 + 2];
                        y2 = dataPts[selectionIndex * 2 + 3];
                    }

                    if ((x1 == -999) || (y1 == -999)) {
                        editPts[0] = pointx;
                        editPts[1] = pointy;
                    } else {
                        editPts[0] = x1;
                        editPts[1] = y1;
                    }
                    editPts[2] = pointx;
                    editPts[3] = pointy;
                    if ((x2 == -999) || (y2 == -999)) {
                        editPts[4] = pointx;
                        editPts[5] = pointy;
                    } else {
                        editPts[4] = x2;
                        editPts[5] = y2;
                    }
                }

                pointString = buildPointString(selectedX, e.y);
                GC gc = new GC(tsCanvas);
                Point extent = gc.stringExtent(pointString);
                tsCanvas.redraw(GRAPHBORDER_LEFT, fontHeight * 2, extent.x,
                        extent.y, true);
                redraw();
            } else {
                if (dialog.isSelectTrace()) {
                    if (!traceSelected) {
                        if (precipPE) {
                            for (int i = 0; i < precipRegions.size(); i++) {
                                if (precipRegions.get(i).contains(e.x, e.y)) {
                                    setCursor(handCursor);
                                    selectableTrace = true;
                                    selectedTraceId = i;
                                    break;
                                } else {
                                    setCursor(arrowCursor);
                                    selectableTrace = false;
                                }
                            }
                        } else {
                        	int traceId=findTracePoint(e.x,e.y);
                        	if (traceId>=0){
                        		setCursor(handCursor);
                                selectableTrace = true;
                                selectedTraceId = traceId;
                        	}else {
                        		setCursor(arrowCursor);
                                selectableTrace = false;
                        	}
                        }
                    }
                }
            }
        }
    }

    /**
     * 
     * @param x : location x (of mouse pointer)
     * @param y : location y (of mouse pointer)
     * @return the nearest trace. -999 if x,y is too far away
     */
    private int findTracePoint(int x, int y) {
    	double distance=Double.MAX_VALUE;
    	int choosingTrace=-999;
    	ArrayList<TraceData> traceList=graphData.getTraces();
    	
    	//this loop is to find the closest point/line for every trace that's on
    	int closePoints[] = new int[traceList.size()];
        for (int traceIndex=0; traceIndex< traceList.size(); traceIndex++) {
        	TraceData td= traceList.get(traceIndex);
        	closePoints[traceIndex]=-999; //default to not found
        	int[] dataPts = td.getLineData(); //dataPts stores x1,y1,x2,y2,x3...
            if (td.isTraceOn() && dataPts!=null) {
                for (int i = 0; i < dataPts.length - 1; i+= 2) {
                    int x1 = dataPts[i];
                    int y1 = dataPts[i + 1];
                    int x2 = x1;
                    int y2 = y1;
                    if (i+4 <= dataPts.length) {
                    	x2 = dataPts[i + 2];
                    	y2 = dataPts[i + 3];
                    }
                    double curDistance=Double.MAX_VALUE;
                    if (x1==x2 && y1==y2) //distance from a point
                    	curDistance=Math.sqrt(Math.pow(x-x1,2)+Math.pow(y-y1, 2));
                    else {//distance from a line segment
                    //from http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
                        double p2X=x2-x1;
                        double p2Y=y2-y1;

                        double something=p2X*p2X + p2Y*p2Y;
                        
                        double u=((x-x1)*p2X+(y-y1)*p2Y)/something;

                        if (u > 1)
                            u = 1;
                        else if (u < 0)
                            u = 0;

                        double xx=x1+u*p2X;
                        double yy=y1+u*p2Y;

                        double dx=xx-x;
                        double dy=yy-y;

                        curDistance=Math.sqrt(dx*dx+dy*dy);
                    }
                    if (curDistance<distance) {
                    	distance=curDistance;
                    	closePoints[traceIndex]=i;
                    	choosingTrace=traceIndex;
                    }
                }
            }
        }

        if (distance<20) //if less than 20 pixels away
        	return choosingTrace;

        return -999;
    }
    /**
     * Handle the Mouse Down Events
     * 
     * @param e
     *            The mouse event
     */
    private void handleMouseDownEvent(MouseEvent e) {
        mouseDown = true;
        selectedX = e.x;
        selectedY = e.y;

        /* Display a popup menu if right clicked */
        if ((e.button == 2) || (e.button == 3)) {
            popupMenu();
            mouseDown = false;
        } else if (!traceSelected && dialog.isSelectTrace()) {
            // Selecting trace
            if (selectableTrace) {
                traceSelected = true;
                dialog.enableEditMenus();
                // redraw so line is white
                redraw();
            }
        } else if (traceSelected && dialog.isSelectMove()) {
            // loop to see if a dot is selected
            if (precipPE) {
                ArrayList<Region> prl = precipPointList.get(selectedTraceId);
                for (int i = 0; i < prl.size(); i++) {
                    if (prl.get(i).contains(e.x, e.y)) {
                        setCursor(northSouthCursor);
                        pointSelected = true;
                        selectionIndex = i;
                        break;
                    } else {
                        pointSelected = false;
                    }
                }
            } else {
                ArrayList<Region> prl = pointList.get(selectedTraceId);
                for (int i = 0; i < prl.size(); i++) {
                    if (prl.get(i).contains(e.x, e.y)) {
                        setCursor(northSouthCursor);
                        pointSelected = true;
                        selectionIndex = i;
                        break;
                    } else {
                        pointSelected = false;
                    }
                }
            }
        } else if (traceSelected && dialog.isDelete()) {
            if (precipPE) {
                ArrayList<Region> ppl = precipPointList.get(selectedTraceId);
                for (int i = 0; i < ppl.size(); i++) {
                    if (ppl.get(i).contains(e.x, e.y)) {
                        deleteIndex = i;
                        deleteList.add(deleteIndex);
                        break;
                    }
                }
            } else {
                ArrayList<Region> prl = pointList.get(selectedTraceId);
                for (int i = 0; i < prl.size(); i++) {
                    if (prl.get(i).contains(e.x, e.y)) {
                        deleteIndex = i;
                        deleteList.add(deleteIndex);
                        break;
                    }
                }
            }
            TraceData td = graphData.getTraceData(selectedTraceId);
            TimeSeriesPoint[] points = td.getTsData();
            for (int j = 0; j < points.length; j++) {
                if (j == deleteIndex) {

                    // Make an observation object for deletion
                    ForecastData data = createPoint(td, points[j]);
                    dialog.addDeletePoint(data);
                    getAgain = false;
                    break;
                }
            }
        } else if (traceSelected && dialog.isSetMissing()) {
            if (precipPE) {
                ArrayList<Region> ppl = precipPointList.get(selectedTraceId);
                for (int i = 0; i < ppl.size(); i++) {
                    if (ppl.get(i).contains(e.x, e.y)) {
                        setMissingIndex = i;
                        setMissingList.add(setMissingIndex);
                        break;
                    }
                }
            } else {
                ArrayList<Region> prl = pointList.get(selectedTraceId);
                for (int i = 0; i < prl.size(); i++) {
                    if (prl.get(i).contains(e.x, e.y)) {
                        setMissingIndex = i;
                        setMissingList.add(setMissingIndex);
                        break;
                    }
                }
            }

            TraceData td = graphData.getTraceData(selectedTraceId);
            TimeSeriesPoint[] points = null;
            if (zoomed) {
            	points = td.getZoomedTsData();
            } else {
            	points = td.getTsData();
            }

            for (int j = 0; j < points.length; j++) {
                if (j == setMissingIndex) {
                    // Make an observation object for setting to missing
                    ForecastData data = createPoint(td, points[j]);
                    data.setValue(new Double(HydroConstants.MISSING_VALUE));
                    dialog.addEditPoint(data);
                    getAgain = false;
                    break;
                }
            }
        } else {
            if (dialog.isZoomAction()) {
                rubberBandX1 = e.x;
                rubberBandY1 = e.y;
                boundingBox = new Rectangle(rubberBandX1, rubberBandY1, 0, 0);
            } else {
                setCursor(crossHairCursor);
            }
            
            if (dialog.isReset()) {
            	zoomed = false;
            	dialog.setReset(false);
            }
        }
    }

    /**
     * Handle the Mouse Up Events
     * 
     * @param e
     *            Mouse Event
     */
    private void handleMouseUpEvent(MouseEvent e) {
        mouseDown = false;

        /* Null the point string or the last location stays displayed */
        pointString = null;
        rubberBandX2 = e.x;
        rubberBandY2 = e.y;

        if (dialog.isZoomAction()) {
        	if ((rubberBandX1 != rubberBandX2) || //avoid click to zoom in on 1 point
        			(rubberBandY1 != rubberBandY2)) {
        		dialog.setZoom(true);
        		zoomed = true;
        		dialog.setZoomAction(false);
        	}
        } else if (pointSelected) {
            int[] dataPts = graphData.getTraceData(selectedTraceId)
                    .getLineData();
            dataPts[selectionIndex * 2 + 1] = e.y;

            graphData.getTraceData(selectedTraceId).setLineData(dataPts);

            setEditData(e.y);
            pointSelected = false;
            getAgain = false;
        } else if (traceSelected && dialog.isInsert()) {
            insertedPoint = new TimeSeriesPoint();
            insertedPoint.setX(pixel2x(graphData, e.x-GRAPHBORDER_LEFT));
            insertedPoint.setY(pixel2y(graphData, e.y));
            ForecastData data = createPoint(
                    graphData.getTraceData(selectedTraceId), insertedPoint);
            dialog.addInsertPoint(data);
            getAgain = false;
        } else if (traceSelected && dialog.isDelete() && (deleteRect != null)) {
            TraceData td = graphData.getTraces().get(selectedTraceId);
            TimeSeriesPoint[] pointArray= null;
            if (!zoomed ){
                 pointArray = td.getTsData();
            } else {
                 pointArray = td.getZoomedTsData();                
            }         

            for (int i = 0; i < pointArray.length; i++) {
                if (deleteRect.contains(pointArray[i].getPixelX(),
                        pointArray[i].getPixelY())) {
                    ForecastData data = new ForecastData();
                    data.setPe(td.getPe());
                    data.setTs(td.getTs());
                    data.setLid(td.getLid());
                    data.setDur(Integer.parseInt(td.getDur()));
                    data.setExtremum(td.getExtremum());
                    data.setProductTime(td.getProductTime());
                    data.setValue(new Double(pointArray[i].getY()));

                    if (td.getTs().toUpperCase().startsWith("F")
                            || td.getTs().toUpperCase().startsWith("C")) {
                        data.setValidTime(pointArray[i].getX());
                        data.setBasisTime(td.getBasistime());
                    } else {
                        data.setObsTime(pointArray[i].getX());
                        data.setValidTime(td.getProductTime());
                    }

                    dialog.addDeletePoint(data);
                    deleteList.add(i);
                }
            }

            deleteRect = null;

        } else if (traceSelected && dialog.isSetMissing()
                && (setMissingRect != null)) {
            TraceData td = graphData.getTraces().get(selectedTraceId);

            TimeSeriesPoint[] pointArray = td.getTsData();

            for (int i = 0; i < pointArray.length; i++) {
                if (setMissingRect.contains(pointArray[i].getPixelX(),
                        pointArray[i].getPixelY())) {
                    ForecastData data = createPoint(td, pointArray[i]);
                    data.setValue(new Double(HydroConstants.MISSING_VALUE));
                    dialog.addEditPoint(data);
                    setMissingList.add(i);
                }
            }
            setMissingRect = null;
        }

        /* Get the data traces */
        traceArray = graphData.getTraces();
        traceArray.trimToSize();

        // Set true so new regions will be created
        createRegions = true;

        setCursor(arrowCursor);
        redraw();
    }

    /**
     *  Reset the time series back to non-zoom-in
     */
    public void resetTS(){
    	dialog.setZoom(false);
		setZoomed(false);
		dialog.setZoomAction(false);
        traceArray = graphData.getTraces();
        traceArray.trimToSize();

        // Set true so new regions will be created
        createRegions = true;

        setCursor(arrowCursor);
        redraw();

    	return;
    }
    
    /**
     * Save the edited data.
     * 
     * @param dataPts
     */
    private void setEditData(int value) {
        TraceData td = graphData.getTraceData(selectedTraceId);
        TimeSeriesPoint[] pa = null;
        TimeSeriesPoint tsp = null;

        if (zoomed) {
            pa = td.getZoomedTsData();
            tsp = pa[selectionIndex];
            tsp.setY(pixel2y(graphData, value));
            graphData.getTraceData(selectedTraceId).setZoomedTsData(pa);
        } else {
            pa = td.getTsData();
            tsp = pa[selectionIndex];
            tsp.setY(pixel2y(graphData, value));
            graphData.getTraceData(selectedTraceId).setTsData(pa);

        }

        // set the value back into the list
        pa[selectionIndex] = tsp;
        ForecastData data = createPoint(td, tsp);
        dialog.addEditPoint(data);

        graphData.getTraceData(selectedTraceId).setTsData(pa);
    }

    /**
     * Make the regions around the lines and points
     * 
     * @param dataPts
     *            the points that make up the lines
     */
    private void makeRegions(ArrayList<TraceData> traceList) {
        if (createRegions == true) {
            /* Dispose of the previous regions */
            for (Region r : regionList) {
                if (r.isDisposed() == false) {
                    r.dispose();
                }
            }
            for (ArrayList<Region> al : pointList) {
                for (Region r : al) {
                    if (r.isDisposed() == false) {
                        r.dispose();
                    }
                }
            }
            for (ArrayList<Region> al : listRegionList) {
                for (Region r : al) {
                    if (r.isDisposed() == false) {
                        r.dispose();
                    }
                }
            }
            regionList.clear();
            pointList.clear();
            listRegionList.clear();

            int dy = 15;

            for (TraceData td : traceList) {
                if (td.isTraceOn()) {
                    int[] dataPts = td.getLineData();

                    // Region around the line
                    Region editRegion = new Region();

                    // Holds the regions for the individual points
                    ArrayList<Region> pointRegionList = new ArrayList<Region>();

                    int x1 = HydroConstants.MISSING_VALUE;
                    int x2 = HydroConstants.MISSING_VALUE;
                    int y1 = HydroConstants.MISSING_VALUE;
                    int y2 = HydroConstants.MISSING_VALUE;

                    if (dataPts.length > 2) {
                        for (int i = 0; i < dataPts.length - 3; i += 2) {
                            int[] ia = new int[8];
                            x1 = dataPts[i];
                            y1 = dataPts[i + 1];
                            x2 = dataPts[i + 2];
                            y2 = dataPts[i + 3];

                            /*
                             * ia[0] = x1; ia[1] = y1 - dy; ia[2] = x2; ia[3] =
                             * y2 - dy; ia[4] = x2; ia[5] = y2 + dy; ia[6] = x1;
                             * ia[7] = y1 + dy;
                             */
                            ia[0] = x1;
                            ia[1] = y1 - dy;
                            ia[2] = x2;
                            ia[3] = y2 - dy;
                            ia[4] = x2;

                            if (y2 > y1) {
                                ia[5] = y2 + (y2 - y1) / 5;
                            } else if (y2 < y1) {
                                ia[5] = y2 - (y1 - y2) / 5;
                            } else
                                ia[5] = y2 + 10;

                            ia[6] = x1;
                            ia[7] = y1 + dy;
                            editRegion.add(ia);

                            // Point region
                            Region pr = new Region();
                            pr.add(x1 - (dy / 2), y1 - (dy / 2), dy + 3, dy + 3);
                            pointRegionList.add(pr);
                        }
                        Region pr = new Region();
                        pr.add(x2 - (dy / 2), y2 - (dy / 2), dy + 3, dy + 3);
                        pointRegionList.add(pr);
                        regionList.add(editRegion);
                        pointList.add(pointRegionList);
                        listRegionList.add(regionList);
                        createRegions = false;
                    } else if (dataPts.length > 0) {
                        x1 = dataPts[0];
                        y1 = dataPts[1];
                        Region pr = new Region();
                        pr.add(x1 - (dy / 2), y1 - (dy / 2), dy + 3, dy + 3);
                        pointRegionList.add(pr);
                        pointList.add(pointRegionList);
                        regionList.add(pr);
                        listRegionList.add(regionList);
                        createRegions = false;
                    } else {
                        regionList.add(editRegion);
                        pointList.add(pointRegionList);
                    }
                }
            }
        }
    }

    /**
     * Set the point array to be drawn.
     * 
     * @param td
     *            The TraceData
     */
    private void setPointArray(TraceData td) {
        /* Array of points for the plot */
        TimeSeriesPoint[] pointArray = td.getTsData();

        ArrayList<TimeSeriesPoint> zoomedPointList = new ArrayList<TimeSeriesPoint>();

        int[] dataPts = new int[pointArray.length * 2];
        ArrayList<Integer> dataPtList = new ArrayList<Integer>();
        int dataIndex = 0;
        int zoomDataIndex=0;
        ArrayList<Integer> al = new ArrayList<Integer>();
        for (int i = 0; i < pointArray.length; i++) {
            if (pointArray[i].getY() != HydroConstants.MISSING_VALUE) {
                if (!zoomed) {
                    dataPtList
                            .add(GRAPHBORDER_LEFT
                                    + x2pixel(graphData, pointArray[i].getX()
                                            .getTime()));
                    pointArray[i].setPixelX(dataPtList.get(dataIndex));
                    dataIndex++;
                    dataPtList.add(GRAPHBORDER
                            * 2
                            + graphAreaHeight
                            - (bottomBorder - y2pixel(graphData,
                                    pointArray[i].getY())));

                    pointArray[i].setPixely(dataPtList.get(dataIndex));
                    dataIndex++;

                    dataPts = new int[dataPtList.size()];
                    for (int j = 0; j < dataPtList.size(); j++) {
                        dataPts[j] = dataPtList.get(j);
                    }
                } else {
                    // Only take points that fall within the window
                    if ((pointArray[i].getX().after(graphData.getXMin()) && pointArray[i]
                            .getX().before(graphData.getXMax()))
                            && ((pointArray[i].getY() > graphData.getYmin()) && (pointArray[i]
                                    .getY() < graphData.getYmax()))) {
                        zoomedPointList.add(pointArray[i]);
                        al.add(GRAPHBORDER_LEFT
                                + x2pixel(graphData, pointArray[i].getX()
                                        .getTime()));
                        pointArray[i].setPixelX(al.get(zoomDataIndex));
                        zoomDataIndex++;
                        al.add(GRAPHBORDER
                                * 2
                                + graphAreaHeight
                                - (lowerAxis - y2pixel(graphData,
                                        pointArray[i].getY())));
                        pointArray[i].setPixely(al.get(zoomDataIndex));
                        zoomDataIndex++;
                    }
                    int[] pts = new int[al.size()];
                    for (int j = 0; j < al.size(); j++) {
                        pts[j] = al.get(j);
                    }
                    dataPts = pts;

                    dataPts = new int[al.size()];
                    for (int j = 0; j < al.size(); j++) {
                        dataPts[j] = al.get(j);
                    }
                }
            }
        }
        td.setLineData(dataPts);
        td.setZoomedTsData(zoomedPointList
                .toArray(new TimeSeriesPoint[zoomedPointList.size()]));
    }

    /**
     * Cancel the Edits
     */
    void cancelEdit() {
        TimeSeriesPoint[] tpa = graphData.getTraceData(selectedTraceId)
                .getPreviousTsData();
        if (tpa != null) {
            ArrayList<TimeSeriesPoint> origTpa = new ArrayList<TimeSeriesPoint>();
            origTpa.ensureCapacity(tpa.length);
            for (TimeSeriesPoint tsp : tpa) {
                TimeSeriesPoint newTsp = new TimeSeriesPoint();
                newTsp.setMode(tsp.getMode());
                newTsp.setOld_value(tsp.getOld_value());
                newTsp.setPixelX(tsp.getPixelX());
                newTsp.setPixely(tsp.getPixelY());
                newTsp.setProbability(tsp.getProbability());
                newTsp.setQuality_code(tsp.getQuality_code());
                newTsp.setRevision(tsp.getRevision());
                newTsp.setSelected(false);
                newTsp.setX(tsp.getX());
                newTsp.setY(tsp.getY());

                origTpa.add(newTsp);
            }
            graphData.getTraceData(selectedTraceId).setTsData(
                    origTpa.toArray(new TimeSeriesPoint[origTpa.size()]));
            setPointArray(graphData.getTraceData(selectedTraceId));
        }

        /* Reset the selection */
        if (traceArray != null) {
            for (TraceData td : traceArray) {
                td.setSelected(false);
            }
        }
    }

    /**
     * if in edit mode use color HydroViewConstants.WHITE else if station mode
     * use counter for color else if group mode get color from config file
     * 
     * @param TraceData
     *            The TraceData Object
     * @param traceIndex
     *            trace index
     * @param PaintEvent
     *            The paint event
     */
    private void setForegroundColor(TraceData td, int traceIndex, GC gc) {
        if (dialog.isInverseVideo()) {
            gc.setForeground(black);
        } else {
            if (traceSelected && (selectedTraceId == traceIndex)
                    && !dialog.isCancel()) { // if in edit mode
                gc.setForeground(parentComp.getDisplay().getSystemColor(
                        SWT.COLOR_WHITE));
            } else if (!groupMode) {
                currentTraceColor = new Color(parentComp.getDisplay(),
                        HydroUtils.getColor(traceIndex));
                gc.setForeground(currentTraceColor);
            } else if (groupMode) {
                if (td.getColorName() != null && HydroUtils.getColor(td.getColorName()) != null) {
                    currentTraceColor = new Color(parentComp.getDisplay(),
                            HydroUtils.getColor(td.getColorName()));
                } else {
                    currentTraceColor = new Color(parentComp.getDisplay(),
                            HydroUtils.getGroupModeColor(traceIndex));
                }
                gc.setForeground(currentTraceColor);
            }
        }
    }

    /**
     * if in edit mode use color HydroViewConstants.WHITE else if station mode
     * use counter for color else if group mode get color from config file
     * 
     * @param TraceData
     *            The TraceData Object
     * @param traceIndex
     *            trace index
     * @param PaintEvent
     *            The paint event
     */
    private void setBackgroundColor(TraceData td, int traceIndex, GC gc) {
        if (dialog.isInverseVideo()) {
            gc.setBackground(black);
        } else {
            if (traceSelected && (selectedTraceId == traceIndex)
                    && !dialog.isCancel()) { // if in edit mode
                gc.setBackground(parentComp.getDisplay().getSystemColor(
                        SWT.COLOR_WHITE));
            } else if (!groupMode) {
                gc.setBackground(new Color(parentComp.getDisplay(), HydroUtils
                        .getColor(traceIndex)));
            } else if (groupMode) {
                  if (td.getColorName() != null && HydroUtils.getColor(td.getColorName()) != null) {
                       gc.setBackground(new Color(parentComp.getDisplay(), HydroUtils
                            .getColor(td.getColorName())));
                  } else {
                       gc.setBackground(new Color(parentComp.getDisplay(),
                            HydroUtils.getGroupModeColor(traceIndex)));
            	}
            }
        }
    }

    /**
     * Get the forecast data.
     * 
     * @param traceData
     *            The TraceData object to fill with forecast data
     * @return The number of traces
     */
    private int getFcstData(TraceData traceData) {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();

        ArrayList<TraceData> traceDataList = new ArrayList<TraceData>();
        ArrayList<TimeSeriesPoint> points = new ArrayList<TimeSeriesPoint>();
        ArrayList<TimeSeriesPoint> pointsbak = new ArrayList<TimeSeriesPoint>();
        ArrayList<Fcstheight> results;

        lid = traceData.getLid();
        ts = traceData.getTs();
        pe = traceData.getPe();
        String name = traceData.getName();
        String dur = traceData.getDur();
        String extremum = traceData.getExtremum();

        int ntraces = 0;

        // Get data from database based on table name and trace info
        String tablename = DbUtils.getTableName(pe, ts);

        /* Get the data from IHFS and store in TimeSeriesPoint object */
        try {
            Date basisTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                    .getTime();
            Date prevBasisTime = Calendar.getInstance(
                    TimeZone.getTimeZone("GMT")).getTime();
            double ymin = 0;
            double ymax = 0;

            /*
             * convert times to to 19-character String time and build where
             * clause
             */
            String begin = HydroConstants.DATE_FORMAT.format(beginDate);
            String end = HydroConstants.DATE_FORMAT.format(endDate);

            String where = String.format(" WHERE lid = '%s' "
                    + " AND pe = '%s'  " + " AND ts = '%s'  "
                    + " AND dur = %d   " + " AND extremum = '%s' "
                    + " AND validtime >= '%s' " + " AND validtime <= '%s' "
                    + " ORDER BY ts, basistime DESC, validtime", traceData
                    .getLid().toUpperCase(), traceData.getPe().toUpperCase(),
                    traceData.getTs().toUpperCase(), Integer.parseInt(traceData
                            .getDur()), traceData.getExtremum().toUpperCase(),
                    begin, end);

            results = dataManager.getForecast(where, tablename);

            if ((results != null) && (results.size() > 0)) {
                int n = 0;
                for (Fcstheight row : results) {
                    TimeSeriesPoint p = new TimeSeriesPoint();
                    TimeSeriesPoint pbak = new TimeSeriesPoint();
                    Date validTime = row.getId().getValidtime();
                    p.setX(validTime);
                    pbak.setX(validTime);
                    Date productTime = row.getProducttime();
                    traceData.setProductTime(productTime);

                    if ((validTime.getTime() >= beginDate.getTime())
                            && (validTime.getTime() <= endDate.getTime())
                            && (row.getValue() != HydroConstants.MISSING_VALUE)) {

                        p.setY(row.getValue());
                        pbak.setY(row.getValue());
                        basisTime = row.getId().getBasistime();
                        if (n == 0) {
                            prevBasisTime = basisTime;
                            ymin = row.getValue();
                            ymax = row.getValue();
                        }

                        if (basisTime.getTime() != prevBasisTime.getTime()) {
                            if (ntraces < MAX_FCST_TRACES) {
                                traceData.setXmin(beginDate);
                                traceData.setXmax(endDate);
                                n = 0; /* Reset npts in new forecast trace */
                                traceData.setBasistime(prevBasisTime);
                                ntraces++;
                                traceData.setTsData(points
                                        .toArray(new TimeSeriesPoint[points
                                                .size()]));
                                points = new ArrayList<TimeSeriesPoint>();

                                if (ntraces >= 1) {
                                    traceDataList.add(traceData);
                                }

                                traceData = new TraceData();
                                traceData.setForecast(true);
                                traceData.setDur(dur);
                                traceData.setExtremum(extremum);
                                traceData.setLid(lid);
                                traceData.setPe(pe);
                                traceData.setTs(ts);
                                traceData.setName(name);
                                traceData.setBasistime(basisTime);
                                traceData.setProductTime(productTime);
                                traceData.setTraceOn(!this.latestFcstFlag);
                            } else {
                                /* reached max fcst traces, break out of loop */
                                break;
                            }
                        }

                        p.setY(row.getValue());
                        p.setOld_value(row.getValue().floatValue());
                        p.setX(validTime);
                        p.setMode('0');
                        p.setRevision(row.getRevision().shortValue());
                        p.setQuality_code(row.getQualityCode());
                        p.setProbability(row.getId().getProbability());
                        p.setValidtime(row.getId().getValidtime());

                        pbak.setY(row.getValue());
                        pbak.setOld_value(row.getValue().floatValue());
                        pbak.setX(validTime);
                        pbak.setMode('0');
                        pbak.setRevision(row.getRevision().shortValue());
                        pbak.setQuality_code(row.getQualityCode());
                        pbak.setProbability(row.getId().getProbability());
                        pbak.setValidtime(row.getId().getValidtime());

                        if ((row.getValue() != HydroConstants.MISSING_VALUE)) {
                            pbak.setY(row.getValue());
                            points.add(p);
                            pointsbak.add(pbak);
                        }

                        if (row.getValue() < ymin) {
                            ymin = row.getValue();
                        }

                        if (row.getValue() > ymax) {
                            ymax = row.getValue();
                        }

                        n++;
                        prevBasisTime = basisTime;
                        traceData.setNpts(n);
                        traceData.setValue_ymin(ymin);
                        traceData.setValue_ymax(ymax);
                        traceData.setYmin(ymin);
                        traceData.setYmax(ymax);
                    }
                }

                /*
                 * Copy last trace into forecast trace
                 */
                if (ntraces < MAX_FCST_TRACES) {
                    traceData.setBasistime(prevBasisTime);
                    traceDataList.add(traceData);
                    ntraces++;
                }
                noFcstDataAvailable = false;
            } else {
                noFcstDataAvailable = true;
                traceDataList.add(traceData);//although nothing from DB
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

        traceData.setTsData(points.toArray(new TimeSeriesPoint[points.size()]));
        traceData.setPreviousTsData(pointsbak
                .toArray(new TimeSeriesPoint[pointsbak.size()]));

        for (TraceData td : traceDataList) {
            graphData.addTrace(td);
        }

        return traceDataList.size();
    }

    /**
     * Callback for the Set PC as PP menus.
     */
    protected void setPcAsPP() {
        if (dialog.isPcasPPOff()) {
            graphData.setDerivepp(HydroConstants.OFF);
        } else if (dialog.isPcasPPInterpolate()) {
            graphData.setDerivepp(HydroConstants.INTERPOLATE);
        } else if (dialog.isPcasPPAssign()) {
            graphData.setDerivepp(HydroConstants.ASSIGN);
        }

        /* Check to see if 'off' has been selected. */
        if (dialog.isPcasPPOff()) {
            graphData.setShowpp(false);
        } else {
            graphData.setShowpp(true);
        }
        redraw();
    }

    /**
     * Display the flood lines or not.
     * 
     * @return true if displaying flood lines
     */
    private boolean isFloodLineDisplay() {
        return ((dialog.getBatchDataAndCategoriesMI().getSelection() || dialog
                .getBatchDataOnlyShowCatMI().getSelection()) || (dialog
                .getScaleStagesDataOnlyShowCategoreMI().getSelection() || dialog
                .getScaleStagesDataAndCategoriesMI().getSelection()));
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @param horizontalSpan
     *            the horizontalSpan to set
     */
    public void setHorizontalSpan(int horizontalSpan) {
        this.horizontalSpan = horizontalSpan;
    }

    /**
     * @param verticalSpan
     *            the verticalSpan to set
     */
    public void setVerticalSpan(int verticalSpan) {
        this.verticalSpan = verticalSpan;
    }

    /**
     * Set grid lines on/off
     * 
     * @param gridLines
     *            boolean to display gridlines or not
     */
    public void showGridLines(boolean display) {
        displayGridLines = display;
        redraw();
    }

    /**
     * Get the Trace List
     * 
     * @return The trace list
     */
    private ArrayList<TraceData> getTraceList() {
        return traceArray;
    }

    /**
     * Get the drawing canvas
     * 
     * @return the Canvas
     */
    public Canvas getCanvas() {
        return tsCanvas;
    }

    /**
     * Set whether or not to get the data again
     * 
     * @param getAgain
     */
    public void setGetAgain(boolean getAgain) {
        this.getAgain = getAgain;
    }

    @Override
    public void aboutToRun(IJobChangeEvent event) {
    }

    @Override
    public void awake(IJobChangeEvent event) {
    }

    @Override
    public void done(IJobChangeEvent event) {
        /* Verify The Job Type */
        TimeSeriesDataJobManager.REQUEST_TYPE requestType = (TimeSeriesDataJobManager.REQUEST_TYPE) event
                .getJob().getProperty(new QualifiedName(null, "REQUEST_TYPE"));
        if (requestType != TimeSeriesDataJobManager.REQUEST_TYPE.REQUEST_TYPE_GRAPH) {
            return;
        }

        tsDataJobManager.removeJobChangeListener(this);
        Display.getDefault().syncExec(new Runnable() {
            @Override
            public void run() {
                try {
                    redraw();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
    }

    @Override
    public void running(IJobChangeEvent event) {
    }

    @Override
    public void scheduled(IJobChangeEvent event) {
    }

    @Override
    public void sleeping(IJobChangeEvent event) {
    }

    /**
     * @return the traceArray
     */
    public ArrayList<TraceData> getTraceArray() {
        return traceArray;
    }
    
    public boolean isZoomed() {
		return zoomed;
	}

	public void setZoomed(boolean zoomed) {
		this.zoomed = zoomed;
	}

}
