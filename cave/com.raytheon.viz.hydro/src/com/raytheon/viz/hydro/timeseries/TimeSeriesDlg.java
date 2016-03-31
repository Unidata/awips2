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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.hydro.CaveHydroSWTDialog;
import com.raytheon.viz.hydro.resource.MultiPointResource;
import com.raytheon.viz.hydro.timeseries.table.SiteInfo;
import com.raytheon.viz.hydro.timeseries.table.TabInfo;
import com.raytheon.viz.hydro.timeseries.util.GraphData;
import com.raytheon.viz.hydro.timeseries.util.GroupInfo;
import com.raytheon.viz.hydro.timeseries.util.LIDData;
import com.raytheon.viz.hydro.timeseries.util.PageInfo;
import com.raytheon.viz.hydro.timeseries.util.PreferredOrderManager;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesUtil;
import com.raytheon.viz.hydro.timeseries.util.TraceData;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDataCache;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.util.StnClassSyncUtil;
import com.raytheon.viz.ui.widgets.DateTimeSpinner;

/**
 * This class displays the Time Series dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation. 
 * 09 JUN 2008  1194       M. Duff     Implement the Dialog.
 * 21 Oct 2008  1520       M. Duff     Implement the Tabular Portion.
 * 06 NOV 2009  2639       M. Duff     Bug fixes.
 * 15 JUN 2010  5828       M. Duff     Added additional error checking for
 *                                     loading of group_definition.cfg.
 * 02 Jul 2010  5280       lbousaidi   added Page number to the display
 * 09 Sep 2010  5399       lbousaidi   Added a new constructor and code to 
 * 									   highlight PE TS in bottomListData 
 * 									   related to selected Lid
 * 14 sep 2010  5282	   lbousaidi   set default selections and re-uses 
 * 									   the open Time Series Control window
 * 24 JAN 2011  7797       bkowal      updates so that Hydro Time Series can
 *                                     be ran as a standalone application
 *                                     without starting CAVE.
 * 01 June 2011 9499       djingtao    update openGraph() 
 * 23 Jul 2012 15180       mpduff      Auto select the first group in the predefined group list
 * 23 Jul 2012 15195       mpduff      Fix Group graphing to use the date widgets.  
 * 08 Aug 2012   570       mpduff      Fix a Ctrl-F in Station list causing IndexOutOfBounds error.  
 * 08 Aug 2012   657       mpduff      Fix error when selecting a TS while no selection has been made 
 *                                     in the Station List.
 * 27 Sep 2012 15302       wkwock      TimeSeries start mode should depends on token timeseries_mode
 *                                     despite start up in CAVE or standalone.
 * 30 Jan 2013 15264       wkwock      Fix the missing group_definition.cfg file crash           
 * 05 Feb 2013 1578        rferrel     Dialog made non-blocking and a singleton.
 * 06 May 2013  1976       mpduff      Code cleanup.
 * 06 Jun 2013 2076        mpduff      Fix station list selection and graph button enabling.
 * 0  Jun 2013 15980       wkwock      Fix selected station not update
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * Oct 13, 2015 4933       rferrel     Log error if unable to find group definition file
 *                                      Fixed formatter resource leaks.
 * 30 Oct, 2015 15102      wkwock      Implements preferred order for PE-D-TS-EXT list
 * Jan 26, 2016 5054       randerso    Allow dialog to be parented to display
 * Mar 17, 2016  5483      randerso    Major GUI cleanup
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TimeSeriesDlg extends CaveHydroSWTDialog {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TimeSeriesDlg.class);

    /* Constants */
    private static final String PREDEFINED_GROUP = "Predefined Group";

    private static final String STATION_SELECTION = "Station Selection";

    private static final String GRAPH = "Graph";

    private static final String PAGE = "Page";

    private static final String GROUP = "Group";

    private static final String TRACE = "Trace";

    private static final String NAME = "name";

    private static final String DESCRIPT = "descript";

    private static final String GRID = "grid";

    private static final String TRACEMODE = "tracemode";

    private static final String PASTHOURS = "pasthours";

    private static final String FUTUREHOURS = "futurehours";

    private static final String POS = "pos";

    private static final String XSIZE = "xsize";

    private static final String YSIZE = "ysize";

    private static final String YLINEAR = "ylinear";

    private static final String YSCALE = "yscale";

    private static final String SHOWCAT = "showcat";

    private static final String DERIVEPP = "derivepp";

    private static final String SHOWPP = "showpp";

    private static final String LATESTFCSTONLY = "latestfcstonly";

    private static final String STN = "stn";

    private static final String COLOR = "color";

    private static TimeSeriesDlg instance;

    private final String[] TS_ORDER = { "R", "F", "P", "M", "C" };

    /**
     * The TimeSeries Display Dialog.
     */
    private TimeSeriesDisplayDlg timeSeriesDisplayDlg = null;

    private DateTimeSpinner beginningTimeControl;

    private DateTimeSpinner endingTimeControl;

    /**
     * Mode combo box.
     */
    private Combo modeCbo;

    /**
     * River check box.
     */
    private Button riverChk;

    /**
     * Precipitation check box.
     */
    private Button precipChk;

    /**
     * Temperature check box.
     */
    private Button tempChk;

    /**
     * Snow check box.
     */
    private Button snowChk;

    /**
     * Other check box.
     */
    private Button otherChk;

    /**
     * Check all check boxes button.
     */
    private Button checkAllBtn;

    /**
     * Search text control.
     */
    private Text searchTF;

    /**
     * ID radio button.
     */
    private Button idRdo;

    /**
     * Name radio button.
     */
    private Button nameRdo;

    /**
     * Type Source order combo box.
     */
    private Combo tsOrderCbo;

    /**
     * Font used for the list controls.
     */
    private Font font;

    /**
     * Top data list box.
     */
    private final java.util.List<Pair<String, Integer>> topDataTableLabels = Arrays
            .asList(new Pair<>("ID", SWT.LEFT), new Pair<>("Name", SWT.LEFT));

    private Table topDataTable;

    /**
     * Bottom data list box.
     */

    private final java.util.List<Pair<String, Integer>> bottomDataTableLabels = Arrays
            .asList(new Pair<>("PE", SWT.LEFT), new Pair<>("TypSrc", SWT.LEFT),
                    new Pair<>("Ext", SWT.LEFT), new Pair<>("Dur", SWT.LEFT),
                    new Pair<>("     ", SWT.LEFT));

    private Table bottomDataTable;

    /**
     * List of Group Data.
     */
    private List groupDataList;

    /**
     * Beginning time calendar.
     */
    private Calendar beginCal;

    /**
     * Ending time calendar.
     */
    private Calendar endCal;

    /**
     * The graph button.
     */
    private Button graphButton = null;

    /**
     * The table button.
     */
    private Button tableButton = null;

    /**
     * The both button.
     */
    private Button bothButton = null;

    /**
     * The Stack Composite.
     */
    private Composite stackComp;

    /**
     * The Stack Layout.
     */
    private final StackLayout stackLayout = new StackLayout();

    /**
     * Set of Location IDs in the upperDataList
     */
    private ArrayList<String> lidList;

    /**
     * Filtered lid list
     */
    private ArrayList<String> filteredLidList;

    /**
     * Tree Map holding the Location Id and Name in sorted order.
     */
    private Map<String, String> stationData;

    /**
     * Map holding the location id and display class.
     */
    private Map<String, String> stnDisplayMap = null;

    /**
     * The Station Group for the stack layout.
     */
    private Group stnGroup;

    /**
     * The Group Group for the stack layout.
     */
    private Group groupGroup;

    /**
     * Keeps track of the station layout being displayed.initializecomponents
     */
    private boolean stnLayoutDisplayed = true;

    /**
     * Previous LID data.
     */
    private LIDData prevLidData = new LIDData();

    /**
     * Current LID data.
     */
    private LIDData currLidData = new LIDData();

    /** List of GroupInfo objects */
    private final ArrayList<GroupInfo> groupList = new ArrayList<GroupInfo>();

    /** Holds the last graphed GroupInfo object */
    private GroupInfo prevGroupInfo;

    private String groupConfigFilePath = null;

    private final ArrayList<SiteInfo> siteInfoList = new ArrayList<SiteInfo>();

    private final ArrayList<TabInfo> tabInfoList = new ArrayList<TabInfo>();

    private final HydroDisplayManager displayManager;

    private boolean openTimeSeriesDisplays = false;

    private GageData gageData = null;

    /**
     * Auto open the graph or tabular display. True opens graph, false opens
     * tabular display.
     */
    private boolean displayGraph = false;

    private TabularTimeSeriesDlg tabularDlg;

    /**
     * Used to indicate if the Hydro TimeSeries application is being ran in
     * standalone mode - outside of CAVE.
     */
    private boolean standaloneMode = false;

    /**
     * Mode ComboBox previous selection.
     */
    private int prevModeIdx;

    private String startMode;

    /**
     * When not in stand alone mode this allows only a single instance of the
     * dialog.
     * 
     * @return instance
     */
    public final static TimeSeriesDlg getInstance() {
        // Independent shell must be recreated after closing.
        if (instance == null || !instance.isOpen()) {
            instance = new TimeSeriesDlg(Display.getCurrent());
        }
        return instance;
    }

    /**
     * Constructor.
     * 
     * @param display
     */
    private TimeSeriesDlg(Display display) {
        super(display, SWT.RESIZE, CAVE.INDEPENDENT_SHELL);
        setText("Time Series Control");

        displayManager = HydroDisplayManager.getInstance();
        if (displayManager.getCurrentLid() != null) {
            currentLid = displayManager.getCurrentLid();
        }
    }

    /**
     * Constructor for stand alone dialog.
     * 
     * @param display
     * 
     * @param groupConfigFile
     *            the user-specified file with group configuration information.
     */
    public TimeSeriesDlg(Display display, File groupConfigFile) {
        this(display);

        this.standaloneMode = true;
        // Ensure That The Group Configuration File Exists.
        if (groupConfigFile == null || !groupConfigFile.exists()) {
            // if it does not, check localization for the file
            IPathManager pm = PathManagerFactory.getPathManager();
            groupConfigFile = pm.getStaticFile(HydroConstants.GROUP_DEFINITION);

            if (groupConfigFile == null || !groupConfigFile.exists()) {
                String name = HydroConstants.GROUP_DEFINITION;
                if (name.startsWith("/")) {
                    name = name.substring(1);
                }
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to locate group configuration file - " + name);
                this.groupConfigFilePath = null;
            } else {
                this.groupConfigFilePath = groupConfigFile.getAbsolutePath();
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Using standard AWIPS 2 group_definition.cfg file.  "
                                        + "Unable to locate specified group configuration file.");
            }
        } else {
            this.groupConfigFilePath = groupConfigFile.getAbsolutePath();
        }
    }

    /**
     * Used by the updateAndOpen methods to proper update the dialogs.
     */
    private void updateOpen() {
        if (!isOpen()) {
            open();
        } else {
            try {
                populateStationList();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            setCurrentData();
            opened();
            bringToTop();
        }
    }

    /**
     * Update dialog and make sure it is open.
     * 
     * @param lid
     * @param displayGraph
     */
    public void updateAndOpen(String lid, boolean displayGraph) {
        this.currentLid = lid;
        this.displayGraph = displayGraph;
        openTimeSeriesDisplays = true;
        updateOpen();
    }

    /**
     * Update dialog and make sure it is open.
     * 
     * @param lid
     * @param PE
     * @param TS
     * @param displayGraph
     */
    public void updateAndOpen(String lid, String PE, String TS,
            boolean displayGraph) {
        this.currentLid = lid;
        this.currentPe = PE;
        this.currentTs = TS;
        this.displayGraph = displayGraph;
        openTimeSeriesDisplays = true;
        updateOpen();
    }

    /**
     * Update dialog and make sure it is open.
     * 
     * @param gageData
     * @param displayGraph
     * 
     * @param lid
     */
    public void updateAndOpen(GageData gageData, boolean displayGraph) {
        currentLid = gageData.getLid();
        this.displayGraph = displayGraph;
        openTimeSeriesDisplays = true;
        this.gageData = gageData;
        updateOpen();
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    /**
     * Allow others to give focus to the dialog.
     */
    public void setFocus() {
        shell.setFocus();
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        // Initialize all of the controls
        initializeComponents();
        setCurrentData();
    }

    @Override
    protected void opened() {
        if (openTimeSeriesDisplays) {
            if (displayGraph) {
                openGraph();
            }
            openTimeSeriesDisplays = false;
        }

        AbstractVizResource<?, ?> rsc = HydroDisplayManager.getInstance()
                .getDisplayedResource();
        if (rsc instanceof MultiPointResource) {
            ((MultiPointResource) rsc).setTs(this);
        }

        checkBottomButtons();
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        // Get the begin and end values from Apps_defaults
        AppsDefaults appsDef = AppsDefaults.getInstance();
        int beginDays = Integer.parseInt(appsDef.getToken(
                "timeseries_begintime", "30"));
        int endDays = Integer.parseInt(appsDef.getToken("timeseries_endtime",
                "5"));
        startMode = appsDef.getToken("timeseries_mode", "STATION");
        Date d = SimulatedTime.getSystemTime().getTime();
        beginCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        beginCal.setTime(d);
        beginCal.add(Calendar.DAY_OF_MONTH, beginDays * -1);
        beginCal.set(Calendar.MINUTE, 0);
        beginCal.set(Calendar.SECOND, 0);
        beginCal.set(Calendar.MILLISECOND, 0);

        endCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        endCal.setTime(d);
        endCal.add(Calendar.DAY_OF_MONTH, endDays);
        endCal.set(Calendar.MINUTE, 0);
        endCal.set(Calendar.SECOND, 0);
        endCal.set(Calendar.MILLISECOND, 0);

        createTimeControls();

        createStationControls();
        createGroupControl();
        createBottomButtons();

        /* Add a complete list of stations to the station list */
        try {
            populateStationList();
        } catch (VizException e) {
            statusHandler.error("Failed to populate station list", e);
        }

        if (startMode.equals("GROUP") && displayGraph == false) {
            modeCbo.select(0);
            stackLayout.topControl = groupGroup;
            stackComp.layout();
            stnLayoutDisplayed = false;
            populateGroupList();
            prevModeIdx = 0;
        }
    }

    /**
     * Create the station controls for the Stack Layout.
     */
    private void createStationControls() {
        stackComp = new Composite(shell, SWT.NONE);
        stackComp.setLayout(stackLayout);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        stackComp.setLayoutData(gd);

        stnGroup = new Group(stackComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        stnGroup.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        stnGroup.setLayoutData(gd);

        stackLayout.topControl = stnGroup;

        createCheckBoxControls();

        /* Add a separator */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label sepLbl = new Label(stnGroup, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createSearchControls();
        SashForm sashForm = new SashForm(stnGroup, SWT.VERTICAL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        sashForm.setLayoutData(gd);
        sashForm.SASH_WIDTH = 6;

        createTopTableControl(sashForm);
        createBottomDataTable(sashForm);

        sashForm.setWeights(new int[] { 50, 50 });
    }

    /**
     * Create the group controls for the Stack Layout.
     */
    private void createGroupControl() {
        groupGroup = new Group(stackComp, SWT.NONE);
        groupGroup.setLayout(new GridLayout());

        groupDataList = new List(groupGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL);

        GC gc = new GC(groupDataList);
        int textWidth = gc.getFontMetrics().getAverageCharWidth() * 30;
        gc.dispose();

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = textWidth;
        gd.heightHint = groupDataList.getItemHeight() * 27;
        groupDataList.setLayoutData(gd);
    }

    /**
     * Create the beginning and ending time controls.
     */
    private void createTimeControls() {
        // --------------------------------------
        // Create beginning time controls
        // --------------------------------------
        Composite timeComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        timeComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        Label beginningTimeLbl = new Label(timeComp, SWT.NONE);
        beginningTimeLbl.setText("Beginning Time Z:");

        beginningTimeControl = new DateTimeSpinner(timeComp, beginCal, 4);
        beginningTimeControl.setLayoutData(new GridData());

        // --------------------------------------
        // Create ending time controls
        // --------------------------------------
        Label endingTimeLbl = new Label(timeComp, SWT.NONE);
        endingTimeLbl.setText("Ending Time Z:");

        endingTimeControl = new DateTimeSpinner(timeComp, endCal, 4);
        endingTimeControl.setLayoutData(new GridData());

        // --------------------------------------
        // Create Mode controls
        // --------------------------------------
        Composite modeComp = new Composite(timeComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        modeComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        modeComp.setLayoutData(gd);

        Label modeLbl = new Label(modeComp, SWT.NONE);
        modeLbl.setText("Mode: ");
        gd = new GridData(SWT.LEFT, SWT.CENTER, true, false);
        modeLbl.setLayoutData(gd);

        modeCbo = new Combo(modeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        modeCbo.add(PREDEFINED_GROUP);
        modeCbo.add(STATION_SELECTION);
        modeCbo.select(1);
        prevModeIdx = 1;

        modeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (prevModeIdx != modeCbo.getSelectionIndex()) {
                    if (stnLayoutDisplayed) {
                        stackLayout.topControl = groupGroup;
                        stackComp.layout();
                        stnLayoutDisplayed = false;
                        populateGroupList();
                    } else {
                        stackLayout.topControl = stnGroup;
                        stackComp.layout();
                        stnLayoutDisplayed = true;
                    }
                    prevModeIdx = modeCbo.getSelectionIndex();
                    checkBottomButtons();
                }
            }
        });
    }

    /**
     * Create the check box controls.
     */
    private void createCheckBoxControls() {
        Composite checkComp = new Composite(stnGroup, SWT.NONE);
        GridLayout checkGl = new GridLayout(3, true);
        checkComp.setLayout(checkGl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        checkComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        riverChk = new Button(checkComp, SWT.CHECK);
        riverChk.setText("River");
        riverChk.setLayoutData(gd);
        riverChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                filterList();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        precipChk = new Button(checkComp, SWT.CHECK);
        precipChk.setText("Precip");
        precipChk.setLayoutData(gd);
        precipChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                filterList();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        tempChk = new Button(checkComp, SWT.CHECK);
        tempChk.setText("Temp");
        tempChk.setLayoutData(gd);
        tempChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                filterList();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        snowChk = new Button(checkComp, SWT.CHECK);
        snowChk.setText("Snow");
        snowChk.setLayoutData(gd);
        snowChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                filterList();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        otherChk = new Button(checkComp, SWT.CHECK);
        otherChk.setText("Other");
        otherChk.setLayoutData(gd);
        otherChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                filterList();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        checkAllBtn = new Button(checkComp, SWT.PUSH);
        checkAllBtn.setText("All");
        checkAllBtn.setLayoutData(gd);
        checkAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                riverChk.setSelection(true);
                precipChk.setSelection(true);
                tempChk.setSelection(true);
                snowChk.setSelection(true);
                otherChk.setSelection(true);
                filterList();
            }
        });
    }

    /**
     * Create the search controls.
     */
    private void createSearchControls() {
        Composite searchComp = new Composite(stnGroup, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        searchComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        searchComp.setLayoutData(gd);

        Composite leftComp = new Composite(searchComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        leftComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        leftComp.setLayoutData(gd);

        Label searchLbl = new Label(leftComp, SWT.NORMAL);
        searchLbl.setText("Search:");

        searchTF = new Text(leftComp, SWT.BORDER);
        searchTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent event) {
                String search = searchTF.getText();
                if (!search.equals("") && !search.equals(" ")) {
                    /* Iterate over the location Ids in the list */
                    ListIterator<String> iter = lidList.listIterator();
                    while (iter.hasNext()) {
                        if (idRdo.getSelection()) {
                            if (iter.next().startsWith(
                                    searchTF.getText().toUpperCase())) {
                                topDataTable.setSelection(iter.previousIndex());
                                break;
                            }
                        } else {
                            String lid = iter.next();
                            if (stationData
                                    .get(lid)
                                    .toUpperCase()
                                    .startsWith(
                                            searchTF.getText().toUpperCase())) {
                                topDataTable.setSelection(iter.previousIndex());
                                break;
                            }
                        }
                    }
                }

                processTopDataListSelection();
            }
        });

        Label tsOrderLbl = new Label(leftComp, SWT.NORMAL);
        tsOrderLbl.setText("TS Order:");

        tsOrderCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        tsOrderCbo.add(" ");
        tsOrderCbo.add("RG");
        tsOrderCbo.add("RP");
        tsOrderCbo.add("RZ");
        tsOrderCbo.add("FF");
        tsOrderCbo.add("FX");
        tsOrderCbo.add("FZ");
        tsOrderCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (topDataTable.getSelectionIndex() != -1) {
                    TableItem item = topDataTable.getItem(topDataTable
                            .getSelectionIndex());
                    String selectedLid = item.getText(0);
                    populateBottomList(selectedLid,
                            tsOrderCbo.getSelectionIndex());
                }
            }
        });
        tsOrderCbo.select(1);

        Composite rightComp = new Composite(searchComp, SWT.NONE);
        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        rightComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        rightComp.setLayoutData(gd);

        idRdo = new Button(rightComp, SWT.RADIO);
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        idRdo.setLayoutData(gd);
        idRdo.setText("ID");
        idRdo.setSelection(true);
        idRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setCursorBusy(true);
                try {
                    populateStationList();
                    checkBottomButtons();
                } catch (VizException e) {
                    statusHandler.error("Failed to populate station list", e);
                }
                setCursorBusy(false);
            }
        });

        nameRdo = new Button(rightComp, SWT.RADIO);
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        nameRdo.setLayoutData(gd);
        nameRdo.setText("Name");
        nameRdo.setSelection(false);
        nameRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setCursorBusy(true);
                try {
                    populateStationList();
                    checkBottomButtons();
                } catch (VizException e) {
                    statusHandler.error("Failed to populate station list", e);
                }
                setCursorBusy(false);
            }
        });
    }

    /**
     * Clear or set the shell's cursor to the current wait cursor.
     * 
     * @param state
     *            - true set to busy cursor
     */
    private void setCursorBusy(boolean state) {
        Cursor waitCursor = null;
        if (state) {
            waitCursor = getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
        }
        shell.setCursor(waitCursor);
    }

    /**
     * Create the top list control.
     */
    private void createTopTableControl(Composite parent) {
        Composite comp = new Composite(parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        comp.setLayout(gl);

        topDataTable = new Table(comp, SWT.BORDER | SWT.MULTI
                | SWT.FULL_SELECTION);
        topDataTable.setHeaderVisible(true);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GC gc = new GC(topDataTable);
        int textWidth = gc.getFontMetrics().getAverageCharWidth() * 30;
        gc.dispose();

        gd.widthHint = textWidth;
        gd.heightHint = topDataTable.getItemHeight() * 9;
        topDataTable.setLayoutData(gd);

        for (Pair<String, Integer> pair : topDataTableLabels) {
            TableColumn column = new TableColumn(topDataTable, pair.getSecond());
            column.setText(pair.getFirst());
            column.pack();
        }

        topDataTable.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                processTopDataListSelection();
            }
        });
    }

    protected void processTopDataListSelection() {
        if (topDataTable.getSelectionIndex() != -1) {
            populateBottomList(lidList.get(topDataTable.getSelectionIndex()),
                    tsOrderCbo.getSelectionIndex());
        }
        checkBottomButtons();
    }

    /**
     * Create the bottom list control.
     */
    private void createBottomDataTable(Composite parent) {
        Composite comp = new Composite(parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        comp.setLayout(gl);

        bottomDataTable = new Table(comp, SWT.BORDER | SWT.MULTI
                | SWT.FULL_SELECTION);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GC gc = new GC(bottomDataTable);
        int textWidth = gc.getFontMetrics().getAverageCharWidth() * 40;
        gc.dispose();

        gd.widthHint = textWidth;
        gd.heightHint = bottomDataTable.getItemHeight() * 9;
        bottomDataTable.setLayoutData(gd);
        bottomDataTable.setHeaderVisible(true);

        for (Pair<String, Integer> pair : bottomDataTableLabels) {
            TableColumn column = new TableColumn(bottomDataTable,
                    pair.getSecond());
            column.setText(pair.getFirst());
            column.pack();
        }

        bottomDataTable.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event e) {
                correctSelections();
            }
        });
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData mainGD = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout buttonGl = new GridLayout(4, false);
        buttonComp.setLayoutData(mainGD);
        buttonComp.setLayout(buttonGl);

        int buttonWidth = 63;

        GridData gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button graphBtn = new Button(buttonComp, SWT.PUSH);
        graphBtn.setText("Graph");
        graphBtn.setLayoutData(gd);
        this.graphButton = graphBtn;
        graphBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                graphButton.setEnabled(false);
                bothButton.setEnabled(false);
                openGraph();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button tableBtn = new Button(buttonComp, SWT.PUSH);
        tableBtn.setText("Table");
        tableBtn.setLayoutData(gd);
        this.tableButton = tableBtn;
        tableBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tableButton.setEnabled(false);
                bothButton.setEnabled(false);
                openTabularDisplay();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button bothBtn = new Button(buttonComp, SWT.PUSH);
        bothBtn.setText("Both");
        bothBtn.setLayoutData(gd);
        this.bothButton = bothBtn;
        bothBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                bothAction();
            }
        });

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Populates the station list box.
     */
    private void populateStationList() throws VizException {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        lidList = new ArrayList<String>();

        /* Check the locview and stnclass table counts */
        if (!dataManager.checkLidCount()) {
            long locCount = dataManager.recordCount("locview", "");
            long stnClassCount = dataManager.recordCount("stnClass", "");
            StringBuilder errMessage = new StringBuilder();
            errMessage.append("Error: Number of records in LocView " + locCount
                    + " ");
            errMessage.append("does not equal number of records in StnClass "
                    + stnClassCount + ". ");
            errMessage
                    .append(""
                            + "Click 'OK' to run the set station class program to correct this problem.\n\n");
            errMessage.append("This may take several minutes to execute.");
            boolean response = MessageDialog.openConfirm(shell,
                    "Tables Out of Sync", errMessage.toString());

            if (response) {
                StnClassSyncUtil.setStnClassAll();
            } else {
                throw new VizException(errMessage.toString());
            }
        }

        stationData = dataManager.getTimeSeriesStationData(nameRdo
                .getSelection());
        stnDisplayMap = dataManager.getStationDisplayMap();

        Set<String> lids = stationData.keySet();

        topDataTable.removeAll();

        Iterator<String> iter = lids.iterator();
        String lid = null;
        while (iter.hasNext()) {
            lid = iter.next();
            TableItem item = new TableItem(topDataTable, SWT.NONE);
            item.setText(0, lid);
            item.setText(1, stationData.get(lid));
            lidList.add(lid);
        }
        for (TableColumn column : topDataTable.getColumns()) {
            column.pack();
        }

        if (currentLid != null) {
            ListIterator<String> iter2 = lidList.listIterator();
            while (iter2.hasNext()) {
                if (idRdo.getSelection()) {
                    if (iter2.next().equalsIgnoreCase(currentLid)) {
                        // Bug with show selection requires the select(0) first.
                        topDataTable.select(0);
                        topDataTable.select(iter2.previousIndex());
                        topDataTable.showSelection();

                        /* set to null so we don't enter this block again */
                        populateBottomList(currentLid,
                                tsOrderCbo.getSelectionIndex());
                        currentLid = null;
                        break;
                    }
                }

            }
        }
    }

    /**
     * Update the enable status of the bottom buttons.
     */
    private void checkBottomButtons() {
        boolean enabled = false;
        if (modeCbo.getText().equals(PREDEFINED_GROUP)) {
            enabled = true;
        } else {
            enabled = topDataTable.getSelectionCount() > 0;
        }

        graphButton.setEnabled(enabled);
        tableButton.setEnabled(enabled);
        bothButton.setEnabled(enabled);
    }

    /**
     * Populate the details list
     */
    private void populateBottomList(String selectedLid, int tsSelection) {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        siteInfoList.clear();
        try {
            /* Returns data ordered by pe asc,dur asc,ts asc,extremum asc. */
            ArrayList<Object[]> data = (ArrayList<Object[]>) dataManager
                    .getSitePEData(selectedLid);

            PreferredOrderManager pom = PreferredOrderManager.getInstance();
            Map<String, String[]> peMap = pom.getPreferedOrder(selectedLid);

            bottomDataTable.removeAll();

            /* Get the lists of PE data */
            LinkedHashMap<String, ArrayList<SiteInfo>> preferredMap = new LinkedHashMap<String, ArrayList<SiteInfo>>();
            LinkedHashMap<String, ArrayList<SiteInfo>> hMap = new LinkedHashMap<String, ArrayList<SiteInfo>>();
            LinkedHashMap<String, ArrayList<SiteInfo>> qMap = new LinkedHashMap<String, ArrayList<SiteInfo>>();
            LinkedHashMap<String, ArrayList<SiteInfo>> pMap = new LinkedHashMap<String, ArrayList<SiteInfo>>();
            LinkedHashMap<String, ArrayList<SiteInfo>> sMap = new LinkedHashMap<String, ArrayList<SiteInfo>>();
            LinkedHashMap<String, ArrayList<SiteInfo>> tMap = new LinkedHashMap<String, ArrayList<SiteInfo>>();
            LinkedHashMap<String, ArrayList<SiteInfo>> otherMap = new LinkedHashMap<String, ArrayList<SiteInfo>>();

            String prevPE = "";
            for (int i = 0; i < data.size(); i++) {

                SiteInfo si = new SiteInfo();
                Object[] row = data.get(i);
                si.setLid(selectedLid);
                si.setPe((String) row[1]);
                si.setTs((String) row[2]);
                si.setExt((String) row[3]);
                si.setDur(((Number) row[4]).intValue());

                boolean preferredLstFlg = false;
                if (peMap != null) {
                    String[] typeSrcLst = peMap.get(si.getPe());

                    if (typeSrcLst != null) {
                        for (String typesrc : typeSrcLst) {

                            if (typesrc.equalsIgnoreCase(si.getTs())) {
                                preferredLstFlg = true;
                                break;
                            }
                        }
                    } else if (peMap.containsKey(si.getPe())) {
                        preferredLstFlg = true;
                    }
                }
                if (preferredLstFlg) {
                    if (!si.getPe().equals(prevPE)) {
                        preferredMap.put(si.getPe(), new ArrayList<SiteInfo>());
                        prevPE = si.getPe();
                    }

                    preferredMap.get(si.getPe()).add(si);
                } else if (si.getPe().startsWith("H")) {
                    if (!si.getPe().equals(prevPE)) {
                        hMap.put(si.getPe(), new ArrayList<SiteInfo>());
                        prevPE = si.getPe();
                    }

                    hMap.get(si.getPe()).add(si);
                } else if (si.getPe().startsWith("Q")) {
                    if (!si.getPe().equals(prevPE)) {
                        qMap.put(si.getPe(), new ArrayList<SiteInfo>());
                        prevPE = si.getPe();
                    }

                    qMap.get(si.getPe()).add(si);
                } else if (si.getPe().startsWith("P")) {
                    if (!si.getPe().equals(prevPE)) {
                        pMap.put(si.getPe(), new ArrayList<SiteInfo>());
                        prevPE = si.getPe();
                    }

                    pMap.get(si.getPe()).add(si);
                } else if (si.getPe().startsWith("S")) {
                    if (!si.getPe().equals(prevPE)) {
                        sMap.put(si.getPe(), new ArrayList<SiteInfo>());
                        prevPE = si.getPe();
                    }

                    sMap.get(si.getPe()).add(si);
                } else if (si.getPe().startsWith("T")) {
                    if (!si.getPe().equals(prevPE)) {
                        tMap.put(si.getPe(), new ArrayList<SiteInfo>());
                        prevPE = si.getPe();
                    }

                    tMap.get(si.getPe()).add(si);
                } else {
                    if (!si.getPe().equals(prevPE)) {
                        otherMap.put(si.getPe(), new ArrayList<SiteInfo>());
                        prevPE = si.getPe();
                    }

                    otherMap.get(si.getPe()).add(si);
                }
            }

            /*
             * <pre> Need to reorder the data by PE in this order: H, Q, P, S,
             * T, then everything else
             * 
             * TS sort in this order: RG, RP, RZ, FF, FX, FZ. If a TS is
             * selected to appear first then the rest will sort in order after
             * that one. </pre>
             */
            boolean tsSelected = true;
            if (tsSelection < 1) {
                tsSelected = false;
            }

            populatePreferredDataInOrder(preferredMap, peMap);
            processDataList(hMap, tsSelected);
            processDataList(qMap, tsSelected);
            processDataList(pMap, tsSelected);
            processDataList(sMap, tsSelected);
            processDataList(tMap, tsSelected);
            processDataList(otherMap, tsSelected);

            bottomDataTable.getColumn(4).setText(selectedLid);
            bottomDataTable.setSelection(0);
        } catch (VizException e) {
            statusHandler.error("Failed to populate time series list", e);
        }
    }

    /**
     * populate data to bottomDataList base on preferred predefined order
     * 
     * @param preferredMap
     * @param peMap
     */
    private void populatePreferredDataInOrder(
            LinkedHashMap<String, ArrayList<SiteInfo>> preferredMap,
            Map<String, String[]> peMap) {
        if (peMap != null && preferredMap != null) {
            for (String pe : peMap.keySet()) {
                java.util.List<SiteInfo> siList = preferredMap.get(pe);

                if (siList == null) {
                    continue;
                }

                String[] tsList = peMap.get(pe);
                if (tsList == null) { // There's PE but no TS in
                                      // preffered_order.txt
                    for (SiteInfo si : siList) {
                        addToBottomTable(si);
                        siteInfoList.add(si);
                    }
                } else { // There's both PE and TS in preferred_order.txt
                    for (String ts : tsList) {
                        for (SiteInfo si : siList) {
                            if (ts.equalsIgnoreCase(si.getTs())) {
                                addToBottomTable(si);
                                siteInfoList.add(si);
                            }
                        }
                    }
                }
            }

            for (TableColumn column : bottomDataTable.getColumns()) {
                column.pack();
            }
        }
    }

    /**
     * A generic function that will call the correct group list population
     * function based on whether the dialog is being ran in standalone mode or
     * not.
     */
    private void populateGroupList() {
        if (!this.standaloneMode) {
            this.populateGroupListForStandard();
        } else {
            this.populateGroupListForStandalone();
        }

        for (GroupInfo gi : this.groupList) {
            for (PageInfo pi : gi.getPageInfoList()) {
                for (GraphData gd : pi.getGraphDataList()) {
                    gd.saveTraceInfo();
                }
            }
        }
    }

    /**
     * Populate the Group List box using the group configuration file that has
     * been identified in the hydro constants file.
     */
    private void populateGroupListForStandard() {
        groupDataList.removeAll();

        IPathManager pm = PathManagerFactory.getPathManager();
        File file = pm.getStaticFile(HydroConstants.GROUP_DEFINITION);

        if (file != null) {
            groupConfigFilePath = file.getAbsolutePath();
            this.readGroupList();
        } else {
            statusHandler.error("Unable to load predefined group file: "
                    + HydroConstants.GROUP_DEFINITION);
        }
    }

    /**
     * Populate the Group List box using the group configuration file that the
     * user has manually specified by setting the "whfs_config_dir" to the
     * directory that contains the file.
     */
    private void populateGroupListForStandalone() {
        groupDataList.removeAll();

        if (this.groupConfigFilePath != null) {
            this.readGroupList();
        }
    }

    /**
     * Read the Group List file.
     */
    private void readGroupList() {
        GroupInfo groupInfo = null;
        PageInfo pageInfo = null;
        GraphData graphData = null;

        try (BufferedReader in = new BufferedReader(new FileReader(
                groupConfigFilePath))) {
            String str;
            while ((str = in.readLine()) != null) {
                if (str.startsWith("#")) {
                    continue;
                }
                if (str.trim().equals("")) {
                    continue;
                }

                boolean showpp_flag = false;

                // Remove any leading whitespace
                String line = str.replaceAll("^\\s+", "");

                if (line.startsWith(GROUP)) {
                    groupInfo = new GroupInfo();
                    groupInfo.setGroupSelected(true);
                    groupList.add(groupInfo);
                }
                String[] parts = line.split(":");

                if (parts[0].equals(GROUP)) {
                    String[] pairs = parts[1].split(",");
                    for (String s : pairs) {
                        String[] values = s.split("=", 2);

                        // make sure we have values to go with the key
                        if (values.length > 1) {
                            if (values[0] != null
                                    && values[0].equalsIgnoreCase(NAME)) {
                                if (values[1] != null) {
                                    groupInfo.setGroupName(values[1]);
                                }
                                groupDataList.add(groupInfo.getGroupName());
                            } else if (values[0].equalsIgnoreCase(DESCRIPT)) {
                                groupInfo.setDescription(values[1]);
                            } else if (values[0].equalsIgnoreCase(GRID)) {
                                if (values[1].equalsIgnoreCase("T")) {
                                    groupInfo.setGridLines(true);
                                } else {
                                    groupInfo.setGridLines(false);
                                }
                            } else if (values[0].equalsIgnoreCase(TRACEMODE)) {
                                groupInfo.setTraceMode(values[1]);
                            } else if (values[0].equalsIgnoreCase(PASTHOURS)) {
                                groupInfo.setPastHours(Integer
                                        .parseInt(values[1]));
                            } else if (values[0].equalsIgnoreCase(FUTUREHOURS)) {
                                groupInfo.setFutureHours(Integer
                                        .parseInt(values[1]));
                            } else {
                                statusHandler.warn("Invalid key/value pair: "
                                        + s);
                            }
                        }
                    }
                } else if (parts[0].equals(PAGE)) {
                    String[] values = parts[1].split("=", 2);
                    if (values.length > 1) {
                        pageInfo = new PageInfo();
                        pageInfo.setTitle(values[1]);
                        groupInfo.addPageInfo(pageInfo);
                    }
                } else if (parts[0].equals(GRAPH)) {
                    graphData = new GraphData();

                    String[] pairs = parts[1].split(",");
                    for (String s : pairs) {
                        String[] values = s.split("=", 2);
                        if (values.length > 1) {
                            if (values[0].equalsIgnoreCase(POS)) {
                                graphData.setGraph_pos(Integer
                                        .parseInt(values[1]));
                            } else if (values[0].equalsIgnoreCase(XSIZE)) {
                                graphData.setXsize(Integer.parseInt(values[1]));
                            } else if (values[0].equalsIgnoreCase(YSIZE)) {
                                graphData.setYsize(Integer.parseInt(values[1]));
                            } else if (values[0].equalsIgnoreCase(YLINEAR)) {
                                graphData.setYlinear(values[1]);
                            } else if (values[0].equalsIgnoreCase(YSCALE)) {
                                graphData.setYscale(values[1]);
                            } else if (values[0].equalsIgnoreCase(SHOWCAT)) {
                                if (values[1].equalsIgnoreCase("T")) {
                                    graphData.setShowcat(true);
                                } else {
                                    graphData.setShowcat(false);
                                }
                            } else if (values[0].equalsIgnoreCase(DERIVEPP)) {
                                graphData.setDerivepp(values[1]);
                            } else if (values[0].equalsIgnoreCase(SHOWPP)) {
                                if (values[1].equalsIgnoreCase("T")) {
                                    showpp_flag = true;
                                } else {
                                    showpp_flag = false;
                                }
                            } else if (values[0]
                                    .equalsIgnoreCase(LATESTFCSTONLY)) {
                                if (values[1].equalsIgnoreCase("T")) {
                                    graphData.setLatestfcstonly(true);
                                } else {
                                    graphData.setLatestfcstonly(false);
                                }
                            } else {
                                statusHandler.warn("Invalid key/value pair: "
                                        + s);
                            }
                        }
                    }

                    // handle the case where there isn't a page element when
                    // there should be
                    if (pageInfo == null) {
                        pageInfo = new PageInfo();
                        groupInfo.addPageInfo(pageInfo);
                    }
                    pageInfo.addGraphData(graphData);
                } else if (parts[0].equals(TRACE)) {
                    TraceData td = new TraceData();
                    String[] pairs = parts[1].split(",");
                    for (String s : pairs) {
                        String[] values = s.split("=", 2);
                        if (values.length > 1) {
                            if (values[0].equalsIgnoreCase(STN)) {
                                td.setLid(values[1]);
                            } else if (values[0]
                                    .equalsIgnoreCase(HydroConstants.PC)) {
                                td.setPc(values[1]);
                                if (showpp_flag == true) {
                                    graphData.setShowpp(true);
                                } else {
                                    graphData.setShowpp(false);
                                }
                            } else if (values[0].equalsIgnoreCase(COLOR)) {
                                td.setColorName(values[1]);
                            }
                        }
                    }
                    graphData.addTrace(td);

                    graphData.setBeginDate(beginCal.getTime());
                    graphData.setEndDate(endCal.getTime());
                } else {
                    statusHandler
                            .warn("Error in Group Definition Config file: "
                                    + line);
                }

                // select the first item in the list
                if (groupDataList.getItemCount() > 0) {
                    groupDataList.select(0);
                }

            }
        } catch (IOException e) {
            statusHandler.error(
                    "Failed to read group definition configuration.", e);
        }
    }

    /**
     * Filter the upper list based on the boxes the user checks.
     */
    private void filterList() {
        // riverChk, precipChk, tempChk, snowChk, otherChk,
        try {
            populateStationList();
        } catch (VizException e) {
            statusHandler.error("Failed to populate station list", e);
        }
        filteredLidList = new ArrayList<String>();
        topDataTable.removeAll();
        boolean validClass = false;
        StringBuilder searchStr = new StringBuilder();
        if (riverChk.getSelection()) {
            searchStr.append("FDR");
        }
        if (precipChk.getSelection()) {
            searchStr.append("P");
        }
        if (tempChk.getSelection()) {
            searchStr.append("T");
        }
        if (snowChk.getSelection()) {
            searchStr.append("S");
        }
        if (otherChk.getSelection()) {
            searchStr.append("O");
        }
        if (searchStr.length() == 0) {
            try {
                populateStationList();
            } catch (VizException e) {
                statusHandler.error("Failed to populate station list", e);
            }
            return;
        } else {
            String lid = null;
            for (int i = 0; i < lidList.size(); i++) {
                for (int j = 0; j < searchStr.length(); j++) {
                    char searchChar = searchStr.charAt(j);
                    lid = lidList.get(i);
                    if (stnDisplayMap.containsKey(lid)) {
                        if (stnDisplayMap.get(lid).indexOf(searchChar) >= 0) {
                            validClass = true;
                            break;
                        }
                    }
                }
                if (validClass) {
                    filteredLidList.add(lid);
                    validClass = false;
                }
            }
        }
        lidList = filteredLidList;

        /* Populate the list */
        String lid = null;
        topDataTable.removeAll();
        for (int i = 0; i < lidList.size(); i++) {
            lid = lidList.get(i);
            TableItem item = new TableItem(topDataTable, SWT.NONE);
            item.setText(0, lid);
            item.setText(1, stationData.get(lid));
        }
        for (TableColumn column : topDataTable.getColumns()) {
            column.pack();
        }
    }

    /**
     * Change the selections.
     */
    private void correctSelections() {
        int count = bottomDataTable.getSelectionCount();
        TableItem[] item = bottomDataTable.getSelection();

        if (count == 1) {
            prevLidData = lidDataFromItem(item[0]);
            currLidData = prevLidData;
        } else if (count > 1) {
            /*
             * Check for valid combination of PEDTSEPs when there are two
             * different data types
             */
            boolean flag = false;
            boolean check1 = false;
            boolean check2 = false;
            int n = 0;

            /* Search for more than 2 different PEs */
            while (n < count && !flag) {
                LIDData tmpData = lidDataFromItem(item[n]);
                check1 = lidCheck(prevLidData, tmpData);
                if (currLidData != null && currLidData.getDur() != null
                        && currLidData.getPe() != null) {
                    check2 = lidCheck(currLidData, tmpData);
                }

                if (check1 && check2) {
                    /*
                     * Save the current PE to currLidData when prevLidData is
                     * the same as currLidData
                     */
                    if (prevLidData.equals(currLidData)) {
                        currLidData = tmpData;
                    } else {
                        flag = true;
                    }
                }
                n++;
            }

            if (flag) {
                /*
                 * Both of the prevLidData and currLidData has filled with
                 * different data, need deselect the PE items equal to
                 * prevLidData,
                 */
                int[] indices = bottomDataTable.getSelectionIndices();
                for (int i = 0; i < count; i++) {
                    LIDData tmpLidData = lidDataFromItem(item[i]);

                    check1 = lidCheck(prevLidData, tmpLidData);

                    if (!check1) {
                        // deselect this row
                        bottomDataTable.deselect(indices[i]);
                    }
                }
            }

            TableItem[] item2 = bottomDataTable.getSelection();
            for (TableItem element : item2) {
                LIDData tmpLidData = lidDataFromItem(element);

                check1 = lidCheck(prevLidData, tmpLidData);
                check2 = lidCheck(currLidData, tmpLidData);

                if (check1 && check2) {
                    /*
                     * Both of the prevLidData and currLidData has filled with
                     * different data, need: 1) move data from currLidData to
                     * prevLidData, and 2) save the new data to currLidData.
                     */
                    prevLidData = currLidData;
                    currLidData = tmpLidData;
                }
            }
        }

        // Reset the selections
        int selectedIndex = bottomDataTable.getSelectionIndex();

        for (int i = 0; i < siteInfoList.size(); i++) {
            SiteInfo si = siteInfoList.get(i);
            if (i == selectedIndex) {
                si.setSelected(true);
            } else {
                si.setSelected(false);
            }
        }

    }

    private LIDData lidDataFromItem(TableItem item) {
        return new LIDData(item.getText(0),
                TimeSeriesUtil.convertDurNameToValue(item.getText(3)));
    }

    /**
     * Check if two LIDs' data belong to the same group.
     * 
     * The rules are: 1) if the PEs are different; or 2) if the PEs of both LID
     * are "PP", but the durations are different
     * 
     * @param lidData1
     *            first lid data to compare
     * @param lidData2
     *            second lid data to compare
     * @return true if the lid data matches
     */
    private boolean lidCheck(LIDData lidData1, LIDData lidData2) {
        boolean result = false;
        if (lidData1 != null && lidData1.getDur() != null
                && lidData1.getPe() != null && lidData2 != null
                && lidData2.getDur() != null && lidData2.getPe() != null) {

            if (!lidData1.getPe().equalsIgnoreCase(lidData2.getPe())) {
                result = true;
            } else if (lidData1.getPe().equalsIgnoreCase("PP")
                    && lidData2.getPe().equalsIgnoreCase("PP")) {
                if (!lidData1.getDur().equalsIgnoreCase(lidData2.getDur())) {
                    result = true;
                }
            }
        }

        return result;
    }

    /**
     * Action called when the both button is pressed.
     */
    private void bothAction() {
        tableButton.setEnabled(false);
        graphButton.setEnabled(false);
        bothButton.setEnabled(false);
        openTabularDisplay();
        openGraph();
        tableButton.setEnabled(true);
        graphButton.setEnabled(true);
        bothButton.setEnabled(true);
        tabularDlg.getShell().moveAbove(this.shell);
        timeSeriesDisplayDlg.getShell().moveAbove(this.shell);
    }

    /**
     * Handle Table option when selected on the Time Series Control Dialog
     */
    private void openTabularDisplay() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        if (!validateForm()) {
            this.enableTableButton();
            this.enableBothButton();
            return;
        } else {
            if (modeCbo.getItem(modeCbo.getSelectionIndex()) == PREDEFINED_GROUP) {
                siteInfoList.clear();
            }

            tabInfoList.clear();

            if (tabularDlg != null && tabularDlg.isOpen()) {
                tabularDlg.close();
            }

            tabularDlg = new TabularTimeSeriesDlg(shell, beginCal.getTime(),
                    endCal.getTime(), this);

            TabInfo tabInfo = new TabInfo();
            tabInfo.setBeginTime(beginCal);
            tabInfo.setEndTime(endCal);

            /* Set up the GraphData objects */
            if (modeCbo.getText().equals(STATION_SELECTION)) {
                GroupInfo groupInfo = new GroupInfo();
                groupInfo.setCurrentPage(0);

                /* Get the data from the station list */
                TableItem selection = topDataTable.getItem(topDataTable
                        .getSelectionIndex());
                tabularDlg.setLid(selection.getText(0));
                tabularDlg.setSiteName(selection.getText(1).trim());
                tabInfo.addBuffer(selection.getText(0) + " "
                        + selection.getText(1).trim());
                tabInfo.setLid(selection.getText(0));

                /* Get the selections from the data list */
                int[] indices = bottomDataTable.getSelectionIndices();

                /*
                 * Loop through the site info list that are the data for the
                 * bottom list
                 */
                for (int i = 0; i < siteInfoList.size(); i++) {
                    for (int ind : indices) {
                        if (ind == i) {
                            siteInfoList.get(i).setSelected(true);
                            break;
                        }
                    }
                }
                tabInfo.setInfoList(siteInfoList);
                tabInfoList.add(tabInfo);

                tabularDlg.setGroupInfo(groupInfo);
            } else {

                /* Set the group info object */
                GroupInfo groupInfo = groupList.get(groupDataList
                        .getSelectionIndex());
                tabularDlg.setGroupInfo(groupInfo);

                for (PageInfo pi : groupInfo.getPageInfoList()) {
                    for (GraphData gd : pi.getGraphDataList()) {
                        for (TraceData td : gd.getTraces()) {
                            SiteInfo si = new SiteInfo();
                            si.setLid(td.getLid());
                            si.setPe(td.getPe());
                            si.setTs(td.getTs());
                            si.setDur(TimeSeriesUtil.convertDur2Short(td
                                    .getDur().charAt(0)));
                            si.setExt(td.getExtremum());

                            if (td.getBasistime() != null) {
                                si.setBasisTime(sdf.format(td.getBasistime()));
                            }
                            siteInfoList.add(si);
                        }
                    }
                }
                tabInfo.setLid(siteInfoList.get(0).getLid());

                tabInfo.setInfoList(siteInfoList);
                tabInfoList.add(tabInfo);
            }
            tabularDlg.setTabInfoList(tabInfoList);
            tabularDlg.open();
        }
    }

    /**
     * Open the graph.
     */
    private void openGraph() {
        if (!validateForm()) {
            this.enableGraphButton();
            this.enableBothButton();
            return;
        } else {

            if (timeSeriesDisplayDlg != null) {
                timeSeriesDisplayDlg.disposeDialog();
            }

            timeSeriesDisplayDlg = new TimeSeriesDisplayDlg(shell, this);

            PageInfo pageInfo = new PageInfo();
            LIDData firstLidData = new LIDData();

            pageInfo.setNumberGraphs(0);

            /* Set up the GraphData objects */
            if (modeCbo.getText().equals(STATION_SELECTION)) {
                int numberGraphs = 1;
                GroupInfo groupInfo = new GroupInfo();
                groupInfo.setCurrentPage(0);

                /* Get the data from the station list */
                TableItem[] sa = topDataTable.getSelection();
                timeSeriesDisplayDlg.setLid(sa[0].getText(0));
                timeSeriesDisplayDlg.setSiteName(sa[0].getText(1).trim());

                /* Get the data from the data list */
                int[] indices = bottomDataTable.getSelectionIndices();

                /* Hold the first value for reference */
                firstLidData = lidDataFromItem(bottomDataTable
                        .getItem(indices[0]));

                ArrayList<TraceData> dataList = new ArrayList<TraceData>();

                for (int indice : indices) {
                    /*
                     * Check the selections and determine if 1 or 2 graphs are
                     * needed
                     */
                    TableItem selection = bottomDataTable.getItem(indice);
                    LIDData lidData = lidDataFromItem(selection);

                    TraceData td = new TraceData();
                    td.setLid(sa[0].getText(0));
                    td.setName(sa[0].getText(1).trim());
                    td.setPe(selection.getText(0));
                    td.setTs(selection.getText(1));
                    td.setExtremum(selection.getText(2));
                    td.setDur(lidData.getDur());

                    dataList.add(td);

                    if (indices.length > 1 && lidCheck(firstLidData, lidData)) {
                        numberGraphs++;
                    }
                }
                /* If only one PE display in a single graph */
                if (numberGraphs == 1) {
                    GraphData graphData = new GraphData();
                    for (int i = 0; i < dataList.size(); i++) {
                        TraceData td = dataList.get(i);
                        td.setForecast(false);
                        if (td.getTs().startsWith("F")
                                || td.getTs().startsWith("C")) {
                            td.setForecast(true);
                        }
                        graphData.addTrace(td);
                    }
                    graphData.setBeginDate(beginCal.getTime());
                    graphData.setEndDate(endCal.getTime());
                    graphData.setGraph_pos(1);
                    graphData.saveTraceInfo();
                    pageInfo.addGraphData(graphData);
                    groupInfo.addPageInfo(pageInfo);
                } else {
                    /* Need two graphs to display the traces */
                    ArrayList<TraceData> secondTraceDataList = new ArrayList<TraceData>();
                    ;
                    // first graph
                    GraphData graph1 = new GraphData();
                    graph1.setGraph_pos(1);
                    graph1.setXsize(6);
                    graph1.setYsize(1);
                    for (int i = 0; i < dataList.size(); i++) {
                        TraceData td = dataList.get(i);
                        td.setForecast(false);
                        if (td.getTs().startsWith("F")
                                || td.getTs().startsWith("C")) {
                            td.setForecast(true);
                        }

                        LIDData tempLidData = new LIDData(td.getPe(),
                                td.getDur());

                        if (!lidCheck(firstLidData, tempLidData)) {
                            graph1.addTrace(td);
                        } else {
                            secondTraceDataList.add(td);
                        }
                    }

                    graph1.saveTraceInfo();
                    pageInfo.addGraphData(graph1);
                    if (secondTraceDataList.size() > 0) {

                        /* Second graph setup */
                        GraphData graph2 = new GraphData();
                        graph2.setNum_traces(0);
                        graph2.setGraph_pos(7);
                        graph2.setXsize(6);
                        graph2.setYsize(1);

                        firstLidData = new LIDData(secondTraceDataList.get(0)
                                .getPe(), secondTraceDataList.get(0).getDur());

                        for (int i = 0; i < secondTraceDataList.size(); i++) {
                            TraceData td = secondTraceDataList.get(i);
                            td.setForecast(false);
                            if (td.getTs().startsWith("F")
                                    || td.getTs().startsWith("C")) {
                                td.setForecast(true);
                            }

                            graph2.addTrace(td);
                        }
                        graph2.saveTraceInfo();
                        pageInfo.addGraphData(graph2);
                    }
                }
                groupInfo.addPageInfo(pageInfo);
                timeSeriesDisplayDlg.setGroupInfo(groupInfo);
            } else {
                GroupInfo groupInfo = groupList.get(groupDataList
                        .getSelectionIndex());

                if (prevGroupInfo == null || !prevGroupInfo.equals(groupInfo)) {
                    int pastHours = groupInfo.getPastHours();
                    int futureHours = groupInfo.getFutureHours();
                    Date d = SimulatedTime.getSystemTime().getTime();
                    beginCal.setTime(d);
                    beginCal.add(Calendar.HOUR_OF_DAY, pastHours * -1);
                    beginningTimeControl.redraw();

                    endCal.setTime(d);
                    endCal.add(Calendar.HOUR_OF_DAY, futureHours);
                    endingTimeControl.redraw();

                    groupInfo.setPastHours(pastHours);
                    groupInfo.setFutureHours(futureHours);
                }
                timeSeriesDisplayDlg.setGroupInfo(groupInfo);

                prevGroupInfo = groupInfo;
            }
            timeSeriesDisplayDlg.setBeginDate(beginCal.getTime());
            timeSeriesDisplayDlg.setEndDate(endCal.getTime());

            if (!timeSeriesDisplayDlg.isOpen()) {
                timeSeriesDisplayDlg.open();
            } else {
                timeSeriesDisplayDlg.createNewGraph();
            }

            graphButton.setEnabled(true);
        }
    }

    /**
     * Validate the user's selections.
     * 
     * @return
     */
    private boolean validateForm() {
        boolean valid = true;
        if (modeCbo.getText().equals(STATION_SELECTION)) {
            if (endCal.getTimeInMillis() - beginCal.getTimeInMillis() < 0) {
                MessageDialog.openWarning(shell, "Invalid Date Selection",
                        "Ending Time is prior to Beginning Time");
                valid = false;
            }

            long numberOfDays = (endCal.getTimeInMillis() - beginCal
                    .getTimeInMillis())
                    / HydroConstants.MILLIS_PER_MINUTE
                    / 60
                    / 24;

            if (numberOfDays > HydroConstants.DAYS_MAX) {
                MessageDialog.openWarning(shell, "Invalid Date Selection",
                        "Time Period exceeds " + HydroConstants.DAYS_MAX
                                + " days");
                valid = false;
            }

            if (topDataTable.getSelectionIndex() == -1) {
                MessageDialog.openWarning(shell, "Invalid Selection",
                        "A Location ID is required");
                valid = false;
            }

            if (bottomDataTable.getSelectionIndex() == -1) {
                MessageDialog.openWarning(shell, "Invalid Selection",
                        "A Physical Element is required");
                valid = false;
            }
        } else {
            if (groupDataList.getSelectionIndex() == -1) {
                MessageDialog.openWarning(shell, "Invalid Selection",
                        "A Group is required");
                valid = false;
            }
        }
        return valid;
    }

    /**
     * Set the PEDTSEP list selection.
     */
    private void setCurrentData() {
        if (gageData == null) {
            gageData = HydroDisplayManager.getInstance().getCurrentData();
        }

        if (gageData != null) {
            String pe = gageData.getPe();
            String ts = gageData.getTs();
            String ext = gageData.getExtremum();

            for (TableItem item : bottomDataTable.getItems()) {
                if (pe.equalsIgnoreCase(item.getText(0))
                        && ts.equalsIgnoreCase(item.getText(1))
                        && ext.equalsIgnoreCase(item.getText(2))) {
                    bottomDataTable.setSelection(item);
                    break;
                }
            }
        }

        /* used for questionable and Bad Data Gui */
        if (currentPe != null && currentTs != null) {
            String qPe = currentPe;
            String qTs = currentTs;

            for (TableItem item : bottomDataTable.getItems()) {
                if (qPe.equalsIgnoreCase(item.getText(0))
                        && qTs.equalsIgnoreCase(item.getText(1))) {
                    bottomDataTable.setSelection(item);
                    break;
                }
            }
        }
    }

    /**
     * Process the data in the list and display it in the bottomDataList.
     * 
     * @param list
     *            The ArrayList of data
     * @param tsSelected
     *            true if a TS was selected
     */
    private void processDataList(
            LinkedHashMap<String, ArrayList<SiteInfo>> dataMap,
            boolean tsSelected) {
        if (dataMap.size() > 0) {
            final String OTHER = "OTHER";
            Set<String> peSet = dataMap.keySet();
            Iterator<String> iter = peSet.iterator();
            Map<String, ArrayList<SiteInfo>> tsMap = new HashMap<String, ArrayList<SiteInfo>>();
            for (String ts : TS_ORDER) {
                tsMap.put(ts, new ArrayList<SiteInfo>());
            }
            tsMap.put(OTHER, new ArrayList<SiteInfo>());

            ArrayList<SiteInfo> list = null;

            String selectedTs = tsOrderCbo.getItem(tsOrderCbo
                    .getSelectionIndex());

            while (iter.hasNext()) {
                String pe = iter.next();
                list = dataMap.get(pe);

                for (String ts : TS_ORDER) {
                    tsMap.get(ts).clear();
                }

                tsMap.get(OTHER).clear();

                OUTER: for (SiteInfo si : list) {
                    for (String ts : TS_ORDER) {
                        if (si.getTs().startsWith(ts)) {
                            tsMap.get(ts).add(si);
                            continue OUTER;
                        }
                    }

                    tsMap.get(OTHER).add(si);
                }

                if (tsSelected) {
                    ArrayList<SiteInfo> siList = tsMap.get(selectedTs
                            .substring(0, 1));
                    ArrayList<SiteInfo> numList = new ArrayList<SiteInfo>();
                    for (SiteInfo si : siList) {
                        // Add the selected TS
                        if (si.getTs().equals(selectedTs)) {
                            // Check for TS values with a digit, those go after
                            // the TS
                            // values not containing digits
                            if (si.getTs().matches("\\D*\\d+\\D*")) {
                                numList.add(si);
                            } else {
                                addToBottomTable(si);
                                siteInfoList.add(si);
                            }
                        }
                    }
                    for (SiteInfo si : numList) {
                        addToBottomTable(si);
                        siteInfoList.add(si);
                    }
                }

                ArrayList<SiteInfo> numList = new ArrayList<SiteInfo>();
                for (String ts : TS_ORDER) {
                    ArrayList<SiteInfo> siList = tsMap.get(ts);
                    for (SiteInfo si : siList) {
                        if (!siteInfoList.contains(si)) {
                            if (si.getTs().matches("\\D*\\d+\\D*")) {
                                numList.add(si);
                            } else {
                                addToBottomTable(si);
                                siteInfoList.add(si);
                            }
                        }
                    }

                    for (SiteInfo si : numList) {
                        addToBottomTable(si);
                        siteInfoList.add(si);
                    }
                    numList.clear();
                }

                numList.clear();
                ArrayList<SiteInfo> siList = tsMap.get(OTHER);
                for (SiteInfo si : siList) {
                    if (si.getTs().matches("\\D*\\d+\\D*")) {
                        numList.add(si);
                    } else {
                        addToBottomTable(si);
                        siteInfoList.add(si);
                    }
                }

                for (SiteInfo si : numList) {
                    addToBottomTable(si);
                    siteInfoList.add(si);
                }

            }

            for (TableColumn column : bottomDataTable.getColumns()) {
                column.pack();
            }
        }
    }

    /**
     * Update the dialog prepare for open.
     * 
     * @param gageData
     * @param displayGraph
     */
    public void updateSelection(GageData gageData, boolean displayGraph) {
        currentLid = gageData.getLid();
        this.displayGraph = displayGraph;
        openTimeSeriesDisplays = true;
        this.gageData = gageData;

        /* Add a complete list of stations to the station list */
        try {
            populateStationList();
        } catch (VizException e) {
            statusHandler.error("Failed to populate station list", e);
        }
        setCurrentData();
        opened();
    }

    /**
     * Add an item to the bottomDataTable
     * 
     * @param si
     *            The SiteInfo object
     */
    private void addToBottomTable(SiteInfo si) {
        HydroDataCache hydroCache = HydroDataCache.getInstance();

        TableItem item = new TableItem(bottomDataTable, SWT.NONE);
        item.setText(0, si.getPe());
        item.setText(1, si.getTs());
        item.setText(2, si.getExt());
        item.setText(3, TimeSeriesUtil.convertDur2Text(si.getDur()));

        String peDesc = hydroCache.getPEDescription(si.getPe());
        if (peDesc != null) {
            item.setText(4, String.format("%-2s=%-22s", si.getPe(), peDesc));
        }

        String tsDesc = hydroCache.getTSDesc(si.getTs());
        if (tsDesc != null) {
            item.setText(4, String.format("%s=%s", si.getTs(), tsDesc));
        }

    }

    /**
     * Enable Graph Button
     */
    public void enableGraphButton() {
        this.graphButton.setEnabled(true);
    }

    /**
     * Enable Table Button
     */
    public void enableTableButton() {
        this.tableButton.setEnabled(true);
    }

    /**
     * Enable Both Button
     */
    public void enableBothButton() {
        this.bothButton.setEnabled(true);
    }

    @Override
    protected void preOpened() {
        super.preOpened();
    }

    /**
     * In case user selected a different station in the Hydro perspective,
     * update currentLid, etc
     */
    public void updateFromDisplayManager() {
        HydroDisplayManager hdm = HydroDisplayManager.getInstance();
        String newLid = hdm.getCurrentLid();
        if (newLid != null && !newLid.equalsIgnoreCase(currentLid)) {
            updateAndOpen(newLid, this.displayGraph);
            openGraph();
        }
    }

}
