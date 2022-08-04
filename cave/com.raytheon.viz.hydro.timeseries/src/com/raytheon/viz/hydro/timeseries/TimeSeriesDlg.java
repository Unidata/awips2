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
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
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
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.timeseries.table.SiteInfo;
import com.raytheon.viz.hydro.timeseries.table.TabInfo;
import com.raytheon.viz.hydro.timeseries.util.GraphInfo;
import com.raytheon.viz.hydro.timeseries.util.GraphInfo.DERIVE_PP;
import com.raytheon.viz.hydro.timeseries.util.GroupData;
import com.raytheon.viz.hydro.timeseries.util.GroupInfo;
import com.raytheon.viz.hydro.timeseries.util.GroupInfo.TraceMode;
import com.raytheon.viz.hydro.timeseries.util.LIDData;
import com.raytheon.viz.hydro.timeseries.util.PageInfo;
import com.raytheon.viz.hydro.timeseries.util.PreferredOrderManager;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesUtil;
import com.raytheon.viz.hydro.timeseries.util.TraceInfo;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDataCache;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.ui.CaveHydroSWTDialog;
import com.raytheon.viz.hydrocommon.util.StnClassSyncUtil;
import com.raytheon.viz.ui.widgets.DateTimeSpinner;

/**
 * This class displays the Time Series dialog for Hydroview.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Nov 29, 2007  373         lvenable     Initial creation.
 * Jun 09, 2008  1194        M. Duff      Implement the Dialog.
 * Oct 21, 2008  1520        M. Duff      Implement the Tabular Portion.
 * Nov 06, 2009  2639        M. Duff      Bug fixes.
 * Jun 15, 2010  5828        M. Duff      Added additional error checking for
 *                                        loading of group_definition.cfg.
 * Jul 02, 2010  5280        lbousaidi    added Page number to the display
 * Sep 09, 2010  5399        lbousaidi    Added a new constructor and code to
 *                                        highlight PE TS in bottomListData
 *                                        related to selected Lid
 * Sep 14, 2010  5282        lbousaidi    set default selections and re-uses the
 *                                        open Time Series Control window
 * Jan 24, 2011  7797        bkowal       updates so that Hydro Time Series can
 *                                        be ran as a standalone application
 *                                        without starting CAVE.
 * Jun 01, 2011  9499        djingtao     update openGraph()
 * Jul 23, 2012  15180       mpduff       Auto select the first group in the
 *                                        predefined group list
 * Jul 23, 2012  15195       mpduff       Fix Group graphing to use the date
 *                                        widgets.
 * Aug 08, 2012  570         mpduff       Fix a Ctrl-F in Station list causing
 *                                        IndexOutOfBounds error.
 * Aug 08, 2012  657         mpduff       Fix error when selecting a TS while no
 *                                        selection has been made in the Station
 *                                        List.
 * Sep 27, 2012  15302       wkwock       TimeSeries start mode should depends
 *                                        on token timeseries_mode despite start
 *                                        up in CAVE or standalone.
 * Jan 30, 2013  15264       wkwock       Fix the missing group_definition.cfg
 *                                        file crash
 * Feb 05, 2013  1578        rferrel      Dialog made non-blocking and a
 *                                        singleton.
 * May 06, 2013  1976        mpduff       Code cleanup.
 * Jun 06, 2013  2076        mpduff       Fix station list selection and graph
 *                                        button enabling.
 * Jun 30, 2013  15980       wkwock       Fix selected station not update
 * Jul 21, 2015  4500        rjpeter      Use Number in blind cast.
 * Oct 13, 2015  4933        rferrel      Log error if unable to find group
 *                                        definition file Fixed formatter
 *                                        resource leaks.
 * 30 Oct, 2015  15102       wkwock       Implements preferred order for
 *                                        PE-D-TS-EXT list
 * 26 Oct, 2015  14217       jwu          Removed DAYS_MAX & MAX_TRACES
 *                                        limitations
 * Jan 26, 2016  5054        randerso     Allow dialog to be parented to display
 * Mar 17, 2016  5483        randerso     Major GUI cleanup
 * May 04, 2016  5602        bkowal       Updates for compatibility with
 *                                        enhanced {@link DateTimeSpinner}.
 * May 12, 2016  5289        tgurney      Add minimize and maximize trim buttons
 * Oct 24, 2016  5955        randerso     Code/JavaDoc cleanup
 * Apr 12, 2018  6619        randerso     Code cleanup.
 * Map 07, 2018  7005        mduff        Clear group list before rereading it.
 * May 16, 2018  6749        randerso     Code cleanup.
 * Jun 27, 2018  6748        randerso     Center on CAVE window. Force station
 *                                        search text to upper case. Significant
 *                                        changes to better match A1.
 *
 * </pre>
 *
 * @author lvenable
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

    private static final String[] TS_ORDER = { "R", "F", "P", "M", "C" };

    private static final long DAYS_MAX = 90;

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
    private java.util.List<String> lidList;

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
    private final java.util.List<GroupInfo> groupList = new ArrayList<>();

    /** Holds the last graphed GroupInfo object */
    private GroupInfo prevGroupInfo;

    private String groupConfigFilePath = null;

    private final java.util.List<SiteInfo> siteInfoList = new ArrayList<>();

    private final java.util.List<TabInfo> tabInfoList = new ArrayList<>();

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

    /**
     * When not in stand alone mode this allows only a single instance of the
     * dialog.
     *
     * @return instance
     */
    public static final synchronized TimeSeriesDlg getInstance() {
        // Independent shell must be recreated after closing.
        if (instance == null || !instance.isOpen()) {
            Shell shell = null;
            if (PlatformUI.isWorkbenchRunning()) {
                IWorkbenchWindow window = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow();
                if (window != null) {
                    shell = window.getShell();
                }
            }

            if (shell != null) {
                instance = new TimeSeriesDlg(shell);
            } else {
                statusHandler.error(
                        "TimeSeriesDlg.getInstance() should only be called from the UI thread in an active CAVE session");
            }
        }
        return instance;

    }

    /**
     * Constructor.
     *
     * @param display
     */
    private TimeSeriesDlg(Shell parent) {
        super(parent, SWT.MAX | SWT.MIN | SWT.RESIZE, CAVE.INDEPENDENT_SHELL);
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
        super(display, SWT.MAX | SWT.MIN | SWT.RESIZE, CAVE.INDEPENDENT_SHELL);
        setText("Time Series Control");

        displayManager = HydroDisplayManager.getInstance();
        if (displayManager.getCurrentLid() != null) {
            currentLid = displayManager.getCurrentLid();
        }

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
                statusHandler.handle(Priority.PROBLEM,
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
        PreferredOrderManager.getInstance().dispose();
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

        checkBottomButtons();
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        // Get the begin and end values from Apps_defaults
        AppsDefaults appsDef = AppsDefaults.getInstance();
        int beginDays = Integer
                .parseInt(appsDef.getToken("timeseries_begintime", "30"));
        int endDays = Integer
                .parseInt(appsDef.getToken("timeseries_endtime", "5"));
        String startMode = appsDef.getToken("timeseries_mode", "STATION");
        Date d = SimulatedTime.getSystemTime().getTime();
        Calendar beginCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        beginCal.setTime(d);
        beginCal.add(Calendar.DAY_OF_MONTH, beginDays * -1);
        beginCal.set(Calendar.MINUTE, 0);
        beginCal.set(Calendar.SECOND, 0);
        beginCal.set(Calendar.MILLISECOND, 0);

        Calendar endCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        endCal.setTime(d);
        endCal.add(Calendar.DAY_OF_MONTH, endDays);
        endCal.set(Calendar.MINUTE, 0);
        endCal.set(Calendar.SECOND, 0);
        endCal.set(Calendar.MILLISECOND, 0);

        createTimeControls(beginCal, endCal);

        createStationControls();
        createGroupControl();
        createBottomButtons();

        /* Add a complete list of stations to the station list */
        try {
            populateStationList();
        } catch (VizException e) {
            statusHandler.error("Failed to populate station list", e);
        }

        if ("GROUP".equals(startMode) && !displayGraph) {
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

        groupDataList = new List(groupGroup,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL);

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
    private void createTimeControls(Calendar beginCal, Calendar endCal) {
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
        Button checkAllBtn = new Button(checkComp, SWT.PUSH);
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
        searchTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
                e.text = e.text.replaceAll("\\s", "");
            }
        });
        searchTF.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                String search = searchTF.getText().trim().toUpperCase();
                if (!search.isEmpty()) {
                    for (TableItem item : topDataTable.getItems()) {
                        if (idRdo.getSelection()) {
                            if (item.getText(0).toUpperCase()
                                    .startsWith(search)) {
                                topDataTable.setSelection(item);
                                break;
                            }
                        } else {

                            if (item.getText(1).toUpperCase()
                                    .startsWith(search)) {
                                topDataTable.setSelection(item);
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
                    TableItem item = topDataTable
                            .getItem(topDataTable.getSelectionIndex());
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

        topDataTable = new Table(comp,
                SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION);
        topDataTable.setHeaderVisible(true);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GC gc = new GC(topDataTable);
        int textWidth = gc.getFontMetrics().getAverageCharWidth() * 30;
        gc.dispose();

        gd.widthHint = textWidth;
        gd.heightHint = topDataTable.getItemHeight() * 9;
        topDataTable.setLayoutData(gd);

        for (Pair<String, Integer> pair : topDataTableLabels) {
            TableColumn column = new TableColumn(topDataTable,
                    pair.getSecond());
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

        bottomDataTable = new Table(comp,
                SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION);

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
        lidList = new ArrayList<>();

        /* Check the locview and stnclass table counts */
        if (!dataManager.checkLidCount()) {
            long locCount = dataManager.recordCount("locview", "");
            long stnClassCount = dataManager.recordCount("stnClass", "");
            StringBuilder errMessage = new StringBuilder();
            errMessage.append(
                    "Error: Number of records in LocView " + locCount + " ");
            errMessage.append("does not equal number of records in StnClass "
                    + stnClassCount + ". ");
            errMessage.append(""
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

        stationData = dataManager
                .getTimeSeriesStationData(nameRdo.getSelection());
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
            for (TableItem item : topDataTable.getItems()) {
                if (currentLid.equalsIgnoreCase(item.getText(0))) {
                    topDataTable.setSelection(item);
                    topDataTable.showSelection();

                    populateBottomList(currentLid,
                            tsOrderCbo.getSelectionIndex());

                    /* set to null so we don't enter this block again */
                    currentLid = null;
                    break;
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
            java.util.List<Object[]> data = dataManager
                    .getSitePEData(selectedLid);

            PreferredOrderManager pom = PreferredOrderManager.getInstance();
            Map<String, String[]> peMap = pom.getPreferedOrder(selectedLid);

            bottomDataTable.removeAll();

            /* Get the lists of PE data */
            LinkedHashMap<String, ArrayList<SiteInfo>> preferredMap = new LinkedHashMap<>();
            LinkedHashMap<String, ArrayList<SiteInfo>> hMap = new LinkedHashMap<>();
            LinkedHashMap<String, ArrayList<SiteInfo>> qMap = new LinkedHashMap<>();
            LinkedHashMap<String, ArrayList<SiteInfo>> pMap = new LinkedHashMap<>();
            LinkedHashMap<String, ArrayList<SiteInfo>> sMap = new LinkedHashMap<>();
            LinkedHashMap<String, ArrayList<SiteInfo>> tMap = new LinkedHashMap<>();
            LinkedHashMap<String, ArrayList<SiteInfo>> otherMap = new LinkedHashMap<>();

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
            for (Entry<String, String[]> entry : peMap.entrySet()) {
                String pe = entry.getKey();
                java.util.List<SiteInfo> siList = preferredMap.get(pe);

                if (siList == null) {
                    continue;
                }

                String[] tsList = entry.getValue();
                if (tsList == null) {
                    // There's PE but no TS in preffered_order.txt
                    for (SiteInfo si : siList) {
                        addToBottomTable(si);
                        siteInfoList.add(si);
                    }
                } else {
                    // There's both PE and TS in preferred_order.txt
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
     * function based on whether the dialog is being ran in stand alone mode or
     * not.
     */
    private void populateGroupList() {
        if (!this.standaloneMode) {
            this.populateGroupListForStandard();
        } else {
            this.populateGroupListForStandalone();
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
        GraphInfo graphInfo = null;
        groupList.clear();

        // Make sure group definition file exists.
        if (groupConfigFilePath == null) {
            return;
        }

        try (BufferedReader in = new BufferedReader(
                new FileReader(groupConfigFilePath))) {
            String str;
            while ((str = in.readLine()) != null) {
                if (str.startsWith("#")) {
                    continue;
                }
                if (str.trim().isEmpty()) {
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
                                if ("T".equalsIgnoreCase(values[1])) {
                                    groupInfo.setGridLines(true);
                                } else {
                                    groupInfo.setGridLines(false);
                                }
                            } else if (values[0].equalsIgnoreCase(TRACEMODE)) {
                                groupInfo.setTraceMode(
                                        TraceMode.fromString(values[1]));
                            } else if (values[0].equalsIgnoreCase(PASTHOURS)) {
                                groupInfo.setPastHours(
                                        Integer.parseInt(values[1]));
                            } else if (values[0]
                                    .equalsIgnoreCase(FUTUREHOURS)) {
                                groupInfo.setFutureHours(
                                        Integer.parseInt(values[1]));
                            } else {
                                statusHandler
                                        .warn("Invalid key/value pair: " + s);
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
                    graphInfo = new GraphInfo();

                    String[] pairs = parts[1].split(",");
                    for (String s : pairs) {
                        String[] values = s.split("=", 2);
                        if (values.length > 1) {
                            if (values[0].equalsIgnoreCase(POS)) {
                                graphInfo.setGraphPos(
                                        Integer.parseInt(values[1]));
                            } else if (values[0].equalsIgnoreCase(XSIZE)) {
                                graphInfo.setXsize(Integer.parseInt(values[1]));
                            } else if (values[0].equalsIgnoreCase(YSIZE)) {
                                graphInfo.setYsize(Integer.parseInt(values[1]));
                            } else if (values[0].equalsIgnoreCase(YLINEAR)) {
                                graphInfo.setYlinear(values[1]);
                            } else if (values[0].equalsIgnoreCase(YSCALE)) {
                                graphInfo.setYscaleToData(
                                        "D".equalsIgnoreCase(values[1]));
                            } else if (values[0].equalsIgnoreCase(SHOWCAT)) {
                                graphInfo.setShowcat(
                                        "T".equalsIgnoreCase(values[1]));
                            } else if (values[0].equalsIgnoreCase(DERIVEPP)) {
                                graphInfo.setDerivepp(
                                        DERIVE_PP.fromString(values[1]));
                            } else if (values[0].equalsIgnoreCase(SHOWPP)) {
                                showpp_flag = "T".equalsIgnoreCase(values[1]);
                            } else if (values[0]
                                    .equalsIgnoreCase(LATESTFCSTONLY)) {
                                graphInfo.setLatestfcstonly(
                                        "T".equalsIgnoreCase(values[1]));
                            } else {
                                statusHandler
                                        .warn("Invalid key/value pair: " + s);
                            }
                        }
                    }
                    if (!showpp_flag) {
                        graphInfo.setDerivepp(DERIVE_PP.NO_PC_TO_PP);
                    }

                    // handle the case where there isn't a page element when
                    // there should be
                    if (pageInfo == null) {
                        pageInfo = new PageInfo();
                        groupInfo.addPageInfo(pageInfo);
                    }
                    pageInfo.addGraphInfo(graphInfo);
                } else if (parts[0].equals(TRACE)) {
                    TraceInfo traceInfo = new TraceInfo();
                    String[] pairs = parts[1].split(",");
                    for (String s : pairs) {
                        String[] values = s.split("=", 2);
                        if (values.length > 1) {
                            if (values[0].equalsIgnoreCase(STN)) {
                                traceInfo.setLid(values[1]);
                            } else if (values[0]
                                    .equalsIgnoreCase(HydroConstants.PC)) {
                                traceInfo.setPc(values[1]);
                            } else if (values[0].equalsIgnoreCase(COLOR)) {
                                traceInfo.setColorName(values[1]);
                            }
                        }
                    }
                    graphInfo.addTraceInfo(traceInfo);
                } else {
                    statusHandler.warn(
                            "Error in Group Definition Config file: " + line);
                }

                // select the first item in the list
                if (groupDataList.getItemCount() > 0) {
                    groupDataList.select(0);
                }

            }
        } catch (IOException e) {
            statusHandler
                    .error("Failed to read group definition configuration.", e);
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
        java.util.List<String> filteredLidList = new ArrayList<>();
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
                if (currLidData != null && currLidData.getPe() != null) {
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
        SiteInfo si = (SiteInfo) item.getData();
        return new LIDData(si.getPe(), si.getDur());
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
        if (lidData1 != null && lidData1.getPe() != null && lidData2 != null
                && lidData2.getPe() != null) {

            if (!lidData1.getPe().equalsIgnoreCase(lidData2.getPe())) {
                result = true;
            } else if (HydroConstants.PP.equalsIgnoreCase(lidData1.getPe())
                    && HydroConstants.PP.equalsIgnoreCase(lidData2.getPe())) {
                if (lidData1.getDur() != lidData2.getDur()) {
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

        // Open display/graph if user's selections are valid.
        if (validateForm()) {
            openTabularDisplay();
            openGraph();
            tabularDlg.getShell().moveAbove(this.shell);
            timeSeriesDisplayDlg.getShell().moveAbove(this.shell);
        }

        tableButton.setEnabled(true);
        graphButton.setEnabled(true);
        bothButton.setEnabled(true);

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
            if (modeCbo
                    .getItem(modeCbo.getSelectionIndex()) == PREDEFINED_GROUP) {
                siteInfoList.clear();
            }

            tabInfoList.clear();

            if (tabularDlg != null && tabularDlg.isOpen()) {
                tabularDlg.close();
            }

            Calendar beginCal = beginningTimeControl.getSelection();
            Calendar endCal = endingTimeControl.getSelection();
            tabularDlg = new TabularTimeSeriesDlg(shell, beginCal.getTime(),
                    endCal.getTime(), this);

            TabInfo tabInfo = new TabInfo();
            tabInfo.setBeginTime(beginCal);
            tabInfo.setEndTime(endCal);

            /* Set up the GraphData objects */
            if (modeCbo.getText().equals(STATION_SELECTION)) {
                GroupInfo groupInfo = new GroupInfo();

                /* Get the data from the station list */
                TableItem selection = topDataTable
                        .getItem(topDataTable.getSelectionIndex());
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
                GroupInfo groupInfo = groupList
                        .get(groupDataList.getSelectionIndex());
                tabularDlg.setGroupInfo(groupInfo);

                for (PageInfo pageInfo : groupInfo.getPageInfoList()) {
                    for (GraphInfo graphInfo : pageInfo.getGraphInfoList()) {
                        for (TraceInfo traceInfo : graphInfo
                                .getTraceInfoList()) {
                            SiteInfo si = new SiteInfo();
                            si.setLid(traceInfo.getLid());
                            si.setPe(traceInfo.getPe());
                            si.setTs(traceInfo.getTs());
                            si.setDur(traceInfo.getDur());
                            si.setExt(traceInfo.getExtremum());

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
                timeSeriesDisplayDlg.close();
                timeSeriesDisplayDlg = null;
            }

            /* Set up the GraphData objects */
            GroupInfo groupInfo;
            if (modeCbo.getText().equals(STATION_SELECTION)) {
                groupInfo = getStationInfo();
            } else {
                groupInfo = groupList.get(groupDataList.getSelectionIndex());

                if (prevGroupInfo == null || !prevGroupInfo.equals(groupInfo)) {
                    int pastHours = groupInfo.getPastHours();
                    int futureHours = groupInfo.getFutureHours();
                    Date d = SimulatedTime.getSystemTime().getTime();
                    Calendar beginCal = beginningTimeControl.getSelection();
                    beginCal.setTime(d);
                    beginCal.add(Calendar.HOUR_OF_DAY, pastHours * -1);
                    beginningTimeControl.setSelection(beginCal);

                    Calendar endCal = endingTimeControl.getSelection();
                    endCal.setTime(d);
                    endCal.add(Calendar.HOUR_OF_DAY, futureHours);
                    endingTimeControl.setSelection(endCal);

                    groupInfo.setPastHours(pastHours);
                    groupInfo.setFutureHours(futureHours);
                }
                prevGroupInfo = groupInfo;
            }
            GroupData groupData = new GroupData(groupInfo);

            Date beginDate = beginningTimeControl.getSelection().getTime();
            Date endDate = endingTimeControl.getSelection().getTime();
            groupData.setBeginDate(beginDate);
            groupData.setEndDate(endDate);

            groupData.getCurrentPage().loadData(beginDate, endDate);
            timeSeriesDisplayDlg = new TimeSeriesDisplayDlg(this, groupData);

            timeSeriesDisplayDlg.open();

            graphButton.setEnabled(true);
        }
    }

    private GroupInfo getStationInfo() {

        int numberGraphs = 1;
        GroupInfo groupInfo = new GroupInfo();

        /* Get the data from the station list */
        TableItem[] sa = topDataTable.getSelection();

        /* Get the data from the data list */
        int[] indices = bottomDataTable.getSelectionIndices();

        /* Hold the first value for reference */
        LIDData firstLidData = lidDataFromItem(
                bottomDataTable.getItem(indices[0]));

        java.util.List<TraceInfo> dataList = new ArrayList<>();

        for (int index : indices) {
            /*
             * Check the selections and determine if 1 or 2 graphs are needed
             */
            TableItem selection = bottomDataTable.getItem(index);
            LIDData lidData = lidDataFromItem(selection);

            TraceInfo traceInfo = new TraceInfo();
            traceInfo.setLid(sa[0].getText(0));
            traceInfo.setName(sa[0].getText(1).trim());
            traceInfo.setPe(selection.getText(0));
            traceInfo.setTs(selection.getText(1));
            traceInfo.setExtremum(selection.getText(2));
            traceInfo.setDur(lidData.getDur());

            dataList.add(traceInfo);

            if (indices.length > 1 && lidCheck(firstLidData, lidData)) {
                numberGraphs++;
            }
        }
        /* If only one PE display in a single graph */
        PageInfo pageInfo = new PageInfo();
        if (numberGraphs == 1) {
            GraphInfo graphInfo = new GraphInfo();
            for (TraceInfo traceInfo : dataList) {
                traceInfo.setForecast(false);
                if (traceInfo.getTs().startsWith("F")
                        || traceInfo.getTs().startsWith("C")) {
                    traceInfo.setForecast(true);
                }
                graphInfo.addTraceInfo(traceInfo);
            }
            graphInfo.setGraphPos(1);
            graphInfo.setXsize(6);
            graphInfo.setYsize(2);
            pageInfo.addGraphInfo(graphInfo);
        } else {
            /* Need two graphs to display the traces */
            java.util.List<TraceInfo> secondTraceInfoList = new ArrayList<>();

            // first graph
            GraphInfo graphInfo1 = new GraphInfo();
            graphInfo1.setGraphPos(1);
            graphInfo1.setXsize(6);
            graphInfo1.setYsize(1);
            for (TraceInfo traceInfo : dataList) {
                traceInfo.setForecast(false);
                if (traceInfo.getTs().startsWith("F")
                        || traceInfo.getTs().startsWith("C")) {
                    traceInfo.setForecast(true);
                }

                LIDData tempLidData = new LIDData(traceInfo.getPe(),
                        traceInfo.getDur());

                if (!lidCheck(firstLidData, tempLidData)) {
                    graphInfo1.addTraceInfo(traceInfo);
                } else {
                    secondTraceInfoList.add(traceInfo);
                }
            }

            pageInfo.addGraphInfo(graphInfo1);
            if (!secondTraceInfoList.isEmpty()) {

                /* Second graph setup */
                GraphInfo graphInfo2 = new GraphInfo();
                graphInfo2.setGraphPos(7);
                graphInfo2.setXsize(6);
                graphInfo2.setYsize(1);

                firstLidData = new LIDData(secondTraceInfoList.get(0).getPe(),
                        secondTraceInfoList.get(0).getDur());

                for (TraceInfo traceInfo : secondTraceInfoList) {
                    traceInfo.setForecast(false);
                    if (traceInfo.getTs().startsWith("F")
                            || traceInfo.getTs().startsWith("C")) {
                        traceInfo.setForecast(true);
                    }

                    graphInfo2.addTraceInfo(traceInfo);
                }
                pageInfo.addGraphInfo(graphInfo2);
            }
        }
        groupInfo.addPageInfo(pageInfo);

        return groupInfo;
    }

    /**
     * Validate the user's selections.
     *
     * @return
     */
    private boolean validateForm() {

        Date beginDate = beginningTimeControl.getSelection().getTime();
        Date endDate = endingTimeControl.getSelection().getTime();
        if (endDate.before(beginDate)) {
            MessageDialog.openWarning(shell, "Invalid Date Selection",
                    "Ending Time is prior to Beginning Time");
            return false;
        }

        /*
         * Check for maximum days allowable for data to be displayed
         */
        long daysmax = (endDate.getTime() - beginDate.getTime())
                / TimeUtil.MILLIS_PER_DAY;
        if (daysmax > DAYS_MAX) {
            String errorMessage = String.format("Time Period exceeds %d days",
                    DAYS_MAX);
            MessageDialog.openWarning(shell, "Invalid Date Selection",
                    errorMessage);
        }

        if (modeCbo.getText().equals(STATION_SELECTION)) {
            if (topDataTable.getSelectionIndex() == -1) {
                MessageDialog.openWarning(shell, "Invalid Selection",
                        "A Location ID is required");
                return false;
            }

            if (bottomDataTable.getSelectionIndex() == -1) {
                MessageDialog.openWarning(shell, "Invalid Selection",
                        "A Physical Element is required");
                return false;
            }
        } else {
            if (groupDataList.getSelectionIndex() == -1) {
                MessageDialog.openWarning(shell, "Invalid Selection",
                        "A Group is required");
                return false;
            }
        }
        return true;
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
            Map<String, ArrayList<SiteInfo>> tsMap = new HashMap<>();
            for (String ts : TS_ORDER) {
                tsMap.put(ts, new ArrayList<SiteInfo>());
            }
            tsMap.put(OTHER, new ArrayList<SiteInfo>());

            java.util.List<SiteInfo> list = null;

            String selectedTs = tsOrderCbo
                    .getItem(tsOrderCbo.getSelectionIndex());

            while (iter.hasNext()) {
                String pe = iter.next();
                list = dataMap.get(pe);

                for (String ts : TS_ORDER) {
                    tsMap.get(ts).clear();
                }

                tsMap.get(OTHER).clear();

                for (SiteInfo si : list) {
                    boolean done = false;
                    for (String ts : TS_ORDER) {
                        if (si.getTs().startsWith(ts)) {
                            tsMap.get(ts).add(si);
                            done = true;
                            break;
                        }
                    }

                    if (!done) {
                        tsMap.get(OTHER).add(si);
                    }
                }

                if (tsSelected) {
                    java.util.List<SiteInfo> siList = tsMap
                            .get(selectedTs.substring(0, 1));
                    java.util.List<SiteInfo> numList = new ArrayList<>();
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

                java.util.List<SiteInfo> numList = new ArrayList<>();
                for (String ts : TS_ORDER) {
                    java.util.List<SiteInfo> siList = tsMap.get(ts);
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
                java.util.List<SiteInfo> siList = tsMap.get(OTHER);
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
        item.setData(si);

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
