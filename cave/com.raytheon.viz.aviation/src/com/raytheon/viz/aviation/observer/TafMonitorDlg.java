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

package com.raytheon.viz.aviation.observer;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXB;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.aviation.BackupRestart;
import com.raytheon.viz.aviation.climatology.ClimateMenuDlg;
import com.raytheon.viz.aviation.climatology.WeatherPlotDialog;
import com.raytheon.viz.aviation.editor.ITafSettable;
import com.raytheon.viz.aviation.editor.TafViewerEditorDlg;
import com.raytheon.viz.aviation.editor.TafViewerEditorDlg.TafSettings;
import com.raytheon.viz.aviation.guidance.PythonGuidanceJob;
import com.raytheon.viz.aviation.guidance.ViewerTab;
import com.raytheon.viz.aviation.monitor.CcfpMonitorObserver;
import com.raytheon.viz.aviation.monitor.CheckNowJob;
import com.raytheon.viz.aviation.monitor.EtaBufMonitorObserver;
import com.raytheon.viz.aviation.monitor.EtaMonitorObserver;
import com.raytheon.viz.aviation.monitor.GfsLampMonitorObserver;
import com.raytheon.viz.aviation.monitor.GfsMonitorObserver;
import com.raytheon.viz.aviation.monitor.IGridDataRetrieveListener;
import com.raytheon.viz.aviation.monitor.LtgMonitorObserver;
import com.raytheon.viz.aviation.monitor.MetarMonitorObserver;
import com.raytheon.viz.aviation.monitor.NotifyAudioManager;
import com.raytheon.viz.aviation.monitor.PythonMonitorJob;
import com.raytheon.viz.aviation.monitor.RltgMonitorObserver;
import com.raytheon.viz.aviation.monitor.ScheduledMonitorTask;
import com.raytheon.viz.aviation.monitor.SiteGridManager;
import com.raytheon.viz.aviation.monitor.TafMonitorObserver;
import com.raytheon.viz.aviation.monitor.TafSiteComp;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;
import com.raytheon.viz.aviation.resource.ResourceEditorDlg;
import com.raytheon.viz.aviation.utility.AlertDialog;
import com.raytheon.viz.aviation.utility.BackupDialog;
import com.raytheon.viz.aviation.utility.IBackupRestart;
import com.raytheon.viz.aviation.utility.TransmissionQueueDlg;
import com.raytheon.viz.aviation.xml.LabelMenu;
import com.raytheon.viz.aviation.xml.MonitorCfg;
import com.raytheon.viz.aviation.xml.TafMonitorCfg;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.IStatusSettable;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * TafMonitorDlg (Terminal Aerodome Forecast Monitor Dialog) class.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Jan 24, 2008  817      grichard     Initial creation.
 * Feb 29, 2008  938      grichard     Subscribed to alerts based on plugins.
 * Mar 25, 2008  934      grichard     Added monitoring rule usage.
 * Apr 07, 2008  934      grichard     Added variable of IStatusSettable.
 * Apr 09, 2008  934      grichard     Added variable of IStatusViewable.
 * Apr 10, 2008  934      grichard     Populated site lists with icaos.
 * Apr 23, 2008  934      grichard     Removed alerting observing code that was
 *                                     migrated.
 * Apr 24, 2008  934      grichard     Retrieve metwatch entities at startup
 *                                     time.
 * May 07, 2008  1121     grichard     Hot fix to upper case current site.
 * May 12, 2008  1119     grichard     Convert 3-letter site to conus 4-letter
 *                                     site.
 * Jun 16, 2008  937      grichard     Improved viewer/editor interaction.
 * Aug 11, 2008  1314     grichard     Used PathManager for pathnames.
 * Aug 11, 2008  1444     grichard     Added persist combo box for metars.
 * Aug 12, 2008  1444     grichard     Added getHoursCbo method.
 * Aug 13, 2008  1444     grichard     Add support for persistence of obs.
 * Aug 28, 2008  1444     grichard     Added force metwatch now checks.
 * Sep 12, 2008  1444     grichard     Accommodate separate message logs.
 * Sep 19, 2008  1444     grichard     Add Taf wx quality check capability.
 * May 06, 2009  1982     grichard     Provide feedback during 'check now'.
 * May 11, 2009  1982     grichard     Added backup/restart monitor feature.
 * May 27, 2009  1982     grichard     Added buffer mos data retrieval.
 * Sep 02, 2009  3027     njensen      Major refactor and cleanup
 * Aug 16, 2010  5340     zhao         GUI resizes to fit all TAF sites
 * Sep 02, 2010  4022     rferrel      Alert highlighting
 * 9/272010      6196     rferrel      Start and end PythonMonitor static queue,
 *                                     and prevent multiple checkNows from being
 *                                     queued.
 * Oct 06, 2010  6009     rferrel      Changes to use product.
 * Oct 27, 2010  7383     rferrel      Save changed blink state in configMgr.
 * Mar 14, 2011  8588     rferrel      Allow monitoring multiple products.
 * Nov 29, 2011  11612    rferrel      Added observers to update viewer tabs.
 * Jul 20, 2012  14570    gzhang/zhao  Added methods for highlighting in TAF
 *                                     viewer
 * Oct 02, 2012  1229     rferrel      Changes to work with non-blocking
 *                                     WeatherPlotDialog.
 * Oct 04, 2012  1229     rferrel      Changes for non-blocking ClimateMenuDlg.
 * Oct 09, 2012  1229     rferrel      Made dialog non-blocking.
 * Oct 10, 2012  1229     rferrel      Changes for non-blocking
 *                                     ResourceEditorDlg.
 * Oct 10, 2012  1229     rferrel      Changes for non-blocking
 *                                     TransmissionQueueDlg.
 * Oct 10, 2012  1229     jkorman      Changes for AlertDialog to support
 *                                     non-blocking.
 * Oct 11, 2012  1229     jkorman      Changes for BackupDialog to support
 *                                     non-blocking.
 * Oct 11, 2012  1229     rferrel      Changes for non-blocking
 *                                     TafViewerEditorDlg.
 * Oct 15, 2012  1229     rferrel      Changes for non-blocking HelpUsageDlg.
 * Nov 28, 2012  1363     rferrel      Dispose of PythonGuidanceJob when
 *                                     closing.
 * Jan 02, 2013  15606    gzhang       Remove GridData widthHint so button/label
 *                                     size change with GUI
 * Mar 07, 2013  1735     rferrel      Performance speed up for retrieving grid
 *                                     data.
 * Aug 09, 2013  2033     mschenke     Switched File.separator to
 *                                     IPathManager.SEPARATOR
 * Sep 15, 2015  4880     njensen      Removed reference to ForecastModel
 * Oct 20, 2015  17445    yteng        Reset alert time for audio alert.
 * May 24, 2017  6119     tgurney      Help text formatting
 * Jan 23, 2018  6692     tgurney      Add prompt arg to closeDisplay()
 * Mar 23, 2018  7111     njensen      Removed obsolete help text
 * Jul 25, 2018  6748     randerso     Fixed to work with changes in
 *                                     CaveSWTDialog.shouldClose(). Code
 *                                     cleanup.
 *
 * </pre>
 *
 * @author grichard
 */
public class TafMonitorDlg extends CaveSWTDialog
        implements IGridDataRetrieveListener {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private List<String> stationList;

    private ScrolledComposite scrolledComp;

    private Composite tafComp;

    private MenuItem blinkMenuItem;

    private Composite tafControlsComp;

    private IStatusSettable msgStatComp;

    /**
     * TAF Viewer/Editor Dialog.
     */
    private ITafSettable tveDlg;

    private WeatherPlotDialog avnPlotDlg;

    private ResourceEditorDlg resDlg;

    private BackupDialog backupDialog;

    private AlertDialog alertDialog;

    private ResourceConfigMgr configMgr;

    private int persistHour = 1;

    private TafMonitorCfg tafMonCfg;

    private List<TafSiteComp> siteRows = new ArrayList<>();

    private TafMonitorObserver tafObserver;

    private MetarMonitorObserver metarObserver;

    private LtgMonitorObserver ltgObserver;

    private CcfpMonitorObserver ccfpObserver;

    private RltgMonitorObserver rltgObserver;

    private ScheduledMonitorTask scheduledCheck;

    private GfsLampMonitorObserver gfslampObserver;

    private GfsMonitorObserver gfsObserver;

    private EtaMonitorObserver etaObserver;

    private EtaBufMonitorObserver etabufObserver;

    private static TafMonitorDlg currentDlg = null;

    private CheckNowJob job;

    private List<String> productDisplayList;

    private ClimateMenuDlg climateMenuDlg;

    private TransmissionQueueDlg tqDlg;

    private HelpUsageDlg usageDlg;

    private boolean promptToClose = true;

    public TafMonitorDlg(Shell parent, List<String> stationList,
            List<String> productDispalyList) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT
                        | CAVE.INDEPENDENT_SHELL | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Monitor");

        this.stationList = stationList;
        this.productDisplayList = productDispalyList;
        synchronized (TafMonitorDlg.class) {
            currentDlg = this;
        }
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        SiteGridManager.clear();
        cleanupMonitoring();
        tveDlg.disposeDialog();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // Initialize data and all of the controls & layouts.
        initializeData();
        initializeComponents();
        setupMonitoring();
        NotifyAudioManager.resetAlertTime();
    }

    @Override
    protected void preOpened() {
        checkNow();
    }

    private void setupMonitoring() {
        job = new CheckNowJob("Checking all data", this);
        PythonMonitorJob.startJobs();
        tafObserver = new TafMonitorObserver(this);
        ProductAlertObserver.addObserver("taf", tafObserver);
        metarObserver = new MetarMonitorObserver(this);
        ProductAlertObserver.addObserver("obs", metarObserver);
        ltgObserver = new LtgMonitorObserver(this);
        ProductAlertObserver.addObserver("binlightning", ltgObserver);
        ccfpObserver = new CcfpMonitorObserver(this);
        ProductAlertObserver.addObserver("ccfp", ccfpObserver);
        rltgObserver = new RltgMonitorObserver(this);
        ProductAlertObserver.addObserver("bufrmosLAMP", rltgObserver);
        gfslampObserver = new GfsLampMonitorObserver(this);
        ProductAlertObserver.addObserver(GfsLampMonitorObserver.pluginName,
                gfslampObserver);
        gfsObserver = new GfsMonitorObserver(this);
        ProductAlertObserver.addObserver(GfsMonitorObserver.pluginName,
                gfsObserver);
        etaObserver = new EtaMonitorObserver(this);
        ProductAlertObserver.addObserver(EtaMonitorObserver.pluginName,
                etaObserver);
        etabufObserver = new EtaBufMonitorObserver(this);
        ProductAlertObserver.addObserver(EtaBufMonitorObserver.pluginName,
                etabufObserver);

        scheduledCheck = new ScheduledMonitorTask(this);
        scheduledCheck.setSystem(true);

        Calendar cal = Calendar.getInstance();
        int minute = cal.get(Calendar.MINUTE);
        int hour = cal.get(Calendar.HOUR_OF_DAY);
        if (minute >= 30) {
            minute = 0;
            hour += 1;
        } else {
            minute = 30;
        }
        cal.set(Calendar.HOUR_OF_DAY, hour);
        cal.set(Calendar.MINUTE, minute);
        cal.set(Calendar.SECOND, 0);
        long delay = cal.getTimeInMillis() - System.currentTimeMillis();
        scheduledCheck.schedule(delay);
    }

    private void cleanupMonitoring() {
        ProductAlertObserver.removeObserver("taf", tafObserver);
        ProductAlertObserver.removeObserver("obs", metarObserver);
        ProductAlertObserver.removeObserver("binlightning", ltgObserver);
        ProductAlertObserver.removeObserver("ccfp", ccfpObserver);
        ProductAlertObserver.removeObserver("bufrmosLAMP", rltgObserver);
        ProductAlertObserver.removeObserver(GfsLampMonitorObserver.pluginName,
                gfslampObserver);
        ProductAlertObserver.removeObserver(GfsMonitorObserver.pluginName,
                gfsObserver);
        ProductAlertObserver.removeObserver(EtaMonitorObserver.pluginName,
                etaObserver);
        ProductAlertObserver.removeObserver(EtaBufMonitorObserver.pluginName,
                etabufObserver);
        PythonMonitorJob.stopJobs();
        scheduledCheck.stop();
        scheduledCheck.cancel();
        job = null;
    }

    private void initializeData() {
        createTafMonitorCfg();
    }

    private void initializeComponents() {

        configMgr = ResourceConfigMgr.getInstance();

        boolean transientDialog = configMgr
                .getDataAsBoolean(ResourceTag.TransientDialogs);

        if (transientDialog) {
            // Parent dialog cannot be displayed on top of this dialog
            tveDlg = new TafViewerEditorDlg(shell, stationList, CAVE.NONE);
        } else {
            // Parent dialog can be displayed on top of this dialog.
            tveDlg = new TafViewerEditorDlg(shell, stationList,
                    CAVE.INDEPENDENT_SHELL);
        }

        createMenus();
        createButtonsComposite();

        createScrolledComposite();
        createScrolledCompositeLabels();
        createBottomMessageControls();
        populateStationsOfInterest();

        scrolledComp.layout();
    }

    /**
     * Create the menus on the display.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenus(menuBar);
        createOptionsMenus(menuBar);
        createHelpMenus(menuBar);

        shell.setMenuBar(menuBar);
    }

    private void createFileMenus(Menu menuBar) {
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("File");

        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        MenuItem checkNowMenuItem = new MenuItem(fileMenu, SWT.NONE);
        checkNowMenuItem.setText("Check Now");
        checkNowMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                checkNow();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        MenuItem restartMenuItem = new MenuItem(fileMenu, SWT.NONE);
        restartMenuItem.setText("Restart");
        restartMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                IBackupRestart brs = BackupRestart.getBackupRestartUtility();
                brs.restartTafMonitor();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        MenuItem quitMenuItem = new MenuItem(fileMenu, SWT.NONE);
        quitMenuItem.setText("Quit");
        quitMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                closeDisplay();
            }
        });
    }

    /**
     * Create the options menus on the display.
     */
    private void createOptionsMenus(Menu menuBar) {
        MenuItem optionsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        optionsMenuItem.setText("Options");

        Menu optionsMenu = new Menu(menuBar);
        optionsMenuItem.setMenu(optionsMenu);

        MenuItem setupMenuItem = new MenuItem(optionsMenu, SWT.NONE);
        setupMenuItem.setText("Setup...");
        setupMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(resDlg)) {
                    resDlg = new ResourceEditorDlg(shell);
                    resDlg.open();
                } else {
                    resDlg.bringToTop();
                }
            }
        });

        MenuItem alertMenuItem = new MenuItem(optionsMenu, SWT.NONE);
        alertMenuItem.setText("Alert...");
        alertMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(alertDialog)) {
                    alertDialog = new AlertDialog(shell);
                    alertDialog.open();
                } else {
                    alertDialog.bringToTop();
                }
            }
        });

        blinkMenuItem = new MenuItem(optionsMenu, SWT.CHECK);
        blinkMenuItem.setText("Blink");
        blinkMenuItem
                .setSelection(configMgr.getDataAsBoolean(ResourceTag.Blink));
        blinkMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean state = blinkMenuItem.getSelection();
                configMgr.setDataAsBoolean(ResourceTag.Blink, state);
                for (TafSiteComp tsc : siteRows) {
                    tsc.setBlinkable(state);
                }
            }

        });
    }

    /**
     * Create the help menus on the display.
     */
    private void createHelpMenus(Menu menuBar) {
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("Help");

        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        MenuItem aboutMenuItem = new MenuItem(helpMenu, SWT.NONE);
        aboutMenuItem.setText("About...");
        aboutMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ActionFactory.ABOUT.create(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow())
                        .run();
            }
        });

        MenuItem usageMenuItem = new MenuItem(helpMenu, SWT.NONE);
        usageMenuItem.setText("Usage...");
        usageMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String descrription = "Usage";

                    String helpText = "This is the main interface to AvnFPS.\n"
                            + "\n"
                            + "All menus and buttons have associated help.  When the mouse cursor is\n"
                            + "placed over context sensitive help's 'hot spot', a 'balloon' message\n"
                            + "will appear.\n" + "\n"
                            + "Successful completion of a task is usually shown in a message window\n"
                            + "at the bottom of the main GUI or dialogs. Important system messages\n"
                            + "will be shown there also.  When an error occurs, a warning/error\n"
                            + "dialog will pop up which requires acknowledgment before one can\n"
                            + "interact further with the application.\n" + "\n"
                            + "Menu options:\n" + "    File:\n"
                            + "        Check Now:  Forces check of all TAFs and transmission\n"
                            + "                    status.\n"
                            + "        Restart:    Restarts the program, using current \n"
                            + "                    configuration\n"
                            + "        Quit:       Terminates the application.\n"
                            + "\n" + "    Options:\n"
                            + "        Setup:      Calls setup configuration dialog which \n"
                            + "                    allows for setting configuration \n"
                            + "                    resources: fonts, colors and values that \n"
                            + "                    affect program behavior.\n"
                            + "        Alert:      Used to select alert criteria 'on-the-fly' \n"
                            + "                    when the program detects a condition \n"
                            + "                    requiring forecaster's action.\n"
                            + "        Blink:      If selected, station id button will blink\n"
                            + "                    when new notification arrives\n"
                            + "    Help:\n"
                            + "        Used to provide version number and location of AvnFPS\n"
                            + "        documentation web sites and this help window.\n"
                            + "\n" + "TAF Editor: Starts TAF editor\n"
                            + "Climate:    Displays Climate GUI\n"
                            + "Plot:       Displays Weather Plot GUI\n"
                            + "Backup:     Invokes list dialog allowing selection of products to \n"
                            + "            monitor.\n" + "\n"
                            + "Queue:  Background color indicates whether last issued forecast was \n"
                            + "        successfully transmitted. The button invokes transmission queue\n"
                            + "        control dialog.\n" + "\n"
                            + "Product monitoring window consists of the following units:\n"
                            + "\n"
                            + "Site Id button: used to invoke TAF editor. Its background color is used \n"
                            + "    to indicate problem with data. A new alert will cause the button \n"
                            + "    to blink. Press right mouse button to stop blinking.\n"
                            + "    \n"
                            + "Last TAF and METAR time labels: those display issue time. When either\n"
                            + "    one is late, the corresponding label is highlighted. If there \n"
                            + "    is no TAF, or TAF is older than 24 hours, time is set to None \n"
                            + "    for both TAF and MTR. \n" + "\n"
                            + "For each monitored data source there is a set of labels indicating \n"
                            + "whether a particular weather element is in agreement with the forecast.\n"
                            + "\n"
                            + "The following data sources are currently available:\n"
                            + "\n"
                            + "Current Observation: Most recent observation\n"
                            + "Nhr Persistence: Most recent observation compared to forecast N hours \n"
                            + "                 ahead\n"
                            + "ltg:             Real-time CG lightning strikes\n"
                            + "rltg:            Radar-based 3 hour lightning probability forecast\n"
                            + "NDFD Grids:      GFE generated grids\n"
                            + "llws:            Low Level Wind Shear, based on METAR and radars',\n"
                            + "                 profilers' or aircrafts' vertical wind profile data\n"
                            + "ccfp:            Collaborative Convective Forecast Product from AWC\n"
                            + "\n"
                            + "Depending on configuration, some of the above can be accessed through \n"
                            + "popup menus associated with the data source heading labels. Use right \n"
                            + "mouse button to display the menu. Not all labels have an associated \n"
                            + "menu.\n" + "\n"
                            + "By pointing mouse cursor at a particular data source you will get \n"
                            + "the forecast, that data values and list of violated rules, if any,\n"
                            + "displayed in a balloon message.\n" + "\n"
                            + "Optional shortcut buttons to the TAF Editor.\n"
                            + "Amd:    call TAF editor initialized for amended TAF for selected site.\n"
                            + "Rtd:    call TAF editor initialized for delayed TAF for selected site.\n"
                            + "Cor:    call TAF editor initialized for corrected TAF for selected site.";
                    usageDlg = new HelpUsageDlg(shell, descrription, helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    public synchronized void checkNow() {
        // Only allow one check to be scheduled.
        if (job.getState() == Job.NONE) {
            job.setSystem(true);
            job.schedule();
        }
    }

    /**
     * Create the buttons composite on the display.
     */
    private void createButtonsComposite() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonsComposite = new Composite(shell, SWT.BORDER);
        GridLayout layoutTC = new GridLayout(5, true);
        buttonsComposite.setLayout(layoutTC);
        buttonsComposite.setLayoutData(gd);
        configMgr.setDefaultColors(buttonsComposite);

        Button tafEditorBtn = new Button(buttonsComposite, SWT.PUSH);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.minimumWidth = 100;
        configMgr.setDefaultFontAndColors(tafEditorBtn, "TAF Editor", gd);
        tafEditorBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tveDlg.updateSettings(TafSettings.OPEN_EDIT, null);
                tveDlg.showDialog();
            }
        });

        Button climBtn = new Button(buttonsComposite, SWT.PUSH);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        climBtn.setText("Climate");
        configMgr.setDefaultFontAndColors(climBtn, "Climate", gd);
        climBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                StatusMessageType[] msgTypes = new StatusMessageType[4];
                msgTypes[0] = StatusMessageType.TafMonMetar;
                msgTypes[1] = StatusMessageType.TafMonWindRose;
                msgTypes[2] = StatusMessageType.TafMonCigVis;
                msgTypes[3] = StatusMessageType.TafMonCigVisTrend;

                if (mustCreate(climateMenuDlg)) {
                    climateMenuDlg = new ClimateMenuDlg(shell, msgTypes,
                            configMgr.getDefaultBackgroundRGB());
                    climateMenuDlg.open();
                } else {
                    climateMenuDlg.bringToTop();
                }
            }
        });

        Button plotBtn = new Button(buttonsComposite, SWT.PUSH);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.minimumWidth = 100;
        configMgr.setDefaultFontAndColors(plotBtn, "Plot", gd);
        plotBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                if (mustCreate(avnPlotDlg)) {
                    avnPlotDlg = new WeatherPlotDialog(shell,
                            StatusMessageType.WeatherPlot, stationList);
                    avnPlotDlg.addCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            avnPlotDlg = null;
                        }
                    });
                    avnPlotDlg.open();
                } else {
                    avnPlotDlg.bringToTop();
                }
            }
        });

        Button backupBtn = new Button(buttonsComposite, SWT.PUSH);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.minimumWidth = 100;
        configMgr.setDefaultFontAndColors(backupBtn, "Backup", gd);
        backupBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(backupDialog)) {
                    backupDialog = new BackupDialog(shell, productDisplayList);
                    backupDialog.open();
                } else {
                    backupDialog.bringToTop();
                }
            }
        });

        Button queueBtn = new Button(buttonsComposite, SWT.FLAT);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.minimumWidth = 100;
        configMgr.setDefaultFontAndColors(queueBtn, "Queue", gd);
        queueBtn.setBackground(getDisplay().getSystemColor(SWT.COLOR_GREEN));
        queueBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(tqDlg)) {
                    tqDlg = new TransmissionQueueDlg(shell);
                    tqDlg.open();
                } else {
                    tqDlg.bringToTop();
                }
            }
        });
    }

    private void createScrolledComposite() {
        scrolledComp = new ScrolledComposite(shell,
                SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        scrolledComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        scrolledComp.setLayoutData(gd);
        configMgr.setDefaultColors(scrolledComp);

        gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        tafComp = new Composite(scrolledComp, SWT.NONE);
        tafComp.setLayout(gl);
        tafComp.layout();
        configMgr.setDefaultColors(tafComp);

        /*
         * Create the composite that will contain the TAF labels and controls
         */

        // Calculate the number of cells needed. The "4" that is added to the
        // monitor number
        // accounts for the 3 cells (site button, check box, and the TAF/MTR
        // time) and the
        // editor shortcuts.
        int monitorNum = tafMonCfg.getMonitorCfgs().size();
        int totalLabelCells = monitorNum + 4;

        gl = new GridLayout(totalLabelCells, false);
        gl.verticalSpacing = 1;
        tafControlsComp = new Composite(tafComp, SWT.NONE);
        tafControlsComp.setLayout(gl);
        configMgr.setDefaultColors(tafControlsComp);

        scrolledComp.setContent(tafComp);
        scrolledComp.setExpandHorizontal(true);
        scrolledComp.setExpandVertical(true);

        scrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                scrolledComp.setMinSize(
                        tafComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
            }
        });

        scrolledComp.layout();
    }

    private void createScrolledCompositeLabels() {

        GridData gd;

        /*
         * Create filler labels
         */
        new Label(tafControlsComp, SWT.NONE);
        new Label(tafControlsComp, SWT.NONE);
        new Label(tafControlsComp, SWT.NONE);

        /*
         * Create the labels and any pop-up menus
         */
        List<MonitorCfg> monCfgs = tafMonCfg.getMonitorCfgs();

        for (final MonitorCfg monCfg : monCfgs) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            final Label lbl = new Label(tafControlsComp, SWT.CENTER);
            lbl.setText(monCfg.getLabelName());
            lbl.setLayoutData(gd);
            configMgr.setDefaultFontAndColors(lbl);

            if (monCfg.getLabelMenus() != null) {
                Menu menu = new Menu(lbl);

                List<LabelMenu> lblMenus = monCfg.getLabelMenus();
                for (LabelMenu lblMenu : lblMenus) {
                    MenuItem persistMI = new MenuItem(menu, SWT.NONE);
                    persistMI.setText(lblMenu.getMenuName());
                    persistMI.setData(
                            new Integer(Integer.valueOf(lblMenu.getValue())));
                    persistMI.addSelectionListener(new SelectionAdapter() {
                        @Override
                        public void widgetSelected(SelectionEvent e) {
                            MenuItem mi = (MenuItem) e.getSource();
                            persistHour = ((Integer) mi.getData()).intValue();
                            lbl.setText(mi.getText());
                            for (TafSiteComp row : siteRows) {
                                row.columnValueChanged(monCfg.getClassName(),
                                        "nhours", persistHour);
                            }
                        }
                    });
                }

                lbl.setMenu(menu);
            }
        }

        boolean showAmdButtons = configMgr
                .getResourceAsBoolean(ResourceTag.AmdButtons);

        if (showAmdButtons) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            Label editorLbl = new Label(tafControlsComp, SWT.CENTER);
            editorLbl.setText("Editor Shortcuts");
            editorLbl.setLayoutData(gd);
            configMgr.setDefaultFontAndColors(editorLbl);
        } else {
            // "Invisible" label
            new Label(tafControlsComp, SWT.NONE);
        }
    }

    private void createBottomMessageControls() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        msgStatComp = new MessageStatusComp(shell, StatusMessageType.TafMonitor,
                configMgr.getDefaultBackgroundRGB(),
                configMgr.getMsgBarBackground());
    }

    @Override
    public boolean shouldClose() {
        if (promptToClose) {
            MessageBox confirmCloseMB = new MessageBox(shell,
                    SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            confirmCloseMB.setText("Confirm Close");
            confirmCloseMB
                    .setMessage("Do you want to close the AvnFPS Monitor?");
            int result = confirmCloseMB.open();

            if (result == SWT.NO) {
                return false;
            }
        }

        tveDlg.disposeDialog();
        PythonGuidanceJob.dispose();
        SiteGridManager.removeRetrieveDataListener(this);
        SiteGridManager.clear();
        return true;
    }

    public boolean closeDisplay() {
        return closeDisplay(true);
    }

    public boolean closeDisplay(boolean prompt) {
        promptToClose = prompt
                && configMgr.getResourceAsBoolean(ResourceTag.ConfirmClose);
        return close();
    }

    private void populateStationsOfInterest() {
        // For all stations of interest to this site:
        // (1) JiBX the monitoring rules and produce a map based on ICAO
        // containing the monitoring rules.
        // (2) add buttons to the TAF Monitor that contain each of the
        // identifiers of the stations of interest to this site.
        // (3) add MetWatch listener for TAFs. New this up with `this' and
        // propagate to simple MetWatch factory to use as IMonitorable
        // interface.
        SiteGridManager.removeRetrieveDataListener(this);
        SiteGridManager.clear();
        SiteGridManager.addSiteIDs(stationList);
        if (stationList != null) {
            for (String stationOfInterest : stationList) {
                if (stationOfInterest != null) {

                    addHorizontalSeparator(tafControlsComp);

                    TafSiteComp tsc = new TafSiteComp(tafControlsComp,
                            stationOfInterest, tveDlg, tafMonCfg, msgStatComp);
                    tsc.setBlinkable(blinkMenuItem.getSelection());
                    siteRows.add(tsc);
                }
            }

            if (!siteRows.isEmpty() && siteRows.get(0).doGridMonitor()) {
                // Only add listener when displaying grid data.
                SiteGridManager.addRetrieveDataListener(this);
            }
        }

    }

    private void addHorizontalSeparator(Composite parentComp) {
        int cols = ((GridLayout) parentComp.getLayout()).numColumns;

        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = cols;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
        configMgr.setDefaultColors(sepLbl);
    }

    private void createTafMonitorCfg() {
        String fs = IPathManager.SEPARATOR;
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile("aviation" + fs + "config" + fs + "gui"
                    + fs + "TafMonitorCfg.xml");
            tafMonCfg = JAXB.unmarshal(path, TafMonitorCfg.class);
        } catch (Exception e) {
            statusHandler.error("Failed to load TafMonitorCfg.xml", e);
        }
    }

    public void showDialog() {
        shell.setVisible(true);
        shell.setFocus();
    }

    public static Map<String, String[]> getCurrentAlertMap(String siteID) {
        Map<String, String[]> alertMap = null;
        synchronized (TafMonitorDlg.class) {
            if (currentDlg != null) {
                if (currentDlg.getDisplay().isDisposed()) {
                    currentDlg = null;
                } else {
                    for (TafSiteComp siteRow : currentDlg.getTafSiteComps()) {
                        if (siteRow.getStationName().equals(siteID)) {
                            alertMap = siteRow.getAlertMap();
                        }
                    }
                }
            }
        }
        return alertMap;
    }

    public List<TafSiteComp> getTafSiteComps() {
        return siteRows;
    }

    public IStatusSettable getMessageBar() {
        return msgStatComp;
    }

    /**
     * Obtain list of active Viewer Tabs.
     *
     * @return viewerTabList
     */
    public final List<ViewerTab> getViewerTabList() {
        return tveDlg.getViewerTabList();
    }

    // ------------------------- DR 14570:

    public static Map<String, String> getCurrentAlertTimeMap(String siteID) {
        Map<String, String> alertTimeMap = null;
        synchronized (TafMonitorDlg.class) {
            if (currentDlg != null) {
                if (currentDlg.getDisplay().isDisposed()) {
                    currentDlg = null;
                } else {
                    for (TafSiteComp siteRow : currentDlg.getTafSiteComps()) {
                        if (siteRow.getStationName().equals(siteID)) {
                            alertTimeMap = siteRow.getAlertTimeMap();
                        }
                    }
                }
            }
        }
        return alertTimeMap;
    }

    // 20120711
    public static Map<String, String[]> getCurrentTempoMap(String siteID) {
        Map<String, String[]> tempoMap = null;
        synchronized (TafMonitorDlg.class) {
            if (currentDlg != null) {
                if (currentDlg.getDisplay().isDisposed()) {
                    currentDlg = null;
                } else {
                    for (TafSiteComp siteRow : currentDlg.getTafSiteComps()) {
                        if (siteRow.getStationName().equals(siteID)) {
                            tempoMap = siteRow.getTempoMap();
                        }
                    }
                }
            }
        }
        return tempoMap;
    }

    @Override
    public void gridDataRetrieved() {
        for (TafSiteComp tsc : siteRows) {
            tsc.fireMonitor(TafSiteComp.GRID_MONITOR_CLASS);
        }
    }
}
