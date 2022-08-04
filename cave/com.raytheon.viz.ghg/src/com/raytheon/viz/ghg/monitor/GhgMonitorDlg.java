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
package com.raytheon.viz.ghg.monitor;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.TreeSet;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.VTECChange;
import com.raytheon.uf.common.activetable.VTECTableChangeNotification;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.gfe.ifpclient.IFPClient;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.ghg.exception.GhgMissingDataException;
import com.raytheon.viz.ghg.monitor.GHGSpatialViewer.ZoomLevel;
import com.raytheon.viz.ghg.monitor.data.GhgAlertCheckData;
import com.raytheon.viz.ghg.monitor.data.GhgAlertData;
import com.raytheon.viz.ghg.monitor.data.GhgAlertsConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgColorData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.AlertsEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.DataEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.FeatureEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.GhgFontSizeEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.SelectionEnum;
import com.raytheon.viz.ghg.monitor.data.GhgData;
import com.raytheon.viz.ghg.monitor.data.GhgDataFilter;
import com.raytheon.viz.ghg.monitor.event.AbstractGhgMonitorEvent;
import com.raytheon.viz.ghg.monitor.event.AbstractGhgMonitorEvent.GhgEventListener;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorFilterChangeEvent;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorTableSelectionEvent;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorZoneSelectionEvent;
import com.raytheon.viz.ghg.monitor.filter.GhgFilterEngine;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.statusline.EdgeLayout;
import com.raytheon.viz.ui.statusline.EdgeLayout.EdgeLayoutData;
import com.raytheon.viz.ui.statusline.EdgeLayout.EdgeLayoutData.EdgeAffinity;
import com.raytheon.viz.ui.statusline.MessageImportance;
import com.raytheon.viz.ui.statusline.StatBar;
import com.raytheon.viz.ui.statusline.StatusMessage;
import com.raytheon.viz.ui.statusline.StatusMessage.Importance;
import com.raytheon.viz.ui.statusline.StatusStore;

/**
 * This class displays the GHG Monitor Dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 25, 2008  N/A      lvenable  Initial creation
 * Jun 17, 2008  1157     MW Fegan  Pass configuration to sub-dialogs.
 * Nov 15, 2012  1298     rferrel   Changes for non-blocking dialog. Changes for
 *                                  non-blocking GhgAlertDlg.
 * Dec 03, 2012  1353     rferrel   Changes for non-blocking GhgFilterDlg.
 *                                  Changes for non-blocking
 *                                  GhgSaveDeleteFilterDlg.
 * Jan 16, 2013  1492     rferrel   Changes for non-blocking GhgFontDlg.
 * Mar 29, 2013  1790     rferrel   Bug fix for non-blocking dialogs.
 * Apr 10, 2014  15769    ryu       Modify default configuration and menus to
 *                                  match A1. Bring monitor to front before
 *                                  sending alert. Adjusted delay for timer so
 *                                  it fires at the top of a minute.
 * Dec 16, 2015  5184     dgilling  Remove viz.gfe dependencies.
 * Feb 05, 2016  5316     randerso  Moved notification registration into
 *                                  GHGMonitorDlg Switched to use
 *                                  GhgSpatialViewer
 * Mar 03, 2016  5316     randerso  Added code to keep Maps menu in sync with
 *                                  map changes
 * Aug 02, 2016  5790     dgilling  Properly consolidate segments.
 * Nov 09, 2016  5995     tgurney   Add maximize and minimize buttons
 * Feb 13, 2017  6118     mapeters  Re-layout status bar when filter text
 *                                  changes, use same font for all of status bar
 * Sep 03, 2019  7919     randerso  Call mapTab.setControl before creating
 *                                  controls on mapComp. This seems to avoid
 *                                  some issues with control parenting in
 *                                  Eclipse 4.10. Code cleanup.
 * Nov 14, 2019  7919     randerso  Significantly reworked GhgTableComp
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class GhgMonitorDlg extends CaveSWTDialog
        implements INotificationObserver, GhgEventListener {
    private static final String HAZARDS_PARM_NAME = "Hazards_SFC";

    private static final String DEFAULT_MAP = "Public";

    private static final ZoomLevel DEFAULT_ZOOM = GHGSpatialViewer.ZoomLevel.ZOOM_1;

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GhgMonitorDlg.class);

    private static final Map<String, GhgConfigData.DataEnum> labelToEnumMap;

    private ActiveTableMode activeTableName;

    private final IFPClient ifpClient;

    private DiscreteDefinition discreteDef;

    private String myWFO;

    private String my4WFO;

    private GridLocation gridLocation;

    private GhgFilterEngine filterEngine;

    private GhgAlertDlg alertDlg;

    private GhgColorDlg colorDlg;

    private GhgFilterDlg filterDlg;

    private GhgFontDlg fontDlg;

    private GhgSaveDeleteFilterDlg deleteFilterDlg;

    private GhgSaveDeleteFilterDlg saveFilterDlg;

    /**
     * Active group one string.
     */
    private static final List<String> ACT_GROUP_ONE = Arrays.asList("CON",
            "EXA", "EXB", "EXT", "NEW");

    /**
     * Active group two string.
     */
    private static final List<String> ACT_GROUP_TWO = Arrays.asList("CAN",
            "EXP", "UPG");

    /**
     * Default timeout value.
     */
    private static final long DEFAULT_TIMEOUT = 30 * TimeUtil.MILLIS_PER_SECOND;

    /**
     * Status bar definition key.
     */
    private static final String STATUS_KEY = com.raytheon.viz.ghg.constants.StatusConstants.CATEGORY_GHG;

    /**
     * A linked hash map of all the column names and a boolean value indicating
     * if the column should be visible or not.
     */
    private LinkedHashMap<String, Boolean> columnsMap;

    /**
     * Filter menu containing filter menu items.
     */
    private Menu filterMenu;

    /**
     * Composite class containing the GHG table.
     */
    private GhgTableComp ghgTableComp;

    /**
     * Composite class containing the GHG text display
     */
    private GhgTextComp ghgTextComp;

    /**
     * Composite class containing the GHG Map Display
     */
    protected GHGSpatialViewer ghgSpatialViewer;

    /**
     * Sash Form used to adjust the area a composite takes up.
     */
    private SashForm sashForm;

    private FilterDisplay filterDisplay;

    private Menu columnsMenu;

    private MenuItem identifyTestMI;

    private List<GhgData> dataList = new ArrayList<>();

    /**
     * Data in the table.
     */
    private List<GhgData> tableData = null;

    /**
     * Timer for auto-updating the dialog
     */
    private Timer timer = null;

    private List<GhgData> filteredTable;

    private boolean refreshing;

    // Initialize labelToEnumMap when the class is first loaded
    static {
        GhgConfigData.DataEnum[] deVals = GhgConfigData.DataEnum.values();
        Map<String, GhgConfigData.DataEnum> temp = new HashMap<>(deVals.length);
        for (GhgConfigData.DataEnum val : deVals) {
            temp.put(val.columnLabel, val);
        }

        temp = Collections.unmodifiableMap(temp);
        labelToEnumMap = temp;
    }

    /**
     * Constructor.
     *
     * @param parent
     *            Parent Shell.
     * @throws GhgMissingDataException
     */
    public GhgMonitorDlg(Shell parent) throws GhgMissingDataException {
        super(parent, SWT.MAX | SWT.MIN | SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.INDEPENDENT_SHELL | CAVE.PERSPECTIVE_INDEPENDENT
                        | CAVE.DO_NOT_BLOCK);

        CAVEMode opMode = CAVEMode.getMode();
        activeTableName = (opMode == CAVEMode.PRACTICE)
                ? ActiveTableMode.PRACTICE
                : ActiveTableMode.OPERATIONAL;

        // connection to ifpServer
        this.ifpClient = connectToIFPServer(
                LocalizationManager.getInstance().getSite());

        /*
         * Using DiscreteDefinition in place of access to VTECTable to get
         * Hazard Description
         */
        ServerResponse<DiscreteDefinition> sr = this.ifpClient
                .getDiscreteDefinition();
        if (sr.isOkay()) {
            this.discreteDef = sr.getPayload();
        } else {
            throw new GhgMissingDataException(String.format(
                    "Unable to retrieve DiscreteDefinition: %s", sr.message()));
        }

        this.filterEngine = new GhgFilterEngine(my4WFO);

        // setup for receiving notification messages from ifpServer
        NotificationManagerJob.addObserver("edex.alerts.vtec", this);
    }

    private IFPClient connectToIFPServer(String site)
            throws GhgMissingDataException {
        IFPClient ifpClient = new IFPClient(VizApp.getWsId(), site);

        ServerResponse<String> sr2 = ifpClient.getSiteID();
        if (sr2.isOkay()) {
            this.myWFO = sr2.getPayload();
            this.my4WFO = SiteMap.getInstance().getSite4LetterId(this.myWFO);
        } else {
            throw new GhgMissingDataException(String
                    .format("Unable to retrieve site ID: %s", sr2.message()));
        }

        ServerResponse<GridLocation> sr3 = ifpClient.getDBGridLocation();
        if (sr3.isOkay()) {
            this.gridLocation = sr3.getPayload();
        } else {
            throw new GhgMissingDataException(String.format(
                    "Unable to retrieve GridLocation: %s", sr3.message()));
        }

        return ifpClient;
    }

    /**
     * Initialize the dialog's configuration
     */
    private void initializeConfiguration() {
        // Load the default configuration file.
        // If this fails, fall back to the hardcoded defaults.
        GhgConfigData configuration = GhgConfigData.buildInstance();

        try {
            // Try and read a saved config file
            configuration.load(false);
        } catch (Exception e) {
            statusHandler.handle(Priority.VERBOSE,
                    "User configuration did not load", e);
        }

        populateColumnsMap();
    }

    @Override
    protected void disposed() {
        // Remove the listener from the list

        ghgTableComp.removeSelectionListener(this);
        ghgSpatialViewer.removeSelectionListener(this);
        GhgConfigData.getInstance().removeFilterChangeListener(this);

        NotificationManagerJob.removeObserver("edex.alerts.vtec", this);

        if (timer != null) {
            timer.cancel();
            timer = null;
        }

    }

    /**
     * Initialize the controls on the display.
     */
    @Override
    protected void initializeComponents(final Shell shell) {
        getParent().addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                shell.dispose();
            }
        });

        // Read in config files
        initializeConfiguration();

        CAVEMode mode = CAVEMode.getMode();
        String activeTableName = (!mode.equals(CAVEMode.PRACTICE)) ? "active"
                : "PRACTICE";
        String practiceText = (!mode.equals(CAVEMode.PRACTICE)) ? ""
                : "     [PRACTICE MODE]";

        // Set up the title bar text
        String userId = System.getProperty("user.name");
        shell.setText("GHG Hazards Monitor: (" + userId + " - "
                + activeTableName + ")" + practiceText);

        GridLayout mainLayout = (GridLayout) shell.getLayout();
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;

        // -----------------------------------------------------
        // Create the SashForm that will be used to manually
        // control the amount of space the top and bottom
        // composites occupy.
        // -----------------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite sashComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 0;
        sashComp.setLayout(gl);
        sashComp.setLayoutData(gd);

        sashForm = new SashForm(sashComp, SWT.VERTICAL);
        sashForm.setLayout(new GridLayout(1, false));
        sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        sashForm.SASH_WIDTH = 10;

        createMainControls(shell);

        sashForm.setWeights(new int[] { 70, 30, 5 });

        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
        createMenus(shell);

        // Set up timer to check for alerting every minute
        initTimer();
    }

    /**
     * Create the main menu bar and menu items.
     *
     * @param shell
     */
    private void createMenus(Shell shell) {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createColumnsMenu(menuBar);
        createFilterMenu(menuBar);
        createAlertsMenu(menuBar);
        createMapMenu(menuBar);
        createAppearanceMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the File menu.
     *
     * @param menuBar
     *            Menu bar.
     */
    private void createFileMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // -------------------------------------------------
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        // Save Configuration menu item
        MenuItem saveConfigMI = new MenuItem(fileMenu, SWT.NONE);
        saveConfigMI.setText("&Save Configuration");
        saveConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                putColumnOrderInConfig();
                putSortingInConfig();

                GhgConfigData.getInstance().save();
                StatusStore.getStatusStore(STATUS_KEY)
                        .addMessage("Saved configuration", Importance.REGULAR);
            }
        });

        // Load Configuration menu item
        MenuItem loadConfigMI = new MenuItem(fileMenu, SWT.NONE);
        loadConfigMI.setText("&Load Configuration");
        loadConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgConfigData.getInstance().load(true);
                applyConfiguration();
            }
        });

        // Default Configuration menu item
        MenuItem defaultConfigMI = new MenuItem(fileMenu, SWT.NONE);
        defaultConfigMI.setText("&Default Configuration");
        defaultConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgConfigData.getInstance().loadDefault();
                applyConfiguration();
            }
        });

        // Menu Separator
        new MenuItem(fileMenu, SWT.SEPARATOR);

        // Exit menu item
        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("&Exit");
        exitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Create the Columns menu.
     *
     * @param menuBar
     *            Menu bar.
     */
    private void createColumnsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Columns menu
        // -------------------------------------
        MenuItem columnsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        columnsMenuItem.setText("&Columns");

        columnsMenu = new Menu(menuBar);
        columnsMenuItem.setMenu(columnsMenu);

        // -----------------------------------------------------
        // Create all the items in the Columns dropdown menu
        // -----------------------------------------------------

        Set<String> keys = columnsMap.keySet();

        for (String key : keys) {
            MenuItem columnsMI = new MenuItem(columnsMenu, SWT.CHECK);
            columnsMI.setText(key);
            columnsMI.setSelection(columnsMap.get(key));
            columnsMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    updateTableColumns(event);
                }
            });
        }

        // Menu Separator
        new MenuItem(columnsMenu, SWT.SEPARATOR);

        // Default menu item
        MenuItem defaultMI = new MenuItem(columnsMenu, SWT.NONE);
        defaultMI.setText("Default");
        defaultMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgConfigData config = GhgConfigData.getInstance();
                config.setDefaultAsCurrent(FeatureEnum.COLUMNS);
                synchColumnsWithConfig();
                // Update the GUI
                ghgTableComp.setVisibleColumns(config.getVisibleColumns());
            }
        });
    }

    /**
     * Create the Filter menu.
     *
     * @param menuBar
     *            Menu bar.
     */
    private void createFilterMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Filter menu
        // -------------------------------------
        MenuItem filterMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        filterMenuItem.setText("&Filter");

        // Create the Filter menu item with a File "dropdown" menu
        filterMenu = new Menu(menuBar);
        filterMenuItem.setMenu(filterMenu);

        // -----------------------------------------------------
        // Create all the items in the Filter dropdown menu
        // -----------------------------------------------------

        updateFilterMenu();
    }

    /**
     * Create the Alerts menu.
     *
     * @param menuBar
     *            Menu bar.
     */
    private void createAlertsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Alerts menu
        // -------------------------------------
        MenuItem alertsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        alertsMenuItem.setText("&Alerts");

        // Create the Alerts menu item with a File "dropdown" menu
        Menu alertsMenu = new Menu(menuBar);
        alertsMenuItem.setMenu(alertsMenu);

        // -----------------------------------------------------
        // Create all the items in the Alerts dropdown menu
        // -----------------------------------------------------

        // Define Alerts menu item
        MenuItem defineAlertsMI = new MenuItem(alertsMenu, SWT.NONE);
        defineAlertsMI.setText("Define Alerts...");
        defineAlertsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showDefineAlertsDialog();
            }
        });

        // Default Filter menu item
        MenuItem defaultFilterMI = new MenuItem(alertsMenu, SWT.NONE);
        defaultFilterMI.setText("Default Filter");
        defaultFilterMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                restoreDefaultAlerts();
            }
        });
    }

    private void restoreDefaultAlerts() {
        GhgConfigData.getInstance().setDefaultAsCurrent(FeatureEnum.ALERTS);
        refresh(false);
    }

    /**
     * Create the Map menu.
     *
     * @param menuBar
     *            Menu bar.
     */
    private void createMapMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Map menu
        // -------------------------------------
        MenuItem mapMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        mapMenuItem.setText("&Map");

        // Create the Alerts menu item with a Map "dropdown" menu
        final Menu mapMenu = new Menu(menuBar);
        mapMenuItem.setMenu(mapMenu);

        // -----------------------------------------------------
        // Create all the items in the Map dropdown menu
        // -----------------------------------------------------
        for (String map : ghgSpatialViewer.knownMaps()) {
            MenuItem item = new MenuItem(mapMenu, SWT.RADIO);
            item.setData(map);
            item.setText("Show " + map);
            item.setSelection(DEFAULT_MAP.equals(map));
            item.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    MenuItem item = (MenuItem) e.widget;
                    if (item.getSelection()) {
                        showMap((String) item.getData());
                    }
                }
            });
        }

        // Menu Separator
        new MenuItem(mapMenu, SWT.SEPARATOR);

        for (ZoomLevel zoom : ZoomLevel.values()) {
            MenuItem item = new MenuItem(mapMenu, SWT.RADIO);
            item.setText(zoom.toString());
            item.setData(zoom);
            item.setSelection(DEFAULT_ZOOM.equals(zoom));
            item.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    MenuItem item = (MenuItem) e.widget;
                    if (item.getSelection()) {
                        zoomMap((ZoomLevel) item.getData());
                    }
                }
            });
        }

        // Menu Separator
        new MenuItem(mapMenu, SWT.SEPARATOR);

        // Show Labels menu item
        final MenuItem showLabelsMI = new MenuItem(mapMenu, SWT.CHECK);
        showLabelsMI.setText("Show Labels");
        showLabelsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                MenuItem item = (MenuItem) e.widget;
                ghgSpatialViewer.setLabelZones(item.getSelection());
            }
        });

        mapMenu.addMenuListener(new MenuAdapter() {

            @Override
            public void menuShown(MenuEvent e) {
                String mapName = ghgSpatialViewer.getCurrentMap();
                int zoomLevel = (int) Math
                        .round(1.0 / ghgSpatialViewer.getZoomLevel());
                for (MenuItem item : mapMenu.getItems()) {
                    Object data = item.getData();
                    if (data instanceof String) {
                        item.setSelection(mapName.equals(data));
                    } else if (data instanceof ZoomLevel) {
                        item.setSelection(
                                zoomLevel == ((ZoomLevel) data).getZoomLevel());
                    } else {
                        item.setSelection(ghgSpatialViewer.isLabelZones());
                    }
                }
            }

        });
    }

    private void showMap(String mapName) {
        ghgSpatialViewer.setMap(mapName);
        clearSelections();
    }

    private void zoomMap(ZoomLevel zoom) {
        ghgSpatialViewer.setZoomLevel(1.0 / zoom.getZoomLevel());
    }

    /**
     * Create the Appearance menu.
     *
     * @param menuBar
     *            Menu bar.
     */
    private void createAppearanceMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Appearance menu
        // -------------------------------------
        MenuItem appearanceMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        appearanceMenuItem.setText("A&ppearance");

        // Create the Appearance menu item with a Map "dropdown" menu
        Menu appearanceMenu = new Menu(menuBar);
        appearanceMenuItem.setMenu(appearanceMenu);

        // -----------------------------------------------------
        // Create all the items in the Appearance dropdown menu
        // -----------------------------------------------------

        // Font menu item
        MenuItem fontMI = new MenuItem(appearanceMenu, SWT.NONE);
        fontMI.setText("Font...");
        fontMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showFontDialog();
            }
        });

        // Color menu item
        MenuItem colorMI = new MenuItem(appearanceMenu, SWT.NONE);
        colorMI.setText("Colors...");
        colorMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showColorDialog();
            }
        });

        // Identify TEST Events menu item
        identifyTestMI = new MenuItem(appearanceMenu, SWT.CHECK);
        identifyTestMI.setText("Identify TEST Events");
        identifyTestMI.setSelection(
                GhgConfigData.getInstance().isIdentifyTestEvents());
        identifyTestMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgConfigData.getInstance()
                        .setIdentifyTestEvents(identifyTestMI.getSelection());
            }
        });
    }

    /**
     * Update the Filter menu with the existing menu items and the updated
     * saved/deleted configurations.
     */
    private void updateFilterMenu() {
        if (filterMenu.getItemCount() > 0) {
            for (int i = filterMenu.getItemCount() - 1; i >= 0; --i) {
                filterMenu.getItem(i).dispose();
            }
        }

        // Define Filter menu item
        MenuItem defineFilterMI = new MenuItem(filterMenu, SWT.NONE);
        defineFilterMI.setText("Define Filter...");
        defineFilterMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showFilterDialog();
                ghgSpatialViewer.refresh();
            }
        });

        // Default Filter menu item
        MenuItem defaultFilterMI = new MenuItem(filterMenu, SWT.NONE);
        defaultFilterMI.setText("Default Filter");
        defaultFilterMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectDefaultFilter();
            }
        });

        // Menu Separator
        new MenuItem(filterMenu, SWT.SEPARATOR);
        // create the Menu list of saved filters
        for (String name : GhgConfigData.getInstance().getFilterNames()) {
            final MenuItem tmpMI = new MenuItem(filterMenu, SWT.NONE);
            tmpMI.setText(name);
            tmpMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    updateCurrentFilter(tmpMI.getText());
                }
            });
        }

        // Menu Separator
        new MenuItem(filterMenu, SWT.SEPARATOR);

        // Save Current Filter menu item
        MenuItem saveCurrentFilterMI = new MenuItem(filterMenu, SWT.NONE);
        saveCurrentFilterMI.setText("Save Current Filter...");
        saveCurrentFilterMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveCurrentFilter();
            }
        });

        // Delete Named Filter menu item
        MenuItem deleteNamedFilterMI = new MenuItem(filterMenu, SWT.NONE);
        deleteNamedFilterMI.setText("Delete Named Filter...");
        deleteNamedFilterMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteNamedFilter();
            }
        });
    }

    /**
     * Populate the columns map with the table column names and the visibility
     * boolean value.
     */
    private void populateColumnsMap() {
        Set<DataEnum> visibleColumns = GhgConfigData.getInstance()
                .getVisibleColumns();

        // Set all columns to false
        columnsMap = new LinkedHashMap<>();
        columnsMap.put("Action", Boolean.FALSE);
        columnsMap.put("VTEC String", Boolean.FALSE);
        columnsMap.put("ETN", Boolean.FALSE);
        columnsMap.put("Phen.Sig", Boolean.FALSE);
        columnsMap.put("Hazard", Boolean.FALSE);
        columnsMap.put("Phen", Boolean.FALSE);
        columnsMap.put("Sig", Boolean.FALSE);
        columnsMap.put("Start", Boolean.FALSE);
        columnsMap.put("End", Boolean.FALSE);
        columnsMap.put("Purge", Boolean.FALSE);
        columnsMap.put("Issue Time", Boolean.FALSE);
        columnsMap.put("Pil", Boolean.FALSE);
        columnsMap.put("Seg", Boolean.FALSE);
        columnsMap.put("WFO", Boolean.FALSE);
        columnsMap.put("GeoID", Boolean.FALSE);
        columnsMap.put("ProductClass", Boolean.FALSE);

        // Set visible columns to true
        for (DataEnum col : visibleColumns) {
            columnsMap.put(col.columnLabel, Boolean.TRUE);
        }
    }

    /**
     * Update the column map to reflect the selected menu option.
     *
     * @param event
     *            Selection event.
     */
    private void updateTableColumns(SelectionEvent event) {
        // Update the column map using the widget source (menu checkbox)
        // and put the changes in the map. The "put" will overwrite the
        // existing entry in the map.
        MenuItem item = (MenuItem) event.getSource();
        String colName = item.getText();
        boolean selection = item.getSelection();

        columnsMap.put(colName, selection);

        DataEnum col = labelToEnumMap.get(colName);
        Set<DataEnum> visCols = GhgConfigData.getInstance().getVisibleColumns();
        // Keep the configuration current
        if (selection) {
            visCols.add(col);
        } else {
            visCols.remove(col);
        }

        // Have the table re-pack the columns which will show/hide the
        // user selected columns
        ghgTableComp.setVisibleColumns(visCols);
    }

    /**
     * Save the current filter.
     */
    private void saveCurrentFilter() {
        if (saveFilterDlg == null) {
            // Show the Save Filter dialog.
            GhgConfigData configuration = GhgConfigData.getInstance();
            saveFilterDlg = new GhgSaveDeleteFilterDlg(getShell(),
                    configuration.getFilterNames(), true);
            saveFilterDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        doSaveCurrentFilter(returnValue.toString());
                    }
                    saveFilterDlg = null;
                }
            });
        }
        saveFilterDlg.open();
    }

    private void doSaveCurrentFilter(String name) {
        GhgConfigData configuration = GhgConfigData.getInstance();

        // If no name was selected then return.
        if (name == null) {
            return;
        }

        // If the user selected name is not already in the saved
        // configuration array then add it and update the menu.
        try {
            // Reject "<defaulT>", "!DIALOG!", etc.
            if (name.matches("\\W*(?:(?i:default)|(?i:dialog))\\W*")
                    || "".equals(name)) {
                throw new VizException("Illegal filter name :" + name);
            }
            configuration.getCurrentFilter().name = name;
            configuration.addNamedFilter(name,
                    configuration.getCurrentFilter());
            updateFilterMenu();
            filterDisplay.updateFilter(name);
        } catch (final VizException e) {
            statusHandler.warn(e.getLocalizedMessage(), e);
        }
    }

    /**
     * Delete a named filter.
     */
    private void deleteNamedFilter() {
        if (deleteFilterDlg == null) {
            // Show the Delete Filter dialog.
            GhgConfigData configuration = GhgConfigData.getInstance();
            deleteFilterDlg = new GhgSaveDeleteFilterDlg(getShell(),
                    configuration.getFilterNames(), false);
            deleteFilterDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        doDeleteNamedFilter(returnValue.toString());
                    }
                    deleteFilterDlg = null;
                }
            });
        }
        deleteFilterDlg.open();
    }

    private void doDeleteNamedFilter(String name) {
        GhgConfigData configuration = GhgConfigData.getInstance();
        // If no name was selected then return.
        if (name == null) {
            return;
        }

        // If the user selected name is in the saved configuration
        // array then remove it and update the menu.
        try {
            configuration.deleteNamedFilter(name);
            updateFilterMenu();
            filterDisplay.updateFilter(configuration.getCurrentFilter().name);
        } catch (final VizException e) {
            statusHandler.warn(e.getLocalizedMessage(), e);
        }
    }

    /**
     * Create the main controls on the dialog.
     *
     * @param shell
     */
    private void createMainControls(Shell shell) {
        createTextMapTabs();
        createDataTable();
        createStatusBar(shell);

        ghgTableComp.addSelectionListener(this);
        ghgSpatialViewer.addSelectionListener(this);
        GhgConfigData.getInstance().addFilterChangeListener(this);
    }

    private void createStatusBar(Shell shell) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite statusComp = new Composite(sashForm, SWT.NONE);
        EdgeLayout el = new EdgeLayout();
        el.verticalAlignment = EdgeLayout.VerticalAlignment.CENTER;
        statusComp.setLayout(el);
        statusComp.setLayoutData(gd);
        final StatBar statBar = StatusStore.getStatusStore(STATUS_KEY)
                .getStatusBar();
        statBar.fill(statusComp);
        calcStatusImportanceMap();

        // make sure statBar's listeners are removed when the dialog closes.
        shell.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                statBar.dispose();
            }

        });

        filterDisplay = new FilterDisplay(statBar.getFont());
        filterDisplay.fill(statusComp);
        filterDisplay.updateFilter(
                GhgConfigData.getInstance().getCurrentFilter().name);
    }

    /**
     * Create the Text and Map tabs.
     */
    private void createTextMapTabs() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite tabComp = new Composite(sashForm, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        tabComp.setLayout(gl);
        tabComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        final TabFolder tabFolder = new TabFolder(tabComp, SWT.NONE);
        tabFolder.setLayoutData(gd);

        TabItem textTab = new TabItem(tabFolder, SWT.NONE);
        textTab.setText("Text");
        ghgTextComp = new GhgTextComp(tabFolder);
        textTab.setControl(ghgTextComp);

        TabItem mapTab = new TabItem(tabFolder, SWT.NONE);
        mapTab.setText("Map");

        Composite mapComp = new Composite(tabFolder, SWT.None);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        mapComp.setLayout(gl);
        mapComp.setLayoutData(gd);
        mapTab.setControl(mapComp);

        ghgSpatialViewer = new GHGSpatialViewer(mapComp, myWFO, gridLocation);
        ghgSpatialViewer.setMap(DEFAULT_MAP);
        ghgSpatialViewer.setZoomLevel(DEFAULT_ZOOM.getZoomLevel());

        tabFolder.setSelection(1);
    }

    /**
     * Create the data table at the bottom of the display.
     */
    private void createDataTable() {
        ghgTableComp = new GhgTableComp(sashForm);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        ghgTableComp.setLayoutData(gd);
        ghgTableComp.setVisibleColumns(
                GhgConfigData.getInstance().getVisibleColumns());
    }

    /**
     * Display the filter dialog.
     */
    private void showFilterDialog() {
        if (filterDlg == null) {
            GhgConfigData configuration = GhgConfigData.getInstance();

            filterDlg = new GhgFilterDlg(getShell(),
                    configuration.getCurrentFilter(), ifpClient, discreteDef,
                    my4WFO);
            filterDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        filterDisplay.updateFilter(returnValue.toString());
                    }
                    filterDlg = null;
                }
            });
        }
        filterDlg.open();
    }

    /**
     * Display the Define Alerts dialog.
     */
    private void showDefineAlertsDialog() {
        if (alertDlg == null) {
            GhgConfigData configuration = GhgConfigData.getInstance();
            alertDlg = new GhgAlertDlg(getShell());
            alertDlg.setAlerts(configuration.getAlerts());
            alertDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof GhgAlertsConfigData) {
                        GhgAlertsConfigData rtnAlerts = (GhgAlertsConfigData) returnValue;
                        GhgConfigData configuration = GhgConfigData
                                .getInstance();
                        configuration.setAlerts(rtnAlerts);
                    }
                    alertDlg = null;
                }
            });
            alertDlg.open();
        } else {
            alertDlg.bringToTop();
        }
    }

    /**
     * Display the Font dialog.
     */
    private void showFontDialog() {
        if (fontDlg == null) {
            GhgConfigData config = GhgConfigData.getInstance();
            fontDlg = new GhgFontDlg(getShell(), config.getCurrentFont());
            fontDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof GhgFontSizeEnum) {
                        // Update the data fonts in the table.
                        GhgFontSizeEnum fontEnum = (GhgFontSizeEnum) returnValue;

                        config.setCurrentFont(fontEnum);
                        ghgTableComp.setTableFont(fontEnum);
                    }
                    fontDlg = null;
                }
            });
        }
        fontDlg.open();
    }

    /**
     * Display the Color dialog.
     */
    private void showColorDialog() {
        if (colorDlg == null) {
            colorDlg = new GhgColorDlg(getShell());
            colorDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof Boolean) {
                        boolean changeColor = (Boolean) returnValue;
                        // Update the alert colors in the table
                        if (changeColor) {
                            ghgTableComp.updateDataColors();

                            updateMapColors();
                        }
                    }
                    colorDlg = null;
                }
            });
        }
        colorDlg.open();
    }

    private void updateMapColors() {
        Set<String> mapZones = new HashSet<>();
        Set<String> monitorZones = new HashSet<>();
        for (GhgData data : GhgMonitorDlg.this.dataList) {
            switch (data.getSelection()) {
            case MapSelection:
                mapZones.add(data.getGeoId());
                break;
            case MonitorSelection:
                monitorZones.add(data.getGeoId());
                break;
            default:
                break;

            }
        }

        if (!mapZones.isEmpty()) {
            ghgSpatialViewer.setHighlightedZones(
                    GhgConfigData.getInstance().getMapSelectionsColors()
                            .getBackgroundRgb(),
                    mapZones.toArray(new String[mapZones.size()]));
        }

        if (!monitorZones.isEmpty()) {
            ghgSpatialViewer.setHighlightedZones(
                    GhgConfigData.getInstance().getMonitorSelectionsColors()
                            .getBackgroundRgb(),
                    monitorZones.toArray(new String[monitorZones.size()]));
        }
    }

    /**
     * Gets the ActiveTable records and returns them as a List<GhgData>. The
     * list is based on the configuration selections.
     *
     * @return List<GhgData> list of GhgData objects
     */
    private List<GhgData> getTableData() {
        ServerResponse<List<ActiveTableRecord>> sr = ifpClient
                .getVTECActiveTable(activeTableName);
        if (!sr.isOkay()) {
            statusHandler.error(String.format("Error getting ActiveTable: %s",
                    sr.message()));
            return Collections.emptyList();
        }

        List<ActiveTableRecord> activeTableList = sr.getPayload();
        List<GhgData> tableData = new ArrayList<>();
        for (ActiveTableRecord rec : activeTableList) {
            GhgData data = new GhgData(rec, discreteDef
                    .getHazardDescription(HAZARDS_PARM_NAME, rec.getPhensig()));
            tableData.add(data);
        }

        return tableData;
    }

    private void refresh(boolean getData) {

        if (refreshing) {
            return;
        }

        try {
            refreshing = true;
            List<GhgData> newList = new ArrayList<>();

            if (getData) {
                newList = getTableData();
                /*
                 * dataList.retainAll() replaces "retained" entries from
                 * newList, which messes up alert level flags, popping up extra
                 * banners.
                 */

                // Remove dead entries from dataList.
                Iterator<GhgData> dlit = dataList.iterator();
                while (dlit.hasNext()) {
                    GhgData dlentry = dlit.next();

                    if (!newList.contains(dlentry)) {
                        dlit.remove();
                    }
                }
                // remove duplicates from newList
                newList.removeAll(dataList);
                // add really new records to dataList
                dataList.addAll(newList);
            }

            //
            // Re-select any records in the old table or map selection.
            //
            List<GhgData> monSelData = ghgTableComp.getMonitorSelectionData();
            for (GhgData data : dataList) {
                if (monSelData.contains(data)) {
                    data.setSelection(SelectionEnum.MonitorSelection);
                } else if (SelectionEnum.MapSelection != data.getSelection()) {
                    data.setSelection(SelectionEnum.NoSelection);
                }
            }

            tableData = calculateMonitorData();

            // Check for alerts
            doAlerting(tableData);

            filteredTable = doFiltering(tableData);

            rebuildTable();
        } finally {
            refreshing = false;
        }
    }

    /**
     * Clear ghgTableComp and re-populate it, using filteredTable as its data
     * source.
     */
    protected void rebuildTable() {

        ghgTableComp.clear();

        boolean visibleRowSelected = false;
        for (GhgData gd : filteredTable) {
            ghgTableComp.addGhgData(gd);
            if (SelectionEnum.NoSelection != gd.getSelection()) {
                visibleRowSelected = true;
            }
        }

        if (!visibleRowSelected) {
            // clear the map
            clearSelections();
        }

        ghgTableComp.setHighlight();
        ghgTableComp.updateDataColors();
    }

    /**
     * Do any alerting that is required, update the records
     *
     * @param dataList
     *            List<GhgData> list of data records
     */
    private void doAlerting(List<GhgData> dataList) {
        GhgAlertsConfigData alertsConfig = GhgConfigData.getInstance()
                .getAlerts();
        for (GhgData data : dataList) {
            GhgAlertCheckData alertCheckData = filterEngine.alertCheck(data);
            AlertsEnum alertType = alertCheckData.getAlertType();
            GhgAlertData alertData = alertsConfig.getAlert(alertType);
            if (alertData.isBanner()) {
                if (alertType == AlertsEnum.AlertLvl1) {
                    if (!data.isAlertLevel1()) {
                        for (GhgData component : data.getCombinedList()) {
                            component.setAlertLevel1(true);
                        }
                        data.setAlertLevel1(true);
                        sendAlert(alertCheckData, data);
                    }
                }

                if (alertType == AlertsEnum.AlertLvl2) {
                    if (!data.isAlertLevel2()) {
                        for (GhgData component : data.getCombinedList()) {
                            component.setAlertLevel2(true);
                        }
                        data.setAlertLevel2(true);
                        sendAlert(alertCheckData, data);
                    }
                }

                if (alertType == AlertsEnum.ExpiredAlert) {
                    if (!data.isAlertExpired()) {
                        for (GhgData component : data.getCombinedList()) {
                            component.setAlertExpired(true);
                        }
                        data.setAlertExpired(true);
                        sendAlert(alertCheckData, data);
                    }
                }
            }
        }

        ghgTableComp.setHighlight();
        ghgTableComp.updateDataColors();
    }

    /**
     * based on the display columns, the sort by, and the alert filter, creates
     * the set of (mostly unfiltered) spreadsheet data and returns it.
     *
     * @return the spreadheet data
     */
    private List<GhgData> calculateMonitorData() {
        GhgConfigData config = GhgConfigData.getInstance();
        List<GhgData> filteredList = new ArrayList<>();

        // do preliminary time filtering first to cut down
        // the record sizes
        GhgDataFilter filter = config.getCurrentFilter();
        if (!filter.includePastEvents) {
            // 2 day threshold
            long pastThreshold = SimulatedTime.getSystemTime().getTime()
                    .getTime() - (2 * TimeUtil.MILLIS_PER_DAY);
            for (GhgData gd : dataList) {
                if (gd.getEndDate().getTime() > pastThreshold) {
                    filteredList.add(gd);
                }
            }
        } else {
            filteredList.addAll(dataList);
        }

        // consolidate records in active table per display filter
        List<GhgData> consolidatedList = consolidate(filteredList);

        // Add the descriptions for the phen/sig
        addPhenSigDescription(consolidatedList);

        return consolidatedList;
    }

    /**
     * Filter the consolidated list of GhgData records according to the current
     * filter. This has to be done at each refresh or timer expiration, because
     * the "include alerts" flag accepts records at alert status, even if they
     * would otherwise be filtered out, and records need to be filtered out when
     * they are far past their expiration time.
     *
     * @param consolidatedList
     *            The records to filter
     * @return the filtered list.
     */
    protected List<GhgData> doFiltering(List<GhgData> consolidatedList) {
        List<GhgData> tableList = new ArrayList<>();
        for (GhgData gd : consolidatedList) {
            // Check if the data are filtered out
            if (filterEngine.filterCheck(gd)) {
                tableList.add(gd);
            }
        }
        return tableList;
    }

    /**
     * Adds the descriptions for the hazards into the records.
     *
     * @param list
     *            List of records to add descriptions to
     */
    private void addPhenSigDescription(List<GhgData> list) {
        for (GhgData rec : list) {
            rec.setHazard(discreteDef.getHazardDescription(HAZARDS_PARM_NAME,
                    rec.getPhenSig()));
        }
    }

    /**
     * consolidates the records in the given table to join those per the
     * displayFilter options.
     *
     * Ported from GHGMonitor.py _consolidate
     *
     * @param dataList
     *            List of GhgData objects
     * @return the consolidated records
     */
    private List<GhgData> consolidate(List<GhgData> dataList) {
        GhgConfigData config = GhgConfigData.getInstance();
        GhgDataFilter filter = config.getCurrentFilter();
        List<GhgData> combinedEntries = new ArrayList<>();

        // set up the comparisons
        List<String> compare = new ArrayList<>();
        compare.add("start");
        compare.add("end");
        compare.add("oid");
        compare.add("etn");
        compare.add("pil");
        compare.add("issueTime");
        compare.add("phensig");

        if (!filter.combineGeoId) {
            compare.add("id");
        }

        if (!filter.combineSegments) {
            compare.add("seg");
        }

        if (!filter.combinePurgeTimes) {
            compare.add("purgeTime");
        }

        // Consolidate
        for (GhgData gd : dataList) {
            boolean found = false;
            for (GhgData rec : combinedEntries) {
                if (recordCompare(gd, rec, compare)) {
                    String act = rec.getAction().split(",")[0];

                    // Combine Actions - can only combine actions that are
                    // similar
                    if (((!act.equals(gd.getAction())))
                            && (!filter.combineActions)) {
                        // don't combine this record
                        continue;
                    } else if (((ACT_GROUP_ONE.contains(gd.getAction()))
                            && (ACT_GROUP_ONE.contains(act)))
                            || ((ACT_GROUP_TWO.contains(gd.getAction()))
                                    && (ACT_GROUP_TWO.contains(act)))) {
                        rec.setAction(addToList(gd.getAction(), rec.getAction(),
                                true));
                    } else {
                        continue;
                    }

                    found = true;
                    rec.setCombined(true);
                    rec.getCombinedList().add(gd);

                    // Combine GeoIDs
                    rec.setGeoId(
                            addToList(gd.getGeoId(), rec.getGeoId(), false));

                    // Combine VTECstrs
                    if (rec.getVtecString().indexOf(gd.getVtecString()) == -1) {
                        rec.setVtecString(addToList(gd.getVtecString(),
                                rec.getVtecString(), false));
                    }

                    // deal with purge times -- take earliest
                    if (gd.getPurgeDate().getTime() < rec.getPurgeDate()
                            .getTime()) {
                        rec.setPurgeDate(gd.getPurgeDate());
                    }

                    // Combine segments and segment text
                    if (MapUtils.isNotEmpty(gd.getSegmentTextMap())) {
                        rec.setSegmentText(gd.getGeoId(), gd.getSegNum().get(0),
                                gd.getSegmentText(gd.getGeoId()));
                    }

                    // Combine alert levels -- keep lowest
                    if (rec.getAlert().compareTo(gd.getAlert()) > 0) {
                        rec.setAlert(gd.getAlert());
                    }

                    rec.setAlertLevel1(
                            rec.isAlertLevel1() && gd.isAlertLevel1());
                    rec.setAlertLevel2(
                            rec.isAlertLevel2() && gd.isAlertLevel2());
                    rec.setAlertExpired(
                            rec.isAlertExpired() && gd.isAlertExpired());

                    if (SelectionEnum.NoSelection != gd.getSelection()) {
                        rec.setSelection(gd.getSelection());
                    }
                }
            }

            if (!found) {
                // Add a COPY, so changes to rec above don't change the original
                combinedEntries.add(new GhgData(gd));
            }
        }

        return combinedEntries;
    }

    /**
     * compares two records for equality based on the fields given.
     *
     * @param rec1
     *            GhgData object
     * @param rec2
     *            GhgData object
     * @param fields
     *            Fields to compare on
     * @return true if equal, false otherwise
     */
    private boolean recordCompare(GhgData rec1, GhgData rec2,
            List<String> fields) {
        boolean equal = true;

        for (String s : fields) {
            if (!compareField(s, rec1, rec2)) {
                return false;
            }
        }

        return equal;
    }

    /**
     * compare the field in the provided GhgData objects.
     *
     * @param field
     *            The field to compare
     * @param rec1
     *            A GhgData object to compare
     * @param rec2
     *            A GhgData object to compare
     * @return true if the field in both objects are equal, false otherwise
     */
    private boolean compareField(String field, GhgData rec1, GhgData rec2) {
        boolean equal = true;

        if ("start".equalsIgnoreCase(field)) {
            if (rec1.getStartDate().getTime() != rec2.getStartDate()
                    .getTime()) {
                equal = false;
            }
        } else if ("end".equalsIgnoreCase(field)) {
            if (rec1.getEndDate().getTime() != rec2.getEndDate().getTime()) {
                equal = false;
            }
        } else if ("oid".equalsIgnoreCase(field)) {
            if (!rec1.getWfo().equals(rec2.getWfo())) {
                equal = false;
            }
        } else if ("etn".equalsIgnoreCase(field)) {
            if (!rec1.getEtn().equals(rec2.getEtn())) {
                equal = false;
            }
        } else if ("pil".equalsIgnoreCase(field)) {
            if (!rec1.getPil().equals(rec2.getPil())) {
                equal = false;
            }
        } else if ("issueTime".equalsIgnoreCase(field)) {
            if (rec1.getIssueTime().getTime() != rec2.getIssueTime()
                    .getTime()) {
                equal = false;
            }
        } else if ("id".equalsIgnoreCase(field)) {
            if (!rec1.getGeoId().equals(rec2.getGeoId())) {
                equal = false;
            }
        } else if ("seg".equalsIgnoreCase(field)) {
            if (!rec1.getSegNum().equals(rec2.getSegNum())) {
                equal = false;
            }
        } else if ("purgeTime".equalsIgnoreCase(field)) {
            if (rec1.getPurgeDate().getTime() != rec2.getPurgeDate()
                    .getTime()) {
                equal = false;
            }
        } else if ("phensig".equalsIgnoreCase(field)) {
            if (!rec1.getPhenSig().equals(rec2.getPhenSig())) {
                equal = false;
            }
        }

        return equal;
    }

    /**
     * Add an item to a comma separated list.
     *
     * @param item
     *            Item to add to the list
     * @param list
     *            The comma separated list
     * @param order
     *            true if list is to be sorted, false and item is added to end
     *            of the list
     * @return The new list
     */
    private String addToList(String item, String list, boolean order) {
        String returnValue = null;
        StringBuilder sb = new StringBuilder();
        String[] items = list.split(",");

        if (order) {
            Set<String> set = new TreeSet<>();
            for (String s : items) {
                set.add(s);
            }
            set.add(item);

            Iterator<String> iter = set.iterator();
            while (iter.hasNext()) {
                sb.append(iter.next() + ",");
            }
            returnValue = sb.toString().substring(0, sb.length() - 1);
        } else {
            list = list.concat("," + item);
            returnValue = list;
        }

        return returnValue;
    }

    /**
     * Call back executed when the user selects a saved filter from the Filter
     * menu.
     *
     * @param name
     *            the name of the filter
     */
    private void updateCurrentFilter(String name) {
        GhgConfigData configuration = GhgConfigData.getInstance();

        try {
            filterDisplay.updateFilter(name);
            configuration.setCurrentFilter(name);
            refresh(true);
        } catch (final VizException e) {
            statusHandler.warn(e.getLocalizedMessage(), e);
        }
    }

    /**
     * Selects the default filter.
     */
    private void selectDefaultFilter() {
        GhgConfigData configuration = GhgConfigData.getInstance();

        configuration.setDefaultAsCurrent(FeatureEnum.FILTERS);
        filterDisplay.updateFilter(configuration.getCurrentFilter().name);
        refresh(true);
    }

    /**
     * A class to represent a filter in the filter dialog's list.
     */
    private class FilterDisplay extends ContributionItem {

        public Label filterText;

        public Font font;

        public FilterDisplay(Font font) {
            this.font = font;
        }

        @Override
        public void fill(Composite parent) {
            Composite comp = new Composite(parent, SWT.NONE);
            EdgeLayoutData layoutData = new EdgeLayoutData();
            layoutData.edgeAffinity = EdgeAffinity.RIGHT;
            comp.setLayoutData(layoutData);

            comp.setLayout(new GridLayout(2, false));

            Label label = new Label(comp, SWT.NONE);
            label.setFont(font);
            label.setText("Filter:");

            filterText = new Label(comp, SWT.BORDER);
            filterText.setFont(font);
            filterText.setLayoutData(
                    new GridData(SWT.FILL, SWT.CENTER, true, true));
        }

        /**
         * Update the filter name shown in the lower-right corner.
         *
         * @param text
         *            The text to set.
         */
        public void updateFilter(String text) {
            // Protect against late updates when Ghg Monitor is closing.
            if (!filterText.isDisposed()) {
                filterText.setText(text);
                filterText.requestLayout();
            }
        }
    }

    /**
     * Apply the current configuration, at dialog startup or after the user has
     * made a change to the current configuration.
     */
    private void applyConfiguration() {
        GhgConfigData configuration = GhgConfigData.getInstance();

        ghgTableComp.updateDataColors();
        updateMapColors();
        GhgFontSizeEnum fontEnum = configuration.getCurrentFont();
        ghgTableComp.setTableFont(fontEnum);

        GhgDataFilter filter = configuration.getCurrentFilter();
        if ((filter == null) || (filter.name == null)) {
            configuration.setDefaultAsCurrent(FeatureEnum.FILTERS);
            filter = configuration.getCurrentFilter();
        }
        updateCurrentFilter(filter.name);
        updateFilterMenu();

        synchColumnsWithConfig();
        refresh(false);

        identifyTestMI.setSelection(configuration.isIdentifyTestEvents());
    }

    /**
     * Put the visible columns in configuration in the order in which they
     * appear on screen.
     */
    private void putColumnOrderInConfig() {
        GhgConfigData configuration = GhgConfigData.getInstance();

        int[] columnIndices = ghgTableComp.getGhgTable().getColumnOrder();
        Set<GhgConfigData.DataEnum> oldVisColumns = configuration
                .getVisibleColumns();
        Set<GhgConfigData.DataEnum> newVisColumns = EnumSet
                .noneOf(DataEnum.class);
        GhgConfigData.DataEnum[] deVals = GhgConfigData.DataEnum.values();
        for (int colIdx : columnIndices) {
            GhgConfigData.DataEnum value = deVals[colIdx];
            if (oldVisColumns.contains(value)) {
                newVisColumns.add(value);
            }
        }

        configuration.setVisibleColumns(newVisColumns);
    }

    /**
     * Set the sortColumn and descending properties of configuration based on
     * the state of ghgTableComp.
     */
    private void putSortingInConfig() {
        GhgConfigData configuration = GhgConfigData.getInstance();

        DataEnum selCol = ghgTableComp.getSelectedColumn();
        configuration.setSortColumn(selCol);

        Table table = ghgTableComp.getGhgTable();
        int sortDir = table.getSortDirection();
        configuration.setDescending(sortDir == SWT.DOWN);
    }

    private void calcStatusImportanceMap() {
        GhgConfigData config = GhgConfigData.getInstance();

        Map<Importance, MessageImportance> statusImportanceMap = new HashMap<>();
        statusImportanceMap.put(Importance.REGULAR,
                new MessageImportance(RGBColors.getRGBColor("green"),
                        RGBColors.getRGBColor("gray40"),
                        RGBColors.getRGBColor("gray80"), true, DEFAULT_TIMEOUT,
                        null, null, null));

        statusImportanceMap.put(Importance.SIGNIFICANT,
                new MessageImportance(RGBColors.getRGBColor("yellow"),
                        RGBColors.getRGBColor("black"),
                        RGBColors.getRGBColor("gray80"), false, DEFAULT_TIMEOUT,
                        "Significant Messages", RGBColors.getRGBColor("black"),
                        new RGB(0xEB, 0xEB, 0x00)));

        statusImportanceMap.put(Importance.URGENT, new MessageImportance(
                RGBColors.getRGBColor("red2"), RGBColors.getRGBColor("white"),
                RGBColors.getRGBColor("gray40"), false, 0, "Urgent Messages",
                RGBColors.getRGBColor("white"), RGBColors.getRGBColor("red2")));

        statusImportanceMap.put(Importance.ALERT,
                new MessageImportance(RGBColors.getRGBColor("orange"),
                        RGBColors.getRGBColor("black"),
                        RGBColors.getRGBColor("gray80"), false, DEFAULT_TIMEOUT,
                        null, null, null));

        GhgColorData alert1Colors = config.getAlertLvl1Colors();
        GhgColorData alert2Colors = config.getAlertLvl2Colors();
        GhgColorData expColors = config.getExpiredAlertColors();

        statusImportanceMap.put(Importance.ALERT1,
                new MessageImportance(alert1Colors.getForegroundRgb(),
                        alert1Colors.getBackgroundRgb(),
                        RGBColors.getRGBColor("gray80"), false, DEFAULT_TIMEOUT,
                        "Alert1 Purge Alert Messages",
                        alert1Colors.getForegroundRgb(),
                        alert1Colors.getBackgroundRgb()));

        statusImportanceMap.put(Importance.ALERT2,
                new MessageImportance(alert2Colors.getForegroundRgb(),
                        alert2Colors.getBackgroundRgb(),
                        RGBColors.getRGBColor("gray80"), false, DEFAULT_TIMEOUT,
                        "Alert2 Purge Alert Messages",
                        alert2Colors.getForegroundRgb(),
                        alert2Colors.getBackgroundRgb()));

        statusImportanceMap.put(Importance.EXPIRED, new MessageImportance(
                expColors.getForegroundRgb(), expColors.getBackgroundRgb(),
                RGBColors.getRGBColor("gray80"), false, DEFAULT_TIMEOUT,
                "Expired Purge Alert Messages", expColors.getForegroundRgb(),
                expColors.getBackgroundRgb()));

        // Set the new status importance map
        StatusStore.getStatusStore(
                com.raytheon.viz.ghg.constants.StatusConstants.CATEGORY_GHG)
                .setImportanceDict(statusImportanceMap);
    }

    private void sendAlert(GhgAlertCheckData alertData, GhgData rec) {
        String headline = discreteDef.getHazardDescription(HAZARDS_PARM_NAME,
                rec.getPhenSig());
        String action = rec.getAction();
        String[] cancelExpire = new String[] { "CAN", "EXP" };
        StringBuilder buffer = new StringBuilder();

        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm'Z' dd-MMM-yyyy");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        String pTimeStr = sdf.format(rec.getPurgeDate());
        String eTimeStr = sdf.format(rec.getEndDate());

        String[] actions = action.split(",");
        if (actions.length == 1) {
            action = actions[0];
        } else {
            action = "None";
        }

        // get the nearest minute
        int deltaMinP = alertData.getDeltaP();

        int deltaMinE = alertData.getDeltaE();

        if ((deltaMinP > 0) && (deltaMinE > 0)) {
            // Not to purge time yet, not to event ending time yet

            // Get the smaller of the two times
            int sooner = Math.min(deltaMinP, deltaMinE);

            if (!Arrays.asList(cancelExpire).contains(action)) {
                if (deltaMinE == sooner) {
                    buffer.append("Event Will End at " + eTimeStr + " in "
                            + sooner + " minutes. ");
                } else {
                    buffer.append("Product will Expire at " + pTimeStr + " in "
                            + sooner + " minutes. ");
                }
            } else if ("CAN".equals(action)) {
                buffer.append(
                        "Event has been Cancelled. No further action required.");
            } else if ("EXP".equals(action)) {
                buffer.append(
                        "Event will be allowed to Expire. No further action required.");
            }
        } else if ((deltaMinP > 0) && (deltaMinE <= 0)) {
            // Not to purge time yet, after event ending time
            if (!Arrays.asList(cancelExpire).contains(action)) {
                buffer.append(
                        "Event has ended, but no EXP/CAN product has been issued. ");
            } else if ("CAN".equals(action)) {
                buffer.append(
                        "Event was cancelled. No further action required.");
            } else if ("EXP".equals(action)) {
                buffer.append(
                        "Event was allowed to expire. No further action required.");
            }
        } else if ((deltaMinP <= 0) && (deltaMinE <= 0)) {
            // After purge time, after event ending time

            // Get the later of the two times
            int later = Math.max(deltaMinP, deltaMinE);

            // Within 30 minutes of purge/end time?
            boolean within30 = false;
            if (later > (-30 * 60)) {
                within30 = true;
            }

            if (within30) {
                if (!Arrays.asList(cancelExpire).contains(action)) {
                    buffer.append(
                            "Event has ended, but no EXP/CAN product has been issued.");
                } else if ("CAN".equals(action)) {
                    buffer.append(
                            "Event was cancelled. No further action required.");
                } else if ("EXP".equals(action)) {
                    buffer.append(
                            "Event was allowed to expire. No further action required.");
                } else {
                    // outside of 30 minute window - ignore this situation
                    return;
                }
            }
        } else if ((deltaMinP <= 0) && (deltaMinE > 0)) {
            buffer.append(
                    "Event is ongoing, but no current product exists describing event. ");
        }

        buffer.append("  Event=" + rec.getPhenSig() + " " + headline);

        StatusMessage.Importance importance = Importance.ALERT1;
        if (alertData.getAlertType() == AlertsEnum.AlertLvl2) {
            importance = Importance.ALERT2;
        } else if (alertData.getAlertType() == AlertsEnum.ExpiredAlert) {
            importance = Importance.EXPIRED;
        }

        bringToTop();
        StatusStore.updateStatus(STATUS_KEY, buffer.toString(), importance);
    }

    private void updateDisplay(VTECTableChangeNotification msg) {
        GhgDataFilter filter = GhgConfigData.getInstance().getCurrentFilter();
        // We need to refresh if any change MIGHT pass the filter.
        boolean needRefresh = false;
        if (filter.includeAlerts || filter.includeMapSelections
                || filter.includePastEvents || filter.includeOrgPilEvents) {
            needRefresh = true;
        } else {
            for (VTECChange change : msg.getChanges()) {
                if (ArrayUtils.isEmpty(filter.wfos) || (Arrays
                        .binarySearch(filter.wfos, change.getSite()) >= 0)) {
                    if (ArrayUtils.isEmpty(filter.phenSigs)
                            || (Arrays.binarySearch(filter.phenSigs,
                                    change.getPhensig()) >= 0)) {
                        if (ArrayUtils.isEmpty(filter.pils)
                                || (Arrays.binarySearch(filter.pils,
                                        change.getPil()) >= 0)) {
                            needRefresh = true;
                            break;
                        }
                    }
                }
            }
        }
        if (needRefresh) {
            refresh(true);
        }
    }

    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        statusHandler.handle(Priority.VERBOSE,
                "Received " + messages.length + " notification messages.");
        for (NotificationMessage msg : messages) {
            try {
                Object payload = msg.getMessagePayload();
                if (payload instanceof VTECTableChangeNotification) {
                    final VTECTableChangeNotification notification = (VTECTableChangeNotification) payload;
                    VizApp.runSync(new Runnable() {
                        @Override
                        public void run() {
                            updateDisplay(notification);
                        }
                    });
                } else {
                    statusHandler.handle(Priority.VERBOSE,
                            "Skipped a " + payload.getClass().getName()
                                    + " notification message");
                }

            } catch (NotificationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error getting notification message", e);
            }
        }
    }

    /**
     * Clear all selections, map and spreadsheet.
     */
    private void clearSelections() {
        // Clear map selections
        List<String> highlightedZones = Collections.emptyList();
        GhgMonitorTableSelectionEvent tableEvent = new GhgMonitorTableSelectionEvent();
        tableEvent.setHighlightedZones(highlightedZones);
        notifyUpdate(tableEvent);

        // Clear spreadsheet selections
        GhgMonitorZoneSelectionEvent zoneEvent = new GhgMonitorZoneSelectionEvent();
        zoneEvent.setHighlightedZones(highlightedZones);
        notifyUpdate(zoneEvent);
    }

    @Override
    protected void preOpened() {
        // Populate the dialog with fresh data
        refresh(true);
        ghgTableComp.updateDataColors();
        applyConfiguration();
    }

    /**
     * Initialize the auto-update timer
     */
    private void initTimer() {
        Date date = SimulatedTime.getSystemTime().getTime();
        long now = date.getTime();
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        cal.add(Calendar.MINUTE, 1);
        cal.set(Calendar.SECOND, 0);
        long delay = cal.getTime().getTime() - now;
        long period = TimeUtil.MILLIS_PER_MINUTE;
        timer = new Timer();
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        // Check for alerts
                        doAlerting(tableData);

                        // Records can be added to or removed from the display
                        // as they age.
                        List<GhgData> filteredData = doFiltering(tableData);
                        if (!filteredData.equals(filteredTable)) {
                            filteredTable = filteredData;
                            rebuildTable();

                        }
                    }
                });
            }
        }, delay, period);
    }

    /**
     * Resynch columnsMap with config. This is usually used when a column config
     * is loaded, so the configuration, columns menu, and table will all agree
     * which columns are visible.
     */
    private void synchColumnsWithConfig() {

        GhgConfigData config = GhgConfigData.getInstance();
        Set<DataEnum> visCols = config.getVisibleColumns();
        for (DataEnum column : DataEnum.values()) {
            columnsMap.put(column.columnLabel, visCols.contains(column));
        }

        for (MenuItem item : columnsMenu.getItems()) {
            String label = item.getText();
            if (columnsMap.containsKey(label)) {
                item.setSelection(columnsMap.get(label));
            }
        }
    }

    @Override
    public void notifyUpdate(AbstractGhgMonitorEvent event) {
        if (event instanceof GhgMonitorTableSelectionEvent) {
            GhgMonitorTableSelectionEvent tableEvent = (GhgMonitorTableSelectionEvent) event;
            ghgTextComp.update(tableEvent.getGhgData());

            Collection<String> highlightedZones = tableEvent
                    .getHighlightedZones();
            ghgSpatialViewer.setHighlightedZones(tableEvent.getSelectionColor(),
                    highlightedZones
                            .toArray(new String[highlightedZones.size()]));

        } else if (event instanceof GhgMonitorZoneSelectionEvent) {
            GhgMonitorZoneSelectionEvent zoneEvent = (GhgMonitorZoneSelectionEvent) event;
            Collection<String> highlightedZones = zoneEvent
                    .getHighlightedZones();
            for (GhgData combinedRecord : this.tableData) {
                SelectionEnum selection = SelectionEnum.NoSelection;
                for (GhgData data : combinedRecord.getCombinedList()) {
                    if (highlightedZones.contains(data.getGeoId())) {
                        selection = SelectionEnum.MapSelection;
                        break;
                    }
                }
                combinedRecord.setSelection(selection);
            }

            ghgTableComp.update(highlightedZones);

        } else if (event instanceof GhgMonitorFilterChangeEvent) {
            filterDisplay.updateFilter(GhgConfigData.CUSTOM_FILTER_NAME);
            refresh(true);

        } else {
            statusHandler.error("Received unexpected notification of type "
                    + event.getClass().getName());
        }

    }
}
