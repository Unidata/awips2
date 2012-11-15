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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.TreeSet;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.FontData;
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

import com.raytheon.uf.common.activetable.VTECChange;
import com.raytheon.uf.common.activetable.VTECTableChangeNotification;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ghg.monitor.constants.GhgMenuConstants;
import com.raytheon.viz.ghg.monitor.data.GhgAlertCheckData;
import com.raytheon.viz.ghg.monitor.data.GhgAlertData;
import com.raytheon.viz.ghg.monitor.data.GhgAlertsConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgColorData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.AlertsEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.DataEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.FeatureEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.SelectionEnum;
import com.raytheon.viz.ghg.monitor.data.GhgData;
import com.raytheon.viz.ghg.monitor.data.GhgDataFilter;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorFilterChangeEvent;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorTableSelectionEvent;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorZoneSelectionEvent;
import com.raytheon.viz.ghg.monitor.filter.GhgFilterEngine;
import com.raytheon.viz.ghg.monitor.listener.GhgMonitorFilterChangeListener;
import com.raytheon.viz.ghg.monitor.listener.GhgMonitorZoneSelectionListener;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation
 * 17Jun2008    1157       MW Fegan    Pass configuration to sub-dialogs.
 * 15 Nov 2012  1298       rferrel     Changes for non-blocking dialog.
 *                                      Changes for non-blocking GhgAlertDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GhgMonitorDlg extends CaveSWTDialog implements
        GhgMonitorFilterChangeListener, GhgMonitorZoneSelectionListener,
        INotificationObserver {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GhgMonitorDlg.class);

    private static final Map<String, GhgConfigData.DataEnum> labelToEnumMap;

    private GhgAlertDlg alertDlg;

    /**
     * Active group one string.
     */
    private final String ACT_GROUP_ONE_STRING = " CON EXA EXB EXT NEW ";

    /**
     * Active group two string.
     */
    private final String ACT_GROUP_TWO_STRING = " CAN EXP UPG ";

    /**
     * Default timeout value.
     */
    private final long DEFAULT_TIMEOUT = 30000;

    /**
     * Status bar definition key.
     */
    private final String STATUS_KEY = com.raytheon.viz.ghg.constants.StatusConstants.CATEGORY_GHG;

    /**
     * The currently selected font data (is null until font is selected)
     */
    private FontData currentFontData = null;

    /**
     * A linked hash map of all the column names and a boolean value indicating
     * if the column should be visible or not.
     */
    private LinkedHashMap<String, Boolean> columnsMap;

    /**
     * A single filter menu item.
     */
    private MenuItem filterMenuItem;

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
    protected GhgMapComp ghgMapComponent;

    /**
     * Sash Form used to adjust the area a composite takes up.
     */
    private SashForm sashForm;

    private FilterDisplay filterDisplay;

    private Menu columnsMenu;

    /**
     * The status importance map.
     */
    private Map<Importance, MessageImportance> statusImportanceMap;

    private List<GhgData> dataList = new ArrayList<GhgData>();

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
        Map<String, GhgConfigData.DataEnum> temp = new HashMap<String, GhgConfigData.DataEnum>(
                deVals.length);
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
     */
    public GhgMonitorDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.INDEPENDENT_SHELL
                | CAVE.DO_NOT_BLOCK);

        // Register as a listener to the display manager and VTECActiveTable
        GhgDisplayManager.getInstance().addGhgMonitorFilterChangeListener(this);
        NotificationManagerJob.addObserver("edex.alerts.vtec", this);
    }

    /**
     * Initialize the dialog's configuration
     */
    private void initializeConfiguration() {
        // Load the default configuration file.
        // If this fails, fall back to the hardcoded defaults.
        GhgConfigData configuration = GhgConfigData.getInstance();

        try {
            configuration.loadDefault();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error loading default configuration", e);
        }

        configuration.makeCurrentFilterDefault();
        configuration.makeCurrentAlertsDefault();
        configuration.makeVisibleColumnsDefault();

        configuration.setDefaultAsCurrent(FeatureEnum.FILTERS);
        configuration.setDefaultAsCurrent(FeatureEnum.ALERTS);
        configuration.setDefaultAsCurrent(FeatureEnum.COLUMNS);
        // configuration.setDefaultAsCurrent(FeatureEnum.COLORS);

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
        GhgDisplayManager.getInstance().removeGhgMonitorChangeListener(this);
        GhgDisplayManager.getInstance().removeGhgMonitorZoneSelectionListener(
                this);
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

        String activeTableName = "active";
        String practiceText = "";
        DataManager dm = DataManager.getCurrentInstance();

        CAVEMode mode = dm.getOpMode();
        if (mode.equals(CAVEMode.PRACTICE)) {
            activeTableName = "PRACTICE";
            practiceText = "     [PRACTICE MODE]";
        }

        // Set up the title bar text
        String userId = System.getProperty("user.name");
        shell.setText("GHG Hazards Monitor: (" + userId + " - "
                + activeTableName + ")" + practiceText);

        GridLayout mainLayout = (GridLayout) shell.getLayout();
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;

        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
        createMenus(shell);

        // -----------------------------------------------------
        // Create the SashForm that will be used to manually
        // control the amount of space the top and bottom
        // composites occupy.
        // -----------------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite sashComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        sashComp.setLayout(gl);
        sashComp.setLayoutData(gd);

        sashForm = new SashForm(sashComp, SWT.VERTICAL);
        sashForm.setLayout(new GridLayout(1, false));
        sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        sashForm.SASH_WIDTH = 10;

        createMainControls(shell);

        sashForm.setWeights(new int[] { 70, 30, 5 });

        // Set up timer to check for alerting every minute
        initTimer();
    }

    /**
     * Create the main menu bar and menu items.
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
                StatusStore.getStatusStore(STATUS_KEY).addMessage(
                        "Saved configuration", Importance.REGULAR);
            }
        });

        // Load Configuration menu item
        MenuItem loadConfigMI = new MenuItem(fileMenu, SWT.NONE);
        loadConfigMI.setText("&Load Configuration");
        loadConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgConfigData.getInstance().load(true);
                applyConfiguration(true);
            }
        });

        // Default Configuration menu item
        MenuItem defaultConfigMI = new MenuItem(fileMenu, SWT.NONE);
        defaultConfigMI.setText("&Default Configuration");
        defaultConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgConfigData.getInstance().loadDefault();
                applyConfiguration(true);
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

        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            String key = iterator.next();

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
                ghgTableComp.showHideColumns();
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
        filterMenuItem = new MenuItem(menuBar, SWT.CASCADE);
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
        Menu mapMenu = new Menu(menuBar);
        mapMenuItem.setMenu(mapMenu);

        // -----------------------------------------------------
        // Create all the items in the Map dropdown menu
        // -----------------------------------------------------

        // Show Marine menu item
        MenuItem showMarineMI = new MenuItem(mapMenu, SWT.RADIO);
        showMarineMI.setText("Show Marine");
        showMarineMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoneDisplay(
                        GhgMenuConstants.ShowMap.SHOW_MARINE);
                ghgMapComponent.updateZone();
                clearSelections();
            }
        });

        // Show FIPS menu item
        MenuItem showFipsMI = new MenuItem(mapMenu, SWT.RADIO);
        showFipsMI.setText("Show FIPS");
        showFipsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoneDisplay(
                        GhgMenuConstants.ShowMap.SHOW_FIPS);
                ghgMapComponent.updateZone();
                clearSelections();
            }
        });

        // Show Public menu item
        MenuItem showPublicMI = new MenuItem(mapMenu, SWT.RADIO);
        showPublicMI.setText("Show Public");
        showPublicMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoneDisplay(
                        GhgMenuConstants.ShowMap.SHOW_PUBLIC);
                ghgMapComponent.updateZone();
                clearSelections();
            }
        });
        showPublicMI.setSelection(true);

        // Show Fire Wx menu item
        MenuItem showFireWxMI = new MenuItem(mapMenu, SWT.RADIO);
        showFireWxMI.setText("Show Fire Wx");
        showFireWxMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoneDisplay(
                        GhgMenuConstants.ShowMap.SHOW_FIRE);
                ghgMapComponent.updateZone();
                clearSelections();
            }
        });

        // Menu Separator
        new MenuItem(mapMenu, SWT.SEPARATOR);

        // No Zoom menu item
        MenuItem noZoomMI = new MenuItem(mapMenu, SWT.RADIO);
        noZoomMI.setText("No Zoom");
        noZoomMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoomLevel(
                        GhgMenuConstants.ZoomLevel.ZOOM_NO_ZOOM.getZoomLevel());
                ghgMapComponent.updateZoom();
            }
        });
        noZoomMI.setSelection(true);

        // x2 Zoom menu item
        MenuItem x2ZoomMI = new MenuItem(mapMenu, SWT.RADIO);
        x2ZoomMI.setText("x2");
        x2ZoomMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoomLevel(
                        GhgMenuConstants.ZoomLevel.ZOOM_2.getZoomLevel());
                ghgMapComponent.updateZoom();
            }
        });

        // x4 Zoom menu item
        MenuItem x4ZoomMI = new MenuItem(mapMenu, SWT.RADIO);
        x4ZoomMI.setText("x4");
        x4ZoomMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoomLevel(
                        GhgMenuConstants.ZoomLevel.ZOOM_4.getZoomLevel());
                ghgMapComponent.updateZoom();
            }
        });

        // x6 Zoom menu item
        MenuItem x6ZoomMI = new MenuItem(mapMenu, SWT.RADIO);
        x6ZoomMI.setText("x6");
        x6ZoomMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoomLevel(
                        GhgMenuConstants.ZoomLevel.ZOOM_6.getZoomLevel());
                ghgMapComponent.updateZoom();
            }
        });

        // x8 Zoom menu item
        MenuItem x8ZoomMI = new MenuItem(mapMenu, SWT.RADIO);
        x8ZoomMI.setText("x8");
        x8ZoomMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoomLevel(
                        GhgMenuConstants.ZoomLevel.ZOOM_8.getZoomLevel());
                ghgMapComponent.updateZoom();
            }
        });

        // x12 Zoom menu item
        MenuItem x12ZoomMI = new MenuItem(mapMenu, SWT.RADIO);
        x12ZoomMI.setText("x12");
        x12ZoomMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoomLevel(
                        GhgMenuConstants.ZoomLevel.ZOOM_12.getZoomLevel());
                ghgMapComponent.updateZoom();
            }
        });

        // x16 Zoom menu item
        MenuItem x16ZoomMI = new MenuItem(mapMenu, SWT.RADIO);
        x16ZoomMI.setText("x16");
        x16ZoomMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setZoomLevel(
                        GhgMenuConstants.ZoomLevel.ZOOM_16.getZoomLevel());
                ghgMapComponent.updateZoom();
            }
        });

        // Menu Separator
        new MenuItem(mapMenu, SWT.SEPARATOR);

        // Show Labels menu item
        final MenuItem showLabelsMI = new MenuItem(mapMenu, SWT.CHECK);
        showLabelsMI.setText("Show Labels");
        showLabelsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setShowLabels(
                        showLabelsMI.getSelection());
                ghgMapComponent.updateLabels();
            }
        });
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
        final MenuItem identifyTestMI = new MenuItem(appearanceMenu, SWT.CHECK);
        identifyTestMI.setText("Identify TEST Events");
        identifyTestMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GhgDisplayManager.getInstance().setIdentifyTestData(
                        identifyTestMI.getSelection());
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
                ghgMapComponent.refresh();
            }
        });

        // Default Filter menu item
        MenuItem defaultFilterMI = new MenuItem(filterMenu, SWT.NONE);
        defaultFilterMI.setText("Default Filter");
        defaultFilterMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectDefaultFilter(true);
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
                    updateCurrentFilter(tmpMI.getText(), true);
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
        List<DataEnum> visibleColumns = GhgConfigData.getInstance()
                .getVisibleColumns();

        // Set all columns to false
        columnsMap = new LinkedHashMap<String, Boolean>();
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
        Collection<DataEnum> visCols = GhgConfigData.getInstance()
                .getVisibleColumns();
        // Keep the configuration current
        if (selection) {
            visCols.add(col);
        } else {
            visCols.remove(col);
        }

        // Have the table re-pack the columns which will show/hide the
        // user selected columns
        ghgTableComp.showHideColumns();
    }

    /**
     * Save the current filter.
     */
    private void saveCurrentFilter() {
        // Show the Save Filter dialog.
        GhgConfigData configuration = GhgConfigData.getInstance();
        GhgSaveDeleteFilterDlg saveFilterDlg = new GhgSaveDeleteFilterDlg(
                getShell(), configuration.getFilterNames(), true);
        String name = (String) saveFilterDlg.open();

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
            configuration
                    .addNamedFilter(name, configuration.getCurrentFilter());
            updateFilterMenu();
            filterDisplay.updateFilter(name);
        } catch (final VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage());
        }
    }

    /**
     * Delete a named filter.
     */
    private void deleteNamedFilter() {
        // Show the Delete Filter dialog.
        GhgConfigData configuration = GhgConfigData.getInstance();
        GhgSaveDeleteFilterDlg deleteFilterDlg = new GhgSaveDeleteFilterDlg(
                getShell(), configuration.getFilterNames(), false);
        String name = (String) deleteFilterDlg.open();

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
            statusHandler.handle(Priority.PROBLEM, e.getMessage());
        }
    }

    /**
     * Create the main controls on the dialog.
     */
    private void createMainControls(Shell shell) {
        createTextMapTabs();
        createDataTable();
        createStatusBar(shell);
    }

    private void createStatusBar(Shell shell) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite statusComp = new Composite(sashForm, SWT.NONE);
        EdgeLayout el = new EdgeLayout();
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

        filterDisplay = new FilterDisplay();
        filterDisplay.fill(statusComp);
        filterDisplay.updateFilter(GhgConfigData.getInstance()
                .getCurrentFilter().name);
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
        ghgTextComp = new GhgTextComp(tabFolder); // , controller);
        textTab.setControl(ghgTextComp);

        TabItem mapTab = new TabItem(tabFolder, SWT.NONE);
        mapTab.setText("Map");
        ghgMapComponent = new GhgMapComp(tabFolder);
        mapTab.setControl(ghgMapComponent);
        GhgDisplayManager.getInstance()
                .addGhgMonitorZoneSelectionListener(this);

        tabFolder.setSelection(1);
    }

    /**
     * Create the data table at the bottom of the display.
     */
    private void createDataTable() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite tableComp = new Composite(sashForm, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        tableComp.setLayout(gl);
        tableComp.setLayoutData(gd);

        ghgTableComp = new GhgTableComp(tableComp, columnsMap);
    }

    /**
     * Display the filter dialog.
     */
    private void showFilterDialog() {
        GhgConfigData configuration = GhgConfigData.getInstance();

        GhgFilterDlg filterDlg = new GhgFilterDlg(getShell(),
                configuration.getCurrentFilter());
        filterDlg.open();
        filterDisplay.updateFilter(configuration.getCurrentFilter().name);
    }

    /**
     * Display the Define Alerts dialog.
     */
    private void showDefineAlertsDialog() {
        if (alertDlg == null) {
            GhgConfigData configuration = GhgConfigData.getInstance();
            alertDlg = new GhgAlertDlg(getShell());
            alertDlg.setAlerts(configuration.getAlerts());
            alertDlg.setCloseCallback(new ICloseCallback() {

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
        }
        alertDlg.open();
    }

    /**
     * Display the Font dialog.
     */
    private void showFontDialog() {
        GhgFontDlg fontDlg = new GhgFontDlg(getShell(),
                GhgConfigData.getInstance());
        currentFontData = (FontData) fontDlg.open();

        // Update the data fonts in the table.
        if (currentFontData != null) {
            ghgTableComp.updateTableFont(currentFontData);
        }
    }

    /**
     * Display the Color dialog.
     */
    private void showColorDialog() {
        GhgColorDlg colorDlg = new GhgColorDlg(getShell());
        boolean changeColor = (Boolean) colorDlg.open();

        // Update the alert colors in the table
        if (changeColor == true) {
            ghgTableComp.updateDataColors();
            ghgMapComponent.updateMapColors();
        }
    }

    public void refresh(boolean getData) {

        if (refreshing) {
            return;
        }

        try {
            refreshing = true;
            GhgDisplayManager dispMan = GhgDisplayManager.getInstance();
            List<GhgData> newList = new ArrayList<GhgData>();

            if (getData) {
                newList = dispMan.getTableData();
                // dataList.retainAll() replaces "retained" entries from
                // newList,
                // which messes up alert level flags, popping up extra banners.
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
            try {
                doAlerting(tableData);
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

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

        if (visibleRowSelected == false) {
            clearSelections(); // clear the map
        }

        ghgTableComp.setHighlight();
        ghgTableComp.updateTable();
    }

    /**
     * Do any alerting that is required, update the records
     * 
     * @param dataList
     *            List<GhgData> list of data records
     * @throws GFEServerException
     */
    private void doAlerting(List<GhgData> dataList) throws GFEServerException {
        GhgAlertsConfigData alertsConfig = GhgConfigData.getInstance()
                .getAlerts();
        for (GhgData data : dataList) {
            GhgAlertCheckData alertCheckData = GhgFilterEngine.alertCheck(data);
            AlertsEnum alertType = alertCheckData.getAlertType();
            GhgAlertData alertData = alertsConfig.getAlert(alertType);
            if (alertData.isBanner()) {
                if (alertType == AlertsEnum.AlertLvl1) {
                    if (data.isAlertLevel1() == false) {
                        for (GhgData component : data.getCombinedList()) {
                            component.setAlertLevel1(true);
                        }
                        data.setAlertLevel1(true);
                        sendAlert(alertCheckData, data);
                    }
                }

                if (alertType == AlertsEnum.AlertLvl2) {
                    if (data.isAlertLevel2() == false) {
                        for (GhgData component : data.getCombinedList()) {
                            component.setAlertLevel2(true);
                        }
                        data.setAlertLevel2(true);
                        sendAlert(alertCheckData, data);
                    }
                }

                if (alertType == AlertsEnum.ExpiredAlert) {
                    if (data.isAlertExpired() == false) {
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
        ghgTableComp.updateTable();
    }

    /**
     * based on the display columns, the sort by, and the alert filter, creates
     * the set of (mostly unfiltered) spreadsheet data and returns it.
     */
    private List<GhgData> calculateMonitorData() {
        GhgConfigData config = GhgConfigData.getInstance();
        List<GhgData> filteredList = new ArrayList<GhgData>();

        // do preliminary time filtering first to cut down
        // the record sizes
        GhgDataFilter filter = config.getCurrentFilter();
        if (filter.includePastEvents == false) {
            // 2 day threshold
            long pastThreshold = SimulatedTime.getSystemTime().getTime()
                    .getTime()
                    - (2 * 86400 * 1000);
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
        List<GhgData> tableList = new ArrayList<GhgData>();
        for (GhgData gd : consolidatedList) {
            // Check if the data are filtered out
            if (GhgFilterEngine.filterCheck(gd) == true) {
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
            try {
                rec.setHazard(GhgData.getHazardDescription(rec.getPhenSig()));
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * consolidates the records in the given table to join those per the
     * displayFilter options. Returns the consolidated records.
     * 
     * Ported from GHGMonitor.py _consolidate
     * 
     * @param dataList
     *            List of GhgData objects
     */
    private List<GhgData> consolidate(List<GhgData> dataList) {
        GhgConfigData config = GhgConfigData.getInstance();
        GhgDataFilter filter = config.getCurrentFilter();
        List<GhgData> combinedEntries = new ArrayList<GhgData>();

        // set up the comparisons
        List<String> compare = new ArrayList<String>();
        compare.add("start");
        compare.add("end");
        compare.add("oid");
        compare.add("etn");
        compare.add("pil");
        compare.add("issueTime");
        compare.add("phensig");

        if (filter.combineGeoId == false) {
            compare.add("id");
        }

        if (filter.combineSegments == false) {
            compare.add("seg");
        }

        if (filter.combinePurgeTimes == false) {
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
                    if (((act.equals(gd.getAction())) == false)
                            && (filter.combineActions == false)) {
                        // don't combine this record
                        continue;
                    } else if (((ACT_GROUP_ONE_STRING.indexOf(gd.getAction()) > -1) && (ACT_GROUP_ONE_STRING
                            .indexOf(act) > -1))
                            || ((ACT_GROUP_TWO_STRING.indexOf(gd.getAction()) > -1) && (ACT_GROUP_TWO_STRING
                                    .indexOf(act) > -1))) {
                        rec.setAction(addToList(gd.getAction(),
                                rec.getAction(), true));
                    } else {
                        continue;
                    }

                    found = true;
                    rec.setCombined(true);
                    rec.getCombinedList().add(gd);

                    // Combine GeoIDs
                    rec.setGeoId(addToList(gd.getGeoId(), rec.getGeoId(), false));

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

                    // Combine segment text
                    if ((gd.getSegmentTextMap() != null)
                            && (gd.getSegmentTextMap().size() > 0)) {
                        rec.setSegmentText(gd.getGeoId(),
                                gd.getSegmentText(gd.getGeoId()));
                    }

                    // Combine segments
                    if (rec.getSegNum().indexOf(gd.getSegNum()) == -1) {
                        rec.setSegNum(addToList(gd.getSegNum(),
                                rec.getSegNum(), true));
                    }

                    // Combine alert levels -- keep lowest
                    if (rec.getAlert().compareTo(gd.getAlert()) > 0) {
                        rec.setAlert(gd.getAlert());
                    }

                    rec.setAlertLevel1(rec.isAlertLevel1()
                            && gd.isAlertLevel1());
                    rec.setAlertLevel2(rec.isAlertLevel2()
                            && gd.isAlertLevel2());
                    rec.setAlertExpired(rec.isAlertExpired()
                            && gd.isAlertExpired());

                    if (SelectionEnum.NoSelection != gd.getSelection()) {
                        rec.setSelection(gd.getSelection());
                    }
                }
            }

            if (found == false) {
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
            if (compareField(s, rec1, rec2) == false) {
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

        if (field.equalsIgnoreCase("start")) {
            if (rec1.getStartDate().getTime() != rec2.getStartDate().getTime()) {
                equal = false;
            }
        } else if (field.equalsIgnoreCase("end")) {
            if (rec1.getEndDate().getTime() != rec2.getEndDate().getTime()) {
                equal = false;
            }
        } else if (field.equalsIgnoreCase("oid")) {
            if (rec1.getWfo().equals(rec2.getWfo()) == false) {
                equal = false;
            }
        } else if (field.equalsIgnoreCase("etn")) {
            if (rec1.getEtn().equals(rec2.getEtn()) == false) {
                equal = false;
            }
        } else if (field.equalsIgnoreCase("pil")) {
            if (rec1.getPil().equals(rec2.getPil()) == false) {
                equal = false;
            }
        } else if (field.equalsIgnoreCase("issueTime")) {
            if (rec1.getIssueTime().getTime() != rec2.getIssueTime().getTime()) {
                equal = false;
            }
        } else if (field.equalsIgnoreCase("id")) {
            if (rec1.getGeoId().equals(rec2.getGeoId()) == false) {
                equal = false;
            }
        } else if (field.equalsIgnoreCase("seg")) {
            if (rec1.getSegNum().equals(rec2.getSegNum()) == false) {
                equal = false;
            }
        } else if (field.equalsIgnoreCase("purgeTime")) {
            if (rec1.getPurgeDate().getTime() != rec2.getPurgeDate().getTime()) {
                equal = false;
            }
        } else if (field.equalsIgnoreCase("phensig")) {
            if (rec1.getPhenSig().equals(rec2.getPhenSig()) == false) {
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
            Set<String> set = new TreeSet<String>();
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
    private void updateCurrentFilter(String name, boolean load) {
        GhgConfigData configuration = GhgConfigData.getInstance();

        try {
            filterDisplay.updateFilter(name);
            configuration.setCurrentFilter(name);
            refresh(true);
        } catch (final VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage());
        }
    }

    /**
     * Selects the default filter.
     */
    private void selectDefaultFilter(boolean load) {
        GhgConfigData configuration = GhgConfigData.getInstance();

        configuration.setDefaultAsCurrent(FeatureEnum.FILTERS);
        filterDisplay.updateFilter(configuration.getCurrentFilter().name);
        refresh(true);
    }

    /**
     * A class to represent a filter in the filter dialog's list.
     */
    private class FilterDisplay extends ContributionItem {

        private Composite comp;

        private Label filterText;

        @Override
        public void fill(Composite parent) {
            comp = new Composite(parent, SWT.NONE);
            EdgeLayoutData layoutData = new EdgeLayoutData();
            layoutData.edgeAffinity = EdgeAffinity.RIGHT;
            comp.setLayoutData(layoutData);

            comp.setLayout(new GridLayout(2, false));

            Label label = new Label(comp, SWT.NONE);
            label.setText("Filter:");

            filterText = new Label(comp, SWT.BORDER);
            filterText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                    true));
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
            }
        }
    }

    /**
     * Apply the current configuration, at dialog startup or after the user has
     * made a change to the current configuration.
     */
    private void applyConfiguration(boolean load) {
        GhgConfigData configuration = GhgConfigData.getInstance();

        ghgTableComp.updateDataColors();
        ghgTableComp.updateTableFont(configuration.getCurrentFont()
                .getFontData(getDisplay()));

        GhgDataFilter filter = configuration.getCurrentFilter();
        if ((filter == null) || (filter.name == null)) {
            configuration.setDefaultAsCurrent(FeatureEnum.FILTERS);
            filter = configuration.getCurrentFilter();
        }
        updateCurrentFilter(filter.name, false);
        updateFilterMenu();

        synchColumnsWithConfig();
        refresh(false);
        ghgTableComp.packColumns();
    }

    /**
     * Put the visible columns in configuration in the order in which they
     * appear onscreen.
     */
    private void putColumnOrderInConfig() {
        GhgConfigData configuration = GhgConfigData.getInstance();

        int[] columnIndices = ghgTableComp.getGhgTable().getColumnOrder();
        List<GhgConfigData.DataEnum> oldVisColumns = configuration
                .getVisibleColumns();
        List<GhgConfigData.DataEnum> newVisColumns = new ArrayList<GhgConfigData.DataEnum>(
                oldVisColumns.size());
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

        int selCol = ghgTableComp.getSelectedColumn();
        configuration.setSortColumn(GhgConfigData.DataEnum.values()[selCol]);

        Table table = ghgTableComp.getGhgTable();
        int sortDir = table.getSortDirection();
        configuration.setDescending(sortDir == SWT.DOWN);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ghg.monitor.listener.GhgMonitorFilterChangeListener#
     * notifyUpdate
     * (com.raytheon.viz.ghg.monitor.event.GhgMonitorFilterChangeEvent)
     */
    @Override
    public void notifyUpdate(GhgMonitorFilterChangeEvent evt) {
        boolean filterChanged = evt.isFilterChanged();
        boolean refreshData = false;
        if (filterChanged == true) {
            filterDisplay.updateFilter(GhgConfigData.CUSTOM_FILTER_NAME);
            refreshData = true;
        }

        refresh(refreshData);
    }

    private void calcStatusImportanceMap() {
        statusImportanceMap = new HashMap<Importance, MessageImportance>();
        GhgConfigData config = GhgConfigData.getInstance();

        statusImportanceMap.put(
                Importance.REGULAR,
                new MessageImportance(RGBColors.getRGBColor("green"), RGBColors
                        .getRGBColor("gray40"),
                        RGBColors.getRGBColor("gray80"), true, DEFAULT_TIMEOUT,
                        null, null, null));

        statusImportanceMap.put(Importance.SIGNIFICANT, new MessageImportance(
                RGBColors.getRGBColor("yellow"),
                RGBColors.getRGBColor("black"),
                RGBColors.getRGBColor("gray80"), false, DEFAULT_TIMEOUT,
                "Significant Messages", RGBColors.getRGBColor("black"),
                new RGB(0xEB, 0xEB, 0x00)));

        statusImportanceMap.put(Importance.URGENT, new MessageImportance(
                RGBColors.getRGBColor("red2"), RGBColors.getRGBColor("white"),
                RGBColors.getRGBColor("gray40"), false, 0, "Urgent Messages",
                RGBColors.getRGBColor("white"), RGBColors.getRGBColor("red2")));

        statusImportanceMap.put(Importance.ALERT, new MessageImportance(
                RGBColors.getRGBColor("orange"),
                RGBColors.getRGBColor("black"),
                RGBColors.getRGBColor("gray80"), false, DEFAULT_TIMEOUT, null,
                null, null));

        GhgColorData alert1Colors = config.getAlertLvl1Colors();
        GhgColorData alert2Colors = config.getAlertLvl2Colors();
        GhgColorData expColors = config.getExpiredAlertColors();

        statusImportanceMap.put(
                Importance.ALERT1,
                new MessageImportance(alert1Colors.getForegroundRgb(),
                        alert1Colors.getBackgroundRgb(), RGBColors
                                .getRGBColor("gray80"), false, DEFAULT_TIMEOUT,
                        "Alert1 Purge Alert Messages", alert1Colors
                                .getForegroundRgb(), alert1Colors
                                .getBackgroundRgb()));

        statusImportanceMap.put(
                Importance.ALERT2,
                new MessageImportance(alert2Colors.getForegroundRgb(),
                        alert2Colors.getBackgroundRgb(), RGBColors
                                .getRGBColor("gray80"), false, DEFAULT_TIMEOUT,
                        "Alert2 Purge Alert Messages", alert2Colors
                                .getForegroundRgb(), alert2Colors
                                .getBackgroundRgb()));

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

    private void sendAlert(GhgAlertCheckData alertData, GhgData rec)
            throws GFEServerException {
        String headline = GhgData.getHazardDescription(rec.getPhenSig());
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

            if (Arrays.asList(cancelExpire).contains(action) == false) {
                if (deltaMinE == sooner) {
                    buffer.append("Event Will End at " + eTimeStr + " in "
                            + sooner + " minutes. ");
                } else {
                    buffer.append("Product will Expire at " + pTimeStr + " in "
                            + sooner + " minutes. ");
                }
            } else if (action.equals("CAN")) {
                buffer.append("Event has been Cancelled. No further action required.");
            } else if (action.equals("EXP")) {
                buffer.append("Event will be allowed to Expire. No further action required.");
            }
        } else if ((deltaMinP > 0) && (deltaMinE <= 0)) {
            // Not to purge time yet, after event ending time
            if (Arrays.asList(cancelExpire).contains(action) == false) {
                buffer.append("Event has ended, but no EXP/CAN product has been issued. ");
            } else if (action.equals("CAN")) {
                buffer.append("Event was cancelled. No further action required.");
            } else if (action.equals("EXP")) {
                buffer.append("Event was allowed to expire. No further action required.");
            }
        } else if ((deltaMinP <= 0) && (deltaMinE <= 0)) {
            // After purge time, after event ending time

            // Get the later of the two times
            int later = Math.max(deltaMinP, deltaMinE);

            // Within 30 minutes of purge/end time?
            boolean within30 = false;
            if (later > -30 * 60) {
                within30 = true;
            }

            if (within30) {
                if (Arrays.asList(cancelExpire).contains(action) == false) {
                    buffer.append("Event has ended, but no EXP/CAN product has been issued.");
                } else if (action.equals("CAN")) {
                    buffer.append("Event was cancelled. No further action required.");
                } else if (action.equals("EXP")) {
                    buffer.append("Event was allowed to expire. No further action required.");
                } else {
                    // outside of 30 minute window - ignore this situation
                    return;
                }
            }
        } else if ((deltaMinP <= 0) && (deltaMinE > 0)) {
            buffer.append("Event is ongoing, but no current product exists describing event. ");
        }

        buffer.append("  " + headline);

        StatusMessage.Importance importance = Importance.ALERT1;
        if (alertData.getAlertType() == AlertsEnum.AlertLvl2) {
            importance = Importance.ALERT2;
        } else if (alertData.getAlertType() == AlertsEnum.ExpiredAlert) {
            importance = Importance.EXPIRED;
        }

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
                if (filter.wfos == null
                        || filter.wfos.length == 0
                        || Arrays.binarySearch(filter.wfos, change.getSite()) >= 0) {
                    if (filter.phenSigs == null
                            || filter.phenSigs.length == 0
                            || Arrays.binarySearch(filter.phenSigs,
                                    change.getPhensig()) >= 0) {
                        if (filter.pils == null
                                || filter.pils.length == 0
                                || Arrays.binarySearch(filter.pils,
                                        change.getPil()) >= 0) {
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.notification.INotificationObserver#
     * notificationArrived
     * (com.raytheon.uf.viz.core.notification.NotificationMessage[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        statusHandler.handle(Priority.VERBOSE, "Received " + messages.length
                + " notification messages.");
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
                    statusHandler.handle(Priority.VERBOSE, "Skipped a "
                            + payload.getClass().getName()
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
        GhgDisplayManager dMan = GhgDisplayManager.getInstance();
        GhgMonitorTableSelectionEvent evt = new GhgMonitorTableSelectionEvent(
                this);
        evt.setHighlightedZones(new String[0]);
        dMan.fireTableSelectionEvent(evt);

        // Clear spreadsheet selections
        GhgMonitorZoneSelectionEvent evt2 = new GhgMonitorZoneSelectionEvent(
                this);
        evt2.setHighlightedZones(new String[0]);
        dMan.fireMapChangeEvent(evt2);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#opened()
     */
    @Override
    protected void opened() {
        // Populate the dialog with fresh data
        refresh(true);
        ghgTableComp.updateTable();
    }

    /**
     * Initialize the auto-update timer
     */
    private void initTimer() {
        int delay = 1000 * 60; // delay for 1 min.
        int period = 1000 * 60; // repeat every min.
        timer = new Timer();
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        // Check for alerts
                        try {
                            doAlerting(tableData);
                        } catch (GFEServerException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }

                        // Records can be added to or removed from the display
                        // as they age.
                        List<GhgData> filteredData = doFiltering(tableData);
                        if (filteredData.equals(filteredTable) == false) {
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
        List<DataEnum> visCols = config.getVisibleColumns();
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ghg.monitor.listener.GhgMonitorZoneSelectionListener
     * #notifyUpdate
     * (com.raytheon.viz.ghg.monitor.event.GhgMonitorZoneSelectionEvent)
     */
    @Override
    public void notifyUpdate(GhgMonitorZoneSelectionEvent evt) {
        String[] highlightedZones = evt.getHighlightedZones();
        // Be polite. Other handlers get the event, too. Sort a copy.
        String[] sortHlZones = Arrays.copyOf(highlightedZones,
                highlightedZones.length);
        Arrays.sort(sortHlZones);
        for (GhgData data : dataList) {
            if (Arrays.binarySearch(sortHlZones, data.getGeoId()) >= 0) {
                data.setSelection(SelectionEnum.MapSelection);
            } else if (SelectionEnum.MapSelection == data.getSelection()
                    || highlightedZones.length > 0) {
                // We may(?) get a zero-length map selection in response to
                // selecting table records. In that case, we need to clear the
                // map selections without interfering with the table (aka
                // monitor) selection. However, if map zones are really being
                // selected, then the table selection should be cleared, too.
                data.setSelection(SelectionEnum.NoSelection);
            }
        }
        refresh(false);
    }
}
