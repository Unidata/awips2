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
package com.raytheon.viz.volumebrowser.vbui;

import java.util.HashMap;
import java.util.List;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.globals.IGlobalChangedListener;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.points.data.Point;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.core.slice.request.HeightScales;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.LeftRightMenu;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.SpaceTimeMenu;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

/**
 * Main Volume Browser dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2009 #2161      lvenable     Initial creation
 * Jul 21, 2012 #875       rferrel     Now uses points.
 * Sep 26, 2012 #1216      rferrel     Point Change listener added to update
 *                                      the Time Series Point menu.
 * Oct  2, 2012 #1234      rferrel     Time series Point menu accounts for 
 *                                      having no points.
 * Jun 23, 2014 #3162      lvenable    Added code to have the Volume Browser display the min/max
 *                                     buttons in the title bar on thin client.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class VolumeBrowserDlg extends CaveSWTDialog implements
        IGlobalChangedListener {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VolumeBrowserDlg.class);

    private VolumeBrowserDialogSettings dialogSettings;

    /**
     * Settings menu item.
     */
    private MenuItem settingsMI;

    /**
     * Space/Time menu item.
     */
    private MenuItem spaceTimeMI;

    /**
     * Left/Right menu item.
     */
    private MenuItem leftRightMI;

    /**
     * Point menu item.
     */
    private MenuItem pointsMI;

    /**
     * Main menu bar.
     */
    private Menu menuBar;

    /**
     * Vertical coordinates menu
     */
    private MenuItem vertCoordMI;

    /**
     * Composite containing the Source, Fields, and Planes lists and the Product
     * Selection table.
     */
    private DataListsProdTableComp listTableComp;

    public final String DIALOG_TITLE = "Volume Browser";

    private SpaceTimeMenu previousSpaceTimeMenu = null;

    private boolean initialized = false;

    private IPointChangedListener pointChangeListener;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public VolumeBrowserDlg(Shell parent) {
        super(parent, SWT.SHELL_TRIM | SWT.RESIZE, CAVE.INDEPENDENT_SHELL
                | CAVE.DO_NOT_BLOCK);
        setText(DIALOG_TITLE);

        dialogSettings = new VolumeBrowserDialogSettings();
        VizGlobalsManager.addListener(VizConstants.LOADMODE_ID, this);
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    @Override
    protected void disposed() {
        VizGlobalsManager.removeListener(VizConstants.LOADMODE_ID, this);
        /*
         * Clear all of the map when closing the dialog.
         */
        MenuItemManager miMgr = MenuItemManager.getInstance();
        miMgr.clearAllMaps();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        initialized = false;
        Shell parent = getParent();
        // Initialize all of the controls and layouts
        initializeComponents();

        shell.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                getProductTable().resizeTableColumns();
            }

        });

        parent.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                VolumeBrowserDlg.this.shell.dispose();
            }
        });

        // closing the dialog doesn't really close it
        shell.addShellListener(new ShellAdapter() {

            @Override
            public void shellClosed(ShellEvent e) {
                VolumeBrowserDlg.this.shell.setVisible(false);
                e.doit = false;
            }
        });

        initialized = true;
    }

    @Override
    protected void preOpened() {
        // Resize the product table columns.
        getProductTable().resizeTableColumns();

        // Set the shell's minimum size so the controls cannot be hidden.
        shell.setMinimumSize(shell.getSize());
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createMenus();

        createDataSelectionListProductTableManager();

        updateToolbarMenus(false);
    }

    /**
     * Create the menu bar and menus.
     */
    private void createMenus() {
        menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createEditMenu(menuBar);
        createToolsMenu(menuBar);
        createSettingsMenu(menuBar);

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
        // Create the File menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // -------------------------------------------------
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        // Clone menu item
        MenuItem cloneMI = new MenuItem(fileMenu, SWT.NONE);
        cloneMI.setText("Clone");
        cloneMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                List<ProductTableData> selectedProducts = getProductTable()
                        .getSelectedData();
                new CloneDialog(new Shell(SWT.MODELESS), selectedProducts)
                        .open();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        // Exit menu item
        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("Exit");
        exitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.notifyListeners(SWT.Close, new Event());
            }
        });
    }

    /**
     * Create the Edit menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createEditMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Edit menu
        // -------------------------------------
        MenuItem editMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        editMenuItem.setText("Edit");

        // Create the Edit menu item with a Edit "dropdown" menu
        Menu editMenu = new Menu(menuBar);
        editMenuItem.setMenu(editMenu);

        // -------------------------------------------------
        // Create all the items in the Edit dropdown menu
        // -------------------------------------------------

        // Clear All menu item
        MenuItem clearAllMI = new MenuItem(editMenu, SWT.NONE);
        clearAllMI.setText("Clear All");
        clearAllMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearAllLists();
            }
        });

        new MenuItem(editMenu, SWT.SEPARATOR);

        // Clear Sources menu item
        MenuItem clearSourcesMI = new MenuItem(editMenu, SWT.NONE);
        clearSourcesMI.setText("Clear Sources");
        clearSourcesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearSourcesList();
            }
        });

        // Clear Fields menu item
        MenuItem clearFieldsMI = new MenuItem(editMenu, SWT.NONE);
        clearFieldsMI.setText("Clear Fields");
        clearFieldsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearFieldsList();
            }
        });

        // Clear Planes menu item
        MenuItem clearPlanesMI = new MenuItem(editMenu, SWT.NONE);
        clearPlanesMI.setText("Clear Planes");
        clearPlanesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearPlanesList();
            }
        });

        new MenuItem(editMenu, SWT.SEPARATOR);

        // Select None menu item
        MenuItem selectNoneMI = new MenuItem(editMenu, SWT.NONE);
        selectNoneMI.setText("Select None");
        selectNoneMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectNone();
            }
        });

        // Select None menu item
        MenuItem selectAllMI = new MenuItem(editMenu, SWT.NONE);
        selectAllMI.setText("Select All");
        selectAllMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectAll();
            }
        });

        new MenuItem(editMenu, SWT.SEPARATOR);

        // Find toggle menu
        MenuItem findMI = new MenuItem(editMenu, SWT.CHECK);
        findMI.setText("Find");

        int longestLength = getLongestMenuItemLength(editMenu);
        StringBuilder findText = new StringBuilder("Find");
        for (int i = 0; i < longestLength - findText.length(); i++) {
            findText.append(" ");
        }
        findText.append('\t');
        findText.append("Ctrl+F");

        findMI.setText(findText.toString());

        findMI.setAccelerator(SWT.CTRL | 'F');
        findMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                listTableComp.toggleFind();
            }
        });
    }

    /**
     * @param editMenu
     * @return
     */
    private int getLongestMenuItemLength(Menu editMenu) {
        int maxLength = 0;
        for (MenuItem menu : editMenu.getItems()) {
            maxLength = Math.max(menu.getText().length(), maxLength);
        }
        return maxLength;
    }

    /**
     * Create the Tools menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createToolsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Tools menu
        // -------------------------------------
        MenuItem toolsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        toolsMenuItem.setText("Tools");

        // Create the Tools menu item with a Tools "dropdown" menu
        Menu toolsMenu = new Menu(menuBar);
        toolsMenuItem.setMenu(toolsMenu);

        // -------------------------------------------------
        // Create all the items in the Tools dropdown menu
        // -------------------------------------------------

        // Baselines menu item
        MenuItem baselinesMI = new MenuItem(toolsMenu, SWT.NONE);
        baselinesMI.setText("Baselines");
        baselinesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ICommandService service = (ICommandService) PlatformUI
                        .getWorkbench().getActiveWorkbenchWindow()
                        .getService(ICommandService.class);
                Command c = service
                        .getCommand("com.raytheon.viz.awipstools.baselines");
                if (c != null) {
                    HashMap<String, Object> params = new HashMap<String, Object>();
                    ExecutionEvent exec = new ExecutionEvent(c, params, null,
                            null);
                    try {
                        c.executeWithChecks(exec);
                    } catch (Exception e) {
                        e.printStackTrace();
                        // UFStatus.handle(Priority.PROBLEM,
                        // Activator.PLUGIN_ID,
                        // StatusConstants.CATEGORY_GFE, null,
                        // "Error executing open python command", e);
                    }
                }
            }
        });

        // Points menu item
        MenuItem pointsMI = new MenuItem(toolsMenu, SWT.NONE);
        pointsMI.setText("Points");
        pointsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ICommandService service = (ICommandService) PlatformUI
                        .getWorkbench().getActiveWorkbenchWindow()
                        .getService(ICommandService.class);
                Command c = service
                        .getCommand("com.raytheon.viz.awipstools.points");
                if (c != null) {
                    HashMap<String, Object> params = new HashMap<String, Object>();
                    ExecutionEvent exec = new ExecutionEvent(c, params, null,
                            null);
                    try {
                        c.executeWithChecks(exec);
                    } catch (Exception e) {
                        e.printStackTrace();
                        // UFStatus.handle(Priority.PROBLEM,
                        // Activator.PLUGIN_ID,
                        // StatusConstants.CATEGORY_GFE, null,
                        // "Error executing open python command", e);
                    }
                }
            }
        });

        // Choose by ID menu item
        MenuItem chooseByIdMI = new MenuItem(toolsMenu, SWT.NONE);
        chooseByIdMI.setText("Choose By ID...");
        chooseByIdMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ICommandService service = (ICommandService) PlatformUI
                        .getWorkbench().getActiveWorkbenchWindow()
                        .getService(ICommandService.class);
                Command c = service
                        .getCommand("com.raytheon.viz.awipstools.choosebyid");
                if (c != null) {
                    HashMap<String, Object> params = new HashMap<String, Object>();
                    ExecutionEvent exec = new ExecutionEvent(c, params, null,
                            null);
                    try {
                        c.executeWithChecks(exec);
                    } catch (Exception e) {
                        e.printStackTrace();
                        // UFStatus.handle(Priority.PROBLEM,
                        // Activator.PLUGIN_ID,
                        // StatusConstants.CATEGORY_GFE, null,
                        // "Error executing open python command", e);
                    }
                }
            }
        });
    }

    /**
     * Create the "Settings" menu. The menu text on the menu bar reflects the
     * selected menu item.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createSettingsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the "Settings" menu
        // -------------------------------------
        settingsMI = new MenuItem(menuBar, SWT.CASCADE);
        settingsMI.setText(ViewMenu.PLANVIEW.displayString);
        settingsMI.setData(ViewMenu.PLANVIEW);

        // Create the "Settings" menu item
        Menu settingsMenu = new Menu(menuBar);
        settingsMI.setMenu(settingsMenu);

        for (ViewMenu settingsItem : ViewMenu.values()) {
            final MenuItem menuItem = new MenuItem(settingsMenu, SWT.NONE);
            menuItem.setText(settingsItem.displayString);
            menuItem.setData(settingsItem);
            menuItem.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    MenuItem mi = (MenuItem) event.getSource();
                    settingMenuSelection(mi);
                    dialogSettings.setViewSelection(mi);
                }
            });
        }

        dialogSettings.setViewSelection((ViewMenu) settingsMI.getData());

        createSpaceTimeMenu(previousSpaceTimeMenu);
    }

    /**
     * Create the data selection lists and the product selection table.
     */
    private void createDataSelectionListProductTableManager() {
        listTableComp = new DataListsProdTableComp(shell);
    }

    /**
     * Clear all of the items in the Sources, Fields, and Planes lists.
     */
    private void clearAllLists() {
        /*
         * Clear the Sources/Fields/Planes lists.
         */
        listTableComp.clearAllLists();

        /*
         * Clear the Times/Product Selection/Inventory lists.
         */
        getProductTable().clearProductTable();
    }

    /**
     * Clear the Sources list.
     */
    private void clearSourcesList() {
        /*
         * Clear the Sources lists.
         */
        listTableComp.clearSourcesList(true);

        /*
         * Clear the Times/Product Selection/Inventory lists.
         */
        getProductTable().clearProductTable();
    }

    /**
     * Clear the Fields list.
     */
    private void clearFieldsList() {
        /*
         * Clear the Fields lists.
         */
        listTableComp.clearFieldsList(true);

        /*
         * Clear the Times/Product Selection/Inventory lists.
         */
        getProductTable().clearProductTable();
    }

    /**
     * Clear the Planes list.
     */
    private void clearPlanesList() {
        /*
         * Clear the Planes lists.
         */
        listTableComp.clearPlanesList(true);

        /*
         * Clear the Times/Product Selection/Inventory lists.
         */
        getProductTable().clearProductTable();
    }

    /**
     * Unselect all of the items in the product selection lists and the product
     * selection table.
     */
    private void selectNone() {
        getProductTable().clearProductTable();

        listTableComp.deselectAllListItems();
        listTableComp.updateMenuInventory();
    }

    /**
     * Select all of the items in the Sources, Fields, and Planes lists.
     * Populate the products table with the products of all of the selected
     * items.
     */
    private void selectAll() {
        // Select all of the items in the Sources/Fields/Planes lists.
        listTableComp.selectAllListItems();
        listTableComp.updateMenuInventory();

        // TODO :
        // Update the product selection lists and select all of the items in the
        // lists.
    }

    /**
     * Handle the menu item selected in the "Settings" menu.
     * 
     * @param mi
     */
    private void settingMenuSelection(MenuItem mi) {
        settingsMI.setText(mi.getText());
        settingsMI.setData(mi.getData());

        if (pointChangeListener != null) {
            PointsDataManager.getInstance().removePointsChangedListener(
                    pointChangeListener);
            pointChangeListener = null;
        }

        if (spaceTimeMI != null) {
            previousSpaceTimeMenu = (SpaceTimeMenu) spaceTimeMI.getData();
            spaceTimeMI.dispose();
            spaceTimeMI = null;
        }

        if (vertCoordMI != null) {
            vertCoordMI.dispose();
            vertCoordMI = null;
        }

        if (leftRightMI != null) {
            leftRightMI.dispose();
            leftRightMI = null;
        }

        if (pointsMI != null) {
            pointsMI.dispose();
            pointsMI = null;
        }

        // reset the dialog settings
        dialogSettings = new VolumeBrowserDialogSettings();

        ViewMenu currentSetting = (ViewMenu) mi.getData();

        if (currentSetting == ViewMenu.PLANVIEW) {
            createSpaceTimeMenu(previousSpaceTimeMenu);
        } else if (currentSetting == ViewMenu.CROSSSECTION) {
            createSpaceTimeMenu(previousSpaceTimeMenu);
            createVCoordMenu();
        } else if (currentSetting == ViewMenu.TIMEHEIGHT) {
            createLeftRightMenu();
            createVCoordMenu();
        } else if (currentSetting == ViewMenu.VARVSHGT) {
            createVCoordMenu();
        } else if (currentSetting == ViewMenu.SOUNDING) {
            // Do nothing. No extra menus need to be created.
        } else if (currentSetting == ViewMenu.TIMESERIES) {
            createPointsMenu();
            pointChangeListener = new IPointChangedListener() {

                @Override
                public void pointChanged() {
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            pointsMI.dispose();
                            createPointsMenu();
                        }
                    });
                }
            };
            PointsDataManager.getInstance().addPointsChangedListener(
                    pointChangeListener);
        }

        dialogSettings.setViewSelection(currentSetting);

        updateToolbarMenus(true);
    }

    /**
     * Create the Space/Time menu.
     */
    private void createSpaceTimeMenu(SpaceTimeMenu previousSpaceTimeMenu) {

        // -------------------------------------
        // Create the "Time/Space" menu
        // -------------------------------------
        spaceTimeMI = new MenuItem(menuBar, SWT.CASCADE);

        if (previousSpaceTimeMenu != null) {
            spaceTimeMI.setText(previousSpaceTimeMenu.displayString);
            spaceTimeMI.setData(previousSpaceTimeMenu);
            previousSpaceTimeMenu.callback.execute();
        } else {
            spaceTimeMI.setText(SpaceTimeMenu.TIME.displayString);
            spaceTimeMI.setData(SpaceTimeMenu.TIME);
            SpaceTimeMenu.TIME.callback.execute();
        }

        // Create the "Time/Space" menu item
        Menu spaceTimeMenu = new Menu(menuBar);
        spaceTimeMI.setMenu(spaceTimeMenu);

        for (final SpaceTimeMenu spaceTimeItem : SpaceTimeMenu.values()) {
            final MenuItem menuItem = new MenuItem(spaceTimeMenu, SWT.NONE);
            menuItem.setText(spaceTimeItem.displayString);
            menuItem.setData(spaceTimeItem);
            menuItem.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    spaceTimeItem.callback.execute();
                    MenuItem mi = (MenuItem) event.getSource();
                    dialogSettings.setSpaceTimeSelection(mi);
                    updateMenu(mi);
                    updateToolbarMenus(true);
                }
            });
        }

        dialogSettings.setSpaceTimeSelection((SpaceTimeMenu) spaceTimeMI
                .getData());
    }

    /**
     * Create the Vertical Coordinates menu.
     */
    private void createVCoordMenu() {
        // -------------------------------------
        // Create the "Vertical Resolution" menu
        // -------------------------------------

        vertCoordMI = new MenuItem(menuBar, SWT.CASCADE);
        Menu vertCoordMenu = new Menu(menuBar);
        vertCoordMI.setMenu(vertCoordMenu);
        for (HeightScale hs : HeightScales.getInstance().getScales()) {
            MenuItem hsItem = new MenuItem(vertCoordMenu, SWT.NONE);
            hsItem.setText(hs.getName());
            hsItem.setData(hs);
            hsItem.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    MenuItem mi = (MenuItem) event.getSource();
                    updateMenu(mi);
                    dialogSettings.setHeightScaleSelection(mi);
                }
            });
        }

        MenuItem first = vertCoordMenu.getItem(0);
        updateMenu(first);
        dialogSettings.setHeightScaleSelection(first);
    }

    /**
     * Create the Left/Right menu.
     */
    private void createLeftRightMenu() {
        // -------------------------------------
        // Create the "Left/Right" menu
        // -------------------------------------
        leftRightMI = new MenuItem(menuBar, SWT.CASCADE);
        leftRightMI.setText(LeftRightMenu.LEFT.displayString);
        leftRightMI.setData(LeftRightMenu.LEFT);

        // Create the "Left/Right" menu item
        Menu lrMenu = new Menu(menuBar);
        leftRightMI.setMenu(lrMenu);

        for (LeftRightMenu leftRightItem : LeftRightMenu.values()) {
            final MenuItem menuItem = new MenuItem(lrMenu, SWT.NONE);
            menuItem.setText(leftRightItem.displayString);
            menuItem.setData(leftRightItem);
            menuItem.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    MenuItem mi = (MenuItem) event.getSource();
                    updateMenu(mi);
                    dialogSettings.setTimeDirectionSelection(mi);
                }
            });
        }

        dialogSettings.setTimeDirectionSelection((LeftRightMenu) leftRightMI
                .getData());
    }

    /**
     * Create the points menu.
     */
    private void createPointsMenu() {
        // -------------------------------------
        // Create the "Points" menu
        // -------------------------------------
        pointsMI = new MenuItem(menuBar, SWT.CASCADE);

        Menu pntsMenu = new Menu(menuBar);
        pointsMI.setMenu(pntsMenu);
        IPointNode firstPoint = populatePointsMenu(pntsMenu, null);
        if (firstPoint == null) {
            statusHandler.handle(Priority.WARN, "No Points available.");
            pointsMI.setText("NO POINTS");
        } else {
            pointsMI.setText("Point " + firstPoint.getName());
            pointsMI.setData(firstPoint);

            dialogSettings.setPointsSelection((Point) pointsMI.getData());
        }
    }

    /**
     * Place in a menu the children of a node. When a child of the node is a
     * group create a menu for it and populate it.
     * 
     * @param pntsMenu
     *            - the menu to add items to
     * @param node
     *            - add the children of this node (may be null)
     * @return firstPoint
     */
    private IPointNode populatePointsMenu(Menu pntsMenu, IPointNode node) {
        IPointNode firstPoint = null;
        IPointNode point = null;
        for (IPointNode child : PointsDataManager.getInstance().getChildren(
                node)) {
            if (child.isGroup()) {
                if (PointsDataManager.getInstance().getChildren(child).size() > 0) {
                    MenuItem menuItem = new MenuItem(pntsMenu, SWT.CASCADE);
                    menuItem.setText(child.getName());
                    menuItem.setData(child);
                    Menu nodeMenu = new Menu(pntsMenu);
                    menuItem.setMenu(nodeMenu);
                    point = populatePointsMenu(nodeMenu, child);
                }
            } else {
                point = child;
                final MenuItem menuItem = new MenuItem(pntsMenu, SWT.NONE);
                menuItem.setText("Point " + child.getName());
                menuItem.setData(child);
                menuItem.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        MenuItem mi = (MenuItem) event.getSource();
                        updateMenu(mi);
                        dialogSettings.setPointsSelection(mi);
                        getProductTable().clearProductTable();
                        listTableComp.addProductsToTable();
                        listTableComp.updateMenuInventory();
                    }
                });
            }
            if (firstPoint == null) {
                firstPoint = point;
            }
        }
        return firstPoint;
    }

    /**
     * Update the menu text and the associated data of the menu item located in
     * the menu bar.
     * 
     * @param mi
     *            Selection event's menu item.
     */
    private void updateMenu(MenuItem mi) {
        if (mi.getData() == null) {
            System.out.println("Data is null");
        }

        Menu menu = mi.getParent();
        while (menu.getParentMenu() != menuBar) {
            menu = menu.getParentMenu();
        }

        // Change the text of the menu item in the menu bar.
        // Set the data of the menu item in the menu bar.
        menu.getParentItem().setText(mi.getText());
        menu.getParentItem().setData(mi.getData());
    }

    /**
     * Update the toolbar menus as they may have changed.
     * 
     * @param packShellFlag
     *            Flag indicating the shell must be packed.
     */
    private void updateToolbarMenus(boolean packShellFlag) {
        shell.setMinimumSize(new org.eclipse.swt.graphics.Point(10, 10));
        shell.setSize(new org.eclipse.swt.graphics.Point(10, 10));

        ViewMenu setting = (ViewMenu) settingsMI.getData();
        SpaceTimeMenu spaceTime = null;

        if (spaceTimeMI != null) {
            spaceTime = (SpaceTimeMenu) spaceTimeMI.getData();
        }

        listTableComp.updateToolbarMenus(setting, spaceTime);

        if (packShellFlag == true) {

            getProductTable().packTable();

            shell.layout();
            shell.pack();

            getProductTable().resizeTableColumns();

            shell.setMinimumSize(shell.getSize());
        }
    }

    public VolumeBrowserDialogSettings getDialogSettings() {
        return dialogSettings;
    }

    public void showVBDialog() {
        shell.setVisible(true);
        shell.setFocus();
    }

    public DataListsProdTableComp getListTableComp() {
        return listTableComp;
    }

    protected ProductTableComp getProductTable() {
        return getListTableComp().getProductTable();
    }

    @Override
    public void updateValue(IWorkbenchWindow window, Object value) {
        LoadMode mode = (LoadMode) value;
        if (initialized
                && !(mode == LoadMode.PROG_LOOP || mode == LoadMode.FCST_TIME_MATCH)
                && dialogSettings.getSpaceTimeSelection() == SpaceTimeMenu.SPACE) {
            dialogSettings
                    .setSpaceTimeSelection(VBMenuBarItemsMgr.SpaceTimeMenu.TIME);
            for (MenuItem item : menuBar.getItems()) {
                Object menuSetting = item.getData();
                if (menuSetting instanceof VBMenuBarItemsMgr.SpaceTimeMenu) {
                    item.setData(VBMenuBarItemsMgr.SpaceTimeMenu.TIME);
                    item.setText(VBMenuBarItemsMgr.SpaceTimeMenu.TIME
                            .getDisplayString());
                    break;
                }
            }
            updateToolbarMenus(true);
        }
    }
}
