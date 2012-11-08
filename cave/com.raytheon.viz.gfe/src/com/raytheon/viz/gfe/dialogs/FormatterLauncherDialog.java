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
package com.raytheon.viz.gfe.dialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.textproduct.ProductDefinition;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.IProductTab;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.IssuanceSiteIdDlg;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ProductAreaComp;
import com.raytheon.viz.gfe.textformatter.TextProductManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * The formatter launcher dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 28, 2008            ebabin      Initial Creation
 * Jul 16, 2008            njensen     Dynamic products menu
 * Jan 15, 2010 3395       ryu         Fix &quot;issued by&quot; functionality
 * Jun 19, 2010 4684       mduff       Corrected the Data Sources menu for 
 *                                     practice and test modes
 * Sep 16, 2010 6831       ryu         Show same product for different areas on a sub-menu
 * Nov 22, 2011 8781       mli		   remove Processor menu
 * Jul 26, 2012 15165      ryu         Set default db source when formatter has no db defined.
 * Oct 23, 2012 1287       rferrel     Changes for non-blocking dialogs and code clean up.
 * Nov 08, 2012 1298       rferrel     Changes for non-blocking IssuanceSiteIdDlg.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class FormatterLauncherDialog extends CaveJFACEDialog implements
        IProductTab {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FormatterLauncherDialog.class);

    private IssuanceSiteIdDlg issuedByDlg;

    private final String BASELINE = "Baseline";

    private final String CIVIL_EMERGENCY = "CivilEmergency";

    private final String HAZARD = "Hazard";

    private final String BASELINE_HAZARD = BASELINE + HAZARD;

    private final String REGION = "Region";

    private final String OTHERS = "";

    private final String PRODUCT_EDITOR = "Product Editor";

    /**
     * Tab folder containing the product tabs.
     */
    private TabFolder tabFolder;

    /**
     * Running image.
     */
    private Image runningImg;

    /**
     * Queued image.
     */
    private Image queuedImg;

    /**
     * New tab image.
     */
    private Image newTabImg;

    /**
     * Finished image.
     */
    private Image finishedImg;

    /**
     * Transmitted image.
     */
    private Image transmittedImg;

    /**
     * Filed image.
     */
    private Image failedImg;

    /**
     * Fcst data source menu item.
     */
    private MenuItem fcstMI = null;

    /**
     * Official data source menu item.
     */
    private MenuItem officialMI = null;

    /**
     * ISC data source menu item.
     */
    private MenuItem iscMI = null;

    /**
     * Default data source menu item.
     */
    private MenuItem defaultMI = null;

    /**
     * Products menu.
     */
    private Menu productsMenu;

    /**
     * Map of product composites.
     */
    private HashMap<String, ProductAreaComp> productMap;

    /**
     * Status label.
     */
    private Label statusLbl;

    /**
     * Status text control.
     */
    private Text textStatusTF;

    private Composite top;

    private ImageRegistry registry;

    private ArrayList<String> statusMessages = new ArrayList<String>();

    private ViewMessagesDialog viewMessageDialog;

    private TextProductManager textProductMgr;

    private String selectedDataSource = null;

    private boolean doClose = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public FormatterLauncherDialog(Shell parent) {
        super(parent);
        setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE | SWT.RESIZE);

        textProductMgr = new TextProductManager();
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        // Code to allow the dialog the have keymappings
        IContextService svc = (IContextService) PlatformUI.getWorkbench()
                .getService(IContextService.class);
        svc.registerShell(getShell(), IContextService.TYPE_DIALOG);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        top.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

        return top;
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        productMap = new HashMap<String, ProductAreaComp>();

        runningImg = getImageRegistry().get("running");
        queuedImg = getImageRegistry().get("queued");
        newTabImg = getImageRegistry().get("newtab");
        finishedImg = getImageRegistry().get("finished");
        transmittedImg = getImageRegistry().get("transmitted");
        failedImg = getImageRegistry().get("failed");
        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
        createMenus();

        createFormatterTabFolder();

        createStatusComp();
        createHideButton();
    }

    /**
     * Create the main menu bar and menu items.
     */
    private void createMenus() {
        Menu menuBar = new Menu(super.getShell(), SWT.BAR);

        createProductsMenu(menuBar);
        createDataSourceMenu(menuBar);
        createIssuedByMenu(menuBar);
        createHelpMenu(menuBar);

        super.getShell().setMenuBar(menuBar);
    }

    /**
     * Create the Products menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createProductsMenu(Menu menuBar) {
        // Create the Products menu item with a Products "dropdown" menu
        productsMenu = new Menu(menuBar);
        productsMenu.addMenuListener(new MenuAdapter() {
            @Override
            public void menuShown(MenuEvent event) {
                createProductsMenuItems();
            }
        });

        MenuItem productsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        LocalizationManager lm = LocalizationManager.getInstance();
        String site = lm.getCurrentSite();
        productsMenuItem.setText("&Products (" + site
                + " and Surrounding CWA's) ");
        productsMenuItem.setMenu(productsMenu);
    }

    /**
     * Create the Data Source menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createDataSourceMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Data Source menu
        // -------------------------------------
        MenuItem dataSourceMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        dataSourceMenuItem.setText("&Data Source");

        // Create the Data Source menu item with a Products "dropdown" menu
        Menu dataSourceMenu = new Menu(menuBar);
        dataSourceMenuItem.setMenu(dataSourceMenu);

        // ------------------------------------------------------
        // Create all the items in the Data Source dropdown menu
        // ------------------------------------------------------

        DataManager dm = DataManager.getCurrentInstance();

        // Get the CAVE operating mode
        CAVEMode mode = dm.getOpMode();

        // Forecast menu item, set text based on operating mode
        fcstMI = new MenuItem(dataSourceMenu, SWT.RADIO);
        if (mode.equals(CAVEMode.OPERATIONAL)) {
            fcstMI.setText("Fcst");
        } else if (mode.equals(CAVEMode.PRACTICE)) {
            fcstMI.setText("Fcst_Prac");
            fcstMI.setSelection(true);
        } else {
            fcstMI.setText("Fcst_Test");
            fcstMI.setSelection(true);
        }
        fcstMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectedDataSource = DataManager.getCurrentInstance()
                        .getParmManager().getMutableDatabase().toString();
            }
        });

        // Only show these menu items when in operational mode
        if (mode.equals(CAVEMode.OPERATIONAL)) {
            // ISC menu item
            iscMI = new MenuItem(dataSourceMenu, SWT.RADIO);
            iscMI.setText("ISC");
            iscMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    getIscDataSource();
                }
            });

            // Official menu item
            officialMI = new MenuItem(dataSourceMenu, SWT.RADIO);
            officialMI.setText("Official");
            officialMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    selectedDataSource = getOfficialDataSource();
                }
            });

            // Default menu item
            defaultMI = new MenuItem(dataSourceMenu, SWT.RADIO);
            defaultMI.setText("Default");
            defaultMI.setSelection(true);
            defaultMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                }
            });
        } else {
            selectedDataSource = DataManager.getCurrentInstance()
                    .getParmManager().getMutableDatabase().toString();
        }
    }

    /**
     * Create the Issued By menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createIssuedByMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Issued By menu
        // -------------------------------------
        MenuItem issuedByMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        issuedByMenuItem.setText("&Issued By");

        // Create the Processor menu item with a Products "dropdown" menu
        Menu issuedByMenu = new Menu(menuBar);
        issuedByMenuItem.setMenu(issuedByMenu);

        // ------------------------------------------------------
        // Create all the items in the Issued By dropdown menu
        // ------------------------------------------------------

        // Issuance Site ID menu item
        MenuItem issuanceSiteMI = new MenuItem(issuedByMenu, SWT.NONE);
        issuanceSiteMI.setText("Issuance Site ID...");
        issuanceSiteMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (issuedByDlg == null || issuedByDlg.getShell() == null
                        || issuedByDlg.isDisposed()) {
                    String issuedBy = textProductMgr.getIssuedBy();
                    issuedByDlg = new IssuanceSiteIdDlg(
                            FormatterLauncherDialog.this.getShell(), issuedBy);
                    issuedByDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String) {
                                textProductMgr.setIssuedBy(returnValue
                                        .toString());
                            }
                        }
                    });
                    issuedByDlg.open();
                } else {
                    issuedByDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the Help menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createHelpMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Help menu
        // -------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        // Create the Processor menu item with a Help "dropdown" menu
        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        // ------------------------------------------------------
        // Create a "Tab Color Key" menu
        // ------------------------------------------------------
        MenuItem tabColorKeyMI = new MenuItem(helpMenu, SWT.CASCADE);
        tabColorKeyMI.setText("Tab Color Key");

        Menu tabColorSubMenu = new Menu(super.getShell(), SWT.DROP_DOWN);
        tabColorKeyMI.setMenu(tabColorSubMenu);

        // --------------------------------------------------------
        // Create the menu items under the "Tab Color Key" menu
        // --------------------------------------------------------
        MenuItem newTabMI = new MenuItem(tabColorSubMenu, SWT.NONE);
        newTabMI.setText("New Tab");
        newTabMI.setImage(newTabImg);

        MenuItem queuedMI = new MenuItem(tabColorSubMenu, SWT.NONE);
        queuedMI.setText("Queued");
        queuedMI.setImage(queuedImg);

        MenuItem runningMI = new MenuItem(tabColorSubMenu, SWT.NONE);
        runningMI.setText("Running");
        runningMI.setImage(runningImg);

        MenuItem finishedMI = new MenuItem(tabColorSubMenu, SWT.NONE);
        finishedMI.setText("Finished");
        finishedMI.setImage(finishedImg);

        MenuItem transmittedMI = new MenuItem(tabColorSubMenu, SWT.NONE);
        transmittedMI.setText("Transmitted");
        transmittedMI.setImage(transmittedImg);

        MenuItem failedMI = new MenuItem(tabColorSubMenu, SWT.NONE);
        failedMI.setText("Failed");
        failedMI.setImage(failedImg);
    }

    /**
     * Create the menu items under 'Products' on the menu bar.
     */
    private void createProductsMenuItems() {
        MenuItem[] menuItems = productsMenu.getItems();
        for (MenuItem menuItem : menuItems) {
            menuItem.dispose();
        }

        // Get extra config file categories; create a Map to manage their lists
        String[] categories = Activator.getDefault().getPreferenceStore()
                .getStringArray("FormatterLauncherDialog_Categories");
        Map<String, ArrayList<String>> categoryMap = new HashMap<String, ArrayList<String>>();

        String[] names = textProductMgr.getProductNames();
        if (names.length > 0) {
            ArrayList<String> civilEmergencies = new ArrayList<String>();
            ArrayList<String> hazards = new ArrayList<String>();
            ArrayList<String> baselines = new ArrayList<String>();
            ArrayList<String> baselineHazards = new ArrayList<String>();
            ArrayList<String> regions = new ArrayList<String>();
            ArrayList<String> others = new ArrayList<String>();

            for (String productName : names) {
                if (productName.startsWith(CIVIL_EMERGENCY)) {
                    civilEmergencies.add(productName);
                } else if (productName.startsWith(HAZARD)) {
                    hazards.add(productName);
                } else if (productName.startsWith(REGION)) {
                    regions.add(productName);
                } else if (productName.startsWith(BASELINE_HAZARD)) {
                    baselineHazards.add(productName);
                } else if (productName.startsWith(BASELINE)) {
                    baselines.add(productName);
                } else {
                    boolean found = false;
                    for (String category : categories) {
                        if (productName.startsWith(category)) {
                            if (categoryMap.get(category) == null) {
                                categoryMap.put(category,
                                        new ArrayList<String>());
                            }
                            categoryMap.get(category).add(productName);
                            found = true;
                        }
                    }
                    if (!found) {
                        others.add(productName);
                    }
                }
            }

            createCategoriedProducts(OTHERS, others);
            new MenuItem(productsMenu, SWT.SEPARATOR);
            createCategoriedProducts(CIVIL_EMERGENCY, civilEmergencies);
            createCategoriedProducts(HAZARD, hazards);

            new MenuItem(productsMenu, SWT.SEPARATOR);
            createCategoriedProducts(BASELINE, baselines);
            createCategoriedProducts(BASELINE_HAZARD, baselineHazards);
            createCategoriedProducts(REGION, regions);

            // Create user-configured category menus
            if (categoryMap.size() > 0) {
                new MenuItem(productsMenu, SWT.SEPARATOR);
                for (String category : categories) {
                    ArrayList<String> categoryProducts = categoryMap
                            .get(category);
                    if (categoryProducts != null) {
                        createCategoriedProducts(category, categoryProducts);
                    }
                }
            }
        }

        // Product correction menu item
        new MenuItem(productsMenu, SWT.SEPARATOR);
        createCorrectionMenuItem();
    }

    private void createCategoriedProducts(String name,
            ArrayList<String> productNames) {
        if (productNames.size() == 0) {
            return;
        }

        Menu baseMenu;
        MenuItem baseItem = null;
        if (name.equals(OTHERS)) {
            baseMenu = productsMenu;
        } else {
            baseItem = new MenuItem(productsMenu, SWT.CASCADE);
            baseItem.setText(name);
            baseMenu = new Menu(baseItem);
            baseItem.setMenu(baseMenu);
        }

        String groupName = null;
        int start = 0;
        for (int i = 0; i < productNames.size(); i++) {
            String[] tArray = productNames.get(i).split("_");
            String gname;
            if (name.equals(tArray[0]) && tArray.length > 1) {
                gname = tArray[0] + "_" + tArray[1];
            } else {
                gname = tArray[0];
            }
            if (!gname.equals(groupName)) {
                // start of new group.
                if (groupName != null) {
                    // Create menu item or sub-menu for last item or group.
                    createItemOrSubmenu(baseMenu, groupName, productNames,
                            start, i - 1);
                }

                // new group
                groupName = gname;
                start = i;
            }
        }

        // Create the last item or sub-menu
        createItemOrSubmenu(baseMenu, groupName, productNames, start,
                productNames.size() - 1);
    }

    private void createItemOrSubmenu(Menu baseMenu, String groupName,
            ArrayList<String> productNames, int start, int end) {
        Menu subMenu;
        MenuItem item, subitem;
        if ((end - start) > 0) {
            item = new MenuItem(baseMenu, SWT.CASCADE);
            subMenu = new Menu(item);
            item.setText(groupName);
            item.setMenu(subMenu);
            for (int j = start; j <= end; j++) {
                String s = productNames.get(j);
                subitem = new MenuItem(subMenu, SWT.NONE);
                subitem.setText(s);
                subitem.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        createProductTab(((MenuItem) event.getSource())
                                .getText());
                    }
                });
            }
        } else {
            item = new MenuItem(baseMenu, SWT.NONE);
            item.setText(productNames.get(start));
            item.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    createProductTab(((MenuItem) event.getSource()).getText());
                }
            });
        }
    }

    private void createCorrectionMenuItem() {
        MenuItem item = new MenuItem(productsMenu, SWT.NONE);
        item.setText("Product Editor / Make Correction");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                createProductTab(PRODUCT_EDITOR);
            }
        });
    }

    /**
     * Create the tab folder that will contain the product tabs.
     */
    private void createFormatterTabFolder() {
        Rectangle screenRect = getShell().getDisplay().getPrimaryMonitor()
                .getBounds();

        int mapWidth = (int) (screenRect.width * 0.60);
        if (GFEPreference.contains("ZoneCombiner_width")) {
            mapWidth = GFEPreference.getIntPreference("ZoneCombiner_width");
        }
        int mapHeight = (int) (screenRect.height * 0.60);
        if (GFEPreference.contains("ZoneCombiner_height")) {
            mapHeight = GFEPreference.getIntPreference("ZoneCombiner_height");
        }
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        // Old fixed tab size was 700x900.
        // Pad zcHeight and zcWidth so default matches.
        gd.heightHint = mapHeight;
        gd.widthHint = mapWidth;
        Composite tabComp = new Composite(top, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        tabComp.setLayout(gl);
        tabComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        tabFolder = new TabFolder(tabComp, SWT.NONE);
        tabFolder.setLayoutData(gd);
    }

    /**
     * Create the product tab.
     * 
     * @param tabName
     *            Tab name.
     */
    private void createProductTab(String tabName) {
        if (productMap.containsKey(tabName) == true) {
            return;
        }

        TabItem newTab = new TabItem(tabFolder, SWT.NONE);
        newTab.setText(tabName);
        setTabImage(newTab, ConfigData.productStateEnum.New);
        newTab.setImage(newTabImg);

        ProductAreaComp comp = new ProductAreaComp(tabFolder, this,
                newTab.getText(), tabName.equals(PRODUCT_EDITOR),
                textProductMgr, CAVEMode.getMode().equals(CAVEMode.PRACTICE));
        productMap.put(tabName, comp);

        newTab.setControl(productMap.get(tabName));
        tabFolder.setSelection(newTab);
    }

    /**
     * Get the selected data source for this product type.
     * 
     * @param productName
     *            The name of the product
     * @return The data source
     */
    public String getSelectedDataSource(String productName) {
        if (fcstMI.getSelection()) {
            selectedDataSource = getFcstDataSource();
        } else if (iscMI.getSelection()) {
            selectedDataSource = getIscDataSource();
        } else if (officialMI.getSelection()) {
            selectedDataSource = getOfficialDataSource();
        } else {
            // Default value
            ProductDefinition prodDef = textProductMgr
                    .getProductDefinition(productName);
            String dataSource = (String) prodDef.get("database");
            if (dataSource == null)
                dataSource = "Official";

            if (dataSource.equals("ISC")) {
                selectedDataSource = getIscDataSource();
            } else if (dataSource.equals("Official")) {
                selectedDataSource = getOfficialDataSource();
            } else {
                selectedDataSource = getFcstDataSource();
            }
        }

        return selectedDataSource;
    }

    /**
     * Create the status composite.
     */
    private void createStatusComp() {
        Composite statusComp = new Composite(top, SWT.NONE);
        statusComp.setLayout(new GridLayout(4, false));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        statusComp.setLayoutData(gd);

        gd = new GridData(30, SWT.DEFAULT);
        statusLbl = new Label(statusComp, SWT.BORDER);
        statusLbl.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_GREEN));
        statusLbl.setLayoutData(gd);

        Label textStatusLbl = new Label(statusComp, SWT.NONE);
        textStatusLbl.setText("Text Status");

        Button viewMessagesBtn = new Button(statusComp, SWT.ARROW | SWT.UP);
        viewMessagesBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showViewMessagesDialog();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        textStatusTF = new Text(statusComp, SWT.BORDER);
        textStatusTF.setLayoutData(gd);
    }

    /**
     * Remove a product tab.
     * 
     * @param tabName
     *            Name of the tab to be removed.
     */
    public void removeProductTab(String tabName) {
        TabItem[] items = tabFolder.getItems();

        for (int i = 0; i < items.length; i++) {
            if (items[i].getText().compareTo(tabName) == 0) {
                productMap.remove(tabName);
                items[i].getControl().dispose();
                items[i].dispose();
                break;
            }
        }
    }

    /**
     * Set the state of the tab (changes the tab image).
     * 
     * @param state
     *            Product state.
     * @param tabName
     *            Name of the tab.
     */
    public void setTabState(ConfigData.productStateEnum state, String tabName) {
        TabItem[] items = tabFolder.getItems();

        for (int i = 0; i < items.length; i++) {
            if (items[i].getText().compareTo(tabName) == 0) {
                setTabImage(items[i], state);
                break;
            }
        }
    }

    /**
     * Set the image of the product tab.
     * 
     * @param ti
     *            Tab item.
     * @param state
     *            Product state.
     */
    private void setTabImage(TabItem ti, ConfigData.productStateEnum state) {
        if (state == ConfigData.productStateEnum.New) {
            ti.setImage(newTabImg);
        } else if (state == ConfigData.productStateEnum.Queued) {
            ti.setImage(queuedImg);
        } else if (state == ConfigData.productStateEnum.Running) {
            ti.setImage(runningImg);
        } else if (state == ConfigData.productStateEnum.Finished) {
            ti.setImage(finishedImg);
        } else if (state == ConfigData.productStateEnum.Transmitted) {
            ti.setImage(transmittedImg);
        } else if (state == ConfigData.productStateEnum.Failed) {
            ti.setImage(failedImg);
        }
    }

    private ImageRegistry getImageRegistry() {
        if (registry == null) {
            registry = new ImageRegistry();
            registry.put("running", AbstractUIPlugin.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/running.gif"));
            registry.put("queued", AbstractUIPlugin.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/queued.gif"));
            registry.put("newtab", AbstractUIPlugin.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/newTab.gif"));
            registry.put("finished", AbstractUIPlugin
                    .imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                            "icons/finished.gif"));
            registry.put("failed", AbstractUIPlugin.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/failed.gif"));
            registry.put("transmitted", AbstractUIPlugin
                    .imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                            "icons/transmitted.gif"));
        }

        return registry;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);

        shell.setText("Formatter Launcher");
    }

    /**
     * For some reason, the button bar has sometimes moved itself to the top of
     * the screen. To combat this problem, we have incorporated the button bar
     * into the dialog area, stubbing out createButtonBar() and
     * createButtonsForButtonBar().
     */
    @Override
    protected Control createButtonBar(Composite parent) {
        return null;
    }

    /**
     * Have the Hide button act like an OK button so it will call the close
     * method when selected.
     */
    private void createHideButton() {
        Composite bar = (Composite) super.createButtonBar(top);
        createButton(bar, IDialogConstants.OK_ID, "Hide", false);
    }

    /**
     * @see createButtonBar
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == IDialogConstants.CLIENT_ID) {
            getShell().setVisible(false);
            return;
        }
        super.buttonPressed(buttonId);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.dialogs.formatterlauncher.IProductTab#updateStatus
     * (java.lang.String)
     */
    @Override
    public void updateStatus(String significance, String status) {
        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss ");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        String s = sdf.format(SimulatedTime.getSystemTime().getTime())
                + significance + " " + status;
        statusMessages.add(0, s);
        textStatusTF.setText(s);
    }

    private void showViewMessagesDialog() {
        if (viewMessageDialog == null || viewMessageDialog.getShell() == null
                || viewMessageDialog.isDisposed()) {
            viewMessageDialog = new ViewMessagesDialog(getParentShell());
            viewMessageDialog.setBlockOnOpen(false);
            viewMessageDialog.open();
        } else {
            viewMessageDialog.setMessageItems();
            viewMessageDialog.bringToTop();
        }
    }

    private void dispose() {
        textProductMgr.dispose();
    }

    class ViewMessagesDialog extends CaveJFACEDialog {

        private List messageList;

        public ViewMessagesDialog(Shell parentShell) {
            super(parentShell);
            setShellStyle(SWT.DIALOG_TRIM);
        }

        @Override
        protected void configureShell(Shell shell) {
            super.configureShell(shell);
            shell.setText("View Messages");
            // Create the main layout for the shell.
            GridLayout mainLayout = new GridLayout(1, false);
            mainLayout.marginHeight = 2;
            mainLayout.marginWidth = 2;
            mainLayout.verticalSpacing = 2;
            shell.setLayout(mainLayout);
            shell.setSize(500, 400);
            shell.setLocation(50, 50);
            // Initialize all of the controls and layouts
            messageList = new List(shell, SWT.BORDER | SWT.SINGLE
                    | SWT.V_SCROLL | SWT.H_SCROLL);
            GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
            gd.heightHint = 320;
            gd.widthHint = 475;
            messageList.setLayoutData(gd);
            setMessageItems();
        }

        public void setMessageItems() {
            String[] messages = new String[statusMessages.size()];
            statusMessages.toArray(messages);

            messageList.removeAll();
            messageList.setItems(messages);
        }
    }

    /**
     * Get the FcstDataSource
     * 
     * @return The FcstDataSource
     */
    private String getFcstDataSource() {
        return DataManager.getCurrentInstance().getParmManager()
                .getMutableDatabase().toString();
    }

    /**
     * Get the ISC Data Source
     * 
     * Note: The call to getIscDatabases returns an array of DatabaseID objects.
     * Here we've decided we are only taking the last one in the list.
     * 
     * @return The ISC Data Source
     */
    private String getIscDataSource() {
        java.util.List<DatabaseID> dbs = DataManager.getCurrentInstance()
                .getParmManager().getIscDatabases();

        if (dbs.size() > 0) {
            // Always return the last one in the list
            return dbs.get(dbs.size() - 1).toString();
        }

        return null;
    }

    /**
     * Get the Official Data source.
     * 
     * @return The Official Data source
     */
    private String getOfficialDataSource() {
        String source = null;
        try {
            ServerResponse<java.util.List<DatabaseID>> sr = DataManager
                    .getCurrentInstance().getClient().getOfficialDBName();
            source = sr.getPayload().get(0).toString();
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to determine official db", e);
        }

        return source;
    }

    /**
     * Perform a real close of the dialog instead of just hiding it.
     * 
     * @return
     */
    public boolean closeDialog() {
        doClose = true;
        return close();
    }

    /**
     * This hides the fromat launcher dialog and any dialogs it creates.
     */
    public void hideDialog() {
        if (viewMessageDialog != null) {
            viewMessageDialog.hide();
        }
        hide();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#close()
     */
    @Override
    public boolean close() {
        if (doClose) {
            dispose();
            return super.close();
        }

        // make clicking the x in the upper right corner just hide the dialog
        // instead of closing it
        hideDialog();
        return false;
    }

    public void closeFormatters() {
        Set<String> keys = new HashSet<String>();
        keys.addAll(productMap.keySet());
        for (String key : keys) {
            removeProductTab(key);
        }
    }
}
