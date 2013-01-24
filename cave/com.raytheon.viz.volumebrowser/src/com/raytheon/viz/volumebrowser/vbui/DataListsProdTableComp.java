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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Pattern;

import net.sf.swtaddons.autocomplete.AutocompleteContentProposalProvider;
import net.sf.swtaddons.autocomplete.combo.AutocompleteComboInput;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.awipstools.IToolChangedListener;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.volumebrowser.datacatalog.DataCatalogManager;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.SpaceTimeMenu;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;
import com.raytheon.viz.volumebrowser.widget.MenuContributionItem;
import com.raytheon.viz.volumebrowser.widget.TitleImgContributionItem;
import com.raytheon.viz.volumebrowser.widget.ToolBarContributionItem;
import com.raytheon.viz.volumebrowser.xml.MenuContribution;
import com.raytheon.viz.volumebrowser.xml.TitleImgContribution;
import com.raytheon.viz.volumebrowser.xml.ToolBarContribution;
import com.raytheon.viz.volumebrowser.xml.VbSource;
import com.raytheon.viz.volumebrowser.xml.VbSourceList;

/**
 * This class manages the Sources, Fields, and Planes data lists and toolbar
 * menus.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2009 #2161      lvenable     Initial creation
 * Jul 31, 2012 #875       rferrel     Now uses markers.
 * Sep 26, 2012 #1216      rferrel     Change listener added to update
 *                                      points menu.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DataListsProdTableComp extends Composite implements
        IDataMenuAction {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataListsProdTableComp.class);

    private IPointChangedListener pointChangeListener;

    /**
     * Perform a regular expression find instead of simply completing the input
     */
    public class AutoFindComboInput extends AutocompleteComboInput {

        class AutoFindContentProposalProvider extends
                AutocompleteContentProposalProvider {

            /**
             * @param proposals
             */
            public AutoFindContentProposalProvider(String[] proposals) {
                super(proposals);
            }

            @Override
            protected List getMatchingProposals(String[] proposals,
                    String contents) {
                return super.getMatchingProposals(
                        getMatches(proposals, contents), "");
            }

            /**
             * @param proposals
             * @param contents
             * @return
             */
            private String[] getMatches(String[] proposals, String contents) {
                List<String> matches = new ArrayList<String>();

                StringBuilder searchString = new StringBuilder("(?i)");
                if (contents.startsWith("*")) {
                    searchString.append(".*");
                    contents = contents.substring(1);
                }
                searchString.append(Pattern.quote(contents));
                searchString.append(".*");

                for (String proposal : proposals) {
                    if (proposal.matches(searchString.toString())) {
                        matches.add(proposal);
                    }
                }
                return matches.toArray(new String[] {});
            }
        }

        /**
         * @param combo
         */
        public AutoFindComboInput(Combo combo) {
            super(combo);
        }

        /*
         * (non-Javadoc)
         * 
         * @seenet.sf.swtaddons.autocomplete.combo.AutocompleteComboInput#
         * getContentProposalProvider(java.lang.String[])
         */
        @Override
        protected AutocompleteContentProposalProvider getContentProposalProvider(
                String[] proposals) {
            return new AutoFindContentProposalProvider(proposals);
        }
    }

    /**
     * One of either source, field, or plane
     */
    public class SelectionControl {

        private final Composite composite;

        private ListController list;

        private final MultiToolbar toolbar;

        private Combo find;

        private final DataSelection data;

        public boolean hasSelectedIndexes() {
            if (list != null && list.getSelectedIndexes() != null) {
                return list.getSelectedIndexes().length > 0;
            }
            return false;
        }

        SelectionControl(Composite parent, DataSelection dataSelection) {

            data = dataSelection;

            composite = new Composite(parent, SWT.NONE);
            GridLayout gl = new GridLayout(1, false);
            gl.marginHeight = 2;
            gl.marginWidth = 2;
            composite.setLayout(gl);
            composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                    false));

            StringBuilder labelText = new StringBuilder();
            labelText.append(dataSelection.toString().charAt(0));
            labelText.append(dataSelection.toString().toLowerCase()
                    .substring(1));

            Label label = new Label(composite, SWT.CENTER);
            label.setText(labelText.toString());
            label.setFont(italicFont);
            label.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

            toolbar = new MultiToolbar(composite, SWT.NONE);
            toolbar.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
            toolbar.addToolItemSelectionListener(new SelectionListener() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    activeList = list;
                    currentDataSelection = data;
                }

                @Override
                public void widgetDefaultSelected(SelectionEvent e) {

                }
            });
            initializeFind(labelText.toString());
        }

        void addList(Composite parent) {
            list = new ListController(parent, data, new IListAction() {

                @Override
                public void listSelectionChange(boolean selected,
                        DataSelection dataSelType) {
                    if (selected == false) {
                        removeProductsFromTable(dataSelType);
                    } else {
                        addProductsToTable();
                    }
                    updateMenuInventory();

                }
            });

            listControls[data.ordinal()] = list;
        }

        /**
         * Selects the found menu and removes it from the possible find entries
         */
        private void handleFindSelection() {

            activeList = listControls[data.ordinal()];
            currentDataSelection = data;

            MenuContributionItem menuItem = (MenuContributionItem) find
                    .getData(find.getText());

            if (menuItem != null) {
                menuItem.handleWidgetSelected();
            } else {
                find.setText("");
            }
        }

        /**
         * 
         */
        private void initializeFind(String labelText) {
            find = new Combo(composite, SWT.DROP_DOWN);

            find.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
            find.setToolTipText("Find " + labelText + "...");
            find.setVisible(false);
            ((GridData) find.getLayoutData()).heightHint = 0;

            new AutoFindComboInput(find);

            find.addSelectionListener(new SelectionAdapter() {
                /*
                 * (non-Javadoc)
                 * 
                 * @see
                 * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org
                 * .eclipse.swt.events.SelectionEvent)
                 */
                @Override
                public void widgetSelected(SelectionEvent e) {
                    handleFindSelection();
                }
            });

            find.addKeyListener(new KeyAdapter() {
                /*
                 * (non-Javadoc)
                 * 
                 * @see
                 * org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse
                 * .swt.events.KeyEvent)
                 */
                @Override
                public void keyReleased(KeyEvent e) {
                    switch (e.character) {
                    case SWT.CR:

                        handleFindSelection();

                    }
                }
            });

        }

        /**
         * 
         */
        public void toggleFind() {
            if (find.isVisible()) {
                find.setVisible(false);
                ((GridData) find.getLayoutData()).heightHint = 0;
                composite.pack();
            } else {
                find.setVisible(true);
                ((GridData) find.getLayoutData()).heightHint = SWT.DEFAULT;
                composite.pack();
            }
        }

        /**
         * removes all entry from the controls find box
         */
        public void clearFind() {
            find.removeAll();
        }

        /**
         * adds a new menu item to the controls find.
         */
        public void addToFind(MenuContributionItem availableItem) {

            if (availableItem.isEnabled()) {

                StringBuilder itemName = new StringBuilder();
                itemName.append(availableItem.getMenuItemText());
                itemName.append(" (")
                        .append(availableItem.getMenuContribution().xml.key)
                        .append(")");
                if (find.indexOf(itemName.toString()) == -1) {
                    find.setData(itemName.toString(), availableItem);
                    find.add(itemName.toString());
                }
            }

        }

        public void autoSelect(List<MenuContributionItem> availableItems) {
            // perform auto selection
            if (availableItems.size() == 1) {
                MenuContributionItem item = availableItems.get(0);
                if (item.isEnabled()) {
                    // Swap out the current data selection and list for the data
                    // selection and list for the auto selected menu item. After
                    // we are done selecting it restore the old state.
                    // This swap assumes that this method is on the UI thread
                    // and that any other method which uses the
                    // currentDataSelection will be on the UI thread. If this is
                    // not true then other threads may get different values for
                    // the currentDataSelection. It would be better to allow
                    // changes to be made to menus that are not the
                    // currentDataSection but that is a complicated change.
                    DataSelection oldSelection = currentDataSelection;
                    ListController oldList = activeList;
                    activeList = listControls[data.ordinal()];
                    currentDataSelection = data;
                    availableItems.get(0).handleWidgetSelected();
                    activeList = oldList;
                    currentDataSelection = oldSelection;
                }
            }
        }

    }

    private SelectionControl sourceControl;

    private SelectionControl fieldControl;

    private SelectionControl planeControl;

    /**
     * Italic font.
     */
    private Font italicFont;

    /**
     * Current active list that operations are performed.
     */
    private ListController activeList;

    /**
     * Data selection enumeration.
     */
    public static enum DataSelection {
        SOURCES, FIELDS, PLANES;
    };

    /**
     * Current data type selected.
     */
    private DataSelection currentDataSelection;

    /**
     * Array of list controls (Sources, Fields, and Planes).
     */
    private ListController[] listControls;

    /**
     * Products table.
     */
    private ProductTableComp prodTable;

    /**
     * Parent composite.
     */
    private final Composite parentComp;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     */
    public DataListsProdTableComp(Composite parentComp) {
        super(parentComp, 0);

        this.parentComp = parentComp;

        initializeComponents();

        ToolsDataManager.getInstance().addBaselinesChangedListener(
                new IToolChangedListener() {
                    @Override
                    public void toolChanged() {
                        VizApp.runAsync(new Runnable() {
                            public void run() {
                                updateMenuInventory();
                            }
                        });
                    }
                });
        PointsDataManager.getInstance().addPointsChangedListener(
                new IPointChangedListener() {
                    @Override
                    public void pointChanged() {
                        VizApp.runAsync(new Runnable() {
                            public void run() {
                                updateMenuInventory();
                            }
                        });
                    }
                });
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createDataLists();
        createProductsTable();
    }

    /**
     * Initialize data and create the Sources, Fields, and Planes controls.
     */
    private void createDataLists() {
        MenuItemManager menuItemMgr = MenuItemManager.getInstance();
        menuItemMgr.setMenuUpdateCallback(this);

        listControls = new ListController[DataSelection.values().length];

        italicFont = new Font(this.getDisplay(), "Sans", 10, SWT.ITALIC
                | SWT.BOLD);

        GridLayout gl = new GridLayout(3, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        this.setLayout(gl);
        this.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // Create the Sources, Fields, and Planes controls.
        sourceControl = new SelectionControl(this, DataSelection.SOURCES);
        fieldControl = new SelectionControl(this, DataSelection.FIELDS);
        planeControl = new SelectionControl(this, DataSelection.PLANES);
        sourceControl.addList(this);
        fieldControl.addList(this);
        planeControl.addList(this);
        // Add a dispose listener to this class so we can dispose of any
        // classes.
        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                italicFont.dispose();
            }
        });

        activeList = sourceControl.list;
    }

    /**
     * Show / Hide the find source, field, and plane drop downs
     */
    public void toggleFind() {
        sourceControl.toggleFind();
        fieldControl.toggleFind();
        planeControl.toggleFind();
        this.pack();
        parentComp.pack();
    }

    /**
     * Create the Products table.
     */
    private void createProductsTable() {
        prodTable = new ProductTableComp(parentComp);
    }

    /**
     * Clear the Sources, Fields, and Planes lists and re-enable all of the menu
     * items.
     */
    public void clearAllLists() {
        deselectAllListItems();
        clearSourcesList(false);
        clearFieldsList(false);
        clearPlanesList(false);
        updateMenuInventory();
    }

    /**
     * Clear the Sources list and re-enable the menu items. Also clear the
     * products table.
     */
    public void clearSourcesList(boolean updateMenu) {
        currentDataSelection = DataSelection.SOURCES;
        sourceControl.list.clearList();
        MenuItemManager.getInstance().enableSourcesMenus();
        prodTable.clearProductTable();
        if (updateMenu) {
            updateMenuInventory();
        }
    }

    /**
     * Clear the Fields list and re-enable the menu items. Also clear the
     * products table.
     */
    public void clearFieldsList(boolean updateMenu) {
        currentDataSelection = DataSelection.FIELDS;
        fieldControl.list.clearList();
        MenuItemManager.getInstance().enableFieldsMenus();
        prodTable.clearProductTable();
        if (updateMenu) {
            updateMenuInventory();
        }
    }

    /**
     * Clear the Planes list and re-enable the menu items. Also clear the
     * products table.
     */
    public void clearPlanesList(boolean updateMenu) {
        currentDataSelection = DataSelection.PLANES;
        planeControl.list.clearList();
        MenuItemManager.getInstance().enablePlanesMenus();
        prodTable.clearProductTable();
        if (updateMenu) {
            updateMenuInventory();
        }
    }

    /**
     * Deselect all of the items in the Sources, Fields, and Planes lists. Also
     * clear the products table.
     */
    public void deselectAllListItems() {
        sourceControl.list.deselectAll();
        fieldControl.list.deselectAll();
        planeControl.list.deselectAll();
        prodTable.clearProductTable();
    }

    /**
     * Select all of the items in the Sources, Fields, and Planes lists.
     */
    public void selectAllListItems() {
        sourceControl.list.selectAll();
        fieldControl.list.selectAll();
        planeControl.list.selectAll();

        /*
         * Create products from the selected items and put them into the
         * products table.
         */
        addProductsToTable();
    }

    /**
     * Create the Sources toolbar menus.
     * 
     * @param setting
     *            The selected "setting".
     */
    private void createSourcesToolBarItems(ViewMenu setting) {
        // Set the current data selection.
        currentDataSelection = DataSelection.SOURCES;

        MenuItemManager menuItemMgr = MenuItemManager.getInstance();

        // Get a list of the Sources from the Sources list control.
        HashMap<String, Object> existingKeys = sourceControl.list
                .getAvailableKeys();

        // Clear the Sources map.
        menuItemMgr.clearSourcesMap();

        // Dispose of all the tool items.
        sourceControl.toolbar.disposeToolbars();

        /*
         * Read in the selected Sources menu xml and create the tool
         * items/menus.
         */

        try {
            // Linked HashMap used to preserve the ordering of categories
            Map<String, List<IContributionItem>> catMap = new LinkedHashMap<String, List<IContributionItem>>();
            Map<String, Map<String, List<IContributionItem>>> subCatMap = new LinkedHashMap<String, Map<String, List<IContributionItem>>>();
            for (VbSource source : VbSourceList.getInstance().getEntries()) {
                if (source.getViews() != null
                        && !source.getViews().contains(setting)) {
                    continue;
                }
                List<IContributionItem> catList = null;
                if (source.getSubCategory() != null) {
                    catList = catMap.get(source.getCategory());
                    if (catList == null) {
                        catList = new ArrayList<IContributionItem>();
                        catMap.put(source.getCategory(), catList);
                    }
                    Map<String, List<IContributionItem>> subMap = subCatMap
                            .get(source.getCategory());
                    if (subMap == null) {
                        subMap = new LinkedHashMap<String, List<IContributionItem>>();
                        subCatMap.put(source.getCategory(), subMap);
                    }
                    catList = subMap.get(source.getSubCategory());
                    if (catList == null) {
                        catList = new ArrayList<IContributionItem>();
                        subMap.put(source.getSubCategory(), catList);
                    }
                } else {
                    catList = catMap.get(source.getCategory());
                    if (catList == null) {
                        catList = new ArrayList<IContributionItem>();
                        catMap.put(source.getCategory(), catList);
                    }
                }
                MenuContribution mContrib = new MenuContribution();
                mContrib.xml.key = source.getKey();
                if (source.getName() != null) {
                    mContrib.xml.menuText = source.getName();
                } else {
                    // Attempt a lookup in the grib model table
                    DatasetInfo info = DatasetInfoLookup.getInstance().getInfo(
                            source.getKey());
                    if (info != null) {
                        mContrib.xml.menuText = info.getTitle();
                    } else {
                        mContrib.xml.menuText = source.getKey();
                    }
                }
                catList.add(new MenuContributionItem(mContrib));
            }
            for (Entry<String, List<IContributionItem>> entry : catMap
                    .entrySet()) {
                ToolBarContribution tbContrib = new ToolBarContribution();
                tbContrib.xml.toolItemText = entry.getKey();
                tbContrib.xml.id = setting.toString() + entry.getKey();
                Map<String, List<IContributionItem>> subMap = subCatMap
                        .get(entry.getKey());
                if (subMap != null) {
                    for (Entry<String, List<IContributionItem>> subEntry : subMap
                            .entrySet()) {
                        TitleImgContribution tContrib = new TitleImgContribution();
                        tContrib.xml.titleText = subEntry.getKey();
                        tContrib.xml.displayDashes = true;
                        tContrib.xml.displayImage = true;
                        entry.getValue().add(
                                new TitleImgContributionItem(tContrib));
                        entry.getValue().addAll(subEntry.getValue());
                    }
                }
                sourceControl.toolbar.add(new ToolBarContributionItem(
                        tbContrib, entry.getValue().toArray(
                                new IContributionItem[0])));

            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        try {
            sourceControl.toolbar.update();
        } catch (Exception e) {
            e.printStackTrace();
        }

        /*
         * Loop and disable any menu items that match the Sources that were
         * previously selected.
         */
        Set<String> keys = existingKeys.keySet();

        for (String key : keys) {
            menuItemMgr.disableMenuItem(key, DataSelection.SOURCES);
        }

        /*
         * Keep the items in the Sources list that also exist in the new menus.
         */
        if (existingKeys.isEmpty() == false) {
            sourceControl.list.retainMatchingItems(menuItemMgr
                    .getMapOfKeys(DataSelection.SOURCES));
        }
    }

    /**
     * Create the Fields toolbar menus.
     * 
     * @param setting
     *            The selected "setting".
     * @param spaceTime
     *            The select Space/Time.
     */
    private void createFieldsToolBarItems(ViewMenu setting,
            SpaceTimeMenu spaceTime) {
        fieldControl.list.clearList();

        MenuItemManager menuItemMgr = MenuItemManager.getInstance();
        menuItemMgr.clearFieldsMap();

        currentDataSelection = DataSelection.FIELDS;
        fieldControl.toolbar.disposeToolbars();

        if (setting == ViewMenu.TIMESERIES) {
            fieldControl.toolbar.populate("toolbar:#fields.menus");
        } else if (setting == ViewMenu.SOUNDING) {
            fieldControl.toolbar.populate("toolbar:#fields.menus.sounding");
        } else if (setting == ViewMenu.TIMEHEIGHT
                || setting == ViewMenu.VARVSHGT) {
            fieldControl.toolbar.populate("toolbar:#fields.menus.timeheight");
        } else if (setting == ViewMenu.PLANVIEW
                && spaceTime == SpaceTimeMenu.SPACE) {
            fieldControl.toolbar.populate("toolbar:#fields.menus.timeheight");
        } else if (setting == ViewMenu.PLANVIEW
                && spaceTime == SpaceTimeMenu.TIME) {
            fieldControl.toolbar.populate("toolbar:#fields.menus");
        } else if (setting == ViewMenu.CROSSSECTION) {
            fieldControl.toolbar.populate("toolbar:#fields.menus.xsect");
        }

        try {
            fieldControl.toolbar.update();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * Create the Planes toolbar menus.
     * 
     * @param setting
     *            The selected "setting".
     * @param spaceTime
     *            The select Space/Time.
     */
    private void createPlanesToolBarItems(ViewMenu setting,
            SpaceTimeMenu spaceTime) {
        planeControl.list.clearList();

        MenuItemManager menuItemMgr = MenuItemManager.getInstance();
        menuItemMgr.clearPlanesMap();

        currentDataSelection = DataSelection.PLANES;

        planeControl.toolbar.disposeToolbars();
        if (pointChangeListener != null) {
            PointsDataManager.getInstance().removePointsChangedListener(
                    pointChangeListener);
            pointChangeListener = null;
        }

        String pointDisplayString = null;
        switch (setting) {
        case PLANVIEW:
            if (spaceTime == SpaceTimeMenu.SPACE) {
                planeControl.toolbar
                        .populate("toolbar:#planes.menus.planview.space");
                break;
            }
        case TIMESERIES:
            planeControl.toolbar.populate("toolbar:#planes.menus.planview");
            break;
        case CROSSSECTION:
            if (spaceTime == SpaceTimeMenu.TIME) {
                planeControl.toolbar
                        .populate("toolbar:#planes.menus.xsect.time");
            } else if (spaceTime == SpaceTimeMenu.SPACE) {
                planeControl.toolbar
                        .populate("toolbar:#planes.menus.xsect.space");
            }
            break;
        case SOUNDING:
            if (pointDisplayString == null) {
                pointDisplayString = "Sounding";
            }
        case TIMEHEIGHT:
            if (pointDisplayString == null) {
                pointDisplayString = "Tsect";
            }
        case VARVSHGT:
            if (pointDisplayString == null) {
                pointDisplayString = "VarHgt";
            }
            ToolBarContribution tbContrib = new ToolBarContribution();
            tbContrib.xml.toolItemText = "Points";
            tbContrib.xml.id = "SoundingPointsButton";

            final PointToolAction pta = new PointToolAction("Points",
                    pointDisplayString);
            planeControl.toolbar.add(pta);

            pointChangeListener = new IPointChangedListener() {

                @Override
                public void pointChanged() {
                    MenuItemManager.getInstance().clearPlanesMap();
                    pta.resetMenu();
                }
            };
            PointsDataManager.getInstance().addPointsChangedListener(
                    pointChangeListener);
        }

        try {
            planeControl.toolbar.update();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Update the Sources, Fields, and Planes toolbar items.
     * 
     * @param setting
     *            The selected "setting".
     * @param spaceTime
     *            The select Space/Time.
     */
    public void updateToolbarMenus(ViewMenu setting, SpaceTimeMenu spaceTime) {
        // Clear the product in the product table since the Fields,
        // and Planes lists will be empty.
        prodTable.clearProductTable();

        createSourcesToolBarItems(setting);
        createFieldsToolBarItems(setting, spaceTime);
        createPlanesToolBarItems(setting, spaceTime);

        int sourcesWidth = sourceControl.toolbar.computeSize(-1, -1).x;
        int fieldsWidth = fieldControl.toolbar.computeSize(-1, -1).x;
        int planesWidth = planeControl.toolbar.computeSize(-1, -1).x;
        int totalWidth = sourcesWidth + fieldsWidth + planesWidth;
        int screenWidth = getDisplay().getPrimaryMonitor().getBounds().width;
        // If the total width of the toolbars is bigger than 80% of the monitor
        // make multiple rows.
        if (screenWidth * 0.90 < totalWidth) {
            // Divide by 70% of screen Width to allow some fudge factor when
            // laying it all out.
            int numBars = totalWidth / (int) (screenWidth * 0.7) + 1;
            sourceControl.toolbar.splitToMultipleBars(numBars);
            fieldControl.toolbar.splitToMultipleBars(numBars);
            planeControl.toolbar.splitToMultipleBars(numBars);
        }

        this.pack();

        updateMenuInventory();

    }

    /**
     * Add a menu selection to the active list control (Sources, Fields, or
     * Planes). If there are items selected in the Sources, Fields, and Planes
     * then a product is added to the product table if data is available.
     * 
     * @param displayStr
     *            The string to be displayed in the list.
     * @param menuContrib
     *            Menu information that contains information about the item
     *            selected.
     */
    public void addToList(String displayStr, MenuContribution menuContrib) {
        activeList.addItemToList(displayStr, menuContrib);

        addProductsToTable();
        updateMenuInventory();
    }

    /**
     * Get the active data selection.
     * 
     * @return The active data selection.
     */
    public DataSelection getActiveDataSelection() {
        return currentDataSelection;
    }

    /**
     * Add product to the products table.
     */
    public void addProductsToTable() {

        final ArrayList<SelectedData> selectedDataArray = getSelectedData();

        if (selectedDataArray.isEmpty() == false) {
            for (SelectedData selectedData : selectedDataArray) {
                prodTable.addProduct(selectedData);
            }
        }

    }

    /**
     * Get the data items that have been selected in the Sources, Fields, and
     * Planes list boxes.
     * 
     * @return ArrayList of SelectedData which contains a combination of all the
     *         selected data in the list controls.
     */
    private ArrayList<SelectedData> getSelectedData() {
        ArrayList<SelectedData> selectedData = new ArrayList<SelectedData>();

        SelectedData selData;
        String uniqueKey;

        int[] sourcesSelIdx = sourceControl.list.getSelectedIndexes();
        int[] fieldsSelIdx = fieldControl.list.getSelectedIndexes();
        int[] planesSelIdx = planeControl.list.getSelectedIndexes();

        /*
         * Loop get the find the products that are not listed in the product
         * table.
         */
        for (int i = 0; i < sourcesSelIdx.length; i++) {

            for (int j = 0; j < fieldsSelIdx.length; j++) {

                for (int k = 0; k < planesSelIdx.length; k++) {
                    uniqueKey = createUniqueKey(sourcesSelIdx[i],
                            fieldsSelIdx[j], planesSelIdx[k]);

                    selData = new SelectedData(
                            sourceControl.list.getItemText(sourcesSelIdx[i]),
                            sourceControl.list.getKey(sourcesSelIdx[i]),
                            fieldControl.list.getItemText(fieldsSelIdx[j]),
                            fieldControl.list.getKey(fieldsSelIdx[j]),
                            planeControl.list.getItemText(planesSelIdx[k]),
                            planeControl.list.getKey(planesSelIdx[k]),
                            uniqueKey);

                    selectedData.add(selData);
                }
            }
        }

        return selectedData;
    }

    /**
     * Create a unique key that will be used to track if a product is in the
     * product table.
     * 
     * @param sourceIdx
     *            Index of the selected item in the Sources list control.
     * @param fieldsIdx
     *            Index of the selected item in the Fields list control.
     * @param planesIdx
     *            Index of the selected item in the Planes list control.
     * @return The unique key string.
     */
    private String createUniqueKey(int sourceIdx, int fieldsIdx, int planesIdx) {
        StringBuilder sb = new StringBuilder();

        sb.append(sourceControl.list.createUniqueKey(sourceIdx)).append("::")
                .append(fieldControl.list.createUniqueKey(fieldsIdx))
                .append("::")
                .append(planeControl.list.createUniqueKey(planesIdx));

        return sb.toString();
    }

    /**
     * Remove the products from the products table. The data selection type
     * passed in will determine which list (Sources, Fields, Planes) the
     * unselected products are to be used.
     * 
     * @param dataSelType
     */
    private void removeProductsFromTable(DataSelection dataSelType) {
        String uniqueKey;

        int[] sourcesSelIdx = { 0 };
        int[] fieldsSelIdx = { 0 };
        int[] planesSelIdx = { 0 };

        if (dataSelType == DataSelection.SOURCES) {
            sourcesSelIdx = sourceControl.list.getUnselectedIndexes();
            fieldsSelIdx = fieldControl.list.getSelectedIndexes();
            planesSelIdx = planeControl.list.getSelectedIndexes();
        } else if (dataSelType == DataSelection.FIELDS) {
            sourcesSelIdx = sourceControl.list.getSelectedIndexes();
            fieldsSelIdx = fieldControl.list.getUnselectedIndexes();
            planesSelIdx = planeControl.list.getSelectedIndexes();
        } else if (dataSelType == DataSelection.PLANES) {
            sourcesSelIdx = sourceControl.list.getSelectedIndexes();
            fieldsSelIdx = fieldControl.list.getSelectedIndexes();
            planesSelIdx = planeControl.list.getUnselectedIndexes();
        }

        /**
         * Loop and remove the unselected products from the products table.
         */
        for (int i = 0; i < sourcesSelIdx.length; i++) {
            for (int j = 0; j < fieldsSelIdx.length; j++) {
                for (int k = 0; k < planesSelIdx.length; k++) {
                    uniqueKey = createUniqueKey(sourcesSelIdx[i],
                            fieldsSelIdx[j], planesSelIdx[k]);

                    prodTable.removeProduct(uniqueKey);
                }
            }
        }
    }

    /**
     * Refreshes the menus to indicate which menus are available based on the
     * currently selected menus and data that is available
     */
    public void updateMenuInventory() {
        String[] selectedSources = sourceControl.list.getSelectedKeys();
        String[] selectedFields = fieldControl.list.getSelectedKeys();
        String[] selectedPlanes = planeControl.list.getSelectedKeys();
        DataCatalogManager.getDataCatalogManager().updateAvailableData(
                selectedSources, selectedFields, selectedPlanes);

    }

    /**
     * Marks all data as unavailable, executes in the UI thread synchronously
     * 
     */
    public void clearAvailableData() {
        final MenuItemManager itemManager = MenuItemManager.getInstance();
        getDisplay().syncExec(new Runnable() {

            @Override
            public void run() {
                itemManager.markAllMenuItemsToNoData();
                sourceControl.clearFind();
                fieldControl.clearFind();
                planeControl.clearFind();
            }
        });
    }

    /**
     * auto selects a source, field, or plane if there is only one available,
     * executes in the UI thread synchronously
     * 
     */
    public void performAutoSelect() {
        final MenuItemManager itemManager = MenuItemManager.getInstance();
        getDisplay().syncExec(new Runnable() {

            @Override
            public void run() {
                // To prevent list from re-populating after clear all, verify at
                // least one item is selected.
                if (sourceControl.hasSelectedIndexes()
                        || fieldControl.hasSelectedIndexes()
                        || planeControl.hasSelectedIndexes()) {
                    sourceControl.autoSelect(itemManager
                            .getAvailableItems(DataSelection.SOURCES));
                    fieldControl.autoSelect(itemManager
                            .getAvailableItems(DataSelection.FIELDS));
                    planeControl.autoSelect(itemManager
                            .getAvailableItems(DataSelection.PLANES));
                }
            }
        });
    }

    /**
     * Marks a a data item(key) as available in the control, executes in the UI
     * thread synchronously
     * 
     * @param key
     * @param control
     */
    private void markAvailableData(final String key,
            final SelectionControl control) {
        final MenuItemManager itemManager = MenuItemManager.getInstance();
        getDisplay().syncExec(new Runnable() {

            @Override
            public void run() {
                Collection<MenuContributionItem> items = itemManager
                        .menuItemHasData(key);
                if (items != null) {
                    for (MenuContributionItem item : items) {
                        control.addToFind(item);
                    }
                }
            }
        });
    }

    /**
     * Marks a source as having available data, executes in the UI thread
     * synchronously
     * 
     * @param source
     */
    public void markAvailableSource(String source) {
        markAvailableData(source, sourceControl);
    }

    /**
     * Marks a field as having available data, executes in the UI thread
     * synchronously
     * 
     * @param source
     */
    public void markAvailableField(String field) {
        markAvailableData(field, fieldControl);
    }

    /**
     * Marks a plane as having available data, executes in the UI thread
     * synchronously
     * 
     * @param source
     */
    public void markAvailablePlane(String plane) {
        markAvailableData(plane, planeControl);
    }

    public ProductTableComp getProductTable() {
        return prodTable;
    }

}
