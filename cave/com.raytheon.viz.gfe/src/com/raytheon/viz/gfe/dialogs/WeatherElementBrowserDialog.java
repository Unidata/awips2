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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * The weather element browser dialog.
 *
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/20/2008              Eric Babin  Initial Creation
 * 06/27/2008              ebabin      Updated to properly add fields.
 * 04/30/2009   2282       rjpeter     Refactored.
 * 08/19/2009   2547       rjpeter     Fix Test/Prac database display.
 * 02/22/2012   14351      mli         update with incoming new grids.
 * 09/12/2012   1117       dgilling    Revert previous changes, retrieve
 *                                     database list from ParmManager
 *                                     not EDEX.
 * 10/30/2012   1298       rferrel     Code clean up non-blocking dialog.
 *                                      Changes for non-blocking WeatherElementGroupDialog.
 * 03/07/2016   5444       randerso    Fix hard coded sizes of GUI elements
 * 02/16/2018   6849       dgilling    Ensure items in Sources and Fields menus
 *                                     are properly enabled and disabled.
 * </pre>
 *
 * @author ebabin
 */

public class WeatherElementBrowserDialog extends CaveJFACEDialog {
    private static final int NUM_LIST_ITEMS = 10;

    private static final int NUM_SEL_ITEMS = 20;

    private static final int NUM_CHARS = 20;

    private static final String IFP = "IFP";

    private DataManager dataManager;

    private Menu sourceMenu, fieldsMenu, presMenu, miscMenu;

    private ToggleSelectList sourceList, fieldsList, planesList,
            productSelectionList;

    private String[] sites;

    private WEBrowserTypeRecord selectedType;

    private final java.util.List<WEBrowserTypeRecord> typeEntries = new ArrayList<>();

    private final java.util.List<String> previousSources = new ArrayList<>();

    private final java.util.List<String> previousFields = new ArrayList<>();

    private final java.util.List<String> previousPlanes = new ArrayList<>();

    private HashMap<String, ParmID> productIDs = new HashMap<>();

    private ToolBar sourceToolBar, fieldsToolBar, planesToolBar;

    private ToolItem sourceToolItem, fieldsToolItem, presToolItem,
            miscToolItem;

    private ParmID[] availableParmIds;

    private ParmID[] currentDisplayedParms;

    private int charWidth;

    /**
     * Constructor
     *
     * @param parent
     * @param dataManager
     */
    public WeatherElementBrowserDialog(Shell parent, DataManager dataManager) {
        super(parent);
        setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE | SWT.RESIZE);
        this.dataManager = dataManager;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(4, false);
        top.setLayout(layout);
        initializeComponents(top);

        return top;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "Load and Dismiss", true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void okPressed() {
        ParmID[] selectedParmIdList = getSelectedParmIDS();
        dataManager.getParmManager().setDisplayedParms(selectedParmIdList);

        super.okPressed();
    }

    @Override
    protected void cancelPressed() {
        super.cancelPressed();
    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Weather Element Browser");
    }

    /**
     * Initialize the components on startup.
     */
    private void initializeComponents(Composite parent) {
        getDatabases();
        loadTypesAndSites();

        GC gc = new GC(parent);
        charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();

        createTypesComp(parent);
        createSourceComp(parent);
        createFieldsComp(parent);
        createPlanesComp(parent);
        createProductSelctionComp(parent);
        createMenus();
        setUpSelections(getCurrentDisplayedParms());
    }

    /**
     * Show the save weather element dialog.
     */
    private void showSaveWeatherElementGroup(
            final Menu loadWeatherElementGroupMenu) {
        // The dialog being opened is modal to the parent dialog. This will
        // prevent the launching of another dialog until the modal dialog is
        // closed.
        final WeatherElementGroupDialog dialog = new WeatherElementGroupDialog(
                getShell(), dataManager, true);
        dialog.setBlockOnOpen(false);
        dialog.addCloseCallback(new ICloseCallback() {

            @Override
            public void dialogClosed(Object returnValue) {
                if (returnValue instanceof Integer) {
                    int returnCode = (Integer) returnValue;
                    if (returnCode == Window.OK) {
                        String groupName = dialog.getSelectedItem();
                        doSaveWeatherElementGroup(loadWeatherElementGroupMenu,
                                groupName);
                    }
                }
            }
        });
        dialog.open();
    }

    private void doSaveWeatherElementGroup(Menu loadWeatherElementGroupMenu,
            String groupName) {
        if (groupName != null) {
            ParmID[] selectedParmIds = getSelectedParmIDS();
            if ((selectedParmIds != null) && (selectedParmIds.length > 0)) {
                ParmID[] availIds = selectedType.getPossibleParmIDs();
                if ((availIds != null) && (availIds.length != 0)) {
                    dataManager.getWEGroupManager().save(groupName,
                            selectedParmIds, availIds);
                    boolean alreadyListed = false;
                    for (MenuItem item : loadWeatherElementGroupMenu.getItems()) {
                        if (item.getText().equals(groupName)) {
                            alreadyListed = true;
                        }
                    }
                    if (!alreadyListed) {
                        addWEGroup(loadWeatherElementGroupMenu, groupName);
                    }
                }
            }
        }
    }

    /**
     * Show delete weather element dialog.
     */
    private void showDeleteWeatherElementGroup(
            final Menu loadWeatherElementGroupMenu) {
        // The dialog being opened is modal to the parent dialog. This will
        // prevent the launching of another dialog until the modal dialog is
        // closed.
        final WeatherElementGroupDialog dialog = new WeatherElementGroupDialog(
                getShell(), this.dataManager, false);
        dialog.setBlockOnOpen(false);
        dialog.addCloseCallback(new ICloseCallback() {

            @Override
            public void dialogClosed(Object returnValue) {
                if (returnValue instanceof Integer) {
                    int returnCode = (Integer) returnValue;
                    if ((returnCode == Window.OK)
                            && (dialog.getSelectedItem() != null)) {
                        String groupName = dialog.getSelectedItem();
                        // we may have just overridden a site or base level
                        // group, need to verify menu item can be deleted
                        if (dataManager.getWEGroupManager().remove(groupName)
                                && !dataManager.getWEGroupManager()
                                        .getInventory().contains(groupName)) {
                            removeWEGroup(loadWeatherElementGroupMenu,
                                    groupName);
                        }
                    }
                }
            }
        });
        dialog.open();
    }

    /**
     * Creates the menus for the dialog.
     */
    private void createMenus() {
        // create menu bar
        Shell shell = super.getShell();

        Menu menuBar = new Menu(shell, SWT.BAR);

        // create main menus
        Menu fileMenu = new Menu(shell, SWT.DROP_DOWN);
        Menu editMenu = new Menu(shell, SWT.DROP_DOWN);
        Menu siteMenu = new Menu(getShell(), SWT.DROP_DOWN);

        // create file sub menus
        final Menu loadWeatherElementGroupMenu = new Menu(shell, SWT.DROP_DOWN);

        // create main menu items
        MenuItem fileMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuHeader.setText("&File");
        fileMenuHeader.setMenu(fileMenu);

        MenuItem editMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        editMenuHeader.setText("&Edit");
        editMenuHeader.setMenu(editMenu);

        MenuItem siteMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        siteMenuHeader.setText("&Site");
        siteMenuHeader.setMenu(siteMenu);

        // create file menu items
        MenuItem fileSaveWeatherElementGroup = new MenuItem(fileMenu, SWT.NONE);
        fileSaveWeatherElementGroup.setText("Save Weather Element Group...");
        fileSaveWeatherElementGroup
                .addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent arg0) {
                        showSaveWeatherElementGroup(loadWeatherElementGroupMenu);
                    }
                });
        MenuItem fileDeleteWeatherElementGroup = new MenuItem(fileMenu,
                SWT.NONE);
        fileDeleteWeatherElementGroup
                .setText("Delete Weather Element Group...");
        fileDeleteWeatherElementGroup
                .addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent arg0) {
                        showDeleteWeatherElementGroup(loadWeatherElementGroupMenu);
                    }
                });

        MenuItem fileLoadWeatherElementGroup = new MenuItem(fileMenu,
                SWT.CASCADE);
        fileLoadWeatherElementGroup.setText("Load Weather Element Group");
        fileLoadWeatherElementGroup.setMenu(loadWeatherElementGroupMenu);

        MenuItem fileQuit = new MenuItem(fileMenu, SWT.NONE);
        fileQuit.setText("Quit");
        fileQuit.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                cancelPressed();
            }

        });

        MenuItem editStartOver = new MenuItem(editMenu, SWT.NONE);
        editStartOver.setText("Start Over");
        editStartOver.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                productSelectionList.removeAll();
                setUpSelections(getCurrentDisplayedParms());
            }
        });

        MenuItem editSelectNone = new MenuItem(editMenu, SWT.NONE);
        editSelectNone.setText("Select None");
        editSelectNone.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                resetAll();
            }
        });

        MenuItem editSelectAll = new MenuItem(editMenu, SWT.NONE);
        editSelectAll.setText("Select All");
        editSelectAll.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                if (productSelectionList.getItemCount() > 0) {
                    productSelectionList.selectAll();
                }
            }
        });

        addSiteLabels(siteMenu);
        initWEGroups(loadWeatherElementGroupMenu);
        shell.setMenuBar(menuBar);
    }

    /**
     * Creates the site radios.
     */
    private void addSiteLabels(Menu siteMenu) {
        for (int i = 0; i < sites.length; i++) {
            MenuItem item = new MenuItem(siteMenu, SWT.RADIO);
            item.setText(sites[i]);
            if (i == 0) {
                item.setSelection(true);
            }
        }
    }

    /**
     * Creates the type items.
     */
    private void createTypesComp(Composite parent) {

        Group group = new Group(parent, SWT.BORDER);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        group.setLayoutData(layoutData);
        group.setLayout(new GridLayout());
        group.setText("Types");

        for (final WEBrowserTypeRecord entry : typeEntries) {
            final Button b = new Button(group, SWT.RADIO | SWT.BORDER);
            layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT);
            b.setLayoutData(layoutData);
            b.setText(entry.getType());
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (b.getSelection()) {
                        selectedType = entry;
                        setupListsForCurrentType();
                    }
                }
            });

            if (typeEntries.indexOf(entry) == 0) {
                selectedType = entry;
                b.setSelection(true);
            }
        }
    }

    private void setupListsForCurrentType() {
        previousSources.clear();
        previousFields.clear();
        previousPlanes.clear();
        fieldsList.removeAll();
        planesList.removeAll();
        sourceList.removeAll();

        // save off productSelectionList
        String[] pList = productSelectionList.getItems();
        String[] pListSelected = productSelectionList.getSelection();

        updateSourceMenu();
        Set<String> sMenu = getMenuStringSet(sourceMenu);

        // populate source list
        for (String product : pList) {
            DatabaseID dbase = productIDs.get(product).getDbId();
            String s = dbase.getModelName();
            if (dbase.getModelDate() != null) {
                synchronized (WEBrowserTypeRecord.SOURCE_FORMAT) {
                    s += " "
                            + WEBrowserTypeRecord.SOURCE_FORMAT.format(dbase
                                    .getModelDate());
                }
            }
            if (sMenu.contains(s)) {
                addToList(s, sourceList);
            }
        }

        // handle newly selected source items
        processSourceSelection();
        Set<String> fMenu = getMenuStringSet(fieldsMenu);

        // populate field list
        for (String product : pList) {
            String f = productIDs.get(product).getParmName();

            if (fMenu.contains(f)) {
                addToList(f, fieldsList);
            }
        }

        // handle newly selected field items
        processFieldSelection();
        Set<String> pMenu = getMenuStringSet(presMenu);
        Set<String> mMenu = getMenuStringSet(miscMenu);

        // populate plane list
        for (String product : pList) {
            String p = productIDs.get(product).getParmLevel();

            if (pMenu.contains(p)) {
                addToList(p, planesList);
            }

            if (mMenu.contains(p)) {
                addToList(p, planesList);
            }
        }

        // handle newly selected plane items
        processPlanesSelection();

        // reset the product list
        productSelectionList.removeAll();
        for (String product : pList) {
            productSelectionList.add(product);
        }

        // select only what was previously selected
        for (String selectedProduct : pListSelected) {
            productSelectionList.select(productSelectionList
                    .indexOf(selectedProduct));
        }
    }

    private void resetAll() {
        previousSources.clear();
        previousFields.clear();
        previousPlanes.clear();
        fieldsList.removeAll();
        planesList.removeAll();
        sourceList.removeAll();
        productSelectionList.removeAll();
        resetMenu(sourceMenu);
        resetMenu(fieldsMenu);
        resetMenu(presMenu);
        resetMenu(miscMenu);
        updateSourceMenu();
    }

    /**
     * Updates the source list and menu, depending on what is selected in type.
     * Automatically checks for dups, and sorts.
     */
    private void updateSourceMenu() {
        resetMenu(sourceMenu);
        List<String> sortedSources = selectedType.getSources();

        if (sortedSources != null) {
            // check if item in source list and then disable it
            for (String source : sortedSources) {
                final MenuItem item = new MenuItem(sourceMenu, SWT.PUSH);
                item.setText(source);
                item.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        addToList(item.getText(), sourceList);
                        processSourceSelection();
                    }
                });
            }
        }

        processSourceSelection();
    }

    private void processSourceSelection() {
        java.util.List<String> currentSourceSelection = java.util.Arrays
                .asList(sourceList.getSelection());

        // process anything newly selected
        java.util.List<String> newSelectedSources = new ArrayList<>(
                currentSourceSelection);
        newSelectedSources.removeAll(previousSources);
        previousSources.removeAll(currentSourceSelection);

        if (!newSelectedSources.isEmpty()) {
            String[] sourceArray = new String[newSelectedSources.size()];
            newSelectedSources.toArray(sourceArray);
            updateProductList(sourceArray, fieldsList.getSelection(),
                    planesList.getSelection(), true);
        }

        if (!previousSources.isEmpty()) {
            String[] sourceArray = new String[previousSources.size()];
            previousSources.toArray(sourceArray);
            updateProductList(sourceArray, fieldsList.getSelection(),
                    planesList.getSelection(), false);
        }

        updateFieldsMenu();

        if ((!newSelectedSources.isEmpty()) || (!previousSources.isEmpty())) {
            previousSources.clear();
            previousSources.addAll(currentSourceSelection);

        }

        updateEnabledMenuItems(sourceMenu, sourceList);
    }

    /**
     * Add sources to the field menu.
     *
     * @param sourceList
     */
    private void updateFieldsMenu() {
        Set<String> fieldsToDelete = getMenuStringSet(fieldsMenu);

        resetMenu(fieldsMenu);
        SortedSet<String> sortedFields = new TreeSet<>();
        Map<String, ParmID[]> fieldMap = selectedType.getFieldMap();
        for (String source : sourceList.getSelection()) {
            if (fieldMap.containsKey(source)) {
                for (ParmID p : fieldMap.get(source)) {
                    String parmName = p.getParmName();
                    sortedFields.add(parmName);
                }
            }
        }

        for (String field : sortedFields) {
            final MenuItem item = new MenuItem(fieldsMenu, SWT.PUSH);
            item.setText(field);
            item.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    addToList(item.getText(), fieldsList);
                    processFieldSelection();
                }
            });
        }

        // generate delete list
        fieldsToDelete.removeAll(sortedFields);
        // remove the delete items from the fields list
        for (String dField : fieldsToDelete) {
            int idxToRemove = fieldsList.indexOf(dField);
            if (idxToRemove >= 0) {
                fieldsList.remove(idxToRemove);
            }
        }

        processFieldSelection();
    }

    private void processFieldSelection() {
        java.util.List<String> currentFieldSelection = java.util.Arrays
                .asList(fieldsList.getSelection());

        java.util.List<String> newSelectedFields = new ArrayList<>(
                currentFieldSelection);
        newSelectedFields.removeAll(previousFields);
        previousFields.removeAll(currentFieldSelection);

        if (!newSelectedFields.isEmpty()) {
            String[] fieldsArray = new String[newSelectedFields.size()];
            newSelectedFields.toArray(fieldsArray);
            updateProductList(sourceList.getSelection(), fieldsArray,
                    planesList.getSelection(), true);
        }

        // process anything no longer selected
        if (!previousFields.isEmpty()) {
            String[] fieldsArray = new String[previousFields.size()];
            previousFields.toArray(fieldsArray);
            updateProductList(sourceList.getSelection(), fieldsArray,
                    planesList.getSelection(), false);
        }

        updatePresMenu(newSelectedFields, previousFields);
        updateMiscMenu(newSelectedFields, previousFields);

        if ((!newSelectedFields.isEmpty()) || (!previousFields.isEmpty())) {
            previousFields.clear();
            previousFields.addAll(currentFieldSelection);
        }

        updateEnabledMenuItems(fieldsMenu, fieldsList);
    }

    /**
     * Update the pressure list
     *
     * @param fields
     *            the Selected field
     * @param addFlag
     *            true if the fields need be added, false if they need to be
     *            removed
     */
    private void updatePresMenu(java.util.List<String> fieldsToAdd,
            java.util.List<String> fieldsToRemove) {
        Set<String> previousPres = getMenuStringSet(presMenu);
        SortedSet<String> sortedNewPres = new TreeSet<>(
                new PressureComparator());
        Map<String, List<String>> pressureMap = selectedType
                .getPressureMap(sourceList.getSelection());

        // generate new menu
        for (String field : fieldsList.getSelection()) {
            if (pressureMap.containsKey(field)) {
                for (String p : pressureMap.get(field)) {
                    if (!sortedNewPres.contains(p)) {
                        sortedNewPres.add(p);
                    }
                }
            }
        }

        // generate delete list
        Set<String> deletedPres = new HashSet<>(previousPres);
        deletedPres.removeAll(sortedNewPres);

        // if any deleted or new menus added, recreate menu
        if ((!deletedPres.isEmpty())
                || (previousPres.size() != sortedNewPres.size())) {
            resetMenu(presMenu);
            for (String s : sortedNewPres) {
                final MenuItem item = new MenuItem(presMenu, SWT.PUSH);
                item.setText(s);
                item.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        addToList(item.getText(), planesList);
                        processPlanesSelection();
                    }
                });
            }

            // remove the delete items from the fields list
            for (String dPres : deletedPres) {
                if (planesList.indexOf(dPres) >= 0) {
                    planesList.remove(dPres);
                }
            }

            // process the removals and cascade
            processPlanesSelection();
        }
    }

    /**
     * Update the misc list
     *
     * @param fields
     *            the Selected field
     * @param addFlag
     *            true if the fields need be added, false if they need to be
     *            removed
     */
    private void updateMiscMenu(java.util.List<String> fieldsToAdd,
            java.util.List<String> fieldsToRemove) {
        Set<String> previousPres = getMenuStringSet(miscMenu);
        SortedSet<String> sortedNewPres = new TreeSet<>();
        Map<String, List<String>> pressureMap = selectedType
                .getMiscMap(sourceList.getSelection());

        // generate new menu
        for (String field : fieldsList.getSelection()) {
            if (pressureMap.containsKey(field)) {
                for (String p : pressureMap.get(field)) {
                    if (!sortedNewPres.contains(p)) {
                        sortedNewPres.add(p);
                    }
                }
            }
        }

        // generate delete list
        Set<String> deletedPres = new HashSet<>(previousPres);
        deletedPres.removeAll(sortedNewPres);

        // if any deleted or new menus added, recreate menu
        if ((!deletedPres.isEmpty())
                || (previousPres.size() != sortedNewPres.size())) {
            resetMenu(miscMenu);
            for (String s : sortedNewPres) {
                final MenuItem item = new MenuItem(miscMenu, SWT.PUSH);
                item.setText(s);
                item.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        addToList(item.getText(), planesList);
                        processPlanesSelection();
                    }
                });
            }

            // remove the delete items from the fields list
            for (String dPres : deletedPres) {
                if (planesList.indexOf(dPres) >= 0) {
                    planesList.remove(dPres);
                }
            }

            // process the removals and cascade
            processPlanesSelection();
        }
    }

    private void processPlanesSelection() {
        java.util.List<String> currentPlaneSelection = java.util.Arrays
                .asList(planesList.getSelection());

        // process anything no longer selected
        java.util.List<String> newSelectedPlanes = new ArrayList<>(
                currentPlaneSelection);
        newSelectedPlanes.removeAll(previousPlanes);

        if (!newSelectedPlanes.isEmpty()) {
            String[] planesArray = new String[newSelectedPlanes.size()];
            newSelectedPlanes.toArray(planesArray);
            updateProductList(sourceList.getSelection(),
                    fieldsList.getSelection(), planesArray, true);
        }

        // process anything no longer selected
        previousPlanes.removeAll(currentPlaneSelection);
        if (!previousPlanes.isEmpty()) {
            String[] planesArray = new String[previousPlanes.size()];
            previousPlanes.toArray(planesArray);
            updateProductList(sourceList.getSelection(),
                    fieldsList.getSelection(), planesArray, false);
        }

        if ((!newSelectedPlanes.isEmpty()) || (!previousPlanes.isEmpty())) {
            previousPlanes.clear();
            previousPlanes.addAll(currentPlaneSelection);
            updateEnabledMenuItems(presMenu, planesList);
            updateEnabledMenuItems(miscMenu, planesList);
        }
    }

    /**
     * Creates the source composite.
     */
    private void createSourceComp(Composite parent) {
        Group group = new Group(parent, SWT.BORDER);
        group.setLayout(new GridLayout(1, true));
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        group.setLayoutData(layoutData);
        group.setText("Sources");

        sourceMenu = new Menu(getParentShell(), SWT.POP_UP);
        sourceToolBar = new ToolBar(group, SWT.NONE);
        sourceToolBar.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        sourceToolItem = new ToolItem(sourceToolBar, SWT.DROP_DOWN);
        sourceToolItem.setText("Source");
        sourceToolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateSourceMenu();
                Rectangle rect = sourceToolItem.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = sourceToolBar.toDisplay(pt);
                sourceMenu.setLocation(pt.x, pt.y);
                sourceMenu.setVisible(true);
            }
        });
        sourceList = new ToggleSelectList(group, SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.MULTI);
        sourceList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                processSourceSelection();
            }
        });
        layoutData = new GridData(GridData.FILL_BOTH);
        Rectangle trim = sourceList.computeTrim(0, 0, charWidth * NUM_CHARS,
                sourceList.getItemHeight() * NUM_LIST_ITEMS);
        layoutData.widthHint = trim.width;
        layoutData.heightHint = trim.height;
        sourceList.setLayoutData(layoutData);
    }

    private static void resetMenu(Menu menu) {
        if (menu != null) {
            for (MenuItem menuItem : menu.getItems()) {
                menuItem.dispose();
            }
        }
    }

    /**
     * Creates the field composite.
     */
    private void createFieldsComp(Composite parent) {
        Group group = new Group(parent, SWT.BORDER);
        group.setLayout(new GridLayout());
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        group.setLayoutData(layoutData);
        group.setText("Fields");

        fieldsMenu = new Menu(getParentShell(), SWT.POP_UP);
        fieldsToolBar = new ToolBar(group, SWT.NONE);
        fieldsToolBar.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        fieldsToolItem = new ToolItem(fieldsToolBar, SWT.DROP_DOWN);
        fieldsToolItem.setText("Field");
        fieldsToolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = fieldsToolItem.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = fieldsToolBar.toDisplay(pt);
                fieldsMenu.setLocation(pt.x, pt.y);
                fieldsMenu.setVisible(true);
            }
        });
        fieldsList = new ToggleSelectList(group, SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.MULTI);
        fieldsList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                processFieldSelection();
            }
        });
        layoutData = new GridData(GridData.FILL_BOTH);
        Rectangle trim = sourceList.computeTrim(0, 0, charWidth * NUM_CHARS,
                sourceList.getItemHeight() * NUM_LIST_ITEMS);
        layoutData.widthHint = trim.width;
        layoutData.heightHint = trim.height;
        fieldsList.setLayoutData(layoutData);
    }

    /**
     * Creates the plans composite.
     */
    private void createPlanesComp(Composite parent) {
        Group group = new Group(parent, SWT.BORDER);
        group.setLayout(new GridLayout(2, false));
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        group.setLayoutData(layoutData);
        group.setText("Planes");

        presMenu = new Menu(getParentShell(), SWT.POP_UP);
        miscMenu = new Menu(getParentShell(), SWT.POP_UP);

        planesToolBar = new ToolBar(group, SWT.NONE);
        planesToolBar.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        presToolItem = new ToolItem(planesToolBar, SWT.DROP_DOWN);
        presToolItem.setText("Pres");
        miscToolItem = new ToolItem(planesToolBar, SWT.DROP_DOWN);
        miscToolItem.setText("Misc");

        presToolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = presToolItem.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = planesToolBar.toDisplay(pt);
                presMenu.setLocation(pt.x, pt.y);
                presMenu.setVisible(true);
            }
        });
        miscToolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = miscToolItem.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = planesToolBar.toDisplay(pt);
                miscMenu.setLocation(pt.x, pt.y);
                miscMenu.setVisible(true);
            }

        });

        planesList = new ToggleSelectList(group, SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.MULTI);
        planesList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                processPlanesSelection();
            }
        });

        layoutData = new GridData(GridData.FILL_BOTH);
        Rectangle trim = sourceList.computeTrim(0, 0, charWidth * NUM_CHARS,
                sourceList.getItemHeight() * NUM_LIST_ITEMS);
        layoutData.widthHint = trim.width;
        layoutData.heightHint = trim.height;
        layoutData.horizontalSpan = 2;
        planesList.setLayoutData(layoutData);
    }

    private Set<String> getMenuStringSet(Menu menu) {
        Set<String> menuStringList = new HashSet<>();

        for (MenuItem menuItem : menu.getItems()) {
            menuStringList.add(menuItem.getText());
        }
        return menuStringList;
    }

    /**
     * Method for disabling/enabling menu items depending if they are already
     * selecting in the past in list.
     *
     * @param menu
     * @param list
     */
    private void updateEnabledMenuItems(Menu menu, ToggleSelectList list) {

        for (MenuItem menuItem : menu.getItems()) {
            if (inSelectedList(list, menuItem.getText())) {
                menuItem.setEnabled(false);
            } else {
                menuItem.setEnabled(true);
            }
        }
    }

    /**
     * Convenience method for checking a list for a specific item.
     *
     * @param list
     * @param item
     * @return boolean of If item is in passed in list.
     */
    private boolean inSelectedList(ToggleSelectList list, String item) {

        if (list != null) {
            for (String listItem : list.getItems()) {
                if (listItem.equalsIgnoreCase(item)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Creates the product selection composite.
     */
    private void createProductSelctionComp(Composite parent) {
        productSelectionList = new ToggleSelectList(parent, SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.MULTI);
        GridData layoutData = new GridData(GridData.FILL_BOTH);
        layoutData.horizontalSpan = 4;
        Rectangle trim = sourceList.computeTrim(0, 0, SWT.DEFAULT,
                sourceList.getItemHeight() * NUM_SEL_ITEMS);
        layoutData.heightHint = trim.height;
        productSelectionList.setLayoutData(layoutData);
    }

    /**
     * Uses the passed in parms, to select the various lists, and items.
     *
     * @param parms
     */
    private void setUpSelections(ParmID[] selections) {
        for (int i = 0; i < selections.length; i++) {
            ParmID parmID = selections[i];
            addToList(parmID.getUIFormattedString(), productSelectionList);

            if (!productIDs.containsKey(parmID.getUIFormattedString())) {
                productIDs.put(parmID.getUIFormattedString(), parmID);
            }
        }

        setupListsForCurrentType();
    }

    /**
     * Load the WE Group
     *
     * @param name
     *            Group name to load
     */
    private void loadWEGroup(String name) {

        ParmID parmsIDs[] = dataManager.getWEGroupManager().getParmIDs(name,
                getAvailableParms());

        setUpSelections(parmsIDs);
    }

    /**
     * Gets the list of available parms.
     *
     * @return a ParmID[] array.
     */
    private ParmID[] getAvailableParms() {
        if (availableParmIds == null) {
            List<ParmID> parms = new ArrayList<>();
            for (DatabaseID database : getDatabases()) {
                // ignore TOPO database type...
                if (!"Topo".equalsIgnoreCase(database.getDbType())) {
                    ParmID[] dbParms = dataManager.getParmManager()
                            .getAvailableParms(database);
                    for (int i = 0; i < dbParms.length; i++) {
                        parms.add(dbParms[i]);
                    }
                }
            }
            availableParmIds = parms.toArray(new ParmID[parms.size()]);
        }

        return availableParmIds;
    }

    /**
     * Checks the selections and updates the product list with the proper
     * product defs.
     */
    private void updateProductList(String[] sources, String[] fields,
            String[] planes, boolean addFlag) {
        if ((sources.length != 0) && (fields.length != 0)
                && (planes.length != 0)) {
            List<ParmID> filteredParmIDs = selectedType
                    .getFilteredParmIDs(sources, fields, planes);

            for (ParmID parmID : filteredParmIDs) {
                if (!productIDs.containsKey(parmID.getUIFormattedString())) {
                    productIDs.put(parmID.getUIFormattedString(), parmID);
                }

                if (addFlag) {
                    addToList(parmID.getUIFormattedString(),
                            productSelectionList);
                } else {
                    removeFromList(parmID.getUIFormattedString(),
                            productSelectionList);
                }
            }
        }
    }

    /**
     * Method for adding an item to a list. Doesn't add item if already in list.
     *
     * @param itemToAdd
     * @param list
     */
    private static void addToList(String itemToAdd, ToggleSelectList list) {
        if (list.indexOf(itemToAdd) != -1) {
            return;
        }
        list.add(itemToAdd, 0);
        list.select(list.indexOf(itemToAdd));
    }

    /**
     * Method for removing an item from a list.
     *
     * @param itemToRemove
     * @param list
     */
    private static void removeFromList(String itemToRemove,
            ToggleSelectList list) {
        if (list.indexOf(itemToRemove) != -1) {
            list.remove(itemToRemove);
        }
    }

    /**
     * Refresh the list of WE groups, (used after call to delete, or save.)
     */
    private void initWEGroups(Menu loadWeatherElementGroupMenu) {
        for (String weGroup : dataManager.getWEGroupManager().getInventory()) {
            addWEGroup(loadWeatherElementGroupMenu, weGroup);
        }
    }

    /**
     * Refresh the list of WE groups, (used after call to delete, or save.)
     */
    private void addWEGroup(Menu loadWeatherElementGroupMenu, String weGroup) {
        final MenuItem item = new MenuItem(loadWeatherElementGroupMenu,
                SWT.NONE);
        item.setText(weGroup);
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                loadWEGroup(item.getText());
            }
        });
    }

    /**
     * Refresh the list of WE groups, (used after call to delete, or save.)
     */
    private void removeWEGroup(Menu loadWeatherElementGroupMenu, String weGroup) {
        for (MenuItem menuItem : loadWeatherElementGroupMenu.getItems()) {
            if (menuItem.getText().equals(weGroup)) {
                menuItem.dispose();
            }
        }
    }

    /**
     *
     * Calls the database and gets the list of types.
     */
    private void loadTypesAndSites() {
        List<String> typeList = new ArrayList<>();
        List<String> siteList = new ArrayList<>();

        DatabaseID mutableID = dataManager.getParmManager()
                .getMutableDatabase();
        typeList.add(mutableID.getDbType());
        siteList.add(mutableID.getSiteId());
        final CAVEMode mode = CAVEMode.getMode();

        if (mutableID.getDbType().isEmpty()) {
            typeEntries.add(new WEBrowserTypeRecord(IFP, mode, dataManager
                    .getParmManager()));
        } else {
            typeEntries.add(new WEBrowserTypeRecord(mutableID.getDbType(),
                    mode, dataManager.getParmManager()));
        }

        for (DatabaseID database : getDatabases()) {
            String databaseType = database.getDbType();
            if ("Prac".equals(databaseType) && !CAVEMode.PRACTICE.equals(mode)) {
                continue;
            } else if ("Test".equals(databaseType)
                    && !CAVEMode.TEST.equals(mode)) {
                continue;
            }

            if (!typeList.contains(databaseType)) {
                typeList.add(databaseType);
                if (databaseType.isEmpty()) {
                    databaseType = IFP;
                }

                typeEntries.add(new WEBrowserTypeRecord(databaseType, mode,
                        dataManager.getParmManager()));
            }
            if (!siteList.contains(database.getSiteId())) {
                siteList.add(database.getSiteId());
            }

        }
        sites = siteList.toArray(new String[siteList.size()]);
    }

    private ParmID[] getCurrentDisplayedParms() {
        if (currentDisplayedParms == null) {
            Parm parms[] = dataManager.getParmManager().getDisplayedParms();
            currentDisplayedParms = new ParmID[parms.length];
            for (int i = 0; i < parms.length; i++) {
                currentDisplayedParms[i] = parms[i].getParmID();
            }
        }
        return currentDisplayedParms;
    }

    private List<DatabaseID> getDatabases() {
        return dataManager.getParmManager().getAvailableDbs();
    }

    private ParmID[] getSelectedParmIDS() {
        List<ParmID> selectedParms = new ArrayList<>();
        if (productSelectionList.getSelectionCount() > 0) {
            for (String key : productSelectionList.getSelection()) {
                if (productIDs.containsKey(key)) {
                    selectedParms.add(productIDs.get(key));
                }
            }
        }
        return selectedParms.toArray(new ParmID[0]);
    }

    /**
     * PressureComparator is a custom Comparator implementation that will sort
     * pressure values in the form MBnnn or MBnnnn, where nnn or nnnn represents
     * a 3 or 4 digit number. These strings will be in numerical order so that
     * MB100 is "less than" MB1000.
     *
     */
    private class PressureComparator implements Comparator<String> {

        @Override
        public int compare(String s1, String s2) {
            /*
             * we expect pressure strings to read MBnnnn (e.g., MB100 or
             * MB1000), thus we'll strip the first two letters of the strings
             * passed to us
             */
            Integer pressure1 = Integer.parseInt(s1.substring(2));
            Integer pressure2 = Integer.parseInt(s2.substring(2));

            return pressure1.compareTo(pressure2);
        }
    }
}
