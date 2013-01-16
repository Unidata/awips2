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

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDisplay;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.d2d.nsharp.rsc.D2DNSharpResourceData;
import com.raytheon.uf.viz.d2d.ui.map.actions.NewMapEditor;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionRenderableDisplay;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightRenderableDisplay;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesRenderableDisplay;
import com.raytheon.uf.viz.xy.varheight.display.VarHeightDescriptor;
import com.raytheon.uf.viz.xy.varheight.display.VarHeightRenderableDisplay;
import com.raytheon.uf.viz.xy.varheight.hodo.VarHeightHodoDescriptor;
import com.raytheon.uf.viz.xy.varheight.rsc.VarHeightResourceData;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.core.rsc.ICombinedResourceData;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;
import com.raytheon.viz.core.slice.request.HeightScales;
import com.raytheon.viz.core.slice.request.VerticalPointRequest.TimeDirection;
import com.raytheon.viz.skewt.SkewtDisplay;
import com.raytheon.viz.skewt.rscdata.SkewTResourceData;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.BundleProductLoader;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.volumebrowser.datacatalog.DataCatalogManager;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.util.CrossSectionUtil;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.LeftRightMenu;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.SpaceTimeMenu;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * 
 * This class manages the Product Table that will be loaded onto the display.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 8, 2009  #2161      lvenable     Initial creation
 * Mar 27, 2012 #14506     Qinglu Lin   For cross section plot along a line of 
 *                                      latitude, swap xStart and xEnd.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ProductTableComp extends Composite {

    /**
     * Set the number of columns in the table.
     */
    private static final int prodSelTableColumnCount = 3;

    /**
     * Width of the "Times" table column.
     */
    private static final int timesColWidth = 100;

    /**
     * Width of the "Inventory" column.
     */
    private static final int inventoryColWidth = 150;

    /**
     * Product selection table containing the product generated by the Source,
     * Fields, and Planes menu selections.
     */
    private Table prodSelTable;

    /**
     * Product selection index array used to keep track of the indexes of the
     * selected products.
     */
    private boolean[] prodSelIdxArray = new boolean[0];

    /**
     * Popup menu that appears when right-clicking on a table item.
     */
    private Menu popupMenu;

    /**
     * Label displaying the total product available.
     */
    private Label totalProductsLbl;

    /**
     * Label displaying the number of selected products.
     */
    private Label selectedProductsLbl;

    /**
     * Difference button.
     */
    private Button diffBtn;

    /**
     * Load button.
     */
    private Button loadBtn;

    /**
     * Table item font.
     */
    private Font tiFont;

    /**
     * The product key is a unique string that is used to identify if a product
     * is already in the table.
     */
    private Set<String> productKeySet;

    /**
     * Array of Product Table Data.
     */
    protected ArrayList<ProductTableData> tableDataArray;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     */
    public ProductTableComp(Composite parentComp) {
        super(parentComp, 0);

        initializeComponents();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        initializeData();

        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        this.setLayout(gl);
        this.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // Create the product table
        createTable();

        // Create the product and selection labels.
        createProductLabels();

        // Create the Diff and Load buttons.
        initializeDiffButton();
        initializeLoadButton();
        createBottomButtons(new Button[] { diffBtn, loadBtn });

        // Update the product labels with the current product
        // and selection counts.
        updateLoadButtonAndProductLabels();

        // Add a dispose listener to this class so we can
        // clean up objects we have created.
        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                tiFont.dispose();
            }
        });
    }

    /**
     * Initializes the load button
     */
    private void initializeLoadButton() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 200;
        loadBtn = new Button(getParent(), SWT.PUSH);
        loadBtn.setText("Load");
        loadBtn.setEnabled(false);
        loadBtn.setLayoutData(gd);
        loadBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loadProducts(false);
            }
        });
    }

    /**
     * Initializes the diff button
     */
    private void initializeDiffButton() {
        GridData gd = new GridData(80, SWT.DEFAULT);
        diffBtn = new Button(getParent(), SWT.PUSH);
        diffBtn.setText("Diff");
        diffBtn.setEnabled(false);
        diffBtn.setLayoutData(gd);
        diffBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loadProducts(true);
            }

        });
    }

    /**
     * Initialize the data map and array. Create table item font.
     */
    private void initializeData() {
        tiFont = new Font(this.getDisplay(), "Fixed", 10, SWT.BOLD);

        productKeySet = new HashSet<String>();
        tableDataArray = new ArrayList<ProductTableData>();
    }

    /**
     * Create the Product Selection Table.
     */
    private void createTable() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 200;
        gd.widthHint = 200;
        gd.verticalIndent = 10;

        prodSelTable = new Table(this, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.MULTI | SWT.FULL_SELECTION);
        prodSelTable.setLayoutData(gd);
        prodSelTable.setHeaderVisible(true);
        prodSelTable.setLinesVisible(true);

        // Add a selection listener so the indexes can be updated.
        prodSelTable.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateTableIndexes((Boolean) prodSelTable.getData());
            }
        });

        /*
         * Add a mouse listener to the table.
         */
        prodSelTable.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent me) {
                if (me.button == 3) {
                    // Set the data to false because we are not
                    // selecting/de-selecting
                    // rows from the table.
                    prodSelTable.setData(false);

                    // The the table item associated with the mouse position
                    // when the mouse
                    // is right clicked.
                    TableItem item = prodSelTable
                            .getItem(new Point(me.x, me.y));

                    // Check if there is a table item where the mouse button was
                    // clicked.
                    if (item == null) {
                        /*
                         * Dispose of the popup menu so the previous created
                         * popup menu does not popup when right-clicking on the
                         * table where a table item does not exist.
                         */
                        if (popupMenu != null) {
                            popupMenu.dispose();
                        }

                        return;
                    }

                    // Create the popup menu and then display it.
                    createPopupMenu(prodSelTable, item);
                    popupMenu.setVisible(true);
                } else if (me.button == 1) {
                    // Set the data to true because we are
                    // selecting/de-selecting
                    // a row from the table.
                    prodSelTable.setData(true);
                }
                // we are toggling between an image or contour
                else if (me.button == 2) {

                    // Set the data to false because we are not
                    // selecting/de-selecting
                    // rows from the table.
                    prodSelTable.setData(false);

                    // The the table item associated with the mouse position
                    // when the mouse
                    // is right clicked.
                    TableItem item = prodSelTable
                            .getItem(new Point(me.x, me.y));

                    // Check if there is a table item where the mouse button was
                    // clicked.
                    if (item == null) {
                        return;
                    }

                    // toggle the item between DisplayTypes
                    toggleDisplayTypes(prodSelTable, item);
                }
            }

            private void toggleDisplayTypes(Table prodSelTable, TableItem item) {

                int itemIndex = prodSelTable.indexOf(item);
                ProductTableData productTableData = tableDataArray
                        .get(itemIndex);

                if (!isMultiDisplayTypeCapable(productTableData
                        .getCatalogEntry().getDialogSettings()
                        .getViewSelection())) {
                    return;
                }

                DisplayType currentDisplayType = productTableData
                        .getDisplayTypeSet().iterator().next();
                List<DisplayType> displayTypes = productTableData
                        .getSelectedData().getDisplayTypes();

                for (int i = 0; i < displayTypes.size(); i++) {
                    if (displayTypes.get(i).equals(currentDisplayType)) {
                        changeProductDisplayType(itemIndex,
                                displayTypes.get((i + 1) % displayTypes.size()));
                    }
                }

            }

        });

        // Add the table columns to the table.
        addTableColumns();
    }

    /**
     * Add the Times, Product Selection List, and Inventory column to the table.
     */
    private void addTableColumns() {
        TableColumn column1 = new TableColumn(prodSelTable, SWT.NONE);
        column1.setText("Times");

        TableColumn column2 = new TableColumn(prodSelTable, SWT.NONE);
        column2.setText("Product Selection List");

        TableColumn column3 = new TableColumn(prodSelTable, SWT.NONE);
        column3.setText("Inventory");

        // Pack the columns
        for (int i = 0; i < prodSelTableColumnCount; i++) {
            prodSelTable.getColumn(i).pack();
        }
    }

    /**
     * Update the table indexes. If the flag passed in is true then the left
     * mouse button was selected and the indexes need to be updated to reflect
     * the selection/deselection. If the flag is false then re-do the indexes
     * and keep the selections the same.
     * 
     * @param flag
     *            Selection flag.
     */
    private void updateTableIndexes(boolean flag) {
        /*
         * If a row is to be selection/de-selected then update the product
         * selection index array. This indicates a left-mouse button click.
         */
        if (flag == true) {
            if (prodSelIdxArray[prodSelTable.getSelectionIndex()] == false) {
                prodSelIdxArray[prodSelTable.getSelectionIndex()] = true;
            } else {
                prodSelIdxArray[prodSelTable.getSelectionIndex()] = false;
            }
        }

        /*
         * The following code is necessary every time a mouse button is clicked.
         * We want to prevent items from being selected/deselected when the
         * right mouse button is clicked so all of the indexes have to be
         * updated according to the product selection index array.
         */
        prodSelTable.deselectAll();

        int indexes[] = new int[prodSelTable.getItemCount()];
        Arrays.fill(indexes, -99);
        int counter = 0;

        for (int i = 0; i < prodSelIdxArray.length; i++) {
            if (prodSelIdxArray[i] == true) {
                indexes[counter] = i;
                ++counter;
            }
        }

        prodSelTable.select(indexes);

        // Update the labels and the Diff and Load buttons.
        updateLoadButtonAndProductLabels();
    }

    /**
     * Select the new product added to the table and update the selection
     * indexes.
     */
    private void selectNewTableItem() {
        /*
         * Update the product selection index array to include the new entry.
         */

        boolean[] tmpIndexes = new boolean[prodSelTable.getItemCount()];

        Arrays.fill(tmpIndexes, true);

        for (int i = 0; i < prodSelIdxArray.length; i++) {
            tmpIndexes[i] = prodSelIdxArray[i];
        }

        prodSelIdxArray = new boolean[prodSelTable.getItemCount()];

        System.arraycopy(tmpIndexes, 0, prodSelIdxArray, 0, tmpIndexes.length);

        // Select the new product in the table.
        prodSelTable.select(prodSelTable.getItemCount() - 1);

        // Need to enable the load button because a new product is
        // added and is selected.
        updateLoadButtonAndProductLabels();
    }

    /**
     * Update the indexes when a product is removed from the table.
     */
    private void updateRemovedProductIndex() {
        // Create a new re-sized product selection array.
        prodSelIdxArray = new boolean[prodSelTable.getItemCount()];

        Arrays.fill(prodSelIdxArray, false);

        int[] selectedIdx = prodSelTable.getSelectionIndices();

        for (int i = 0; i < selectedIdx.length; i++) {
            prodSelIdxArray[selectedIdx[i]] = true;
        }
    }

    /**
     * Create the products and product selection labels.
     */
    private void createProductLabels() {
        Composite prodLabelComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        prodLabelComp.setLayout(gl);
        prodLabelComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // Label that displays the number of products in the table.
        Label prodLbl = new Label(prodLabelComp, SWT.NONE);
        prodLbl.setText("Products: ");

        totalProductsLbl = new Label(prodLabelComp, SWT.NONE);
        totalProductsLbl.setText("0");
        totalProductsLbl.setLayoutData(new GridData(80, SWT.DEFAULT));

        // Label that displays the number of products that are selected.
        Label selLoadingLbl = new Label(prodLabelComp, SWT.NONE);
        selLoadingLbl.setText("Selected for loading: ");

        selectedProductsLbl = new Label(prodLabelComp, SWT.NONE);
        selectedProductsLbl.setText("0");
        selectedProductsLbl.setLayoutData(new GridData(80, SWT.DEFAULT));
    }

    /**
     * Create the Bottom Buttons
     */
    protected void createBottomButtons(Button[] buttons) {

        Composite buttonComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(buttons.length, false);
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        for (Button button : buttons) {
            button.setParent(buttonComp);

        }
    }

    /**
     * Update the product labels to reflect how many products are in the table
     * and how many products are selected.
     */
    private void updateProductLabels() {
        totalProductsLbl.setText(String.valueOf(prodSelTable.getItemCount()));
        selectedProductsLbl.setText(String.valueOf(prodSelTable
                .getSelectionCount()));
    }

    /**
     * Create a popup menu for the table item that was right-clicked.
     * 
     * @param parent
     *            Parent control.
     * @param tableItem
     *            Table item that was right-clicked.
     */
    private void createPopupMenu(Control parent, final TableItem tableItem) {
        // Need to create menu on the fly...
        if (popupMenu != null) {
            popupMenu.dispose();
        }

        popupMenu = new Menu(parent);

        ProductTableData productData = (ProductTableData) tableItem.getData();

        MenuItem productTitleItem = new MenuItem(popupMenu, SWT.NONE);
        productTitleItem.setText("Product: " + productData.getName());

        // Separator bar.
        new MenuItem(popupMenu, SWT.SEPARATOR);

        if (isMultiDisplayTypeCapable(productData.getCatalogEntry()
                .getDialogSettings().getViewSelection())) {

            for (final DisplayType displayType : productData.getSelectedData()
                    .getDisplayTypes()) {

                if (displayType.equals(productData.getDisplayTypeSet()
                        .iterator().next())) {
                    continue;
                }

                MenuItem changeDisplayTypeMenuItem = new MenuItem(popupMenu,
                        SWT.NONE);
                changeDisplayTypeMenuItem.setText("Change to "
                        + productData.getName(displayType));
                changeDisplayTypeMenuItem
                        .addSelectionListener(new SelectionAdapter() {
                            /*
                             * (non-Javadoc)
                             * 
                             * @seeorg.eclipse.swt.events.SelectionAdapter#
                             * widgetSelected
                             * (org.eclipse.swt.events.SelectionEvent)
                             */
                            @Override
                            public void widgetSelected(SelectionEvent e) {
                                changeProductDisplayType(
                                        prodSelTable.indexOf(tableItem),
                                        displayType);
                            }
                        });
            }

        }

        MenuItem loadItem = new MenuItem(popupMenu, SWT.NONE);
        loadItem.setText("Load this product now");
        loadItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                loadSingleProduct(prodSelTable.indexOf(tableItem));
            }
        });

        if (isMultiDisplayTypeCapable(productData.getCatalogEntry()
                .getDialogSettings().getViewSelection())) {

            MenuItem loadAllDisplayTypesMenuItem = new MenuItem(popupMenu,
                    SWT.NONE);

            StringBuilder menuName = new StringBuilder();

            for (DisplayType displayType : productData.getSelectedData()
                    .getDisplayTypes()) {

                if (menuName.length() == 0) {
                    menuName.append("Load as ");
                } else {
                    menuName.append(" + ");
                }

                menuName.append(productData.getName(displayType));

            }

            loadAllDisplayTypesMenuItem.setText(menuName.toString());

            loadAllDisplayTypesMenuItem
                    .addSelectionListener(new SelectionAdapter() {

                        @Override
                        public void widgetSelected(SelectionEvent e) {
                            loadAllDisplayTypes(prodSelTable.indexOf(tableItem));
                        }

                        private void loadAllDisplayTypes(int indexOf) {

                            ProductTableData productTableData = tableDataArray
                                    .get(indexOf);
                            DisplayType currentDisplayType = productTableData
                                    .getDisplayTypeSet().iterator().next();

                            productTableData.getDisplayTypeSet().clear();

                            for (DisplayType displayType : productTableData
                                    .getSelectedData().getDisplayTypes()) {

                                productTableData.getDisplayTypeSet().add(
                                        displayType);

                            }
                            List<ResourcePair> resources = productTableData
                                    .getResourcesToLoad();
                            loadResources(false, resources);

                            unselectProduct(indexOf);

                            productTableData.getDisplayTypeSet().clear();
                            productTableData.getDisplayTypeSet().add(
                                    currentDisplayType);

                            updateLoadButtonAndProductLabels();
                        }

                    });

        }

        MenuItem inventoryItem = new MenuItem(popupMenu, SWT.NONE);
        inventoryItem.setText("Show detailed inventory...");

        inventoryItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayInventotyForProduct(tableItem);
            }
        });

        // Set the pop-up menu as the pop-up for the shell
        parent.setMenu(popupMenu);
    }

    /**
     * @param indexOf
     * @param displayType
     */
    protected void changeProductDisplayType(int index, DisplayType displayType) {
        TableItem ti = prodSelTable.getItem(index);
        ProductTableData productData = getProductData(index);

        // don't go about doing this if we already set the type to the given
        // type
        if (productData.getDisplayTypeSet().contains(displayType)) {
            return;
        }

        productData.getDisplayTypeSet().remove(
                productData.getDisplayTypeSet().iterator().next());
        productData.getDisplayTypeSet().add(displayType);

        ti.setText(1, productData.getName());
    }

    protected void changeProductDisplayType(int index, String displayTypeString) {
        DisplayType displayType = DisplayType.valueOf(displayTypeString);
        changeProductDisplayType(index, displayType);
    }

    private void displayInventotyForProduct(TableItem tableItem) {
        ProductTableData tableData = getProductData(tableItem);
        InventoryDlg inventoryDlg = new InventoryDlg(getShell(), tableData);
        inventoryDlg.open();
    }

    private ProductTableData getProductData(TableItem ti) {
        return (ProductTableData) ti.getData();
    }

    protected ProductTableData getProductData(int index) {
        return getProductData(prodSelTable.getItem(index));
    }

    /**
     * 
     * @return a list of all selected data, an empty list if nothing is selected
     */
    public List<ProductTableData> getSelectedData() {

        List<ProductTableData> selectedData = new ArrayList<ProductTableData>();

        for (int selectedIndex : prodSelTable.getSelectionIndices()) {
            ProductTableData productData = tableDataArray.get(selectedIndex);
            selectedData.add(productData);
        }

        return selectedData;
    }

    /**
     * Update the Load and Diff button state to be enabled/disabled.
     */
    private void updateLoadButtonAndProductLabels() {
        // enable the load button only if products are selected
        loadBtn.setEnabled(prodSelTable.getSelectionCount() != 0);

        // enable the diff button if there are two products and they are of the
        // same display type
        diffBtn.setEnabled(isDifferenceProduct(prodSelTable
                .getSelectionIndices()));

        updateProductLabels();
    }

    /**
     * Unselect the product at the specified index.
     * 
     * @param index
     *            The index or indices of the product(s) to be unselected.
     * @param secondProductIndex
     */
    private void unselectProduct(int... index) {
        for (int i : index) {
            prodSelIdxArray[i] = false;
            prodSelTable.deselect(i);
        }
    }

    /**
     * Unselect all products
     */
    protected void selectNone() {
        prodSelTable.deselectAll();
        Arrays.fill(prodSelIdxArray, false);
        updateLoadButtonAndProductLabels();
    }

    /**
     * Select all products
     */
    protected void selectAll() {
        prodSelTable.selectAll();
        Arrays.fill(prodSelIdxArray, true);
        updateLoadButtonAndProductLabels();
    }

    /**
     * Load all of the selected products.
     * 
     * @param difference
     *            if true load the a difference of the selected products.
     */
    private void loadProducts(boolean difference) {
        int[] prodIndexes = prodSelTable.getSelectionIndices();

        List<ResourcePair> resources = new ArrayList<ResourcePair>(
                prodIndexes.length);
        for (int i = 0; i < prodIndexes.length; i++) {
            ProductTableData productData = tableDataArray.get(prodIndexes[i]);
            resources.addAll(productData.getResourcesToLoad());
        }

        loadResources(difference, resources);

        unselectProduct(prodIndexes);

        updateLoadButtonAndProductLabels();
    }

    private void loadResources(boolean difference,
            List<ResourcePair> resourceList) {
        if (difference) {
            try {
                ((ICombinedResourceData) resourceList.get(0).getResourceData())
                        .setSecondaryData(resourceList.get(1).getResourceData());
                ((ICombinedResourceData) resourceList.get(0).getResourceData())
                        .setCombineOperation(CombineOperation.DIFFERENCE);
                resourceList.remove(1);
            } catch (Exception e) {
                resourceList.clear();
                throw new RuntimeException(
                        "Unable to obtain a difference request", e);
            }
        }
        VolumeBrowserDialogSettings dialogSettings = VolumeBrowserAction
                .getVolumeBrowserDlg().getDialogSettings();

        ResourceType resourceType = dialogSettings.getViewSelection()
                .getResourceType();

        AbstractRenderableDisplay display = null;

        switch (resourceType) {
        case PLAN_VIEW:
            String editorId = DescriptorMap.getEditorId(MapDescriptor.class
                    .getName());

            IEditorPart editorPart = EditorUtil.findEditor(editorId);
            if (editorPart == null) {
                try {
                    new NewMapEditor().execute(null);
                } catch (ExecutionException e) {
                    throw new RuntimeException(e);
                }
                editorPart = EditorUtil.findEditor(editorId);
            }
            AbstractEditor editor = (AbstractEditor) editorPart;
            display = (AbstractRenderableDisplay) editor.getActiveDisplayPane()
                    .getRenderableDisplay().createNewDisplay();
            try {
                display.setDescriptor(new MapDescriptor());
            } catch (VizException e) {
                throw new RuntimeException(e);
            }
            break;
        case CROSS_SECTION:
            CrossSectionRenderableDisplay csDisplay = new CrossSectionRenderableDisplay();
            CrossSectionDescriptor csDesc = csDisplay.getDescriptor();
            csDesc.setRenderableDisplay(csDisplay);

            IDataCatalogEntry catalogEntry = getSelectedData().get(0)
                    .getCatalogEntry();
            VBMenuBarItemsMgr.SpaceTimeMenu currentSpaceTime = dialogSettings
                    .getSpaceTimeSelection();
            String selectedPlaneKey = catalogEntry.getSelectedData()
                    .getPlanesKey();

            // the selected Plane in Time setting will be either a Baseline or a
            // Lat/Lon Line
            if (currentSpaceTime == SpaceTimeMenu.TIME) {
                java.awt.Rectangle coverageRectangle = VbUtil
                        .getMapCoverageRectangle();
                LineString line = null;

                if (selectedPlaneKey.startsWith("Lon")) {
                    double yEnd, yStart, xEnd, xStart;
                    yEnd = coverageRectangle.getMinY();
                    yStart = coverageRectangle.getMaxY();
                    xEnd = Double.parseDouble(selectedPlaneKey.replace("Lon",
                            ""));
                    xStart = xEnd;
                    line = (new GeometryFactory())
                            .createLineString(new Coordinate[] {
                                    new Coordinate(xStart, yStart),
                                    new Coordinate(xEnd, yEnd) });
                } else if (selectedPlaneKey.startsWith("Lat")) {
                    double yEnd, yStart, xEnd, xStart;
                    xStart = coverageRectangle.getMinX();
                    xEnd = coverageRectangle.getMaxX();
                    yEnd = Double.parseDouble(selectedPlaneKey.replace("Lat",
                            ""));
                    yStart = yEnd;
                    line = (new GeometryFactory())
                            .createLineString(new Coordinate[] {
                                    new Coordinate(xStart, yStart),
                                    new Coordinate(xEnd, yEnd) });

                }
                // assume we have selected a baseline
                else {
                    ToolsDataManager dataManager = ToolsDataManager
                            .getInstance();

                    line = dataManager.getBaseline(selectedPlaneKey.replace(
                            "Line", ""));
                    // default to Baseline A if all else fails

                    if (line == null) {
                        line = dataManager.getBaseline("A");
                    }

                }

                // set the line at which the results are displayed
                csDesc.setLines(Arrays.asList(line));
                csDesc.setLineID(catalogEntry.getSelectedData().getPlanesText());
            } else if (currentSpaceTime == SpaceTimeMenu.SPACE) {
                if (selectedPlaneKey.equals("LATS")) {
                    csDesc.setLines(CrossSectionUtil.getAllLats());
                    csDesc.setLineID("AllLAT");
                } else if (selectedPlaneKey.equals("LONS")) {
                    csDesc.setLines(CrossSectionUtil.getAllLons());
                    csDesc.setLineID("AllLON");
                }
            }
            csDesc.setHeightScale(dialogSettings.getHeightScaleSelection());
            display = csDisplay;
            break;
        case VAR_HEIGHT:
            VarHeightRenderableDisplay vhDisplay = new VarHeightRenderableDisplay();
            VarHeightDescriptor vhDesc = vhDisplay.getDescriptor();
            vhDesc.setRenderableDisplay(vhDisplay);
            vhDesc.setHeightScale(dialogSettings.getHeightScaleSelection());
            display = vhDisplay;
            break;
        case TIME_SERIES:
            display = new TimeSeriesRenderableDisplay();
            break;
        case TIME_HEIGHT:
            TimeHeightRenderableDisplay thDisplay = new TimeHeightRenderableDisplay();
            TimeHeightDescriptor thDesc = thDisplay.getDescriptor();
            thDesc.setRenderableDisplay(thDisplay);
            thDesc.setHeightScale(dialogSettings.getHeightScaleSelection());
            LeftRightMenu leftRightMenu = dialogSettings
                    .getTimeDirectionSelection();
            if (leftRightMenu == LeftRightMenu.LEFT) {
                thDesc.timeDirection = TimeDirection.RIGHT_TO_LEFT;
            } else {
                thDesc.timeDirection = TimeDirection.LEFT_TO_RIGHT;
            }
            display = thDisplay;
            break;
        case SOUNDING:
            // need to handle both legacy skeqwT and nsharp skewT at the same
            // time.
            List<ResourcePair> varheightSkewtResources = new ArrayList<ResourcePair>();
            List<ResourcePair> legacySkewtResources = new ArrayList<ResourcePair>();
            List<ResourcePair> nsharpSkewtResources = new ArrayList<ResourcePair>();
            for (ResourcePair pair : resourceList) {
                if (pair.getResourceData() instanceof D2DNSharpResourceData) {
                    nsharpSkewtResources.add(pair);
                } else if (pair.getResourceData() instanceof SkewTResourceData) {
                    legacySkewtResources.add(pair);
                } else if (pair.getResourceData() instanceof VarHeightResourceData) {
                    varheightSkewtResources.add(pair);
                }
            }
            if (!nsharpSkewtResources.isEmpty()) {
                display = new NsharpSkewTPaneDisplay();
                display.setDescriptor(new NsharpSkewTPaneDescriptor());
                if (!legacySkewtResources.isEmpty()) {
                    resourceList.removeAll(legacySkewtResources);
                    loadResources(difference, legacySkewtResources);
                }
                if (!varheightSkewtResources.isEmpty()) {
                    resourceList.removeAll(varheightSkewtResources);
                    loadResources(difference, varheightSkewtResources);
                }
            } else if (!legacySkewtResources.isEmpty()) {
                display = new SkewtDisplay();
                if (!varheightSkewtResources.isEmpty()) {
                    resourceList.removeAll(varheightSkewtResources);
                    loadResources(difference, varheightSkewtResources);
                }
            } else {
                VarHeightRenderableDisplay vhhDisplay = new VarHeightRenderableDisplay();
                VarHeightHodoDescriptor vhhDesc = new VarHeightHodoDescriptor();
                vhhDisplay.setDescriptor(vhhDesc);
                vhhDesc.setRenderableDisplay(vhhDisplay);
                vhhDesc.setHeightScale(HeightScales.fromName("Log 1050-150"));
                display = vhhDisplay;
            }
            break;
        }

        String editorId = DescriptorMap.getEditorId(display.getDescriptor()
                .getClass().getName());

        IEditorPart activeEditor = EditorUtil.getActiveEditor();
        AbstractEditor editor = UiUtil.createOrOpenEditor(editorId,
                display.cloneDisplay());

        if (editor != null) {
            if (activeEditor != null && editor != activeEditor
                    && activeEditor instanceof IMultiPaneEditor
                    && editor instanceof IMultiPaneEditor) {
                IMultiPaneEditor activeMpe = (IMultiPaneEditor) activeEditor;
                IMultiPaneEditor mpe = (IMultiPaneEditor) editor;

                if (mpe.getNumberofPanes() < activeMpe.getNumberofPanes()) {
                    for (int i = mpe.getNumberofPanes(); i < activeMpe
                            .getNumberofPanes(); ++i) {
                        mpe.addPane(display.createNewDisplay());
                    }

                    IDisplayPane selectedPane = activeMpe
                            .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
                    if (selectedPane != null) {
                        IDisplayPane[] allPanes = activeMpe.getDisplayPanes();
                        for (int i = 0; i < allPanes.length; ++i) {
                            if (selectedPane == allPanes[i]) {
                                IDisplayPane newSelectedPane = mpe
                                        .getDisplayPanes()[i];
                                mpe.setSelectedPane(
                                        IMultiPaneEditor.LOAD_ACTION,
                                        newSelectedPane);
                                break;
                            }
                        }
                    }
                }
            }
            for (ResourcePair pair : resourceList) {
                display.getDescriptor().getResourceList().add(pair);
            }

            Bundle b = new Bundle();
            b.setDisplays(new AbstractRenderableDisplay[] { display });
            Job j = new BundleProductLoader(editor, b);
            j.schedule();
        }
    }

    /**
     * @param prodIndexes
     * @return
     */
    private boolean isDifferenceProduct(int... prodIndexes) {

        return (prodIndexes.length == 2
                && getProductData(prodIndexes[0]).getCatalogEntry()
                        .getDialogSettings().getViewSelection() != ViewMenu.SOUNDING && getProductData(
                prodIndexes[0]).getDisplayTypeSet().equals(
                getProductData(prodIndexes[1]).getDisplayTypeSet()));
    }

    /**
     * Load the product at the specified index.
     * 
     * @param selectedItemIndex
     *            Index of the selected product.
     */
    private void loadSingleProduct(int selectedItemIndex) {
        ProductTableData productData = tableDataArray.get(selectedItemIndex);
        List<ResourcePair> resources = productData.getResourcesToLoad();
        loadResources(false, resources);
        unselectProduct(selectedItemIndex);
        updateLoadButtonAndProductLabels();
    }

    /**
     * Resize the table columns. This will maintain sizes for the Times and
     * Inventory columns. The products column will adjust in size.
     */
    public void resizeTableColumns() {
        Rectangle tableBounds = prodSelTable.getBounds();

        prodSelTable.getColumn(0).setWidth(timesColWidth);
        prodSelTable.getColumn(1).setWidth(
                tableBounds.width - (timesColWidth + inventoryColWidth) - 20);
        // prodSelTable.getColumn(2).setWidth(inventoryColWidth);
    }

    /**
     * Clear the products table.
     */
    public void clearProductTable() {
        synchronized (productKeySet) {
            productKeySet.clear();
            tableDataArray.clear();
            prodSelTable.removeAll();

            prodSelIdxArray = new boolean[0];

            updateLoadButtonAndProductLabels();
        }
    }

    /**
     * Pack the table. This will resize the table to fit as the display changes
     * size due to menu changes.
     */
    public void packTable() {
        prodSelTable.pack();
    }

    /**
     * Remove a product that matches the specified key.
     * 
     * @param key
     *            Product key used to identify the product to be deleted.
     */
    public void removeProduct(String key) {
        removeProductFromDataArray(key);
        updateLoadButtonAndProductLabels();
    }

    /**
     * Remove a product from the table data array.
     * 
     * @param key
     *            Product key used to identify the product.
     */
    private void removeProductFromDataArray(String key) {
        synchronized (productKeySet) {

            productKeySet.remove(key);

            for (int i = 0; i < tableDataArray.size(); i++) {
                if (tableDataArray.get(i).hasUniqueKey(key) == true) {
                    tableDataArray.remove(i);
                    prodSelTable.remove(i);
                    updateRemovedProductIndex();
                    break;
                }
            }
        }
    }

    protected void addProduct(final ProductTableData tblData) {
        synchronized (productKeySet) {
            String uniqueKey = tblData.getSelectedData().getUniqueKey();
            if (!productKeySet.contains(uniqueKey)) {
                return;
            }
            for (ProductTableData existing : tableDataArray) {
                if (existing.getSelectedData().getUniqueKey()
                        .equals(tblData.getSelectedData().getUniqueKey())) {
                    // Do not add if it is already in the table
                    return;
                }
            }
            tableDataArray.add(tblData);

            final TableItem ti = new TableItem(prodSelTable, SWT.NONE);

            ti.setText(0, tblData.getTime());
            ti.setText(1, tblData.getName());
            ti.setFont(2, tiFont);
            ti.setText(2, tblData.getProductInventory()
                    .getInventoryStatusString());

            ti.addDisposeListener(new DisposeListener() {

                @Override
                public void widgetDisposed(DisposeEvent e) {
                    tblData.getProductInventory().cancelUpdateJob();
                }

            });

            final Runnable updateInventoryStrings = new Runnable() {
                /*
                 * (non-Javadoc)
                 * 
                 * @see java.lang.Runnable#run()
                 */
                @Override
                public void run() {
                    try {
                        ti.setText(0, tblData.getProductInventory()
                                .getLatestForecastTime());
                        ti.setText(2, tblData.getProductInventory()
                                .getInventoryStatusString());
                        updateScrollBar();
                    } catch (SWTException e) {
                        if (!e.getMessage().contains("Widget is disposed")) {
                            throw e;
                        }
                    }
                }
            };

            tblData.getProductInventory().addJobChangeListener(
                    new JobChangeAdapter() {
                        @Override
                        public void done(IJobChangeEvent event) {
                            getDisplay().asyncExec(updateInventoryStrings);
                        }
                    });

            updateInventoryStrings.run();

            ti.setData(tblData);

            selectNewTableItem();
        }
    }

    /**
     * Add a product to the product table.
     * 
     * @param productParms
     *            Product parameters.
     * @param selectedData
     *            Data that contains Sources, Fields, and Planes information.
     */
    public void addProduct(final SelectedData selectedData) {
        synchronized (productKeySet) {
            if (productKeySet.contains(selectedData.getUniqueKey())) {
                return;
            }
            productKeySet.add(selectedData.getUniqueKey());
        }
        new Job("Loading Product...") {
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                final IDataCatalogEntry catalogEntry = DataCatalogManager
                        .getDataCatalogManager().getDataCatalogEntry(
                                selectedData);
                if (catalogEntry != null) {
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            addProduct(new ProductTableData(catalogEntry));
                        }
                    });
                }
                return Status.OK_STATUS;
            }

        }.schedule();

    }

    protected void updateScrollBar() {
        prodSelTable.getColumn(2).pack();
    }

    /**
     * 
     * @param currentSetting
     * @return return true if the current view is capable of selecting and
     *         displaying multiple display types, false otherwise
     */
    private boolean isMultiDisplayTypeCapable(ViewMenu currentSetting) {

        switch (currentSetting) {
        case VARVSHGT:
        case SOUNDING:
        case TIMESERIES:
            return false;
        }

        return true;
    }
}
