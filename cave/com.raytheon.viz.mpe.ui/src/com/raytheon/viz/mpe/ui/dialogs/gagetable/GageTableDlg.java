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
package com.raytheon.viz.mpe.ui.dialogs.gagetable;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JViewport;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.xml.bind.JAXB;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.math.NumberUtils;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPEGageData;
import com.raytheon.viz.mpe.ui.IEditTimeChangedListener;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.gagetable.xml.GageTableColumnData;
import com.raytheon.viz.mpe.ui.dialogs.gagetable.xml.GageTableSettings;
import com.raytheon.viz.mpe.ui.dialogs.gagetable.xml.GageTableSortType;

/**
 * MPE Gage Table Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 28, 2009 2476       mpduff     Initial creation.
 * Mar 08, 2013 15725      snaples    Updated to fix resort issues when editing value.
 * Jan 28, 2014 16994      snaples    Updated populateGridCombo to get correct filename prefix for matching up selection.
 * Feb 02, 2014  16201     snaples    Added saved data flag support
 * Apr 16, 2014  3025      mpduff     Fix sort method.
 * Nov 18, 2015 18093      snaples    Fixed GridComboListener to trigger table update when changing compare column.
 * Dec 02, 2015 18094      lbousaidi  added the sorting method for multi column sorting.
 * Dec 07, 2015 18137      lbousaidi  fixed sorting after editing gages.
 * Jan 13, 2016 18092      snaples    Updated to have column adjustment by drag and drop.
 * Mar 10, 2016 18707       lbousaidi  revised the sorting so it doesn't always resort using LID
 * Mar 14, 2016 5467       bkowal     Replace deprecated localization file usage.
 * Mar 14, 2016 18723      snaples    Added prodContains method and updated columnselector to not allow use of any
 *                                    product that is not in the mpe_generate_list for Diff compare.
 * Feb 21, 2017 6036       lvenable   Fixed spacing issues and removed the complex GridBag layout to use the
 *                                    BorderLayout to simplify the code.
 * Mar 01, 2017 6158       mpduff     Changed how sorting works.
 * Mar 06, 2017 6144       mpduff     Fixed bug with undoing edits.
 * May 12, 2017 6283       bkowal     Restored and correctly implemented column selection.
 * May 18, 2017 6283       bkowal     Reset the selected columns in the {@link GageTableProductManager}
 *                                    instance when the settings are read.
 * Jun 22, 2017 6158       mpduff     Set sort settings on startup.
 * Jul 14, 2017 6358       mpduff      Changed how settings are handled.
 * Aug 07, 2017 6240       mpduff      Fix merge issues.
 * Nov 26, 2018 7632       lsingh     Java 11 Upgrade. 
 *                                    Updated model to use correct return-type for 
 *                                    DefaultTableModel.getDataVector().
 *                                    
 * </pre>
 * 
 * @author mpduff
 */

public class GageTableDlg extends JFrame implements IEditTimeChangedListener {
    private static final long serialVersionUID = -4230332238083384449L;

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /**
     * The x coordinate.
     */
    private final int xCoord;

    /**
     * The y coordinate.
     */
    private final int yCoord;

    /**
     * Has the data changed?
     */
    private boolean dataChanged = false;

    /**
     * Gage table model.
     */
    private GageTableModel tableModel = null;

    /**
     * The Search text field.
     */
    private JTextField searchTextField = null;

    /**
     * The grid selection combo box.
     */
    private final JComboBox<String> gridCombo = new JComboBox<String>();

    private final GridComboListener gridComboListener = new GridComboListener();

    /**
     * Selected index of the gridCombo JComboBox.
     */
    private int gridComboSelection = 0;

    /**
     * Currently selected date.
     */
    private Date currentDate = null;

    /**
     * The Table.
     */
    private JTable table = null;

    /**
     * Hour format for title bar.
     */
    private SimpleDateFormat hrFormat = null;

    /**
     * Date format for title bar.
     */
    private SimpleDateFormat dateFormat = null;

    /**
     * The selected grid.
     */
    private String selectedGrid = null;

    /**
     * List of columns.
     */
    private List<GageTableColumn> columnData = null;

    /**
     * Sort ascending?
     */
    private boolean ascending = true;

    private int sortColumnIndex = 0;

    private JScrollPane scrollPane = null;

    private JPanel gageTablePanel = null;

    /**
     * HashMap of edited gages.
     */
    private final Map<String, GageTableRowData> editMap = new HashMap<>();

    private MPEDisplayManager displayManager;

    private GageTableDataManager dataManager;

    /**
     * Constructor.
     */
    public GageTableDlg() {
        /* Find the center point of the main dialog */
        Rectangle rect = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell().getBounds();
        xCoord = rect.x + (rect.width / 2);
        yCoord = rect.y + (rect.height / 2);
        hrFormat = new SimpleDateFormat("HH");
        hrFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        dateFormat = new SimpleDateFormat("MMM dd, yyyy");
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        selectedGrid = appsDefaults.getToken("mpe_selected_grid_gagediff");

        dataManager = GageTableDataManager.getInstance();
    }

    /**
     * Open the dialog.
     */
    public void open() {
        displayManager = MPEDisplayManager.getCurrent();
        currentDate = displayManager.getCurrentEditDate();

        dataManager.setSelectedGrid(selectedGrid);

        /* Set the title bar */
        StringBuilder dateDisplayString = new StringBuilder();
        dateDisplayString.append(hrFormat.format(currentDate) + "Z ");
        dateDisplayString.append(dateFormat.format(currentDate));
        setTitle("MPE Gage Table  " + dateDisplayString.toString());

        initializeComponents();

        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                confirmation();
            }
        });

        initialTableSort();

        pack();

        setMinimumSize(
                new Dimension(this.getBounds().width, this.getBounds().height));

        /*
         * Get the bounds of this window and calculate the location so it's
         * centered on the parent window.
         */
        java.awt.Rectangle bounds = this.getBounds();
        setLocation(xCoord - (bounds.width / 2), yCoord - (bounds.height / 2));

        setVisible(true);
        displayManager.registerEditTimeChangedListener(this);
    }

    /**
     * Sort the table.
     */
    private void initialTableSort() {
        GageTableSortSettings sortSettings = dataManager.getSortSettings();
        List<String> columns = dataManager.getColumns();
        if (sortSettings == null) {
            sortSettings = new GageTableSortSettings();
        }

        List<String> sortColumns = sortSettings.getSortColumns();
        List<String> copySortColumns = new ArrayList<>(sortColumns.size());

        /*
         * Need to copy and reverse the column sort order to replicate clicking
         * the columns
         */
        for (int i = sortColumns.size() - 1; i >= 0; i--) {
            copySortColumns.add(sortColumns.get(i));
        }
        for (String colName : copySortColumns) {
            boolean ascending = sortSettings.getSortDirections().get(colName);
            sortColumn(columns.indexOf(colName), ascending);
        }
    }

    /**
     * Sort the column.
     * 
     * @param index
     *            Column index to sort on
     * @param ascending
     *            true = sort ascending
     */
    private void sortColumn(int index, boolean ascending) {
        GageTableDataManager dataManager = GageTableDataManager.getInstance();
        GageTableSortSettings columnSettings = dataManager.getSortSettings();

        if (columnSettings == null) {
            columnSettings = new GageTableSortSettings();
        }

        // The index of the column to sort
        int vColIndex = index;

        columnSettings = setSortColumns(columnSettings, vColIndex, ascending);

        dataManager.setSortSettings(columnSettings);

        sortAllRowsBy(table.getColumnModel(), vColIndex, ascending);
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        Container container = getContentPane();
        container.setLayout(new BorderLayout(10, 10));

        createFileMenu();
        createTopPanel(container);
        createJTable(container);
        gridCombo.addActionListener(gridComboListener);
        populateGridCombo();
        createButtons(container);

    }

    /**
     * Create the menus.
     * 
     * @param menuBar
     *            The menu bar to hold the menus
     */
    private void createFileMenu() {
        JMenuBar menuBar = new JMenuBar();
        setJMenuBar(menuBar);

        JMenu fileMenu = new JMenu("File");
        fileMenu.setMnemonic('F');
        JMenuItem columnSelectionMenuItem = new JMenuItem("Column Selection");
        columnSelectionMenuItem.setMnemonic('C');
        fileMenu.add(columnSelectionMenuItem);

        JMenuItem refreshMenuItem = new JMenuItem("Refresh");
        refreshMenuItem.setMnemonic('R');
        fileMenu.add(refreshMenuItem);

        JMenuItem saveSettingsMenuItem = new JMenuItem("Save Settings");
        saveSettingsMenuItem.setMnemonic('S');
        fileMenu.add(saveSettingsMenuItem);

        columnSelectionMenuItem
                .addActionListener(new ChangeColumnsDisplayedMenuListener());

        RefreshMenuListener RefreshMenuListener = new RefreshMenuListener();
        refreshMenuItem.addActionListener(RefreshMenuListener);

        saveSettingsMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                saveSettings();
            }
        });
        menuBar.add(fileMenu);
    }

    /**
     * Create the search text field and the grid selection combo box.
     * 
     * @param container
     *            JPanel with a BorderLayout.
     */
    private void createTopPanel(Container container) {
        JPanel boxPanel = new JPanel();
        boxPanel.setLayout(new BoxLayout(boxPanel, BoxLayout.X_AXIS));

        JLabel searchLabel = new JLabel("Search for LID: ");
        searchTextField = new JTextField(5);
        searchTextField.setMaximumSize(searchTextField.getPreferredSize());

        searchTextField.addKeyListener(new KeyActionListener());

        boxPanel.add(searchLabel);
        boxPanel.add(searchTextField);
        boxPanel.add(Box.createHorizontalStrut(30));
        boxPanel.add(Box.createHorizontalGlue());

        JLabel gridLabel = new JLabel(
                "Select Grid Field Compared With Gage:  ");
        boxPanel.add(gridLabel);
        boxPanel.add(gridCombo);

        container.add(boxPanel, BorderLayout.NORTH);
    }

    /**
     * Populate the Grid Field Combo box.
     * 
     * create a list for "select grid field" to allow user customize the grid
     * field which they want to compare with gage value field, then the
     * difference of gagevalue-gridvalue will be displayed in the
     * "Diff (Gage - Grid)" colunm. The default field "Best Estimate QPE" will
     * be used as the grid field if 1) token mpe_selected_grid_gagediff is NOT
     * set AND 2)the "Best Estimate QPE
     * " field IS included in mpe_generate_list token. Otherwise, "No field
     * selected" will be highlighted in the combobox list
     */
    private void populateGridCombo() {
        // Determine the selected grid
        gridCombo.removeActionListener(gridComboListener);
        if (selectedGrid == null) {
            for (int i = 0; i < columnData.size(); i++) {
                if (columnData.get(i).getPrefix().equalsIgnoreCase(
                        GageTableProductManager.MPE_BESTQPE)) {
                    selectedGrid = "BESTQPE";
                    break;
                }
            }
        }

        int gridSelectionIndex = 0;
        for (int i = 0; i < columnData.size(); i++) {
            if (columnData.get(i).isDataColumn()) {
                if (prodContains(columnData.get(i))) {
                    gridCombo.addItem(columnData.get(i).getName());
                    if (selectedGrid.equalsIgnoreCase(
                            columnData.get(i).getProductDescriptor()
                                    .getProductFilenamePrefix())) {
                        gridComboSelection = gridSelectionIndex;
                    }
                    gridSelectionIndex++;
                } else {
                    continue;
                }
            }
        }

        gridCombo.addActionListener(gridComboListener);
        gridCombo.setSelectedIndex(gridComboSelection);
    }

    // Check to see if product is available for compare (Diff)
    private boolean prodContains(GageTableColumn gageTableColumn) {
        GageTableProductManager prodMgr = GageTableProductManager.getInstance();
        List<GageTableColumn> availColumns = prodMgr
                .getAvailableGageTableColumnList();
        for (int i = 0; i < availColumns.size(); i++) {
            if (availColumns.get(i).getProductDescriptor()
                    .getProductFilenamePrefix()
                    .equals((gageTableColumn.getProductDescriptor()
                            .getProductFilenamePrefix()))) {
                return true;
            }
        }

        return false;
    }

    /**
     * Create the Save and Cancel buttons at the bottom of the dialog.
     * 
     * @param container
     *            JPanel with the BorderLayout
     */
    private void createButtons(Container container) {
        JButton saveButton = new JButton("Save");
        JButton cancelButton = new JButton("Cancel");

        JPanel boxPanel = new JPanel();
        boxPanel.setLayout(new BoxLayout(boxPanel, BoxLayout.X_AXIS));

        saveButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                saveChanges();
                displayManager.setSavedData(false);
            }
        });

        cancelButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                confirmation();
            }
        });

        boxPanel.add(saveButton);
        boxPanel.add(Box.createHorizontalGlue());
        boxPanel.add(cancelButton);

        container.add(boxPanel, BorderLayout.SOUTH);
    }

    /**
     * Launches a confirm dialog when closing with changes not saved.
     */
    private void confirmation() {
        boolean close = true;
        if (dataChanged) {
            // Modal dialog with yes/no button
            int answer = JOptionPane.showConfirmDialog(this,
                    "Are you sure you want to close without saving?");
            if (answer != JOptionPane.YES_OPTION) {
                close = false;
            }
        }

        if (close) {
            tableModel = null;
            setVisible(false);
            dispose();
        }
    }

    /**
     * Create the JTable.
     * 
     * @param container
     *            JPanel with the BorderLayout
     */
    private void createJTable(Container container) {
        gageTablePanel = new JPanel(new CardLayout());
        table = null;
        table = new GageJTable();
        tableModel = new GageTableModel(this);
        table.setModel(tableModel);
        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

        JTableHeader header = table.getTableHeader();
        header.addMouseListener(new ColumnHeaderListener());

        table.setColumnSelectionAllowed(false);
        table.setRowSelectionAllowed(true);
        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        table.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);

        /* Center all table data */
        JLabel renderer = ((JLabel) table.getDefaultRenderer(Object.class));
        renderer.setHorizontalAlignment(SwingConstants.CENTER);

        table.getModel().addTableModelListener(new GageTableModelListener());

        scrollPane = new JScrollPane(table);
        gageTablePanel.add(scrollPane);

        container.add(gageTablePanel, BorderLayout.CENTER);
    }

    /**
     * Regardless of sort order (ascending or descending), null values always
     * appear last.
     * 
     * @param model
     *            The GageTableModel data model
     * @param colIndex
     *            The column index
     * @param ascending
     *            True sorts ascending, descending if false
     */
    private void sortAllRowsBy(TableColumnModel model, int colIndex,
            boolean ascending) {
        TableColumn tc = model.getColumn(colIndex);
        int modelIdx = tc.getModelIndex();
        @SuppressWarnings("unchecked")
        Vector<Vector<String>> data = this.tableModel.getDataVector();
        Collections.sort(data, new ColumnSorter(modelIdx, ascending));
        tableModel.setDataVector(data);
    }

    public void refreshSort() {
        GageTableDataManager dataManager = GageTableDataManager.getInstance();

        GageTableSortSettings sortSettings = dataManager.getSortSettings();
        if (sortSettings == null
                || CollectionUtils.isEmpty(sortSettings.getSortColumns())) {
            /*
             * Revert to the default sort based on the first column.
             */
            sortColumnIndex = 0;
            ascending = true;
            sortAllRowsBy(table.getColumnModel(), sortColumnIndex, ascending);
        } else {
            /*
             * Need to resort by all of the selected columns in order.
             */
            for (String sortColumn : sortSettings.getSortColumns()) {
                int columnIndex = tableModel.getColumns().indexOf(sortColumn);
                if (columnIndex < 0) {
                    /*
                     * Should be unlikely assuming that cleanup of the sort
                     * columns has been implemented properly and is in use.
                     */
                    throw new IllegalStateException("A specified sort column: "
                            + sortColumn + " is not currently displayed.");
                }
                sortColumnIndex = columnIndex;
                ascending = Boolean.TRUE.equals(
                        sortSettings.getSortDirections().get(sortColumn));
                sortAllRowsBy(table.getColumnModel(), sortColumnIndex,
                        ascending);
            }
        }
    }

    /**
     * Scroll the selected row in the JTable to the visible area.
     * 
     * @param table
     *            The JTable
     * @param rowIndex
     *            The index of the row to scroll
     * @param vColIndex
     *            The column index
     */
    public void scrollToVisible(JTable table, int rowIndex, int vColIndex) {
        if (!(table.getParent() instanceof JViewport)) {
            return;
        }

        JViewport viewport = (JViewport) table.getParent();

        /*
         * This rectangle is relative to the table where the northwest corner of
         * cell (0,0) is always (0,0).
         */
        java.awt.Rectangle rect = table.getCellRect(rowIndex, vColIndex, true);

        // Scroll the area into view
        viewport.setViewPosition(rect.getLocation());
    }

    /**
     * Launch the item selection dialog.
     */
    private void launchItemSelectionDlg() {
        GageTableProductManager manager = GageTableProductManager.getInstance();
        GageTableDataManager dataManager = GageTableDataManager.getInstance();

        List<GageTableColumn> availableProductColumnList = new ArrayList<>();
        Map<String, GageTableColumn> prodMap = manager
                .getGageTableProductColumnMap();

        Set<String> keySet = prodMap.keySet();

        Iterator<String> iter = keySet.iterator();

        while (iter.hasNext()) {
            availableProductColumnList.add(prodMap.get(iter.next()));
        }

        List<String> availableListItems = new ArrayList<>();

        // Add the non-data columns
        String[] baseColumns = GageTableConstants.BASE_COLUMNS;
        for (String s : baseColumns) {
            availableListItems.add(s);
        }

        for (GageTableColumn c : availableProductColumnList) {
            GageTableProductDescriptor desc = c.getProductDescriptor();
            availableListItems.add(desc.getProductName());
        }

        List<GageTableColumn> selectedProductColumnList = manager
                .getSelectedColumns();

        String[] selectedListItems = new String[selectedProductColumnList
                .size()];
        for (int i = 0; i < selectedProductColumnList.size(); i++) {
            selectedListItems[i] = selectedProductColumnList.get(i).getName();
        }

        // Launch the dialog
        ItemsSelectionDialog dlg = new ItemsSelectionDialog(this,
                "Gage Table Column Selector",
                availableListItems
                        .toArray(new String[availableListItems.size()]),
                selectedListItems);

        // Get the selected columns for display
        String[] selectedColumns = dlg.getSelectedItems();

        Map<String, GageTableColumn> colMap = manager
                .getGageTableProductColumnMap();
        List<GageTableColumn> colList = new ArrayList<GageTableColumn>();

        for (int i = 0; i < selectedColumns.length; i++) {
            String value = selectedColumns[i];
            if (colMap.get(manager.lookupProductPrefix(value)) == null) {
                GageTableColumn c = new GageTableColumn(null);
                c.setDataColumn(false);
                c.setName(value);
                colList.add(c);
            } else {
                GageTableColumn col = colMap
                        .get(manager.lookupProductPrefix(value));
                col.setName(value);
                col.setPrefix(manager.lookupProductPrefix(value));
                colList.add(col);
            }
        }

        manager.setSelectedColumns(colList);
        dataManager.updateVisibleColumns(colList);

        // Fire event to notify listeners of changes
        GageTableUpdateEvent event = new GageTableUpdateEvent(this, true);
        manager.fireUpdateEvent(event);
    }

    /**
     * Save the settings out to an XML file.
     */
    private void saveSettings() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);
        ILocalizationFile newXmlFile = pm.getLocalizationFile(lc, "hydro"
                + IPathManager.SEPARATOR + "MPEGageTableDisplaySettings.xml");

        try (SaveableOutputStream os = newXmlFile.openOutputStream()) {
            JAXB.marshal(getSettingsXML(), os);
            os.save();
        } catch (Exception e) {
            statusHandler.error("Failed to save MPE Settings.", e);
        }
    }

    /**
     * Returns the xml for the configuration of the window.
     * 
     * @return GageTableSettings XML element object
     */
    private GageTableSettings getSettingsXML() {
        GageTableDataManager dataMan = GageTableDataManager.getInstance();
        GageTableSettings settingsElement = new GageTableSettings();

        // Get the column info to save to the file
        GageTableSortSettings settings = dataMan.getSortSettings();
        List<String> sortColumns = settings.getSortColumns();
        Enumeration<TableColumn> colEnum = table.getColumnModel().getColumns();

        while (colEnum.hasMoreElements()) {
            TableColumn col = colEnum.nextElement();
            GageTableColumnData data = new GageTableColumnData();
            data.setName((String) col.getHeaderValue());
            data.setWidth(java.math.BigInteger.valueOf(col.getWidth()));

            if (sortColumns.contains(col.getHeaderValue())) {
                GageTableSortType sort = new GageTableSortType();
                int sortOrder = sortColumns.indexOf(col.getHeaderValue());
                sort.setOrder(java.math.BigInteger.valueOf(sortOrder));
                boolean ascending = settings.getSortDirections()
                        .get(col.getHeaderValue());
                sort.setAscending(ascending);
                data.setSort(sort);
            }

            settingsElement.getColumn().add(data);
        }

        return settingsElement;
    }

    private class GageJTable extends JTable {

        private static final long serialVersionUID = 5664094340115122252L;

        @Override
        public void createDefaultColumnsFromModel() {
            columnData = dataManager.getColumnDataList();

            super.createDefaultColumnsFromModel();

            final int columnCount = getModel().getColumnCount();

            ColumnHeaderToolTips tips = new ColumnHeaderToolTips();

            for (int i = 0; i < columnCount; i++) {
                TableColumn col = table.getColumnModel().getColumn(i);
                col.setHeaderRenderer(new GageTableHeaderCellRenderer());
                col.setMinWidth(25);
                GageTableColumn c = columnData.get(i);
                col.setPreferredWidth(c.getWidth());
                tips.setToolTip(col, c.getToolTipText());
                if (getTableHeader() != null) {
                    final MouseMotionListener[] mouseListeners = getTableHeader()
                            .getMouseMotionListeners();
                    if (mouseListeners != null && mouseListeners.length > 0) {
                        for (MouseMotionListener mouseListener : mouseListeners) {
                            if (mouseListener instanceof ColumnHeaderToolTips) {
                                getTableHeader().removeMouseMotionListener(
                                        mouseListener);
                            }
                        }
                    }
                    getTableHeader().addMouseMotionListener(tips);
                }
            }
        }
    }

    /**
     * Action listener for the Refresh Menu item.
     */
    private class RefreshMenuListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            // Refresh the display
            tableModel.refreshTable();
            SwingUtilities.updateComponentTreeUI(table);
        }
    }

    /**
     * Action listener for the Change Columns Menu item.
     */
    private class ChangeColumnsDisplayedMenuListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            launchItemSelectionDlg();
        }
    }

    /**
     * Action listener for the Grid Combo Box.
     */
    private class GridComboListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            if (gridCombo.getSelectedItem() == null) {
                return;
            }
            GageTableProductManager prodManager = GageTableProductManager
                    .getInstance();
            GageTableDataManager dataManager = GageTableDataManager
                    .getInstance();
            gridComboSelection = gridCombo.getSelectedIndex();

            List<GageTableColumn> colList = prodManager.getSelectedColumns();

            for (GageTableColumn col : colList) {
                GageTableProductDescriptor prod = col.getProductDescriptor();
                if (prod != null) {
                    if (prod.getProductName().equalsIgnoreCase(
                            (String) gridCombo.getSelectedItem())) {
                        dataManager.setSelectedGrid(
                                prod.getProductFilenamePrefix());
                        break;
                    }
                }
            }
            /*
             * setting the selected index ensures that when we refresh the combo
             * box it displays the correct field
             */
            dataManager.setSelectedGridIndex(gridComboSelection);
            tableModel.refreshTable();
            TableColumnModel colModel = table.getColumnModel();
            sortAllRowsBy(colModel, sortColumnIndex, ascending);
            gridCombo.setSelectedIndex(dataManager.getSelectedGridIndex());
        }
    }

    /**
     * Key listener for the search text field.
     */
    private class KeyActionListener extends KeyAdapter {
        @Override
        public void keyPressed(KeyEvent evt) {

        }

        @Override
        public void keyReleased(KeyEvent evt) {
            String lid = searchTextField.getText().toUpperCase();
            @SuppressWarnings("unchecked")
            Vector<Vector<String>> dataVector = tableModel.getDataVector();
            for (int i = 0; i < dataVector.size(); i++) {
                Vector<String> data = dataVector.get(i);
                for (int j = 0; j < data.size(); j++) {
                    if (data.get(0).startsWith(lid)) {

                        // Select the row
                        table.setRowSelectionInterval(i, i);
                        scrollToVisible(table, i, j);
                        break;
                    }
                }
            }

            // Refresh the display
            SwingUtilities.updateComponentTreeUI(table);
        }
    }

    /**
     * Column header listener for catching mouse clicks when sorting.
     */
    private class ColumnHeaderListener extends MouseAdapter {

        @Override
        public void mouseClicked(MouseEvent evt) {
            handleMouseClick(evt);
        }

        void handleMouseClick(MouseEvent evt) {
            if ((evt.getButton() == MouseEvent.BUTTON1)
                    || (evt.getButton() == MouseEvent.BUTTON3)) {
                GageTableDataManager dataManager = GageTableDataManager
                        .getInstance();
                GageTableSortSettings columnSettings = dataManager
                        .getSortSettings();

                if (columnSettings == null) {
                    columnSettings = new GageTableSortSettings();
                }

                /* Sort columns on right click of column header */
                JTable table = ((JTableHeader) evt.getSource()).getTable();
                TableColumnModel colModel = table.getColumnModel();

                /* Sort the opposite way this time */
                ascending = !ascending;
                // The index of the column whose header was clicked
                sortColumnIndex = colModel.getColumnIndexAtX(evt.getX());

                // Return if not clicked on any column header
                if (sortColumnIndex == -1) {
                    return;
                }

                columnSettings = setSortColumns(columnSettings, sortColumnIndex,
                        ascending);

                dataManager.setSortSettings(columnSettings);

                sortAllRowsBy(table.getColumnModel(), sortColumnIndex,
                        ascending);
            }
        }
    }

    /**
     * This comparator is used to sort vectors of data.
     */
    private class ColumnSorter implements Comparator<Vector<String>> {
        int colIndex;

        boolean ascending;

        ColumnSorter(int colIndex, boolean ascending) {
            this.colIndex = colIndex;
            this.ascending = ascending;
        }

        @Override
        public int compare(Vector<String> v1, Vector<String> v2) {
            String o1 = v1.get(colIndex);
            String o2 = v2.get(colIndex);
            int response = 0;
            // Treat empty strains like nulls
            if (o1.length() == 0) {
                o1 = null;
            }
            if (o2.length() == 0) {
                o2 = null;
            }

            // Sort nulls so they appear last, regardless
            // of sort order
            if ((o1 == null) && (o2 == null)) {
                response = 0;
            } else if (o1 == null) {
                response = 1;
            } else if (o2 == null) {
                response = -1;
            } else {
                if (ascending) {
                    // Check for equality first
                    response = o1.compareTo(o2);
                    if (response != 0) {
                        if (o1.toString().trim().equalsIgnoreCase("M")) {
                            response = -1;
                        } else if (o2.toString().trim().equalsIgnoreCase("M")) {
                            response = 1;
                        } else {
                            // Values
                            if (NumberUtils.isNumber(o1)
                                    && NumberUtils.isNumber(o2)) {
                                double d1 = Double.parseDouble(o1);
                                double d2 = Double.parseDouble(o2);
                                if (d1 > d2) {
                                    response = 1;
                                } else {
                                    response = -1;
                                }
                            }
                        }
                    }
                } else {
                    // Check for equality first
                    response = o1.compareTo(o2);
                    response *= -1;
                    if (response != 0) {
                        if (o1.toString().trim().equalsIgnoreCase("M")) {
                            response = 1;
                        } else if (o2.toString().trim().equalsIgnoreCase("M")) {
                            response = -1;
                        } else {
                            // Values
                            if (NumberUtils.isNumber(o1)
                                    && NumberUtils.isNumber(o2)) {
                                double d1 = Double.parseDouble(o1);
                                double d2 = Double.parseDouble(o2);
                                if (d1 > d2) {
                                    response = -1;
                                } else {
                                    response = 1;
                                }
                            }
                        }
                    }
                }
            }
            return response;
        }

    }

    /**
     * Controls the column header tool tips.
     */
    private class ColumnHeaderToolTips extends MouseMotionAdapter {
        // Current column whose tooltip is being displayed.
        TableColumn curCol;

        // Maps TableColumn objects to tooltips
        Map<TableColumn, String> tips = new HashMap<TableColumn, String>();

        // If tooltip is null, removes any tooltip text.
        public void setToolTip(TableColumn col, String tooltip) {
            if (tooltip == null) {
                tips.remove(col);
            } else {
                tips.put(col, tooltip);
            }
        }

        @Override
        public void mouseMoved(MouseEvent evt) {
            TableColumn col = null;
            JTableHeader header = (JTableHeader) evt.getSource();
            JTable table = header.getTable();
            TableColumnModel colModel = table.getColumnModel();

            // Get the column
            int colIndex = colModel.getColumnIndexAtX(evt.getX());

            if (colIndex >= 0) {
                col = colModel.getColumn(colIndex);
            }

            if (col != curCol) {
                header.setToolTipText(tips.get(col));
                curCol = col;
            }
        }
    }

    private class GageTableModelListener implements TableModelListener {

        /**
         * Called when the table model fires change events.
         * 
         * @param e
         *            The TableModelEvent for the change
         */
        @Override
        public void tableChanged(TableModelEvent e) {
            GageTableDataManager dataManager = GageTableDataManager
                    .getInstance();
            if ((e != null) && (e.getType() == TableModelEvent.UPDATE)) {
                // Get the changed info
                int cellRow = table.convertRowIndexToModel(e.getFirstRow());
                int cellColumn = e.getColumn();

                // Get the data list structure from the Table Manager.
                List<GageTableRowData> rowDataList = dataManager
                        .getGageTableRowList();

                if ((cellRow != TableModelEvent.HEADER_ROW)
                        && (cellColumn != TableModelEvent.ALL_COLUMNS)
                        && (cellRow == e.getLastRow())) {
                    String newValue = (String) tableModel.getValueAt(cellRow,
                            cellColumn);

                    GageTableRowData rowData = rowDataList.get(cellRow);
                    for (GageTableRowData row : rowDataList) {
                        if (row.getGageData().getId().equalsIgnoreCase(
                                (String) tableModel.getValueAt(cellRow, 0))) {
                            rowData = row;
                            break;
                        }
                    }

                    /*
                     * Update the edited row. The custom cell editor makes sure
                     * that the value is either a real number or 'm' or 'M' or
                     * "".
                     */

                    if (newValue.isEmpty()) {
                        rowData.setValueEdited(false);
                        editMap.remove(
                                tableModel.getValueAt(e.getFirstRow(), 0));
                    } else {
                        if (newValue.equalsIgnoreCase("m")) {
                            rowData.setEditValue(-999.0);
                        } else {
                            rowData.setEditValue(
                                    Float.valueOf(newValue));
                        }
                        rowData.setValueEdited(true);
                        int indexOf = rowDataList.indexOf(rowData);
                        rowDataList.set(indexOf, rowData);
                        editMap.put((String) tableModel
                                .getValueAt(e.getFirstRow(), 0), rowData);
                        dataChanged = true;
                    }
                }
            }
        }
    }

    /**
     * Save the edit changes.
     */
    private void saveChanges() {
        MPEDataManager mpeDataManager = MPEDataManager.getInstance();

        if (!editMap.isEmpty()) {
            Set<String> editKeys = editMap.keySet();
            Iterator<String> iter = editKeys.iterator();
            while (iter.hasNext()) {
                String lid = iter.next();
                MPEGageData gageData = editMap.get(lid).getGageData();
                if (gageData.getId().startsWith("PSEUDO")) {
                    gageData.setEdit(String
                            .valueOf(editMap.get(lid).getEditValue() * 25.4));
                } else {
                    if (editMap.get(lid).getEditValue() == -999) {
                        gageData.setEdit("M");
                    } else {
                        gageData.setEdit(String
                                .valueOf(editMap.get(lid).getEditValue()));
                    }
                    gageData.setManedit(true);
                }
                mpeDataManager.addEditedGage(gageData);
            }
        }

        setVisible(false);
        tableModel = null;
        dispose();
    }

    public void setDataChanged(boolean dataChanged) {
        this.dataChanged = dataChanged;
    }

    /**
     * Set the sort order of the columns.
     * 
     * @param settings
     *            The GageTableColumnSettings
     * @param index
     *            The selected column index
     * @return
     */
    private GageTableSortSettings setSortColumns(GageTableSortSettings settings,
            int index, boolean ascending) {
        TableColumnModel colModel = table.getColumnModel();
        String colName = (String) colModel.getColumn(index).getHeaderValue();

        settings.getSortDirections().put(colName, ascending);
        List<String> sortCols = settings.getSortColumns();
        if (sortCols.isEmpty()) {
            sortCols.add(colName);
            return settings;
        }

        if (!sortCols.get(0).equals(colName)) {
            if (sortCols.contains(colName)) {
                sortCols.remove(colName);
            }
            sortCols.add(0, colName);
            if (sortCols.size() > 4) {
                sortCols.remove(4);
            }

            settings.setSortColumns(sortCols);
        }

        return settings;
    }

    /**
     * Update the data in the table to reflect the newDate
     * 
     * @param newDate
     *            the new date for the data
     */
    public void updateDate(Date newDate) {
        /* Set the title bar */
        StringBuilder dateDisplayString = new StringBuilder();
        dateDisplayString.append(hrFormat.format(newDate) + "Z ");
        dateDisplayString.append(dateFormat.format(newDate));
        setTitle("MPE Gage Table  " + dateDisplayString.toString());

        currentDate = newDate;

        // Fire event to notify listeners of changes
        GageTableUpdateEvent event = new GageTableUpdateEvent(this, true);
        GageTableProductManager.getInstance().fireUpdateEvent(event);
        sortAllRowsBy(table.getColumnModel(), sortColumnIndex, ascending);
    }

    @Override
    public void editTimeChanged(Date oldTime, Date newTime) {
        updateDate(newTime);
    }

    @Override
    public void dispose() {
        if (displayManager != null) {
            displayManager.unregisterEditTimeChangedListener(this);
        }
        super.dispose();
    }
}
