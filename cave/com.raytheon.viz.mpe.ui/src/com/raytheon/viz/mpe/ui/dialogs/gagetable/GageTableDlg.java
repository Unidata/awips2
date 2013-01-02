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
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.math.BigInteger;
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

import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.ohd.AppsDefaults;
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
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GageTableDlg extends JFrame implements IEditTimeChangedListener {
    private static final long serialVersionUID = -4230332238083384449L;

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
    private final JComboBox gridCombo = new JComboBox();

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

    int sortColumnIndex = 0;

    /**
     * List of non-data columns.
     */
    private final List<String> baseColumns = new ArrayList<String>();

    private JScrollPane scrollPane = null;

    private JPanel gageTablePanel = null;

    /**
     * HashMap of edited gages.
     */
    private final Map<String, GageTableRowData> editMap = new HashMap<String, GageTableRowData>();

    private MPEDisplayManager displayManager;

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

        // Get a list of non-data column names
        for (String colName : GageTableConstants.BASE_COLUMNS) {
            baseColumns.add(colName);
        }

    }

    /**
     * Open the dialog.
     */
    public void open() {
        // Instantiate the product and data manager classes
        GageTableDataManager dataManager = GageTableDataManager.getInstance();

        readSettingsFile();

        displayManager = MPEDisplayManager.getCurrent();
        currentDate = displayManager.getCurrentEditDate();

        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        selectedGrid = appsDefaults.getToken("mpe_selected_grid_gagediff");
        dataManager.setSelectedGrid(selectedGrid);

        columnData = dataManager.getColumnDataList();

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

        setPreferredSize(new Dimension(750, 500));
        setMinimumSize(new Dimension(750, 500));
        pack();

        sortTable();

        /*
         * Get the bounds of this window and calculate the location so it's
         * centered on the parent window.
         */
        java.awt.Rectangle bounds = this.getBounds();
        setLocation(xCoord - (bounds.width / 2), yCoord - (bounds.height / 2));

        setVisible(true);
        // tableModel.refreshTable();

        displayManager.registerEditTimeChangedListener(this);
    }

    /**
     * Sort the table.
     */
    private void sortTable() {
        // Sort the columns based on the settings
        int one = -999;
        int two = -999;
        int three = -999;
        int four = -999;
        boolean oneAscending = false;
        boolean twoAscending = false;
        boolean threeAscending = false;
        boolean fourAscending = false;

        for (int i = 0; i < columnData.size(); i++) {

            // First get the column sort order
            GageTableColumn col = columnData.get(i);
            if (col.getSortOrder() != -999) {
                if (col.getSortOrder() == 0) {
                    one = i;
                    oneAscending = col.isAscending();
                } else if (col.getSortOrder() == 1) {
                    two = i;
                    twoAscending = col.isAscending();
                } else if (col.getSortOrder() == 2) {
                    three = i;
                    threeAscending = col.isAscending();
                } else if (col.getSortOrder() == 3) {
                    four = i;
                    fourAscending = col.isAscending();
                }
            }
        }

        // sort the columns in order
        if (four != -999) {
            sortColumn(four, fourAscending);
        }

        if (three != -999) {
            sortColumn(three, threeAscending);
        }

        if (two != -999) {
            sortColumn(two, twoAscending);
        }

        if (one != -999) {
            sortColumn(one, oneAscending);
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
        GageTableSortSettings columnSettings = dataManager.getColumnSettings();

        if (columnSettings == null) {
            columnSettings = new GageTableSortSettings();
        }

        // The index of the column to sort
        int vColIndex = index;

        columnSettings = setSortColumns(columnSettings, vColIndex, ascending);

        dataManager.setColumnSettings(columnSettings);

        sortAllRowsBy(tableModel, vColIndex, ascending);
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        Container container = getContentPane();
        container.setLayout(new GridBagLayout());

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
     */
    private void createTopPanel(Container container) {
        searchTextField = new JTextField(5);
        JPanel searchPanel = new JPanel();
        JPanel gridPanel = new JPanel();
        JLabel searchLabel = new JLabel("Search for LID: ");
        GridBagConstraints constraints = new GridBagConstraints();

        searchTextField.addKeyListener(new KeyActionListener());

        // Create horizontal box container
        Box box = new Box(BoxLayout.X_AXIS);

        box.add(searchPanel);
        searchPanel.add(searchLabel, BorderLayout.CENTER);
        searchPanel.add(searchTextField, BorderLayout.CENTER);
        searchPanel.setMinimumSize(new Dimension(170, 35));
        searchPanel.setPreferredSize(new Dimension(170, 50));
        searchPanel.setMaximumSize(new Dimension(170, 50));

        JLabel gridLabel = new JLabel("Select Grid Field Compared With Gage:  ");
        gridPanel.add(gridLabel);
        gridPanel.add(gridCombo);
        gridPanel.setMinimumSize(new Dimension(600, 35));
        gridPanel.setPreferredSize(new Dimension(600, 50));

        box.add(gridPanel);

        constraints.anchor = GridBagConstraints.FIRST_LINE_START;
        constraints.fill = GridBagConstraints.NONE;
        constraints.insets = new Insets(5, 5, 0, 0);

        constraints.weightx = 0.0;
        constraints.weighty = 0.0;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 2;
        constraints.gridheight = 1;

        container.add(box, constraints);
    }

    /**
     * Populate the Grid Field Combo box.
     * 
     * create a list for "select grid field" to allow user customize the grid
     * field which they want to compare with gage value field, then the
     * difference of gagevalue-gridvalue will be displayed in the
     * "Diff (Gage - Grid)" colunm. The default field "Best Estimate QPE" will
     * be used as the grid field if 1) token mpe_selected_grid_gagediff is NOT
     * set AND 2)the "Best Estimate
     * QPE" field IS included in mpe_generate_list token. Otherwise, "No field
     * selected" will be highlighted in the combobox list
     */
    private void populateGridCombo() {
        // Determine the selected grid
        gridCombo.removeActionListener(gridComboListener);
        if (selectedGrid == null) {
            for (int i = 0; i < columnData.size(); i++) {
                if (columnData.get(i).getPrefix()
                        .equalsIgnoreCase(GageTableProductManager.MPE_BESTQPE)) {
                    selectedGrid = "BESTQPE";
                    break;
                }
            }
        }

        int gridSelectionIndex = 0;
        for (int i = 0; i < columnData.size(); i++) {
            if (columnData.get(i).isDataColumn()) {
                gridCombo.addItem(columnData.get(i).getName());
                if (selectedGrid
                        .equalsIgnoreCase(columnData.get(i).getPrefix())) {
                    gridComboSelection = gridSelectionIndex;
                }
                gridSelectionIndex++;
            }
        }
        gridCombo.addActionListener(gridComboListener);
        gridCombo.setSelectedIndex(gridComboSelection);
    }

    private void createButtons(Container container) {
        JButton saveButton = new JButton("Save");
        JButton cancelButton = new JButton("Cancel");

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.anchor = GridBagConstraints.LAST_LINE_START;
        constraints.fill = GridBagConstraints.NONE;
        constraints.insets = new Insets(5, 5, 5, 5);

        constraints.weightx = 0.0;
        constraints.weighty = 0.0;
        constraints.gridx = 0;
        constraints.gridy = 3;

        constraints.gridwidth = 1;
        constraints.gridheight = 1;

        container.add(saveButton, constraints);

        constraints.anchor = GridBagConstraints.EAST;
        constraints.gridx = 1;
        container.add(cancelButton, constraints);

        saveButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                saveChanges();
            }
        });

        cancelButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                confirmation();
            }
        });
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
            GageTableDataManager.setNull();
            dispose();
        }
    }

    /**
     * Create the JTable.
     */
    private void createJTable(Container container) {
        gageTablePanel = new JPanel();
        gageTablePanel.setLayout(new GridLayout(1, 1));
        table = null;
        table = new JTable();
        tableModel = new GageTableModel();
        table.setModel(tableModel);
        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

        JTableHeader header = table.getTableHeader();
        header.addMouseListener(new ColumnHeaderListener());

        // Disable autoCreateColumnsFromModel otherwise all the column
        // customizations
        // and adjustments will be lost when the model data is sorted
        table.setAutoCreateColumnsFromModel(false);
        table.setColumnSelectionAllowed(false);
        table.setRowSelectionAllowed(true);
        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        table.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);

        /* Center all table data */
        JLabel renderer = ((JLabel) table.getDefaultRenderer(Object.class));
        renderer.setHorizontalAlignment(SwingConstants.CENTER);

        int columnCount = table.getModel().getColumnCount();

        ColumnHeaderToolTips tips = new ColumnHeaderToolTips();

        for (int i = 0; i < columnCount; i++) {
            TableColumn col = table.getColumnModel().getColumn(i);
            col.setHeaderRenderer(new GageTableHeaderCellRenderer());
            col.setMinWidth(25);
            GageTableColumn c = columnData.get(i);
            col.setPreferredWidth(c.getWidth());
            tips.setToolTip(col, c.getToolTipText());
        }

        header.addMouseMotionListener(tips);
        table.getModel().addTableModelListener(new GageTableModelListener());

        scrollPane = new JScrollPane(table);
        gageTablePanel.add(scrollPane);

        GridBagConstraints constraints = new GridBagConstraints();
        constraints.anchor = GridBagConstraints.LINE_START;
        constraints.fill = GridBagConstraints.BOTH;
        constraints.insets = new Insets(5, 5, 5, 5);

        constraints.weightx = 1.0;
        constraints.weighty = 1.0;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.gridheight = 1;

        container.add(gageTablePanel, constraints);
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
    public void sortAllRowsBy(GageTableModel model, int colIndex,
            boolean ascending) {
        Vector<Vector<String>> data = model.getDataVector();
        Collections.sort(data, new ColumnSorter(colIndex, ascending));
        model.setDataVector(data);
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

        // This rectangle is relative to the table where the
        // northwest corner of cell (0,0) is always (0,0).
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

        List<GageTableColumn> availableProductColumnList = new ArrayList<GageTableColumn>();
        Map<String, GageTableColumn> prodMap = manager
                .getGageTableProductColumnMap();

        Set<String> keySet = prodMap.keySet();

        Iterator<String> iter = keySet.iterator();

        while (iter.hasNext()) {
            availableProductColumnList.add(prodMap.get(iter.next()));
        }

        List<String> availableListItems = new ArrayList<String>();

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
        ItemsSelectionDialog dlg = new ItemsSelectionDialog(
                this,
                "Gage Table Column Selector",
                availableListItems.toArray(new String[availableListItems.size()]),
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
                GageTableColumn col = colMap.get(manager
                        .lookupProductPrefix(value));
                col.setName(value);
                col.setPrefix(manager.lookupProductPrefix(value));
                colList.add(col);
            }
        }

        manager.setSelectedColumns(colList);
        dataManager.setColumnDataList(colList);

        // Fire event to notify listeners of changes
        GageTableUpdateEvent event = new GageTableUpdateEvent(this, true);
        manager.fireUpdateEvent(event);
    }

    /**
     * Read the settings XML file. There is a single file for the site.
     */
    private void readSettingsFile() {
        IPathManager pm = PathManagerFactory.getPathManager();
        GageTableDataManager dataManager = GageTableDataManager.getInstance();
        GageTableProductManager prodManager = GageTableProductManager
                .getInstance();
        Map<String, GageTableColumn> columnMap = prodManager
                .getGageTableProductColumnMap();

        List<GageTableColumn> columnDataList = new ArrayList<GageTableColumn>();
        try {
            String path = pm.getFile(
                    pm.getContext(LocalizationType.COMMON_STATIC,
                            LocalizationLevel.SITE),
                    "hydro" + File.separatorChar
                            + "MPEGageTableDisplaySettings.xml")
                    .getAbsolutePath();
            File f = new File(path);
            GageTableSettings settings = null;

            if (f.exists()) {
                settings = JAXB.unmarshal(f, GageTableSettings.class);
            } else {
                settings = getDefaultSettings();
            }
            List<GageTableColumnData> columnSettingList = settings.getColumn();
            Map<String, Integer> columnWidthMap = dataManager
                    .getColumnWidthMap();

            for (GageTableColumnData c : columnSettingList) {
                GageTableColumn column = null;
                if (prodManager.lookupProductPrefix(c.getName()) != null) {
                    GageTableProductDescriptor prodDesc = columnMap.get(
                            prodManager.lookupProductPrefix(c.getName()))
                            .getProductDescriptor();
                    ;
                    column = new GageTableColumn(prodDesc);
                    column.setName(prodDesc.getProductName());
                    column.setToolTipText(prodDesc.getProductDescription());
                } else {
                    // Non-data column, doesn't have a product descriptor
                    column = new GageTableColumn(null);
                    column.setName(c.getName());

                    if (column.getName().equalsIgnoreCase("LID")) {
                        column.setToolTipText("Location ID");
                    } else if (column.getName().startsWith("Diff")) {
                        column.setToolTipText("Difference between Gage Value and Grid Data");
                    } else {
                        column.setToolTipText(column.getName());
                    }
                }
                column.setWidth(c.getWidth().intValue());
                columnWidthMap.put(column.getName(), column.getWidth());

                if (c.getSort() != null) {
                    column.setSortOrder(c.getSort().getOrder().intValue());
                    column.setAscending(c.getSort().isAscending());
                }
                if (baseColumns.contains(column.getName())) {
                    column.setDataColumn(false);
                } else {
                    column.setDataColumn(true);
                }

                columnDataList.add(column);
            }

            dataManager.setColumnDataList(columnDataList);
            dataManager.setColumnWidthMap(columnWidthMap);
        } catch (Exception e) {
            System.out.println("MPE Settings file not found");
        }
    }

    /**
     * Save the settings out to an XML file.
     */
    private void saveSettings() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile newXmlFile = pm.getLocalizationFile(lc,
                "hydro/MPEGageTableDisplaySettings.xml");

        try {
            JAXB.marshal(getSettingsXML(), newXmlFile.getFile());
            newXmlFile.save();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Gets the default settings and creates a settings object.
     * 
     * @return GageTableSettings data
     */
    private GageTableSettings getDefaultSettings() {
        GageTableSettings settings = null;
        GageTableProductManager prodManager = GageTableProductManager
                .getInstance();

        settings = new GageTableSettings();

        // Get the non-data columns
        String[] baseColumns = GageTableConstants.BASE_COLUMNS;
        for (String s : baseColumns) {
            GageTableColumnData col = new GageTableColumnData();
            col.setName(s);
            col.setWidth(BigInteger.valueOf(GageTableConstants.DEFAULT_WIDTH));
            settings.getColumn().add(col);
        }

        // Get the data columns defined in Apps_defaults
        List<GageTableColumn> colList = prodManager
                .getAvailableGageTableColumnList();

        for (GageTableColumn tableCol : colList) {
            GageTableColumnData col = new GageTableColumnData();
            col.setName(tableCol.getName());
            col.setWidth(BigInteger.valueOf(GageTableConstants.DEFAULT_WIDTH));
            settings.getColumn().add(col);
        }

        return settings;
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
        GageTableSortSettings settings = dataMan.getColumnSettings();
        int sortCol1 = -999;
        int sortCol2 = -999;
        int sortCol3 = -999;
        int sortCol4 = -999;

        if (settings != null) {
            sortCol1 = settings.getSortCol1Index();
            sortCol2 = settings.getSortCol2Index();
            sortCol3 = settings.getSortCol3Index();
            sortCol4 = settings.getSortCol4Index();
        }
        // Remove duplicate sort columns
        if (sortCol2 == sortCol1) {
            sortCol2 = -999;
        }

        if ((sortCol3 == sortCol2) || (sortCol3 == sortCol1)) {
            sortCol3 = -999;
        }

        if ((sortCol4 == sortCol3) || (sortCol4 == sortCol2)
                || (sortCol4 == sortCol1)) {
            sortCol4 = -999;
        }

        Enumeration<TableColumn> colEnum = table.getColumnModel().getColumns();
        int index = 0;
        for (; colEnum.hasMoreElements();) {
            TableColumn col = colEnum.nextElement();
            GageTableColumnData data = new GageTableColumnData();
            data.setName((String) col.getHeaderValue());
            data.setWidth(BigInteger.valueOf(col.getWidth()));

            if ((sortCol1 != -999) && (sortCol1 == index)) {
                GageTableSortType sort = new GageTableSortType();
                sort.setOrder(BigInteger.ZERO);
                if (settings.getAscending1() == 1) {
                    sort.setAscending(true);
                } else {
                    sort.setAscending(false);
                }
                data.setSort(sort);
            }

            if ((sortCol2 != -999) && (sortCol2 == index)) {
                GageTableSortType sort = new GageTableSortType();
                sort.setOrder(BigInteger.valueOf(1));
                if (settings.getAscending2() == 1) {
                    sort.setAscending(true);
                } else {
                    sort.setAscending(false);
                }
                data.setSort(sort);
            }

            if ((sortCol3 != -999) && (sortCol3 == index)) {
                GageTableSortType sort = new GageTableSortType();
                sort.setOrder(BigInteger.valueOf(2));
                if (settings.getAscending3() == 1) {
                    sort.setAscending(true);
                } else {
                    sort.setAscending(false);
                }
                data.setSort(sort);
            }

            if ((sortCol4 != -999) && (sortCol4 == index)) {
                GageTableSortType sort = new GageTableSortType();
                sort.setOrder(BigInteger.valueOf(3));
                if (settings.getAscending4() == 1) {
                    sort.setAscending(true);
                } else {
                    sort.setAscending(false);
                }
                data.setSort(sort);
            }

            settingsElement.getColumn().add(data);
            index++;
        }

        return settingsElement;
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
                        dataManager.setSelectedGrid(prod
                                .getProductFilenamePrefix());
                        break;
                    }
                }
            }
            sortAllRowsBy(tableModel, sortColumnIndex, ascending);

        }
    }

    /**
     * Key listener for the search text field.
     */
    public class KeyActionListener extends KeyAdapter {
        @Override
        public void keyPressed(KeyEvent evt) {

        }

        @Override
        public void keyReleased(KeyEvent evt) {
            String lid = searchTextField.getText().toUpperCase();
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
    public class ColumnHeaderListener extends MouseAdapter {

        @Override
        public void mouseClicked(MouseEvent evt) {
            if ((evt.getButton() == MouseEvent.BUTTON1)
                    || (evt.getButton() == MouseEvent.BUTTON3)) {
                GageTableDataManager dataManager = GageTableDataManager
                        .getInstance();
                GageTableSortSettings columnSettings = dataManager
                        .getColumnSettings();

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

                columnSettings = setSortColumns(columnSettings,
                        sortColumnIndex, ascending);

                dataManager.setColumnSettings(columnSettings);

                sortAllRowsBy(tableModel, sortColumnIndex, ascending);
            }
        }
    }

    /**
     * This comparator is used to sort vectors of data.
     */
    public class ColumnSorter implements Comparator<Vector<String>> {
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
                    if (o1.toString().trim().equalsIgnoreCase("M")) {
                        response = -1;
                    } else if (o2.toString().trim().equalsIgnoreCase("M")) {
                        response = 1;
                    } else {
                        response = o1.compareTo(o2);
                    }
                } else {
                    if (o1.toString().trim().equalsIgnoreCase("M")) {
                        response = 1;
                    } else if (o2.toString().trim().equalsIgnoreCase("M")) {
                        response = -1;
                    } else {
                        response = o2.compareTo(o1);
                    }
                }
            }
            // if equal use the id to make the final determination
            if (response == 0 && colIndex != 0) {
                ColumnSorter columnSorter = new ColumnSorter(0, true);
                response = columnSorter.compare(v1, v2);

            }
            return response;
        }

    }

    /**
     * Controls the column header tool tips.
     */
    public class ColumnHeaderToolTips extends MouseMotionAdapter {
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
            getContentPane().setCursor(new Cursor(Cursor.WAIT_CURSOR));
            int width = GageTableConstants.DEFAULT_WIDTH;

            if ((e != null) && (e.getType() == TableModelEvent.UPDATE)) {
                table = null;
                table = new JTable(tableModel);

                JTableHeader header = table.getTableHeader();
                header.addMouseListener(new ColumnHeaderListener());
                // Disable autoCreateColumnsFromModel otherwise all the column
                // customizations and adjustments will be lost when the model
                // data is sorted
                table.setAutoCreateColumnsFromModel(false);
                table.setColumnSelectionAllowed(false);
                table.setRowSelectionAllowed(true);
                table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                // make the user's edits stored without need for type "return"
                // key or click
                // on JTable
                table.putClientProperty("terminateEditOnFocusLost",
                        Boolean.TRUE);

                /* Center all table data */
                JLabel renderer = ((JLabel) table
                        .getDefaultRenderer(Object.class));
                renderer.setHorizontalAlignment(SwingConstants.CENTER);

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
                        if (row.getGageData()
                                .getId()
                                .equalsIgnoreCase(
                                        (String) tableModel.getValueAt(cellRow,
                                                0))) {
                            rowData = row;
                            break;
                        }
                    }

                    // Update the edited row.
                    // The custom cell editor makes sure that the value is
                    // either a real number
                    // or 'm' or 'M' or "".
                    if (newValue.equals("")) {
                        rowData.setValueEdited(false);
                        // rowData.setEditValue(-999.0);
                    } else {
                        if (newValue.equalsIgnoreCase("m")) {
                            rowData.setEditValue(-999.0);
                        } else {
                            rowData.setEditValue(new Float(newValue)
                                    .floatValue());
                        }
                        rowData.setValueEdited(true);
                    }
                    // rowData.setValueEdited(true);
                    int indexOf = rowDataList.indexOf(rowData);
                    rowDataList.set(indexOf, rowData);
                    editMap.put(
                            (String) tableModel.getValueAt(e.getFirstRow(), 0),
                            rowData);
                    dataChanged = true;
                }
                dataManager.setGageTableRowList(rowDataList);
                gageTablePanel.remove(scrollPane);
                scrollPane = new JScrollPane(table);
                gageTablePanel.add(scrollPane);

                List<GageTableColumn> columnList = dataManager
                        .getColumnDataList();
                ColumnHeaderToolTips tips = new ColumnHeaderToolTips();

                for (int i = 0; i < columnList.size(); i++) {
                    TableColumn col = table.getColumnModel().getColumn(i);
                    col.setHeaderRenderer(new GageTableHeaderCellRenderer());
                    col.setMinWidth(25);
                    GageTableColumn c = columnList.get(i);

                    if (dataManager.getColumnWidthMap().get(c.getName()) == null) {
                        width = GageTableConstants.DEFAULT_WIDTH;
                    } else {
                        width = dataManager.getColumnWidthMap()
                                .get(c.getName());
                    }
                    col.setPreferredWidth(width);
                    tips.setToolTip(col, c.getToolTipText());
                }

                header.addMouseMotionListener(tips);

                // Update the grid combobox
                gridCombo.removeAllItems();
                columnData = columnList;
                populateGridCombo();

            } else {
                Enumeration<TableColumn> colEnum = table.getColumnModel()
                        .getColumns();
                for (; colEnum.hasMoreElements();) {
                    TableColumn tableColumn = colEnum.nextElement();
                    tableColumn
                            .setHeaderRenderer(new GageTableHeaderCellRenderer());
                }
            }
            // sortTable();
            getContentPane().setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
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
                    gageData.setEdit(String.valueOf(editMap.get(lid)
                            .getEditValue() * 25.4));
                } else {
                    if (editMap.get(lid).getEditValue() == -999) {
                        gageData.setEdit("M");
                    } else {
                        gageData.setEdit(String.valueOf(editMap.get(lid)
                                .getEditValue()));
                    }
                    gageData.setManedit(true);
                }
                mpeDataManager.addEditedGage(gageData);
            }
        }

        setVisible(false);
        tableModel = null;
        GageTableDataManager.setNull();
        dispose();
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
    private GageTableSortSettings setSortColumns(
            GageTableSortSettings settings, int index, boolean ascending) {
        settings.setSortCol4Index(settings.getSortCol3Index());
        settings.setSortCol3Index(settings.getSortCol2Index());
        settings.setSortCol2Index(settings.getSortCol1Index());
        settings.setSortCol1Index(index);

        settings.setAscending4(settings.getAscending3());
        settings.setAscending3(settings.getAscending2());
        settings.setAscending2(settings.getAscending1());
        if (ascending) {
            settings.setAscending1(1);
        } else {
            settings.setAscending1(0);
        }

        if (settings.getSortCol1Index() == settings.getSortCol2Index()) {
            settings.setSortCol3Index(settings.getSortCol2Index());
            settings.setSortCol2Index(settings.getSortCol1Index());
            settings.setAscending3(settings.getAscending2());
            settings.setAscending2(settings.getAscending1());
        } else if (settings.getSortCol1Index() == settings.getSortCol3Index()) {
            settings.setSortCol3Index(settings.getSortCol2Index());
            settings.setAscending3(settings.getAscending2());
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
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.IEditTimeChangedListener#editTimeChanged(java
     * .util.Date, java.util.Date)
     */
    @Override
    public void editTimeChanged(Date oldTime, Date newTime) {
        updateDate(newTime);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.Window#dispose()
     */
    @Override
    public void dispose() {
        if (displayManager != null) {
            displayManager.unregisterEditTimeChangedListener(this);
        }
        super.dispose();
    }

}
