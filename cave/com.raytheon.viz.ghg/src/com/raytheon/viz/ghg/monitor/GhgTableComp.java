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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.osgi.framework.Bundle;

import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ghg.Activator;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.DataEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.SelectionEnum;
import com.raytheon.viz.ghg.monitor.data.GhgData;
import com.raytheon.viz.ghg.monitor.data.GhgTableRowData;
import com.raytheon.viz.ghg.monitor.event.AbstractGhgMonitorEvent.GhgEventListener;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorTableSelectionEvent;

/**
 * This class contains the GHG table and handle all of the table interactions.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation 
 * 19Jun2008    1157       MW Fegan    Added banner popup for alerts.
 * 27Mar2009    1881       wldougher   Enhance performance
 * 27Aug2013    2301       dgilling    Fix Image loading for icons.
 * Dec 16, 2015 5184       dgilling    Remove viz.gfe dependencies.
 * Feb 05, 2016 #5316      randerso    Moved notification registration into GHGMonitorDlg
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GhgTableComp extends Composite implements IGhgSelectedTableColumn {

    /**
     * Font used for editors.
     */
    private Font originalTableFont;

    /**
     * Font data used to set the font of the data in the table.
     */
    private FontData tableFontData;

    /**
     * GHG table containing all of the GHG data.
     */
    private Table ghgTable;

    /**
     * A map of the column names (key) and whether they are visible or not
     * (value).
     */
    private Map<String, Boolean> columnsMap;

    /**
     * ArrayList of table columns.
     */
    private List<TableColumn> tableColumns;

    /**
     * ArrayList of the rows of data in the table.
     */
    private List<GhgTableRowData> ghgTableRowArray;

    /**
     * Previous ArrayList of the rows of data in the table.
     */
    private List<GhgTableRowData> lastGhgTableRowArray = new ArrayList<GhgTableRowData>();

    /**
     * The current selected column.
     */
    private int selectedColumn = -1;

    /**
     * Sort image indicating an ascending sort.
     */
    private Image upImage;

    /**
     * Sort image indicating an descending sort.
     */
    private Image downImage;

    /**
     * The last column selected for sorting.
     */
    private TableColumn lastSelectedColumn;

    /**
     * Table selection listener list
     */
    private List<GhgEventListener> tableSelectionListenerList = new ArrayList<>();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param columnsMap
     *            Map of the visible/hidden table columns.
     */
    public GhgTableComp(Composite parent, /* GhgDisplayManager displayMgr, */
            Map<String, Boolean> columnsMap) {
        super(parent, SWT.NONE);

        this.columnsMap = columnsMap;

        init();
    }

    /**
     * Initialize the composite.
     */
    private void init() {
        Display display = getParent().getDisplay();

        Bundle bundle = Activator.getDefault().getBundle();
        upImage = IconUtil.getImage(bundle, "sortUp.gif", display);
        downImage = IconUtil.getImage(bundle, "sortDown.gif", display);

        // Get the Font Data for the Table
        originalTableFont = new Font(display, "Monospace", 10, SWT.NORMAL);

        tableFontData = originalTableFont.getFontData()[0];

        originalTableFont.dispose();

        // Set up the composite.
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 200;
        gd.widthHint = 600;
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        setLayout(gl);
        setLayoutData(gd);

        initializeComponents();

        addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent arg0) {
                disposeItems();
            }
        });
    }

    /**
     * Dispose needed items
     */
    private void disposeItems() {
        upImage.dispose();
        downImage.dispose();
        ghgTable.dispose();

        for (GhgTableRowData row : ghgTableRowArray) {
            if (row != null) {
                row.disposeTableItem();
            }
        }
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {
        ghgTableRowArray = new ArrayList<GhgTableRowData>();

        createTableAndColumns();
    }

    /**
     * This forces the table to resize its columns and paint correctly
     */
    public void updateTable() {
        for (int i = 0; i < ghgTableRowArray.size(); i++) {
            GhgTableRowData row = ghgTableRowArray.get(i);
            row.regenerateTableItem();
            row.setHighlight();
        }

        // Sort the table data.
        sortTableData(lastSelectedColumn);

        packColumns();
    }

    /**
     * Create the GHG table and the table columns.
     */
    private void createTableAndColumns() {
        // Create the GHG table.
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 150;
        gd.widthHint = 600;
        ghgTable = new Table(this, SWT.BORDER | SWT.MULTI | SWT.VIRTUAL
                | SWT.FULL_SELECTION);
        ghgTable.setHeaderVisible(true);
        ghgTable.setLayoutData(gd);
        ghgTable.setSortDirection(SWT.UP);

        // Add a listener to the table to support the virtual table style
        ghgTable.addListener(SWT.SetData, new Listener() {

            @Override
            public void handleEvent(Event event) {
                int index = event.index;
                ghgTable.deselect(index);
                TableItem item = (TableItem) event.item;
                GhgTableRowData row = ghgTableRowArray.get(index);
                row.setRegularColors(item, index, row.getGhgData());
                GhgConfigData config = GhgConfigData.getInstance();
                Display display = item.getDisplay();
                Font font = config.getCurrentFont().getFont(display);

                // Calling dispose() on the old font here generates segfaults...
                item.setFont(font);
                item.setText(row.getGhgData().getDataCellNames());
            }
        });

        // Add a listener to the table so we can determine when the table is
        // mouse clicked.
        ghgTable.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                event.doit = false;
                setSelection();
            }
        });

        tableColumns = new ArrayList<TableColumn>();

        Set<String> keys = columnsMap.keySet();

        String key;
        int counter = 0;

        // Create the table columns.
        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            key = iterator.next();
            TableColumn tc = new TableColumn(ghgTable, SWT.CENTER);
            tc.setText(key);
            tc.setMoveable(false);
            tc.setData(new Integer(counter));

            // Make the "Purge" column the default sorted column.
            if (key.compareTo("Purge") == 0) {
                selectedColumn = counter;
                lastSelectedColumn = tc;
            }

            ++counter;

            tc.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    // If there is no data to sort then return.
                    if (ghgTableRowArray.size() == 0) {
                        return;
                    }

                    TableColumn tc = (TableColumn) event.getSource();
                    if ((lastSelectedColumn != null)
                            && lastSelectedColumn.equals(tc)) {
                        int oldDir = ghgTable.getSortDirection();
                        int newDir = (oldDir == SWT.UP) ? SWT.DOWN : SWT.UP;
                        ghgTable.setSortDirection(newDir);
                    }

                    refreshSortImage(tc);

                    // Sort the table data.
                    sortTableData(tc);

                    setHighlight();
                }

            });

            if (columnsMap.get(key) == false) {
                tc.setWidth(0);
                tc.setResizable(false);
            } else {
                tc.setWidth(150);
            }

            tableColumns.add(tc);
        }

        packColumns();
    }

    /**
     * Add new GHG data to the table.
     * 
     * @param data
     *            GHG data.
     */
    public void addGhgData(GhgData data) {

        // Create the new GHG data record.
        GhgTableRowData rowData = new GhgTableRowData(ghgTable, data, this,
                tableFontData);

        // Add the data record
        ghgTableRowArray.add(rowData);

        ghgTable.setItemCount(ghgTableRowArray.size());

        // Clear all the rows after the one that changed so SWT will reload
        // any that are visible.
        // ghgTable.clearAll();

        refreshSortImage(lastSelectedColumn);

        packColumns();
    }

    /**
     * 
     */
    public void refreshSortImage(TableColumn tc) {
        if (lastSelectedColumn != null) {
            lastSelectedColumn.setImage(null);
        }
        lastSelectedColumn = tc;

        if (tc != null) {
            if (ghgTableRowArray.size() > 0) {
                if (ghgTable.getSortDirection() == SWT.DOWN) {
                    tc.setImage(downImage);
                } else {
                    tc.setImage(upImage);
                }
            } else {
                tc.setImage(null);
            }
        }
    }

    /**
     * 
     * @param index
     * @return
     */
    public GhgData getGhgRowData(int index) {
        return ghgTableRowArray.get(index).getGhgData();
    }

    /**
     * Sort the table data by the table column passed in.
     * 
     * @param tc
     *            Column to be sorted.
     * @param updateSortImage
     *            Sort ascending/descending.
     */
    public void sortTableData(TableColumn tc) {
        int dir = ghgTable.getSortDirection();

        ghgTable.setSortDirection(dir);

        selectedColumn = ((Integer) tc.getData()).intValue();
        Collections.sort(ghgTableRowArray);
        ghgTable.clearAll();

        for (int i = 0; i < ghgTableRowArray.size(); i++) {
            GhgTableRowData row = ghgTableRowArray.get(i);
            row.regenerateTableItem();
        }

        packColumns();
        lastSelectedColumn = tc;
    }

    /**
     * Pack the columns to resize to fit the data.
     */
    public void packColumns() {
        for (int i = 0; i < tableColumns.size(); i++) {
            TableColumn tc = tableColumns.get(i);

            String tcText = tc.getText();
            if (Boolean.TRUE.equals(columnsMap.get(tcText))) {
                tc.setResizable(true);
                tc.pack();
                for (DataEnum denum : DataEnum.values()) {
                    if (denum.columnLabel.equals(tcText)) {
                        if (tc.getWidth() < denum.minColWidth) {
                            tc.setWidth(denum.minColWidth);
                        }
                        break;
                    }
                }
            } else {
                tc.setWidth(0);
                tc.setResizable(false);
            }
        }

        refreshSortImage(lastSelectedColumn);
    }

    /**
     * Show/Hide the selected columns.
     */
    public void showHideColumns() {
        for (int i = 0; i < ghgTableRowArray.size(); i++) {
            GhgTableRowData row = ghgTableRowArray.get(i);
            row.setHighlight();
        }

        packColumns();
    }

    /**
     * Get the selected table column.
     * 
     * @return Table column index.
     */
    @Override
    public int getSelectedColumn() {
        return selectedColumn;
    }

    /**
     * Get the indexes of all of the visible columns.
     * 
     * @return Indexes of the visible columns.
     */
    @Override
    public List<Integer> getVisibleColumnIndexes() {
        ArrayList<Integer> intArray = new ArrayList<Integer>();
        TableColumn tc;
        for (int i = 0; i < tableColumns.size(); i++) {
            tc = tableColumns.get(i);

            if (Boolean.TRUE.equals(columnsMap.get(tc.getText()))) {
                intArray.add(i);
            }
        }

        return intArray;
    }

    /**
     * Update the font of the data in the GHG table.
     * 
     * @param fontData
     *            Font data to be used to set the font.
     */
    public void updateTableFont(FontData fontData) {
        tableFontData = fontData;
        for (int i = 0; i < ghgTableRowArray.size(); i++) {
            ghgTableRowArray.get(i).setTableItemFont(tableFontData);
            ghgTableRowArray.get(i).regenerateTableItem();
        }
        packColumns();
    }

    /**
     * Update the colors in the GHG table to reflect any color changes.
     */
    public void updateDataColors() {
        for (int i = 0; i < ghgTableRowArray.size(); i++) {
            ghgTableRowArray.get(i).regenerateTableItem();
        }
        packColumns();
    }

    public Table getGhgTable() {
        return ghgTable;
    }

    public void setGhgTable(Table ghgTable) {
        this.ghgTable = ghgTable;
    }

    public void setDescending(int colnum, boolean descending) {
        if (descending) {
            ghgTable.setSortDirection(SWT.DOWN);
        } else {
            ghgTable.setSortDirection(SWT.UP);
        }
        TableColumn tc = ghgTable.getColumn(colnum);
        refreshSortImage(tc);
    }

    /**
     * This is just a wrapper around ghgTable's method.
     * 
     * @param columnOrder
     */
    public void setColumnOrder(int[] columnOrder) {
        ghgTable.setColumnOrder(columnOrder);
    }

    public void clear() {
        ghgTable.setItemCount(0);

        // Copy this list of row data
        lastGhgTableRowArray.clear();
        lastGhgTableRowArray.addAll(ghgTableRowArray);

        ghgTableRowArray.clear();
        ghgTable.clearAll();
    }

    /**
     * Select a row in response to a click on the table.
     */
    private void setSelection() {
        // Find out which row was clicked.
        int selectedIndex = ghgTable.getSelectionIndex();

        // Clear SWT's selection so we can highlight with our colors.
        ghgTable.deselectAll();

        for (int i = 0; i < ghgTableRowArray.size(); i++) {
            GhgTableRowData row = ghgTableRowArray.get(i);
            row.setMapSelected(false);

            if (i == selectedIndex) {
                row.setSelected(true);

                // update the map to highlight this selection
                String geoIds = row.getGhgData().getGeoId();
                String[] geoIdArray = geoIds.split(",");

                GhgMonitorTableSelectionEvent evt = new GhgMonitorTableSelectionEvent();
                evt.setHighlightedZones(Arrays.asList(geoIdArray));
                List<GhgData> dataList = new ArrayList<GhgData>(1);
                GhgData rowData = row.getGhgData();
                rowData.setSelection(SelectionEnum.MonitorSelection);
                dataList.add(rowData);
                evt.setGhgData(dataList);
                evt.setSelectionColor(GhgConfigData.getInstance()
                        .getMonitorSelectionsColors().getBackgroundRgb());
                fireTableSelectionEvent(evt);
            } else {
                row.setSelected(false);
            }

            row.setHighlight();
        }
        packColumns();
    }

    private void setSelection(Collection<String> highlightedZones) {
        ghgTable.deselectAll();
        Set<String> idSet = new HashSet<String>();
        List<GhgData> dataList = new ArrayList<GhgData>();

        for (int i = 0; i < ghgTableRowArray.size(); i++) {
            GhgTableRowData row = ghgTableRowArray.get(i);

            // unselect the selected rows
            if (row.isSelected()) {
                row.setSelected(false);
                row.setHighlight();
            }

            if (row.isMapSelected()) {
                row.setMapSelected(false);
                row.setHighlight();
            }

            List<String> idList = new ArrayList<String>();

            for (String s : highlightedZones) {
                if (row.getGhgData().getGeoId().contains(s)) {
                    row.setMapSelected(true);
                    row.setHighlight();
                    idList.add(row.getGhgData().getGeoId());
                }
            }

            for (String s : idList) {
                if (s.contains(",")) {
                    String[] parts = s.split(",");
                    for (String id : parts) {
                        idSet.add(id);
                    }
                } else {
                    idSet.add(idList.get(0));
                }
            }

            dataList.add(row.getGhgData());
        }

        // build the event object
        GhgMonitorTableSelectionEvent evt = new GhgMonitorTableSelectionEvent();
        evt.setHighlightedZones(idSet);
        evt.setGhgData(dataList);
        evt.setSelectionColor(GhgConfigData.getInstance()
                .getMapSelectionsColors().getBackgroundRgb());

        fireTableSelectionEvent(evt);
    }

    public void update(Collection<String> highlightedZones) {
        setSelection(highlightedZones);
    }

    /**
     * Set the highlighting in the dialog. Normally, there is only one selected
     * row, a map selection or a monitor (table) selection. However, when a
     * "combine" checkbox is unchecked, a single-row selection may become
     * expanded into multiple rows.
     */
    public void setHighlight() {

        GhgMonitorTableSelectionEvent evt = new GhgMonitorTableSelectionEvent();
        List<GhgData> dataList = new ArrayList<GhgData>(1);
        StringBuilder geoIdBuffer = new StringBuilder();
        String sep = "";
        // Go through the list looking for map- or monitor-selected rows.
        for (int i = 0; i < ghgTableRowArray.size(); i++) {
            GhgTableRowData row = ghgTableRowArray.get(i);
            row.setHighlight();
            if (row.isSelected() || row.isMapSelected()) {

                // Accumulate GeoIDs.
                geoIdBuffer.append(sep);
                geoIdBuffer.append(row.getGhgData().getGeoId());
                sep = ",";

                dataList.add(row.getGhgData());
                if (row.isMapSelected()) {
                    evt.setSelectionColor(GhgConfigData.getInstance()
                            .getMapSelectionsColors().getBackgroundRgb());
                } else {
                    evt.setSelectionColor(GhgConfigData.getInstance()
                            .getMonitorSelectionsColors().getBackgroundRgb());
                }
            }
        }

        if (geoIdBuffer.length() > 0) {
            // update the map to highlight this selection
            String geoIds = geoIdBuffer.toString();
            String[] geoIdArray = geoIds.split(",");
            evt.setHighlightedZones(Arrays.asList(geoIdArray));
            evt.setGhgData(dataList);
            fireTableSelectionEvent(evt);
        }

        sortTableData(lastSelectedColumn);
        packColumns();
    }

    /**
     * Get the uncombined GhgData records of the monitor selection (the rows
     * that were selected by a click in the table). If no rows are selected,
     * return an empty list.
     * 
     * @return the list of GhgDatas of the monitor selection row(s).
     */
    public List<GhgData> getMonitorSelectionData() {
        List<GhgData> selectionData = new ArrayList<GhgData>();
        for (GhgTableRowData row : ghgTableRowArray) {
            if (row.isSelected()) {
                selectionData.addAll(row.getGhgData().getCombinedList());
            }
        }
        return selectionData;
    }

    /**
     * Add a listener to the list.
     * 
     * @param listener
     */
    public void addSelectionListener(GhgEventListener listener) {
        if (!tableSelectionListenerList.contains(listener)) {
            tableSelectionListenerList.add(listener);
        }
    }

    /**
     * Remove a listener from the list.
     * 
     * @param listener
     */
    public void removeSelectionListener(GhgEventListener listener) {
        tableSelectionListenerList.remove(listener);
    }

    /**
     * Fire the table change event.
     * 
     * @param evt
     *            The GhgMonitorTableSelectionEvent object
     */
    private void fireTableSelectionEvent(GhgMonitorTableSelectionEvent evt) {
        for (GhgEventListener listener : tableSelectionListenerList) {
            listener.notifyUpdate(evt);
        }
    }

}