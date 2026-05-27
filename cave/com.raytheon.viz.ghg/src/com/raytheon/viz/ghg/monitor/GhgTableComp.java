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
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.ghg.monitor.data.GhgAlertData;
import com.raytheon.viz.ghg.monitor.data.GhgAlertsConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgColorData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.AlertsEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.DataEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.GhgFontSizeEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.SelectionEnum;
import com.raytheon.viz.ghg.monitor.data.GhgData;
import com.raytheon.viz.ghg.monitor.event.AbstractGhgMonitorEvent.GhgEventListener;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorTableSelectionEvent;

/**
 * This class contains the GHG table and handle all of the table interactions.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Mar 25, 2008  N/A      lvenable   Initial creation
 * Jun 19, 2008  1157     MW Fegan   Added banner popup for alerts.
 * Mar 27, 2009  1881     wldougher  Enhance performance
 * Aug 27, 2013  2301     dgilling   Fix Image loading for icons.
 * Dec 16, 2015  5184     dgilling   Remove viz.gfe dependencies.
 * Feb 05, 2016  5316     randerso   Moved notification registration into
 *                                   GHGMonitorDlg
 * Nov 01, 2016  5979     njensen    Cleanup
 * Sep 04, 2019  7919     randerso   Complete rewrite
 * Oct 28, 2020  8268     randerso   Updated to preserve column widths on sort
 * Nov 02, 2020  8268     randerso   Filter nulls from columnsToPack
 *
 * </pre>
 *
 * @author randerso
 */
public class GhgTableComp extends Composite {

    private static final String RESIZED_WIDTH = "resizedWidth";

    private static final String UNSORTED_WIDTH = "unsortedWidth";

    private static final String SORTED_WIDTH = "sortedWidth";

    private static final int COLUMN_PADDING = 16;

    private static final DataEnum DEFAULT_SORT_COLUMN = DataEnum.PURGE;

    /**
     * Provides text for the columns in the attribute viewer from the ghgData
     * list
     *
     */
    private class TableLabelProvider extends LabelProvider
            implements ITableLabelProvider {

        @Override
        public Image getColumnImage(Object element, int columnIndex) {
            return null;
        }

        @Override
        public String getColumnText(Object element, int columnIndex) {
            return get(DataEnum.values()[columnIndex], (GhgData) element);
        }

        public String get(DataEnum field, GhgData ghgData) {
            return ghgData.getDisplayString(field);
        }

    }

    private class ListContentProvider implements IStructuredContentProvider {
        private List<Object> contents;

        @Override
        public Object[] getElements(Object input) {
            if (contents != null && contents == input) {
                return contents.toArray();
            }
            return new Object[0];
        }

        @SuppressWarnings("unchecked")
        @Override
        public void inputChanged(Viewer viewer, Object oldInput,
                Object newInput) {
            if (newInput instanceof List) {
                contents = (List<Object>) newInput;
            } else {
                contents = null;
            }
        }
    }

    /**
     * Compares rows using values from the selected column and direction for
     * sorting the table in the desired order.
     *
     */
    private static class ColumnComparator extends ViewerComparator {

        private DataEnum sortColumn;

        private int sortDirection = SWT.UP;

        @Override
        @SuppressWarnings({ "unchecked" })
        public int compare(Viewer viewer, Object a, Object b) {
            Comparable<Object> aValue = (Comparable<Object>) ((GhgData) a)
                    .getAttribute(sortColumn);
            Comparable<Object> bValue = (Comparable<Object>) ((GhgData) b)
                    .getAttribute(sortColumn);

            return aValue.compareTo(bValue)
                    * (sortDirection == SWT.UP ? 1 : -1);
        }

        /**
         * Set the column by which the table should be sorted
         *
         * @param sortColumn
         *            DataEnum of sort column
         * @param sortDirection
         *            SWT.UP for ascending, SWT.DOWN for descending
         */
        public void setSortColumn(DataEnum sortColumn, int sortDirection) {
            this.sortColumn = sortColumn;
            this.sortDirection = sortDirection;
        }

    }

    private TableViewer viewer;

    private List<GhgData> tableData;

    private Set<DataEnum> visibleColumns;

    private Map<RGB, Color> allocatedColors = new HashMap<>();

    private Font currentFont = null;

    private ListenerList<GhgEventListener> selectionListeners = new ListenerList<>(
            ListenerList.IDENTITY);

    private boolean packing = false;

    /**
     * Constructor
     *
     * @param parent
     */
    public GhgTableComp(Composite parent) {
        super(parent, SWT.NONE);
        this.tableData = new ArrayList<>();

        GridLayout layout = new GridLayout();
        setLayout(layout);

        viewer = new TableViewer(this,
                SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL | SWT.FULL_SELECTION);

        Table table = viewer.getTable();
        table.setLinesVisible(true);

        table.setHeaderVisible(true);

        for (DataEnum data : DataEnum.values()) {
            TableViewerColumn tvc = new TableViewerColumn(viewer, SWT.LEFT,
                    data.ordinal());
            final TableColumn column = tvc.getColumn();
            column.setText(data.columnLabel);
            column.setData(data);

            column.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    setSortColumn((TableColumn) e.getSource(), null);
                }
            });

            column.addControlListener(new ControlAdapter() {

                @Override
                public void controlResized(ControlEvent e) {
                    /* if not resizing due to repacking */
                    if (!packing) {
                        TableColumn tc = (TableColumn) e.getSource();

                        /* save resized width */
                        tc.setData(RESIZED_WIDTH, tc.getWidth());
                    }
                }

            });
        }

        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.heightHint = table.getItemHeight() * 5;
        table.setLayoutData(layoutData);

        IContentProvider contentProvider = new ListContentProvider();
        viewer.setLabelProvider(new TableLabelProvider());
        viewer.setContentProvider(contentProvider);
        viewer.setUseHashlookup(true);
        viewer.setInput(tableData);

        table.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                setSelection();
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                setSelection();
            }

        });

        ColumnComparator comparator = new ColumnComparator();
        viewer.setComparator(comparator);

        visibleColumns = EnumSet.allOf(DataEnum.class);

        packColumns(false);
        setSortColumn(DEFAULT_SORT_COLUMN, true);
    }

    @Override
    public void dispose() {
        super.dispose();

        if (currentFont != null) {
            currentFont.dispose();
            currentFont = null;
        }

        synchronized (allocatedColors) {
            Iterator<Entry<RGB, Color>> iter = allocatedColors.entrySet()
                    .iterator();
            while (iter.hasNext()) {
                iter.next().getValue().dispose();
                iter.remove();
            }
        }
    }

    protected void setSelection() {
        // Find out which row was clicked.
        Table table = viewer.getTable();
        int row = table.getSelectionIndex();
        TableItem selectedItem = null;
        if (row >= 0) {
            selectedItem = table.getItem(row);
        }

        // Clear SWT's selection so we can highlight with our colors.
        table.deselectAll();

        for (TableItem item : table.getItems()) {
            if (item != selectedItem) {
                GhgData ghgData = (GhgData) item.getData();
                ghgData.setSelection(SelectionEnum.NoSelection);
                highlightRow(item, SelectionEnum.NoSelection);
            }
        }

        if (selectedItem != null) {
            // update the map to highlight this selection
            GhgData ghgData = (GhgData) selectedItem.getData();
            String geoIds = ghgData.getGeoId();
            String[] geoIdArray = geoIds.split(",");

            GhgMonitorTableSelectionEvent evt = new GhgMonitorTableSelectionEvent();
            evt.setHighlightedZones(List.of(geoIdArray));
            List<GhgData> dataList = new ArrayList<>(1);
            ghgData.setSelection(SelectionEnum.MonitorSelection);
            dataList.add(ghgData);
            evt.setGhgData(dataList);
            evt.setSelectionColor(GhgConfigData.getInstance()
                    .getMonitorSelectionsColors().getBackgroundRgb());
            fireTableSelectionEvent(evt);

            highlightRow(selectedItem, SelectionEnum.MonitorSelection);
        }
    }

    private void highlightRow(TableItem tableItem, SelectionEnum source) {
        boolean alertFlag = false;
        List<GhgColorData> colorList = new ArrayList<>();

        GhgColorData cellColor = null;
        GhgConfigData config = GhgConfigData.getInstance();

        Table table = viewer.getTable();
        GhgData ghgData = (GhgData) tableItem.getData();
        /* Determine what colors are appropriate for highlighting */

        // Get the alert color if it exists
        if (ghgData != null) {
            cellColor = getAlertColor(ghgData);
        }

        // alert status, add the alert color to the list
        if (cellColor != null) {
            colorList.add(cellColor);
            alertFlag = true;
        }

        // Add selection state color to the color list
        switch (source) {
        case MapSelection:
            cellColor = config.getMapSelectionsColors();
            colorList.add(cellColor);
            break;
        case MonitorSelection:
            cellColor = config.getMonitorSelectionsColors();
            colorList.add(cellColor);
            break;

        default:
            if (!alertFlag) {
                cellColor = config.getRegularEntriesColors();
                colorList.add(cellColor);
            }
        }

        if (ghgData != null
                && GhgConfigData.getInstance().isIdentifyTestEvents()) {
            // test products with VTEC "T" codes get "Test" highlighting
            String vtecStr = ghgData.getVtecString();
            if ("T".equals(vtecStr.substring(1, 2))) {
                cellColor = config.getTestProductsColors();
                colorList.add(cellColor);
            }
        }

        Color fgColor = null;
        Color bgColor = null;
        int colorIdx = 0;
        for (int i = 0; i < table.getColumnCount(); i++) {
            DataEnum data = (DataEnum) table.getColumn(i).getData();
            if (visibleColumns.contains(data)) {
                GhgColorData colorData = colorList.get(colorIdx);
                fgColor = allocateColor(colorData.getForegroundRgb());
                bgColor = allocateColor(colorData.getBackgroundRgb());
                tableItem.setForeground(i, fgColor);
                tableItem.setBackground(i, bgColor);
                colorIdx++;
                if (colorIdx >= colorList.size()) {
                    colorIdx = 0;
                }
            }
        }
    }

    private Color allocateColor(RGB rgb) {
        synchronized (allocatedColors) {
            Color color = allocatedColors.get(rgb);
            if (color == null) {
                color = new Color(getDisplay(), rgb);
                allocatedColors.put(rgb, color);
            }
            return color;
        }
    }

    private GhgColorData getAlertColor(GhgData record) {
        GhgConfigData config = GhgConfigData.getInstance();
        GhgAlertsConfigData alerts = config.getAlerts();

        if (alerts == null) {
            return null;
        }

        int currentMinute = (int) (SimulatedTime.getSystemTime().getTime()
                .getTime() / TimeUtil.MILLIS_PER_MINUTE);

        // NATIONAL WEATHER SERVICE INSTRUCTION 10-1703
        GhgAlertData alert1 = alerts.getAlert(AlertsEnum.AlertLvl1);
        int minutesToAlert1 = alert1.getTime();

        GhgAlertData alert2 = alerts.getAlert(AlertsEnum.AlertLvl2);
        int minutesToAlert2 = alert2.getTime();

        Date endDate = record.getEndDate();
        int purgeTimeMinutes = (int) (endDate.getTime()
                / TimeUtil.MILLIS_PER_MINUTE);
        int deltaMinP = purgeTimeMinutes - currentMinute;
        if (deltaMinP <= 0) {
            return config.getExpiredAlertColors();
        } else if (deltaMinP <= minutesToAlert2) {
            return config.getAlertLvl2Colors();
        } else if (deltaMinP <= minutesToAlert1) {
            return config.getAlertLvl1Colors();
        }

        return null;
    }

    private void fireTableSelectionEvent(GhgMonitorTableSelectionEvent evt) {
        for (GhgEventListener listener : selectionListeners) {
            listener.notifyUpdate(evt);
        }
    }

    private int computeColumnWidth(GC gc, DataEnum data) {
        int width = 0;
        switch (data) {
        case ACTION:
            for (String action : GhgConfigData.vtecActionNames) {
                width = Math.max(width, gc.textExtent(action).x);
            }
            break;

        case END:
            width = Math.max(gc.textExtent("99:99Z 99-Mmm-99").x,
                    gc.textExtent(GhgData.UFN_STRING).x);
            break;

        case START:
        case ISSUE_TIME:
        case PURGE:
            width = gc.textExtent("99:99Z 99-Mmm-99").x;
            break;

        case ETN:
            width = gc.textExtent("0000").x;
            break;

        case GEO_ID:
            width = gc.textExtent("WWW999,WWW999,WWW999").x;
            break;

        case HAZARD:
            for (Map<String, String> map : GhgConfigData.getInstance()
                    .getVtecTable().values()) {
                String hdln = map.get("hdln");
                if (hdln != null) {
                    width = Math.max(width, gc.textExtent(hdln).x);
                }
            }
            break;

        case PHEN:
            for (Map<String, String> map : GhgConfigData.getInstance()
                    .getVtecTable().values()) {
                String phen = map.get("phen");
                if (phen != null) {
                    width = Math.max(width, gc.textExtent(phen).x);
                }
            }
            break;

        case PHEN_SIG:
            for (String phensig : GhgConfigData.getInstance().getVtecTable()
                    .keySet()) {
                width = Math.max(width, gc.textExtent(phensig).x);
            }
            break;
        case PIL:
            for (String pil : GhgConfigData.vtecPILNames) {
                width = Math.max(width, gc.textExtent(pil).x);
            }
            break;

        case PRODUCT_CLASS:
            width = gc.textExtent("W").x;
            break;

        case SEG:
            width = gc.textExtent("99").x;
            break;

        case SIG:
            for (Map<String, String> map : GhgConfigData.getInstance()
                    .getVtecTable().values()) {
                String sig = map.get("sig");
                if (sig != null) {
                    width = Math.max(width, gc.textExtent(sig).x);
                }
            }
            break;

        case VTEC_STRING:
            width = gc.textExtent(
                    "/W.WWW.WWWW.WW.W.9999.999999T9999Z-999999T9999Z/").x;
            break;

        case WFO:
            width = gc.textExtent("WWWW").x;
            break;

        default:
            break;

        }
        return width + COLUMN_PADDING;
    }

    /**
     * @param visibleColumns
     */
    public void setVisibleColumns(Set<DataEnum> visibleColumns) {
        this.visibleColumns = visibleColumns;
        packColumns(false);
        updateDataColors();
    }

    /**
     * @param column
     *            DataEnum value of sort column
     * @param ascending
     *            True for ascending, False for descending, null for toggle
     */
    public void setSortColumn(DataEnum column, Boolean ascending) {
        for (TableColumn tc : viewer.getTable().getColumns()) {
            if (column.equals(tc.getData())) {
                setSortColumn(tc, ascending);
                break;
            }
        }
    }

    /**
     * @param newSortColumn
     *            newSortColumn
     * @param ascending
     *            True for ascending, False for descending, null for toggle
     */
    private void setSortColumn(TableColumn newSortColumn, Boolean ascending) {
        Table table = viewer.getTable();
        TableColumn oldSortColumn = table.getSortColumn();
        table.setSortColumn(newSortColumn);

        int sortDirection = table.getSortDirection();
        if (ascending == null) {
            if (oldSortColumn == newSortColumn) {
                /* toggle current direction */
                sortDirection = (table.getSortDirection() == SWT.UP ? SWT.DOWN
                        : SWT.UP);
            }
        } else {
            sortDirection = (ascending ? SWT.UP : SWT.DOWN);
        }

        table.setSortDirection(sortDirection);
        ColumnComparator comparator = ((ColumnComparator) viewer
                .getComparator());
        comparator.setSortColumn((DataEnum) newSortColumn.getData(),
                sortDirection);

        viewer.refresh();
        if (oldSortColumn != newSortColumn) {
            packColumns(true, oldSortColumn, newSortColumn);
        }
        updateDataColors();
    }

    /**
     * @param listener
     */
    public void addSelectionListener(GhgEventListener listener) {
        this.selectionListeners.add(listener);
    }

    /**
     * @param listener
     */
    public void removeSelectionListener(GhgEventListener listener) {
        this.selectionListeners.remove(listener);
    }

    /**
     * @param fontEnum
     */
    public void setTableFont(GhgFontSizeEnum fontEnum) {
        Font font = new Font(getDisplay(), fontEnum.getFontName(),
                fontEnum.getFontHeight(), SWT.NORMAL);
        Table table = viewer.getTable();
        table.setFont(font);
        if (currentFont != null) {
            currentFont.dispose();
        }
        currentFont = font;

        /* clear stored widths after font change */
        for (TableColumn tc : table.getColumns()) {
            tc.setData(SORTED_WIDTH, null);
            tc.setData(UNSORTED_WIDTH, null);
            tc.setData(RESIZED_WIDTH, null);
        }
        packColumns(false);
    }

    /**
     *
     */
    public void updateDataColors() {
        Table table = viewer.getTable();
        for (TableItem item : table.getItems()) {
            GhgData data = (GhgData) item.getData();
            highlightRow(item, data.getSelection());
        }
    }

    /**
     * @return the monitor selection data
     */
    public List<GhgData> getMonitorSelectionData() {
        List<GhgData> selectionData = new ArrayList<>();
        for (GhgData ghgData : tableData) {
            if (ghgData.isMonitorSelected()) {
                selectionData.addAll(ghgData.getCombinedList());
            }
        }

        return selectionData;
    }

    /**
     *
     */
    public void clear() {
        this.tableData.clear();
        viewer.setInput(this.tableData);
    }

    /**
     * @param ghgData
     */
    public void addGhgData(GhgData ghgData) {
        this.tableData.add(ghgData);
        viewer.add(ghgData);
    }

    /**
     *
     */
    public void setHighlight() {
        GhgMonitorTableSelectionEvent evt = new GhgMonitorTableSelectionEvent();
        List<GhgData> dataList = new ArrayList<>(1);
        StringBuilder geoIdBuffer = new StringBuilder();
        String sep = "";
        // Go through the table looking for map- or monitor-selected rows.
        for (TableItem item : viewer.getTable().getItems()) {
            GhgData ghgData = (GhgData) item.getData();
            // row.setHighlight();
            if (ghgData.isMapSelected() || ghgData.isMapSelected()) {

                // Accumulate GeoIDs.
                geoIdBuffer.append(sep);
                geoIdBuffer.append(ghgData.getGeoId());
                sep = ",";

                dataList.add(ghgData);
                if (ghgData.isMapSelected()) {
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
            evt.setHighlightedZones(List.of(geoIdArray));
            evt.setGhgData(dataList);
            fireTableSelectionEvent(evt);
        }
    }

    private void packColumns(boolean forSorting, TableColumn... columns) {
        packing = true;

        Table table = viewer.getTable();
        GC gc = new GC(table);
        int totalWidth = 0;

        List<TableColumn> columnsToPack;
        if (columns.length > 0) {
            columnsToPack = Arrays.stream(columns).filter(tc -> tc != null)
                    .collect(Collectors.toList());
        } else {
            columnsToPack = List.of(table.getColumns());
        }
        TableColumn sortColumn = table.getSortColumn();
        for (TableColumn column : table.getColumns()) {
            DataEnum data = (DataEnum) column.getData();
            if (visibleColumns.contains(data)) {
                int width = column.getWidth();
                if (columnsToPack.contains(column)) {
                    column.pack();
                    width = column.getWidth();

                    int extent = computeColumnWidth(gc, data);

                    width = Math.max(width, extent);

                    if (column.getData(UNSORTED_WIDTH) == null
                            && column != sortColumn) {
                        /* save width without sort indicator */
                        column.setData(UNSORTED_WIDTH, width);
                    }

                    if (column.getData(SORTED_WIDTH) == null
                            && column == sortColumn) {
                        /* save width with sort indicator */
                        column.setData(SORTED_WIDTH, width);
                    }

                    Integer resizedWidth = (Integer) column
                            .getData(RESIZED_WIDTH);

                    if (resizedWidth != null) {
                        width = resizedWidth;
                    }

                    if (forSorting) {
                        if (column == sortColumn) {
                            Integer sortedWidth = (Integer) column
                                    .getData(SORTED_WIDTH);
                            if (sortedWidth != null && sortedWidth > width) {
                                width = sortedWidth;
                            }
                        } else {
                            Integer unsortedWidth = (Integer) column
                                    .getData(UNSORTED_WIDTH);
                            if (unsortedWidth != null
                                    && width > unsortedWidth) {
                                if (resizedWidth != null) {
                                    width = resizedWidth;
                                } else {
                                    width = unsortedWidth;
                                }
                            }
                        }
                    }

                    column.setWidth(width);
                }
                totalWidth += width;
            } else {
                column.setWidth(0);
            }
        }
        gc.dispose();

        if (!getShell().isVisible()) {
            ((GridData) table.getLayoutData()).widthHint = totalWidth;
        }

        packing = false;
    }

    /**
     * @return the Table GUI element
     */
    public Table getGhgTable() {
        return this.viewer.getTable();
    }

    /**
     * @return the selected column
     */
    public DataEnum getSelectedColumn() {
        return (DataEnum) viewer.getTable().getSortColumn().getData();
    }

    /**
     * @param highlightedZones
     */
    public void update(Collection<String> highlightedZones) {
        Table table = viewer.getTable();
        table.deselectAll();

        Set<String> idSet = new HashSet<>();
        List<GhgData> dataList = new ArrayList<>();

        for (TableItem item : table.getItems()) {
            highlightRow(item, SelectionEnum.NoSelection);

            List<String> idList = new ArrayList<>();
            GhgData ghgData = (GhgData) item.getData();
            if (ghgData != null) {
                for (String s : highlightedZones) {
                    if (ghgData.getGeoId().contains(s)) {
                        highlightRow(item, SelectionEnum.MapSelection);
                        idList.add(ghgData.getGeoId());
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

                dataList.add(ghgData);
            }
        }

        // build the event object
        GhgMonitorTableSelectionEvent evt = new GhgMonitorTableSelectionEvent();
        evt.setHighlightedZones(idSet);
        evt.setGhgData(dataList);
        evt.setSelectionColor(GhgConfigData.getInstance()
                .getMapSelectionsColors().getBackgroundRgb());

        fireTableSelectionEvent(evt);
    }

}
