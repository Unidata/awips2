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
package com.raytheon.uf.viz.monitor.ui.dialogs;

import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.SortDirection;
import com.raytheon.uf.viz.monitor.data.ColumnAttribData;
import com.raytheon.uf.viz.monitor.data.TableCellData;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.uf.viz.monitor.data.TableRowData;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants;

/**
 * Abstract table composite that is the main foundation for a table displaying
 * TableData.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr  7, 2009            lvenable    Initial creation
 * Oct  7, 2013  #2436     lvenable    Disposed of the sort color.
 * Nov  7, 2013  DR 16703  gzhang      Check in code for Lee for FFMP and Safeseas
 * Feb 23, 2016  #5393     randerso    Fix column width issues, code cleanup
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class TableComp extends Composite {
    /**
     * Main table control.
     */
    protected Table table;

    /**
     * Parent composite.
     */
    protected Composite parent;

    /**
     * Data to be displayed in the table.
     */
    protected TableData tableData;

    /**
     * Application name.
     */
    protected CommonConfig.AppName appName;

    /**
     * Table item font.
     */
    private Font tiFont;

    /**
     * Table index variable to keep track of the table index.
     */
    protected int tableIndex = -1;

    /**
     * Color of the lines defining the borders of the table cell.
     */
    private Color lineColor;

    private int imageWidth;

    private int imageHeight;

    private Color sortColor;

    /**
     * A map that contains a table column and the sort direction.
     */
    protected HashMap<TableColumn, Integer> columnSortMap;

    private TableColumn sortColumn;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param data
     *            Data to be displayed in the table.
     * @param appName
     *            Application name.
     */
    public TableComp(Composite parent, TableData data,
            CommonConfig.AppName appName) {
        super(parent, 0);

        this.parent = parent;
        this.tableData = data;

        this.appName = appName;
    }

    /**
     * Initialize method.
     */
    protected void init() {
        tiFont = new Font(parent.getDisplay(), "Sans", 10, SWT.NORMAL);

        sortColor = new Color(parent.getDisplay(), 133, 104, 190);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        makeImageCalculations();

        initData();

        addTopTableControls(this);

        createTable();

        createTableColumns();

        createTableItems();

        packColumns();

        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent arg0) {
                tiFont.dispose();
                lineColor.dispose();
                sortColor.dispose();
                if (sortColumn != null) {
                    Image image = sortColumn.getImage();
                    if (image != null) {
                        image.dispose();
                    }
                }
            }
        });
    }

    /**
     * Determine the height and width of the image displayed in the table
     * column. Since all of the icons have to be the same size, all of the table
     * column names have to be looked at to determine the number of lines and
     * the maximum number of characters for the largest table column title.
     */
    private void makeImageCalculations() {
        Image image = new Image(this.getDisplay(), 100, 100);
        GC gc = new GC(image);
        gc.setFont(tiFont);

        int maxTextWidth = 0;
        int maxTextHeight = 0;

        String[] columnKeys = getColumnKeys(appName);

        for (String key : columnKeys) {
            String columnName = getColumnAttributeData(key).getColumnName();

            Point textExtent = gc.textExtent(columnName);
            maxTextWidth = Math.max(maxTextWidth, textExtent.x);
            maxTextHeight = Math.max(maxTextHeight, textExtent.y);
        }

        imageWidth = maxTextWidth + 2;
        imageHeight = maxTextHeight + 2;

        gc.dispose();
        image.dispose();
    }

    /**
     * Initialize the data.
     */
    private void initData() {
        lineColor = new Color(parent.getDisplay(), 80, 80, 80);
        columnSortMap = new HashMap<TableColumn, Integer>();
    }

    /**
     * Create the table.
     */
    private void createTable() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 350;

        table = new Table(this, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.FULL_SELECTION);
        table.setLayoutData(gd);
        table.setHeaderVisible(true);

        /*
         * Add a paint listener to the table that will draw lines around each
         * cell and also draw a blue line to show the selected row.
         */
        table.addListener(SWT.PaintItem, new Listener() {
            @Override
            public void handleEvent(Event event) {
                table.deselectAll();
                event.gc.setForeground(lineColor);
                event.gc.setLineWidth(1);
                int currentCol = event.index;

                Rectangle rect = ((TableItem) event.item).getBounds(currentCol);
                event.gc.drawRectangle(rect.x - 1, rect.y - 1, rect.width,
                        rect.height);

                // Draw an extra line on the edges of the table cell to hide the
                // white lines
                // dividing the columns;
                event.gc.setLineWidth(1);
                event.gc.drawLine(rect.x + rect.width - 2, rect.y - 1, rect.x
                        + rect.width - 2, rect.y - 1 + rect.height);

                // Draw a top line
                event.gc.drawLine(rect.x, rect.y, rect.x + rect.width, rect.y);

                // Draw a bottom line if this is the last row of the table
                TableItem ti = (TableItem) event.item;
                int idx = table.indexOf(ti);
                if (idx == table.getItemCount() - 1) {
                    event.gc.drawLine(rect.x, rect.y + rect.height - 2, rect.x
                            + rect.width, rect.y + rect.height - 2);
                }

                if (tableIndex >= 0) {
                    event.gc.setForeground(parent.getDisplay().getSystemColor(
                            SWT.COLOR_BLUE));
                    event.gc.setLineWidth(3);
                    TableItem item = table.getItem(tableIndex);
                    rect = item.getBounds(currentCol);
                    event.gc.drawRectangle(rect.x - 1, rect.y - 1, rect.width,
                            rect.height);
                }
            }
        });

        /*
         * Add a mouse listener to the table.
         */
        table.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent event) {
                if (event.button == 1) {
                    tableMouseDownAction(event);
                } else if (event.button == 3) {
                    tableColRightMouseAction(event);
                }
            }

        });

        table.addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent event) {
                tableMouseHoverAction(event);
            }
        });

    }

    /**
     * Create the table columns.
     */
    private void createTableColumns() {
        TableColumn tc;
        String[] columns = getColumnKeys(appName);

        for (int i = 0; i < columns.length; i++) {
            tc = new TableColumn(table, SWT.CENTER);
            String columnName = getColumnAttributeData(columns[i])
                    .getColumnName();
            tc.setText(columnName);
            tc.setData(columns[i]);

            tc.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    // If there is no data to sort then return.
                    if (table.getItemCount() == 0) {
                        return;
                    }

                    TableColumn tc = (TableColumn) event.getSource();

                    // Sort the table data.
                    sortTableData(tc);
                }
            });

            if (getColumnAttributeData(columns[i]).getSortDir() == CommonTableConfig.SortDirection.Both
                    .getSortDir()) {
                columnSortMap.put(tc, SWT.UP);
            }
        }

        // TODO : Need to hide columns on startup if needed...
    }

    /**
     * Create the table items.
     */
    private void createTableItems() {
        table.removeAll();

        if (tableData == null) {
            return;
        }

        // Loop of the rows of data in the table data object.
        for (TableRowData rowData : tableData.getTableRows()) {
            TableItem ti = new TableItem(table, SWT.NONE);
            ti.setFont(tiFont);

            TableCellData[] cellData = rowData.getTableCellDataArray();

            for (int i = 0; i < cellData.length; i++) {
                ti.setText(i, cellData[i].displayString());
                Color c = new Color(parent.getDisplay(),
                        cellData[i].getBackgroungRGB());
                ti.setBackground(i, c);
                c.dispose();
            }
        }
    }

    /**
     * Set the data to be displayed in the table.
     * 
     * @param td
     */
    public void setTableData(TableData td) {
        tableData = td;

        createTableItems();

        TableColumn column = sortColumn;
        if (column == null) {
            column = table.getColumn(0);
        }

        sortTableData(column);

        packColumns();
    }

    /**
     * Sort the data in the table by the specified table column.
     * 
     * @param tc
     *            Table column to sort.
     */
    private void sortTableData(TableColumn tc) {
        /*
         * Check if the column is sortable.
         */
        String sortCol = (String) tc.getData();
        int sortDir = getColumnAttributeData(sortCol).getSortDir();

        if (sortDir == SWT.NONE) {
            return;
        }

        if (sortColumn != null) {
            Image image = sortColumn.getImage();
            sortColumn.setImage(null);
            image.dispose();
            sortColumn.setText(getColumnAttributeData(
                    (String) sortColumn.getData()).getColumnName());
        }

        sortColumn = tc;

        /*
         * Set the sort image, pack the column and sort the data.
         */
        tc.setImage(getSortHeaderImage(getColumnAttributeData(sortCol)
                .getColumnName()));
        tc.setText("");
        if (sortDir == SortDirection.Both.getSortDir()) {
            if (columnSortMap.get(tc) == SWT.UP) {
                sortDir = SWT.DOWN;
                columnSortMap.put(tc, SWT.DOWN);
            } else {
                sortDir = SWT.UP;
                columnSortMap.put(tc, SWT.UP);
            }
        }

        int columnIndex = getColumnIndex(appName, sortCol);

        if (sortCol == "SSZT_SwellPeriod" || sortCol == "SSZT_Swell2Period") {
            if (MonitorConfigConstants.isRankSwellPeriodHigh()) {
                sortDir = SWT.DOWN;
            } else {
                sortDir = SWT.UP;
            }
        }
        tableData.setSortColumnAndDirection(columnIndex, sortDir);

        tableData.sortData();

        createTableItems();

    }

    private Image getSortHeaderImage(String header) {
        Image image = new Image(parent.getDisplay(), imageWidth, imageHeight);
        GC gc = new GC(image);
        gc.setFont(tiFont);
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.setBackground(sortColor);
        gc.fillRectangle(0, 0, imageWidth, imageHeight);

        Point textExtent = gc.textExtent(header);
        int xCoord = (imageWidth - textExtent.x) / 2;
        int yCoord = (imageHeight - textExtent.y) / 2;

        gc.drawText(header, xCoord, yCoord, true);
        return image;
    }

    /**
     * Get the list of visible table columns.
     * 
     * @return Boolean array indicating which columns are visible or not.
     */
    public boolean[] getVisibleColumns() {
        TableColumn[] tCols = table.getColumns();
        boolean[] visibleCols = new boolean[tCols.length];

        for (int i = 0; i < tCols.length; i++) {
            if (tCols[i].getWidth() == 0) {
                visibleCols[i] = false;
            } else {
                visibleCols[i] = true;
            }
        }

        return visibleCols;
    }

    /**
     * Show/Hide table columns.
     * 
     * @param visCols
     *            Boolean array indicating which table columns are visible.
     */
    public void showHideTableColumns(boolean[] visCols) {
        TableColumn[] tCols = table.getColumns();

        for (int i = 0; i < visCols.length; i++) {
            if (!visCols[i]) {
                tCols[i].setWidth(0);
            } else {
                table.getColumn(i).pack();
                table.getColumn(i).setWidth(table.getColumn(i).getWidth() + 2);
            }
        }
    }

    protected abstract void tableColRightMouseAction(MouseEvent event);

    /**
     * Handle the mouse button down on the table.
     * 
     * @param event
     *            Mouse event.
     */
    protected abstract void tableMouseDownAction(MouseEvent event);

    /**
     * Handle the mouse hover on the table.
     * 
     * @param event
     *            Mouse event.
     */
    protected abstract void tableMouseHoverAction(MouseEvent event);

    /**
     * Add controls above the table (if needed).
     * 
     * @param parentComp
     *            Parent composite.
     */
    protected abstract void addTopTableControls(Composite parentComp);

    /**
     * Get the column keys.
     * 
     * @param appName
     *            Application name.
     * @return String array of column keys.
     */
    protected abstract String[] getColumnKeys(CommonConfig.AppName appName);

    /**
     * Get the column attributes for the specified column name.
     * 
     * @param colName
     *            Column name.
     * @return Column attribute data.
     */
    protected abstract ColumnAttribData getColumnAttributeData(String colName);

    /**
     * Get the column index.
     * 
     * @param appName
     *            Application name.
     * @param sortCol
     *            Sort column.
     * @return Column index.
     */
    protected abstract int getColumnIndex(CommonConfig.AppName appName,
            String sortCol);

    /**
     * Pack the table columns.
     */
    private void packColumns() {
        for (int i = 0; i < table.getColumnCount(); i++) {
            TableColumn column = table.getColumn(i);
            column.pack();
            column.setWidth(column.getWidth() + 2);
        }

        getShell().pack();
    }

}
