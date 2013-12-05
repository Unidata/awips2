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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
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

import com.raytheon.uf.common.monitor.data.ColumnAttribData;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.ThreshColNames;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfigData.COLUMN_NAME;
import com.raytheon.uf.viz.monitor.ffmp.xml.FFMPConfigBasinXML;
import com.raytheon.uf.viz.monitor.ffmp.xml.FFMPTableColumnXML;

/**
 * Abstract table composite that is the main foundation for a table displaying
 * TableData.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2009            lvenable     Initial creation
 * Mar 15,2012	DR 14406  gzhang       Fixing QPF Column Title Missing 
 * Mar 20,2012	DR 14250  gzhang       Eliminating column Missing values 
 * Aug 01, 2012 14168     mpduff       Only allow filtering if ColorCell is true
 * Jun 04, 2013 #1984     lvenable     Save images instead of disposing them when setting
 *                                     the table column images.  This is to fix the Windows
 *                                     issue on the images being blank and throwing errors.
 *                                     Also cleaned up some code.
 * Jun 11, 2013 2075      njensen      Optimized createTableItems()
 * Nov 07, 2013 DR 16703  gzhang	   Check in code for Lee for FFMP Table line
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class FFMPTable extends Composite {
    /** Default column width */
    protected static final int DEFAULT_COLUMN_WIDTH = 95;// DR14406: old value:
                                                         // 75 too small

    /** DR14406: For columns with more words */
    protected static final int EXTRA_COLUMN_WIDTH = 28;

    private static final String NAME = "Name";

    protected String currentPfaf = null;

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
    protected FFMPTableData tableData;

    /**
     * Application name.
     */
    protected CommonConfig.AppName appName = AppName.FFMP;

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

    /**
     * Color of the table column when sorted.
     */
    private Color sortColor;

    /**
     * Default width for each column.
     */
    private int defaultColWidth;

    protected boolean columnMinimumSize = false;

    private TableColumn sortedTableColumn;

    private FFMPConfig ffmpConfig;

    private String siteKey;

    protected Font columnFont;

    protected int imageWidth = 0;

    protected int imageHeight = 0;

    protected int textWidth = 0;

    protected int textHeight = 0;

    private ArrayList<Integer> indexArray = new ArrayList<Integer>();

    private Point extent = new Point(0, 0);

    /**
     * Array of images displayed in the table columns.
     */
    protected List<Image> columnImgs = new ArrayList<Image>();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param data
     *            Data to be displayed in the table.
     */
    public FFMPTable(Composite parent, FFMPTableData data, String siteKey) {
        super(parent, 0);

        this.siteKey = siteKey;
        this.parent = parent;
        this.tableData = data;

        ffmpConfig = FFMPConfig.getInstance();
    }

    /**
     * Initialize method.
     */
    protected void init() {
        tiFont = new Font(parent.getDisplay(), "Arial", 10, SWT.NORMAL);

        sortColor = new Color(parent.getDisplay(), 133, 104, 190);

        columnFont = new Font(parent.getDisplay(), "Monospace", 9, SWT.NORMAL);

        lineColor = new Color(parent.getDisplay(), 80, 80, 80);
        defaultColWidth = getDefaultColWidth();

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);// false);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        makeImageCalculations();

        addTopTableControls(this);

        createTable();

        createTableColumns();

        ffmpConfig.createAttributesDlgData(siteKey);

        sortTableUsingConfig();

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent arg0) {
                tiFont.dispose();
                lineColor.dispose();
                columnFont.dispose();
                sortColor.dispose();
                disposeColumnImages();
            }
        });
    }

    /**
     * Create the table.
     */
    private void createTable() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 350;

        table = new Table(this, SWT.BORDER | SWT.VIRTUAL | SWT.V_SCROLL
                | SWT.FULL_SELECTION);
        table.setLayoutData(gd);
        table.setHeaderVisible(true);

        /*
         * Add a paint listener to the table that will draw lines around each
         * cell and also draw a blue line to show the selected row.
         */
        table.addListener(SWT.PaintItem, new Listener() {

            public void handleEvent(Event event) {

                table.deselectAll();
                event.gc.setForeground(lineColor);
                event.gc.setLineWidth(1);
                int currentCol = event.index;
                
                TableItem ti = (TableItem) event.item;
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
                int index = table.indexOf(ti);
                if (index == table.getItemCount() - 1) {
                    event.gc.drawLine(rect.x, rect.y + rect.height - 2, rect.x
                            + rect.width, rect.y + rect.height - 2);
                }
                
                if ((tableIndex >= 0) && (tableIndex < table.getItemCount())) {
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

        table.addListener(SWT.SetData, new Listener() {
            public void handleEvent(Event event) {
                TableItem item = (TableItem) event.item;
                int tmpIndex = table.indexOf(item);
                int index = tableIndexToDataIndex(tmpIndex);

                item.setFont(tiFont);

                FFMPTableCellData[] cellData = tableData.getTableRows()
                        .get(index).getTableCellDataArray();

                for (int j = 0; j < cellData.length; j++) {
                    if (cellData[j] != null) {
                        item.setText(j, cellData[j].displayString());
                        item.setBackground(j, cellData[j].getBackgroungColor());
                    }
                }
                table.getColumn(0).setWidth(extent.x + 10);
                table.redraw();
            }
        });

        /*
         * Add a mouse listener to the table.
         */
        table.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent event) {
                tableMouseDownAction(event);
            }

        });

        table.addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {
                tableMouseMoveAction(e);
            }
        });
    }

    /**
     * Create the table columns.
     */
    private void createTableColumns() {
        TableColumn tc;
        String[] columns = getColumnKeys();

        for (int i = 0; i < columns.length; i++) {
            /*
             * Left justify the first column and center the remaining columns.
             */
            if (i == 0) {
                tc = new TableColumn(table, SWT.NONE);
            } else {
                tc = new TableColumn(table, SWT.CENTER);
            }

            tc.setData(columns[i]);
            tc.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    TableColumn[] cols = table.getColumns();
                    for (int j = 0; j < cols.length; j++) {
                        cols[j].setImage(null);
                        cols[j].setWidth(defaultColWidth);
                    }

                    // reset the tableIndex
                    tableIndex = -1;

                    /*
                     * Check of the column is sortable.
                     */
                    TableColumn tc = (TableColumn) event.getSource();

                    sortedTableColumn = tc;

                    if (tc.getWidth() < defaultColWidth) {
                        tc.setWidth(defaultColWidth);
                    }

                    // Sort the table data.
                    sortTableData(tc);
                }
            });

            if (columnMinimumSize == true) {
                tc.addControlListener(new ControlAdapter() {
                    @Override
                    public void controlResized(ControlEvent e) {
                        /*
                         * Prevent the user from resizing the column to be
                         * smaller than the default column size.
                         */
                        TableColumn tc = (TableColumn) e.getSource();
                        if ((tc.getWidth() < defaultColWidth)
                                && (tc.getWidth() != 0)) {
                            tc.setWidth(defaultColWidth);
                        }
                    }
                });
            }
        }

        int sortedColIdx = ffmpConfig.getStartSortIndex(this.siteKey);
        TableColumn stc = table.getColumn(sortedColIdx);

        sortedTableColumn = stc;
    }

    /**
     * Create the table items.
     */
    private void createTableItems() {
        int sortColIndex = table.indexOf(sortedTableColumn);
        boolean isAFilterCol = false;
        ThreshColNames sortedThreshCol = null;
        boolean reverseFilter = false;
        double filterNum = Double.NaN;

        String sortedColumnName = getColumnKeys()[sortColIndex];

        FFMPConfigBasinXML ffmpCfgBasin = FFMPConfig.getInstance()
                .getFFMPConfigData();

        ArrayList<FFMPTableColumnXML> ffmpTableCols = ffmpCfgBasin
                .getTableColumnData();

        boolean sortedColumnIsName = sortedColumnName.equalsIgnoreCase(NAME);

        if (!sortedColumnIsName) {
            for (ThreshColNames threshColName : ThreshColNames.values()) {
                if (sortedColumnName.contains(threshColName.name())) {
                    sortedThreshCol = threshColName;
                    break;
                }
            }

            // Check if the sorted column is a column that will contain a
            // filter. Check the gui config to see if colorCell is true. If
            // false then do not apply filter
            for (FFMPTableColumnXML xml : ffmpTableCols) {
                if (xml.getColumnName().contains(sortedThreshCol.name())) {
                    if (ffmpConfig.isColorCell(sortedThreshCol)) {
                        // Only filter if colorCell is true
                        isAFilterCol = true;
                        filterNum = ffmpConfig.getFilterValue(sortedThreshCol);
                        reverseFilter = ffmpConfig
                                .isReverseFilter(sortedThreshCol);
                    }
                    break;
                }
            }
        }

        table.removeAll();

        if (tableData == null) {
            return;
        }

        // Loop of the rows of data in the table data object.
        indexArray.clear();
        FFMPTableRowData rowData;
        ArrayList<FFMPTableRowData> rowArray = tableData.getTableRows();
        indexArray.ensureCapacity(rowArray.size());

        GC gc = new GC(table);
        gc.setFont(tiFont);

        for (int t = 0; t < rowArray.size(); t++) {

            rowData = rowArray.get(t);
            FFMPTableCellData[] cellData = rowData.getTableCellDataArray();

            extent.x = Math.max(gc.stringExtent(cellData[0].displayString()).x,
                    extent.x);

            /*
             * Check if the data value is Not A Number.
             */
            if (!sortedColumnIsName) {
                float dataVal = cellData[sortColIndex].getValueAsFloat();

                // DR 14250 fix: any value not a number will be omitted
                if (Float.isNaN(dataVal)) {
                    continue;
                }

                if (isAFilterCol) {
                    if (reverseFilter) {
                        if (dataVal > filterNum) {
                            continue;
                        }
                    } else {
                        if (dataVal < filterNum) {
                            continue;
                        }
                    }
                }
            }
            indexArray.add(t);

            // Check to see if this is the selected row
            if (rowData.getPfaf().equals(currentPfaf)) {
                tableIndex = indexArray.indexOf(t);
            }
        }

        /*
         * VIRTUAL TABLE
         * 
         * If a virtual table is going to be used then we may have to create a
         * temp Table Data object that will only contain the data that will be
         * displayed in the table (filtered data). It may work better for the
         * display if the the virtual table is loading from a second data set
         * for the display. This will allow a 1 to 1 with the index of the table
         * item and the index of the data. Not sure what the overhead will be
         * but it shouldn't be too bad. The table data is fairly small in size
         * and shouldn't be a memory issue.
         * 
         * Below, instead of creating table items the data should be moved to
         * the new list and then the table setItemCount should be called with
         * the about of data in the new list.
         * 
         * NOTE: When the data is ordered, filtering takes place on the column
         * of data. The filtered data is not displayed so setItemCount will only
         * display x number of rows. If the filtered data is at the bottom then
         * there shouldn't be an issue.
         * 
         * One possible way to solve this is to have an array that holds only
         * the indexes into the table data. For example: the array would be an
         * array of indexes and the size would be the number of table items that
         * should be displayed in the table.
         * 
         * When you select a table item cross-reference the table index with the
         * index array to get the right table data element.
         */

        // Set this for the virtual table
        table.setItemCount(indexArray.size());

        showHideTableColumns();

        if (indexArray.size() > 0) {
            table.getColumn(0).setWidth(extent.x + 10);
            table.getColumn(0).pack();
        }

        gc.dispose();
    }

    /**
     * Set the data to be displayed in the table.
     * 
     * @param td
     */
    public void setTableData(FFMPTableData td) {
        extent.x = 0;

        clearTableSelection();
        tableData = td;
        sortTableData(sortedTableColumn);
        calculateTableSize();
    }

    /**
     * Calculate the table size by adding the widths of each column + a little
     * extra.
     */
    public void calculateTableSize() {
        int hsize = 0;
        TableColumn[] tCols = table.getColumns();
        for (int i = 0; i < tCols.length; i++) {
            if (i == 0) {
                hsize += this.extent.x + 10;
            } else {
                hsize += tCols[i].getWidth();
            }
        }

        hsize += 20;
        ((GridData) table.getLayoutData()).widthHint = hsize;
    }

    /**
     * Clear the table selection.
     */
    public void clearTableSelection() {
        tableIndex = -1;
        table.deselectAll();
    }

    /**
     * Sort the table using the configuration data.
     */
    public void sortTableUsingConfig() {
        int sortedColIdx = ffmpConfig.getStartSortIndex(this.siteKey);
        TableColumn tc = table.getColumn(sortedColIdx);

        sortedTableColumn = tc;

        TableColumn[] cols = table.getColumns();
        for (int j = 0; j < cols.length; j++) {
            cols[j].setImage(null);
            cols[j].setWidth(defaultColWidth);
        }

        /*
         * Set the sort image, pack the column and sort the data.
         */
        setColumnImages();
        tc.pack();

        if (tc.getWidth() < defaultColWidth) {
            tc.setWidth(defaultColWidth);
        }

        sortTableData(tc);
    }

    /**
     * Get the index list
     * 
     * @return list of indices
     */
    protected List<Integer> getIndexList() {
        return indexArray;
    }

    /**
     * Get the data index from the table index.
     * 
     * @param tableIndex
     *            The table index
     * @return the data index
     */
    protected int tableIndexToDataIndex(int tableIndex) {
        if (tableIndex >= indexArray.size()) {
            return 0;
        }

        return indexArray.get(tableIndex);
    }

    /**
     * Sort the data in the table by the specified table column.
     * 
     * @param tc
     *            Table column to sort.
     */
    private void sortTableData(TableColumn tc) {
        if (tableData == null) {
            return;
        }

        String sortCol = (String) tc.getData();

        int sortDir = getColumnAttributeData(sortCol).getSortDir();
        int columnIndex = getColumnIndex(sortCol);

        tableData.sortData(columnIndex, sortDir);

        FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig.getInstance()
                .getTableConfigData(this.siteKey);
        String[] colNames = ffmpTableCfgData.getTableColumnKeys();
        String sortedColName = colNames[columnIndex];
        String guidRankSource = null;
        if (sortedColName.contains("_")) {
            String[] parts = sortedColName.split("_");
            sortedColName = parts[1];
            guidRankSource = parts[0];
        }
        ffmpConfig.getFFMPConfigData().setColumnSorted(
                sortedColName + "," + guidRankSource);

        setColumnImages();

        createTableItems();

        showHideTableColumns();
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
        gc.setFont(columnFont);

        int maxTextLength = -1;

        textWidth = gc.getFontMetrics().getAverageCharWidth();
        textHeight = gc.getFontMetrics().getHeight();

        FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig.getInstance()
                .getTableConfigData(siteKey);
        String[] colNames = ffmpTableCfgData.getTableColumnKeys();

        for (String str : colNames) {
            String colName = ffmpTableCfgData.getTableColumnAttr(str)
                    .getOriginalName();

            String[] nameArray = colName.split(" ");

            for (String tmpStr : nameArray) {
                maxTextLength = Math.max(maxTextLength, tmpStr.length());
            }
        }

        imageWidth = maxTextLength * textWidth + EXTRA_COLUMN_WIDTH;
        imageHeight = textHeight * 2;

        gc.dispose();
        image.dispose();
    }

    /**
     * For each table column create an image and then set the table column to
     * that image.
     */
    private void setColumnImages() {

        FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig.getInstance()
                .getTableConfigData(siteKey);
        String[] colNameKeys = ffmpTableCfgData.getTableColumnKeys();

        TableColumn tc;

        disposeColumnImages();

        // Loop over the column name keys
        for (int i = 0; i < colNameKeys.length; i++) {
            String colName = ffmpTableCfgData
                    .getTableColumnAttr(colNameKeys[i]).getSplitColumnName();
            tc = table.getColumn(i);

            Image img = new Image(this.getDisplay(), imageWidth, imageHeight);

            GC gc = new GC(img);
            gc.setFont(columnFont);

            // Set the initial foreground and background colors.
            gc.setForeground(this.getDisplay().getSystemColor(SWT.COLOR_WHITE));
            gc.setBackground(this.getDisplay().getSystemColor(SWT.COLOR_BLACK));

            // Set the background color to the sort color if that column is
            // sorted.
            if (tc == sortedTableColumn) {
                gc.setBackground(sortColor);
            }

            gc.fillRectangle(0, 0, imageWidth, imageHeight);

            /*
             * If there is a \n in the string then find the largest of the 2
             * strings and use that to determine the x coord. For the y coord
             * set y based on
             */
            int xCoord = 0;
            int yCoord = 0;
            if (colName.indexOf("\n") > 0) {
                String[] tmpArray = colName.split("\n");

                for (int j = 0; j < tmpArray.length; j++) {

                    /*
                     * Fixes for DR14406
                     */
                    xCoord = Math.round((imageWidth / 2)
                            - (tmpArray[j].length() * textWidth / 2));
                    yCoord = j * (textHeight + 1);
                    gc.drawText(tmpArray[j], xCoord, yCoord, true);
                }
            } else {
                xCoord = Math.round((imageWidth / 2)
                        - (colName.length() * textWidth / 2));
                yCoord = imageHeight / 2 - textHeight / 2 - 1;
                gc.drawText(colName, xCoord, yCoord, true);
            }

            gc.dispose();
            tc.setImage(img);

            columnImgs.add(img);
        }
    }

    /**
     * Dispose of all the table column images and clear the array of images.
     */
    protected void disposeColumnImages() {
        for (Image img : columnImgs) {
            if (img != null) {
                img.dispose();
            }
        }

        columnImgs.clear();
    }

    /**
     * Get the list of visible table columns.
     * 
     * @return Boolean array indicating which columns are visible or not.
     */
    public AttributesDlgData getVisibleColumns() {
        return ffmpConfig.getVisibleColumns(siteKey);
    }

    /**
     * Show/Hide table columns.
     * 
     * @param visCols
     *            Boolean array indicating which table columns are visible.
     */
    public void showHideTableColumns(AttributesDlgData attrData) {
        TableColumn[] tCols = table.getColumns();
        String[] columnKeys = getColumnKeys();
        String source = null;
        for (int i = 0; i < columnKeys.length; i++) {
            String col = columnKeys[i];
            if (col.contains(COLUMN_NAME.GUID.getColumnName())
                    || col.contains(COLUMN_NAME.RATIO.getColumnName())
                    || col.contains(COLUMN_NAME.DIFF.getColumnName())) {
                if (col.contains("_")) {
                    String[] parts = col.split("_");
                    source = parts[0];
                    col = parts[1];
                }
                if (attrData.isGuidColumnIncluded(source)) {
                    if (attrData.isColumnVisible(col)) {
                        tCols[i].setWidth(defaultColWidth);
                    } else {
                        tCols[i].setWidth(0);
                    }
                } else {
                    tCols[i].setWidth(0);
                }
            } else {
                if (attrData.isColumnVisible(col)) {
                    if (i == 0) {
                        tCols[i].setWidth(table.getColumn(i).getWidth());
                    } else {
                        tCols[i].setWidth(defaultColWidth);
                    }

                    setQPFColName(tCols[i], col);
                } else {
                    tCols[i].setWidth(0);
                }
            }
        }
    }

    /**
     * Show and hide the columns based on the configuration data.
     */
    public void showHideTableColumns() {
        AttributesDlgData attrData = ffmpConfig.getVisibleColumns(siteKey);
        showHideTableColumns(attrData);
    }

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
    protected abstract void tableMouseMoveAction(MouseEvent event);

    /**
     * Add controls above the table (if needed).
     * 
     * @param parentComp
     *            Parent composite.
     */
    protected abstract void addTopTableControls(Composite parentComp);

    /**
     * Get the default column width.
     * 
     * @return The column width.
     */
    protected abstract int getDefaultColWidth();

    /**
     * Get the column keys.
     * 
     * @return String array of column keys.
     */
    protected abstract String[] getColumnKeys();

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
     * @param sortCol
     *            Sort column.
     * @return Column index.
     */
    protected abstract int getColumnIndex(String sortCol);

    /**
     * DR14406 code: QPF column's name should be re-set when a user choose
     * another type of QPF from the Attributes... button.
     * 
     * See FfmpTableConfigData.setQpfType() with ColumnAttribData
     * 
     * @param tCols
     *            : TableColumn
     * @param col
     *            : Column name
     */
    private void setQPFColName(TableColumn tCols, String col) {

        if (COLUMN_NAME.QPF.getColumnName().equalsIgnoreCase(col)) {

            setColumnImages();
            tCols.setWidth(defaultColWidth + EXTRA_COLUMN_WIDTH);// 38);

        }
    }
}