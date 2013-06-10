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
package com.raytheon.uf.viz.monitor.scan.tables;

import java.util.ArrayList;
import java.util.Date;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.DMDTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanColors;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;
import com.raytheon.uf.viz.monitor.scan.commondialogs.DrawSettings;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTimeHeightData;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTrendGraphData;
import com.raytheon.uf.viz.monitor.scan.commondialogs.TimeHeightDlg;

/**
 * 
 * DMD table used to display the data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039      lvenable     Initial creation
 * Apr 26, 2013 #1945      lvenable    Improved SCAN performance, reworked
 *                                     some bad code, and some code cleanup.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANDmdTableComp extends SCANTableTrendGraphLayer implements
        IRequestTimeHeightData {

    private TimeHeightDlg timeHeightDlg = null;

    private IRequestTimeHeightData timeHeightCB;

    /** Clutter control column name. */
    private String clutterColName = "";

    public SCANDmdTableComp(Composite parent, SCANTableData tableData,
            ITableAction tableActionCB,
            IRequestTrendGraphData requestDataCallback,
            IRequestTimeHeightData timeHeightCB, String site) {
        super(parent, tableData, tableActionCB, requestDataCallback, site);
        this.timeHeightCB = timeHeightCB;

        init();
    }

    @Override
    protected void setColumnImages() {
        /*
         * If the clutter control & sort column hasn't changed then return
         * because the images will not change.
         */
        if (scanCfg.isClutterControl(scanTable, clutterColName)
                && lastSortColIndex == sortedColumnIndex) {
            return;
        }

        TableColumn[] tCols = table.getColumns();

        for (int i = 0; i < tCols.length; i++) {
            String colName = (String) tCols[i].getData();
            Image img = new Image(this.getDisplay(), imageWidth, imageHeight);

            GC gc = new GC(img);
            gc.setFont(columnFont);
            gc.setAntialias(SWT.ON);

            // Set the initial foreground and background colors.
            gc.setForeground(this.getDisplay().getSystemColor(SWT.COLOR_WHITE));
            gc.setBackground(this.getDisplay().getSystemColor(SWT.COLOR_BLACK));

            // Set the foreground color to the clutter control color if the
            // column is a clutter control.
            if (scanCfg.isClutterControl(scanTable, colName) == true) {
                clutterColName = colName;
                gc.setForeground(scanCfg
                        .getScanColor(ScanColors.ClutterControl));
            }

            // Set the background color to the sort color if that column is
            // sorted.
            if (sortedColumnIndex == -1) {
                scanCfg.getDefaultName();
                String sortColName = scanCfg.getDefaultRank(this.scanTable);
                int colIndex = scanCfg.getColumnIndex(scanTable, sortColName);
                sortedColumnIndex = colIndex;
            }

            if (table.indexOf(tCols[i]) == sortedColumnIndex) {
                gc.setBackground(scanCfg.getScanColor(ScanColors.Sort));
            }

            lastSortColIndex = sortedColumnIndex;

            gc.fillRectangle(0, 0, imageWidth, imageHeight);

            int colNameExt = gc.stringExtent(colName).x;

            int xCoord = (imageWidth / 2) - (colNameExt / 2);

            gc.drawText(colName, xCoord, 3, true);

            gc.dispose();
            tCols[i].setImage(img);

            img.dispose();
        }
    }

    @Override
    protected void tableMouseDownAction(MouseEvent event) {
        mouseDownPt.x = event.x;
        mouseDownPt.y = event.y;

        TableItem item = table.getItem(mouseDownPt);

        if (item == null) {
            return;
        }

        int rowIndex = table.indexOf(item);
        int colIndex = -1;

        Rectangle rect;

        for (int i = 0; i < table.getColumnCount(); i++) {
            rect = item.getBounds(i);

            if (rect.contains(mouseDownPt)) {
                colIndex = i;
                break;
            }
        }

        if (colIndex < 0) {
            return;
        }

        if (event.button == 1) {
            if ((colIndex == scanCfg.getColumnIndex(ScanTables.DMD, "ident"))
                    || (colIndex == scanCfg.getColumnIndex(ScanTables.DMD,
                            "strmID"))) {
                tableIndex = rowIndex;
                int index = scanCfg.getColumnIndex(ScanTables.DMD, "ident");
                tableActionCB.centerByIdent(table.getItem(tableIndex).getText(
                        index));
            } else {
                String name = (String) table.getColumn(colIndex).getData();

                if (scanCfg.canViewTrend(scanTable, name) == true) {
                    String ident = tableData.getTableRows().get(rowIndex)
                            .getTableCellData(0).getCellText();
                    displayTrendGraphDialog(ident, name);
                }
            }
        } else if (event.button == 3) {
            if (colIndex == 0) {
                String ident = tableData.getTableRows().get(rowIndex)
                        .getTableCellData(0).getCellText();
                displayTrendSetsGraphDialog(ident);
            } else {
                String name = (String) table.getColumn(colIndex).getData();
                if (scanCfg.canViewTimeHeight(scanTable, name)) {
                    String ident = tableData.getTableRows().get(rowIndex)
                            .getTableCellData(0).getCellText();
                    displayTimeHeightDialog(ident, name);
                }
            }
        }
    }

    @Override
    protected void tableMouseMoveAction(MouseEvent event) {

        mouseMovePt.x = event.x;
        mouseMovePt.y = event.y;

        TableItem item = table.getItem(mouseMovePt);

        if (item == null) {
            table.setToolTipText(null);
            return;
        }

        Rectangle rect;
        rect = item.getBounds(scanCfg.getCountyColumnIndex(scanTable));

        if ((scanCfg.showTips(scanTable) == false)
                && (rect.contains(mouseMovePt) == false)) {
            prevMousePt.x = -9999;
            prevMousePt.y = -9999;
            table.setToolTipText(null);
            return;
        }

        for (int i = 0; i < table.getColumnCount(); i++) {
            rect = item.getBounds(i);

            if (rect.contains(mouseMovePt) && rect.contains(prevMousePt)) {
                return;
            }

            if (rect.contains(mouseMovePt)) {
                prevMousePt.x = mouseMovePt.x;
                prevMousePt.y = mouseMovePt.y;

                if (((String) table.getColumn(i).getData())
                        .equals(DMDTable.COUNTY.getColName())) {
                    table.setToolTipText(tableData.getTableRows()
                            .get(table.indexOf(item)).getTableCellData(i)
                            .getCellText());
                    return;
                }

                table.setToolTipText(toolTipMgr.getTableCellTip(scanTable,
                        (String) table.getColumn(i).getData()));
                return;
            }
        }

        prevMousePt.x = -9999;
        prevMousePt.y = -9999;
        table.setToolTipText(null);
    }

    private void displayTimeHeightDialog(String ident, String attrName) {
        TreeMap<Long, DMDTableDataRow> graphData;

        SCANConfigEnums.DMDTable tableCol = SCANConfigEnums.DMDTable
                .valueOf(attrName.toUpperCase());
        graphData = timeHeightCB.requestTimeHeightData(tableCol, ident);

        if ((timeHeightDlg == null) || timeHeightDlg.isDisposed()) {
            timeHeightDlg = new TimeHeightDlg(getParent().getShell(),
                    scanTable, ident, attrName, getIdentList(), graphData, this);
            timeHeightDlg.open();
        } else {
            timeHeightDlg.setGraphData(graphData, ident, attrName);
        }
    }

    /**
     * Force the Time Height Graph to redraw.
     */
    public void redrawTimeHeightGraph() {
        if ((timeHeightDlg == null) || timeHeightDlg.isDisposed()) {
            return;
        }

        timeHeightDlg.redrawGraph();
    }

    public boolean timeHeightDisplayed() {
        if ((timeHeightDlg != null) && (timeHeightDlg.shellDisposed() == false)) {
            return true;
        }

        return false;
    }

    public void updateTimeHeightGraph(ScanMonitor scanMonitor, String site,
            Date time) {
        TreeMap<Long, DMDTableDataRow> data = scanMonitor
                .getTimeHeightGraphData(site, timeHeightDlg.getTableColumn(),
                        timeHeightDlg.getIdent(), time);

        ArrayList<SCANTableRowData> rows = this.tableData.getTableRows();

        ArrayList<String> identList = new ArrayList<String>();
        for (SCANTableRowData row : rows) {
            identList.add(row.getIdent());
        }

        timeHeightDlg.setIdentArray(identList.toArray(new String[identList
                .size()]));

        for (SCANTableRowData row : rows) {
            if (timeHeightDlg.getIdent().equals(row.getIdent())) {
                timeHeightDlg.setGraphData(data);
                return;
            }
        }

        timeHeightDlg.displayMessage();
    }

    public void closeOpenTimeHeightGraphs() {
        if ((timeHeightDlg != null) && (timeHeightDlg.shellDisposed() == false)) {
            timeHeightDlg.close();
        }

        timeHeightDlg = null;
    }

    /**
     * Set the tableIndex for the alarm selection row.
     */
    public void alarmSelection(String ident) {
        int count = 0;
        ArrayList<SCANTableRowData> rowList = tableData.getTableRows();
        for (SCANTableRowData scanTableRowData : rowList) {
            if (scanTableRowData.getIdent().equals(ident)) {

                tableIndex = count;
                break;
            }
            count++;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTimeHeightData
     * #requestTimeHeightData
     * (com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.DMDTable,
     * java.lang.String)
     */
    @Override
    public TreeMap<Long, DMDTableDataRow> requestTimeHeightData(
            DMDTable tableCol, String dmdIdent) {
        TreeMap<Long, DMDTableDataRow> graphData;
        graphData = timeHeightCB.requestTimeHeightData(tableCol, dmdIdent);

        return graphData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTimeHeightData
     * #getDialogTime()
     */
    @Override
    public Date getDialogTime() {
        return timeHeightCB.getDialogTime();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTimeHeightData
     * #getDrawSettings()
     */
    @Override
    public DrawSettings getDrawSettings() {
        return timeHeightCB.getDrawSettings();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTimeHeightData
     * #setDrawSettings
     * (com.raytheon.uf.viz.monitor.scan.commondialogs.DrawSettings)
     */
    @Override
    public void setDrawSettings(DrawSettings drawSettings) {
        timeHeightCB.setDrawSettings(drawSettings);
    }
}
