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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.CELLTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanColors;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.WARN_TYPE;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTrendGraphData;

/**
 * 
 * CELL table used to display the data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039      lvenable     Initial creation
 * Apr 29, 2013 #1945      lvenable    Code cleanup for SCAN performance.
 * Jun 04, 2013 #1984      lvenable    Save images instead of disposing them when setting
 *                                     the table column images.  This is to fix the Windows
 *                                     issue on the images being blank and throwing errors.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANCellTableComp extends SCANTableTrendGraphLayer {
    /** Unwarned TVS tooltip text */
    private static final String UNWARNED_TOR_TOOLTIP = "\nA storm meeting Tornado "
            + "warning criteria\nis outside any TOR warning polygon.";

    /** Unwarned SVR tooltip text */
    private static final String UNWARNED_SVR_TOOLTIP = "\nA storm meeting Severe "
            + "Weather warning\ncriteria is outside any SVR warning polygon.";

    /** Radius interpolation column name. */
    private String radVarColName = "";

    /** Clutter control column name. */
    private String clutterCoName = "";

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param tableData
     *            Table data.
     * @param tableActionCB
     *            Table action callback
     * @param requestDataCallback
     *            Request data callback
     * @param site
     *            The site.
     */
    public SCANCellTableComp(Composite parent, SCANTableData tableData,
            ITableAction tableActionCB,
            IRequestTrendGraphData requestDataCallback, String site) {
        super(parent, tableData, tableActionCB, requestDataCallback, site);

        init();
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
            if (colIndex == 0) {
                tableIndex = rowIndex;
                if ((tableIndex >= 0) || (tableIndex > table.getItemCount())) {
                    tableActionCB.centerByStormId(table.getItem(tableIndex)
                            .getText());
                    redrawTable();
                }
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

        if (scanCfg.showTips(scanTable) == false
                && rect.contains(mouseMovePt) == false) {
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

                String toolTip = toolTipMgr.getTableCellTip(scanTable,
                        (String) table.getColumn(i).getData());

                if (i == 0) {
                    WARN_TYPE wt = tableData.getTableRows()
                            .get(table.indexOf(item)).getTableCellData(0)
                            .getWarnType();
                    if (wt == WARN_TYPE.SEVERE) {
                        toolTip = toolTip.concat(UNWARNED_SVR_TOOLTIP);
                    } else if (wt == WARN_TYPE.TVS) {
                        toolTip = toolTip.concat(UNWARNED_TOR_TOOLTIP);
                    }
                } else if (((String) table.getColumn(i).getData())
                        .equals(CELLTable.COUNTY.getColName())) {
                    toolTip = tableData.getTableRows().get(table.indexOf(item))
                            .getTableCellData(i).getCellText();
                }

                table.setToolTipText(toolTip);
                return;
            }
        }

        prevMousePt.x = -9999;
        prevMousePt.y = -9999;
        table.setToolTipText(null);
    }

    /**
     * This method creates the images that will be displayed in the tables
     * columns. The CELL table will display sort images
     */
    @Override
    protected void setColumnImages() {

        /*
         * If the Radius Interpolation, Clutter Control, or sort column hasn't
         * changed then return because the images will not change.
         */
        if (scanCfg.isRadVar(scanTable, radVarColName)
                && scanCfg.isClutterControl(scanTable, clutterCoName)
                && lastSortColIndex == sortedColumnIndex) {
            return;
        }

        TableColumn[] tCols = table.getColumns();

        disposeColumnImages();

        for (int i = 0; i < tCols.length; i++) {
            String colName = (String) tCols[i].getData();
            Image img = new Image(this.getDisplay(), imageWidth, imageHeight);

            GC gc = new GC(img);
            gc.setFont(columnFont);
            gc.setAntialias(SWT.ON);

            // Set the initial foreground and background colors.
            gc.setForeground(this.getDisplay().getSystemColor(SWT.COLOR_WHITE));
            gc.setBackground(this.getDisplay().getSystemColor(SWT.COLOR_BLACK));

            /*
             * Set the background color to the RadVar color if the column is a
             * radius interpolation.
             * 
             * NOTE: In the legacy system, if a column is sorted the sorted
             * color overrides the Rad Var color. That is why this is
             * checked/set first.
             */

            if (scanCfg.isRadVar(scanTable, colName) == true) {
                radVarColName = colName;
                gc.setBackground(scanCfg.getScanColor(ScanColors.RadVar));
            }

            // Set the foreground color to the clutter control color if the
            // column is a clutter control.
            if (scanCfg.isClutterControl(scanTable, colName) == true) {
                clutterCoName = colName;
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

            columnImgs.add(img);
        }
    }
}
