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

import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;

/**
 * 
 * TVS table used to display the data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANTvsTableComp extends SCANTable {
    private Point mouseMovePt = new Point(0, 0);

    private Point mouseDownPt = new Point(0, 0);

    private Point prevMousePt = new Point(-9999, -9999);

    public SCANTvsTableComp(Composite parent, SCANTableData tableData,
            ITableAction tableActionCB, String site) {
        super(parent, tableData, tableActionCB, site);

        init();
    }

    // @Override
    // protected void setColumnImages() {
    // TableColumn[] tCols = table.getColumns();
    //
    // for (int i = 0; i < tCols.length; i++) {
    // String colName = (String) tCols[i].getData();
    // Image img = new Image(this.getDisplay(), imageWidth, imageHeight);
    //
    // GC gc = new GC(img);
    // gc.setFont(columnFont);
    //
    // // Set the initial foreground and background colors.
    // gc.setForeground(this.getDisplay().getSystemColor(SWT.COLOR_WHITE));
    // gc.setBackground(this.getDisplay().getSystemColor(SWT.COLOR_BLACK));
    //
    // // Set the background color to the sort color if that column is
    // // sorted.
    // if (sortedColumnIndex == -1) {
    // scanCfg.getDefaultName();
    // String sortColName = scanCfg.getDefaultRank(this.scanTable);
    // int colIndex = scanCfg.getColumnIndex(scanTable, sortColName);
    // sortedColumnIndex = colIndex;
    // }
    // if (table.indexOf(tCols[i]) == sortedColumnIndex) {
    // gc.setBackground(scanCfg.getScanColor(ScanColors.Sort));
    // }
    //
    // gc.fillRectangle(0, 0, imageWidth, imageHeight);
    //
    // int xCoord = (imageWidth / 2) - (colName.length() * textWidth / 2);
    //
    // gc.drawText(colName, xCoord, 3, true);
    //
    // gc.dispose();
    // tCols[i].setImage(img);
    //
    // img.dispose();
    // }
    // }

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
        // center on the TVS location
        if (event.button == 1) {
            if (colIndex == scanCfg.getColumnIndex(ScanTables.TVS, "ident")) {
                tableIndex = rowIndex;
                tableActionCB.centerByIdent(table.getItem(tableIndex).getText(
                        colIndex));
            } else if (colIndex == scanCfg.getColumnIndex(ScanTables.TVS,
                    "strmID")) {
                tableIndex = rowIndex;
                tableActionCB.centerByStormId(table.getItem(tableIndex)
                        .getText(colIndex));
            }
        }
    }

    // @Override
    // protected void tableMouseMoveAction(MouseEvent event) {
    // /*
    // * TODO: Looking at the WES the TVS table is empty. Need to look at the
    // * legacy code to determine if there are tool tip texts for the cells.
    // */
    // }
}
