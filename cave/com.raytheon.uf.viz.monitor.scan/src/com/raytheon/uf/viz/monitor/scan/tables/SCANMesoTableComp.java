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
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;

/**
 * 
 * Table for the MESO table dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2009  #3039      lvenable     Initial creation
 * Apr 29, 2013 #1945      lvenable    Code cleanup for SCAN performance.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANMesoTableComp extends SCANTable {

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param tableData
     *            Data to be displayed in the table.
     * @param tableActionCB
     *            Callback called when action are taken on the table.
     */
    public SCANMesoTableComp(Composite parent, SCANTableData tableData,
            ITableAction tableActionCB, String site) {
        super(parent, tableData, tableActionCB, site);

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
        // center on the Meso location
        if (event.button == 1) {
            if (colIndex == scanCfg.getColumnIndex(ScanTables.MESO, "strmID")) {
                tableIndex = rowIndex;
                tableActionCB.centerByStormId(table.getItem(
                        table.getSelectionIndex()).getText());
            } else if (colIndex == scanCfg.getColumnIndex(ScanTables.MESO,
                    "ident")) {
                tableIndex = rowIndex;
                String ident = tableData.getTableRows().get(rowIndex)
                        .getTableCellData(1).getCellText();
                tableActionCB.centerByIdent(ident);
            }
        }
    }
}
