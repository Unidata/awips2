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

import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.ColumnAttribData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.ThreshColNames;

public class FFMPTableComp extends FFMPTable {
    private FfmpTableConfig tableConfig;

    /**
     * Table selection callback.
     */
    private ITableSelection tableSelectionCB;

    /**
     * Mouse point.
     */
    private Point mousePt = new Point(0, 0);

    private boolean vgb;

    private String siteKey;

    public FFMPTableComp(Composite parent, FFMPTableData data, ITableSelection tableSelectionCB, String siteKey) {
        super(parent, data, siteKey);

        this.siteKey = siteKey;

        this.tableSelectionCB = tableSelectionCB;
        tableConfig = FfmpTableConfig.getInstance();

        columnMinimumSize = true;

        init();
    }

    @Override
    protected void addTopTableControls(Composite parentComp) {
        // No controls needed
    }

    @Override
    protected ColumnAttribData getColumnAttributeData(String colName) {
        return tableConfig.getTableConfigData(siteKey).getTableColumnAttr(colName);
    }

    @Override
    protected int getColumnIndex(String sortCol) {
        return tableConfig.getTableConfigData(siteKey).getTableColumnIndex(sortCol);
    }

    @Override
    protected String[] getColumnKeys() {
        return tableConfig.getTableConfigData(siteKey).getTableColumnKeys();
    }

    @Override
    protected int getDefaultColWidth() {
        return DEFAULT_COLUMN_WIDTH;
    }

    @Override
    protected void tableMouseDownAction(MouseEvent event) {
        tableIndex = table.getSelectionIndex();
        mousePt.x = event.x;
        mousePt.y = event.y;
        TableItem item = table.getItem(mousePt);

        if ((item == null) || item.isDisposed()) {
            return;
        }

        int tmpIndex = table.indexOf(item);
        int index = tableIndexToDataIndex(tmpIndex);
        vgb = tableData.getTableRows().get(index).getTableCellData(0).isVGB();

        Rectangle rect = item.getBounds(0);
        if (rect.contains(mousePt)) {
            currentPfaf = tableData.getTableRows().get(index).getPfaf();

            if (event.button == 1) {
                String name = item.getText(0);
                tableSelectionCB.tableSelection(currentPfaf, name);
            } else if (event.button == 3) {
                tableSelectionCB.displayBasinTrend(currentPfaf);
            }
        }
        table.deselectAll();
        table.redraw();
    }

    @Override
    protected void tableMouseMoveAction(MouseEvent event) {
        mousePt.x = event.x;
        mousePt.y = event.y;

        TableItem item = table.getItem(mousePt);

        if (item == null) {
            table.setToolTipText(null);
            return;
        }

        int tmpIndex = table.indexOf(item);
        int index = tableIndexToDataIndex(tmpIndex);

        Rectangle rect;

        for (int i = 0; i < table.getColumnCount(); i++) {
            rect = item.getBounds(i);
            if (rect.contains(mousePt)) {
                table.setToolTipText(tableData.getTableRows().get(index)
                        .getTableCellDataArray()[i].getHoverText());
            }
        }
    }

    public void updateThresholds(ThreshColNames threshColumn) {

        // TODO : loop over index array

        ArrayList<FFMPTableRowData> rowData = tableData.getTableRows();
        for (int i = 0; i < rowData.size(); i++) {
            rowData.get(i).getTableCellData(threshColumn.getColIndex())
                    .generateCellColor();
        }

        sortTableUsingConfig();
    }

    public boolean isVGB() {
        return vgb;
    }

    public String getCurrentPfaf() {
        return currentPfaf;
    }
}
