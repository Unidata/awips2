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
import java.util.Collections;
import java.util.HashMap;

import org.eclipse.swt.SWT;

import com.raytheon.uf.viz.monitor.ui.dialogs.ISortColumn;

/**
 * Data class that contains an array of TableRowData objects. Each TableRowData
 * object is one row of data in the table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2009            lvenable     Initial creation
 * May 7, 2013   1986   njensen   Optimized sortData()
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FFMPTableData implements ISortColumn {
    /**
     * Array of TableRowData.
     */
    private ArrayList<FFMPTableRowData> tableRows;

    /**
     * Current column index to sort on.
     */
    private int currentSortColumnIndex = 0;

    /**
     * Current sort direction.
     */
    private int currentSortDirection = SWT.DOWN;

    private HashMap<String, Object> pfafMap;

    /**
     * Constructor.
     */
    public FFMPTableData() {
        tableRows = new ArrayList<FFMPTableRowData>();
        pfafMap = new HashMap<String, Object>();
    }

    /**
     * Add or replace a row of cell data to the table rows collection. A row is
     * replaced if the area Id already exists.
     * 
     * @param data
     *            Table row data.
     */
    public void addDataRow(FFMPTableRowData data) {
        pfafMap.put(data.getPfaf().toString(), null);
        data.setSortCallback(this);
        tableRows.add(data);
    }

    public boolean containsPfaf(String pfaf) {
        return pfafMap.containsKey(pfaf);
    }

    /**
     * Replace the data in a table cell.
     * 
     * @param columnIndex
     *            Column index.
     * @param areaID
     *            Area ID (acts like a primary key).
     * @param data
     *            Table cell data.
     */
    public void replaceTableCellData(int columnIndex, String areaID,
            FFMPTableCellData data) {
        for (FFMPTableRowData tableRowData : tableRows) {
            if (areaID.equals(tableRowData.getTableCellData(0).getCellText()) == true) {
                tableRowData.setTableCellData(columnIndex, data);
            }
        }
    }

    /**
     * Get the table cell data.
     * 
     * @param columnIndex
     *            Column index.
     * @param areaID
     *            Area ID (acts like a primary key).
     * @return Table cell data.
     */
    public FFMPTableCellData getTableCellData(int columnIndex, String areaID) {
        for (FFMPTableRowData tableRowData : tableRows) {
            if (areaID.equals(tableRowData.getTableCellData(0).getCellText()) == true) {
                return tableRowData.getTableCellData(0);
            }
        }

        return null;
    }

    /**
     * Get table row data.
     * 
     * @param id
     *            Area ID.
     * @return Table row data.
     */
    public FFMPTableRowData getTableRowData(String id) {
        for (FFMPTableRowData trd : tableRows) {
            if (id.equals(trd.getTableCellData(0).getCellText()) == true) {
                return trd;
            }
        }

        return null;
    }

    /**
     * Get array of table row data.
     * 
     * @return Array of table row data.
     */
    public ArrayList<FFMPTableRowData> getTableRows() {
        return tableRows;
    }

    /**
     * Get the sort column index.
     * 
     * @return The index of the column to be sorted.
     */
    @Override
    public int getSortColumn() {
        return currentSortColumnIndex;
    }

    /**
     * Get the sort direction.
     * 
     * @return The sort direction.
     */
    @Override
    public int getSortDirection() {
        return currentSortDirection;
    }

    /**
     * Sort the table data.
     * 
     * @param columnIndex
     *            the column to sort on
     * @param direction
     *            the direction to sort by
     */
    public void sortData(int columnIndex, int direction) {
        if (columnIndex != currentSortColumnIndex
                || direction != currentSortDirection) {
            currentSortColumnIndex = columnIndex;
            currentSortDirection = direction;
            Collections.sort(tableRows);
            if (getSortDirection() == SWT.DOWN) {
                Collections.reverse(tableRows);
            }
        }
    }
}
