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
package com.raytheon.uf.viz.datadelivery.common.ui;

/**
 * This is a generic table data manager that will be used to manage data that will be displayed in a table.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 6, 2012            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0	
 */

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;

public class TableDataManager<T extends ITableData<T>> implements ISortTable {

    /**
     * Array of data.
     */
    private final ArrayList<T> tableData;

    /**
     * Column name.
     */
    private String currentSortColumnName = "";

    /**
     * Current sort direction.
     */
    private SortDirection currentSortDirection = SortDirection.ASCENDING;

    /**
     * Constructor.
     * 
     * @param tableType
     *            Type of table the data will be use for.
     */
    public TableDataManager(TABLE_TYPE tableType) {
        tableData = new ArrayList<T>();

        String[] columns = DataDeliveryUtils.getColumnTitles(tableType);

        if (columns.length > 0) {
            currentSortColumnName = columns[0];
        }
    }

    /**
     * Add a "row" of data that will be displayed in the table.
     * 
     * @param data
     *            Data to be added to the data array.
     */
    public void addDataRow(T data) {
        data.setSortCallback(this);
        tableData.add(data);
    }

    /**
     * Get the data array.
     * 
     * @return The data array.
     */
    public ArrayList<T> getDataArray() {
        return tableData;
    }

    /**
     * Sort the data array.
     */
    public void sortData() {
        Collections.sort(tableData);
    }

    /**
     * Clear the data array.
     */
    public void clearAll() {
        tableData.clear();
    }

    /**
     * Remove all of the items in the remove list from the data array.
     * 
     * @param removeList
     *            The list of data to remove.
     */
    public void removeAll(List<T> removeList) {
        tableData.removeAll(removeList);
    }

    /**
     * Get the row of data at the specified index.
     * 
     * @param index
     *            Index.
     * @return The data at the the specified index.
     */
    public T getDataRow(int index) {
        if (index >= 0 && index < tableData.size()) {
            return tableData.get(index);
        }
        else {
            return tableData.get(0);
        }
    }

    /**
     * Remove data at the specified index.
     * 
     * @param index
     *            Index of the data to be removed.
     */
    public void removeDataRow(int index) {
        if (index >= 0 && index < tableData.size()) {
            tableData.remove(index);
        }
    }

    /**
     * Set the sort column.
     * 
     * @param columnName
     *            The column name to sort on.
     */
    @Override
    public void setSortColumn(String columnName) {
        currentSortColumnName = columnName;
    }

    /**
     * Set the sort direction.
     * 
     * @param direction
     *            Sort direction (ascending or descending).
     */
    @Override
    public void setSortDirection(SortDirection direction) {
        currentSortDirection = direction;
    }

    /**
     * Get the sort column text.
     */
    @Override
    public String getSortColumnText() {
        return currentSortColumnName;
    }

    /**
     * Get the sort direction.
     */
    @Override
    public SortDirection getSortDirection() {
        return currentSortDirection;
    }
}
