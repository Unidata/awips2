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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.eclipse.swt.SWT;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.ui.dialogs.ISortColumn;

/**
 * 
 * The class hold all of the data that will be displayed in a table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039     lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANTableData implements ISortColumn {
    /**
     * Array of table row data.
     */
    private ArrayList<SCANTableRowData> tableRows;

    /**
     * SCAN table identifier.
     */
    private ScanTables scanTable = ScanTables.CELL;

    /**
     * Map of unique identifiers.
     */
    private HashMap<String, Object> identMap;

    /**
     * Current sort column index.
     */
    private int currentSortColumnIndex = 0;

    /**
     * Current sort direction.
     */
    private int currentSortDirection = SWT.DOWN;

    private List<String> featureIds = null;

    /**
     * Constructor.
     * 
     * @param scanTable
     *            SCAN table identifier.
     */
    public SCANTableData(ScanTables scanTable) {
        this.scanTable = scanTable;
        tableRows = new ArrayList<SCANTableRowData>();
        identMap = new HashMap<String, Object>();
        featureIds = new ArrayList<String>();
    }

    /**
     * Set the sort column and direction.
     * 
     * @param columnIndex
     *            Column index.
     * @param direction
     *            Sort direction.
     */
    public void setSortColumnAndDirection(int columnIndex, int direction) {
        currentSortColumnIndex = columnIndex;
        currentSortDirection = direction;
    }

    /**
     * Sort the table data.
     */
    public void sortData() {
        Collections.sort(tableRows);
    }

    public void sortDefault() {
        ArrayList<SCANTableRowData> defSortedList = new ArrayList<SCANTableRowData>();
        defSortedList.ensureCapacity(tableRows.size());

        for (String fId : featureIds) {
            for (SCANTableRowData row : tableRows) {
                if (row.getIdent().equals(fId)) {
                    defSortedList.add(row);
                    break;
                }
            }
        }

        tableRows.clear();
        for (SCANTableRowData row : defSortedList) {
            tableRows.add(row);
        }
    }

    /**
     * Add a data row.
     * 
     * @param data
     *            SCAN table data row.
     */
    public void addDataRow(SCANTableRowData data) {
        identMap.put(data.getIdent(), null);
        data.setSortCallback(this);
        tableRows.add(data);
    }

    /**
     * Get the table name.
     * 
     * @return SCAN table identifier.
     */
    public ScanTables getTableName() {
        return scanTable;
    }

    /**
     * Get the sort column index.
     */
    @Override
    public int getSortColumn() {
        return currentSortColumnIndex;
    }

    /**
     * Get the sort direction.
     */
    @Override
    public int getSortDirection() {
        return currentSortDirection;
    }

    /**
     * Get the array of table rows.
     * 
     * @return Array of table rows.
     */
    public ArrayList<SCANTableRowData> getTableRows() {
        return tableRows;
    }

    public List<String> getFeatureIds() {
        return featureIds;
    }

    public void setFeatureIds(List<String> featureIds) {
        this.featureIds = featureIds;
    }

    public int getNumberOfDataRows() {
        return tableRows.size();
    }
}
