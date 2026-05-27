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
package com.raytheon.viz.mpe.ui.dialogs.gagetable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.viz.mpe.ui.dialogs.gagetable.xml.GageTableColumnData;
import com.raytheon.viz.mpe.ui.dialogs.gagetable.xml.GageTableSortType;

/**
 * Sort Settings for the Gage Table.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2009 2476       mpduff      Initial creation
 * Mar 01, 2017 6158       mpduff      Changed how sorting works.
 * Jun 22, 2017 6158       mpduff      Added setColumnData method.
 * Jul 14, 2017 6358       mpduff      Changed sort direction.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class GageTableSortSettings {
    private int columnWidth = 100;

    private List<String> sortColumns = new ArrayList<>(4);

    private Map<String, Boolean> sortDirections = new HashMap<>();

    public GageTableSortSettings() {
        // Default constructor
    }

    /**
     * @return the columnWidth
     */
    public int getColumnWidth() {
        return columnWidth;
    }

    /**
     * @param columnWidth
     *            the columnWidth to set
     */
    public void setColumnWidth(int columnWidth) {
        this.columnWidth = columnWidth;
    }

    public List<String> getSortColumns() {
        return this.sortColumns;
    }

    public void setSortColumns(List<String> sortColumns) {
        this.sortColumns = sortColumns;
    }

    public Map<String, Boolean> getSortDirections() {
        return sortDirections;
    }

    public void setSortDirections(Map<String, Boolean> sortDirections) {
        this.sortDirections = sortDirections;
    }

    public void setColumnData(List<GageTableColumnData> columnSettingList) {
        String[] sortColumnOrder = new String[4];

        for (GageTableColumnData data : columnSettingList) {
            GageTableSortType sortType = data.getSort();
            if (sortType != null) {
                String columnName = data.getName();
                int sortOrder = sortType.getOrder().intValue();
                sortColumnOrder[sortOrder] = columnName;
                boolean ascending = sortType.isAscending();
                sortDirections.put(columnName, ascending);
            }
        }

        sortColumns.clear();
        for (String name : sortColumnOrder) {
            if (name != null) {
                sortColumns.add(name);
            }
        }
    }
}
