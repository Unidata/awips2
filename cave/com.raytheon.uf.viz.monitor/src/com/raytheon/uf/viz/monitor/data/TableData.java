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
package com.raytheon.uf.viz.monitor.data;

import java.util.ArrayList;
import java.util.Collections;

import org.eclipse.swt.SWT;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.SortDirection;
import com.raytheon.uf.viz.monitor.ui.dialogs.ISortColumn;

/**
 * Data class that contains an array of TableRowData objects.  Each TableRowData object
 * is one row of data in the table.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2009             lvenable     Initial creation
 * Dec 3, 2009  3424       zhao         added addTableRowData(TableRowData)
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class TableData implements ISortColumn
{
    /**
     * Array of TableRowData.
     */
    private ArrayList<TableRowData> tableRows;
    
    /**
     * Current column index to sort on.
     */
    private int currentSortColumnIndex = 0;
    
    /**
     * Current sort direction.
     */
    private int currentSortDirection = SWT.DOWN;
    
    /**
     * Sort column index.
     */
    private int sortColumnIndex = 0;
    
    /**
     * Sort direction.
     */
    private int sortDirection = SWT.NONE;
    
    /**
     * Primary column index.  This is the first column and will always sort this column first.
     */
    private int primaryColIndex = 0;
    
    /**
     * Application name.
     */
    private CommonConfig.AppName appName;
    
    /**
     * Constructor.
     * @param app Application name.
     */
    public TableData(CommonConfig.AppName app)
    {
        tableRows = new ArrayList<TableRowData>();
        appName = app;
    }

    /**
     * Add a row of cell data to the table rows collection.  
     * @param data Table row data.
     */
    public void addTableRowData(TableRowData data)
    {        
        data.setSortCallback(this);
        tableRows.add(data);
    }    
    
    /**
     * Add or replace a row of cell data to the table rows collection.  A row is
     * replaced if the area Id already exists.
     * @param data Table row data.
     */
    public void addReplaceDataRow(TableRowData data)
    {        
        data.setSortCallback(this);
        String areaID = data.getTableCellData(0).getCellText();
        int counter = 0;
        for (TableRowData tableRowData : tableRows)
        {            
            if (areaID.compareTo(tableRowData.getTableCellData(0).getCellText()) == 0)
            {
                tableRows.set(counter, data);
                return;
            }
            
            ++counter;
        }

        tableRows.add(data);
    }
    
    /**
     * Replace the data in a table cell.
     * @param columnIndex Column index.
     * @param areaID Area ID (acts like a primary key).
     * @param data Table cell data.
     */
    public void replaceTableCellData(int columnIndex, String areaID, TableCellData data)
    {
        for (TableRowData tableRowData : tableRows)
        {            
            if (areaID.equals(tableRowData.getTableCellData(0).getCellText()) == true)
            {
                tableRowData.setTableCellData(columnIndex, data);
            }
        }
    }
    
    /**
     * Get the table cell data.
     * @param columnIndex Column index.
     * @param areaID Area ID (acts like a primary key).
     * @return Table cell data.
     */
    public TableCellData getTableCellData(int columnIndex, String areaID)
    {        
        for (TableRowData tableRowData : tableRows)
        {            
            if (areaID.equals(tableRowData.getTableCellData(0).getCellText()) == true)
            {
                return tableRowData.getTableCellData(0);
            }
        }
        
        return null;
    }
    
    /**
     * Get table row data.
     * @param id Area ID.
     * @return Table row data.
     */
    public TableRowData getTableRowData(String id)
    {
        for (TableRowData trd : tableRows)
        {
            if (id.equals(trd.getTableCellData(0).getCellText()) == true)
            {
                return trd;
            }
        }
        
        return null;
    }
    
    /**
     * Get array of table row data.
     * @return Array of table row data.
     */
    public ArrayList<TableRowData> getTableRows()
    {
        return tableRows;
    }
    
    /**
     * Set the sort column and direction.
     * @param columnIndex Column index.
     * @param direction Sort direction.
     */
    public void setSortColumnAndDirection(int columnIndex, int direction)
    {
        sortColumnIndex = columnIndex;
        sortDirection = direction;
    }

    /**
     * Get the sort column index.
     * @return The index of the column to be sorted.
     */
    @Override
    public int getSortColumn()
    {
        return currentSortColumnIndex;
    }
    
    /**
     * Get the sort direction.
     * @return The sort direction.
     */
    @Override
    public int getSortDirection()
    {
        return currentSortDirection;
    }
    
    /**
     * Set the primary column index (act like a primary key).
     * @param index Column index.
     */
    public void setPrimaryColumnIndex(int index)
    {
        primaryColIndex = index;
    }
    
    /**
     * Sort the table data.
     */
    public void sortData()
    {
        /*
         * Sort the data by Area ID.
         */
        currentSortColumnIndex = primaryColIndex;
        currentSortDirection = SortDirection.Ascending.getSortDir();
        
        Collections.sort(tableRows);
        
        /*
         * Sort the data by the selected column.
         */
        if (primaryColIndex != sortColumnIndex)
        {
            currentSortColumnIndex = sortColumnIndex;
            currentSortDirection = sortDirection;
            
            Collections.sort(tableRows);
        }
    }
    
    /**
     * Get the application name.
     * @return The application name.
     */
    public CommonConfig.AppName getAppName()
    {
        return appName;
    }
    
    /**
     * get stations in zoneID
     * 
     * [obsolete: avoid using this method; 
     * get station table data using ObMultiHrsReports' getStationTableData 
     * method for a specific nominal-time (the latest nominal-time by 
     * default) and a specific zone -- Dec 17, 2009, zhao]
     *  
     * @param zoneID
     * @return
     */
    public TableData getStationTableOfAZone (String zoneID){
    	TableData stationTable = new TableData(this.getAppName());
    	ArrayList<TableRowData> tableRows=this.getTableRows();

        for (TableRowData tableRowData : tableRows)
        {            
       		if (zoneID.compareTo(tableRowData.getTableCellData(1).getCellText())==0)
            {
       			TableRowData trd = new TableRowData(tableRowData.getNumberOfCellData()-1);
       			trd.setTableCellData(0, tableRowData.getTableCellData(0));
       			
       			//the 2nd cell in tableRowData should be areaID, so skip it.
       			for (int i=1;i<tableRowData.getNumberOfCellData()-1;i++)
       				trd.setTableCellData(i, tableRowData.getTableCellData(i+1));

       			stationTable.addReplaceDataRow(trd);
            }
        }
        
        return stationTable;
    }

    /**
     * Set table rows 
     * @param tableRows 
     */
	public void setTableRows(ArrayList<TableRowData> tableRows) {
		this.tableRows = tableRows;
	}   
}
