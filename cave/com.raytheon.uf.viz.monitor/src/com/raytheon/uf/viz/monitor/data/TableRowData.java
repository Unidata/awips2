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

import org.eclipse.swt.SWT;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.viz.monitor.ui.dialogs.ISortColumn;

/**
 * Data class that contains an array of TableCellData.  Each TableCellData represents one
 * cell of data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2009            lvenable     Initial creation
 * Dec 5, 2009  3424      zhao         added appName and constructor TableRowData(appName)
 * 
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class TableRowData implements Comparable<TableRowData>
{
	/**
	 *  Application name (snow, fog, safeseas, etc)
	 */
	CommonConfig.AppName appName;
		
    /**
     * Array of TableCellData.
     */
    private TableCellData[] rowCells;
    
    /**
     * Number of cells in a row.
     */
    private int numCells = 0;
    
    /**
     * Sort callback.
     */
    private ISortColumn sortCB = null;
    
    /**
     * Constructor.
     * @param numberOfColumns Number of columns (cells) to be in the array.
     */
    public TableRowData(int numberOfColumns)
    {
        numCells = numberOfColumns;
        rowCells = new TableCellData[numCells];
    }
    
    public TableRowData(CommonConfig.AppName appName, int numCells) {
    	this.appName = appName;
    	rowCells = new TableCellData[numCells];
    }
    public TableRowData(CommonConfig.AppName appName) {
    	this.appName = appName;
    	if ( appName == CommonConfig.AppName.FOG ) {
    		numCells = 13;
    	} else if ( appName == CommonConfig.AppName.SAFESEAS ) { 
    		numCells = 23;
    	} else if ( appName == CommonConfig.AppName.SNOW ) {
    		numCells = 19;
    	} else {
    		// cases of SCAN, FFMP
    	}
    	rowCells = new TableCellData[numCells];
    }
    
    /**
     * Get the table cell data.
     * @param columnIndex Column index.
     * @return Table cell data.
     */
    public TableCellData getTableCellData(int columnIndex)
    {        
        return rowCells[columnIndex];
    }
    
    /**
     * Set the table cell data.
     * @param columnIndex Column index.
     * @param newData New table cell data.
     */
    public void setTableCellData(int columnIndex, TableCellData newData)
    {        
        if (columnIndex >= rowCells.length)
        {
            return;
        }
        
        rowCells[columnIndex] = newData;
    }
    
    /**
     * Set the sort callback.
     * @param callback Sort callback.
     */
    public void setSortCallback(ISortColumn callback)
    {
        sortCB = callback;
    }
    
    /**
     * Get the number of table cell data.
     * @return The number of table cell data.
     */
    public int getNumberOfCellData()
    {
        return rowCells.length;
    }
    
    /**
     * Get the array of table cell data.
     * @return Array of table cell data.
     */
    public TableCellData[] getTableCellDataArray()
    {
        return rowCells;
    }

    /**
     * compareTo method used to sort the data.
     */
    @Override
    public int compareTo(TableRowData otherObj)
    {        
        int selectedIndex = sortCB.getSortColumn();
        int direction = sortCB.getSortDirection();        
        
        String thisData = rowCells[selectedIndex].sortByString(direction);
        String otherData = otherObj.rowCells[selectedIndex].sortByString(direction);
        
        int x = thisData.compareTo(otherData);
        
        if (direction == SWT.DOWN)
        {
            if (x < 0)
            {
                return 1;
            }
            else if (x > 0)
            {
                return -1;
            }
        }
        
        return x;
    }
}
