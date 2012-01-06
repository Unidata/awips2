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

import com.raytheon.uf.viz.monitor.ui.dialogs.ISortColumn;

/**
 * 
 * The class contains one row of data (a row of SCANTableCellData).
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
public class SCANTableRowData implements Comparable<SCANTableRowData>
{
    /**
     * Array of table cell data.
     */
    private SCANTableCellData[] rowCells;
    
    /**
     * Sort column callback.
     */
    private ISortColumn sortCB = null;
    
    /**
     * Identifier.
     */
    private String ident;
    
    /**
     * VCP value
     */
    private Integer vcp=0;

    /**
     * COnstructor.
     * @param numberOfColumns Number of columns in the data row.
     */
    public SCANTableRowData(int numberOfColumns)
    {
        rowCells = new SCANTableCellData[numberOfColumns];
    }
    
    /**
     * Get the table cell data at the specified column index.
     * @param columnIndex Column index.
     * @return Table cell data.
     */
    public SCANTableCellData getTableCellData(int columnIndex)
    {
        return rowCells[columnIndex];
    }
    
    /**
     * Set the table cell data.
     * @param columnIndex Column index.
     * @param newData New table cell data.
     */
    public void setTableCellData(int columnIndex, SCANTableCellData newData)
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
    public SCANTableCellData[]  getTableCellDataArray()
    {
        return rowCells;
    }
    
    /**
     * Set the identifier.
     * @param ident Identifier.
     */
    public void setIdent(String ident)
    {
        this.ident = ident;
    }
    
    /**
     * Get the identifier.
     * @return Identifier.
     */
    public String getIdent()
    {
        return ident;
    }
    
    /**
     * Compare to method.
     */
    @Override
    public int compareTo(SCANTableRowData otherObj)
    {        
        int selectedIndex = sortCB.getSortColumn();
        int direction = sortCB.getSortDirection();      
        int x = 0;
        
        Object thisData = rowCells[selectedIndex].sortByObject(direction);
        Object otherData = otherObj.rowCells[selectedIndex].sortByObject(direction);
        
        if (thisData instanceof String)
        {
            x = ((String)thisData).compareTo((String)otherData);
        }
        else if (thisData instanceof Double)
        {
            double thisNumber = (Double)thisData;
            double otherNumber = (Double)otherData;
            
            if (thisNumber < otherNumber)
            {
                x = -1;
            }
            else if (thisNumber > otherNumber)
            {
                x = 1;
            }
            else
            {
                x = 0;
            }
        }
        
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

    public void setVcp(Integer vcp) {
        this.vcp = vcp;
    }
    
    public Integer getVcp() {
        return vcp;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < rowCells.length; i++) {
            sb.append(rowCells[i].getCellText() + " ");
        }
        sb.append("\n");
        return sb.toString();
    }
}
