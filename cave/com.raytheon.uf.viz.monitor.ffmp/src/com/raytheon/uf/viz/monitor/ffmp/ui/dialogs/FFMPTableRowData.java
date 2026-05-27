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

import com.raytheon.uf.viz.monitor.ui.dialogs.ISortColumn;

/**
 * Data class that contains an array of TableCellData. Each TableCellData
 * represents one cell of data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2009            lvenable     Initial creation
 * May 7, 2013    1986   njensen   Sped up compareTo()
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FFMPTableRowData implements Comparable<FFMPTableRowData> {
    /**
     * Array of TableCellData.
     */
    private FFMPTableCellData[] rowCells;

    /**
     * Number of cells in a row.
     */
    private int numCells = 0;

    /**
     * Sort callback.
     */
    private ISortColumn sortCB = null;

    private String pfaf;

    /**
     * Constructor.
     * 
     * @param numberOfColumns
     *            Number of columns (cells) to be in the array.
     */
    public FFMPTableRowData(int numberOfColumns) {
        numCells = numberOfColumns;
        rowCells = new FFMPTableCellData[numCells];
    }

    /**
     * Get the table cell data.
     * 
     * @param columnIndex
     *            Column index.
     * @return Table cell data.
     */
    public FFMPTableCellData getTableCellData(int columnIndex) {
        return rowCells[columnIndex];
    }

    /**
     * Set the table cell data.
     * 
     * @param columnIndex
     *            Column index.
     * @param newData
     *            New table cell data.
     */
    public void setTableCellData(int columnIndex, FFMPTableCellData newData) {
        if (columnIndex >= rowCells.length) {
            return;
        }

        rowCells[columnIndex] = newData;
    }

    /**
     * Set the sort callback.
     * 
     * @param callback
     *            Sort callback.
     */
    public void setSortCallback(ISortColumn callback) {
        sortCB = callback;
    }

    /**
     * Get the number of table cell data.
     * 
     * @return The number of table cell data.
     */
    public int getNumberOfCellData() {
        return rowCells.length;
    }

    /**
     * Get the array of table cell data.
     * 
     * @return Array of table cell data.
     */
    public FFMPTableCellData[] getTableCellDataArray() {
        return rowCells;
    }

    public void setPfaf(String pfaf) {
        this.pfaf = pfaf;
    }

    public String getPfaf() {
        return pfaf;
    }

    /**
     * compareTo method used to sort the data.
     */
    @Override
    public int compareTo(FFMPTableRowData otherObj) {
        int selectedIndex = sortCB.getSortColumn();

        FFMPTableCellData selectedCell = rowCells[selectedIndex];
        if (selectedCell.getCellText() != null) {
            String thisText = selectedCell.getCellText();
            String otherText = otherObj.rowCells[selectedIndex].getCellText();
            return thisText.compareTo(otherText);
        } else {
            float thisFloat = selectedCell.sortByNumber();
            float otherFloat = otherObj.rowCells[selectedIndex].sortByNumber();
            return Float.compare(thisFloat, otherFloat);
        }
    }
}
