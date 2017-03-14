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

import java.text.SimpleDateFormat;
import java.util.TimeZone;
import java.util.Vector;

import javax.swing.table.DefaultTableModel;

/**
 * GageTable Dialog table model.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2009 2476       mpduff     Initial creation
 * Jan 13, 2016 18092      snaples    Updated to use DefaultTableModel instead of Abstract
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GageTableModel extends DefaultTableModel implements
        GageTableListener {
    private static final long serialVersionUID = -814822107762024666L;

    /**
     * The date format.
     */
    private static final SimpleDateFormat sdf;

    /**
     * Row data.
     */
    private static Vector<Vector<String>> rows = null;

    /**
     * Column data.
     */
    private static Vector<String> columns = null;
    
    
    static {
        sdf = new SimpleDateFormat("yyyyMMddHH");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        GageTableDataManager dataManager = GageTableDataManager.getInstance();
        
        // Get the data
        rows = dataManager.getRows();
        columns = dataManager.getColumns();
    }

    /**
     * Constructor.
     */
    public GageTableModel() {
        GageTableProductManager productManager = GageTableProductManager
                .getInstance();
        GageTableDataManager dataManager = GageTableDataManager.getInstance();
        
        // Get the data
        rows = dataManager.getRows();
        columns = dataManager.getColumns();
        productManager.addGageTableListener(this);
    }

    /**
     * Refresh the table.
     */
    public void refreshTable() {
        GageTableDataManager dataManager = GageTableDataManager.getInstance();
        dataManager.reloadData();
        updateData(dataManager.getRows(), dataManager.getColumns());
        fireTableStructureChanged();
    }

    /**
     * Update the data for the model.
     * 
     * @param rows
     *      The new row data
     * @param columns
     *      The new column data
     */
    public void updateData(Vector<Vector<String>> rows, Vector<String> columns) {
        GageTableModel.rows = rows;
        GageTableModel.columns = columns;
    }
    
    /**
     * Get the column count.
     */
    @Override
    public int getColumnCount() {
        return columns.size();
    }

    /**
     * Get the row count.
     */
    @Override
    public int getRowCount() {
        return rows.size();
    }

    /**
     * Get the value at the indicated cell.
     * 
     * @param rowIndex
     *      The index of the row
     * @param columnIndex
     *      The index of the column
     */
    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        return rows.get(rowIndex).get(columnIndex);
    }

    /**
     * Is cell editable?
     * 
     * @param row
     *      The index of the row
     * @param col
     *      The index of the column
     */
    @Override
    public boolean isCellEditable(int row, int col) {
        // Note that the data/cell address is constant,
        // no matter where the cell appears onscreen.
        // col 2 is the Edit Gage Value column
        if (col == 2) {
            return true;
        } else {
            return false;
        }
    }

    
    @Override
    public void setValueAt(Object value, int row, int col) {
        Vector<String> record = rows.get(row);
        record.setElementAt((String) value, col);
        fireTableCellUpdated(row, col);
    }

    @Override
    public String getColumnName(int col) {
        return columns.get(col);
    }

    /**
     * Returns the rows data Vector.
     * 
     * @return rows
     *      The Vector of data
     */
    public Vector<Vector<String>> getDataVector() {
        return rows;
    }

    /**
     * Set the rows data Vector.
     * 
     * @param rows
     *      The Vector of data
     */
    public void setDataVector(Vector<Vector<String>> rows) {
        GageTableModel.rows = rows;
    }

    /**
     * @return the columns
     */
    public Vector<String> getColumns() {
        return columns;
    }

    /**
     * @param columns the columns to set
     */
    public void setColumns(Vector<String> columns) {
        GageTableModel.columns = columns;
    }

    @Override
    public void notifyUpdate(GageTableUpdateEvent ue) {
        System.out.println("GageTableModel.notifyUpdate() called...");
        if (ue.isChanged()) {
            refreshTable();
        }
    }
}
