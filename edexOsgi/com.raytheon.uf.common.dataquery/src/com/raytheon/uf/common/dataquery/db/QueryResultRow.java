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

package com.raytheon.uf.common.dataquery.db;

import java.util.Arrays;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Encapsulates a returned row from a database query.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 07, 2008  1673     bphillip    Initial Creation
 * Dec 18, 2013  2579     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@DynamicSerialize
public class QueryResultRow {

    /**
     * The values of the returned columns
     */
    @DynamicSerializeElement
    private Object[] columnValues;

    /**
     * Constructs an empty QueryResultRow
     */
    public QueryResultRow() {

    }

    /**
     * Constructs a QueryResultRow with the provied column values
     * 
     * @param columnValues
     *            The values of the columns
     */
    public QueryResultRow(Object[] columnValues) {
        this.columnValues = columnValues;
    }

    /**
     * Gets the column value at the specified index in the select clause
     * 
     * @param columnIndex
     *            The column index
     * @return The column value at the specified index
     */
    public Object getColumn(int columnIndex) {
        if (columnValues.length < columnIndex) {
            throw new ArrayIndexOutOfBoundsException(
                    "Cannot get result for column index: " + columnIndex
                            + " Row size is: " + columnValues.length);
        }
        return columnValues[columnIndex];
    }

    public String toString() {
        return Arrays.toString(columnValues);
    }

    /**
     * Gets the column values
     * 
     * @return The column values
     */
    public Object[] getColumnValues() {
        return columnValues;
    }

    /**
     * Sets the column values
     * 
     * @param columnValues
     *            The column values
     */
    public void setColumnValues(Object[] columnValues) {
        this.columnValues = columnValues;
    }
}
