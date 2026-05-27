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

package com.raytheon.uf.common.grib.tables;

import java.util.HashMap;
import java.util.Map;

/**
 * Class encapsulating data about a grib table
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 07, 2009  1994     bphillip  Initial Creation
 * May 06, 2016  5572     bsteffen  Move to common
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GribTable {

    /** The center defining the table */
    private int centerId;

    /** The subcenter defining the table */
    private int subcenterId;

    /** The table number represented */
    private String tableNumber;

    /** The values of the table */
    private Map<Integer, Object> values;

    /**
     * Creates a new GribTable
     * 
     * @param centerId
     *            The center defining the table
     * @param tableNumber
     *            The table number
     */
    public GribTable(int centerId, int subcenterId, String tableNumber) {
        this.centerId = centerId;
        this.subcenterId = subcenterId;
        this.tableNumber = tableNumber;
        values = new HashMap<>();
    }

    /**
     * Gets an object from the table
     * 
     * @param key
     *            The key into the table
     * @return The value from the table, null if not present
     */
    public Object get(int key) {
        return values.get(key);
    }

    /**
     * Adds an entry to the table
     * 
     * @param key
     *            The key to the table
     * @param value
     *            The value in the table
     */
    public void addEntry(int key, Object value) {
        values.put(key, value);
    }

    /**
     * Checks if a key exists in the table
     * 
     * @param key
     *            The key to check for
     * @return True if key is present, else false
     */
    public boolean contains(int key) {
        return values.containsKey(key);
    }

    /**
     * Gets the defining center
     * 
     * @return The defining center
     */
    public int getCenterId() {
        return centerId;
    }

    /**
     * Gets the table number
     * 
     * @return The table number
     */
    public String getTableNumber() {
        return tableNumber;
    }

    /**
     * Gets the table
     * 
     * @return The table
     */
    public Map<Integer, Object> getValues() {
        return values;
    }

    /**
     * Gets the subcenter id
     * 
     * @return The subcenter id
     */
    public int getSubcenterId() {
        return subcenterId;
    }

}
