/*****************************************************************************************
 * COPYRIGHT (c), 2009, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.edex.util.ncgrib;

import java.util.HashMap;
import java.util.Map;

/**
 * Class encapsulating data about a grib table
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class NcgribTable {

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
    public NcgribTable(int centerId, int subcenterId, String tableNumber) {
        this.centerId = centerId;
        this.subcenterId = subcenterId;
        this.tableNumber = tableNumber;
        values = new HashMap<Integer, Object>();
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
