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
package com.raytheon.edex.plugin.shef.data.precip;

import java.util.List;

/**
 * Persistent precipitation record storage.
 * <p>
 * Supports creating, reading, updating (add only), and clearing all records
 * from the store.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2008 1548       jelkins     Initial creation
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class PrecipRecordStorage {

    private static List<PrecipRecord> records;

    private static PrecipRecordStorage storage;

    private static int recordCount;
    
    /**
     * Class constructor.
     * <p>
     * The getStorage() static method should be used instead of a constructor
     * for accessing this class.
     */
    private PrecipRecordStorage() {
        recordCount = 0;
    }

    public synchronized void incrementRecordCount() {
        recordCount++;
    }
    
    public int getRecordCount() {
        return recordCount;
    }
    
    /**
     * Clear all records from the store
     */
    public void clear() {
        recordCount = 0;
    }

    /**
     * @return a pre-existing PrecipRecordStorage if one already exists or a new
     *         one if one doesn't exist.
     */
    public static PrecipRecordStorage getStorage() {
        if (storage == null) {
            storage = new PrecipRecordStorage();
        }
        return storage;
    }

}
