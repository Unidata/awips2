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
package com.raytheon.uf.edex.plugin.loctables.util.store;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * The StationIdMap store strategy does not directly store data but allows
 * a client to aggregate ObStationRow data for further processing once the
 * map has been fully populated.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2011            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class StationIdMap extends AbstractStoreStrategy implements Iterable<ObStationRow>  {

    private Map<String,ObStationRow> locMap = new HashMap<String,ObStationRow>();
    
    /**
     * 
     * @see com.raytheon.uf.edex.plugin.loctables.util.store.RowStoreStrategy#store(com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow)
     */
    @Override
    public boolean store(ObStationRow row) {
        boolean stored = false;
        if((locMap != null) && (row != null)) {
            System.out.println(row.toSQLInsertString());
            if(!locMap.containsKey(row.getGid())) {
                locMap.put(row.getGid(),row);
                stored = true;
            }
        }
        return stored;
    }

    /**
     * Does the internal map contain a key to the specified row?
     * @param row A row that provides the retrieval key.
     * @return Returns true if the internal key exists, false otherwise.
     */
    public boolean contains(ObStationRow row) {
        boolean contained = false;
        if((locMap != null) && (row != null)) {
            contained = locMap.containsKey(row.getGid());
        }
        return contained;
    }

    /**
     * Get this maps representation of a given external row.
     * @param row A row that provides the retrieval key.
     * @return The internal row if it exists, return a null otherwise.
     */
    public ObStationRow get(ObStationRow row) {
        ObStationRow containedRow = null;
        if((locMap != null) && (row != null)) {
            containedRow = locMap.get(row.getGid());
        }
        return containedRow;
    }
    
    /**
     * 
     * @see java.io.Closeable#close()
     */
    @Override
    public void close() throws IOException {
        locMap.clear();
        locMap = null;
    }

    /**
     * @see java.lang.Iterable#iterator()
     */
    @Override
    public Iterator<ObStationRow> iterator() {
        ArrayList<ObStationRow> intList = new ArrayList<ObStationRow>();
        if(locMap != null) {
            intList.addAll(locMap.values());
        }
        return intList.iterator();
    }
}
