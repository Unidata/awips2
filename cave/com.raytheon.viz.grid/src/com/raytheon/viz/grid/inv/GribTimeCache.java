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
package com.raytheon.viz.grid.inv;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.time.DataTime;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GribTimeCache {

    protected static final int CACHE_SIZE = 500;

    protected static final int CACHE_TIME = 300000;

    private static GribTimeCache instance;

    public static GribTimeCache getInstance() {
        if (instance == null) {
            instance = new GribTimeCache();
        }
        return instance;
    }

    private GribTimeCache() {

    }

    private class CacheEntry {

        public CacheEntry(Set<DataTime> times) {
            this.insertTime = System.currentTimeMillis();
            this.times = times;
        }

        public long insertTime;

        public Set<DataTime> times;

    }

    private Map<GribMapKey, CacheEntry> cache = new LinkedHashMap<GribMapKey, CacheEntry>(
            135, .75f, true) {

        private static final long serialVersionUID = 2022670836957170184L;

        /*
         * (non-Javadoc)
         * 
         * @see java.util.LinkedHashMap#removeEldestEntry(java.util.Map.Entry)
         */
        @Override
        protected boolean removeEldestEntry(Entry<GribMapKey, CacheEntry> eldest) {
            return this.size() > CACHE_SIZE;
        }
    };

    public synchronized void setTimes(GribRequestableLevelNode gNode,
            Set<DataTime> times) {
        cache.put(new GribMapKey(gNode.getRequestConstraintMap()),
                new CacheEntry(times));
    }

    public synchronized Set<DataTime> getTimes(GribRequestableLevelNode gNode) {
        GribMapKey key = new GribMapKey(gNode.getRequestConstraintMap());
        CacheEntry entry = cache.get(key);
        if (entry == null) {
            return null;
        }
        if (entry.insertTime + CACHE_TIME < System.currentTimeMillis()) {
            cache.remove(key);
            return null;
        }
        return entry.times;
    }

    public synchronized void clearTimes(GribMapKey key) {
        cache.remove(key);
    }

    public synchronized void flush() {
        cache.clear();
    }

}
