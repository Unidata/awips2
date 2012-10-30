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

import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;

/**
 * Cache times for GridRequestableNode's to avoid multiple trips to edex for the
 * same data. The hit rate tends to be low when requesting individual products
 * but can get very high when requesting complex derived parameters or model
 * families since often many of the derived parameters can have dependencies on
 * the same base parameters.
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

public class GridTimeCache {

    protected static final int CACHE_SIZE = 500;

    protected static final int CACHE_TIME = 300000;

    private static GridTimeCache instance;

    public static GridTimeCache getInstance() {
        if (instance == null) {
            instance = new GridTimeCache();
        }
        return instance;
    }

    private GridTimeCache() {

    }

    private class CacheEntry {

        public CacheEntry(Set<TimeAndSpace> times) {
            this.insertTime = System.currentTimeMillis();
            this.times = times;
        }

        public long insertTime;

        public Set<TimeAndSpace> times;

    }

    private Map<GridMapKey, CacheEntry> cache = new LinkedHashMap<GridMapKey, CacheEntry>(
            135, .75f, true) {

        private static final long serialVersionUID = 2022670836957170184L;

        // used to make sure we don't spam the logs
        private long lastLogTime;

        // Tracks the shortest amount of time that something has remained in the
        // cache recently.
        private long minTimeInCache = CACHE_TIME;

        /*
         * (non-Javadoc)
         * 
         * @see java.util.LinkedHashMap#removeEldestEntry(java.util.Map.Entry)
         */
        @Override
        protected boolean removeEldestEntry(Entry<GridMapKey, CacheEntry> eldest) {
            if (this.size() > CACHE_SIZE) {
                // It is normal for stale entries to stay in the map until this
                // purges them, but we want some logging for non stale entries.
                long curTime = System.currentTimeMillis();
                long timeInCache = curTime - eldest.getValue().insertTime;
                // only print if the entry is within its valid time,
                if (timeInCache < CACHE_TIME
                // Don't print more than once a minute (60000ms) unless this
                // entry has been purged faster(was in the cache for less time)
                // than the last output message.
                        && (curTime - lastLogTime > 60000 || timeInCache < minTimeInCache)) {
                    System.out.println(GridTimeCache.class.getSimpleName()
                            + " is purging after only " + timeInCache + "ms");
                    lastLogTime = curTime;
                    // this is now the shortest time in cache.
                    minTimeInCache = timeInCache;
                }
                return true;
            }
            return false;
        }
    };

    public synchronized void setTimes(GridRequestableNode gNode,
            Set<TimeAndSpace> times) {
        cache.put(new GridMapKey(gNode.getRequestConstraintMap()),
                new CacheEntry(times));
    }

    public synchronized Set<TimeAndSpace> getTimes(GridRequestableNode gNode) {
        GridMapKey key = new GridMapKey(gNode.getRequestConstraintMap());
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

    public synchronized void clearTimes(GridMapKey key) {
        cache.remove(key);
    }

    public synchronized void flush() {
        cache.clear();
    }

}
