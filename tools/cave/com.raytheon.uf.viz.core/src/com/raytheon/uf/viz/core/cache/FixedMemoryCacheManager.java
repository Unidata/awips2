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
package com.raytheon.uf.viz.core.cache;

import com.raytheon.uf.common.util.cache.LRUCache;

/**
 * Fixed memory cache manager, uses an LRU map and weak reference map to cache
 * objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class FixedMemoryCacheManager extends SingletonCacheManager {

    /**
     * Our managed cache, taking advantage of the LRUCache TODO: Make memory
     * size user preference, have intelligent default. Also add in timer that
     * purges data not accessed for last X minues. Make minutes user
     * configurable
     */
    private LRUCache<Object, CacheObject<?, ?>> managedCache = new LRUCache<Object, CacheObject<?, ?>>(
            16 * 1024 * 1024) {

        @Override
        protected void removeItem(Item item) {
            super.removeItem(item);
            disposer.dispose(item.value);
        }

    };

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.cache.CacheObject.ICacheObjectManager#
     * objectRequested(com.raytheon.uf.viz.core.cache.CacheObject)
     */
    @Override
    public <M, T> void objectRequested(CacheObject<M, T> cacheObject) {
        managedCache.put(cacheObject.getMetadata(), cacheObject);
    }

}
