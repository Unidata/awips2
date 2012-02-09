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
package com.raytheon.uf.viz.thinclient.cave.cache;

import java.io.File;

import com.raytheon.uf.viz.thinclient.cache.AbstractCachePersistance;
import com.raytheon.uf.viz.thinclient.cave.cache.map.CacheDbMapQueryFactory;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * Cache persistence for direct map query caching
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class MapQueryCachePersistence extends AbstractCachePersistance {

    public MapQueryCachePersistence() {
        super(ThinClientPreferenceConstants.P_CACHE_MAPS, "maps.cache");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.cache.ThinClientCacheManager.ICachePersistance
     * #store(java.io.File)
     */
    @Override
    public void store(File cacheFile) {
        CacheDbMapQueryFactory.storeCache(cacheFile);
        // MapQueryCache.storeCache(cacheFile);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.cache.ThinClientCacheManager.ICachePersistance
     * #restore(java.io.File)
     */
    @Override
    public void restore(File cacheFile) {
        CacheDbMapQueryFactory.restoreCache(cacheFile);
        // MapQueryCache.restoreCache(cacheFile);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.cache.AbstractCachePersistance#enable()
     */
    @Override
    protected void enable() {
        CacheDbMapQueryFactory.setEnableCaching(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.cache.AbstractCachePersistance#disable()
     */
    @Override
    protected void disable() {
        CacheDbMapQueryFactory.setEnableCaching(false);
    }

}
