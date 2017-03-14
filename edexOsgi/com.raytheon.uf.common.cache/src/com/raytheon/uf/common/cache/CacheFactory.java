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
package com.raytheon.uf.common.cache;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class CacheFactory {
    private static CacheFactory instance = new CacheFactory();

    private ConcurrentMap<String, ICache> cacheMap = new ConcurrentHashMap<String, ICache>();

    public static CacheFactory getInstance() {
        return instance;
    }

    /**
     * Returns
     * 
     * @param cacheName
     * @return
     */
    public ICache getCache(String cacheName) throws CacheException {
        ICache cache = cacheMap.get(cacheName);

        if (cache == null) {
            throw new CacheException("Requested Cache " + cacheName
                    + " not configured");
        }

        return cache;
    }

    /**
     * Throws exception if cache already exists.
     * 
     * @param cacheName
     * @param cache
     */
    public void addCache(String cacheName, ICache cache) {
        cacheMap.put(cacheName, cache);
    }
    
    public void removeCache(String cacheName) {
    	
    	cacheMap.remove(cacheName);
    }
}
