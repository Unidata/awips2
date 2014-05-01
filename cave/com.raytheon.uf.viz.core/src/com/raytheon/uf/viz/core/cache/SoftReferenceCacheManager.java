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

import com.raytheon.uf.viz.core.cache.CacheObject.ICacheObjectManager;
import com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetriever;

/**
 * TODO Write soft reference cache manager
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

public class SoftReferenceCacheManager implements ICacheObjectManager {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.cache.CacheObject.ICacheObjectManager#newCacheObject
     * (java.lang.Object,
     * com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetriever)
     */
    @Override
    public <M, T> CacheObject<M, T> newCacheObject(M metadata,
            IObjectRetriever<M, T> retriever) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.cache.CacheObject.ICacheObjectManager#
     * objectRequested(com.raytheon.uf.viz.core.cache.CacheObject)
     */
    @Override
    public <M, T> void objectRequested(CacheObject<M, T> cacheObject) {
        // TODO Auto-generated method stub

    }

    @Override
    public <M, T> void objectRetrieved(CacheObject<M, T> cacheObject) {
        // TODO Auto-generated method stub

    }

}
