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

import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.WeakHashMap;

import com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetriever;

/**
 * 
 * Keeps track of which cache objects already exists so that the same metadata
 * always returns the same cahce object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SingletonCacheManager extends NoopCacheManager {

    /** all referenced objects map, auto cleaned up through weak references */
    private Map<CacheObject<?, ?>, WeakReference<CacheObject<?, ?>>> referencedObjects = new WeakHashMap<CacheObject<?, ?>, WeakReference<CacheObject<?, ?>>>();

    @SuppressWarnings("unchecked")
    @Override
    public <M, T> CacheObject<M, T> newCacheObject(M metadata,
            IObjectRetriever<M, T> retriever) {
        CacheObject<M, T> cacheObject = super.newCacheObject(metadata,
                retriever);
        // Get our weak reference to the cache object
        WeakReference<CacheObject<?, ?>> cacheObjectRef = referencedObjects
                .get(cacheObject);

        if (cacheObjectRef != null) {
            cacheObject = (CacheObject<M, T>) cacheObjectRef.get();
        }

        cacheObjectRef = new WeakReference<CacheObject<?, ?>>(cacheObject);

        referencedObjects.put(cacheObject, cacheObjectRef);

        return (CacheObject<M, T>) cacheObject;
    }
}
