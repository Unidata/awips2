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

import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.common.util.cache.ICacheObject;
import com.raytheon.uf.viz.core.jobs.JobPool;

/**
 * Cache object which will live forever until explicitly diposed of by user.
 * User gets rid of object permanently using ThriftObjectCache
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public final class CacheObject<M, T> implements ICacheObject {

    private static ICacheObjectManager manager = new SingletonCacheManager();

    private static JobPool requestJobPool = new JobPool("Retrieving data", 8);

    /**
     * Manager interface for managing the cached objects
     */
    public static interface ICacheObjectManager {

        public <M extends Object, T extends Object> CacheObject<M, T> newCacheObject(
                M metadata, IObjectRetriever<M, T> retriever);

        public <M, T> void objectRequested(CacheObject<M, T> cacheObject);

        public <M, T> void objectRetrieved(CacheObject<M, T> cacheObject);
    }

    /**
     * Callback to be notified when the object has been requested
     */
    public static interface ICacheObjectCallback<T> {
        /**
         * Object has been requested and is now available
         * 
         * @param object
         */
        public void objectArrived(T object);
    }

    /**
     * Object to be used for retriever cache objects, should only store
     * information needed to request the object and use that information in the
     * hashCode/equals methods. Retriever MUST override hashCode/equals in order
     * to properly use the cache!
     */
    public static interface IObjectRetriever<M, T> {
        /**
         * Retrieve the object needed, do not store it anywhere, just retrieve
         * it and return it. THIS METHOD SHOULD ONLY BE CALLED INTERNALLY BY THE
         * GeneralCacheObject!
         * 
         * @return the retrieved object
         */
        public T retrieveObject(M metadata);

        /**
         * Get the size of the object passed in in bytes
         * 
         * @param object
         * @return
         */
        public int getSize(T object);

    }

    /**
     * If your data object needs to be disposed, implement this method which
     * will be called before retrieved objects are garbage collected.
     */
    public static interface IObjectRetrieverAndDisposer<M, T> extends
            IObjectRetriever<M, T> {

        /**
         * Dispose of any resources the object may hold that require disposing
         * 
         * @param object
         */
        public void disposeObject(T object);

    }

    /**
     * Job class used to request objects asynchronously
     */
    private class CacheJob implements Runnable {

        private Object lock = new Object();

        private boolean tooLate = false;

        private boolean cancel = false;

        private Set<ICacheObjectCallback<T>> callbacks = new HashSet<ICacheObjectCallback<T>>();

        private CacheJob(ICacheObjectCallback<T> callback) {
            this();
            callbacks.add(callback);
        }

        private CacheJob() {
            super();
        }

        @Override
        public void run() {
            if (cancel) {
                return;
            }
            // get the object
            T object = getObjectSync();
            // Mark that we are too late and subsequest requests should just
            // notify the callback directly instead of adding to callbacks
            synchronized (lock) {
                tooLate = true;
            }
            // Notify callbacks we have arrived
            for (ICacheObjectCallback<T> callback : callbacks) {
                callback.objectArrived(object);
            }
        }

        /**
         * Add a cache object callback to be notified
         * 
         * @param callback
         */
        private void addCallback(ICacheObjectCallback<T> callback) {
            synchronized (lock) {
                if (!tooLate) {
                    this.callbacks.add(callback);
                } else {
                    callback.objectArrived(getObjectSync());
                }
            }
        }

        private void cancel() {
            cancel = true;
        }
    }

    private M metadata;

    /** The retriever object for the object to be cached */
    private IObjectRetriever<M, T> retriever;

    /** The job to be used to request asynchronously */
    private CacheJob job = null;

    /** Object to be cached */
    private T object;

    private Object retrievalLock = new Object();

    private Object jobLock = new Object();

    int size = 0;

    CacheObject(M metadata, IObjectRetriever<M, T> retriever) {
        this.metadata = metadata;
        this.retriever = retriever;
    }

    /**
     * Get's the needed object, blocking until the object is available.
     * 
     * @return the cached object
     */
    public final T getObjectSync() {
        synchronized (retrievalLock) {
            if (object == null) {
                // System.out.println("getObjectSync: Retrieving object!");
                setObject(retriever.retrieveObject(getMetadata()));
                CacheObject.objectRetrieved(this);
            }
        }
        CacheObject.objectRequested(this);
        return object;
    }

    /**
     * Get the object asynchronously, this method returns null if the object
     * needs to be requested or the object if it has already been requested
     * 
     * @return the object or null if being requested
     */
    public final T getObjectAsync() {
        T object = this.object;
        if (object == null) {
            synchronized (jobLock) {
                CacheJob curJob = job;
                if (curJob == null) {
                    // System.out.println("getObjectASYNC: scheduling job...");
                    curJob = new CacheJob();
                    requestJobPool.schedule(curJob);
                    job = curJob;
                }
            }
        } else {
            return object;
        }
        return null;
    }

    /**
     * Get the object asynchronously notifying the callback when it is
     * available. This notification may happen on the currently executing thread
     * if the object has already been retrieved or on a separate thread after
     * retrieval
     * 
     * @param callback
     */
    public final void getObjectAsync(ICacheObjectCallback<T> callback) {
        synchronized (jobLock) {
            CacheJob curJob = job;
            if (curJob == null) {
                curJob = new CacheJob(callback);
                // System.out
                // .println("getObjectASYNC: Scheduling job with callback");
                requestJobPool.schedule(curJob);
                job = curJob;
            } else {
                // System.out
                // .println("getObjectASYNC: Job not done, adding callback...");
                curJob.addCallback(callback);
            }
        }
    }

    /**
     * Get the metadata used to retrieve the object
     * 
     * @return
     */
    public final M getMetadata() {
        return metadata;
    }

    final boolean needsDispose() {
        return retriever instanceof IObjectRetrieverAndDisposer;
    }

    final IObjectRetrieverAndDisposer<M, T> getDisposer() {
        return (IObjectRetrieverAndDisposer<M, T>) retriever;
    }

    final IObjectRetriever<M, T> getRetriever() {
        return retriever;
    }

    final T getObject() {
        return object;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.cache.ICacheObject#getSize()
     */
    @Override
    public final int getSize() {
        return size;
    }

    /**
     * Disposes the object by setting the reference to null so garbage
     * collection may occur
     */
    void dispose() {
        // System.out.println("Disposing object!");
        synchronized (jobLock) {
            if (job != null) {
                job.cancel();
            }
            job = null;
        }
        synchronized (retrievalLock) {
            if (object != null && needsDispose()) {
                getDisposer().disposeObject(object);
            }
            object = null;
        }
    }

    private void setObject(T object) {
        this.object = object;
        size = retriever.getSize(object);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((metadata == null) ? 0 : metadata.hashCode());
        result = prime * result
                + ((retriever == null) ? 0 : retriever.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        CacheObject<?, ?> other = (CacheObject<?, ?>) obj;
        if (metadata == null) {
            if (other.metadata != null)
                return false;
        } else if (!metadata.equals(other.metadata))
            return false;
        if (retriever == null) {
            if (other.retriever != null)
                return false;
        } else if (!retriever.equals(other.retriever))
            return false;
        return true;
    }

    /**
     * Construct a new cache object given the metadata and retriever. The
     * metadata object MUST override hashCode and equals in order for the cache
     * to function properly
     * 
     * @param metadata
     * @param retriever
     * @return
     */
    public static synchronized <M extends Object, T extends Object> CacheObject<M, T> newCacheObject(
            M metadata, IObjectRetriever<M, T> retriever) {
        return manager.newCacheObject(metadata, retriever);
    }

    /**
     * Notify that the object has been requested, add the object to the managed
     * cache.
     * 
     * @param cacheObject
     */
    private static void objectRequested(CacheObject<?, ?> cacheObject) {
        manager.objectRequested(cacheObject);
    }

    /**
     * Notify that the object has been requested, add the object to the managed
     * cache.
     * 
     * @param cacheObject
     */
    private static void objectRetrieved(CacheObject<?, ?> cacheObject) {
        manager.objectRetrieved(cacheObject);
    }

}
