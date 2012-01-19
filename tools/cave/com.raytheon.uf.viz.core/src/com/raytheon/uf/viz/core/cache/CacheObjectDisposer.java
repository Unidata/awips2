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

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.WeakHashMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetrieverAndDisposer;

/**
 * 
 * Watch cache objects and dispose of the data when it is no longer used.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 1, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class CacheObjectDisposer {

    private class CleanerJob extends Job {

        public CleanerJob() {
            super("Disposing Cache Objects");
            this.setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            clean();
            return Status.OK_STATUS;
        }

        @Override
        public boolean shouldSchedule() {
            return !refMap.isEmpty();
        }

    }

    private static class CacheData<T> extends WeakReference<CacheObject<?, T>> {

        private final IObjectRetrieverAndDisposer<?, T> retriever;

        private T data;

        public CacheData(CacheObject<?, T> cacheObject,
                ReferenceQueue<CacheObject<?, ?>> refQueue) {
            super(cacheObject, refQueue);
            this.retriever = (IObjectRetrieverAndDisposer<?, T>) cacheObject.getRetriever();
            this.data = cacheObject.getObject();
        }

        public void clear() {
            data = null;
        }

        public void dispose() {
            if (data != null) {
                retriever.disposeObject(data);
            }
        }
    }

    private CleanerJob job = new CleanerJob();

    private ReferenceQueue<CacheObject<?, ?>> refQueue = new ReferenceQueue<CacheObject<?, ?>>();

    private Map<CacheObject<?, ?>, CacheData<?>> refMap = new WeakHashMap<CacheObject<?, ?>, CacheData<?>>();

    /**
     * Watch this cache object, when it is no longer referenced call dispose on
     * it's object.
     * 
     * @param <T>
     * @param cacheObject
     */
    public <T> void watch(CacheObject<?, T> cacheObject) {
        if (!cacheObject.needsDispose()) {
            return;
        }
        CacheData<?> data = refMap.remove(cacheObject);
        if (data != null) {
            data.clear();
        }
        data = new CacheData<T>(cacheObject, refQueue);
        refMap.put(cacheObject, data);
        clean();
    }

    /**
     * force disposal of this cache object, and stop watching it.
     * 
     * @param <T>
     * @param cacheObject
     */
    public <T> void dispose(CacheObject<?, T> cacheObject) {
        CacheData<?> data = refMap.remove(cacheObject);
        if (data != null) {
            data.clear();
        }
        cacheObject.dispose();
        clean();
    }

    private void clean() {
        CacheData<?> cd = (CacheData<?>) refQueue.poll();
        while (cd != null) {
            cd.dispose();
            cd = (CacheData<?>) refQueue.poll();
        }
        job.schedule(1000);
    }

}
