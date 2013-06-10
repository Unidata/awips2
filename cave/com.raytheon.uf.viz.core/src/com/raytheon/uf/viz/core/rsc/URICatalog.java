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

package com.raytheon.uf.viz.core.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.datastructure.DecisionTree;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.URICatalog.IURIRefreshCallback;

/**
 * 
 * Provides a memory model for the availability of data products
 * 
 * Provides a listing of the latest product available for datauri map.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2007            chammack    Initial Creation.
 * Jan 14, 2013 1442       rferrel     Added query method.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class URICatalog extends DecisionTree<List<IURIRefreshCallback>> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(URICatalog.class);

    /** Singleton instance */
    private static URICatalog instance;

    private RebuildSchedulerJob rebuildSchedulerJob;

    protected Map<Map<String, RequestConstraint>, List<IURIRefreshCallback>> queuedRCMap = new HashMap<Map<String, RequestConstraint>, List<IURIRefreshCallback>>();

    private Map<Map<String, RequestConstraint>, List<IURIRefreshCallback>> queryRCMap = new HashMap<Map<String, RequestConstraint>, List<IURIRefreshCallback>>();

    /**
     * Singleton accessor
     * 
     * @return the instance of the uri catalog
     */
    public static synchronized URICatalog getInstance() {
        if (instance == null) {
            instance = new URICatalog();
        }
        return instance;
    }

    protected static synchronized void setCustomInstance(URICatalog catalog) {
        if (instance != null) {
            for (DataPair pair : instance.getDataPairs()) {
                catalog.insertCriteria(pair.metadata, pair.data, false);
            }
            catalog.rebuildTree();
        }
        instance = catalog;
    }

    /**
     * Private constructor
     */
    protected URICatalog() {
        this.rebuildSchedulerJob = new RebuildSchedulerJob();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.datastructure.DecisionTree#insertCriteria(java.
     * util.Map, java.lang.Object, boolean)
     */
    @Override
    public void insertCriteria(Map<String, RequestConstraint> searchCriteria,
            List<IURIRefreshCallback> item, boolean rebuild) {
        searchCriteria.remove("identifier");
        searchCriteria.remove("dataURI");
        searchCriteria.remove("persistenceTime");
        searchCriteria.remove("insert_time");
        searchCriteria.remove("decoderGettable");
        synchronized (this) {
            super.insertCriteria(searchCriteria, item, rebuild);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.datastructure.DecisionTree#searchTree(java.util
     * .Map)
     */
    @Override
    public List<List<IURIRefreshCallback>> searchTree(
            Map<String, Object> searchCriteria) {
        searchCriteria.remove("identifier");
        searchCriteria.remove("dataURI");
        searchCriteria.remove("persistenceTime");
        searchCriteria.remove("decoderGettable");
        return super.searchTree(searchCriteria);
    }

    /**
     * 
     * Used to trigger rebuilds.
     * 
     * This batches requests together to reduce the number of queries
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Sep 16, 2009            chammack     Initial creation
     * 
     * </pre>
     * 
     * @author chammack
     * @version 1.0
     */
    private class RebuildSchedulerJob extends Job {

        /**
         * The amount of time to batch requests together.
         * 
         * Decreasing this number will reduce wait time on menu refresh, but
         * will increase load time due to more queries
         * 
         */
        private static final long SCHEDULE_THRESHOLD = 1000; // 1 sec

        /**
         * The last time that the data changed
         */
        public long time = 0;

        public RebuildSchedulerJob() {
            super("Menu Rebuild Scheduler");
            this.setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            long workingTime = 0;
            do {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    break;
                }
                workingTime = time;
            } while (System.currentTimeMillis() - workingTime < SCHEDULE_THRESHOLD);

            // Account for queued events that may arrive during the processing
            // below:

            do {
                workingTime = time;
                Map<Map<String, RequestConstraint>, List<IURIRefreshCallback>> runRCList = null;
                Map<Map<String, RequestConstraint>, List<IURIRefreshCallback>> runQueryList = null;
                synchronized (URICatalog.this) {
                    if (queuedRCMap.size() > 0) {
                        runRCList = queuedRCMap;
                        queuedRCMap = new HashMap<Map<String, RequestConstraint>, List<IURIRefreshCallback>>();
                    }

                    if (queryRCMap.size() > 0) {
                        runQueryList = queryRCMap;
                        queryRCMap = new HashMap<Map<String, RequestConstraint>, List<IURIRefreshCallback>>();
                    }

                }
                
                if (runRCList != null) {
                    catalogAndQueryDataURIsInternal(runRCList, monitor);
                }
                
                if (runQueryList != null) {
                    doQuery(runQueryList, monitor);
                }
            } while (!monitor.isCanceled() && workingTime != time
                    && (queuedRCMap.size() > 0 || queryRCMap.size() > 0));
            return Status.OK_STATUS;
        }
    }

    /*
     * public void catalogAndQueryDataURI(String dataURI, IURIRefreshCallback
     * runnable) {
     * 
     * synchronized (this) { List<IURIRefreshCallback> runnables =
     * queuedMap.get(dataURI); if (runnables == null) { runnables = new
     * ArrayList<IURIRefreshCallback>(); queuedMap.put(dataURI, runnables); }
     * runnables.add(runnable);
     * 
     * this.rebuildSchedulerJob.schedule(); } }
     */

    public void catalogAndQueryDataURI(Map<String, RequestConstraint> map,
            IURIRefreshCallback runnable) {
        synchronized (this) {
            List<IURIRefreshCallback> runnables = queuedRCMap.get(map);
            if (runnables == null) {
                runnables = new ArrayList<IURIRefreshCallback>();
                queuedRCMap.put(map, runnables);
            }
            runnables.add(runnable);

            scheduleRebuildSchedulerJob();
        }
    }

    protected void scheduleRebuildSchedulerJob() {
        this.rebuildSchedulerJob.schedule();
    }

    protected void cancelRebuildSchedulerJob() {
        this.rebuildSchedulerJob.cancel();
    }

    protected void catalogAndQueryDataURIsInternal(
            Map<Map<String, RequestConstraint>, List<IURIRefreshCallback>> rcMap,
            IProgressMonitor monitor) {
        try {
            for (Map.Entry<Map<String, RequestConstraint>, List<IURIRefreshCallback>> entry : rcMap
                    .entrySet()) {
                Map<String, RequestConstraint> map = entry.getKey();
                List<IURIRefreshCallback> runnable = entry.getValue();
                if (monitor.isCanceled()) {
                    // If we are cancelled add our runnables back into the queue
                    // instead of requesting times
                    synchronized (this) {
                        List<IURIRefreshCallback> runnables = queuedRCMap
                                .get(map);
                        if (runnables == null) {
                            queuedRCMap.put(map, runnable);
                        } else {
                            runnables.addAll(runnable);
                        }
                    }
                    continue;
                }
                doCallbacks(map, runnable);

                for (Map<String, RequestConstraint> updateMap : DataCubeContainer
                        .getBaseUpdateConstraints(map)) {
                    insert(updateMap, rcMap.get(map));
                }
            }

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error performing latest time query", e);
        }

        long t0 = System.currentTimeMillis();
        rebuildTree();
        System.out
                .println("Rebuild took: " + (System.currentTimeMillis() - t0));

    }

    private void doCallbacks(Map<String, RequestConstraint> map,
            List<IURIRefreshCallback> runnable) throws VizException {
        DataTime[] dt = DataCubeContainer.performTimeQuery(map, true);
        DataTime newDataTime = null;
        if (dt != null && dt.length > 0) {
            newDataTime = dt[dt.length - 1];
        }

        final DataTime dataTime = newDataTime;
        if (runnable != null) {

            Iterator<IURIRefreshCallback> iterator = runnable.iterator();

            while (iterator.hasNext()) {
                final IURIRefreshCallback r = iterator.next();
                r.updateTime(dataTime);
            }
        }
    }

    /**
     * Perform query using the existing constraints in map.
     * 
     * @param map
     */
    public void query(Map<String, RequestConstraint> map) {
        synchronized (this) {
            List<List<IURIRefreshCallback>> runableList = searchTreeUsingContraints(map);
            List<IURIRefreshCallback> runnables = queryRCMap.get(map);
            if (runnables == null) {
                runnables = new ArrayList<URICatalog.IURIRefreshCallback>();
                queryRCMap.put(map, runnables);
            }

            for (List<IURIRefreshCallback> runnable : runableList) {
                runnables.addAll(runnable);
            }
            rebuildSchedulerJob.schedule();
        }
    }

    private void doQuery(
            Map<Map<String, RequestConstraint>, List<IURIRefreshCallback>> queryMap,
            IProgressMonitor monitor) {
        try {
            for (Map.Entry<Map<String, RequestConstraint>, List<IURIRefreshCallback>> entry : queryMap
                    .entrySet()) {
                Map<String, RequestConstraint> map = entry.getKey();
                List<IURIRefreshCallback> runnable = entry.getValue();
                if (monitor.isCanceled()) {
                    // If we are cancelled add our runnables back into the queue
                    // instead of requesting times
                    synchronized (this) {
                        List<IURIRefreshCallback> runnables = queryRCMap
                                .get(map);
                        if (runnables == null) {
                            queryRCMap.put(map, runnable);
                        } else {
                            runnables.addAll(runnable);
                        }
                    }
                    continue;
                }
                doCallbacks(map, runnable);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    private void insert(Map<String, RequestConstraint> map,
            List<IURIRefreshCallback> runnables) throws VizException {
        insertCriteria(map, runnables, false);
    }

    public static interface IURIRefreshCallback {

        /** check and update most recent time */
        public abstract void updateTime(DataTime time);
    }
}
