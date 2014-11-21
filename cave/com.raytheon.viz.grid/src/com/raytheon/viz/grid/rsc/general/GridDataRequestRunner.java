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
package com.raytheon.viz.grid.rsc.general;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;

/**
 * 
 * Manages asynchronously requesting data for GridResources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2011            bsteffen    Initial creation
 * Jun 04, 2013 2041       bsteffen    Improve exception handing in grid
 *                                     resources.
 * Jun 24, 2013 2140       randerso    Moved safe name code into AbstractVizResource
 * Oct 07, 2014 3668       bclement    uses executor instead of eclipse job
 *                                      renamed to GridDataRequestRunner
 * Oct 23, 2014 3668       bsteffen    replace executor with job pool so user
 *                                     sees progress.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
class GridDataRequestRunner implements Runnable {

    private static final int POOL_SIZE = Integer.getInteger(
            "grid.request.pool.size", 10);

    private static final JobPool jobPool = new JobPool("Requesting Grid Data",
            POOL_SIZE);

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridDataRequestRunner.class);

    private static class GridDataRequest {

        public final DataTime time;

        public final List<PluginDataObject> pdos;

        public List<GeneralGridData> gridData;

        public VizException exception;

        // True only if exception has already been handled(i.e. the user has
        // been notified.)
        public boolean exceptionHandled = false;

        public GridDataRequest(DataTime time, List<PluginDataObject> pdos) {
            this.time = time;
            if (pdos == null) {
                this.pdos = null;
            } else {
                this.pdos = new ArrayList<PluginDataObject>(pdos);
            }

        }

        public boolean shouldRequest() {
            return (gridData == null) && (exception == null);
        }

    }

    /**
     * This class is not designed to handle multiple requests concurrently. To
     * ensure this doesn't happen we track when it is scheduled and do not
     * schedule again. It would have been simpler to synchronize the run method
     * but that ties up threads from the pool that other resources should use.
     * So we don't leave dangling requests this should only be modified while
     * synchronized on requests.
     */
    private volatile boolean scheduled = false;

    private AbstractGridResource<?> resource;

    private List<GridDataRequest> requests = new ArrayList<GridDataRequest>();

    public GridDataRequestRunner(AbstractGridResource<?> resource) {
        this.resource = resource;
    }

    @Override
    public void run() {
        for (GridDataRequest request = getNext(); request != null; request = getNext()) {
            try {
                request.gridData = resource.getData(request.time, request.pdos);
                if (request.gridData == null) {
                    /*
                     * need to remove unfulfillable requests to avoid infinite
                     * loop.
                     */
                    synchronized (requests) {
                        requests.remove(request);
                    }
                } else {
                    resource.issueRefresh();
                }
            } catch (VizException e) {
                request.exception = e;
                resource.issueRefresh();
            }
        }
    }

    /**
     * Get the next request that should be sent
     * 
     * @return null if no request should be sent
     */
    protected GridDataRequest getNext() {
        synchronized (requests) {
            for (GridDataRequest request : requests) {
                if (request.shouldRequest()) {
                    return request;
                }
            }
            scheduled = false;
        }
        return null;
    }

    /**
     * @param time
     * @param pdos
     * @return null if no requests matching time have been fulfilled
     */
    public List<GeneralGridData> requestData(DataTime time,
            List<PluginDataObject> pdos) {
        GridDataRequest request = new GridDataRequest(time, pdos);
        synchronized (requests) {
            Iterator<GridDataRequest> itr = requests.iterator();
            while (itr.hasNext()) {
                GridDataRequest r = itr.next();
                if (r.time.equals(time)) {
                    itr.remove();
                    if (r.gridData != null) {
                        return r.gridData;
                    } else if ((r.pdos == null) && (pdos == null)) {
                        request = r;
                    } else if ((r.pdos != null) && r.pdos.equals(pdos)) {
                        request = r;
                    }
                }
            }
            requests.add(0, request);
            if ((request.exception != null) && !request.exceptionHandled) {
                handleExceptions();
            }
            if (!scheduled && request.shouldRequest()) {
                scheduled = true;
                jobPool.schedule(this);
            }
        }
        return null;
    }

    private void handleExceptions() {

        List<GridDataRequest> failedRequests = new ArrayList<GridDataRequest>(
                requests.size());
        synchronized (requests) {
            for (GridDataRequest request : requests) {
                if ((request.exception != null) && !request.exceptionHandled) {
                    failedRequests.add(request);
                }
            }
        }
        if (failedRequests.isEmpty()) {
            return;
        }
        String safeResourceName = resource.getSafeName();
        boolean multiple = failedRequests.size() > 1;
        GridDataRequest request = failedRequests.get(0);
        // Only log one message as a PROBLEM
        statusHandler.handle(Priority.PROBLEM,
                getFailureMessage(safeResourceName, request, multiple),
                request.exception);
        if (multiple) {
            // Log all other messages as INFO so that if there are differences
            // they will be in the log.
            for (GridDataRequest r : failedRequests) {
                statusHandler.handle(Priority.INFO,
                        getFailureMessage(safeResourceName, r, false),
                        r.exception);
                r.exceptionHandled = true;
            }
        } else {
            request.exceptionHandled = true;
        }

    }

    /**
     * Attempt to generate a pretty message to display to the user.
     * 
     * @param resourceName
     *            the name of the resource
     * @param request
     *            a failed request
     * @param multiple
     *            whether this message should be for the current request or
     *            multiple frames.
     * @return
     */
    private String getFailureMessage(String resourceName,
            GridDataRequest request, boolean multiple) {
        StringBuilder message = new StringBuilder("Error requesting data for ");
        message.append(resourceName);
        if (multiple) {
            message.append(" Multiple frames");
        } else if (request.time != null) {
            message.append(" ").append(request.time.getLegendString());
        }
        if (request.exception != null) {
            message.append(": ");
            message.append(request.exception.getLocalizedMessage());
        }
        return message.toString();
    }

    public void remove(DataTime time) {
        synchronized (requests) {
            Iterator<GridDataRequest> itr = requests.iterator();
            while (itr.hasNext()) {
                GridDataRequest r = itr.next();
                if (r.time.equals(time)) {
                    itr.remove();
                }
            }
        }
    }

    public void stopAndClear() {
        synchronized (requests) {
            requests.clear();
            if (jobPool.cancel(this)) {
                scheduled = false;
            }
        }
    }

}
