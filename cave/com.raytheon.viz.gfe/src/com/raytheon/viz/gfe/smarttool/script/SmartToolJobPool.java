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
package com.raytheon.viz.gfe.smarttool.script;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import jep.JepException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.gfe.StatusConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.jobs.AsyncProgressJob;
import com.raytheon.viz.gfe.smarttool.EditAction;
import com.raytheon.viz.gfe.smarttool.SmartToolException;
import com.raytheon.viz.gfe.smarttool.Tool;

/**
 * Job pool for running smart tools off the UI thread.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 09, 2013  #2367    dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SmartToolJobPool {

    protected LinkedBlockingQueue<SmartToolRequest> workQueue = new LinkedBlockingQueue<SmartToolRequest>();

    protected LinkedBlockingQueue<Job> jobQueue = new LinkedBlockingQueue<Job>();

    protected List<Job> jobList;

    protected boolean cancel = false;

    protected Object cancelLock = new Object();

    protected Object joinLock = new Object();

    private final DataManager dataMgr;

    private final int poolMaxSize;

    /**
     * Creates a new SmartToolJobPool with the specified size parameters.
     * 
     * @param corePoolSize
     *            The minimum size of the job pool--will always have at least
     *            this many Jobs ready to execute.
     * @param poolMaxSize
     *            The maximum size of the job pool.
     * @param dataMgr
     *            DataManager instance.
     */
    public SmartToolJobPool(int corePoolSize, int poolMaxSize,
            DataManager dataMgr) {
        this.dataMgr = dataMgr;
        this.poolMaxSize = poolMaxSize;
        for (int i = 0; i < corePoolSize; i++) {
            Job job = new SmartToolJob(this.dataMgr);
            jobQueue.add(job);
        }
        this.jobList = new CopyOnWriteArrayList<Job>();
    }

    /**
     * Enqueue the specified request into the job pool's request queue. Will be
     * worked by first available job.
     * 
     * @param request
     *            SmartToolRequest containing information on procedure to
     *            execute.
     */
    public void schedule(SmartToolRequest request) {
        // do not schedule while canceling(cancel should be fast).
        synchronized (cancelLock) {
            if (cancel) {
                return;
            }
            // do not schedule while joining, join might be slow but the javaDoc
            // warns others.
            synchronized (joinLock) {
                if (!isJobAvailable()) {
                    Job job = jobQueue.poll();
                    if ((job == null) && (jobList.size() < poolMaxSize)) {
                        job = new SmartToolJob(dataMgr);
                    }
                    if (job != null) {
                        job.schedule();
                        jobList.add(job);
                    }
                }
                workQueue.offer(request);
            }
        }
    }

    private boolean isJobAvailable() {
        for (Job job : jobList) {
            SmartToolJob toolJob = (SmartToolJob) job;
            if (!toolJob.isRunning()) {
                return true;
            }
        }

        return false;
    }

    /**
     * Join on the Jobs in the pool. Attempting to schedule other Jobs will
     * block until join has returned so be careful when calling
     */
    public void join() {
        synchronized (joinLock) {
            for (Job j : jobList) {
                try {
                    j.join();
                } catch (InterruptedException e) {
                    // Ignore interupt
                }
            }
        }
    }

    /**
     * Cancel the job pool, will clear out the workQueue then join on all jobs
     * running. Once canceled all future calls to schedule will be ignored.
     */
    public void cancel() {
        cancel(true);
    }

    /**
     * Cancel the job pool, will clear out the workQueue and optionally join
     * running jobs. Once canceled all future calls to schedule will be ignored.
     * 
     * @param join
     *            true if you want to join before returning.
     */
    public void cancel(boolean join) {
        synchronized (cancelLock) {
            cancel = true;
            workQueue.clear();
            for (Job j : jobList) {
                j.cancel();
            }
        }
        if (join) {
            join();
        }
    }

    /**
     * Cancels the specified request. Returns true if the provided request was
     * waiting to be run but now is not. Returns false if the provided request
     * is already running or if it was not enqueued to begin with.
     * 
     * @param request
     *            The request to cancel.
     * @return True, if the request was in the queue. False, if it was already
     *         being worked by the pool or if it was not in the queue.
     */
    public boolean cancel(SmartToolRequest request) {
        return workQueue.remove(request);
    }

    /**
     * A job pool is considered active if any of the jobs it contains are
     * servicing a request or there is still requests to be worked off in the
     * queue.
     * 
     * @return If any jobs are working off a request or there are requests still
     *         in the work queue.
     */
    public boolean isActive() {
        if (!workQueue.isEmpty()) {
            return true;
        }
        for (Job job : jobList) {
            SmartToolJob toolJob = (SmartToolJob) job;
            if (toolJob.isRunning()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Get the number requests remaining in the queue and the number of jobs in
     * the pool currently working off a request.
     * 
     * @return The number requests remaining in the queue and the number of jobs
     *         in the pool currently working off a request.
     */
    public int[] getWorkRemaining() {
        int jobsRunning = 0;
        for (Job job : jobList) {
            SmartToolJob toolJob = (SmartToolJob) job;
            if (toolJob.isRunning()) {
                jobsRunning++;
            }
        }

        return new int[] { jobsRunning, workQueue.size() };
    }

    protected class SmartToolJob extends Job {

        private final IUFStatusHandler statusHandler = UFStatus
                .getHandler(SmartToolJob.class);

        private SmartToolController python;

        private final DataManager dataMgr;

        private volatile boolean running;

        public SmartToolJob(DataManager dataMgr) {
            super("GFE Smart Tool Job");
            this.dataMgr = dataMgr;
            this.running = false;
            setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                python = SmartToolFactory.buildController(dataMgr);
            } catch (JepException e) {
                jobList.remove(this);
                statusHandler.error("Error initializing procedure python", e);
                return new Status(IStatus.ERROR, StatusConstants.PLUGIN_ID,
                        "Error initializing procedure python", e);
            }

            IStatus statusCode = Status.OK_STATUS;
            try {
                while (!monitor.isCanceled()) {
                    try {
                        SmartToolRequest request = null;
                        try {
                            request = workQueue.poll(
                                    TimeUtil.MILLIS_PER_SECOND,
                                    TimeUnit.MILLISECONDS);
                        } catch (InterruptedException e) {
                            statusCode = Status.CANCEL_STATUS;
                            break;
                        }

                        if (monitor.isCanceled()) {
                            statusCode = Status.CANCEL_STATUS;
                            break;
                        }

                        if (request != null) {
                            running = true;

                            python.processFileUpdates();
                            if (monitor.isCanceled()) {
                                statusCode = Status.CANCEL_STATUS;
                                break;
                            }

                            Object retVal = null;
                            try {
                                execute(python, request, monitor);
                                retVal = null;
                            } catch (Throwable t) {
                                String toolName = request.getPreview()
                                        .getEditAction().getItemName();
                                statusHandler.error("Error running smart tool "
                                        + toolName, t);
                                retVal = t;
                            } finally {
                                if (request.getPreview() != null) {
                                    dataMgr.getEditActionProcessor()
                                            .wrapUpExecute(
                                                    request.getPreview(), true);
                                }
                                request.requestComplete(retVal);
                                running = false;
                            }
                        }
                    } catch (Throwable t) {
                        statusHandler.error(
                                "Unhandled exception in SmartToolJob.", t);
                    }
                }
            } finally {
                if (python != null) {
                    python.dispose();
                    python = null;
                }
            }

            return statusCode;
        }

        /**
         * Executes a smart tool.
         * 
         * @param controller
         * @param request
         * @param monitor
         * @throws SmartToolException
         */
        private void execute(SmartToolController controller,
                SmartToolRequest request, IProgressMonitor monitor)
                throws SmartToolException {
            EditAction ea = request.getPreview().getEditAction();
            String toolName = ea.getItemName();

            Job progressJob = new AsyncProgressJob(toolName, this);
            progressJob.schedule();
            IStatus pjStatus = Status.CANCEL_STATUS;

            try {
                if (request.getOuterLevel()) {
                    dataMgr.getParmOp().clearUndoParmList();
                }
                Tool tool = new Tool(dataMgr.getParmManager(), request
                        .getPreview().getParm(), ea.getItemName(), python);
                tool.execute(ea.getItemName(), request.getPreview().getParm(),
                        ea.getRefSet(), ea.getTimeRange(),
                        request.getVarDict(), ea.getMissingDataMode(), monitor);
                pjStatus = Status.OK_STATUS;
            } catch (SmartToolException e) {
                pjStatus = new Status(IStatus.WARNING, Activator.PLUGIN_ID,
                        "Error in smart tool " + toolName, e);
                throw e;
            } finally {
                controller.garbageCollect();
                progressJob.done(pjStatus);
            }
        }

        public boolean isRunning() {
            return running;
        }
    }
}
