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
package com.raytheon.viz.gfe.procedures;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import jep.JepException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.gfe.StatusConstants;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.jobs.AsyncProgressJob;

/**
 * Job pool for running GFE procedures. Since JEP/JNI requires that the thread
 * that initialized the python interpreter is the same one that runs it, this
 * pool initializes an interpreter for procedures and then sleeps until a
 * request is enqueued.
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

public class ProcedureJobPool {

    protected LinkedBlockingQueue<ProcedureRequest> workQueue = new LinkedBlockingQueue<ProcedureRequest>();

    protected LinkedBlockingQueue<Job> jobQueue = new LinkedBlockingQueue<Job>();

    protected List<Job> jobList;

    protected boolean cancel = false;

    protected Object cancelLock = new Object();

    protected Object joinLock = new Object();

    private final DataManager dataMgr;

    private final int poolMaxSize;

    /**
     * Creates a new ProcedureJobPool with the specified size parameters.
     * 
     * @param corePoolSize
     *            The minimum size of the job pool--will always have at least
     *            this many Jobs ready to execute.
     * @param poolMaxSize
     *            The maximum size of the job pool.
     * @param dataMgr
     *            DataManager instance.
     */
    public ProcedureJobPool(int corePoolSize, int poolMaxSize,
            DataManager dataMgr) {
        this.dataMgr = dataMgr;
        this.poolMaxSize = poolMaxSize;
        for (int i = 0; i < corePoolSize; i++) {
            Job job = new ProcedureJob(this.dataMgr);
            jobQueue.add(job);
        }
        this.jobList = new CopyOnWriteArrayList<Job>();
    }

    /**
     * Enqueue the specified request into the job pool's request queue. Will be
     * worked by first available job. If calling from an existing thread in the
     * job pool, that thread will be reused to execute the request.
     * 
     * @param request
     *            ProcedureRequest containing information on procedure to
     *            execute.
     */
    public void schedule(ProcedureRequest request) {
        ProcedureJob reuseJob = null;

        // do not schedule while canceling(cancel should be fast).
        synchronized (cancelLock) {
            if (cancel) {
                return;
            }
            // do not schedule while joining, join might be slow but the javaDoc
            // warns others.
            synchronized (joinLock) {
                boolean jobAvailable = false;
                Thread currentThread = Thread.currentThread();
                for (Job job : jobList) {
                    Thread jobThread = job.getThread();
                    ProcedureJob procJob = (ProcedureJob) job;
                    if (currentThread == jobThread) {
                        // this occurs when a running procedure uses
                        // SmartScript.callProcedure()
                        // for efficiency we want to just stay on this thread
                        reuseJob = procJob;
                        jobAvailable = true;
                        break;
                    } else if (!procJob.isRunning()) {
                        jobAvailable = true;
                    }
                }

                if (reuseJob == null) {
                    if (!jobAvailable) {
                        Job job = jobQueue.poll();
                        if ((job == null) && (jobList.size() < poolMaxSize)) {
                            job = new ProcedureJob(dataMgr);
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

        if (reuseJob != null) {
            reuseJob.processRequest(request);
        }
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
    public boolean cancel(ProcedureRequest request) {
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
            ProcedureJob procJob = (ProcedureJob) job;
            if (procJob.isRunning()) {
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
            ProcedureJob procJob = (ProcedureJob) job;
            if (procJob.isRunning()) {
                jobsRunning++;
            }
        }

        return new int[] { jobsRunning, workQueue.size() };
    }

    protected class ProcedureJob extends Job {

        private final IUFStatusHandler statusHandler = UFStatus
                .getHandler(ProcedureJob.class);

        private ProcedureController python;

        private final DataManager dataMgr;

        private volatile boolean running;

        public ProcedureJob(DataManager dataMgr) {
            super("GFE Procedures Job");
            this.dataMgr = dataMgr;
            this.running = false;
            setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                python = ProcedureFactory.buildController(dataMgr);
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
                        ProcedureRequest request = null;
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

                            processRequest(request);
                            running = false;
                        }
                    } catch (Throwable t) {
                        statusHandler.error(
                                "Unhandled exception in ProcedureJob.", t);
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

        protected void processRequest(ProcedureRequest request) {
            Object retVal = null;
            try {
                execute(python, request);
                retVal = null;
            } catch (Throwable t) {
                statusHandler
                        .handle(Priority.PROBLEM, "Error running procedure "
                                + request.getProcedureName(), t);
                retVal = t;
            } finally {
                dataMgr.getEditActionProcessor().wrapUpExecute(
                        request.getPreview(), false);
                request.requestComplete(retVal);
            }
        }

        /**
         * Executes a procedure
         * 
         * @param procedureName
         *            the name of the procedure
         * @param request
         *            the request containing data on the procedure to run.
         * @throws Exception
         * @throws JepException
         */
        private void execute(ProcedureController controller,
                ProcedureRequest request) throws Exception, JepException {
            String procedureName = request.getProcedureName();
            Job progressJob = new AsyncProgressJob(procedureName, this);
            IStatus pjStatus = Status.CANCEL_STATUS;
            progressJob.schedule();

            try {
                List<String> argNames = controller.getMethodArguments(
                        procedureName, "execute");
                Map<String, Object> argMap = getArgValues(argNames,
                        request.getRefSet(), request.getTimeRange());
                controller.setVarDict(request.getVarDict());
                controller.executeProcedure(procedureName, argMap);
                pjStatus = Status.OK_STATUS;
            } catch (Exception e) {
                pjStatus = new Status(IStatus.WARNING, Activator.PLUGIN_ID,
                        "Error in procedure " + procedureName, e);
                throw e;
            } catch (JepException e) {
                pjStatus = new Status(IStatus.WARNING, Activator.PLUGIN_ID,
                        "Error in procedure " + procedureName, e);
                throw e;
            } finally {
                controller.garbageCollect();
                progressJob.done(pjStatus);
            }
        }

        /**
         * Maps a procedure's execute's argument name to an object
         * 
         * @param args
         *            the name of the objects
         * @param refSet
         *            the edit area to run the procedure on
         * @param timeRange
         *            the time range to run the procedure on
         * @return a map of argument names to objects
         * @throws GFEException
         */
        private Map<String, Object> getArgValues(List<String> args,
                ReferenceData refSet, TimeRange timeRange) throws GFEException {
            Map<String, Object> argValueMap = new HashMap<String, Object>();
            // For each argument in args, append a value to the argValueList
            for (String arg : args) {
                if (arg.equals("varDict")) {
                    argValueMap.put("varDict", null);
                } else if (arg.equals("editArea")) {
                    argValueMap.put("editArea", refSet);
                } else if (arg.equals("timeRange")) {
                    argValueMap.put("timeRange", timeRange);
                } else if (arg.equals("self")) {
                    // skip
                } else {
                    throw new GFEException("Unknown argument " + arg);
                }

            }
            return argValueMap;
        }

        public boolean isRunning() {
            return running;
        }
    }
}
