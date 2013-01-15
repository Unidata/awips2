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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
import com.raytheon.uf.viz.core.jobs.AbstractQueueJob;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.jobs.AsyncProgressJob;

/**
 * Job for running GFE procedures. Since JEP/JNI requires that the thread that
 * initialized the python interpreter is the same one that runs it, this job
 * initializes an interpreter for procedures and then sleeps until a request is
 * enqueued.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2009             njensen     Initial creation
 * Jan 8, 2013  1486       dgilling    Support changes to BaseGfePyController.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ProcedureJob extends AbstractQueueJob<ProcedureRequest> {
    /**
     * Maximum number of jobs to keep for a given Data Manager.
     */
    private final static int maxJobs = 3;

    /**
     * Amount of time to keep inactive jobs servers around.
     */
    private final static long expireTime = 5L * 60L * 1000L;

    /**
     * Index of job with the queue. Will break code if not zero.
     */
    private final static int QUEUE_JOB_INDEX = 0;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProcedureJob.class);

    private static Map<DataManager, List<ProcedureJob>> instanceMap = null;

    private ProcedureController python;

    private DataManager dataMgr;

    private ProcedureRequest request;

    protected ProcedureJob(DataManager dataMgr) {
        super("GFE Procedures Job");
        this.dataMgr = dataMgr;
    }

    private void getRequest() throws InterruptedException {
        if (instanceMap == null) {
            request = null;
            return;
        }

        List<ProcedureJob> jobList = instanceMap.get(dataMgr);
        if (jobList == null || jobList.size() == 0
                || jobList.get(QUEUE_JOB_INDEX).queue == null) {
            request = null;
        } else {
            request = jobList.get(QUEUE_JOB_INDEX).queue.poll(1000L,
                    TimeUnit.MILLISECONDS);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        long starTime = System.currentTimeMillis();
        boolean expireJob = instanceMap.get(dataMgr).get(QUEUE_JOB_INDEX) != this;
        try {
            python = ProcedureFactory.buildController(dataMgr);
        } catch (JepException e) {
            ProcedureJob.removeJob(dataMgr, this);
            return new Status(IStatus.ERROR, StatusConstants.PLUGIN_ID,
                    "Error initializing guidance python", e);
        }

        try {
            while (monitor.isCanceled() == false) {
                // ProcedureRequest request;
                try {
                    getRequest();
                } catch (InterruptedException e) {
                    continue;
                }
                // May have been canceled while waiting.
                if (monitor.isCanceled()) {
                    break;
                }
                synchronized (this) {
                    try {
                        if (request != null) {
                            python.processFileUpdates();
                            processRequest(request);
                            if (request != null) {
                                request.requestComplete(null);
                            }
                        } else if (expireJob
                                && ((instanceMap.get(dataMgr).size() > maxJobs) || (System
                                        .currentTimeMillis() - starTime) > expireTime)) {
                            ProcedureJob.removeJob(dataMgr, this);
                            break;
                        }
                    } catch (Throwable t) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error running tool ", t);
                        if (request != null) {
                            request.requestComplete(t);
                        }
                    } finally {
                        request = null;
                    }
                }
            }
        } finally {
            if (python != null) {
                python.dispose();
                python = null;
            }
        }

        return Status.OK_STATUS;
    }

    /**
     * Remove a job from the Data Manger's job list.
     * 
     * @param dataMgr
     *            - The job's data manager
     * @param job
     *            - The job to remove
     */
    private static synchronized void removeJob(DataManager dataMgr,
            ProcedureJob job) {
        if (instanceMap == null) {
            return;
        }

        List<ProcedureJob> jobList = instanceMap.get(dataMgr);

        if (jobList != null) {
            jobList.remove(job);

            // Removing job with queue remove job list so next request will set
            // up new queue.
            if (job.queue != null) {
                jobList.clear();
                instanceMap.remove(dataMgr);
            }
        }
    }

    public void processRequest(ProcedureRequest request) {
        this.execute(python, request.getProcedureName(), request.getRefSet(),
                request.getTimeRange(), request.getVarDict());
        this.dataMgr.getEditActionProcessor().wrapUpExecute(
                request.getPreview(), false);
    }

    /**
     * This manages the scheduling of jobs to service a Data Manger's requests.
     * 
     * @param dataMgr
     *            - Data Manger for the request
     * @param request
     *            - The request to service
     * @return state - true when job available to process request otherwise
     *         false and request is queued to wait for next available job
     */
    public static synchronized boolean enqueue(DataManager dataMgr,
            ProcedureRequest request) {
        if (instanceMap == null) {
            instanceMap = new HashMap<DataManager, List<ProcedureJob>>();
        }

        List<ProcedureJob> jobList = instanceMap.get(dataMgr);
        if (jobList == null) {
            jobList = new ArrayList<ProcedureJob>();
            // Add the first job which contains the queue used by all jobs in
            // the list.
            ProcedureJob job = new ProcedureJob(dataMgr);
            jobList.add(job);
            instanceMap.put(dataMgr, jobList);
            job.setSystem(true);
            job.schedule();
        }
        boolean jobAvailable = false;
        for (ProcedureJob job : jobList) {
            if (job.request == null) {
                jobAvailable = true;
                break;
            }
        }

        // All jobs for data manager are busy add another.
        // To mimic AWIPS I allow any number of jobs.
        // The check in the run will reduce the number to maxJobs.
        if (jobAvailable == false) {
            ProcedureJob job = new ProcedureJob(dataMgr);
            job.setSystem(true);
            jobList.add(job);
            // Never used additional job's queue
            job.queue = null;
            job.schedule();
            jobAvailable = true;
        }

        jobList.get(QUEUE_JOB_INDEX).enqueue(request);
        return jobAvailable;
    }

    /**
     * This returns an array of two integers the first is the number of
     * Procedure Tool Jobs being processed and the second is the number in the
     * queue waiting to be processed.
     * 
     * @return cnts
     */
    public static int[] getJobCount() {
        int[] cnt = new int[] { 0, 0 };
        if (instanceMap != null) {
            for (List<ProcedureJob> jobList : instanceMap.values()) {
                cnt[1] += jobList.get(QUEUE_JOB_INDEX).queue.size();
                for (ProcedureJob job : jobList) {
                    if (job.request != null) {
                        ++cnt[0];
                    }
                }
            }
        }
        return cnt;
    }

    /**
     * Determine if there are any Procedure Tool Jobs queued and/or being
     * processed.
     * 
     * @return true when there are job(s)s queued or being processed otherwise
     *         false
     */
    public static boolean haveJobs() {
        boolean result = false;

        if (instanceMap != null) {
            for (List<ProcedureJob> jobList : instanceMap.values()) {
                // Any pending requests.
                if (jobList.get(QUEUE_JOB_INDEX).queue.size() > 0) {
                    result = true;
                    break;
                }

                // Any requests being processed.
                for (ProcedureJob job : jobList) {
                    if (job.request != null) {
                        result = true;
                        break;
                    }
                }
            }
        }
        return result;
    }

    /**
     * This terminates all the Data Managers' jobs.
     */
    public static synchronized void shutdown() {
        // TODO This currently joins with a job waiting for it to finish which
        // can take a long time and may even be waiting for user to input. Must
        // find a wait to kill any GUI associated with a request and if python
        // running a way to terminate it so no waiting is involved.
        if (instanceMap != null) {
            for (List<ProcedureJob> jobList : instanceMap.values()) {
                jobList.get(QUEUE_JOB_INDEX).queue.clear();

                // Do in reverse order so last job cancel is the one with the
                // queue.
                for (int index = jobList.size() - 1; index >= 0; --index) {
                    jobList.get(index).cancel();
                }
            }

            for (List<ProcedureJob> jobList : instanceMap.values()) {
                for (ProcedureJob job : jobList) {
                    synchronized (job) {
                        try {
                            if (job.getState() != Job.NONE) {
                                job.join();
                            }
                        } catch (InterruptedException ex) {
                            System.err.println("here SmartToolJob");
                        }
                    }
                }
            }

            for (List<ProcedureJob> jobList : instanceMap.values()) {
                jobList.clear();
            }

            instanceMap.clear();
            instanceMap = null;
        }
    }

    /**
     * Executes a procedure
     * 
     * @param procedureName
     *            the name of the procedure
     * @param refSet
     *            the edit area to run the procedure against
     * @param timeRange
     *            the time range to run the procedure against
     * @param varDict
     *            the cached varDict for the procedure, or null if there is none
     *            (should be null unless called from within another procedure)
     */
    private void execute(ProcedureController controller, String procedureName,
            ReferenceData refSet, TimeRange timeRange, String varDict) {

        Job progressJob = new AsyncProgressJob(procedureName, this);
        IStatus pjStatus = Status.CANCEL_STATUS;
        try {
            List<String> argNames = controller.getMethodArguments(
                    procedureName, "execute");
            Map<String, Object> argMap = getArgValues(argNames, refSet,
                    timeRange);
            controller.setVarDict(varDict);
            progressJob.schedule();
            controller.executeProcedure(procedureName, argMap);
            pjStatus = Status.OK_STATUS;
        } catch (Exception e) {
            pjStatus = new Status(IStatus.WARNING, Activator.PLUGIN_ID,
                    "Error in procedure " + procedureName, e);
            statusHandler.handle(Priority.PROBLEM, "Error executing procedure "
                    + procedureName, e);
        } catch (JepException e) {
            pjStatus = new Status(IStatus.WARNING, Activator.PLUGIN_ID,
                    "Error in procedure " + procedureName, e);
            statusHandler.handle(Priority.PROBLEM, "Error executing procedure "
                    + procedureName, e);
        } finally {
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

}
