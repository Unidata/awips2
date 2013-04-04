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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.jobs.AbstractQueueJob;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.jobs.AsyncProgressJob;
import com.raytheon.viz.gfe.smarttool.EditAction;
import com.raytheon.viz.gfe.smarttool.SmartToolException;
import com.raytheon.viz.gfe.smarttool.Tool;

/**
 * Job for running smart tools off the UI thread
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010            njensen     Initial creation
 * Jan 18, 2013    1509  njensen  Garbage collect after running tool
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolJob extends AbstractQueueJob<SmartToolRequest> {

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
            .getHandler(SmartToolJob.class);

    private static Map<DataManager, List<SmartToolJob>> instanceMap = null;

    private DataManager dataMgr;

    /**
     * The request being processed.
     */
    private SmartToolRequest request = null;

    protected SmartToolJob(DataManager dataMgr) {
        super("GFE Smart Tool Job");
        this.dataMgr = dataMgr;
    }

    private void getRequest() throws InterruptedException {
        if (instanceMap == null) {
            request = null;
            return;
        }

        List<SmartToolJob> jobList = instanceMap.get(dataMgr);
        if (jobList == null || jobList.size() == 0
                || jobList.get(QUEUE_JOB_INDEX).queue == null) {
            request = null;
        } else {
            request = jobList.get(QUEUE_JOB_INDEX).queue.poll(1000L,
                    TimeUnit.MILLISECONDS);
        }
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        SmartToolController python = null;
        long starTime = System.currentTimeMillis();
        boolean expireJob = instanceMap.get(dataMgr).get(QUEUE_JOB_INDEX) != this;
        try {
            python = SmartToolFactory.buildController(dataMgr);
        } catch (JepException e) {
            SmartToolJob.removeJob(dataMgr, this);
            return new Status(IStatus.ERROR, StatusConstants.PLUGIN_ID,
                    "Error initializing guidance python", e);
        }

        try {
            // Used req to wrap up request after leaving the synchronized
            // region.
            SmartToolRequest req = null;
            while (monitor.isCanceled() == false) {
                try {
                    getRequest();

                    // May have been canceled while waiting.
                    if (monitor.isCanceled()) {
                        break;
                    }

                    synchronized (this) {
                        if (request != null) {
                            starTime = System.currentTimeMillis();
                            python.processFileUpdates();
                            EditAction ea = request.getPreview()
                                    .getEditAction();
                            Job progressJob = new AsyncProgressJob(
                                    ea.getItemName(), this);
                            progressJob.schedule();
                            IStatus pjResult = Status.CANCEL_STATUS;
                            try {
                                Tool tool = new Tool(dataMgr.getParmManager(),
                                        request.getPreview().getParm(),
                                        ea.getItemName(), python);
                                tool.execute(ea.getItemName(), request
                                        .getPreview().getParm(),
                                        ea.getRefSet(), ea.getTimeRange(),
                                        request.getVarDict(), ea
                                                .getMissingDataMode(), monitor);
                                request.requestComplete(null);
                                pjResult = Status.OK_STATUS;

                            } catch (SmartToolException e) {
                                pjResult = new Status(IStatus.WARNING,
                                        Activator.PLUGIN_ID,
                                        "Error in smart tool", e);
                                throw e;
                            } finally {
                                python.garbageCollect();
                                progressJob.done(pjResult);
                                req = request;
                                request = null;
                            }
                        } else if (expireJob
                                && ((instanceMap.get(dataMgr).size() > maxJobs) || (System
                                        .currentTimeMillis() - starTime) > expireTime)) {
                            SmartToolJob.removeJob(dataMgr, this);
                            break;
                        }
                    }
                } catch (InterruptedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Smart tool thread interrupted", e);
                    break;
                } catch (SmartToolException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error running tool ", e);
                    if (req != null) {
                        req.requestComplete(e);
                    }
                } catch (Throwable t) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error running tool ", t);
                    if (req != null) {
                        req.requestComplete(t);
                    }
                } finally {
                    if (req != null && req.getPreview() != null) {
                        this.dataMgr.getEditActionProcessor().wrapUpExecute(
                                req.getPreview(), true);
                    }
                    req = null;
                }
            }
            System.err.println("SmartToolJob exit loop: "
                    + monitor.isCanceled());
        } finally {
            System.err.println("shutdown instance of SmartToolJob");
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
            SmartToolJob job) {
        if (instanceMap == null) {
            return;
        }

        List<SmartToolJob> jobList = instanceMap.get(dataMgr);

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
            SmartToolRequest request) {
        if (instanceMap == null) {
            instanceMap = new HashMap<DataManager, List<SmartToolJob>>();
        }

        List<SmartToolJob> jobList = instanceMap.get(dataMgr);
        if (jobList == null) {
            jobList = new ArrayList<SmartToolJob>();
            // Add the first job which contains the queue used by all jobs in
            // the list.
            SmartToolJob job = new SmartToolJob(dataMgr);
            jobList.add(job);
            instanceMap.put(dataMgr, jobList);
            job.setSystem(true);
            job.schedule();
        }
        boolean jobAvailable = false;
        for (SmartToolJob job : jobList) {
            if (job.request == null) {
                jobAvailable = true;
                break;
            }
        }

        // All jobs for data manager are busy add another.
        // To mimic AWIPS I allow any number of jobs.
        // The check in the run will reduce the number to maxJobs.
        if (jobAvailable == false) {
            SmartToolJob job = new SmartToolJob(dataMgr);
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
     * This returns an array of two integers the first is the number of Smart
     * Tool Jobs being processed and the second is the number in the queue
     * waiting to be processed.
     * 
     * @return cnts
     */
    public static int[] getJobCount() {
        int[] cnt = new int[] { 0, 0 };
        if (instanceMap != null) {
            for (List<SmartToolJob> jobList : instanceMap.values()) {
                cnt[1] += jobList.get(QUEUE_JOB_INDEX).queue.size();
                for (SmartToolJob job : jobList) {
                    if (job.request != null) {
                        ++cnt[0];
                    }
                }
            }
        }
        return cnt;
    }

    /**
     * Determine if there are any Smart Tool Jobs queued and/or being processed.
     * 
     * @return true when there are job(s)s queued or being processed otherwise
     *         false
     */
    public static boolean haveJobs() {
        boolean result = false;

        if (instanceMap != null) {
            for (List<SmartToolJob> jobList : instanceMap.values()) {
                // Any pending requests.
                if (jobList.get(QUEUE_JOB_INDEX).queue.size() > 0) {
                    result = true;
                    break;
                }

                // Any requests being processed.
                for (SmartToolJob job : jobList) {
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
        // can take a long time and may even be waiting for user input. Must
        // find a wait to kill any GUI associated with a request and if python
        // running a way to terminate it so no waiting is involved.
        if (instanceMap != null) {
            for (List<SmartToolJob> jobList : instanceMap.values()) {
                jobList.get(QUEUE_JOB_INDEX).queue.clear();

                // Do in reverse order so last job cancel is the one with the
                // queue.
                for (int index = jobList.size() - 1; index >= 0; --index) {
                    jobList.get(index).cancel();
                }
            }

            for (List<SmartToolJob> jobList : instanceMap.values()) {
                for (SmartToolJob job : jobList) {
                    synchronized (job) {
                        try {
                            if (job.getState() != Job.NONE) {
                                job.join();
                            }
                        } catch (InterruptedException ex) {
                            // System.err.println("here SmartToolJob");
                        }
                    }
                }
            }

            for (List<SmartToolJob> jobList : instanceMap.values()) {
                jobList.clear();
            }

            instanceMap.clear();
            instanceMap = null;
        }
    }
}
