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
package com.raytheon.viz.aviation.monitor;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;

import jep.JepException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.jobs.AbstractQueueJob;
import com.raytheon.uf.viz.core.jobs.IRequestCompleteListener;
import com.raytheon.viz.aviation.activator.Activator;

/**
 * A job that performs the monitoring/rules compare within python. A FIFO queue
 * is used so only one monitor request is evaluated at a time. While it is a
 * singleton, the job will finish and be disposed if stop() is called, with a
 * new one created when it is next requested.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2009            njensen     Initial creation
 * Sep 27,2010  6185      rferrel     Allow multiple jobs off a static queue.
 * May 13,2011  8611      rferrel     Added request's type to the results
 * Mar 06, 2013 1735      rferrel     Have python reference proper class.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PythonMonitorJob extends AbstractQueueJob<MonitorRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PythonMonitorJob.class);

    private static final int JOB_CNT = 3;

    private static LinkedBlockingQueue<MonitorRequest> staticQueue;

    private static PythonMonitorJob[] jobs;

    private PythonScript python;

    private boolean stop = false;

    private PythonMonitorJob(String name) {
        super(name);
    }

    private void initializePython() throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        File runner = pathMgr.getStaticFile("aviation/python/MonitorEntry.py");
        String filePath = runner.getPath();
        String includePath = PyUtil.buildJepIncludePath(runner.getParentFile()
                .getPath(), AvnPyUtil.getLoggingHandlerDir(), AvnPyUtil
                .getPointDataDir(), AvnPyUtil.getCommonPythonDir());
        python = new PythonScript(filePath, includePath,
                PythonMonitorJob.class.getClassLoader());
    }

    /**
     * Allows jobs to get request off the static queue.
     * 
     * @return
     */
    private static MonitorRequest pollRequest() {
        try {
            return staticQueue.poll();
        } catch (NullPointerException ex) {
            // Do nothing
        }
        return null;
    }

    /**
     * Places request on static queue.
     * 
     * @param req
     * @return true if request is accepted.
     */
    public static boolean offerRequest(MonitorRequest req) {
        try {
            return staticQueue.offer(req);
        } catch (NullPointerException ex) {
            // do nothing
        }
        return false;
    }

    /**
     * Start up the desired number of jobs to monitor the static queue.
     */
    synchronized public static void startJobs() {
        jobs = new PythonMonitorJob[JOB_CNT];
        for (int i = 0; i < JOB_CNT; ++i) {
            PythonMonitorJob job = new PythonMonitorJob(
                    "AvnPFS Python Monitor - " + i);
            job.setSystem(true);
            job.schedule();
            jobs[i] = job;
            if (i == JOB_CNT - 1) {
                staticQueue = jobs[i].queue;
            }
            jobs[i].queue = null;
        }
    }

    /**
     * Terminates the jobs monitoring the static queue.
     */
    synchronized public static void stopJobs() {
        staticQueue = null;
        if (jobs != null) {
            for (int i = 0; i < JOB_CNT; i++) {
                if (jobs[i] != null) {
                    jobs[i].stop = true;
                    jobs[i] = null;
                }
            }
            jobs = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @SuppressWarnings("unchecked")
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        try {
            initializePython();
        } catch (JepException e) {
            return new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                    "Error initializing guidance python", e);
        }

        try {
            while (!stop) {
                MonitorRequest request = PythonMonitorJob.pollRequest();
                if (request != null) {
                    final IRequestCompleteListener<Map<?, ?>> listener = request
                            .getListener();
                    Map<String, Object> args = new HashMap<String, Object>();
                    args.put("request", request);
                    try {
                        final Map<String, Object> result = (Map<String, Object>) python
                                .execute("monitor", args);
                        result.put("type", request.getType());
                        VizApp.runAsync(new Runnable() {
                            @Override
                            public void run() {
                                listener.requestComplete(result);
                            }
                        });
                    } catch (JepException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error monitoring", e);
                    }
                } else {
                    try {
                        Thread.sleep(20);
                    } catch (InterruptedException e) {
                        break;
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
}
