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
package com.raytheon.viz.gfe.core.parm.vcparm;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;

import jep.JepException;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2012            dgilling     Initial creation
 * Mar 12, 2015  #4246     randerso     Changes to support VCModules at base, site, and user levels
 * Jun 03, 2019  7852      dgilling     Update code for jep 3.8.
 *
 * </pre>
 *
 * @author dgilling
 * @version 1.0
 */

public class VCModuleJobPool {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VCModuleJobPool.class);

    protected LinkedBlockingQueue<VCModuleRequest> workQueue = new LinkedBlockingQueue<>();

    protected List<Job> jobList;

    protected boolean cancel = false;

    protected Object cancelLock = new Object();

    protected Object joinLock = new Object();

    public VCModuleJobPool(String name, DataManager dataMgr, int size) {
        this(name, dataMgr, size, null, null);
    }

    public VCModuleJobPool(String name, DataManager dataMgr, int size,
            Boolean system) {
        this(name, dataMgr, size, system, null);
    }

    public VCModuleJobPool(String name, DataManager dataMgr, int size,
            Boolean system, Integer priority) {
        jobList = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            PooledJob job = new PooledJob(name, dataMgr);
            if (system != null) {
                job.setSystem(system);
            }
            if (priority != null) {
                job.setPriority(priority);
            }
            jobList.add(job);
            job.schedule();
        }
    }

    public void enqueue(VCModuleRequest request) {
        // do not schedule while canceling(cancel should be fast.
        synchronized (cancelLock) {
            if (cancel) {
                return;
            }
            // do not schedule while joining, join might be slow but the
            // javaDoc warns others.
            synchronized (joinLock) {
                workQueue.offer(request);
            }
        }
    }

    /**
     * Join on the <code>Job</code>s in the pool. Attempting to schedule other
     * <code>Job</code>s will block until join as returned so be careful when
     * calling
     */
    public void join() {
        synchronized (joinLock) {
            for (Job j : jobList) {
                try {
                    j.join();
                } catch (InterruptedException e) {
                    // Ignore interrupt
                }
            }
        }
    }

    /**
     * Cancel the job pool, will clear out the workQueue then join on all jobs
     * running
     */
    public void cancel() {
        synchronized (cancelLock) {
            cancel = true;
            workQueue.clear();
        }
        for (Job job : jobList) {
            job.cancel();
        }
        join();
    }

    /**
     * Cancels the specified request. Returns true if the provided runnable was
     * waiting to be run but now is now. Returns false if the provided runnable
     * is already running or if it was not enqueued to begin with.
     *
     * @param request
     * @return
     */
    public boolean cancel(VCModuleRequest request) {
        return workQueue.remove(request);
    }

    protected class PooledJob extends Job {

        protected DataManager dataMgr;

        protected VCModuleController python;

        public PooledJob(String name, DataManager dataMgr) {
            super(name);
            this.dataMgr = dataMgr;
            this.python = null;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                if (python == null) {
                    python = VCModuleControllerFactory.buildInstance(dataMgr);
                }

                while (!monitor.isCanceled()) {
                    try {
                        VCModuleRequest request = workQueue.poll(1L,
                                TimeUnit.SECONDS);
                        if (request != null) {
                            processRequest(request);
                        }
                    } catch (InterruptedException e) {
                        // ignore, but log
                        statusHandler.handle(
                                Priority.DEBUG,
                                "VCModuleJobPool received interrupt: "
                                        + e.getLocalizedMessage(), e);
                    }
                }
            } catch (JepException e) {
                statusHandler.handle(
                        Priority.WARN,
                        "Could not instantiate VCModuleController: "
                                + e.getLocalizedMessage(), e);
            } finally {
                if (python != null) {
                    try {
                        python.dispose();
                    } catch (JepException e) {
                        statusHandler
                                .debug("Failed to dispose script instance.", e);
                    }
                }
            }

            return Status.CANCEL_STATUS;
        }

        protected void processRequest(VCModuleRequest request) {
            Object result = null;

            try {
                if ("getMethodArgs".equals(request.getMethodName())) {
                    result = python.getMethodArguments(
                            request.getModuleName(),
                            (String) request.getArgMap().get(
                                    PyConstants.METHOD_NAME));
                } else {
                    result = python.executeMethod(request.getModuleName(),
                            request.getMethodName(), request.getJepArgs(),
                            request.getType());
                }
            } catch (Throwable t) {
                statusHandler.handle(
                        Priority.DEBUG,
                        "Exception thrown in VCModule's python.execute(): "
                                + t.getLocalizedMessage(), t);
                result = t;
            }

            request.setResult(result);
        }
    }
}
