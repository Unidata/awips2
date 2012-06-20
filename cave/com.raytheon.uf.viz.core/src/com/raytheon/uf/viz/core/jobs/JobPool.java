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
package com.raytheon.uf.viz.core.jobs;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

/**
 * 
 * Create a specific number of Eclipse Jobs to handle tasks. This can be useful
 * if you have dozens or hundreds of tasks that each take a short time. Creating
 * a job for each task can result in more threads than is useful. If you instead
 * use a JobPool it reduces the number of threads by limiting the number of
 * eclipse jobs tBhat are created. For many tasks a JobPool may perform faster
 * than using eclipse Jobs directly because thread creation and context
 * switching are reduced.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class JobPool {

    protected LinkedBlockingQueue<Runnable> workQueue = new LinkedBlockingQueue<Runnable>();

    protected LinkedBlockingQueue<Job> jobQueue = new LinkedBlockingQueue<Job>();

    protected List<Job> jobList;

    public JobPool(String name, int size) {
        this(name, size, null, null);
    }

    public JobPool(String name, int size, Boolean system) {
        this(name, size, system, null);
    }

    public JobPool(String name, int size, Boolean system, Integer priority) {
        jobList = new ArrayList<Job>(size);
        for (int i = 0; i < size; i++) {
            PooledJob job = new PooledJob(name);
            if (system != null) {
                job.setSystem(system);
            }
            if (priority != null) {
                job.setPriority(priority);
            }
            jobList.add(job);
            jobQueue.add(job);
        }
    }

    public synchronized void schedule(Runnable runnable) {
        workQueue.offer(runnable);
        Job job = jobQueue.poll();
        if (job != null) {
            job.schedule();
        }
    }

    /**
     * Join on the Runnables in the pool. Attempting to schedule other Runnables
     * will block until join as returned so be careful when calling
     */
    public synchronized void join() {
        for (Job j : jobList) {
            try {
                j.join();
            } catch (InterruptedException e) {
                // Ignore interupt
            }
        }
    }

    /**
     * Cancel the job pool, will clear out the workQueue then join on all jobs
     * running
     */
    public synchronized void cancel() {
        workQueue.clear();
        join();
    }

    /**
     * Cancels the specified runnable. Returns true if the provided runnable was
     * waiting to be run but now is now. Returns false if the provided runnable
     * is already running or if it was not enqueued to begin with.
     * 
     * @param runnable
     * @return
     */
    public synchronized boolean cancel(Runnable runnable) {
        return workQueue.remove(runnable);
    }

    /**
     * A JobPool is considered active if any of the jobs it contains are running
     * or waiting to be run. When all scheduled work is run
     * 
     * @return
     */
    public boolean isActive() {
        if (!workQueue.isEmpty()) {
            return true;
        }
        for (Job job : jobList) {
            int state = job.getState();
            if (state == Job.RUNNING || state == Job.WAITING) {
                return true;
            }
        }
        return false;
    }

    /**
     * get the number of tasks(Runnables) that are waiting to be run. This does
     * not include tasks that are currently running so even if there are no
     * waiting tasks the pool may still be active.
     * 
     * @return
     */
    public int getWorkRemaining() {
        return workQueue.size();
    }

    protected class PooledJob extends Job {

        public PooledJob(String name) {
            super(name);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            boolean done = false;
            try {
                // immediatly offer myself up as available. This may result in
                // double scheduling, but we never miss any work.
                jobQueue.offer(this);
                Runnable runnable = null;
                while ((runnable = workQueue.poll()) != null) {
                    runnable.run();
                }
                done = true;
            } finally {
                if (!done) {
                    // if this didn't finish then an error occured, let the
                    // error go, but schedule this job again to finish its
                    // work.
                    this.schedule();
                }
            }
            return Status.OK_STATUS;
        }

    }

}
