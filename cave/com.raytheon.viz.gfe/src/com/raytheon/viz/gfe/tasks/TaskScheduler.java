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
package com.raytheon.viz.gfe.tasks;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.gfe.tasks.AbstractGfeTask.TaskStatus;

/**
 * GFE Task Scheduler
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 07, 2011            randerso    Initial creation
 * Mar 03, 2012  #346      dgilling    Use identity-based ListenerLists.
 * May 28, 2014  #2841     randerso    Made scheduler generic so multiple instances
 *                                     can be created to process different script types
 * Aug 20, 2015  #4749     dgilling    Add shutdown().
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TaskScheduler<TaskType extends AbstractGfeTask> extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TaskScheduler.class);

    private List<AbstractGfeTask> allTasks;

    private BlockingQueue<AbstractGfeTask> pendingTasks;

    private BlockingQueue<AbstractGfeTask> finishedTasks;

    private int pendingTaskLimit;

    private int runningTaskLimit;

    private int runningTasks;

    private ListenerList listeners;

    protected TaskScheduler(int pendingTaskLimit, int runningTaskLimit,
            int finishedTaskLimit) {
        super("Task Manager Job");
        this.runningTaskLimit = runningTaskLimit;
        this.pendingTaskLimit = pendingTaskLimit;

        listeners = new ListenerList(ListenerList.IDENTITY);

        pendingTasks = new ArrayBlockingQueue<AbstractGfeTask>(pendingTaskLimit);
        runningTasks = 0;
        finishedTasks = new ArrayBlockingQueue<AbstractGfeTask>(
                finishedTaskLimit);
        allTasks = new ArrayList<AbstractGfeTask>(pendingTaskLimit
                + runningTaskLimit + finishedTaskLimit);

    }

    protected void queueTask(TaskType task) {
        task.setScheduler(this);
        try {
            pendingTasks.add(task);
            allTasks.add(task);
            fireTaskStatusChanged(task);
        } catch (IllegalStateException e) {
            task.cancel();
            statusHandler
                    .error(String
                            .format("Unable to queue job: %s. Pending task limit (%d) exceeded.",
                                    task.getDisplayName(), pendingTaskLimit));
        }
        this.schedule();
    }

    protected synchronized void forceRunTask(TaskType task) {
        if (pendingTasks.remove(task)) {
            runTask(task);
        }
    }

    protected synchronized void cancelTask(TaskType task) {
        // remove from pending list in case it's still pending
        // will do nothing if it's not in the pending list anymore
        pendingTasks.remove(task);
        task.cancel();
    }

    private synchronized void runTask(AbstractGfeTask task) {
        // System.out.println("runTask");
        runningTasks++;
        task.start();

        this.schedule();
        fireTaskStatusChanged(task);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        // System.out.println("pending: " + pendingTasks.size() + " running: "
        // + runningTasks);
        while ((runningTasks < runningTaskLimit) && (pendingTasks.size() > 0)) {
            AbstractGfeTask task = pendingTasks.poll();
            runTask(task);
        }

        return Status.OK_STATUS;
    }

    protected synchronized void taskCompleted(TaskType task) {
        // System.out.println("taskCompleted");
        runningTasks--;
        finishTask(task);
    }

    protected synchronized void taskCanceled(TaskType task) {
        task.status = TaskStatus.CANCELED;
        // System.out.println("taskCompleted");
        finishTask(task);
    }

    private void finishTask(TaskType task) {
        if (finishedTasks.remainingCapacity() == 0) {
            AbstractGfeTask t = finishedTasks.poll();
            allTasks.remove(t);
            File logFile = t.getLogFile();
            if ((logFile != null) && logFile.exists()) {
                logFile.delete();
            }
        }
        task.cleanUp();
        finishedTasks.add(task);

        this.schedule();
        fireTaskStatusChanged(task);
    }

    private void fireTaskStatusChanged(AbstractGfeTask task) {
        for (Object listener : listeners.getListeners()) {
            ((ITaskStatusChangedListener) listener).taskStatusChanged(task);
        }
    }

    protected List<AbstractGfeTask> getTaskList() {
        return allTasks;
    }

    protected void addTaskStatusChangedListener(
            ITaskStatusChangedListener listener) {
        listeners.add(listener);
    }

    protected void removeTaskStatusChangedListener(
            ITaskStatusChangedListener listener) {
        listeners.remove(listener);
    }

    /**
     * Empties the work queue of all pending scripts and waits for all running
     * tasks to complete.
     */
    protected void shutdown() {
        Collection<AbstractGfeTask> tasksToCancel = new ArrayList<>();
        pendingTasks.drainTo(tasksToCancel);
        for (AbstractGfeTask task : tasksToCancel) {
            cancelTask((TaskType) task);
        }

        while (runningTasks > 0) {
            try {
                Thread.sleep(50);
            } catch (InterruptedException e) {
                /*
                 * We're shutting down anyway, so no need to log the exception.
                 * Just finish the shutdown process.
                 */
                break;
            }
        }

        finishedTasks.clear();
        allTasks.clear();
        listeners.clear();
    }
}
