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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.tasks.AbstractGfeTask.TaskStatus;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 07, 2011            randerso    Initial creation
 * Mar 03, 2012  #346      dgilling    Use identity-based ListenerLists.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TaskScheduler extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TaskScheduler.class);

    private List<AbstractGfeTask> allTasks;

    private ArrayBlockingQueue<AbstractGfeTask> pendingTasks;

    private ArrayBlockingQueue<AbstractGfeTask> finishedTasks;

    private int runningTasks, runningTaskLimit;

    private ListenerList listeners;

    protected TaskScheduler() {
        super("Task Manager Job");
        listeners = new ListenerList(ListenerList.IDENTITY);

        PythonPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        int pendingTaskLimit = 20;
        if (prefs.contains("ProcessMonitorMaxPendingTasks")) {
            pendingTaskLimit = prefs.getInt("ProcessMonitorMaxPendingTasks");
        }

        runningTaskLimit = 1;
        if (prefs.contains("ProcessMonitorMaxTasks")) {
            runningTaskLimit = prefs.getInt("ProcessMonitorMaxTasks");
        }
        int finishedTaskLimit = 10;
        if (prefs.contains("ProcessMonitorMaxOldTasks")) {
            finishedTaskLimit = prefs.getInt("ProcessMonitorMaxOldTasks");
        }

        pendingTasks = new ArrayBlockingQueue<AbstractGfeTask>(pendingTaskLimit);
        runningTasks = 0;
        finishedTasks = new ArrayBlockingQueue<AbstractGfeTask>(
                finishedTaskLimit);
        allTasks = new ArrayList<AbstractGfeTask>(pendingTaskLimit
                + runningTaskLimit + finishedTaskLimit);

    }

    protected void queueTask(AbstractGfeTask task) {
        allTasks.add(task);
        task.setScheduler(this);
        try {
            pendingTasks.add(task);
        } catch (IllegalStateException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to queue job "
                    + task.getDisplayName(), e);
        }
        this.schedule();
        fireTaskStatusChanged(task);
    }

    protected synchronized void forceRunTask(AbstractGfeTask task) {
        if (pendingTasks.remove(task)) {
            runTask(task);
        }
    }

    protected synchronized void cancelTask(AbstractGfeTask task) {
        // remove from pending list in case it's still pending
        // will do nothing if it's not in the pending list anymore
        pendingTasks.remove(task);
        task.cancel();
    }

    protected synchronized void runTask(AbstractGfeTask task) {
        System.out.println("runTask");
        // try {
        // String[] cmdArray = task.getCommand().split(" ");
        // File f = new File(scriptsBaseDir + File.separator + cmdArray[0]);
        // String dir = f.getParentFile().getAbsolutePath();
        // cmdArray[0] = f.getAbsolutePath();
        // ProcessBuilder pb = new ProcessBuilder(cmdArray);
        // pb.directory(new File(dir));
        // pb.redirectErrorStream(true);
        // task.setLogFile(File.createTempFile("gfe_", ".log", logBaseDir));
        // task.setProcessBuilder(pb);
        runningTasks++;
        task.start();

        // } catch (IOException e) {
        // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
        // StatusConstants.CATEGORY_GFE, null,
        // "Error creating log file for job " + task.getDisplayName()
        // + ": " + e.getLocalizedMessage(), e);
        // }
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
        System.out.println("pending: " + pendingTasks.size() + " running: "
                + runningTasks);
        while (runningTasks < runningTaskLimit && pendingTasks.size() > 0) {
            AbstractGfeTask task = pendingTasks.poll();
            runTask(task);
        }

        return Status.OK_STATUS;
    }

    protected synchronized void taskCompleted(AbstractGfeTask task) {
        System.out.println("taskCompleted");
        runningTasks--;
        finishTask(task);
    }

    protected synchronized void taskCanceled(AbstractGfeTask task) {
        task.status = TaskStatus.CANCELED;
        System.out.println("taskCompleted");
        finishTask(task);
    }

    private void finishTask(AbstractGfeTask task) {
        if (finishedTasks.remainingCapacity() == 0) {
            AbstractGfeTask t = finishedTasks.poll();
            allTasks.remove(t);
            t.getLogFile().delete();
        }
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
        return Collections.unmodifiableList(allTasks);
    }

    protected void addTaskStatusChangedListener(
            ITaskStatusChangedListener listener) {
        listeners.add(listener);
    }

    protected void removeTaskStatusChangedListener(
            ITaskStatusChangedListener listener) {
        listeners.remove(listener);
    }
}
