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
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.textformatter.TextFormatter;

/**
 * GFE Task Manager
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 07, 2011           randerso  Initial creation
 * May 28, 2014  2841     randerso  Separated product script and formatters into
 *                                  separate schedulers.
 * Aug 20, 2015  4749     dgilling  Add shutdown().
 * Jan 25, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public class TaskManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TaskManager.class);

    private static TaskManager instance = new TaskManager();

    /**
     * @return the singleton TaskManager instance
     */
    public static TaskManager getInstance() {
        return instance;
    }

    private TaskScheduler<ScriptTask> scriptScheduler;

    private TaskScheduler<TextFormatter> formatterScheduler;

    private TaskManager() {
        int pendingScriptLimit = GFEPreference
                .getInt("ProcessMonitorMaxPendingScripts", 10);

        int runningScriptLimit = GFEPreference
                .getInt("ProcessMonitorMaxScripts", 1);

        int finishedScriptLimit = GFEPreference
                .getInt("ProcessMonitorMaxOldScripts", 5);

        scriptScheduler = new TaskScheduler<>(pendingScriptLimit,
                runningScriptLimit, finishedScriptLimit);

        int pendingFormatterLimit = GFEPreference
                .getInt("ProcessMonitorMaxPendingFormatters", 10);

        int runningFormatterLimit = GFEPreference
                .getInt("ProcessMonitorMaxFormatters", 1);

        int finishedFormatterLimit = GFEPreference
                .getInt("ProcessMonitorMaxOldFormatters", 5);

        formatterScheduler = new TaskScheduler<>(pendingFormatterLimit,
                runningFormatterLimit, finishedFormatterLimit);
    }

    /**
     * Create a script task
     *
     * @param name
     *            display name
     * @param cmd
     *            script command
     */
    public void createScriptTask(String name, String cmd) {
        ScriptTask task = new ScriptTask(name, cmd);
        scriptScheduler.queueTask(task);
    }

    /**
     * Queue a text formatter
     *
     * @param formatter
     */
    public void queueFormatter(TextFormatter formatter) {
        formatterScheduler.queueTask(formatter);
    }

    /**
     * Force run a task (don't wait in the queue)
     *
     * @param task
     */
    public void forceRunTask(AbstractGfeTask task) {
        if (task instanceof ScriptTask) {
            scriptScheduler.forceRunTask((ScriptTask) task);
        } else if (task instanceof TextFormatter) {
            formatterScheduler.forceRunTask((TextFormatter) task);
        } else {
            statusHandler
                    .error("Unknown task type: " + task.getClass().getName());
        }
    }

    /**
     * Cancel a task
     *
     * @param task
     */
    public void cancelTask(AbstractGfeTask task) {
        if (task instanceof ScriptTask) {
            scriptScheduler.cancelTask((ScriptTask) task);
        } else if (task instanceof TextFormatter) {
            formatterScheduler.cancelTask((TextFormatter) task);
        } else {
            statusHandler
                    .error("Unknown task type: " + task.getClass().getName());
        }
    }

    /**
     * @return the list of tasks
     */
    public List<AbstractGfeTask> getTaskList() {
        List<AbstractGfeTask> scriptTasks = scriptScheduler.getTaskList();
        List<AbstractGfeTask> formatterTasks = formatterScheduler.getTaskList();
        List<AbstractGfeTask> allTasks = new ArrayList<>(
                scriptTasks.size() + formatterTasks.size());
        allTasks.addAll(scriptTasks);
        allTasks.addAll(formatterTasks);
        return allTasks;
    }

    /**
     * @param name
     * @return the named task
     */
    public AbstractGfeTask getTask(String name) {
        List<AbstractGfeTask> taskList = this.getTaskList();
        for (int i = taskList.size() - 1; i >= 0; i--) {
            AbstractGfeTask task = taskList.get(i);
            if (name.equals(task.getDisplayName())) {
                return task;
            }
        }
        return null;
    }

    /**
     * Add a task status changed listener
     *
     * @param listener
     */
    public void addTaskStatusChangedListener(
            ITaskStatusChangedListener listener) {
        scriptScheduler.addTaskStatusChangedListener(listener);
        formatterScheduler.addTaskStatusChangedListener(listener);
    }

    /**
     * Remove a task status changed listener
     *
     * @param listener
     */
    public void removeTaskStatusChangedListener(
            ITaskStatusChangedListener listener) {
        scriptScheduler.removeTaskStatusChangedListener(listener);
        formatterScheduler.removeTaskStatusChangedListener(listener);
    }

    /**
     * Clears all current internal state from the current task schedulers.
     */
    public void shutdown() {
        formatterScheduler.shutdown();
        scriptScheduler.shutdown();
    }
}
