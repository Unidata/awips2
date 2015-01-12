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
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.textformatter.TextFormatter;

/**
 * GFE Task Manager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2011            randerso     Initial creation
 * May 28, 2014  #2841    randerso     Separated product script and formatters into
 *                                     separate schedulers.
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TaskManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TaskManager.class);

    private static TaskManager instance = new TaskManager();

    public static TaskManager getInstance() {
        return instance;
    }

    private TaskScheduler<ScriptTask> scriptScheduler;

    private TaskScheduler<TextFormatter> formatterScheduler;

    private TaskManager() {
        PythonPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();

        int pendingScriptLimit = 10;
        if (prefs.contains("ProcessMonitorMaxPendingScripts")) {
            pendingScriptLimit = prefs
                    .getInt("ProcessMonitorMaxPendingScripts");
        }

        int runningScriptLimit = 1;
        if (prefs.contains("ProcessMonitorMaxScripts")) {
            runningScriptLimit = prefs.getInt("ProcessMonitorMaxScripts");
        }
        int finishedScriptLimit = 5;
        if (prefs.contains("ProcessMonitorMaxOldScripts")) {
            finishedScriptLimit = prefs.getInt("ProcessMonitorMaxOldScripts");
        }

        scriptScheduler = new TaskScheduler<ScriptTask>(pendingScriptLimit,
                runningScriptLimit, finishedScriptLimit);

        int pendingFormatterLimit = 10;
        if (prefs.contains("ProcessMonitorMaxPendingFormatters")) {
            pendingFormatterLimit = prefs
                    .getInt("ProcessMonitorMaxPendingFormatters");
        }

        int runningFormatterLimit = 1;
        if (prefs.contains("ProcessMonitorMaxFormatters")) {
            runningFormatterLimit = prefs.getInt("ProcessMonitorMaxFormatters");
        }
        int finishedFormatterLimit = 5;
        if (prefs.contains("ProcessMonitorMaxOldFormatters")) {
            finishedFormatterLimit = prefs
                    .getInt("ProcessMonitorMaxOldFormatters");
        }

        formatterScheduler = new TaskScheduler<TextFormatter>(
                pendingFormatterLimit, runningFormatterLimit,
                finishedFormatterLimit);
    }

    public void createScriptTask(String name, String cmd) {
        ScriptTask task = new ScriptTask(name, cmd);
        scriptScheduler.queueTask(task);
    }

    public void queueFormatter(TextFormatter formatter) {
        formatterScheduler.queueTask(formatter);
    }

    public void forceRunTask(AbstractGfeTask task) {
        if (task instanceof ScriptTask) {
            scriptScheduler.forceRunTask((ScriptTask) task);
        } else if (task instanceof TextFormatter) {
            formatterScheduler.forceRunTask((TextFormatter) task);
        } else {
            statusHandler.error("Unknown task type: "
                    + task.getClass().getName());
        }
    }

    public void cancelTask(AbstractGfeTask task) {
        if (task instanceof ScriptTask) {
            scriptScheduler.cancelTask((ScriptTask) task);
        } else if (task instanceof TextFormatter) {
            formatterScheduler.cancelTask((TextFormatter) task);
        } else {
            statusHandler.error("Unknown task type: "
                    + task.getClass().getName());
        }
    }

    public List<AbstractGfeTask> getTaskList() {
        List<AbstractGfeTask> scriptTasks = scriptScheduler.getTaskList();
        List<AbstractGfeTask> formatterTasks = formatterScheduler.getTaskList();
        List<AbstractGfeTask> allTasks = new ArrayList<AbstractGfeTask>(
                scriptTasks.size() + formatterTasks.size());
        allTasks.addAll(scriptTasks);
        allTasks.addAll(formatterTasks);
        return allTasks;
    }

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

    public void addTaskStatusChangedListener(ITaskStatusChangedListener listener) {
        scriptScheduler.addTaskStatusChangedListener(listener);
        formatterScheduler.addTaskStatusChangedListener(listener);
    }

    public void removeTaskStatusChangedListener(
            ITaskStatusChangedListener listener) {
        scriptScheduler.removeTaskStatusChangedListener(listener);
        formatterScheduler.removeTaskStatusChangedListener(listener);
    }
}
