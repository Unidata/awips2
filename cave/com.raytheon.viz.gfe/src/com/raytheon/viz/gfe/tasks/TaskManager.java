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

import java.util.List;

import com.raytheon.viz.gfe.textformatter.TextFormatter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TaskManager {
    private static TaskManager instance = new TaskManager();

    public static TaskManager getInstance() {
        return instance;
    }

    private TaskScheduler taskScheduler;

    private TaskManager() {
        taskScheduler = new TaskScheduler();
    }

    public void createScriptTask(String name, String cmd) {
        ScriptTask task = new ScriptTask(name, cmd);
        taskScheduler.queueTask(task);
    }

    public void queueFormatter(TextFormatter formatter) {
        taskScheduler.queueTask(formatter);
    }

    public void forceRunTask(AbstractGfeTask task) {
        taskScheduler.forceRunTask(task);
    }

    public void cancelTask(AbstractGfeTask task) {
        taskScheduler.cancelTask(task);
    }

    public List<AbstractGfeTask> getTaskList() {
        return taskScheduler.getTaskList();
    }

    public AbstractGfeTask getTask(String name) {
        List<AbstractGfeTask> taskList = taskScheduler.getTaskList();
        for (int i = taskList.size() - 1; i >= 0; i--) {
            AbstractGfeTask task = taskList.get(i);
            if (name.equals(task.getDisplayName())) {
                return task;
            }
        }
        return null;
    }

    public void addTaskStatusChangedListener(ITaskStatusChangedListener listener) {
        taskScheduler.addTaskStatusChangedListener(listener);
    }

    public void removeTaskStatusChangedListener(
            ITaskStatusChangedListener listener) {
        taskScheduler.removeTaskStatusChangedListener(listener);
    }
}
