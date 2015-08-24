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
import java.io.IOException;
import java.util.Date;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Base class for GFE tasks
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2011            randerso    Initial creation
 * May 28, 2014  #2841     randerso    Made TaskScheduler generic
 * Aug 20, 2015  #4749     dgilling    Add cleanUp.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class AbstractGfeTask extends Thread implements
        Comparable<AbstractGfeTask> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractGfeTask.class);

    public static enum TaskStatus {
        PENDING, RUNNING, FINISHED, CANCELED
    };

    private static File logBaseDir;

    protected Date queuedTime;

    protected Date startedTime;

    protected Date finishedTime;

    protected TaskStatus status;

    private String displayName;

    private File logFile;

    private TaskScheduler<AbstractGfeTask> scheduler;

    protected AbstractGfeTask(String displayName) {
        this.displayName = displayName;
        this.queuedTime = SimulatedTime.getSystemTime().getTime();
        this.status = TaskStatus.PENDING;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(AbstractGfeTask o) {
        int result = 0;
        switch (status) {
        case CANCELED:
        case FINISHED:
            result = o.finishedTime.compareTo(finishedTime);
            break;

        case RUNNING:
        case PENDING:
            result = o.queuedTime.compareTo(queuedTime);
        }
        return result;
    }

    protected File createLogFile() throws IOException {
        this.logFile = File.createTempFile("gfe_", ".log", getLogDir());
        return this.logFile;
    }

    public abstract String getCommand();

    /**
     * @return the displayName
     */
    public String getDisplayName() {
        return displayName;
    }

    /**
     * @return the finishedTime
     */
    public Date getFinishedTime() {
        return finishedTime;
    }

    private File getLogDir() {
        if (logBaseDir == null) {
            try {
                IPathManager pathMgr = PathManagerFactory.getPathManager();
                LocalizationContext caveStaticUser = pathMgr.getContext(
                        LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
                logBaseDir = pathMgr.getLocalizationFile(caveStaticUser,
                        FileUtil.join("gfe", "logs")).getFile(false);

                if (!logBaseDir.exists()) {
                    logBaseDir.mkdirs();
                } else {
                    // clear out old log files
                    for (File file : logBaseDir.listFiles()) {
                        file.delete();
                    }
                }
            } catch (LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);

            }
        }
        return logBaseDir;
    }

    /**
     * @return the logFile
     */
    public File getLogFile() {
        return logFile;
    }

    /**
     * @return the queuedTime
     */
    public Date getQueuedTime() {
        return queuedTime;
    }

    protected void setScheduler(TaskScheduler scheduler) {
        this.scheduler = scheduler;
    }

    /**
     * @return the startedTime
     */
    public Date getStartedTime() {
        return startedTime;
    }

    /**
     * @return the status
     */
    public TaskStatus getStatus() {
        return status;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Thread#start()
     */
    @Override
    public synchronized void start() {
        super.start();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Thread#run()
     */
    @Override
    public void run() {
        this.startedTime = SimulatedTime.getSystemTime().getTime();
        this.status = TaskStatus.RUNNING;

        try {
            logFile = createLogFile();
            doRun();
        } catch (Throwable e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } finally {
            this.finishedTime = SimulatedTime.getSystemTime().getTime();
            if (this.status != TaskStatus.CANCELED) {
                this.status = TaskStatus.FINISHED;
            }
            taskCompleted();
        }
    }

    public abstract void doRun();

    public abstract void cancel();

    public final void taskCanceled() {
        this.scheduler.taskCanceled(this);
    }

    public final void taskCompleted() {
        this.scheduler.taskCompleted(this);
    }

    /**
     * Called when a task is completed. At this point the task should release
     * any object references not needed for the Process Monitor dialog to aid in
     * garbage collection.
     */
    public void cleanUp() {
        scheduler = null;
    }
}
