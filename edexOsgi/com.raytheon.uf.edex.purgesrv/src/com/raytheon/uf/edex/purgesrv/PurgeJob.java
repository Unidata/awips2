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
package com.raytheon.uf.edex.purgesrv;

import java.lang.reflect.Method;
import java.sql.SQLException;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.purge.PurgeLogger;

/**
 * 
 * This class encapsulates the purge activity for a plugin into a cluster task.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2012   #470      bphillip     Initial creation
 * Jun 20, 2012  NC#606     ghull        send purge-complete messages 
 * May 08, 2013  1814       rjpeter      Added time to live to topic
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class PurgeJob extends Thread {

    /** The type of purge */
    public enum PURGE_JOB_TYPE {
        PURGE_ALL, PURGE_EXPIRED
    }

    public static final String PLUGIN_PURGED_TOPIC = "jms-generic:topic:pluginPurged?timeToLive=60000";

    private long startTime;

    /** The cluster task name to use for purge jobs */
    public static final String TASK_NAME = "Purge Plugin Data";

    /** The plugin associated with this purge job */
    private final String pluginName;

    /** The type of purge job being executed */
    private final PURGE_JOB_TYPE purgeType;

    /** Last time job has printed a timed out message */
    private final long lastTimeOutMessage = 0;

    /**
     * Creates a new Purge job for the specified plugin.
     * 
     * @param pluginName
     *            The plugin to be purged
     * @param purgeType
     *            The type of purge to be executed
     */
    public PurgeJob(String pluginName, PURGE_JOB_TYPE purgeType) {
        // Give the thread a name
        this.setName("Purge-" + pluginName.toUpperCase() + "-Thread");
        this.pluginName = pluginName;
        this.purgeType = purgeType;
    }

    @Override
    public void run() {

        // Flag used to track if this job has failed
        boolean failed = false;
        startTime = System.currentTimeMillis();
        PurgeLogger.logInfo("Purging expired data...", pluginName);
        PluginDao dao = null;

        try {
            dao = PluginFactory.getInstance().getPluginDao(pluginName);
            if (dao.getDaoClass() != null) {
                dao.purgeExpiredData();

                PurgeLogger.logInfo("Data successfully Purged!", pluginName);

                EDEXUtil.getMessageProducer().sendAsyncUri(PLUGIN_PURGED_TOPIC,
                        pluginName);

            } else {
                Method m = dao.getClass().getMethod("purgeExpiredData",
                        new Class[] {});
                if (m != null) {
                    if (m.getDeclaringClass().equals(PluginDao.class)) {
                        PurgeLogger
                                .logWarn(
                                        "Unable to purge data.  This plugin does not specify a record class and does not implement a custom purger.",
                                        pluginName);
                    } else {
                        if (this.purgeType.equals(PURGE_JOB_TYPE.PURGE_EXPIRED)) {
                            dao.purgeExpiredData();
                        } else {
                            dao.purgeAllData();
                        }

                        PurgeLogger.logInfo("Data successfully Purged!",
                                pluginName);

                        EDEXUtil.getMessageProducer().sendAsyncUri(
                                PLUGIN_PURGED_TOPIC, pluginName);
                    }
                }
            }
        } catch (Exception e) {
            failed = true;
            // keep getting next exceptions with sql exceptions to ensure
            // we can see the underlying error
            PurgeLogger
                    .logError("Error purging expired data!\n", pluginName, e);
            Throwable t = e.getCause();
            while (t != null) {
                if (t instanceof SQLException) {
                    SQLException se = ((SQLException) t).getNextException();
                    PurgeLogger.logError("Next exception:", pluginName, se);
                }
                t = t.getCause();
            }
        } finally {
            ClusterTask purgeLock = PurgeManager.getInstance().getPurgeLock();
            try {
                /*
                 * Update the status accordingly if the purge failed or
                 * succeeded
                 */
                PurgeDao purgeDao = new PurgeDao();
                PurgeJobStatus status = purgeDao
                        .getJobForPlugin(this.pluginName);
                if (status == null) {
                    PurgeLogger.logError(
                            "Purge job completed but no status object found!",
                            this.pluginName);
                } else {
                    if (failed) {
                        status.incrementFailedCount();
                        if (status.getFailedCount() >= PurgeManager
                                .getInstance().getFatalFailureCount()) {
                            PurgeLogger
                                    .logFatal(
                                            "Purger for this plugin has reached or exceeded consecutive failure limit of "
                                                    + PurgeManager
                                                            .getInstance()
                                                            .getFatalFailureCount()
                                                    + ".  Data will no longer being purged for this plugin.",
                                            pluginName);
                        } else {
                            PurgeLogger.logError("Purge job has failed "
                                    + status.getFailedCount()
                                    + " consecutive times.", this.pluginName);
                            // Back the start time off by half an hour to try to
                            // purgin soon, don't want to start immediately so
                            // it doesn't ping pong between servers in a time
                            // out scenario
                            Date startTime = status.getStartTime();
                            startTime.setTime(startTime.getTime() - (1800000));
                        }
                    } else {
                        status.setFailedCount(0);
                    }

                    /*
                     * This purger thread has exceeded the time out duration but
                     * finally finished. Output a message and update the status
                     */
                    int deadPurgeJobAge = PurgeManager.getInstance()
                            .getDeadPurgeJobAge();
                    Calendar purgeTimeOutLimit = Calendar.getInstance();
                    purgeTimeOutLimit.setTimeZone(TimeZone.getTimeZone("GMT"));
                    purgeTimeOutLimit.add(Calendar.MINUTE, -deadPurgeJobAge);
                    if (startTime < purgeTimeOutLimit.getTimeInMillis()) {
                        PurgeLogger
                                .logInfo(
                                        "Purge job has recovered from timed out state!!",
                                        pluginName);
                    }
                    status.setRunning(false);
                    purgeDao.update(status);
                    /*
                     * Log execution times
                     */
                    long executionTime = getAge();
                    long execTimeInMinutes = executionTime / 60000;
                    if (execTimeInMinutes > 0) {
                        PurgeLogger.logInfo("Purge run time: " + executionTime
                                + " ms (" + execTimeInMinutes + " minutes)",
                                this.pluginName);
                    } else {
                        PurgeLogger.logInfo("Purge run time: " + executionTime
                                + " ms", this.pluginName);
                    }
                }
            } catch (Throwable e) {
                PurgeLogger
                        .logError(
                                "An unexpected error occurred upon completion of the purge job",
                                this.pluginName, e);
            } finally {
                ClusterLockUtils.unlock(purgeLock, false);
            }
        }
    }

    public void printTimedOutMessage(int deadPurgeJobAge) {
        // only print message every 5 minutes
        if (System.currentTimeMillis() - lastTimeOutMessage > 300000) {
            PurgeLogger.logFatal(
                    "Purger running time has exceeded timeout duration of "
                            + deadPurgeJobAge
                            + " minutes.  Current running time: "
                            + (getAge() / 60000) + " minutes", pluginName);
            printStackTrace();
        }
    }

    /**
     * Prints the stack trace for this job thread.
     */
    public void printStackTrace() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("Stack trace for Purge Job Thread:\n");
        buffer.append(getStackTrace(this));
        // If this thread is blocked, output the stack traces for the other
        // blocked threads to assist in determining the source of the
        // deadlocked
        // threads
        if (this.getState().equals(State.BLOCKED)) {
            buffer.append("\tDUMPING OTHER BLOCKED THREADS\n");
            buffer.append(getBlockedStackTraces());

        }
        PurgeLogger.logError(buffer.toString(), this.pluginName);

    }

    /**
     * Gets the stack traces for all other threads in the BLOCKED state in the
     * JVM
     * 
     * @return The stack traces for all other threads in the BLOCKED state in
     *         the JVM
     */
    private String getBlockedStackTraces() {
        StringBuffer buffer = new StringBuffer();
        Map<Thread, StackTraceElement[]> threads = Thread.getAllStackTraces();
        for (Thread t : threads.keySet()) {
            if (t.getState().equals(State.BLOCKED)) {
                if (t.getId() != this.getId()) {
                    buffer.append(getStackTrace(t));
                }
            }
        }

        return buffer.toString();
    }

    /**
     * Gets the stack trace for the given thread
     * 
     * @param thread
     *            The thread to get the stack trace for
     * @return The stack trace as a String
     */
    private String getStackTrace(Thread thread) {
        StringBuffer buffer = new StringBuffer();
        StackTraceElement[] stack = Thread.getAllStackTraces().get(thread);
        buffer.append("\tThread ID: ").append(thread.getId())
                .append("  Thread state: ").append(this.getState())
                .append("\n");
        if (stack == null) {
            buffer.append("No stack trace could be retrieved for this thread");
        } else {
            for (StackTraceElement element : stack) {
                buffer.append("\t\t").append(element).append("\n");
            }
        }
        return buffer.toString();
    }

    public long getStartTime() {
        return startTime;
    }

    public long getAge() {
        return System.currentTimeMillis() - startTime;
    }
}
