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
package com.raytheon.viz.gfe.dialogs.sbu.jobs;

import java.util.concurrent.ArrayBlockingQueue;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ServiceBackupJobManager extends Thread {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServiceBackupJob.class);

    private ArrayBlockingQueue<ServiceBackupJob> jobQueue = new ArrayBlockingQueue<ServiceBackupJob>(
            10);

    private static ServiceBackupJobManager instance;

    private boolean running;

    public static ServiceBackupJobManager getInstance() {
        if (instance == null) {
            instance = new ServiceBackupJobManager();
        }
        return instance;
    }

    private ServiceBackupJobManager() {
        setDaemon(true);
    }

    public boolean isRunning() {
        return running;
    }

    public void run() {
        running = true;
        while (true) {
            try {
                ServiceBackupJob job = jobQueue.take();
                try {
                    job.run();
                    if (job.aborted()) {
                        statusHandler.error("Service Backup Job: ["
                                + job.getName() + "] ABORTED!");
                        jobQueue.clear();
                    } else if (job.failed()) {
                        statusHandler.error("Service Backup Job: ["
                                + job.getName() + "] FAILED!  See Service Backup logs for details.");
                        jobQueue.clear();
                    } else {
                        statusHandler.info("Service Backup Job: ["
                                + job.getName() + "] successfully completed!");
                    }
                } catch (Throwable e) {
                    statusHandler.error("Service Backup Job: [" + job.getName()
                            + "] FAILED!", e);
                    jobQueue.clear();
                }
            } catch (InterruptedException e) {

            }
        }
    }

    public void addJob(ServiceBackupJob job) {
        jobQueue.add(job);
    }
}
