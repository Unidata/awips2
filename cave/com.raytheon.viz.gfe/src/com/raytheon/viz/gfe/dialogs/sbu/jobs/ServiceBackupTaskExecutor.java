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

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;

/**
 * Service Backup Task Executor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2015   #4300     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ServiceBackupTaskExecutor extends Thread {

    private List<AbstractSbuTask> tasks;

    private boolean cancelled;

    private JobProgress jobStatus;

    /**
     * 
     */
    public ServiceBackupTaskExecutor(List<AbstractSbuTask> tasks) {
        this.tasks = tasks;
    }

    public void cancel() {
        this.cancelled = true;
        this.interrupt();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Thread#start()
     */
    @Override
    public synchronized void start() {
        this.jobStatus = JobProgress.IN_PROGRESS;
        this.cancelled = false;
        super.start();
    }

    @Override
    public void run() {
        for (AbstractSbuTask task : tasks) {
            if (cancelled) {
                break;
            }

            task.run();

            if (!task.getStatus().equals(JobProgress.SUCCESS)) {
                cancelled = true;
                jobStatus = JobProgress.FAILED;
            }
        }
        jobStatus = JobProgress.SUCCESS;
    }

    public JobProgress getJobStatus() {
        return this.jobStatus;
    }
}
