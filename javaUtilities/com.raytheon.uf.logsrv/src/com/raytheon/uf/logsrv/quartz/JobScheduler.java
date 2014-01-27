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
package com.raytheon.uf.logsrv.quartz;

import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SchedulerFactory;
import org.quartz.Trigger;
import org.quartz.TriggerUtils;
import org.quartz.impl.StdSchedulerFactory;

import com.raytheon.uf.logsrv.config.LogSrvConfig;

/**
 * Schedules the quartz jobs in the main log process so at timed intervals, the
 * number of logging events in the db are reported and the error report is
 * generated and emailed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class JobScheduler {

    public static void scheduleJobs(LogSrvConfig config)
            throws SchedulerException {
        SchedulerFactory factory = new StdSchedulerFactory();
        Scheduler sched = factory.getScheduler();
        sched.start();

        JobDetail job = new JobDetail("Create and Send Report Job",
                CreateSendReportJob.class);
        job.getJobDataMap().put("config", config);
        String[] split = config.getTimeToSend().split(":");
        int hour = Integer.parseInt(split[0]);
        int minute = Integer.parseInt(split[1]);
        Trigger trigger = TriggerUtils.makeDailyTrigger(hour, minute);
        trigger.setName("Report Trigger");
        sched.scheduleJob(job, trigger);

        JobDetail countJob = new JobDetail("Count Rows Job", CountRowsJob.class);
        Trigger countTrigger = TriggerUtils.makeHourlyTrigger();
        countTrigger.setName("Count Trigger");
        sched.scheduleJob(countJob, countTrigger);
    }

}
