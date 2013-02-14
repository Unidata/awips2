package com.raytheon.uf.edex.datadelivery.harvester.cron;

import org.quartz.CronTrigger;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SchedulerFactory;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.CrawlLauncher;

/**
 * Harvester Job for Quartz in code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 4, 2012  1038      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class HarvesterJobController<T extends CrawlLauncher> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HarvesterJobController.class);

    private Class<T> clazz;

    private String name;

    public HarvesterJobController(String name, String cron, Class<T> clazz) {

        try {
            setClazz(clazz);
            setName(name);
            SchedulerFactory schedFactory = new org.quartz.impl.StdSchedulerFactory();
            Scheduler schedular = schedFactory.getScheduler();
            // get rid of any previous jobs
            // schedular.deleteJob(name, "Crawler");
            JobDetail jobDetail = null;
            try {
                jobDetail = schedular.getJobDetail(name, "Crawler");
            } catch (SchedulerException se) {
                statusHandler.info("Job doesn't exist!");
            }

            if (jobDetail != null) {
                // reschedule
                CronTrigger trigger = (CronTrigger) schedular.getTrigger(name,
                        "Crawler");
                String cronEx = trigger.getCronExpression();
                if (!cron.equals(cronEx)) {
                    trigger.setCronExpression(cron);
                    schedular.rescheduleJob(name, "Crawler", trigger);
                    statusHandler.info("Rescheduling Job: " + name);
                }
            } else {
                jobDetail = new JobDetail(name, "Crawler", clazz);
                jobDetail.getJobDataMap().put(name, "FULL");
                CronTrigger trigger = new CronTrigger(name, "Crawler");
                trigger.setCronExpression(cron);
                schedular.scheduleJob(jobDetail, trigger);
            }

        } catch (Exception e) {
            statusHandler.error("Unable to schedule job: " + name + " error: "
                    + e.getMessage());
        }
    }

    public Class<T> getClazz() {
        return clazz;
    }

    public String getName() {
        return name;
    }

    public void setClazz(Class<T> clazz) {
        this.clazz = clazz;
    }

    public void setName(String name) {
        this.name = name;
    }

}
