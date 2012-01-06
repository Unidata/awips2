package com.raytheon.viz.ui.personalities.awips;

import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;

/**
 * Track job execution time
 * 
 * @author Wufeng Zhou
 * 
 */

public class CaveJobChangeListener extends AbstractCavePerformanceMonitor
        implements IJobChangeListener {

    private static CaveJobChangeListener instance = new CaveJobChangeListener();

    public static CaveJobChangeListener getInstance() {
        return instance;
    }

    private CaveJobChangeListener() {
        super("caveJobLog");
    }

    @Override
    public void aboutToRun(IJobChangeEvent event) {
        Job job = event.getJob();
        if (excludeJobLogging(job))
            return;

        String key = job.getClass().getName() + "__" + job.toString();

        startTimeMap.put(key, System.currentTimeMillis());
        // out.println("aboutToRun: Job " + key);
    }

    @Override
    public void done(IJobChangeEvent event) {
        Job job = event.getJob();
        if (excludeJobLogging(job))
            return;

        String key = job.getClass().getName() + "__" + job.toString();
        if (startTimeMap.get(key) == null) {
            log("done: Fail to find start time for " + key);
        } else {
            Long startTime = startTimeMap.get(key);
            startTimeMap.remove(key);
            long delta = System.currentTimeMillis() - startTime.longValue();

            Integer count = runCountMap.get(job.getClass().getName());
            if (count == null)
                count = new Integer(0);
            Long totalTime = runTotalTimeMap.get(job.getClass().getName());
            if (totalTime == null)
                totalTime = new Long(0);
            // now the new count
            count = count + 1;
            totalTime = totalTime + delta;
            runCountMap.put(job.getClass().getName(), count);
            runTotalTimeMap.put(job.getClass().getName(), totalTime);
            long avgTime = totalTime / count;

            log("Job Run: " + key + " exe " + delta + "ms" + ", cnt="
                    + count);
        }
    }

    @Override
    public void sleeping(IJobChangeEvent event) {
        Job job = event.getJob();
        String key = job.getClass().getName() + "__" + job.toString();
        log("sleeping: job " + key);
    }

    /**
     * want to exclude logging of some of the jobs that run really short and
     * very frequently. e.g.,
     * com.raytheon.viz.ui.statusline.TimeDisplay$TimeUpdateJob is run every
     * second to update UI clock with run time of 1ms
     * com.raytheon.viz.gfe.ui.GfeMenuManagerJob with run time of about 0ms
     * 
     * @param job
     * @return
     */
    private static String[] EXCLUDED_JOBS = {
            "com.raytheon.viz.alerts.observers.ProductAlertObserver",
            "com.raytheon.viz.ui.statusline.TimeDisplay$TimeUpdateJob",
            "com.raytheon.viz.gfe.ui.GfeMenuManagerJob",
            "com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob$JobWrapper" };

    private static boolean excludeJobLogging(Job job) {
        if (!job.getClass().getName().startsWith("com.raytheon")
                && !job.getClass().getName().startsWith("gov")) {
            return true; // only time jobs developed by Raytheon and government
        }
        for (String ejob : EXCLUDED_JOBS) {
            if (job.getClass().getName().startsWith(ejob))
                return true;
        }

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.runtime.jobs.IJobChangeListener#awake(org.eclipse.core
     * .runtime.jobs.IJobChangeEvent)
     */
    @Override
    public void awake(IJobChangeEvent event) {
        // Do Nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.runtime.jobs.IJobChangeListener#running(org.eclipse.
     * core.runtime.jobs.IJobChangeEvent)
     */
    @Override
    public void running(IJobChangeEvent event) {
        // Do Nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.runtime.jobs.IJobChangeListener#scheduled(org.eclipse
     * .core.runtime.jobs.IJobChangeEvent)
     */
    @Override
    public void scheduled(IJobChangeEvent event) {
        // Do Nothing
    }
}
