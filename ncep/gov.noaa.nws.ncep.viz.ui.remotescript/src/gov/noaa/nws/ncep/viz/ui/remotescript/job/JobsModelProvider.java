/*
 * gov.noaa.nws.ncep.viz.ui.remotescript.job.JobsModelProvider
 * 
 * 26 March 2014
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.ui.remotescript.job;

/**
 * Model part of the remote job MVC. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/14        ?           B. Yin  Initial Creation.
 * 
 * </pre>
 * 
 * @author byin
 * 
 */

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;

import com.raytheon.uf.viz.core.VizApp;

public enum JobsModelProvider {
    INSTANCE;

    private List<RemoteScriptJob> jobs;

    private List<IRemoteJobObserver> observers;

    /**
     * Private constructor.
     */
    private JobsModelProvider() {

        jobs = new ArrayList<RemoteScriptJob>();
        observers = new ArrayList<IRemoteJobObserver>();

    }

    /**
     * Get remote jobs submitted.
     * 
     * @return
     */
    public List<RemoteScriptJob> getRemoteScriptJobs() {
        return jobs;
    }

    /**
     * Add a new job.
     * 
     * @param job
     */
    public void addRemoteScriptJob(RemoteScriptJob job) {
        notifyObservers(job);
        jobs.add(job);
        runJob(job);
    }

    /**
     * Update an existing job.
     * 
     * @param job
     */
    public void updateRemoteScriptJob(RemoteScriptJob job) {
        notifyObservers(job);
    }

    /**
     * Register a job observer.
     * 
     * @param observer
     */
    public void registerJobObserver(IRemoteJobObserver observer) {
        observers.add(observer);
    }

    /**
     * Notify observers when a new job is added or an existing job is updated.
     * 
     * @param job
     */
    private void notifyObservers(RemoteScriptJob job) {
        for (IRemoteJobObserver observer : observers) {
            if (jobs.contains(job)) {
                observer.updateRemoteJob(job);
            } else {
                observer.addRemoteJob(job);
            }
        }
    }

    /**
     * Run the specified job and add a job change listener.
     * 
     * @param job
     */
    private void runJob(RemoteScriptJob job) {

        job.addJobChangeListener((new JobChangeAdapter() {
            public void done(IJobChangeEvent event) {

                final IJobChangeEvent evt = event;
                VizApp.runSync(new Runnable() {
                    public void run() {
                        RemoteScriptJob ajob = (RemoteScriptJob) evt.getJob();
                        updateRemoteScriptJob(ajob);
                    }
                });

            }
        }));

        job.setUser(true);

        job.schedule();
    }
}
