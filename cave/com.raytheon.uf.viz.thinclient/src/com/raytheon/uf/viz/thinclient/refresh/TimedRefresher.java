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
package com.raytheon.uf.viz.thinclient.refresh;

import java.util.Calendar;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.viz.thinclient.Activator;

/**
 * Class that runs on a timer based on a property a specific task
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TimedRefresher implements IPropertyChangeListener {

    public static interface RefreshTimerTask {
        public void scheduled();

        public void stopped();

        public void run();
    }

    private Job job;

    private RefreshTimerTask refreshTask;

    private String propertyId;

    private long startOfLastRun = System.currentTimeMillis();

    private long intervalInMillis;

    public TimedRefresher(RefreshTimerTask task, String propertyId) {
        this.refreshTask = task;
        this.propertyId = propertyId;

        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        schedule(store.getInt(propertyId));
    }

    /**
     * Schedule the timed refresher to run at the specified interval in millis.
     * 
     * @param intervalInMinutes
     *            interval to run at in minutes. A value less than equal to 0
     *            will cancel the task all together.
     */
    private synchronized void schedule(int intervalInMinutes) {
        intervalInMillis = intervalInMinutes * 1000l * 60;
        if (intervalInMillis > 0) {
            if (job == null) {
                // We need a new job
                refreshTask.scheduled();
                job = new Job("") {

                    @Override
                    protected IStatus run(IProgressMonitor monitor) {
                    	System.out.println(refreshTask.getClass().getSimpleName() + " : " + Calendar.getInstance().getTime().toString() + " : running");
                        startOfLastRun = System.currentTimeMillis();
                        try {
                            refreshTask.run();
                        } catch (RuntimeException e) {
                            reschedule();
                            throw e;
                        }
                        reschedule();
                        return Status.OK_STATUS;
                    }

                };
                job.setSystem(true);
            } else {
                job.cancel();
            }
            reschedule();
        } else if (job != null) {
            job.cancel();
            job = null;
            refreshTask.stopped();

        }
    }

    private synchronized void reschedule() {
        if (intervalInMillis > 0) {        	
            long timePassed = System.currentTimeMillis() - startOfLastRun;
            if (timePassed > intervalInMillis) {
            	System.out.println(refreshTask.getClass().getSimpleName() + " : " + Calendar.getInstance().getTime().toString() + " : Scheduled now");
                job.schedule();
            } else {
            	System.out.println(refreshTask.getClass().getSimpleName() + " : " + Calendar.getInstance().getTime().toString() + " : Scheduled in " + ((intervalInMillis - timePassed)/1000/60) + " minutes");
                job.schedule(intervalInMillis - timePassed);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse
     * .jface.util.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (propertyId.equals(event.getProperty())) {
            Object newObject = event.getNewValue();
            int newInterval = -1;
            if (newObject instanceof Integer) {
                newInterval = (Integer) event.getNewValue();
            } else if (newObject instanceof String) {
                newInterval = Integer.valueOf((String) newObject);
            }
            schedule(newInterval);
        }
    }
}
