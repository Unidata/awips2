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
package com.raytheon.uf.viz.alertviz.ui.timer;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;

/**
 * This is a general timer class that is used for blinking text and audio files.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 02 Apr 2009             lvenable    TTR fixes.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AlertTimer {
    /**
     * Callback called when the timer fires or when the timer is finished.
     */
    private ITimerAction actionCB;

    /**
     * Parent display.
     */
    private Display parentDisplay;

    /**
     * Time in milliseconds between executions.
     */
    private long periodInMillis = 1000;

    private long startTime = 0;

    /**
     * Timer duration in seconds.
     */
    private long durationInSeconds = 3;

    /**
     * Flag indicating if the timer is running or not.
     */
    private boolean isRunning = false;

    /**
     * Blink text flag.
     */
    private boolean blinkText;

    private Job job = new Job("AlertTimer") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            if (isRunning) {
                long curTime = System.currentTimeMillis();
                if (parentDisplay == null || parentDisplay.isDisposed()
                        || (curTime - startTime) / 1000 > durationInSeconds) {
                    cancelTimer();
                }

                if (isRunning) {
                    parentDisplay.syncExec(new Runnable() {
                        public void run() {
                            actionCB.timerAction(blinkText);
                        }
                    });

                    long delay = periodInMillis
                            - (System.currentTimeMillis() - curTime);
                    if (delay > 0) {
                        job.schedule(delay);
                    } else {
                        job.schedule();
                    }
                }
            }

            return Status.OK_STATUS;
        }

    };

    /**
     * Constructor.
     * 
     * @param parentDisplay
     *            Parent display.
     * @param callback
     *            Timer callback.
     * @param periodInMillis
     *            Time in milliseconds between executions.
     */
    public AlertTimer(Display parentDisplay, ITimerAction callback,
            long periodInMillis) {
        this.parentDisplay = parentDisplay;
        actionCB = callback;
        this.periodInMillis = periodInMillis;
        this.job.setSystem(true);
    }

    /**
     * Constructor.
     * 
     * @param parentDisplay
     *            Parent display.
     * @param callback
     *            Timer callback.
     * @param periodInMillis
     *            Time in milliseconds between executions.
     * @param durationInSeconds
     *            Timer duration in seconds.
     */
    public AlertTimer(Display parentDisplay, ITimerAction callback,
            long periodInMillis, long durationInSeconds) {
        this(parentDisplay, callback, periodInMillis);

        /**
         * If the duration in seconds is set to 0 then set the duration to 1
         * hour. The current system has it as a "run forever" but 1 hour should
         * be long enough.
         */
        if (durationInSeconds == 0) {
            this.durationInSeconds = 3600;
        } else {
            this.durationInSeconds = durationInSeconds;
        }
    }

    /**
     * Set the timer duration.
     * 
     * @param durationInSeconds
     *            Duration in seconds.
     */
    public void setTimerDuration(long durationInSeconds) {
        /**
         * If the duration in seconds is set to 0 then set the duration to 1
         * hour. The current system has it as a "run forever" but 1 hour should
         * be long enough.
         */
        if (durationInSeconds == 0) {
            this.durationInSeconds = 3600;
        } else {
            this.durationInSeconds = durationInSeconds;
        }
    }

    /**
     * Start the timer.
     * 
     * @param durationInSeconds
     *            Timer duration in seconds.
     * @param blinkText
     *            Flag indicating if the text should blink.
     */
    public void startTimer(long durationInSeconds, boolean blinkText) {
        /**
         * If the duration in seconds is set to 0 then set the duration to 1
         * hour. The current system has it as a "run forever" but 1 hour should
         * be long enough.
         */
        if (durationInSeconds == 0) {
            this.durationInSeconds = 3600;
        } else {
            this.durationInSeconds = durationInSeconds;
        }

        this.blinkText = blinkText;
        startTimer();
    }

    /**
     * Start the timer.
     * 
     * @param durationInSeconds
     *            Duration in seconds.
     */
    public void startTimer(long durationInSeconds) {
        startTimer(durationInSeconds, false);
    }

    /**
     * Start the timer.
     */
    public void startTimer() {
        // reset the start time
        synchronized (this) {
            startTime = System.currentTimeMillis();
            if (!isRunning) {
                isRunning = true;
                job.schedule();
            }
        }
    }

    /**
     * Setting alertPopupDlg Cancel the timer.
     */
    public void cancelTimer() {
        // only synchronize on cancelling the timer, don't do the syncExec in
        // the sync block.
        boolean cancel = false;
        synchronized (this) {
            if (isRunning) {
                isRunning = false;
                cancel = true;
            }
        }

        if (cancel) {
            if (parentDisplay.isDisposed() == true) {
                return;
            }

            parentDisplay.syncExec(new Runnable() {
                public void run() {
                    actionCB.timerCompleted();
                }
            });
        }
    }

    /**
     * Check if the timer is running.
     * 
     * @return True if the timer is running, false otherwise.
     */
    public boolean timerIsRunning() {
        return isRunning;
    }
}
