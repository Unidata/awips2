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
package com.raytheon.uf.viz.alertviz.ui.audio;

import java.time.Duration;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.core.VizApp;

/**
 * Repeats playing the System beep. Same code as
 * com.raytheon.viz.texteditor.alarmalert.util.AlarmBeepJob
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------------------------
 * Mar 03, 2011  8059     rferrel   Initial creation
 * Nov 26, 2013  16772    gzhang    use Display.beep()
 * Sep 20, 2018  7457     randerso  Renamed and implemented IAlertSoundJob
 *
 * </pre>
 *
 * @author rferrel
 */

public class AlertBeepJob extends Job implements IAlertSoundJob {

    /** time between beeps in milliseconds */
    private static final Duration BEEP_PERIOD = Duration.ofSeconds(1);

    private boolean repeat;

    private boolean canceled;

    private List<IJobFinshedListener> listenerList;

    /**
     * @param seconds
     */
    public AlertBeepJob() {
        super("AlarmBeepJob");
        listenerList = new CopyOnWriteArrayList<>();
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        IStatus status = Status.OK_STATUS;
        if (!canceled) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    Display.getDefault().beep();
                }
            });

            if (repeat) {
                this.schedule(BEEP_PERIOD.toMillis());
            } else {
                fireFinishListeners();
            }
        }
        return status;
    }

    @Override
    protected void canceling() {
        canceled = true;
    }

    private void fireFinishListeners() {
        for (IJobFinshedListener listener : listenerList) {
            listener.audioJobFinished(this);
        }
    }

    @Override
    public void kill() {
        this.cancel();
        fireFinishListeners();
    }

    @Override
    public void play() {
        repeat = false;
        this.schedule();
    }

    @Override
    public void loop() {
        repeat = true;
        this.schedule();
    }

    @Override
    public void addFinishedListener(IJobFinshedListener listener) {
        listenerList.add(listener);
    }
}
