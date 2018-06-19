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

import java.awt.Toolkit;

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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 03, 2011 #8059      rferrel     Initial creation
 * Nov 26, 2013 DR16772    gzhang      use Display.beep()
 * 
 * </pre>
 * @author rferrel
 * @version 1.0
 */

public class AlarmBeepJob extends Job {

    private boolean disposed;

    private int count = 0;

    /**
     * @param seconds
     */
    public AlarmBeepJob(int seconds) {
        super("AlarmBeepJob");
        if (seconds <= 0) {
            this.count = Integer.MAX_VALUE;
        } else {
            this.count = seconds;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        IStatus status = Status.OK_STATUS;
        if (count > 0) { 
            VizApp.runAsync(new Runnable(){@Override public void run(){Display.getDefault().beep();}}); // DR 16772   //Toolkit.getDefaultToolkit().beep();
            reSchedule();
            count--;
        } else {
            dispose();
        }
        return status;
    }

    /**
     * Schedule this job to run after the desired interval
     */
    private void reSchedule() {
        if (!disposed) {
            this.schedule(1 * 1000);
        }
    }

    /**
     * Cancel this job and release its reference to the DataManager
     */
    public void dispose() {
        this.disposed = true;
        this.cancel();
    }
}
