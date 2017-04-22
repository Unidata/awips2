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
package com.raytheon.viz.texteditor.alarmalert.util;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.progress.UIJob;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19, 2009            mnash     Initial creation
 * Jul 25, 2012 15122      rferrel     Add sound repeat interval.
 * Nov 26, 2013 16781  mgamazaychikov  Changed AWT Toolkit.getDefaultToolkit().beep to 
 *                                     SWT Display.beep()
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlarmBeepJob extends UIJob {

    private static final int BEEP_COUNT = 5;

    private static final long BEEP_INTERVAL = 1000L;

    private boolean disposed;

    private long delay;

    private int count = 0;

    /**
     * @param name
     */
    public AlarmBeepJob(String name, long delay) {
        super(name);

        if (delay > (BEEP_COUNT * BEEP_INTERVAL)) {
            this.delay = delay;
        } else {
            this.delay = (BEEP_COUNT + 1) * BEEP_INTERVAL;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    public IStatus runInUIThread(IProgressMonitor monitor) {
        IStatus status = Status.OK_STATUS;
        if (count < BEEP_COUNT) {
            Display.getCurrent().beep();
            if (!disposed) {
                this.schedule(BEEP_INTERVAL);
            }
            count++;
        } else if (!disposed) {
            schedule(delay - (BEEP_COUNT * BEEP_INTERVAL));
            count = 0;
        }
        return status;
    }

    /**
     * Cancel this job and release its reference to the DataManager
     */
    public void dispose() {
        this.disposed = true;
        this.cancel();
    }

}
