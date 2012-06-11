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
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Button;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.texteditor.alarmalert.dialogs.AlarmAlertBell;

/**
 * TODO 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class FlashBellJob extends Job {

    private AlarmAlertBell bell;
    
    private int delay;
    
    private boolean disposed = false;
    
    /**
     * 
     * @param name
     * @param bell
     * @param delay
     */
    public FlashBellJob(String name, AlarmAlertBell bell, int delay) {
        super(name);
    
        this.bell = bell;
        
        this.delay = delay;
        
        schedule(delay);
    }

    /**
     * 
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        if((bell != null) && (bell.isActive())) {
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    System.out.println("Flashing bell");
                    bell.flash();
                }
            });
            System.out.println("Scheduling bell");
            schedule(delay);
        } else {
            dispose();
        }
        return Status.OK_STATUS;
    }

    /**
     * Cancel this job and release its reference to the DataManager
     */
    public void dispose() {
        System.out.println("Disposing bell job");
        disposed = true;
        bell = null;
        this.cancel();
    }

}
