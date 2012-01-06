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
package com.raytheon.viz.aviation.monitor;

import java.util.Calendar;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.viz.aviation.observer.TafMonitorDlg;

/**
 * Applies the monitoring rules at a specified interval. Intended for use for
 * updating monitor to display at 00 and 30 after the hour.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ScheduledMonitorTask extends Job {

    private TafMonitorDlg dlg;

    private boolean stop = false;

    public ScheduledMonitorTask(TafMonitorDlg dialog) {
        super("Scheduled monitor job");
        this.dlg = dialog;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        if (!stop) {
            dlg.checkNow();

            // Set up to run on next hour or 1/2 hour.
            Calendar cal = Calendar.getInstance();
            cal.setTimeInMillis(System.currentTimeMillis());
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);

            if (cal.get(Calendar.MINUTE) < 30) {
                cal.set(Calendar.MINUTE, 30);
            } else {
                cal.set(Calendar.MINUTE, 0);
                cal.add(Calendar.HOUR, 1);
            }

            // Check again in case canceled while running.
            if (!stop) {
                schedule(cal.getTimeInMillis() - System.currentTimeMillis());
            }
        }

        return Status.OK_STATUS;
    }

    public void stop() {
        stop = true;
    }

}
