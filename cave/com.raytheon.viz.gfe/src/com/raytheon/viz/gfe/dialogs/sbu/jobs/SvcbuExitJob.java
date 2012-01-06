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
package com.raytheon.viz.gfe.dialogs.sbu.jobs;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.dialogs.sbu.ServiceBackupDlg;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class SvcbuExitJob extends ServiceBackupJob {

    private ServiceBackupDlg dialog;

    /**
     * @param name
     */
    public SvcbuExitJob(ServiceBackupDlg dialog, String primarySite) {
        super("Exit Service Backup", primarySite);
        this.dialog = dialog;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.dialogs.sbu.jobs.ServiceBackupJob#run()
     */
    @Override
    public void run() {
        VizApp.runAsync(new CloseSvcBuDialog(dialog));
    }

    private class CloseSvcBuDialog implements Runnable {

        ServiceBackupDlg dialog;

        public CloseSvcBuDialog(ServiceBackupDlg dialog) {
            this.dialog = dialog;
        }

        public void run() {
            this.dialog.close();
        }
    }
}
