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
package com.raytheon.viz.gfe;

import com.raytheon.viz.gfe.dialogs.sbu.ServiceBackupDlg;
import com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent;

/**
 * Bring up the Service Backup Dialog in stand alone mode as a blocking dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2011            bphillip     Initial creation
 * Oct 26, 2012 1287       rferrel     Change to force blocking of ServiceBackupDlg.
 * Mar 21, 2013 1447       dgilling    Fix dialog construction so this dialog
 *                                     is created as a top-level shell.
 * Jun 11, 2014 DR-17401    lshi                              
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ServiceBackupComponent extends AbstractCAVEComponent {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#startInternal
     * (java.lang.String)
     */
    @Override
    protected void startInternal(String componentName) throws Exception {
        ServiceBackupDlg svcBuDlg = new ServiceBackupDlg(null);
        if (!svcBuDlg.isTerminated())
        {
        	svcBuDlg.setBlockOnOpen(true);
        	svcBuDlg.open();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#getRuntimeModes
     * ()
     */
    @Override
    protected int getRuntimeModes() {
        return ALERT_VIZ;
    }

}
