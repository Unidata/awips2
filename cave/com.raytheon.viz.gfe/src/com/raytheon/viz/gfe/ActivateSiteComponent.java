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

import com.raytheon.viz.gfe.dialogs.sbu.SiteActivationDlg;
import com.raytheon.viz.ui.personalities.awips.AbstractAWIPSComponent;

/**
 * Display Activate Site dialog standalone and blocked.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2011            bphillip    Initial creation
 * Oct 26, 2012 1287       rferrel     Force blocking of SiteActivationDlg.
 * Jan 26, 2015 5054       randerso    Use null shell as parent,
 *                                     Results in display as parent for jface dialogs
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ActivateSiteComponent extends AbstractAWIPSComponent {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#startInternal
     * (java.lang.String)
     */
    @Override
    protected void startInternal(String componentName) throws Exception {
        SiteActivationDlg dlg = new SiteActivationDlg(null);
        dlg.setBlockOnOpen(true);
        dlg.open();
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
