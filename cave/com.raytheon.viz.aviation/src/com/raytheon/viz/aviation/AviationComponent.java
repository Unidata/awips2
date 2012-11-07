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
package com.raytheon.viz.aviation;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.personalities.awips.AbstractCAVEDialogComponent;

/**
 * This class starts AviationDialog as a stand alone component with runtime mode
 * of ALERT_VIZ.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2011            mschenke     Initial creation
 * Oct 08, 2012 1229       rferrel     Make a blocking dialog.
 * Oct 17, 2012 1229       rferrel     Changes for non-blocking
 *                                      AviationDialog.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AviationComponent extends AbstractCAVEDialogComponent {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#startInternal
     * (java.lang.String)
     */
    @Override
    protected void startInternal(String componentName) throws Exception {
        AviationDialog aviationDlg = new AviationDialog(new Shell(
                Display.getCurrent()));
        aviationDlg.open();
        blockUntilClosed(aviationDlg);
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
        return (ALERT_VIZ);
    }

}
