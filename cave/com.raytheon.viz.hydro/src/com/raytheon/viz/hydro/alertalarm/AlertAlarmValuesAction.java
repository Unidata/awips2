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

package com.raytheon.viz.hydro.alertalarm;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

/**
 * Action for Alert Alarm Data Values Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 6/27/07                  lvenable    Initial creation.
 * 12/05/12     1353        rferrel     Changes for non-blocking AlertAlarmValuesDlg.
 * 03/27/2013   1790        rferrel     Bug fix for non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class AlertAlarmValuesAction extends AbstractHandler {
    private AlertAlarmValuesDlg alertAlarmDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        if (alertAlarmDlg == null || alertAlarmDlg.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            alertAlarmDlg = new AlertAlarmValuesDlg(shell);
            alertAlarmDlg.open();
        } else {
            alertAlarmDlg.bringToTop();
        }

        return null;
    }

}
