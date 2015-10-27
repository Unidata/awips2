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
package com.raytheon.viz.gfe.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.dialogs.isc.ISCRequestReplyDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;

/**
 * Action to launch ISC request/reply dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2015  #4858     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ShowIscRequestReplyDialog extends AbstractHandler {

    private ISCRequestReplyDlg iscRequestDlg;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        if (!SimulatedTimeOperations.isTransmitAllowed()) {
            SimulatedTimeOperations.displayFeatureLevelWarning(shell,
                    "ISC Request/Reply");
            return null;
        }

        if (iscRequestDlg == null || iscRequestDlg.getShell() == null
                || iscRequestDlg.isDisposed()) {
            iscRequestDlg = new ISCRequestReplyDlg(shell);
            iscRequestDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    iscRequestDlg = null;

                }
            });
            iscRequestDlg.open();
        } else {
            iscRequestDlg.bringToTop();
        }

        return null;
    }

    @Override
    public boolean isEnabled() {
        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        if (dm != null) {
            return CAVEMode.getMode().equals(CAVEMode.OPERATIONAL)
                    && dm.requestISC();
        }

        return false;
    }
}
