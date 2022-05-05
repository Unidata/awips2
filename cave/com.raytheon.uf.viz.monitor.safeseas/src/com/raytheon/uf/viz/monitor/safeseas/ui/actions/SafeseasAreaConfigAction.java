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
package com.raytheon.uf.viz.monitor.safeseas.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.monitor.safeseas.ui.dialogs.SSMonitoringAreaConfigDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * The SAFESEAS Action
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 28, 2009 3963       dhladky    Initial creation.
 * March 5, 2012 14413     zhao       Launch AreaConfigDlg w/o monitor
 * Nov.27, 2012 1297       skorolev   Cleanup code for non-blocking dialog.
 * May 08, 2014 3086       skorolev   Added CloseCallback to dialog.
 * Sep 16, 2014 2757       skorolev   Added test of dialog on dispose.
 * Sep 19, 2014 3220       skorolev   Added check on dispose.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class SafeseasAreaConfigAction extends AbstractHandler {

    /**
     * SAFESEAS Monitoring Area Configuration Dialog.
     */
    private SSMonitoringAreaConfigDlg configDlg;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if (configDlg == null || configDlg.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            configDlg = new SSMonitoringAreaConfigDlg(shell,
                    "SAFESEAS Monitor Area Configuration");
            configDlg.addCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    configDlg = null;
                }
            });
        }
        configDlg.open();
        return null;
    }
}
