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
package com.raytheon.uf.viz.monitor.ffmp.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.monitor.ffmp.fffg.FFFGDlg;

/**
 * Kick off the FFMP dialog and application backend processes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 1/27/09                  dhladky    Initial Creation.
 * 1/29/2012    1353        rferrel     Changes for non-blocking FFFGDlg.
 * 
 * </pre>
 * 
 * @author dhladky
 * 
 */
public class FFFGAction extends AbstractHandler {

    private FFFGDlg fffgDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        // Independent dialog is disposed on close and cannot be reopened.
        if (fffgDlg == null || fffgDlg.getShell() == null
                || fffgDlg.isDisposed()) {
            Shell fshell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            fffgDlg = new FFFGDlg(fshell);
        }
        fffgDlg.open();

        return null;
    }

}
