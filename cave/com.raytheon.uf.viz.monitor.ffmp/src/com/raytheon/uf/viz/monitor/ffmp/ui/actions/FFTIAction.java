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

import com.raytheon.uf.viz.monitor.ffmp.ffti.FFTIControlDlg;

/**
 * Kick off the FFTI dialog and application backend processes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 1/27/10       4265       dhladky     Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * 
 */
public class FFTIAction extends AbstractHandler {

    private FFTIControlDlg fftiControlDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        System.out.println("Activating/Action the FFTI dialogs...");

        if ((fftiControlDlg == null) || fftiControlDlg.isDisposed()) {
            fftiControlDlg = new FFTIControlDlg(shell);
            fftiControlDlg.open();
            fftiControlDlg = null;
        }
        return null;
    }

}
