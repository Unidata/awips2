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

import com.raytheon.viz.gfe.dialogs.FormatterLauncherDialog;

/**
 * TODO Add Description FormatterlauncherAction.java Apr 11, 2008
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Apr 11, 2008					Eric Babin Initial Creation
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class FormatterlauncherAction extends AbstractHandler {

    private static volatile FormatterLauncherDialog dialog = null;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        if (dialog == null) {
            dialog = new FormatterLauncherDialog(shell);
            dialog.setBlockOnOpen(true);
            dialog.open();
            dialog.dispose();
            dialog = null;
        } else if (dialog.getShell() == null) {
            dialog.open();
        } else {
            dialog.getShell().setVisible(true);
            dialog.getShell().setActive();
            if (dialog.buttonBar != null) {
                dialog.buttonBar.moveAbove(null);
            }
        }
        return null;
    }
    
    public static FormatterLauncherDialog getFormatterLauncher() {
        return dialog;
    }
}
