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
package com.raytheon.viz.hydro.appsdefaults;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2011            mpduff     Initial creation
 * Dec 06, 2012 1353       rferrel     Changes for non blocking GetAppsDefaults.
 *                                      Changes for non blocking SHEFAppsDefaultsDlg.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class AppsDefaultsAction extends AbstractHandler {
    private GetAppsDefaults gad;

    private SHEFAppsDefaultsDlg dlg;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        String action = event.getParameter("Action");
        if (action.equals("gad")) {
            if (gad == null) {
                gad = new GetAppsDefaults(shell);
            }
            gad.open();
        } else {
            if (dlg == null) {
                dlg = new SHEFAppsDefaultsDlg(shell);
            }
            dlg.open();
        }
        return null;
    }

}
