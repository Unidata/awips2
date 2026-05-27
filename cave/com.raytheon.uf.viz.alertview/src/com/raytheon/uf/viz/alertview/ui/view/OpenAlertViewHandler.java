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
package com.raytheon.uf.viz.alertview.ui.view;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.alertview.Alert;

/**
 * 
 * {@link IHandler} that opens an {@link AlertView}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 18, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class OpenAlertViewHandler extends AbstractHandler {

    private final Alert alert;

    public OpenAlertViewHandler() {
        this.alert = null;
    }

    public OpenAlertViewHandler(Alert alert) {
        this.alert = alert;
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        openInAlertView(alert);
        return null;
    }

    public static void openInAlertView(Alert alert) {
        IWorkbench workbench = PlatformUI.getWorkbench();
        IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
        if (workbench.isStarting()) {
            workbench.addWindowListener(new ShowAlertViewWindowListener(alert));
        } else if (window != null) {
            AlertView.show(window, alert);
        }
    }

    /**
     * If the user clicks on the alert popup while the workbench is starting
     * then this listener will be used to wait until the workbench window is
     * opened and then display the alert.
     */
    private static class ShowAlertViewWindowListener implements IWindowListener {

        private final Alert alert;

        public ShowAlertViewWindowListener(Alert alert) {
            this.alert = alert;
        }

        @Override
        public void windowOpened(IWorkbenchWindow window) {
            AlertView.show(window, alert);
            window.getWorkbench().removeWindowListener(this);
        }

        @Override
        public void windowDeactivated(IWorkbenchWindow window) {
            /* Unused method required by interface. */
        }

        @Override
        public void windowClosed(IWorkbenchWindow window) {
            /* Unused method required by interface. */
        }

        @Override
        public void windowActivated(IWorkbenchWindow window) {
            /* Unused method required by interface. */
        }
    }

}
