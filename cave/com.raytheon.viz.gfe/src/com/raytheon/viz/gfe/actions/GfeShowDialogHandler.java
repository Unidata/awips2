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

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * An abstract handler for showing GFE dialogs. Handlers that extend this will
 * have automatic benefits of non-blocking behavior and automatic cleanup of the
 * dialog reference.
 * 
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2015  4749      njensen     Initial creation
 *
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class GfeShowDialogHandler extends AbstractHandler {

    protected CaveJFACEDialog dialog;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        if (dm == null) {
            return null;
        }

        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            dialog = createDialog(shell, dm, event);
            dialog.setBlockOnOpen(false);
            dialog.addCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    dialog = null;
                }

            });
            dialog.open();
        } else {
            dialog.bringToTop();
        }
        return null;
    }

    /**
     * Creates a dialog based on the class and using the args passed in.
     * 
     * @param shell
     * @param dm
     * @param event
     * @return
     */
    protected abstract CaveJFACEDialog createDialog(Shell shell,
            DataManager dm, ExecutionEvent event);

}
