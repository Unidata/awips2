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
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm.InterpState;
import com.raytheon.viz.gfe.core.parm.ParmState.InterpMode;
import com.raytheon.viz.gfe.dialogs.GridsInterpolateDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Handler to show Grid Interpolation Dialog and initiate interpolation
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer        Description
 * ------------ ----------  --------------  --------------------------
 * Feb 27, 2008             Eric Babin      Initial Creation
 * Jun  4, 2008    #1161    Ron Anderson    Reworked
 * Oct 25, 2012 #1287       rferrel         Changes for non-blocking GridsInterpolateDialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ShowGridsInterpolationDialog extends AbstractHandler {
    private GridsInterpolateDialog dialog;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            dialog = new GridsInterpolateDialog(shell);
            dialog.setBlockOnOpen(false);
            dialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof Integer) {
                        int returnCode = (Integer) returnValue;
                        if (returnCode == IDialogConstants.OK_ID) {
                            InterpMode interpMode = dialog.getInterpMode();
                            int interval = dialog.getInterval() * 3600;
                            int duration = dialog.getDuration() * 3600;

                            DataManager
                                    .getCurrentInstance()
                                    .getParmOp()
                                    .interpolateSelected(interpMode,
                                            InterpState.ASYNC, interval,
                                            duration);
                        }
                    }
                    dialog = null;
                }
            });
            dialog.open();
        } else {
            dialog.bringToTop();
        }

        return null;
    }
}
