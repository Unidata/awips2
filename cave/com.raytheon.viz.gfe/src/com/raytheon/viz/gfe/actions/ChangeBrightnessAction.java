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

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.dialogs.BrightnessDialog;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Action class for dispalying the Change Brightness dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2010            wldougher     Initial creation
 * Oct 30, 2012 1298       rferrel     Changes for non-blocking BrightnessDialog.
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class ChangeBrightnessAction extends AbstractRightClickAction {
    private BrightnessDialog dialog;

    protected Parm parm;

    public ChangeBrightnessAction(Parm parm) {
        super("Brightness...");
        this.parm = parm;
    }

    @Override
    public void run() {
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            dialog = new BrightnessDialog(shell, parm);
            dialog.setBlockOnOpen(false);
            dialog.open();
        } else {
            dialog.bringToTop();
        }
    }

}
