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

import com.raytheon.viz.gfe.dialogs.DisplayAttributesDialog;
import com.raytheon.viz.gfe.rsc.GFEResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Action to bring up the Dispaly attributes dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2009            mschenke     Initial creation
 * Oct 22, 2012 1287       rferrel     Changes for non-blocking DisplayAttributesDialog.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DisplayAttributesAction extends AbstractRightClickAction {

    private GFEResource rsc;

    private DisplayAttributesDialog dialog;

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            rsc = ((GFEResource) this.getSelectedRsc());

            dialog = new DisplayAttributesDialog(shell, rsc);
            dialog.setBlockOnOpen(false);
            dialog.open();
        } else {
            dialog.bringToTop();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {

        return "Display Attributes";
    }
}
