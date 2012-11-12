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
package com.raytheon.viz.gfe.textproduct.action;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.core.script.NewInputValidator;
import com.raytheon.viz.gfe.dialogs.NewTextProductDialog;

/**
 * Action for creating a new text product
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2008 1562       askripsky   Initial creation
 * Nov 12, 2012 1298       rferrel     Changes for non-blocking NewTextProductDialog.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class NewAction extends Action {

    /**
     * The action to create a new text product.
     */
    public NewAction() {
        super("New...");
    }

    /**
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        // Do this...
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        // The dialog being opened is modal to the parent dialog. This will
        // prevent the launching of another dialog until the modal dialog is
        // closed.
        NewTextProductDialog dlg = new NewTextProductDialog(shell,
                "New Text Product", new NewInputValidator());
        dlg.setBlockOnOpen(false);
        dlg.open();
    }
}
