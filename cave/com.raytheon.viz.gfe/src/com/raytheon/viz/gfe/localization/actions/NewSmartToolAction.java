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
package com.raytheon.viz.gfe.localization.actions;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.localization.dialogs.NewInputValidator;
import com.raytheon.viz.gfe.localization.dialogs.NewToolDialog;
import com.raytheon.viz.gfe.localization.util.AbstractScriptUtil;

/**
 * Action for creating a new smart tool
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 21, 2008           njensen   Initial creation
 * Nov 12, 2012  1298     rferrel   Changes for non-blocking NewToolDialog.
 * Aug 11, 2016  5816     randerso  Moved to gfe.localization.actions. Code
 *                                  refactored and cleaned up
 * 
 * </pre>
 * 
 * @author njensen
 */

public class NewSmartToolAction extends NewAction {

    /**
     * Constructor
     * 
     * @param util
     */
    public NewSmartToolAction(AbstractScriptUtil util) {
        super(util);
    }

    @Override
    public void run() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        // The dialog being opened is modal to the parent dialog. This will
        // prevent the launching of another dialog until the modal dialog is
        // closed.
        NewToolDialog d = new NewToolDialog(shell, "MyTool",
                new NewInputValidator(), util);
        d.setBlockOnOpen(false);
        d.open();
    }
}
