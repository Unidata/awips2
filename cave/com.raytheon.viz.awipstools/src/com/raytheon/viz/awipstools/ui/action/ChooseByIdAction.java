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
package com.raytheon.viz.awipstools.ui.action;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.awipstools.ui.dialog.ChooseByIdDialog;
import com.raytheon.viz.ui.actions.LoadBundleHandler;

/**
 * Opens the {@link ChooseByIdDialog} and loads several other tools from the
 * bundles/tools/ChooseById.xml localization file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 03, 2013  2310     bsteffen    Rewritten to extend LoadBundleHandler.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 2.0
 */
public class ChooseByIdAction extends LoadBundleHandler {

    private ChooseByIdDialog chooseByIdDialog;

    public ChooseByIdAction() {
        super("bundles/tools/ChooseById.xml");
    }

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if (chooseByIdDialog == null || chooseByIdDialog.isDisposed()) {
            chooseByIdDialog = new ChooseByIdDialog(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell());
            chooseByIdDialog.open();
        } else {

            // find and activate the dialog
            for (Shell s : chooseByIdDialog.getParent().getShells()) {
                if (s.getText().equals(ChooseByIdDialog.DIALOG_TITLE)) {
                    s.setVisible(true);
                    s.setActive();
                }
            }
        }
        return super.execute(arg0);
    }

}
