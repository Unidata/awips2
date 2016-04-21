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

import com.raytheon.viz.awipstools.ui.dialog.PutHomeCursorDialog;
import com.raytheon.viz.awipstools.ui.layer.HomeToolLayer;

/**
 * Opens the {@link PutHomeCursorDialog} and loads the {@link HomeToolLayer}
 * bundles/tools/Home.xml localization file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 03, 2013  2310     bsteffen    Rewritten to extend HomeToolAction.
 * Apr 21, 2014  3041     lvenable    Added dispose check.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 2.0
 */
public class PutHomeCursorAction extends HomeToolAction {

    private PutHomeCursorDialog putHomeCursorDialog;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if (putHomeCursorDialog == null || putHomeCursorDialog.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            putHomeCursorDialog = new PutHomeCursorDialog(shell);
            putHomeCursorDialog.open();
        }

        return super.execute(arg0);
    }

}
