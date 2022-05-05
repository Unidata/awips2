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

package com.raytheon.viz.texteditor;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.texteditor.dialogs.TextEditorDialog;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog.CAVE;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Text Editor Action: opens a new Text Editor Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 6/27/06                  randerso   Initial creation.
 * 10/11/2007   482         grichard   Reformatted file.
 * 01/18/2016   5045        randerso   Merged CaveSWTDialog and CaveSWTDialogBase
 * Jan 29, 2017 6944        tgurney    Allow only one editor to be open at a time
 *
 * </pre>
 *
 * @author randerso
 *
 */
public class TextEditorAction extends AbstractHandler {

    private static TextEditorDialog textEditorDlg;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        if ((textEditorDlg == null) || textEditorDlg.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            synchronized (TextEditorDialog.class) {
                textEditorDlg = new TextEditorDialog(shell,
                        CAVE.INDEPENDENT_SHELL);
            }
            textEditorDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    synchronized (TextEditorDialog.class) {
                        textEditorDlg = null;
                    }
                }
            });
            textEditorDlg.open();
        } else {
            textEditorDlg.bringToTop();
        }

        return null;
    }
}
