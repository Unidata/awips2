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
package com.raytheon.viz.mpe.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.mpe.ui.dialogs.gagetable.GageTableDlg;

/**
 * Launch action for the Gage Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 28, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GageTableAction extends AbstractHandler {
    private Cursor waitCursor = null;

    private Cursor arrowCursor = null;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        waitCursor = new Cursor(shell.getDisplay(), SWT.CURSOR_WAIT);
        arrowCursor = new Cursor(shell.getDisplay(), SWT.CURSOR_ARROW);
        
        shell.setCursor(waitCursor);
        GageTableDlg gageTable = new GageTableDlg();
        gageTable.open();
//        try {
//            gageTable.showGageTable();
//        } catch (ParseException e) {
//            e.printStackTrace();
//            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
//                    | SWT.OK);
//            mb.setText("Error");
//            mb
//                    .setMessage("Unable to open Gage Table Dialog.");
//            mb.open();
//        } catch (IOException e) {
//            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
//                    | SWT.OK);
//            mb.setText("Error");
//            mb
//                    .setMessage("Unable to open Gage Table Dialog.");
//            mb.open();
//            e.printStackTrace();
//        }
        shell.setCursor(arrowCursor);
        
        return null;
    }

}
