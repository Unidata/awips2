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
package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureListDlg;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureListDlg.Mode;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * DeleteAWIPSProcedure
 * 
 * Delete an AWIPS procedure
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Sep 13, 2007             chammack    Initial Creation.
 *    Jul 8, 2008  #1183       chammack    Migrate to new localization
 *    Oct 16, 2012 #1229       rferrel     Changes for non-blocking ProcedureListDlg.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class DeleteAWIPSProcedure extends AbstractHandler {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DeleteAWIPSProcedure.class);

    private ProcedureListDlg listDlg;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (listDlg == null || listDlg.getShell() == null
                || listDlg.isDisposed()) {
            listDlg = new ProcedureListDlg("Delete Procedure",
                    HandlerUtil.getActiveShell(event), Mode.DELETE);
            listDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile selectedFile = (LocalizationFile) returnValue;
                        try {
                            selectedFile.delete();
                        } catch (LocalizationOpFailedException e) {
                            statusHandler.handle(
                                    Priority.PROBLEM,
                                    "Error deleting procedure: "
                                            + selectedFile.getName());
                        }
                    }
                }
            });
            listDlg.open();
        } else {
            listDlg.bringToTop();
        }

        return null;
    }

}
