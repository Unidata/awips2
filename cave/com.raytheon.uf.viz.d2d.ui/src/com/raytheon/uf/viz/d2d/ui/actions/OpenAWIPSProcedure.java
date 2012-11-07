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

import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.OpenProcedureListDlg;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureDlg;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.actions.LoadSerializedXml;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * OpenAWIPSProcedure
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Sep 13, 2007             chammack    Initial Creation.
 *    Oct 16, 2012 1229        rferrel     Change to use ProcedureDlg.displayDialog.
 *    Oct 16, 2012 1229        rferrel     Changes for non-blocking ProcedureListDlg.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class OpenAWIPSProcedure extends AbstractHandler {

    private OpenProcedureListDlg dialog;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            dialog = new OpenProcedureListDlg(HandlerUtil.getActiveShell(event));
            dialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile selectedFile = (LocalizationFile) returnValue;
                        File f = selectedFile.getFile();
                        Procedure p = (Procedure) LoadSerializedXml
                                .deserialize(f);
                        ProcedureDlg.displayDialog(LocalizationUtil
                                .extractName(selectedFile.getName()), p,
                                VizWorkbenchManager.getInstance()
                                        .getCurrentWindow().getShell());
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
