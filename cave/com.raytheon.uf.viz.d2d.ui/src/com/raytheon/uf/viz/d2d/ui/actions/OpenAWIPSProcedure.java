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

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureDlg;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.actions.LoadPerspectiveHandler;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.dialogs.localization.VizLocalizationFileListDlg;
import com.raytheon.viz.ui.dialogs.localization.VizOpenLocalizationFileListDlg;

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
 *    Oct 16, 2012 1229        rferrel     Changes for non-blocking VizLocalizationFileListDlg.
 *    Jun 07, 2013 2074        mnash       Don't open the dialog if no procedures are deserialized
 *    Aug 11, 2014 3480        bclement    added logging
 *    Jun 02, 2015 #4401       bkowal      Updated to use {@link VizOpenLocalizationFileListDlg}.
 *    Jun 30, 2015 #4401       bkowal      Specify the localization type when constructing a
 *                                         {@link VizOpenLocalizationFileListDlg}.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class OpenAWIPSProcedure extends AbstractHandler {

    private VizOpenLocalizationFileListDlg dialog;

    private static final IUFStatusHandler log = UFStatus
            .getHandler(OpenAWIPSProcedure.class);

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
            dialog = new VizOpenLocalizationFileListDlg("Open Procedure",
                    HandlerUtil.getActiveShell(event),
                    ProcedureDlg.PROCEDURES_DIR, "procedures",
                    LocalizationType.CAVE_STATIC);
            dialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile selectedFile = (LocalizationFile) returnValue;
                        File f = selectedFile.getFile();
                        Procedure p = (Procedure) LoadPerspectiveHandler
                                .deserialize(f);
                        if (p != null) {
                            log.info("Loading display file: "
                                    + f.getAbsolutePath());
                            ProcedureDlg.displayDialog(LocalizationUtil
                                    .extractName(selectedFile.getName()), p,
                                    VizWorkbenchManager.getInstance()
                                            .getCurrentWindow().getShell());
                        }
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
