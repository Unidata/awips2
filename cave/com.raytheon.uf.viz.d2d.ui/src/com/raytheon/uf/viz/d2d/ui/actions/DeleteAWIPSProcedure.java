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

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.dialogs.localization.VizLocalizationFileListDlg;
import com.raytheon.viz.ui.dialogs.localization.VizLocalizationFileListDlg.Mode;

/**
 * Delete an AWIPS procedure
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Sep 13, 2007             chammack    Initial Creation.
 * Jul 8, 2008  #1183       chammack    Migrate to new localization
 * Oct 16, 2012 #1229       rferrel     Changes for non-blocking VizLocalizationFileListDlg.
 * Jun 02, 2015 #4401       bkowal      Updated to use {@link VizLocalizationFileListDlg}.
 * Jun 30, 2015 #4401       bkowal      Specify the localization type when constructing a
 *                                      {@link VizLocalizationFileListDlg}.
 * Nov 12, 2015 4834        njensen     Changed LocalizationOpFailedException to LocalizationException
 * Jan 11, 2016 5242        kbisanz     Replaced calls to deprecated LocalizationFile methods
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class DeleteAWIPSProcedure extends AbstractHandler {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DeleteAWIPSProcedure.class);

    private VizLocalizationFileListDlg listDlg;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (listDlg == null || listDlg.getShell() == null
                || listDlg.isDisposed()) {
            listDlg = new VizLocalizationFileListDlg("Delete Procedure",
                    HandlerUtil.getActiveShell(event), Mode.DELETE,
                    ProcedureDlg.PROCEDURES_DIR, "procedures",
                    LocalizationType.CAVE_STATIC);
            listDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof ILocalizationFile) {
                        ILocalizationFile selectedFile = (ILocalizationFile) returnValue;
                        try {
                            selectedFile.delete();
                        } catch (LocalizationException e) {
                            statusHandler.handle(
                                    Priority.PROBLEM,
                                    "Error deleting procedure: "
                                            + selectedFile.getPath());
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
