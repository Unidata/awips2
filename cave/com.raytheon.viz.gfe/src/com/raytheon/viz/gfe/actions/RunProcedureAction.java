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
package com.raytheon.viz.gfe.actions;

import java.util.List;

import jep.JepException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.procedures.ProcedureRequest;
import com.raytheon.viz.gfe.procedures.ProcedureSelectionDlg;
import com.raytheon.viz.gfe.procedures.ProcedureUtil;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
import com.raytheon.viz.gfe.smarttool.PreviewInfo;
import com.raytheon.viz.gfe.ui.runtimeui.SelectionDlg;

/**
 * Action that runs a procedure
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 04, 2008            njensen     Initial creation
 * Nov 15, 2012  #1298     rferrel     Changes for non-blocking ProcedureSelectionDlg.
 * Dec 09, 2013  #2367     dgilling    Use new ProcedureJobPool.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class RunProcedureAction extends AbstractHandler {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RunProcedureAction.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        String procedureName = event.getParameter("name");
        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        try {
            List<FieldDefinition> varList = dm.getProcedureInterface()
                    .getVarDictWidgets(procedureName);
            if (varList == null || varList.isEmpty()) {
                // no VariableList found on procedure, just run it
                PreviewInfo pi = ProcedureUtil.checkAndBuildPreview(dm,
                        procedureName);
                if (pi != null) {
                    ProcedureRequest req = ProcedureUtil.buildProcedureRequest(
                            procedureName, dm);
                    if (req != null) {
                        dm.getProcedureJobPool().schedule(req);
                    }
                }
            } else {
                // The ProcedureSelectionDlg changes based on the procedure.
                // Since it is non-modal several dialogs may be displayed. This
                // mimics the AWIPS 1 behavior.

                // make the gui, let it handle running the procedure
                SelectionDlg sd = new ProcedureSelectionDlg(PlatformUI
                        .getWorkbench().getActiveWorkbenchWindow().getShell(),
                        procedureName, dm, varList);
                sd.setBlockOnOpen(false);
                sd.open();
            }
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting VariableList", e);
        }

        return null;
    }

}
