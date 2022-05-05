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
package com.raytheon.viz.gfe.procedures;

import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
import com.raytheon.viz.gfe.smarttool.PreviewInfo;
import com.raytheon.viz.gfe.ui.runtimeui.SelectionDlg;

/**
 * Utilities for GFE procedures
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 09, 2010            njensen     Initial creation
 * Apr 26, 2012  14748     ryu         Use edit area and time range from preview info
 * Dec 09, 2013  #2367     dgilling    Use new ProcedureJobPool.
 * Jul 17, 2015  4575      njensen     Changed varDict from String to Map
 * Jul 27, 2015  4263      dgilling    Support ProcedureMetadataManager.
 * Sep 16, 2015  4871      randerso    Return modified varDict from Procedure
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ProcedureUtil {

    public static ProcedureRequest buildProcedureRequest(String procedureName,
            DataManager dataMgr) {
        ProcedureRequest req = new ProcedureRequest();
        req.setProcedureName(procedureName);
        req.setRefSet(dataMgr.getRefManager().getActiveRefSet());
        req.setTimeRange(dataMgr.getSpatialDisplayManager()
                .getGlobalTimeRange());
        return req;
    }

    public static PreviewInfo checkAndBuildPreview(DataManager dm,
            String procName) {
        ReferenceData ref = dm.getRefManager().getActiveRefSet();
        TimeRange tr = dm.getSpatialDisplayManager().getGlobalTimeRange();
        PreviewInfo pi = dm.getEditActionProcessor().prepareExecute(
                "Procedure", procName, ref, tr, false);
        return pi;
    }

    public static Object[] callFromSmartScript(final DataManager dm,
            final String procName, ReferenceData editArea, TimeRange timeRange,
            Map<String, Object> varDict) {
        PreviewInfo pi = dm.getEditActionProcessor().prepareExecute(
                "Procedure", procName, editArea, timeRange, false);

        final ProcedureRequest req = new ProcedureRequest();
        req.setProcedureName(procName);
        req.setPreview(pi);
        req.setRefSet(pi.getEditAction().getRefSet());
        req.setTimeRange(pi.getEditAction().getTimeRange());
        req.setVarDict(varDict);

        final int[] returnCode = new int[1];
        if (varDict != null && (!varDict.isEmpty())) {
            returnCode[0] = IDialogConstants.OK_ID;
        } else {
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    List<FieldDefinition> varList = dm.getProcedureInterface()
                            .getVarDictWidgets(procName);
                    if ((varList != null) && (!varList.isEmpty())) {
                        /*
                         * The SelectionDlg changes based on the procedure.
                         * Since it is non-modal several dialogs may be
                         * displayed. This mimics the AWIPS 1 behavior.
                         */
                        SelectionDlg sd = new SelectionDlg(PlatformUI
                                .getWorkbench().getActiveWorkbenchWindow()
                                .getShell(), procName, dm, varList, true);

                        /*
                         * must block because this method needs the results to
                         * determine what to return.
                         */
                        sd.setBlockOnOpen(true);
                        sd.open();
                        returnCode[0] = sd.getReturnCode();
                        req.setVarDict(sd.getValues());
                    }
                }
            });
        }

        if (returnCode[0] == IDialogConstants.OK_ID) {
            dm.getProcedureJobPool().schedule(req);
            return new Object[] { req.getResult(), req.getVarDict() };
        }
        return new Object[] { null, null };
    }
}
