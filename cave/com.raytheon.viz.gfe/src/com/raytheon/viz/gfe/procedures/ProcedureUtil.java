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

import jep.JepException;

import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
import com.raytheon.viz.gfe.smarttool.PreviewInfo;

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
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ProcedureUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProcedureUtil.class);

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

    public static Object callFromSmartScript(final DataManager dm,
            final String procName, ReferenceData editArea, TimeRange timeRange,
            String varDict) {
        PreviewInfo pi = dm.getEditActionProcessor().prepareExecute(
                "Procedure", procName, editArea, timeRange, false);

        final ProcedureRequest req = new ProcedureRequest();
        req.setProcedureName(procName);
        req.setPreview(pi);
        req.setRefSet(pi.getEditAction().getRefSet());
        req.setTimeRange(pi.getEditAction().getTimeRange());

        if (varDict != null) {
            req.setVarDict(varDict);
        } else {
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    try {
                        List<FieldDefinition> varList = dm
                                .getProcedureInterface().getVarDictWidgets(
                                        procName);
                        if (varList != null && varList.size() > 0) {
                            // make the gui, let it handle running the procedure
                            ProcedureSelectionBlockingDlg sd = new ProcedureSelectionBlockingDlg(
                                    PlatformUI.getWorkbench()
                                            .getActiveWorkbenchWindow()
                                            .getShell(), procName, dm, varList);
                            sd.open();
                            Map<String, Object> resultMap = sd
                                    .getVarDictResult();
                            if (resultMap != null) {
                                String userVarDict = dm.getProcedureInterface()
                                        .transformVarDict(resultMap);
                                req.setVarDict(userVarDict);
                            }
                        } else {
                            req.setVarDict(null);
                        }
                    } catch (JepException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error getting VariableList", e);
                    }
                }
            });
        }

        dm.getProcedureJobPool().schedule(req);
        return req.getResult();
    }
}
