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
package com.raytheon.viz.gfe.smarttool;

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
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
import com.raytheon.viz.gfe.smarttool.script.SmartToolBlockingSelectionDlg;
import com.raytheon.viz.gfe.smarttool.script.SmartToolRequest;
import com.raytheon.viz.gfe.smarttool.script.SmartToolSelectionDlg;
import com.raytheon.viz.gfe.ui.runtimeui.SelectionDlg;

/**
 * Utilities for smart tools
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2008            njensen     Initial creation
 * Dec 1,  2009  1426      ryu         Add time range warning
 * Nov 15, 2012 1298       rferrel     Changes for non-blocking prcedures.
 * Jun 25, 2013  16065     ryu         Passing outerLevel to smart tool job.
 * Dec 10, 2013  #2367     dgilling    Use new SmartToolJobPool.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SmartUtil.class);

    /**
     * Checks if LD_PRELOAD is set in the environment. If not, jep may have
     * issues importing modules. (Note that this presumes LD_PRELOAD was set
     * correctly to point at the python .so file).
     * 
     * @return if LD_PRELOAD is set
     */
    public static boolean isLdPreloadSet() {
        String preload = System.getenv().get("LD_PRELOAD");
        boolean set = false;
        if (preload != null) {
            set = true;
        }

        return set;
    }

    public static SmartToolRequest buildSmartToolRequest(DataManager dm,
            PreviewInfo preview, boolean outerLevel) {
        SmartToolRequest req = new SmartToolRequest();
        req.setPreview(preview);
        req.setOuterLevel(outerLevel);
        return req;
    }

    public static void runTool(String toolName) {
        DataManager dm = DataManager.getCurrentInstance();
        List<FieldDefinition> varList = null;
        try {
            varList = dm.getSmartToolInterface().getVarDictWidgets(toolName);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting VariableList for procedure: " + toolName, e);
        }
        if (varList == null || varList.size() == 0) {
            runToolNoVarDict(dm, toolName);
        } else {
            // The SmartToolSelectionDlg changes based on the procedure.
            // Since it is non-modal several dialogs may be displayed. This
            // mimics the AWIPS 1 behavior.

            // make the gui, let it handle running the tool
            SelectionDlg sd = new SmartToolSelectionDlg(PlatformUI
                    .getWorkbench().getActiveWorkbenchWindow().getShell(),
                    toolName, dm, varList);
            sd.setBlockOnOpen(false);
            sd.open();
        }
    }

    private static void runToolNoVarDict(DataManager dm, String toolName) {
        PreviewInfo pi = checkAndBuildPreview(dm, toolName);
        if (pi != null) {
            SmartToolRequest req = buildSmartToolRequest(dm, pi, true);
            if (req != null) {
                dm.getSmartToolJobPool().schedule(req);
            }
        }
    }

    public static PreviewInfo checkAndBuildPreview(DataManager dm,
            String toolName) {
        ReferenceData ref = dm.getRefManager().getActiveRefSet();
        // prepareExecute() will fill in the time range.
        // Filling it in here prevents "TimeRange" warnings from being
        // generated.
        TimeRange tr = null;
        // TimeRange tr = dm.getSpatialDisplayManager().getGlobalTimeRange();
        PreviewInfo pi = dm.getEditActionProcessor().prepareExecute("Tool",
                toolName, ref, tr, true);
        return pi;
    }

    public static Object callFromSmartScript(final DataManager dm,
            final String toolName, final String elementName,
            ReferenceData editArea, TimeRange timeRange, String varDict,
            boolean emptyEditAreaFlag, List<String> passErrors,
            String missingDataMode, Parm parm) {
        EditAction editAction = new EditAction(toolName, elementName,
                timeRange, editArea, emptyEditAreaFlag,
                MissingDataMode.valueFrom(missingDataMode));
        PreviewInfo pi = new PreviewInfo(editAction, passErrors, parm);
        final SmartToolRequest req = SmartUtil.buildSmartToolRequest(dm, pi,
                false);

        if (varDict != null) {
            req.setVarDict(varDict);
        } else {
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    try {
                        List<FieldDefinition> varList = dm
                                .getSmartToolInterface().getVarDictWidgets(
                                        toolName);
                        if (varList != null && varList.size() > 0) {
                            // The SmartToolBlockingSelectionDlg changes based
                            // on the procedure. Since it is non-modal several
                            // dialogs may be displayed. This mimics the AWIPS 1
                            // behavior.

                            // make the gui, let it handle running the procedure
                            SmartToolBlockingSelectionDlg sd = new SmartToolBlockingSelectionDlg(
                                    PlatformUI.getWorkbench()
                                            .getActiveWorkbenchWindow()
                                            .getShell(), toolName, dm, varList);

                            // must block because this method needs the results
                            // to determine what to return.
                            sd.setBlockOnOpen(true);
                            sd.open();
                            Map<String, Object> resultMap = sd
                                    .getVarDictResult();
                            if (resultMap != null) {
                                String userVarDict = dm.getSmartToolInterface()
                                        .transformVarDict(resultMap);
                                req.setVarDict(userVarDict);
                            }
                        } else {
                            // set it to something so we don't trigger the null
                            // == user cancelled below
                            req.setVarDict("{}");
                        }
                    } catch (JepException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error getting VariableList for procedure: "
                                        + toolName, e);
                    }
                }
            });
        }

        dm.getSmartToolJobPool().schedule(req);
        return req.getResult();
    }
}
