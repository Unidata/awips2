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

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
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
 * Nov 15, 2012  1298      rferrel     Changes for non-blocking procedures.
 * Jun 25, 2013  16065     ryu         Passing outerLevel to smart tool job.
 * Dec 10, 2013  2367      dgilling    Use new SmartToolJobPool.
 * Jun 05, 2015  4259      njensen     Removed LD_PRELOAD check
 * Jul 17, 2015  4575      njensen     Changed varDict from String to Map
 * Jul 23, 2015  4263      dgilling    Support SmartToolMetadataManager.
 * Sep 16, 2015  4871      randerso    Return modified varDict from Tool
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartUtil {

    public static SmartToolRequest buildSmartToolRequest(DataManager dm,
            PreviewInfo preview, boolean outerLevel) {
        SmartToolRequest req = new SmartToolRequest();
        req.setPreview(preview);
        req.setOuterLevel(outerLevel);
        return req;
    }

    public static void runTool(String toolName) {
        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        List<FieldDefinition> varList = dm.getSmartToolInterface()
                .getVarDictWidgets(toolName);
        if ((varList == null) || (varList.size() == 0)) {
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

    public static Object[] callFromSmartScript(final DataManager dataMgr,
            final String toolName, final String elementName,
            ReferenceData editArea, TimeRange timeRange,
            Map<String, Object> varDict, boolean emptyEditAreaFlag,
            List<String> passErrors, String missingDataMode, Parm parm) {
        EditAction editAction = new EditAction(toolName, elementName,
                timeRange, editArea, emptyEditAreaFlag,
                MissingDataMode.valueFrom(missingDataMode));
        PreviewInfo pi = new PreviewInfo(editAction, passErrors, parm);
        final SmartToolRequest req = SmartUtil.buildSmartToolRequest(dataMgr,
                pi, false);

        final int[] returnCode = new int[1];
        if (varDict != null) {
            req.setVarDict(varDict);
            returnCode[0] = IDialogConstants.OK_ID;
        } else {
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    List<FieldDefinition> varList = dataMgr
                            .getSmartToolInterface()
                            .getVarDictWidgets(toolName);
                    if ((varList != null) && (varList.size() > 0)) {
                        /*
                         * The SelectionDlg changes based on the procedure.
                         * Since it is non-modal several dialogs may be
                         * displayed. This mimics the AWIPS 1 behavior.
                         */
                        SelectionDlg sd = new SelectionDlg(PlatformUI
                                .getWorkbench().getActiveWorkbenchWindow()
                                .getShell(), toolName, dataMgr, varList, true);

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
            dataMgr.getSmartToolJobPool().schedule(req);
            return new Object[] { req.getResult(), req.getVarDict() };
        }
        return new Object[] { null, null };
    }
}
