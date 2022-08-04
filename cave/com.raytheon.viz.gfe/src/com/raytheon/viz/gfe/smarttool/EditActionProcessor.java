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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.HighlightMsg;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.ShowEmptyEditAreaWarningMsg;
import com.raytheon.viz.gfe.core.msgs.ShowTimeRangeWarningMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.dialogs.EmptyEditAreaWarningDialog;
import com.raytheon.viz.gfe.dialogs.TimeRangeWarningDialog;

/**
 * Partially ported from Awips 1. Somewhat modified/changed to make work with
 * A2.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 03, 2010           njensen   Initial creation
 * Nov 07, 2012  1298     rferrel   Keep EmptyEditAreaWarningDialog blocking.
 *                                  Keep TimeRangeWarningdialog blocking.
 * Jan 08, 2013  1486     dgilling  Support changes to BaseGfePyController.
 * Mar 14, 2014  15813    ryu       Fixed default time range used.
 * Jul 23, 2015  4263     dgilling  Support SmartToolMetadataManager.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Feb 07, 2018  6882     randerso  Changed to use ReferenceData.isEmpty()
 *
 * </pre>
 *
 * @author njensen
 */

public class EditActionProcessor {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditActionProcessor.class);

    private DataManager dataMgr;

    private String previewColor;

    private int returnCode;

    /**
     * Constructor
     *
     * @param dm
     */
    public EditActionProcessor(DataManager dm) {
        this.dataMgr = dm;
        previewColor = GFEPreference.getString("TimeBlockPreview_color");
    }

    private void busyCursor() {
        if (PlatformUI.isWorkbenchRunning()) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    IWorkbenchWindow window = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow();
                    Shell shell = window.getShell();
                    shell.setCursor(shell.getDisplay()
                            .getSystemCursor(SWT.CURSOR_WAIT));
                }
            });
        }
    }

    private void normalCursor() {
        if (PlatformUI.isWorkbenchRunning()) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    IWorkbenchWindow window = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow();
                    window.getShell().setCursor(window.getShell().getDisplay()
                            .getSystemCursor(SWT.CURSOR_ARROW));
                }

            });
        }
    }

    private PreviewInfo preview(String itemKind, final String itemName,
            boolean highlightGrids, ReferenceData useEditArea,
            TimeRange useTimeRange) {
        // Get preview information for a tool or procedure
        // Check for ability to execute itemName
        if ((itemName == null) || itemName.isEmpty()) {
            return null;
        }

        // Determine the Parms that would be affected
        Parm parm = null;
        String element = null;
        Parm effectiveParm = null;
        if ("Tool".equals(itemKind)) {
            element = dataMgr.getSmartToolInterface()
                    .getWeatherElementEdited(itemName);
            if (element == null) {
                processError("ExecuteOrClassError",
                        "Tool Not Found: " + itemName, null);
                return null;
            }
            if (!"variableElement".equals(element) && !"None".equals(element)) {
                Parm activeParm = dataMgr.getSpatialDisplayManager()
                        .getActivatedParm();
                if ((activeParm == null)
                        || !element.equals(activeParm.expressionName())) {
                    String msg = itemName + " modifies " + element + ".  ";
                    msg += "Make this the Editable Weather Element to run the tool.";
                    processError("ExecuteOrClassError", msg, null);
                    return null;
                }
            }

            effectiveParm = dataMgr.getParmManager().getParmInExpr(element,
                    true);
            if ((effectiveParm != null) || "None".equals(element)) {
                parm = effectiveParm;
            } else {
                String message = "No Weather Element Set To Edit. ";
                processError("ExecuteOrClassError", message, null);
                return null;
            }
        } else if ("Procedure".equals(itemKind)) {
            effectiveParm = null;
            element = "";
        } else {
            String message = "Non-executable Item: " + itemName;
            processError("ExecuteOrClassError", message, null);
            return null;
        }

        final List<String> warnings = new ArrayList<>();
        // Determine the edit area that would be affected
        boolean emptyEditAreaFlag = false;
        ReferenceData refset = null;
        if ((useEditArea != null) && !useEditArea.isEmpty()) {
            refset = useEditArea;
        } else {
            refset = determineEditArea(warnings);
            if ((refset == null) || refset.isEmpty()) {
                emptyEditAreaFlag = true;
            }
        }

        // Determine the time range that would be affected
        TimeRange timeRange = null;
        if ((useTimeRange != null) && useTimeRange.isValid()) {
            timeRange = useTimeRange;
        } else {
            timeRange = determineTimeRange(effectiveParm, warnings);
        }

        // Sort warnings
        Collections.sort(warnings);

        // Save the preview information
        // Check for variableElement and substitute the editable
        // element's expression name
        if ("variableElement".equals(element)) {
            Parm active = dataMgr.getSpatialDisplayManager().getActivatedParm();
            if (active != null) {
                element = active.expressionName();
            }
        }

        // Create the PreviewInfo
        // varList = self.__editActionMgr.varList(itemName);
        EditAction editAction = new EditAction(itemName, element, timeRange,
                refset, emptyEditAreaFlag);
        PreviewInfo previewInfo = new PreviewInfo(editAction, warnings, parm);

        busyCursor();
        if (highlightGrids) {
            // This following if block is a deviation from AWIPS1. In
            // AWIPS 1, the variable parm was created as a list and when
            // weatherElementEdited = None the list had a single element, None.
            // Since we have just the single Parm instance in this code, the
            // null check would cause the function to return null and smart
            // tools that edit no weather elements would fail to run.
            if (((previewInfo.getParm() == null) && !"None".equals(element))
                    || (previewInfo.getEditAction().getTimeRange() == null)) {
                return null;
            }
            Parm p = previewInfo.getParm();
            if (p != null) {
                HighlightMsg msg = new HighlightMsg(p,
                        new TimeRange[] {
                                previewInfo.getEditAction().getTimeRange() },
                        true, previewColor);
                msg.send();
            }
        }

        return previewInfo;
    }

    private void erasePreview(PreviewInfo previewInfo) {
        normalCursor();
        if ((previewInfo != null) && (previewInfo.getParm() != null)) {
            HighlightMsg msg = new HighlightMsg(previewInfo.getParm(),
                    new TimeRange[] {
                            previewInfo.getEditAction().getTimeRange() },
                    false, previewColor);
            msg.send();
        }
    }

    private ReferenceData determineEditArea(List<String> warnings) {
        // Determine the Edit Area to use

        // If pre-defined in Edit Area in varDict, load it
        // Otherwise, use active ref set
        ReferenceData refset = dataMgr.getRefManager().getActiveRefSet();
        if ((refset == null) || refset.isEmpty()) {
            warnings.add("EmptyEditArea");
        }

        return refset;
    }

    private TimeRange determineTimeRange(Parm parm, List<String> warnings) {
        // Determine the Time Range to use

        // TimeRange = Determined by EditActionProcessor
        // per call to Tool or Procedure
        // TimeInfluence = Determined by Tool
        // per call to method --
        // Abstime i.e. start time of current grid
        // Try to use selectionTimeRange
        TimeRange timeRange = dataMgr.getParmOp().getSelectionTimeRange();
        if ((timeRange != null) && timeRange.isValid()) {
            // Check to make sure it intersects SETime so results will
            // be visible
            Date seTime = dataMgr.getSpatialDisplayManager()
                    .getSpatialEditorTime();
            if (!timeRange.contains(seTime)) {
                warnings.add("SETimeSync");
            }
        } else {
            // Try to use TimeRange of Parm being edited or active parm
            timeRange = null;
            if (parm != null) {
                timeRange = parm.getParmState().getSelectedTimeRange();
                if (!timeRange.isValid()) {
                    timeRange = null;
                }
            }
            // Use SE Time
            if (timeRange == null) {
                // Use SE Time
                Date seTime = dataMgr.getSpatialDisplayManager()
                        .getSpatialEditorTime();
                if (seTime != null) {
                    timeRange = new TimeRange(seTime, (10 * 1000));
                }
            }
        }

        // If only one grid will be affected,
        // do not put out time range warning
        if ((timeRange != null) && (parm != null)) {
            IGridData[] grids = parm.getGridInventory(timeRange);
            // Return a warning flag if time range could
            // span multiple grids
            if (grids.length >= 2) {
                warnings.add("TimeRange");
            }
        }

        if (timeRange == null) {
            warnings.add("EmptyTimeRange");
        }
        return timeRange;
    }

    private void processError(String errorType, String errorInfo,
            List<String> passErrors) {
        // Report error unless it is to be passed back to
        // the caller for reporting
        // Return the error and its info

        if ((passErrors == null)
                || ((passErrors != null) && !passErrors.contains(errorType))) {
            boolean tb = false;
            if ("StandardError".equals(errorType)) {
                tb = true;
            }
            handleError(errorInfo, tb);
        }
        // return Exceptions.EditActionError(errorType,errorInfo);
    }

    private void handleError(String errorMsg, boolean tracebackFlag) {
        statusHandler.handle(Priority.PROBLEM, errorMsg);
    }

    /**
     * Prepare to execute smart tool
     *
     * @param itemKind
     * @param itemName
     * @param refData
     * @param timeRange
     * @param highlightGrids
     * @return the preview info
     */
    public PreviewInfo prepareExecute(String itemKind, String itemName,
            ReferenceData refData, TimeRange timeRange,
            boolean highlightGrids) {
        // Prepare to execute item
        // Return previewInfo and modes
        ReferenceData useEditArea = null;
        TimeRange useTimeRange = null;

        // Set up Time Range, Edit Area, modes for running
        // tool or procedure
        // Display appropriate warnings or error messages
        if (!refData.equals(new ReferenceData())
                && !refData.equals(dataMgr.getRefManager().emptyRefSet())) {
            useEditArea = refData;
        }
        if (!((new TimeRange()).equals(timeRange))) {
            useTimeRange = timeRange;
        }
        PreviewInfo previewInfo = preview(itemKind, itemName, highlightGrids,
                useEditArea, useTimeRange);
        if (previewInfo == null) {
            return null;
        }

        // If current action is different from last one,
        // show any warnings that were detected by preview
        if (previewInfo.getWarnings().size() > 0) {
            List<String> warnings = editWarnings(previewInfo.getWarnings(),
                    itemKind, itemName);
            for (String warning : warnings) {
                // if (!warnings.contains(warning))
                // continue;
                if (!warnUser(warning)) {
                    // Cancel the request
                    erasePreview(previewInfo);
                    return null;
                }
            }
        }

        // Use Full Area if proceeded with Empty Edit Area Warning
        if (previewInfo.getWarnings().contains("EmptyEditArea")) {
            previewInfo.getEditAction()
                    .setRefSet(dataMgr.getRefManager().fullRefSet());
        }

        // Get modes
        // modes = getModes(argList);
        return previewInfo;
    }

    private List<String> editWarnings(List<String> warnings, String actionType,
            final String name) {
        final List<String> checkedList = new ArrayList<>();
        if (warnings.contains("ALL")) {
            checkedList.add("TimeRange");
            checkedList.add("EmptyTimeRange");
            checkedList.add("SETimeSync");
            checkedList.add("EmptyEditArea");
            checkedList.add("ActiveElement");
        } else {
            checkedList.addAll(warnings);
        }

        if ("Procedure".equals(actionType)
                && (checkedList.contains("EmptyTimeRange")
                        || checkedList.contains("EmptyEditArea"))) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    Collection<String> argList = dataMgr.getProcedureInterface()
                            .getMethodArguments(name);
                    if (!argList.contains("timeRange")
                            && checkedList.contains("EmptyTimeRange")) {
                        checkedList.remove("EmptyTimeRange");
                    }
                    if (!argList.contains("editArea")
                            && checkedList.contains("EmptyEditArea")) {
                        checkedList.remove("EmptyEditArea");
                    }
                }

            });
        }

        return checkedList;
    }

    /**
     * Wrap up execute
     *
     * @param previewInfo
     * @param highlightGrids
     */
    public void wrapUpExecute(PreviewInfo previewInfo, boolean highlightGrids) {
        if (highlightGrids && (previewInfo != null)) {
            erasePreview(previewInfo);
        }
        normalCursor();
        if (previewInfo != null) {
            // Save undisplayed parms
            dataMgr.getParmOp().saveAllParameters(true, false);
        }
    }

    private boolean warnUser(String warning) {
        if (!PlatformUI.isWorkbenchRunning()) {
            return true;
        }
        // Warn the user given a warning set up by Preview
        // and ask whether or not to proceed.

        if ("TimeRange".equals(warning)) {
            boolean ask = Message
                    .inquireLastMessage(ShowTimeRangeWarningMsg.class)
                    .isEnabled();
            if (ask) {
                VizApp.runSync(new Runnable() {
                    @Override
                    public void run() {
                        // Simple warning keep as a blocking dialog.
                        TimeRangeWarningDialog prompt = new TimeRangeWarningDialog(
                                PlatformUI.getWorkbench()
                                        .getActiveWorkbenchWindow().getShell());
                        prompt.setBlockOnOpen(true);
                        returnCode = prompt.open();
                    }
                });
                return (returnCode == Dialog.OK);
            } else {
                return true;
            }
        } else if ("EmptyEditArea".equals(warning)) {
            boolean ask = Message
                    .inquireLastMessage(ShowEmptyEditAreaWarningMsg.class)
                    .isEnabled();
            if (ask) {
                VizApp.runSync(new Runnable() {
                    @Override
                    public void run() {
                        // Simple warning keep as a blocking dialog.
                        EmptyEditAreaWarningDialog prompt = new EmptyEditAreaWarningDialog(
                                PlatformUI.getWorkbench()
                                        .getActiveWorkbenchWindow().getShell());
                        prompt.setBlockOnOpen(true);
                        returnCode = prompt.open();
                    }
                });
                return (returnCode == Dialog.OK);
            } else {
                return true;
            }
        } else if ("EmptyTimeRange".equals(warning)) {
            String message = "Empty Time Range -- Please Select Time Range in Grid Manager.";
            processError("ExecuteOrClassError", message, null);
            return false;
        } else if ("SETimeSync".equals(warning)) {
            String message = "Time Range does not intersect Spatial Editor time. "
                    + "Please set Time so results will be visible. ";
            processError("ExecuteOrClassError", message, null);
            return false;
        } else {
            return true;
        }
    }
}
