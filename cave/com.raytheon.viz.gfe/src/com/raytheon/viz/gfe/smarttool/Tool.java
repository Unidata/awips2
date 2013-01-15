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

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import jep.JepException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.MissingDataModeMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.query.QueryFactory;
import com.raytheon.viz.gfe.query.QueryScript;
import com.raytheon.viz.gfe.smarttool.SmartToolException.ErrorType;
import com.raytheon.viz.gfe.smarttool.script.SmartToolController;

/**
 * Ported from Tool.py
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2007            njensen     Initial creation
 * Jan 08, 2013  1486      dgilling    Support changes to BaseGfePyController.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class Tool {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Tool.class);

    private static final String CANCEL_MSG_START = "jep.JepException: <type 'exceptions.RuntimeError'>: Cancel: Cancel >>>";

    private final IParmManager parmMgr;

    private Parm inputParm;

    private final SmartToolController tool;

    private String toolName;

    private boolean saveMutableFlag;

    private boolean startedParmEdit;

    /**
     * This field used solely as a place to store the result of re-evaluating
     * the query-based edit area in execute()
     */
    private ReferenceData trueEditArea;

    /**
     * Constructor
     * 
     * @param aParmMgr
     *            the parm manager
     * @param aToolName
     *            the name of the tool
     * @param anInputParm
     *            the parm to edit
     * @param aTool
     *            the smart tool controller
     * @throws SmartToolException
     */
    public Tool(IParmManager aParmMgr, Parm anInputParm, String aToolName,
            SmartToolController aTool) throws SmartToolException {
        parmMgr = aParmMgr;
        inputParm = anInputParm;
        toolName = aToolName;
        tool = aTool;

        try {
            if (!tool.isInstantiated(toolName)) {
                tool.instantiatePythonScript(toolName);
            }
        } catch (JepException e) {
            throw new SmartToolException("Error instantiating python tool "
                    + toolName + ": " + e.getMessage());
        }
    }

    /**
     * Returns the objects that should be passed to the smart tool in python
     * 
     * @param args
     *            the names of the arguments
     * @param gridTimeRange
     *            the time range of the grid
     * @param toolTimeRange
     *            the time range selected in the grid manager
     * @param editArea
     *            the mask of the data
     * @param dataMode
     *            the missing data mode
     * @return
     * @throws SmartToolException
     */
    public Object[] getArgValues(List<String> args, TimeRange gridTimeRange,
            TimeRange toolTimeRange, ReferenceData editArea,
            MissingDataMode dataMode) throws SmartToolException {

        ArrayList<Object> argValueList = new ArrayList<Object>();
        // For each argument in args, append a value to the argValueList
        for (String arg : args) {
            Object result = null;
            try {
                if (arg.indexOf("_PickUpValue") >= 0) {
                    result = getParmAttr(arg, "pickUpValue");
                } else if (arg.indexOf("_DeltaValue") >= 0) {
                    result = getParmAttr(arg, "deltaValue");
                } else if (arg.indexOf("_FuzzValue") >= 0) {
                    result = getParmAttr(arg, "fuzzValue");
                } else if (arg.indexOf("_SmoothSize") >= 0) {
                    result = getParmAttr(arg, "smoothSize");
                } else if (arg.indexOf("WEname") >= 0) {
                    result = inputParm.expressionName();
                } else if (arg.indexOf("editArea") >= 0) {
                    result = editArea;
                } else if (arg.indexOf("_GridInfo") >= 0) {
                    result = getGridInfo(arg, gridTimeRange);
                } else if (arg.indexOf("_GridHistory") >= 0) {
                    result = getGridHistory(arg, gridTimeRange);
                } else if (arg.indexOf("_Grid") >= 0) {
                    result = getResult(arg, "TimeWtAverage", gridTimeRange,
                            dataMode);
                } else if (arg.indexOf("_MaxGrid") >= 0) {
                    result = getResult(arg, "Max", gridTimeRange, dataMode);
                } else if (arg.indexOf("_MinGrid") >= 0) {
                    result = getResult(arg, "Min", gridTimeRange, dataMode);
                } else if (arg.indexOf("_SumGrid") >= 0) {
                    result = getResult(arg, "Sum", gridTimeRange, dataMode);
                } else if (arg.indexOf("GridTimeRange") >= 0) {
                    result = gridTimeRange;
                } else if (arg.indexOf("ToolTimeRange") >= 0) {
                    result = toolTimeRange;
                } else if (arg.indexOf("varDict") >= 0) {
                    // skip this, it will be handled at execute
                    // result = self.__varDict;
                    result = null;
                } else if (arg.equals("self")) {
                    result = null;
                } else {
                    Parm parm = parmMgr.getParmInExpr(arg, true, inputParm);
                    if (parm == null) {
                        String msg = "Cannot Find Weather Element for " + arg;
                        throw new SmartToolException(msg, ErrorType.NO_DATA);
                    } else {
                        // numeric
                        result = getResult(arg, "TimeWtAverage", gridTimeRange,
                                dataMode);
                    }
                }
            } catch (SmartToolException e) {
                throw e;
            } catch (VizException e) {
                throw new SmartToolException(
                        "Error getting value for argument " + arg, e);
            }
            argValueList.add(result);
        }

        return argValueList.toArray(new Object[argValueList.size()]);
    }

    /**
     * Returns the attribute for a particular parm's name
     * 
     * @param arg
     *            the name of the parm
     * @param attrStr
     *            the attribute
     * @return
     * @throws SmartToolException
     */
    public Object getParmAttr(String arg, String attrStr)
            throws SmartToolException {
        String parmName = arg.split("_")[0];
        Parm parm = parmMgr.getParmInExpr(parmName, true, inputParm);
        Object attr = null;
        if (parm == null) {
            String msg = "Cannot Find Weather Element for " + arg;
            throw new SmartToolException(msg, ErrorType.NO_DATA);
        }
        ParmState state = parm.getParmState();
        String s = "" + attrStr.charAt(0);
        String capital = s.toUpperCase();
        attrStr = attrStr.replaceFirst(s, capital);
        Class<ParmState> c = ParmState.class;
        try {
            Method getter = c.getMethod("get" + attrStr);
            attr = getter.invoke(state, (Object[]) null);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting parm attribute", e);
        }

        return attr;
    }

    /**
     * Returns the grid data for the specified parameters
     * 
     * @param arg
     *            the name of the parm
     * @param mode
     *            the mode as specified by GridCycler.getCorrespondingResult()
     * @param gridTimeRange
     *            the time range of the grid
     * @param dataMode
     *            the missing data mode
     * @return
     * @throws SmartToolException
     * @throws GFEOperationFailedException
     */
    public IGridData getResult(String arg, String mode,
            TimeRange gridTimeRange, MissingDataMode dataMode)
            throws SmartToolException, GFEOperationFailedException {
        // Get Corresponding grids for the given argument and mode
        // The corresponding grid(s) will be of the following type:
        // If toolType is "point-based": Python tuple of tuple
        // If toolType is "numeric": Numeric Python array
        // Return a result and an error exception
        String parmName = arg.split("_")[0];
        Parm parm = parmMgr.getParmInExpr(parmName, true, inputParm);
        if (parm == null) {
            String msg = "Cannot Find Weather Element for " + arg;
            throw new SmartToolException(msg, ErrorType.NO_DATA);
        }
        IGridData[] resultA = null;
        IGridData result = null;
        resultA = GridCycler.getInstance().getCorrespondingResult(parm,
                gridTimeRange, mode, dataMode);
        if (resultA.length > 0) {
            result = resultA[0];
        }
        if (result == null) {
            if (dataMode == MissingDataMode.SKIP
                    || dataMode == MissingDataMode.CREATE) {
                String msg = "Skipped grid " + arg;
                throw new SmartToolException(msg, ErrorType.SKIPPED_GRID);
            } else {
                String msg = "No corresponding grids for " + arg;
                throw new SmartToolException(msg, ErrorType.NO_DATA);
            }
        }
        return result;
    }

    /**
     * Returns the grid history corresponding to the parm name and time range
     * 
     * @param arg
     *            the name of the parm
     * @param gridTimeRange
     *            the time range of the grid
     * @return
     * @throws SmartToolException
     */
    public Object getGridHistory(String arg, TimeRange gridTimeRange)
            throws SmartToolException {
        String parmName = arg.split("_")[0];
        Parm parm = parmMgr.getParmInExpr(parmName, true, inputParm);
        if (parm == null) {
            String msg = "Cannot Find Weather Element for " + arg;
            throw new SmartToolException(msg, ErrorType.NO_DATA);
        }
        IGridData[] grids = parm.getGridInventory(gridTimeRange);
        ArrayList<Object> historyList = new ArrayList<Object>();
        for (IGridData grid : grids) {
            ArrayList<GridDataHistory> gridHistList = new ArrayList<GridDataHistory>();
            for (GridDataHistory gdh : grid.getHistory()) {
                // GridDataHistory gridHistory = new GridDataHistory(gdh
                // .getOrigin(), gdh.getOriginParm(), gdh
                // .getOriginTimeRange(), gdh.getTimeModified(), gdh
                // .getWhoModified(), gdh.getUpdateTime(), gdh
                // .getPublishTime(), gdh.getLastSentTime());
                GridDataHistory gridHistory = gdh;
                gridHistList.add(gridHistory);
            }
            historyList.add(gridHistList);
        }

        // if (historyList.size() == 1) {
        // return historyList.get(0);
        // }
        return historyList;
    }

    /**
     * Returns the grid info corresponding to the parm name and time range
     * 
     * @param arg
     *            the name of the parm
     * @param gridTimeRange
     *            the time range of the grid
     * @return
     * @throws SmartToolException
     */
    public Object getGridInfo(String arg, TimeRange gridTimeRange)
            throws SmartToolException {
        String parmName = arg.split("_")[0];
        Parm parm = parmMgr.getParmInExpr(parmName, true, inputParm);
        if (parm == null) {
            String msg = "Cannot Find Weather Element for " + arg;
            throw new SmartToolException(msg, ErrorType.NO_DATA);
        }
        IGridData[] grids = parm.getGridInventory(gridTimeRange);
        if (grids.length == 0) {
            return null;
        }
        List<GridParmInfo> infoList = new ArrayList<GridParmInfo>();
        for (IGridData grid : grids) {
            GridParmInfo gridInfo = new GridParmInfo(grid.getParm()
                    .getGridInfo());
            infoList.add(gridInfo);
        }
        if (infoList.size() == 1) {
            return infoList.get(0);
        }
        return infoList;
    }

    /**
     * Executes a smart tool
     * 
     * @param toolName
     *            the name of the tool
     * @param inputParm
     *            the parm to edit
     * @param editArea
     *            the mask of the data to edit
     * @param timeRange
     *            the time range to execute over
     * @param missingDataMode
     * @throws SmartToolException
     */
    public void execute(String toolName, Parm inputParm,
            final ReferenceData editArea, TimeRange timeRange, String varDict,
            MissingDataMode missingDataMode, IProgressMonitor monitor)
            throws SmartToolException {
        MissingDataMode dataMode;
        if (missingDataMode == null) {
            dataMode = Message.inquireLastMessage(MissingDataModeMsg.class)
                    .getMode();
        } else {
            dataMode = missingDataMode;
        }

        if (inputParm == null) {
            inputParm = parmMgr.getParmInExpr("variableElement", true);
            if (inputParm == null) {
                String message = "Smart Tool " + toolName
                        + ": No Weather Element to Edit";
                throw new SmartToolException(message);
            }
        }
        this.inputParm = inputParm;

        String weToEdit = null;
        try {
            weToEdit = tool.getWeatherElementEdited(toolName);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error determining weather element to edit", e);
        }
        Parm parmToEdit = null;
        if (weToEdit != null && !weToEdit.equals("None")) {
            parmToEdit = this.inputParm;
        }

        startedParmEdit = false;

        // Determine the pre, execute, and post methods to call
        // If present, instantiate Tool class

        // Check the tool modes to make sure they make sense

        // Get the gridInventory for the timeRange
        IGridData[] grids = this.inputParm.getGridInventory(timeRange);
        if (grids.length == 0) {
            String message = "Smart Tool " + toolName
                    + ": No Grids To Edit for " + inputParm.expressionName();
            throw new SmartToolException(message);
            // AFPS.UserAlertMsg_send_mh(self.__msgHand, message, "A", "GFE")
        }

        // Make sure parm is mutable
        if (parmToEdit != null) {
            saveMutableFlag = this.inputParm.isMutable();
            this.inputParm.setMutable(true);
        }

        // Clear missing grids
        GridCycler.getInstance().clearMissingData();
        boolean saveParams = false;

        try {
            tool.setVarDict(varDict);
            // # PreProcess Tool
            handlePreAndPostProcess("preProcessTool", null, timeRange,
                    editArea, dataMode);
            statusHandler.handle(Priority.DEBUG, "Running smartTool: "
                    + toolName);

            // Iterate over time range
            // Process each grid in the time range.
            int numberOfGrids = grids.length;
            for (int i = 0; i < numberOfGrids; i++) {
                if (monitor.isCanceled()) {
                    return;
                }

                IGridData grid = grids[i];
                int index = i;
                // # Show progress on a grid basis for numeric and parm-based
                // if toolType == "numeric" or toolType == "parm-based":
                // percent = (index+1)/numberOfGrids * 100.0
                // AFPS.ProgressBarMsg_send_mh(self.__msgHand, "SmartTool",
                // percent)
                if (!grid.isOkToEdit() && parmToEdit != null) {
                    String message = "Smart Tool " + toolName
                            + ": Encountered locked grid. ";
                    message += "Grid skipped.";
                    throw new SmartToolException(message, ErrorType.LOCKED_GRID);
                }

                final Date timeInfluence;
                Date seTime = DataManager.getCurrentInstance()
                        .getSpatialDisplayManager().getSpatialEditorTime();
                if (grids.length == 1 && grid.getGridTime().contains(seTime)) {
                    timeInfluence = seTime;
                } else {
                    timeInfluence = grid.getGridTime().getStart();
                }
                TimeRange gridTimeRange = grid.getGridTime();
                boolean first = false;
                // boolean last = false;
                if (index == 0) {
                    first = true;
                }
                // if (index == numberOfGrids - 1) {
                // last = true;
                // }

                // Re-evaluate edit area if a query
                if (editArea.isQuery()) {
                    // have to use runSync here to force the QueryScript to
                    // execute on the proper thread for JEP
                    if (Display.getDefault().isDisposed() == false) {
                        VizApp.runSync(new Runnable() {
                            @Override
                            public void run() {
                                QueryScript script;
                                try {
                                    script = QueryFactory
                                            .getCachedScript(DataManager
                                                    .getCurrentInstance());
                                    HashMap<String, Object> argMap = new HashMap<String, Object>();
                                    argMap.put("expression",
                                            editArea.getQuery());
                                    argMap.put("timeInfluence", timeInfluence);
                                    Tool.this.trueEditArea = (ReferenceData) script
                                            .execute("evaluate", argMap);
                                } catch (JepException e) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            "Error re-evaluating edit area "
                                                    + editArea.getId()
                                                            .getName() + ": "
                                                    + e.getLocalizedMessage(),
                                            e);
                                }
                            }
                        });
                    }
                } else {
                    trueEditArea = editArea;
                }

                try {
                    handlePreAndPostProcess("preProcessGrid", gridTimeRange,
                            timeRange, trueEditArea, dataMode);

                    if (monitor.isCanceled()) {
                        return;
                    }

                    numeric(parmToEdit, first, trueEditArea, gridTimeRange,
                            timeRange, timeInfluence, dataMode);

                    if (monitor.isCanceled()) {
                        return;
                    }

                    handlePreAndPostProcess("postProcessGrid", gridTimeRange,
                            timeRange, trueEditArea, dataMode);
                } catch (SmartToolException e) {
                    if (e.getErrorType() == ErrorType.SKIPPED_GRID) {
                        String pname = "None";
                        if (parmToEdit != null) {
                            pname = parmToEdit.expressionName();
                        }
                        String msg = "Skipped: " + pname + ":"
                                + gridTimeRange.toString();
                        GridCycler.getInstance().addMissingData(msg);
                        continue;
                    } else {
                        throw e;
                    }
                }
            } // end of grids for loop

            // # PostProcess Tool
            handlePreAndPostProcess("postProcessTool", null, timeRange,
                    trueEditArea, dataMode);
            saveParams = true;
        } catch (SmartToolException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (JepException e) {
            if (!e.getMessage().startsWith(CANCEL_MSG_START)) {
                statusHandler.handle(Priority.PROBLEM, "Error executing "
                        + toolName + ": " + e.getLocalizedMessage(), e);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error executing "
                    + toolName + ": " + e.getLocalizedMessage(), e);
        } finally {
            cleanUp(parmToEdit, saveParams, toolName, dataMode);
        }
    }

    /**
     * Executes the numeric smart tool
     * 
     * @param parmToEdit
     *            the parm to edit
     * @param first
     *            if this is the first grid
     * @param editArea
     *            the mask of the data
     * @param gridTimeRange
     *            the time of the grid
     * @param gridTimeRange
     *            the time range selected in the grid manager
     * @param timeInfluence
     *            the time influence
     * @param dataMode
     *            the missing data mode
     * @throws SmartToolException
     * @throws JepException
     * @throws GFEOperationFailedException
     */
    private void numeric(Parm parmToEdit, boolean first,
            ReferenceData editArea, TimeRange gridTimeRange,
            TimeRange toolTimeRange, Date timeInfluence,
            MissingDataMode dataMode) throws SmartToolException, JepException,
            GFEOperationFailedException {
        // Process a tool whose arguments are Numeric arrays

        if (parmToEdit != null) {
            if (first) {
                try {
                    parmToEdit.startParmEdit(new Date[] { timeInfluence });
                } catch (GFEOperationFailedException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Error during start parm edit for "
                                            + toolName
                                            + " - already running."
                                            + "  Please wait for the operation to complete and try again.",
                                    e);
                    return;
                }
                startedParmEdit = true;
            } else {
                try {
                    parmToEdit.extendParmEdit(new Date[] { timeInfluence });
                } catch (GFEOperationFailedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error during extend parm edit", e);
                    return;
                }
            }
        }
        List<String> executeArgs = tool.getMethodArguments(toolName, "execute");
        Object gridResult = null;
        Object[] argValues = getArgValues(executeArgs, gridTimeRange,
                toolTimeRange, editArea, dataMode);
        HashMap<String, Object> argMap = new HashMap<String, Object>();
        for (int i = 0; i < executeArgs.size(); i++) {
            argMap.put(executeArgs.get(i), argValues[i]);
        }

        gridResult = tool.executeTool(parmToEdit, toolName, argMap);

        // Update the parm grid with the result
        if (gridResult != null) {
            GridCycler.getInstance().storeNumericGrid(parmToEdit,
                    gridTimeRange, editArea, gridResult);
        }
    }

    /**
     * Cleans up a smart tool execution or failure and displays any missing data
     * message
     * 
     * @param parmToEdit
     *            the parm to edit
     * @param save
     *            if the parm should be saved
     * @param toolname
     *            the name of the tool
     * @param dataMode
     *            the missing data mode
     */
    private void cleanUp(Parm parmToEdit, boolean save, String toolname,
            MissingDataMode dataMode) {
        if (parmToEdit != null) {
            inputParm.setMutable(saveMutableFlag);
            // If modified parm is not loaded, save it automatically
            if (save) {
                boolean inputParmInDisplayedParms = false;
                Parm[] parms = parmMgr.getDisplayedParms();
                for (Parm p : parms) {
                    if (p.equals(inputParm)) {
                        inputParmInDisplayedParms = true;
                        break;
                    }
                }
                if (!inputParmInDisplayedParms) {
                    parmMgr.saveParm(inputParm);
                }
            }
            if (startedParmEdit) {
                parmToEdit.endParmEdit();
            }
        }
        parmMgr.deleteTemporaryParms();

        // Report Skipped or Created Grids
        String msg = "Tool: " + toolname + " -- ";
        List<String> missingData = GridCycler.getInstance().getMissingData();
        if (missingData.size() > 0) {
            if (dataMode == MissingDataMode.SKIP) {
                msg += "Grids Skipped due to Missing Data: \n";
            } else if (dataMode == MissingDataMode.CREATE) {
                msg += "Grids Created or Skipped due to Missing Data: \n";
            }
            for (String s : missingData) {
                msg += " " + s + "\n";
            }

            statusHandler.handle(Priority.PROBLEM, msg);
            missingData.clear();
        }
    }

    private void handlePreAndPostProcess(String methodName,
            TimeRange gridTimeRange, TimeRange toolTimeRange,
            ReferenceData editArea, MissingDataMode dataMode)
            throws SmartToolException, JepException {
        if (tool.hasMethod(toolName, methodName)) {
            List<String> prePostToolArgs = tool.getMethodArguments(toolName,
                    methodName);
            Object[] prePostToolObjs = getArgValues(prePostToolArgs,
                    gridTimeRange, toolTimeRange, editArea, dataMode);
            HashMap<String, Object> prePostToolMap = new HashMap<String, Object>();
            for (int i = 0; i < prePostToolArgs.size(); i++) {
                prePostToolMap.put(prePostToolArgs.get(i), prePostToolObjs[i]);
            }
            tool.runToolMethod(toolName, methodName, prePostToolMap);
        }
    }
}
