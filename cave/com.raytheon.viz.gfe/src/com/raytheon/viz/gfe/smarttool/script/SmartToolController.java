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
package com.raytheon.viz.gfe.smarttool.script;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jep.JepException;
import jep.NDArray;

import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.BaseGfePyController;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.griddata.VectorGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * Controller for getting the information on smart tools and running them. All
 * smart tools execute through this interface.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2008            njensen     Initial creation
 * Oct 29, 2013  2476      njensen     Renamed numeric methods to numpy
 * 10/31/2013    2508      randerso    Change to use DiscreteGridSlice.getKeys()
 * Oct 14, 2014  3676      njensen     Promoted getNumpyResult() to parent class
 * Apr 23, 2015  4259      njensen     Updated for new JEP API
 * 
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolController extends BaseGfePyController {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SmartToolController.class);

    private LocalizationFile smartToolDir;

    private LocalizationFile utilitiesDir;

    /**
     * Constructor
     * 
     * @param filePath
     *            path to the SmartToolInterface.py
     * @param anIncludePath
     *            path of directories to include
     * @param classLoader
     *            Java classloader
     * @param dataManager
     *            current DataManager
     * @throws JepException
     */
    public SmartToolController(String filePath, String anIncludePath,
            ClassLoader classLoader, DataManager dataManager)
            throws JepException {
        super(filePath, anIncludePath, classLoader, dataManager, "Tool");

        LocalizationContext baseCtx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE);

        smartToolDir = GfePyIncludeUtil.getSmartToolsLF(baseCtx);
        smartToolDir.addFileUpdatedObserver(this);

        utilitiesDir = GfePyIncludeUtil.getUtilitiesLF(baseCtx);
        utilitiesDir.addFileUpdatedObserver(this);

        String scriptPath = GfePyIncludeUtil.getSmartToolsIncludePath();
        jep.eval(INTERFACE + " = SmartToolInterface('" + scriptPath + "')");

        List<String> errors = getStartupErrors();
        if (errors.size() > 0) {
            StringBuffer sb = new StringBuffer();
            sb.append("Error importing the following smart tools:\n");
            for (String s : errors) {
                sb.append(s);
                sb.append("\n");
            }
            statusHandler.handle(Priority.PROBLEM, sb.toString());
        }
        jep.eval("import TimeRange, numpy");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.python.PythonInterpreter#dispose()
     */
    @Override
    public void dispose() {
        smartToolDir.removeFileUpdatedObserver(this);
        utilitiesDir.removeFileUpdatedObserver(this);
        super.dispose();
    }

    /**
     * Runs the specified method on a smart tool
     * 
     * @param toolName
     *            the name of the tool
     * @param methodName
     *            the name of the method to run
     * @param args
     *            the arguments to the method
     * @throws JepException
     */
    public void runToolMethod(String toolName, String methodName,
            Map<String, Object> args) throws JepException {
        args.put(PyConstants.METHOD_NAME, methodName);
        args.put(PyConstants.MODULE_NAME, toolName);
        args.put(PyConstants.CLASS_NAME, pythonClassName);

        internalExecute("runTool", INTERFACE, args);
    }

    /**
     * Runs the smart tool's execute method
     * 
     * @param parmToEdit
     *            the parm that's being edited
     * @param toolName
     *            the name of the tool being executed
     * @param args
     *            the arguments to the tool
     * @return the result of the tool's execution
     * @throws JepException
     */
    public Object executeTool(Parm parmToEdit, String toolName,
            Map<String, Object> args) throws JepException {

        runToolMethod(toolName, "execute", args);
        if (parmToEdit == null) {
            return null;
        } else {
            return getNumpyResult(parmToEdit.getGridInfo().getGridType());
        }
    }

    /**
     * Lists all the tools that apply to the specified parm
     * 
     * @param parm
     *            the parm to find applicable tools for
     * @return the names of tools that apply to the parm
     */
    @SuppressWarnings(value = "unchecked")
    public String[] listTools(Parm parm) {
        String parmName = null;
        String parmTypeName = null;
        if (parm != null) {
            parmName = parm.getParmID().getParmName();
            parmTypeName = parm.getGridInfo().getGridType().name();
        }
        Set<String> set = null;
        String[] tools = new String[0];
        try {
            if ((parmName == null) && (parmTypeName == null)) {
                set = (Set<String>) execute("getScripts", INTERFACE, null);
            } else {
                HashMap<String, Object> argMap = new HashMap<String, Object>(2);
                if (parmName != null) {
                    argMap.put("weatherElement", parmName);
                }
                if (parmTypeName != null) {
                    argMap.put("gridType", parmTypeName);
                }
                set = (Set<String>) execute("getScripts", INTERFACE, argMap);
            }
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error determining list of tools", e);
        }
        if (set != null) {
            tools = set.toArray(new String[set.size()]);
            Arrays.sort(tools);
        }
        return tools;
    }

    /**
     * Gets the WeatherElementEdited variable from the smart tool
     * 
     * @param toolName
     *            the name of the tool
     * @return the WeatherElementEdited
     * @throws JepException
     */
    public String getWeatherElementEdited(String toolName) throws JepException {
        HashMap<String, Object> args = new HashMap<String, Object>(1);
        args.put("name", toolName);
        return (String) execute("getWeatherElementEdited", INTERFACE, args);
    }

    /**
     * Evaluates python method arguments for smart tools, transforming Java
     * objects into python objects where appropriate.
     */
    @SuppressWarnings("unchecked")
    @Override
    protected void evaluateArgument(String argName, Object argValue)
            throws JepException {
        if (argValue instanceof IGridData) {
            IGridData grid = (IGridData) argValue;
            if (grid instanceof VectorGridData) {
                NDArray<float[]>[] data = (NDArray<float[]>[]) grid
                        .getGridSlice().getNDArray();
                String magName = argName + "Mag";
                String dirName = argName + "Dir";
                jep.set(magName, data[0]);
                jep.set(dirName, data[1]);
                jep.eval(argName + " = [" + magName + ", " + dirName + "]");
                jep.eval(magName + " = None");
                jep.eval(dirName + " = None");
            } else if (grid instanceof ScalarGridData) {
                jep.set(argName, grid.getGridSlice().getNDArray());
            } else if (grid instanceof DiscreteGridData) {
                jep.set("discreteGridData", grid.getGridSlice().getNDArray());
                DiscreteKey[] keys = ((DiscreteGridData) grid)
                        .getDiscreteSlice().getKeys();
                List<String> stringKeys = new ArrayList<String>();
                for (DiscreteKey k : keys) {
                    stringKeys.add(k.toString());
                }
                String pythonList = PyUtil.listToList(stringKeys);
                jep.eval("discreteGridKeys = " + pythonList);
                jep.eval(argName + " = (discreteGridData, discreteGridKeys)");
            } else if (grid instanceof WeatherGridData) {
                jep.set("weatherGridData", grid.getGridSlice().getNDArray());
                WeatherKey[] keys = ((WeatherGridData) grid).getWeatherSlice()
                        .getKeys();
                ArrayList<String> stringKeys = new ArrayList<String>();
                for (WeatherKey k : keys) {
                    stringKeys.add(k.toString());
                }
                String pythonList = PyUtil.listToList(stringKeys);
                jep.eval("weatherGridKeys = " + pythonList);
                jep.eval(argName + " = (weatherGridData, weatherGridKeys)");
            }
        } else if (argValue instanceof WxValue) {
            if (argValue instanceof VectorWxValue) {
                VectorWxValue vec = (VectorWxValue) argValue;
                Float mag = vec.getMag();
                Float dir = vec.getDir();
                String magName = argName + "Mag";
                String dirName = argName + "Dir";
                jep.set(magName, mag);
                jep.set(dirName, dir);
                jep.eval(argName + " = [" + magName + ", " + dirName + "]");
                jep.eval(magName + " = None");
                jep.eval(dirName + " = None");
            } else if (argValue instanceof ScalarWxValue) {
                Float val = ((ScalarWxValue) argValue).getValue();
                jep.set(argName, val);
            } else if (argValue instanceof DiscreteWxValue) {
                String key = ((DiscreteWxValue) argValue).getDiscreteKey()
                        .toString();
                jep.set(argName, key);
            } else if (argValue instanceof WeatherWxValue) {
                String key = ((WeatherWxValue) argValue).getWeatherKey()
                        .toString();
                jep.set(argName, key);
            }
        } else if (argValue instanceof TimeRange) {
            jep.set("timeRangeTemp", argValue);
            jep.eval(argName + " = TimeRange.TimeRange(timeRangeTemp)");
        } else if (argName.startsWith("varDict")) {
            jep.eval(argName + " = varDict");
        } else {
            super.evaluateArgument(argName, argValue);
        }
    }
}
