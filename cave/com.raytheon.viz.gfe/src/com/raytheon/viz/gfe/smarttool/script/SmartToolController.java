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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
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
            return getNumericResult(parmToEdit.getGridInfo().getGridType());
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
            if (parmName == null && parmTypeName == null) {
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
     * Transforms the execution result of a smart tool to a type that can be
     * handled by the GridCycler
     * 
     * @param type
     *            the type of data that is coming back from the smart tool
     * @return the result of the execution in Java format
     * @throws JepException
     */
    protected Object getNumericResult(GridType type) throws JepException {
        Object result = null;
        boolean resultFound = (Boolean) jep.getValue(RESULT + " is not None");

        if (resultFound) {
            int xDim, yDim = 0;
            switch (type) {
            case SCALAR:
                float[] scalarData = (float[]) jep.getValue(RESULT
                        + ".astype(numpy.float32)");
                xDim = (Integer) jep.getValue(RESULT + ".shape[1]");
                yDim = (Integer) jep.getValue(RESULT + ".shape[0]");
                result = new Grid2DFloat(xDim, yDim, scalarData);
                break;
            case VECTOR:
                float[] mag = (float[]) jep.getValue(RESULT
                        + "[0].astype(numpy.float32)");
                float[] dir = (float[]) jep.getValue(RESULT
                        + "[1].astype(numpy.float32)");
                xDim = (Integer) jep.getValue(RESULT + "[0].shape[1]");
                yDim = (Integer) jep.getValue(RESULT + "[0].shape[0]");

                Grid2DFloat magGrid = new Grid2DFloat(xDim, yDim, mag);
                Grid2DFloat dirGrid = new Grid2DFloat(xDim, yDim, dir);
                result = new Grid2DFloat[] { magGrid, dirGrid };
                break;
            case WEATHER:
            case DISCRETE:
                byte[] bytes = (byte[]) jep.getValue(RESULT
                        + "[0].astype(numpy.int8)");
                String[] keys = (String[]) jep.getValue(RESULT + "[1]");
                xDim = (Integer) jep.getValue(RESULT + "[0].shape[1]");
                yDim = (Integer) jep.getValue(RESULT + "[0].shape[0]");

                Grid2DByte grid = new Grid2DByte(xDim, yDim, bytes);
                List<String> keysList = new ArrayList<String>();
                Collections.addAll(keysList, keys);

                result = new Object[] { grid, keysList };
                break;
            }

            jep.eval(RESULT + " = None");
        }

        return result;
    }

    /**
     * Evaluates python method arguments for smart tools, transforming Java
     * objects into python objects where appropriate.
     */
    @Override
    protected void evaluateArgument(String argName, Object argValue)
            throws JepException {
        if (argValue instanceof IGridData) {
            if (argValue instanceof VectorGridData) {
                VectorGridData grid = (VectorGridData) argValue;
                Grid2DFloat mag = (grid.getVectorSlice()).getMagGrid();
                Grid2DFloat dir = (grid.getVectorSlice()).getDirGrid();
                String magName = argName + "Mag";
                String dirName = argName + "Dir";
                jep.setNumeric(magName, mag.getFloats(), mag.getXdim(),
                        mag.getYdim());
                jep.setNumeric(dirName, dir.getFloats(), dir.getXdim(),
                        dir.getYdim());
                jep.eval(argName + " = [" + magName + ", " + dirName + "]");
                jep.eval(magName + " = None");
                jep.eval(dirName + " = None");
            } else if (argValue instanceof ScalarGridData) {
                ScalarGridData grid = (ScalarGridData) argValue;
                Grid2DFloat f = (grid.getScalarSlice()).getScalarGrid();
                jep.setNumeric(argName, f.getFloats(), f.getXdim(), f.getYdim());
            } else if (argValue instanceof DiscreteGridData) {
                DiscreteGridData grid = (DiscreteGridData) argValue;
                jep.set("discreteGridData", grid);
                jep.eval("discreteGridNumpy = discreteGridData.__numpy__");
                jep.eval("discreteGridNumpy = discreteGridNumpy[0]");
                DiscreteKey[] keys = grid.getDiscreteSlice().getKey();
                ArrayList<String> stringKeys = new ArrayList<String>();
                for (DiscreteKey k : keys) {
                    stringKeys.add(k.toString());
                }
                String pythonList = PyUtil.listToList(stringKeys);
                jep.eval("discreteGridKeys = " + pythonList);
                jep.eval(argName + " = (discreteGridNumpy, discreteGridKeys)");
            } else if (argValue instanceof WeatherGridData) {
                WeatherGridData grid = (WeatherGridData) argValue;
                jep.set("weatherGridData", grid);
                jep.eval("weatherGridNumpy = weatherGridData.__numpy__");
                jep.eval("weatherGridNumpy = weatherGridNumpy[0]");
                WeatherKey[] keys = grid.getWeatherSlice().getKeys();
                ArrayList<String> stringKeys = new ArrayList<String>();
                for (WeatherKey k : keys) {
                    stringKeys.add(k.toString());
                }
                String pythonList = PyUtil.listToList(stringKeys);
                jep.eval("weatherGridKeys = " + pythonList);
                jep.eval(argName + " = (weatherGridNumpy, weatherGridKeys)");
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
