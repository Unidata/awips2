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
package com.raytheon.viz.gfe;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.python.controller.PythonScriptController;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;

/**
 * Base controller for executing GFE python scripts (smart tools and procedures)
 * in a similar manner.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2008            njensen     Initial creation
 * Jan 08, 2013  1486      dgilling    Refactor based on PythonScriptController.
 * Jan 18, 2013            njensen     Added garbageCollect()
 * Oct 14, 2014  3676      njensen     Moved getNumpyResult(GridType) here and
 *                                      hardened it by separating jep.getValue()
 *                                      calls from python copying/casting to correct types
 * Feb 05, 2015  4089      njensen     Replaced previous hardening with ensureResultType()
 * Jul 07, 2015  14739     ryu         Added getVarDict().
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class BaseGfePyController extends PythonScriptController {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(BaseGfePyController.class);

    protected DataManager dataMgr;

    /**
     * Constructor
     * 
     * @param filePath
     *            path to the python interface
     * @param anIncludePath
     *            path of directories to include
     * @param classLoader
     *            Java classloader
     * @param dataManager
     *            current DataManager
     * @param aPythonClassName
     *            the class name for the python scripts
     * @throws JepException
     */
    protected BaseGfePyController(String filePath, String anIncludePath,
            ClassLoader classLoader, DataManager dataManager,
            String aPythonClassName) throws JepException {
        super(filePath, anIncludePath, classLoader, aPythonClassName);
        dataMgr = dataManager;
    }

    /**
     * Instantiates an instance of the class in the module
     * 
     * @param moduleName
     *            the name of the module to instantiate
     * @throws JepException
     */
    @Override
    public void instantiatePythonScript(String moduleName) throws JepException {
        Map<String, Object> instanceMap = getStarterMap(moduleName);
        instanceMap.put("dbss", dataMgr);
        execute("instantiate", INTERFACE, instanceMap);
    }

    /**
     * Gets the VariableList variable from the python module
     * 
     * @param moduleName
     *            the name of the module
     * @return the variable list, or null if there is not one
     * @throws JepException
     */
    protected Object getVariableList(String moduleName) throws JepException {
        HashMap<String, Object> args = new HashMap<String, Object>(1);
        args.put("name", moduleName);
        Object obj = execute("getVariableList", INTERFACE, args);
        return obj;
    }

    /**
     * Sets a module's varDict (variable list inputs), or sets the varDict to
     * None if there is not a variable list
     * 
     * @param varDict
     *            a string representation of a python dictionary
     * @throws JepException
     */
    public void setVarDict(String varDict) throws JepException {
        if (varDict == null) {
            jep.eval("varDict = None");
        } else {
            jep.eval("varDict = " + varDict);
        }
    }

    /**
     * Gets a module's varDict (variable list inputs)
     * 
     * @return   a string representation of a python dictionary
     */
    public String getVarDict() {
        String varDict = null;
        try {
            jep.eval("temp = str(varDict)");
            varDict = (String) jep.getValue("temp");
            jep.eval("temp = None");
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception while getting varDict", e);
        }
        return varDict;
    }

    /**
     * Processes a module's varDict (variable list inputs), or sets the varDict
     * to an empty dictionary if there is not a variable list
     * 
     * @param moduleName
     *            the name of the module to process the varDict for
     * @throws JepException
     */
    @SuppressWarnings("unchecked")
    public List<FieldDefinition> getVarDictWidgets(String moduleName)
            throws JepException {
        if (!isInstantiated(moduleName)) {
            instantiatePythonScript(moduleName);
        }
        List<FieldDefinition> fieldDefs = null;
        if (getVariableList(moduleName) != null) {
            jep.eval("widgetList = " + INTERFACE + ".getVariableListInputs('"
                    + moduleName + "')");
            Object processedVarDict = jep.getValue("widgetList");
            fieldDefs = (List<FieldDefinition>) processedVarDict;
        }

        return fieldDefs;
    }

    public String transformVarDict(Map<String, Object> map) {
        String varDict = null;
        try {
            jep.eval("import JUtil");
            jep.set("varDictMap", map);
            jep.eval("temp = JUtil.javaMapToPyDict(varDictMap)");
            varDict = (String) jep.getValue("temp");
            jep.eval("varDictMap = None");
            jep.eval("temp = None");
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception while transforming varDict", e);
        }
        return varDict;
    }

    /**
     * Runs the python garbage collector. This should be run at the end of a
     * procedure or tool in case the custom python used tk. If the python used
     * tk and it is not garbage collected, errors about invalid threads may
     * occur when the garbage collector runs in another python interpreter.
     */
    public void garbageCollect() {
        try {
            jep.eval("import gc");
            jep.eval("gcResult = gc.collect()");
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error garbage collecting GFE python interpreter", e);
        }
    }

    /**
     * Transforms the execution result of a python GFE script to a type expected
     * based on the GridType. Currently used by smart tools and VC modules.
     * 
     * @param type
     *            the type of data that is coming back
     * @return the result of the execution in Java format
     * @throws JepException
     */
    protected Object getNumpyResult(GridType type) throws JepException {
        Object result = null;
        boolean resultFound = (Boolean) jep.getValue(RESULT + " is not None");

        if (resultFound) {
            int xDim, yDim = 0;

            // this will safely alter the result dtypes in place if necessary
            ensureResultType(type);

            switch (type) {
            case SCALAR:
                // don't make python func calls within a jep.getValue() call
                float[] scalarData = (float[]) jep.getValue(RESULT);
                xDim = (Integer) jep.getValue(RESULT + ".shape[1]");
                yDim = (Integer) jep.getValue(RESULT + ".shape[0]");
                result = new Grid2DFloat(xDim, yDim, scalarData);
                break;
            case VECTOR:
                // don't make python func calls within a jep.getValue() call
                float[] mag = (float[]) jep.getValue(RESULT + "[0]");
                float[] dir = (float[]) jep.getValue(RESULT + "[1]");
                xDim = (Integer) jep.getValue(RESULT + "[0].shape[1]");
                yDim = (Integer) jep.getValue(RESULT + "[0].shape[0]");

                Grid2DFloat magGrid = new Grid2DFloat(xDim, yDim, mag);
                Grid2DFloat dirGrid = new Grid2DFloat(xDim, yDim, dir);
                result = new Grid2DFloat[] { magGrid, dirGrid };
                break;
            case WEATHER:
            case DISCRETE:
                // don't make python func calls within a jep.getValue() call
                byte[] bytes = (byte[]) jep.getValue(RESULT + "[0]");
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
     * Checks that a result numpy array is of the correct dtype for the
     * GridType, and if not, corrects the type to ensure it comes across to Java
     * safely.
     * 
     * If the correct type is found, nothing is done and therefore memory and
     * speed is saved over the alternative of always calling astype(dtype) or
     * ascontiguousarray(array, dtype), both of which will create a copy of the
     * array.
     * 
     * Note that if you attempt jep.getValue(array.astype(dtype)) or
     * jep.getValue(numpy.ascontiguousarray(array, dtype)) you can potentially
     * crash the JVM. jep.getValue(variable) should primarily retrieve variables
     * that are globally scoped in the python interpreter as opposed to created
     * on the fly.
     * 
     * @param type
     */
    protected void ensureResultType(GridType type) throws JepException {
        String safeType = null;
        switch (type) {
        case SCALAR:
        case VECTOR:
            safeType = "'float32'";
            break;
        case DISCRETE:
        case WEATHER:
            safeType = "'int8'";
            break;
        }
        String safetyCheck = RESULT + " = " + "NumpyJavaEnforcer.checkdTypes("
                + RESULT + ", " + safeType + ")";
        jep.eval("import NumpyJavaEnforcer");
        jep.eval(safetyCheck);
    }
}
