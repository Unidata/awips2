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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;
import jep.NDArray;

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
 * Apr 23, 2015  4259      njensen     Updated for new JEP API
 * Jul 17, 2015  4575      njensen     Changed varDict from String to Map
 * Sep 16, 2015  4871      randerso    Return modified varDict from Tool/Procedure
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
    public void setVarDict(Map<String, Object> varDict) throws JepException {
        if (varDict == null) {
            jep.eval("varDict = None");
        } else {
            jep.set("varDict", varDict);
            jep.eval("import JUtil");
            jep.eval("varDict = JUtil.javaObjToPyVal(varDict)");
        }
    }

    /**
     * Gets a module's varDict (variable list inputs)
     * 
     * @return the varDict
     * @throws JepException
     */
    public Map<String, Object> getVarDict() throws JepException {
        @SuppressWarnings("unchecked")
        Map<String, Object> javaDict = (Map<String, Object>) jep
                .getValue("varDict");
        return javaDict;
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
    @SuppressWarnings("unchecked")
    protected Object getNumpyResult(GridType type) throws JepException {
        Object result = null;
        boolean resultFound = (Boolean) jep.getValue(RESULT + " is not None");

        if (resultFound) {
            // this will safely alter the result dtypes in place if necessary
            ensureResultType(type);

            /*
             * FIXME We reverse the x and y dimensions because that's what AWIPS
             * 1 did and that makes the pre-existing python code compatible.
             * Java ordering is x,y while python is ordering is y,x. It's
             * confusing and questionable at best so someday someone should
             * correct all that. Good luck.
             */
            switch (type) {
            case SCALAR:
                // don't make python func calls within a jep.getValue() call
                NDArray<float[]> arr = (NDArray<float[]>) jep.getValue(RESULT);
                result = new Grid2DFloat(arr.getDimensions()[1],
                        arr.getDimensions()[0], arr.getData());
                break;
            case VECTOR:
                // don't make python func calls within a jep.getValue() call
                NDArray<float[]> mag = (NDArray<float[]>) jep.getValue(RESULT
                        + "[0]");
                NDArray<float[]> dir = (NDArray<float[]>) jep.getValue(RESULT
                        + "[1]");
                Grid2DFloat magGrid = new Grid2DFloat(mag.getDimensions()[1],
                        mag.getDimensions()[0], mag.getData());
                Grid2DFloat dirGrid = new Grid2DFloat(dir.getDimensions()[1],
                        dir.getDimensions()[0], dir.getData());
                result = new Grid2DFloat[] { magGrid, dirGrid };
                break;
            case WEATHER:
            case DISCRETE:
                // don't make python func calls within a jep.getValue() call
                NDArray<byte[]> bytes = (NDArray<byte[]>) jep.getValue(RESULT
                        + "[0]");
                List<String> keysList = (List<String>) jep.getValue(RESULT
                        + "[1]");
                Grid2DByte grid = new Grid2DByte(bytes.getDimensions()[1],
                        bytes.getDimensions()[0], bytes.getData());

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
