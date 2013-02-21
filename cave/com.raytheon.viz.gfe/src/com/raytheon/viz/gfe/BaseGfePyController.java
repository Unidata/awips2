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
     * Processes a module's varDict (variable list inputs), or sets the varDict
     * to an empty dictionary if there is not a variable list
     * 
     * @param moduleName
     *            the name of the module to process the varDict for
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
            jep.eval("gc.collect()");
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error garbage collecting GFE python interpreter", e);
        }
}
}
