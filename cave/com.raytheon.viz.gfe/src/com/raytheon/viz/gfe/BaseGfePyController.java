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

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jep.JepException;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.uf.common.python.PythonScript;
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
 * Jan 18, 2013           njensen     Added garbageCollect()
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class BaseGfePyController extends PythonScript implements
        ILocalizationFileObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(BaseGfePyController.class);

    protected static final String INTERFACE = "interface";

    protected DataManager dataMgr;

    protected final String pythonClassName;

    protected Set<String> pendingRemoves = new HashSet<String>();

    protected Set<String> pendingAdds = new HashSet<String>();

    protected Set<String> pendingReloads = new HashSet<String>();

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
        super(filePath, anIncludePath, classLoader);
        dataMgr = dataManager;
        pythonClassName = aPythonClassName;
    }

    /**
     * Convenience method for getting an argument map with moduleNames and
     * classNames
     * 
     * @param moduleName
     *            the name of the module
     * @return an argument map
     */
    protected Map<String, Object> getStarterMap(String moduleName) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put(PyConstants.MODULE_NAME, moduleName);
        args.put(PyConstants.CLASS_NAME, pythonClassName);
        return args;
    }

    /**
     * Checks if a module has the specified method
     * 
     * @param moduleName
     *            the name of the module to check
     * @param methodName
     *            the method to look up
     * @return if the method exists
     * @throws JepException
     */
    public boolean hasMethod(String moduleName, String methodName)
            throws JepException {
        Map<String, Object> args = getStarterMap(moduleName);
        args.put(PyConstants.METHOD_NAME, methodName);
        return (Boolean) execute("hasMethod", INTERFACE, args);
    }

    /**
     * Instantiates an instance of the class in the module
     * 
     * @param moduleName
     *            the name of the module to instantiate
     * @throws JepException
     */
    public void instantiatePythonTool(String moduleName) throws JepException {
        Map<String, Object> instanceMap = getStarterMap(moduleName);
        instanceMap.put("dbss", dataMgr);
        execute("instantiate", INTERFACE, instanceMap);
    }

    public boolean isInstantiated(String moduleName) throws JepException {
        HashMap<String, Object> argMap = new HashMap<String, Object>(1);
        argMap.put(PyConstants.MODULE_NAME, moduleName);
        return (Boolean) execute("isInstantiated", INTERFACE, argMap);
    }

    /**
     * Returns the names of the specified method's arguments
     * 
     * @param moduleName
     *            the name of the module
     * @param methodName
     *            the name of the method
     * @return the method arguments from the python
     * @throws JepException
     */
    @SuppressWarnings(value = "unchecked")
    public String[] getMethodArguments(String moduleName, String methodName)
            throws JepException {
        Map<String, Object> instanceMap = getStarterMap(moduleName);
        instanceMap.put(PyConstants.METHOD_NAME, methodName);
        List<String> list = (List<String>) execute("getMethodArgNames",
                INTERFACE, instanceMap);
        return list.toArray(new String[list.size()]);
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
            instantiatePythonTool(moduleName);
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
     * Gets formatted errors of failures to import smart tools from the initial
     * scan
     * 
     * @return a list of error messages
     * @throws JepException
     */
    @SuppressWarnings("unchecked")
    protected List<String> getStartupErrors() throws JepException {
        return (List<String>) execute("getStartupErrors", INTERFACE, null);
    }

    protected void reloadModule(String name) throws JepException {
        HashMap<String, Object> argMap = new HashMap<String, Object>(1);
        argMap.put(PyConstants.MODULE_NAME, name);
        execute("reloadModule", INTERFACE, argMap);

        // it was already initialized, need to get a new instance in the
        // interpreter now that the module was reloaded
        // if it wasn't already initialized, it's either a utility or will be
        // initialized when it's first used
        if (this.isInstantiated(name)) {
            this.instantiatePythonTool(name);
        }
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        File file = new File(message.getFileName());
        String name = file.getName().replaceAll("\\.py.*", "");
        FileChangeType changeType = message.getChangeType();
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile lf = pm.getLocalizationFile(message.getContext(),
                message.getFileName());

        if (message.getChangeType() == FileChangeType.ADDED) {
            if (lf != null) {
                lf.getFile();
            }
            pendingAdds.add(name);
        } else if (changeType == FileChangeType.DELETED) {
            if (lf != null) {
                File toDelete = lf.getFile();
                toDelete.delete();
            }
            pendingRemoves.add(name);
        } else if (changeType == FileChangeType.UPDATED) {
            if (lf != null) {
                lf.getFile();
            }
            pendingReloads.add(name);
        }
    }

    /**
     * Updates the modules in the interpreter that have been scheduled to be
     * updated. This must be called from the correct thread to work.
     */
    public void processFileUpdates() {
        for (String toolName : pendingRemoves) {
            try {
                removeModule(toolName);
            } catch (JepException e) {
                statusHandler.handle(Priority.PROBLEM, "Error removing module "
                        + toolName, e);
            }
        }
        pendingRemoves.clear();

        for (String toolName : pendingAdds) {
            try {
                addModule(toolName);
            } catch (JepException e) {
                String pythonErrMsg = PythonErrorExtractor.getPythonError(e,
                        toolName);
                if (pythonErrMsg == null) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error adding module " + toolName, e);
                } else {
                    statusHandler.error(pythonErrMsg, e);
                }
            }
        }
        pendingAdds.clear();

        for (String toolName : pendingReloads) {
            try {
                reloadModule(toolName);
            } catch (JepException e) {
                String pythonErrMsg = PythonErrorExtractor.getPythonError(e,
                        toolName);
                if (pythonErrMsg == null) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error reloading module " + toolName + "\n", e);
                } else {
                    statusHandler.error(pythonErrMsg, e);
                }
            }
        }
        pendingReloads.clear();
    }

    protected void removeModule(String name) throws JepException {
        HashMap<String, Object> argMap = new HashMap<String, Object>(1);
        argMap.put(PyConstants.MODULE_NAME, name);
        execute("removeModule", INTERFACE, argMap);
    }

    protected void addModule(String name) throws JepException {
        HashMap<String, Object> argMap = new HashMap<String, Object>(1);
        argMap.put(PyConstants.MODULE_NAME, name);
        execute("addModule", INTERFACE, argMap);
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
