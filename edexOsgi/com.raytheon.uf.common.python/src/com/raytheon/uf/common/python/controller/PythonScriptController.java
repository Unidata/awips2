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
package com.raytheon.uf.common.python.controller;

import java.io.File;
import java.util.Collections;
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
import com.raytheon.uf.common.python.PythonErrorExtractor;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Base controller for executing python scripts in a similar manner. Sub-classes
 * should base their implementation on this class and for the python code
 * develop an extension based on
 * /build.edex/esb/data/utility/common_static/base/python/MasterInterface.py.
 * <p>
 * Script instances are "cached" within the interpreter, so this class
 * implements the ILocalizationFileObserver interface so the cached instances
 * can be properly updated as necessary.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 12, 2012            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public abstract class PythonScriptController extends PythonScript implements
        ILocalizationFileObserver {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PythonScriptController.class);

    protected static final String INTERFACE = "interface";

    protected final String pythonClassName;

    protected Set<String> pendingRemoves = new HashSet<String>();

    protected Set<String> pendingAdds = new HashSet<String>();

    protected Set<String> pendingReloads = new HashSet<String>();

    /**
     * Constructor
     * 
     * @param filePath
     *            path to the python interface, which should be based on
     *            MasterInterface.py
     * @param anIncludePath
     *            path of directories to include
     * @param classLoader
     *            Java classloader
     * @param aPythonClassName
     *            the class name for the python scripts
     * @throws JepException
     *             If a python Error is thrown during instantiation of the
     *             underlying python script.
     */
    protected PythonScriptController(String filePath, String anIncludePath,
            ClassLoader classLoader, String aPythonClassName)
            throws JepException {
        super(filePath, anIncludePath, classLoader);
        this.pythonClassName = aPythonClassName;
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
        Map<String, Object> args = new HashMap<String, Object>();
        args.put(PyConstants.MODULE_NAME, moduleName);
        args.put(PyConstants.CLASS_NAME, pythonClassName);
        return args;
    }

    /**
     * Gets formatted list of errors detailing errors trying to import python
     * scripts based on initial scan.
     * 
     * @return a list of error messages
     * @throws JepException
     *             If an Error is thrown during python execution.
     */
    @SuppressWarnings("unchecked")
    protected List<String> getStartupErrors() throws JepException {
        return (List<String>) execute("getStartupErrors", INTERFACE, null);
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
     *             If an Error is thrown during python execution.
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
     *             If an Error is thrown executing the module's constructor.
     */
    public void instantiatePythonScript(String moduleName) throws JepException {
        Map<String, Object> instanceMap = getStarterMap(moduleName);
        execute("instantiate", INTERFACE, instanceMap);
    }

    /**
     * Determines whether or not a specified module has been instantiated.
     * 
     * @param moduleName
     *            The name of the module to check.
     * @return If the module has been instantiated or not.
     * @throws JepException
     *             If an Error is thrown during python execution.
     */
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
     *             If an Error is thrown during python execution.
     */
    @SuppressWarnings(value = "unchecked")
    public List<String> getMethodArguments(String moduleName, String methodName)
            throws JepException {
        Map<String, Object> instanceMap = getStarterMap(moduleName);
        instanceMap.put(PyConstants.METHOD_NAME, methodName);
        List<String> list = (List<String>) execute("getMethodArgNames",
                INTERFACE, instanceMap);
        return Collections.unmodifiableList(list);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationFileObserver#fileUpdated
     * (com.raytheon.uf.common.localization.FileUpdatedMessage)
     */
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

    /**
     * Reload an updated module in the interpreter's "cache".
     * 
     * @param name
     *            Module to be reloaded.
     * @throws JepException
     *             If an Error is thrown during python execution.
     */
    protected void reloadModule(String name) throws JepException {
        boolean wasInstantiated = isInstantiated(name);

        HashMap<String, Object> argMap = new HashMap<String, Object>(1);
        argMap.put(PyConstants.MODULE_NAME, name);
        execute("reloadModule", INTERFACE, argMap);

        // it was already initialized, need to get a new instance in the
        // interpreter now that the module was reloaded
        // if it wasn't already initialized, it's either a utility or will be
        // initialized when it's first used
        if (wasInstantiated) {
            instantiatePythonScript(name);
        }
    }

    /**
     * Add a module to the interpreter's "cache".
     * 
     * @param name
     *            Module to be added.
     * @throws JepException
     *             If an Error is thrown during python execution.
     */
    protected void removeModule(String name) throws JepException {
        HashMap<String, Object> argMap = new HashMap<String, Object>(1);
        argMap.put(PyConstants.MODULE_NAME, name);
        execute("removeModule", INTERFACE, argMap);
    }

    /**
     * Remove a module from the interpreter's "cache".
     * 
     * @param name
     *            Module to be removed.
     * @throws JepException
     *             If an Error is thrown during python execution.
     */
    protected void addModule(String name) throws JepException {
        HashMap<String, Object> argMap = new HashMap<String, Object>(1);
        argMap.put(PyConstants.MODULE_NAME, name);
        execute("addModule", INTERFACE, argMap);
    }
}
