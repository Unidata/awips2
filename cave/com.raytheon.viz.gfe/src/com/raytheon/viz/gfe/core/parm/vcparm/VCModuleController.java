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
package com.raytheon.viz.gfe.core.parm.vcparm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.viz.gfe.BaseGfePyController;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * A PythonScript subclass that replaces the Python/C++ interface that was
 * available to AWIPS1's <code>VCModule</code> class. Primarily used for
 * Python->Java data type conversions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            dgilling     Initial creation
 * Jan 08, 2013  1486      dgilling     Support changes to BaseGfePyController.
 * Oct 14, 2014  3676      njensen      Removed decodeGD(GridType) since it was
 *                                       a copy of getNumpyResult()
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class VCModuleController extends BaseGfePyController {

    private static final String CLASS_NAME = "VCParm";

    private List<String> tempGridNames;

    /**
     * Constructor.
     * 
     * @param aFilePath
     *            the path to the python script
     * @param anIncludePath
     *            the python include path, with multiple directories being
     *            separated by ":"
     * @param aClassLoader
     *            the Java classloader to use for importing Java classes inside
     *            python
     * @param dataMgr
     *            TODO
     * @throws JepException
     */
    protected VCModuleController(String aFilePath, String anIncludePath,
            ClassLoader aClassLoader, DataManager dataMgr) throws JepException {
        super(aFilePath, anIncludePath, aClassLoader, dataMgr, CLASS_NAME);
        this.tempGridNames = new ArrayList<String>();

        String scriptPath = GfePyIncludeUtil.getVCModulesIncludePath();
        jep.eval(INTERFACE + " = VCModuleInterface('" + scriptPath + "')");
    }

    /**
     * Retrieves a list of argument names for the specified Python method.
     * 
     * @param moduleName
     *            TODO
     * @param method
     *            The name of the method.
     * @return List of argument names for the specified Python method, except
     *         for "self".
     * @throws JepException
     */
    public List<String> getMethodArgs(String moduleName, String method)
            throws JepException {
        List<String> argNames = new ArrayList<String>(getMethodArguments(
                moduleName, method));
        argNames.remove("self");
        return argNames;
    }

    public Object executeMethod(String moduleName, String methodName,
            Map<String, Object> args, GridType type) throws JepException {
        if (!isInstantiated(moduleName)) {
            instantiatePythonScript(moduleName);
        }

        args.put(PyConstants.METHOD_NAME, methodName);
        args.put(PyConstants.MODULE_NAME, moduleName);
        args.put(PyConstants.CLASS_NAME, pythonClassName);

        internalExecute("runMethod", INTERFACE, args);
        Object obj = null;
        if (!methodName.equals("calcGrid")) {
            jep.eval(RESULT + " = JUtil.pyValToJavaObj(" + RESULT + ")");
            obj = jep.getValue(RESULT);
            jep.eval(RESULT + " = None");
        } else {
            obj = getNumpyResult(type);
            // getNumpyResult will set result to None to free up memory
        }

        return obj;
    }

    /**
     * Override of the <code>evaluateArgument</code> method which handles the
     * custom argument structures defined in <code>VCModule</code>. Important so
     * that we can convert the Java datatypes to Python native objects (and
     * numpy arrays for the grids).
     * 
     * @param argName
     *            The argument name.
     * @param argValue
     *            the value of the argument
     * 
     * @see com.raytheon.uf.common.python.PythonInterpreter#evaluateArgument(java
     *      .lang.String, java.lang.Object)
     */
    @Override
    protected void evaluateArgument(String argName, Object argValue)
            throws JepException {
        if (argValue instanceof IVcModuleArgument) {
            IVcModuleArgument castedArg = (IVcModuleArgument) argValue;
            Collection<String> tempGrids = castedArg.evaluateArgument(jep,
                    argName);
            tempGridNames.addAll(tempGrids);
        } else {
            super.evaluateArgument(argName, argValue);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.python.PythonScript#cleanupArgs(java.util.List)
     */
    @Override
    protected void cleanupArgs(List<String> args) throws JepException {
        super.cleanupArgs(args);
        for (String gridName : tempGridNames) {
            jep.eval("del " + gridName);
        }
        tempGridNames.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.BaseGfePyController#instantiatePythonTool(java.lang
     * .String)
     */
    @Override
    public void instantiatePythonScript(String moduleName) throws JepException {
        Map<String, Object> instanceMap = getStarterMap(moduleName);
        execute("instantiate", INTERFACE, instanceMap);
    }
}
