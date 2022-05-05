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
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.viz.gfe.BaseGfePyController;
import com.raytheon.viz.gfe.core.DataManager;

import jep.JepException;

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
 * Mar 12, 2015  #4246     randerso     Changes to support VCModules at base, site, and user levels
 *
 * </pre>
 *
 * @author dgilling
 * @version 1.0
 */

public class VCModuleController extends BaseGfePyController {

    private static final String CLASS_NAME = "VCParm";

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
     *            the data manager for this gfe instance
     * @throws JepException
     */
    protected VCModuleController(String aFilePath, String anIncludePath,
            ClassLoader aClassLoader, DataManager dataMgr) throws JepException {
        super(aFilePath, anIncludePath, aClassLoader, dataMgr, CLASS_NAME);

        String scriptPath = GfePyIncludeUtil.getVCModulesIncludePath(dataMgr
                .getSiteID());
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
        List<String> argNames = new ArrayList<>(getMethodArguments(
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
        if (!"calcGrid".equals(methodName)) {
            obj = jep.getValue(RESULT);
            jep.eval(RESULT + " = None");
        } else {
            obj = getNumpyResult(type);
            // getNumpyResult will set result to None to free up memory
        }

        return obj;
    }

    @Override
    public void instantiatePythonScript(String moduleName) throws JepException {
        Map<String, Object> instanceMap = getStarterMap(moduleName);
        execute("instantiate", INTERFACE, instanceMap);
    }
}
