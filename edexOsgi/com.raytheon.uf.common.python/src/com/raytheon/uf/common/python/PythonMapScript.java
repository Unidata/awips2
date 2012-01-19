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
package com.raytheon.uf.common.python;

import java.util.List;
import java.util.Map;

import jep.JepException;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2008            wdougherty     Initial creation
 * </pre>
 * 
 * @author wdougherty
 * @version 1.0
 */
@Deprecated
public class PythonMapScript extends PythonScript {
    public PythonMapScript(String filePath, String anIncludePath,
            ClassLoader classLoader, List<String> preEvals) throws JepException {
        super(filePath, anIncludePath, classLoader, preEvals);
    }

    public PythonMapScript(String filePath, String anIncludePath,
            ClassLoader classLoader) throws JepException {
        super(filePath, anIncludePath, classLoader);
    }

    public PythonMapScript(String script, String includePath)
            throws JepException {
        super(script, includePath);
    }

    /**
     * Set Python variables in the current script from argmap. For each item in
     * the map, see if its value is a string beginning with "::". If it is not,
     * create a python variable in this test script whose name is the same as
     * the key, and assign it a value equal to the key's value.
     * 
     * @param argmap
     *            a map of argument names to values
     * @throws JepException
     *             if Jep chokes on the generated code.
     */
    public void setArgs(Map<String, Object> argmap) throws JepException {
        Object val;

        for (String arg : argmap.keySet()) {
            if (arg.startsWith("::")) {
                continue;
            }
            val = argmap.get(arg);
            evaluateArgument(arg, val);
        }
    }

    /**
     * Execute a method in a script. This is basically the same as the execute()
     * method of the base PythonScript class, except that it takes a Map of
     * argument names to values instead of twin arrays, and it has special
     * handling when the key is a String beginning with "::". The purpose of the
     * special handling is to allow complicated objects such as tuples and
     * arrays to be created in the script with eval() and then used as
     * parameters.
     * 
     * @param methodName
     *            The name of a method to execute.
     * @param instanceName
     *            The name of a class or module within which the method can be
     *            found.
     * @param argmap
     *            Maps argument names to argument values. Argument names
     *            beginning with "::" are interpreted as defining variables
     *            which have already been declared in the script.
     * @return The Object produced by executing the method, which may be null.
     * @throws JepException
     *             if Jep chokes on the scripted code.
     */
    public Object execute(String methodName, String instanceName,
            Map<String, Object> argmap) throws JepException {

        setArgs(argmap);

        StringBuilder sb = new StringBuilder();

        sb.append(PythonScript.RESULT);
        sb.append(" = ");
        if (instanceName != null) {
            sb.append(instanceName).append(".");
        }
        sb.append(methodName).append("(");
        // add method parameters
        String ref;
        String sep = "";
        for (String arg : argmap.keySet()) {
            if ("self".equals(arg)) {
                continue;
            }

            if (arg.startsWith("::")) {
                ref = (String) argmap.get(arg);
                arg = arg.substring(2);
            } else {
                ref = arg;
            }
            sb.append(sep);
            sb.append(arg).append("=").append(ref);

            sep = ",";
        }
        sb.append(")");
        // run the method
        jep.eval(sb.toString());
        // get the result
        return getExecutionResult();
    }

    /**
     * Interpret an arbitrary Python expression in the script. This is just a
     * wrapper around the eval() method of the Jep instance of PythonScript.
     * 
     * @param pythonExpr
     *            Python expression to interpret.
     * @return true if the expression can be interpreted, false otherwise
     *         (though usually a JepException is thrown).
     * @throws JepException
     *             if Jepp chokes on the expression.
     */
    public boolean eval(String pythonExpr) throws JepException {
        return jep.eval(pythonExpr);
    }
}
