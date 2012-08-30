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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;

import jep.JepException;

/**
 * Generic interface for executing Python scripts natively from Java. All
 * interfacing with Python should go through this class. If custom behavior is
 * needed, the recommended course of action is to extend this class.
 * 
 * Due to JNI, the thread that creates an instance of PythonScript must be the
 * same thread for any actions on that script.
 * 
 * dispose() should be called when the script is no longer needed to help ensure
 * memory is cleaned up.
 * 
 * The profile option uses Python's built-in cProfiler and if enabled, will run
 * whatever method is sent to internalExecute() through cProfiler, dumping the
 * results to a file in the /tmp/ directory with the name of the method being
 * executed. This should never be enabled in a deployed/production system and is
 * only used for improving the performance of code.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2008             njensen     Initial creation
 * Mar 21, 2008            njensen     Major refactor
 * June 9, 2008            njensen     Refactor
 * Sep 18, 2009   2899     njensen     Added cProfile support
 * Dec 7, 2009    3310     njensen     Separated some functionality up to PythonInterpreter
 * Jun 26, 2012   #776     dgilling    Fix leaking of global names.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PythonScript extends PythonInterpreter {

    protected static final String RESULT = "__result";

    protected boolean profile = false;

    /**
     * Constructor
     * 
     * @param aFilePath
     *            the path to the python script
     * @param anIncludePath
     *            the python include path, with multiple directories being
     *            separated by :
     * @throws JepException
     */
    public PythonScript(String aFilePath, String anIncludePath)
            throws JepException {
        super(aFilePath, anIncludePath);
    }

    /**
     * Constructor
     * 
     * @param aFilePath
     *            the path to the python script
     * @param anIncludePath
     *            the python include path, with multiple directories being
     *            separated by :
     * @param aClassLoader
     *            the java classloader to use for importing java classes inside
     *            python
     * @throws JepException
     */
    public PythonScript(String aFilePath, String anIncludePath,
            ClassLoader aClassLoader) throws JepException {
        super(aFilePath, anIncludePath, aClassLoader);
    }

    /**
     * Constructor
     * 
     * @param aFilePath
     *            the path to the python script
     * @param anIncludePath
     *            the python include path, with multiple directories being
     *            separated by :
     * @param aClassLoader
     *            the java classloader to use for importing java classes inside
     *            python
     * @param profile
     *            or not to run the python profiler. This should only ever be
     *            true for development purposes, not production code.
     * @throws JepException
     */
    public PythonScript(String aFilePath, String anIncludePath,
            ClassLoader aClassLoader, boolean profile) throws JepException {
        this(aFilePath, anIncludePath, aClassLoader);
        this.profile = profile;
    }

    /**
     * Constructor
     * 
     * @param anIncludePath
     *            the python include path, with multiple directories being
     *            separated by :
     * @param aClassLoader
     *            the java classloader to use for importing java classes inside
     *            python
     * @throws JepException
     */
    public PythonScript(String anIncludePath, ClassLoader aClassLoader)
            throws JepException {
        super(anIncludePath, aClassLoader);
    }

    /**
     * Constructor
     * 
     * @param aFilePath
     *            the path to the python script
     * @param anIncludePath
     *            the python include path, with multiple directories being
     *            separated by :
     * @param aClassLoader
     *            the java classloader to use for importing java classes inside
     *            python
     * @param preEvals
     *            String statements to be run by the python interpreter before
     *            the file at aFilePath. This is generally used to create global
     *            vars in the python interpreter.
     * @throws JepException
     */
    public PythonScript(String aFilePath, String anIncludePath,
            ClassLoader aClassLoader, List<String> preEvals)
            throws JepException {
        super(aFilePath, anIncludePath, aClassLoader, preEvals);
    }

    /**
     * Constructor
     * 
     * @param anIncludePath
     *            the python include path, with multiple directories being
     *            separated by :
     * @param aClassLoader
     *            the java classloader to use for importing java classes inside
     *            python
     * @param preEvals
     *            String statements to be run by the python interpreter
     *            immediately. This is generally used to create global vars in
     *            the python interpreter.
     * @throws JepException
     */
    public PythonScript(String anIncludePath, ClassLoader aClassLoader,
            List<String> preEvals) throws JepException {
        super(anIncludePath, aClassLoader, preEvals);
    }

    /**
     * Executes the method of the python script on the instance (if not null).
     * Supports keyword and default arguments in python. This should be called
     * by an implementation of execute().
     * 
     * @param methodName
     *            the name of the method to execute
     * @param instanceName
     *            the name of the instance the method is on
     * @param args
     *            the map of argument names to values to pass the method
     * @throws JepException
     */
    protected void internalExecute(String methodName, String instanceName,
            Map<String, Object> args) throws JepException {
        StringBuffer sb = new StringBuffer();
        if (profile) {
            sb.append("cProfile.run('");
        }
        sb.append(RESULT);
        sb.append(" = ");
        if (instanceName != null) {
            sb.append(instanceName);
            sb.append(".");
        }
        sb.append(methodName);
        sb.append("(");
        List<String> uniqueNames = new ArrayList<String>();
        if (args != null && args.size() > 0) {
            Set<Entry<String, Object>> entries = args.entrySet();
            Iterator<Entry<String, Object>> itr = entries.iterator();
            while (itr.hasNext()) {
                Entry<String, Object> entry = itr.next();
                String key = entry.getKey();
                if (!key.equals("self")) {
                    Object value = entry.getValue();
                    String uniqueKey = key;
                    uniqueKey += UUID.randomUUID().toString().replace('-', '_');
                    uniqueNames.add(uniqueKey);
                    evaluateArgument(uniqueKey, value);
                    sb.append(key);
                    sb.append("=");
                    sb.append(uniqueKey);
                    if (itr.hasNext()) {
                        sb.append(", ");
                    }
                }
            }
        }
        sb.append(")");
        if (profile) {
            sb.append("', '/tmp/");
            sb.append(methodName);
            sb.append("')");
            jep.eval("import cProfile");
        }
        jep.eval(sb.toString());
        cleanupArgs(uniqueNames);
    }

    protected void cleanupArgs(List<String> args) throws JepException {
        if (args != null && !args.isEmpty()) {
            for (String key : args) {
                if (!key.equals("self")) {
                    jep.eval("del " + key);
                }
            }
        }
    }

    /**
     * Executes the script's specified method. Subclasses should override this
     * for custom execution and return behavior or for calling instance methods.
     * 
     * @param methodName
     *            the name of the method to execute
     * @param args
     *            the map of argument names to values to pass to the python
     *            method
     * @return the result of the method
     * @throws JepException
     */
    public Object execute(String methodName, Map<String, Object> args)
            throws JepException {
        return execute(methodName, null, args);
    }

    /**
     * Executes the script's specified method. Subclasses should override this
     * for custom execution and return behavior or for calling instance methods.
     * 
     * @param methodName
     *            the name of the method to execute
     * @param instanceName
     *            the name of the instance on which to execute the method, or
     *            null if no instance
     * @param args
     *            the map of argument names to values to pass to the python
     *            method
     * @return the result of the method
     * @throws JepException
     */
    public Object execute(String methodName, String instanceName,
            Map<String, Object> args) throws JepException {
        internalExecute(methodName, instanceName, args);
        Object obj = getExecutionResult();
        jep.eval(RESULT + " = None");
        return obj;
    }

    /**
     * Convenience method for getting the result after execute has been called.
     * Subclasses should override this if they wish to manipulate the result
     * before it's returned.
     * 
     * @return
     * @throws JepException
     */
    protected Object getExecutionResult() throws JepException {
        return jep.getValue(RESULT);
    }

    /**
     * Gets the arguments names of a specific method.
     * 
     * @param methodName
     *            the name of the method to retrieve arguments for
     * @param instanceName
     *            the name of the class instance, or null if it's not an
     *            instance method
     * @return the names of the arguments
     * @throws JepException
     */
    public String[] getArgumentNames(String methodName, String instanceName)
            throws JepException {
        String instStart = "";
        if (instanceName != null) {
            instStart = instanceName + ".";
        }
        int argcount = (Integer) jep.getValue(instStart + methodName
                + ".func_code.co_argcount");
        String args = (String) jep.getValue(instStart + methodName
                + ".func_code.co_varnames");
        String[] split = args.split(",");
        String[] arguments = new String[argcount];
        for (int i = 0; i < argcount; i++) {
            arguments[i] = split[i].replaceAll("[(')]", "").trim();
        }
        return arguments;
    }

    /**
     * Instantiates a python class with the given instance name
     * 
     * @param instanceName
     *            the name to assign the instance
     * @param className
     *            the name of the python class
     * @param initArgs
     *            the arguments to the python class's constructor __init__
     * @throws JepException
     */
    public void instantiatePythonClass(String instanceName, String className,
            Map<String, Object> initArgs) throws JepException {
        StringBuffer sb = new StringBuffer();
        sb.append(instanceName);
        sb.append(" = ");
        sb.append(className);
        sb.append("(");
        if (initArgs != null) {
            Set<String> keySet = initArgs.keySet();
            Iterator<String> itr = keySet.iterator();
            while (itr.hasNext()) {
                String key = itr.next();
                jep.set(key, initArgs.get(key));
                sb.append(key);
                sb.append("=");
                sb.append(key);
                if (itr.hasNext()) {
                    sb.append(", ");
                }
            }
        }
        sb.append(")");
        jep.eval(sb.toString());
    }

}
