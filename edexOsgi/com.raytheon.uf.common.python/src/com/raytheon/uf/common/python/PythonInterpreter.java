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

import jep.Jep;
import jep.JepException;

/**
 * Interfaces to a native python interpreter with JEP.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 07, 2009            njensen     Initial creation
 * Sep 05, 2013  #2307     dgilling    Remove constructor without explicit
 *                                     ClassLoader.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class PythonInterpreter {

    private static final String CLEANUP = "def cleanup():\n"
            + "   g = globals()\n" + "   for i in g:\n"
            + "      if not i.startswith('__') and not i == 'cleanup':\n"
            + "         g[i] = None\n\n";

    protected Jep jep;

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
    public PythonInterpreter(String aFilePath, String anIncludePath,
            ClassLoader aClassLoader) throws JepException {
        jep = new Jep(false, anIncludePath, aClassLoader);
        initializeJep(aFilePath, null);
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
    public PythonInterpreter(String anIncludePath, ClassLoader aClassLoader)
            throws JepException {
        jep = new Jep(false, anIncludePath, aClassLoader);
        initializeJep(null, null);
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
    public PythonInterpreter(String aFilePath, String anIncludePath,
            ClassLoader aClassLoader, List<String> preEvals)
            throws JepException {
        jep = new Jep(false, anIncludePath, aClassLoader);
        initializeJep(aFilePath, preEvals);
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
    public PythonInterpreter(String anIncludePath, ClassLoader aClassLoader,
            List<String> preEvals) throws JepException {
        jep = new Jep(false, anIncludePath, aClassLoader);
        initializeJep(null, preEvals);
    }

    /**
     * Initializes the jep instance, enabling java imports and running the
     * script (if provided)
     * 
     * @param filePath
     *            the path to the python script or null if no script is to be
     *            run
     * 
     * @throws JepException
     * 
     */
    private void initializeJep(String filePath, List<String> preEvals)
            throws JepException {
        // this enables easy import of java classes in the python script
        jep.eval("import JavaImporter");

        if (preEvals != null) {
            for (String statement : preEvals) {
                jep.eval(statement);
            }
        }

        if (filePath != null) {
            jep.runScript(filePath);
        }
    }

    /**
     * Evaluates an argument in the python interpreter. Should be overridden by
     * subclasses where the python scripts wants python objects, not just
     * references to Java objects.
     * 
     * @param argName
     *            the name the argument will receive in the python interpreter
     * @param argValue
     *            the value of the argument
     * @throws JepException
     */
    protected void evaluateArgument(String argName, Object argValue)
            throws JepException {
        jep.set(argName, argValue);
    }

    /**
     * Disposes of the jep instance. Should be called whenever the system no
     * longer needs this PythonScript instance to free memory.
     */
    public void dispose() {
        cleanupGlobals();
        jep.close();
    }

    public void cleanupGlobals() {
        try {
            jep.eval(CLEANUP);
            jep.eval("cleanup()");
            jep.eval("cleanup = None");
            jep.eval("import gc");
            jep.eval("uncollected = gc.collect(2)");
            jep.eval("uncollected = None");
            jep.eval("gc = None");
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

}
