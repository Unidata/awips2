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
package com.raytheon.viz.aviation.climatedata;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringJoiner;
import java.util.UUID;
import java.util.stream.Collectors;

import com.raytheon.viz.aviation.monitor.AvnPyUtil;

import jep.Jep;
import jep.JepConfig;
import jep.JepException;
import jep.NamingConventionClassEnquirer;
import jep.SharedInterpreter;

/**
 * Executes AvnFPS climate python code in a Jep SharedInterpreter.
 *
 *
 * Implementation notes:
 *
 * This really is just a thin wrapper around the Jep SharedInterpreter class
 * that sets up the JepConfig and sys.path before executing the script.
 *
 * This is the only instance in all of CAVE code where a SharedInterpreter is
 * used instead of a SubInterpreter. This is why this class is not part of the
 * PythonInterpreter / PythonScript hierarchy.
 *
 * All SharedInterpreter instances have some amount of shared internal state
 * between them, notably sys.path. For more information see javadoc for
 * {@link jep.SharedInterpreter} and {@link jep.SubInterpreter}.
 *
 *
 * Rationale:
 *
 * The reason we are using SharedInterpreters in this case is that AvnFPS
 * climate code makes heavy use of the PyTables package, which has frequently
 * had problems when used with SubInterpreters--including possibly corrupted
 * internal state of C extension modules, that was not resolved by adding
 * various modules in PyTables to the Jep shared modules list.
 *
 * The use of SharedInterpreters to make PyTables work correctly is considered
 * to be a stopgap solution until when/if AvnFPS climate code can be rewritten
 * to remove all use of PyTables (or if some other better solution is
 * discovered).
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2019  7878       tgurney     Initial creation
 * Jul 13, 2023 2035920    tgurney     Rewrite to use SharedInterpreter
 * Aug 02, 2023 2035998    dgilling    Add evaluateArgument back to this class.
 *
 * </pre>
 *
 * @author tgurney
 */

public class ClimatePythonScript implements AutoCloseable {

    private Jep jep;

    private static boolean jepConfiged = false;

    /**
     * Create a new Python interpreter instance with AvnFPS locations on its
     * import path. Then executes the provided scriptFile immediately
     */
    public ClimatePythonScript(File scriptFile) {
        synchronized (ClimatePythonScript.class) {
            /*
             * SharedInterpreter can have a JepConfig set on it only once
             * globally
             */
            if (!jepConfiged) {
                JepConfig jepConfig = new JepConfig();
                jepConfig.setClassLoader(getClass().getClassLoader());
                jepConfig.setClassEnquirer(new NamingConventionClassEnquirer());
                jepConfig.redirectStdout(System.out);
                jepConfig.redirectStdErr(System.err);
                SharedInterpreter.setConfig(jepConfig);
                jepConfiged = true;
            }
        }
        jep = new SharedInterpreter();
        /*
         * sys.path is shared across all SharedInterpreter instances so only add
         * each given path to sys.path if it's not already in there.
         */
        String[] includePaths = { scriptFile.getParentFile().getPath(),
                AvnPyUtil.getLoggingHandlerDir(), AvnPyUtil.getPointDataDir(),
                AvnPyUtil.getCommonPythonDir() };
        StringBuilder sb = new StringBuilder();
        sb.append("import sys\n");
        for (String s : includePaths) {
            sb.append("if '" + s + "' not in sys.path:\n");
            sb.append("    sys.path.append('" + s + "')\n");
        }
        jep.exec(sb.toString());
        jep.runScript(scriptFile.getAbsolutePath());
    }

    /** Execute a method in the Python script */
    public Object execute(String methodName, Map<String, Object> args)
            throws JepException {
        Collection<String> jepArgNames = new ArrayList<>();
        StringJoiner execString = new StringJoiner(", ", methodName + "(", ")");
        for (Entry<String, Object> arg : args.entrySet()) {
            String jepArg = evaluateArgument(arg.getKey(), arg.getValue());
            execString.add(arg.getKey() + "=" + jepArg);
            jepArgNames.add(jepArg);
        }
        Object retVal = jep.getValue(execString.toString());
        cleanupArgs(jepArgNames);
        return retVal;
    }

    @Override
    public void close() throws Exception {
        jep.close();
    }

    /**
     * This function will take the input parameters to a function to be executed
     * by python and converts any Java List instances to python list instances.
     * This is necessary because we execute certain python scripts that attempt
     * to pickle its input for later usage (e.g.,
     * ClimateDataUpdate.stnPickle()).
     *
     * @param argName
     * @param argValue
     * @return
     * @throws JepException
     */
    private String evaluateArgument(String argName, Object argValue)
            throws JepException {
        String jepArgName = argName
                + UUID.randomUUID().toString().replace('-', '_');

        if (argValue instanceof List) {
            List<?> list = (List<?>) argValue;
            String prefix = jepArgName + " = ['";
            String suffix = "']";
            String cmd = list.stream().map(Object::toString)
                    .collect(Collectors.joining("', '", prefix, suffix));
            jep.exec(cmd);
        } else {
            jep.set(jepArgName, argValue);
        }

        return jepArgName;
    }

    private void cleanupArgs(Collection<String> args) throws JepException {
        if ((args != null) && (!args.isEmpty())) {
            for (String argName : args) {
                if (!"self".equals(argName)) {
                    jep.exec("del " + argName);
                }
            }
        }
    }
}
