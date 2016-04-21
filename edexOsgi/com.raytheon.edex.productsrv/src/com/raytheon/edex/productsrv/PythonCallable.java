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
package com.raytheon.edex.productsrv;

import java.util.concurrent.Callable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jep.JepException;

import com.raytheon.uf.common.python.PythonEval;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2011            ekladstrup     Initial creation
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class PythonCallable implements Callable<Object> {

    private static ThreadLocalPython threadPython = new ThreadLocalPython();

    private String script = null;

    public PythonCallable(String script) {
        this.script = script;
    }

    @Override
    public Object call() throws Exception {
        // long threadID = Thread.currentThread().getId();
        // System.err.println("ThreadID " + threadID);

        // get jep instance for current running thread
        PythonEval py = null;
        try {
            py = threadPython.get();
            // System.err.println("PythonEval " + py.hashCode());
        } catch (JepRuntimeException e) {
            // if py is null ( should be ) remove value so next try will re-init
            if (py == null) {
                threadPython.remove();
            }
            // if py is non null but we would want to re-init we must dispose
            // the jep instance

            // unwrap JepException and return
            JepException realException = e.unwrapJepException();
            return realException;
        }

        // Execute python script
        try {
            String wrapped = wrapper(this.script);
            py.eval(wrapped);
            py.eval("result = executeScript()");
            py.eval("executeScript = None");
            Object result = py.getValue("result");
            py.eval("result = None");
            return result;
        } catch (JepException e) {
            return e;
        }
    }

    private static String wrapper(String script) {
        String wrapper = "def executeScript():\n    "
                + script.replaceAll("\n", "\n    ") + "\n";

        if (wrapper.indexOf("'''") > -1) {
            Pattern pattern = Pattern.compile("'''.*'''", Pattern.DOTALL);
            Matcher matcher = pattern.matcher(wrapper);
            if (matcher.find()) {
                wrapper = wrapper.substring(0, wrapper.indexOf("'''"))
                        + matcher.group().replaceAll("\n    ", "\n")
                        + wrapper.substring(wrapper.indexOf("'''",
                                wrapper.indexOf("'''") + 3) + 3);
            }
        }
        return wrapper;
    }

}
