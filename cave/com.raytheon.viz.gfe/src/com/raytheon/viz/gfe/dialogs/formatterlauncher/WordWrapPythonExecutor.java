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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.python.PythonScript;

/**
 * Executor service for running GFE's python-based word wrapping code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2015  #4959     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class WordWrapPythonExecutor {

    private final ExecutorService execService;

    private PythonScript python;

    public WordWrapPythonExecutor() {
        this.execService = Executors.newSingleThreadExecutor();
    }

    public synchronized String callWordWrapPython(
            final Map<String, Object> classArgs,
            final Map<String, Object> methodArgs) throws InterruptedException,
            ExecutionException {
        Callable<String> wordWrapTask = new Callable<String>() {

            @Override
            public String call() throws Exception {
                if (python == null) {
                    List<String> preEvals = Arrays.asList("import textwrap");
                    python = new PythonScript(
                            GfePyIncludeUtil.getCommonGfeIncludePath(),
                            getClass().getClassLoader(), preEvals);
                }

                python.instantiatePythonClass("_wrapper",
                        "textwrap.TextWrapper", classArgs);
                return (String) python.execute("fill", "_wrapper", methodArgs);
            }
        };

        Future<String> result = execService.submit(wordWrapTask);
        return result.get();
    }

    public void dispose() {
        /*
         * Have to create a Runnable to dispose the PythonScript instance to
         * avoid thread access issues.
         */
        Runnable disposeTask = new Runnable() {

            @Override
            public void run() {
                if (python != null) {
                    python.close();
                    python = null;
                }
            }
        };
        execService.submit(disposeTask);

        if (execService != null) {
            execService.shutdown();
        }
    }
}
