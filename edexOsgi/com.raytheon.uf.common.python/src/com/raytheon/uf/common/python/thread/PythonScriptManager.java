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
package com.raytheon.uf.common.python.thread;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;

import jep.JepException;

import com.raytheon.uf.common.python.PythonScript;

/**
 * Manages Jep scripts in a limited number of threads. Input to the scripts is
 * through invariant ScriptRequest objects in a concurrent queue. Output from
 * the scripts is through a concurrent Map based on the id from the
 * ScriptRequest.
 * <p>
 * Why? Currently there are problems when a python script that includes numpy is
 * disposed of in edex such as leaked memory and errors reloading the library
 * again.
 * <p>
 * Instead of using this class it is recommended that developers instead use
 * {@link PythonJobCoordinator#newInstance(AbstractPythonScriptFactory<S>)}
 * instead to provide multi-threaded Python execution to their code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 05, 2013  #2307     dgilling     Use better PythonScript constructor,
 *                                      mark as Deprecated.
 * 
 * </pre>
 */
@Deprecated
public class PythonScriptManager {

    private static final Object NullScriptResult = new Object();

    private LinkedBlockingQueue<ScriptRequest> inQueue = new LinkedBlockingQueue<ScriptRequest>();

    private ConcurrentHashMap<Integer, Object> outputMap = new ConcurrentHashMap<Integer, Object>();

    /**
     * Script execution request to script execution threads. An invariant class.
     * All attributes are final and public.
     */
    public static class ScriptRequest {
        /**
         * Initialization Constructor.
         * 
         * @param id
         * @param method
         * @param args
         */
        public ScriptRequest(Integer id, String method, Map<String, Object> args) {
            this.id = id;
            this.method = method;
            this.args = args;
        }

        /**
         * Initialization constructor that defaults requestId to the current
         * thread's hashCode.
         * 
         * @param method
         * @param args
         */
        public ScriptRequest(String method, Map<String, Object> args) {
            this.id = Thread.currentThread().hashCode();
            this.method = method;
            this.args = args;
        }

        public final Integer id;

        public final String method;

        public final Map<String, Object> args;
    }

    /**
     * Creates a python script manager for the specified script with the given
     * include path. Thread count determines the number of listener threads.
     * 
     * @param scriptPath
     * @param pythonIncludePath
     * @param threadCount
     * @throws JepException
     */
    public PythonScriptManager(final String scriptPath,
            final String pythonIncludePath, final int threadCount)
            throws JepException {
        ExecutorService executor = Executors.newFixedThreadPool(threadCount);

        executor.execute(new Runnable() {
            @Override
            public void run() {
                PythonScript script;
                try {
                    script = new PythonScript(scriptPath, pythonIncludePath,
                            this.getClass().getClassLoader());
                } catch (JepException e1) {
                    throw new RuntimeException("Failed to initialize script: "
                            + scriptPath);
                }

                while (true) {
                    Integer requestId = null;
                    try {
                        ScriptRequest request = inQueue.take();
                        if (request != null) {
                            requestId = request.id;
                            Object obj = script.execute(request.method,
                                    request.args);
                            if (obj == null) {
                                obj = NullScriptResult;
                            }
                            outputMap.put(requestId, obj);
                        }
                    } catch (JepException e) {
                        outputMap.put(requestId, e);
                    } catch (InterruptedException e) {
                        outputMap.put(requestId, e);
                    }
                }
            }
        });
    }

    /**
     * Blocking request for a script execution result. If an exception is thrown
     * during script processing, then it will be thrown here.
     * 
     * @param request
     *            script execution request.
     * @return script processing output
     * @throws Exception
     * @throws JepException
     */
    public Object callScript(ScriptRequest request) throws Exception,
            JepException {
        return this.callScript(request, 500);
    }

    /**
     * Blocking request for a script execution result. If an exception is thrown
     * during script processing, then it will be thrown here.
     * 
     * @param request
     *            script execution request.
     * @param milliseconds
     *            sleep time between queries for script output.
     * @return script processing output
     * @throws Exception
     * @throws JepException
     */
    public Object callScript(ScriptRequest request, long milliseconds)
            throws Exception, JepException {
        inQueue.offer(request);
        Object result = null;
        while (!outputMap.containsKey(request.id)) {
            try {
                Thread.sleep(milliseconds);
            } catch (InterruptedException e) {
                throw e;
            }
        }
        result = outputMap.remove(request.id);
        if (result instanceof JepException) {
            throw (JepException) result;
        } else if (result instanceof Exception) {
            throw (Exception) result;
        } else if (result == NullScriptResult) {
            result = null;
        }
        return result;
    }
}
