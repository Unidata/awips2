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
package com.raytheon.uf.common.python.multiprocessing;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import jep.JepException;

import com.raytheon.uf.common.python.PythonInterpreter;

/**
 * A PythonScript with support for multiprocessing. NOTE: If you want to do
 * further processing in python in the original process, you should write that
 * code in your python and use PythonScript instead. If you want the results
 * from the separate process sent back to Java as soon as they're processed, use
 * this class.
 * 
 * This class executes a python method in a separate process with a few
 * assumptions. The separate method must have an argument "queue" that will be
 * the queue that result objects are put on. The result objects will be sent
 * back to the original process and transformed into Java objects for the
 * PyProcessListener.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2009            njensen     Initial creation
 * Mar 29, 2011 8774      rferrel     Clean up process when execute halted.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PythonProcess extends PythonInterpreter {

    // The current python the p.terminate() doesn't properly shutdown the p
    // thread; the os.kill does. In either case p.is_alive() is True.
    // Also the q.empty() and q.qsize() are not reliable sometimes get Empty
    // exception when q.qsize() > 0 therefore just catch the Empty exception and
    // ignore.
    private static final String WHILE_PROCESSING = "while p and p.is_alive():\n"
            + "   new_time = time.time()\n"
            + "   elapsed = new_time - default_time\n"
            + "   if elapsed > timeout or processState.isActive() == False:\n"
            + "      os.kill(p.pid, signal.SIGKILL)\n"
            + "      if processState.isActive():\n"
            + "         errorMessage.setMessage('Error: Retrieve timed out after %d seconds' % timeout)\n"
            + "      else:\n"
            + "         errorMessage.setMessage('Process Teminated')\n"
            + "      p = None\n"
            + "   if q.empty():\n"
            + "      time.sleep(0.1)\n"
            + "   else:\n"
            + "      try:\n"
            + "        while True:\n"
            + "           val = q.get_nowait()\n"
            + "           listener.objReceived(JUtil.pyValToJavaObj(val))\n"
            + "      except Empty: pass\n";

    private static final String PULL_FINISHED = "try:\n" + "  while True:\n"
            + "    val = q.get_nowait()\n"
            + "    listener.objReceived(JUtil.pyValToJavaObj(val))\n"
            + "except Empty: pass\n";

    final ProcessState processState = new ProcessState();

    final ErrorMessage errorMessage = new ErrorMessage();

    public PythonProcess(String filePath, String anIncludePath,
            ClassLoader classLoader) throws JepException {
        super(filePath, anIncludePath, classLoader);
        jep.eval("from multiprocessing import Queue, Process");
        jep.eval("from Queue import Empty");
        jep.eval("import time");
        jep.eval("import JUtil");
        jep.eval("import os, signal");
    }

    protected void internalExecute(String methodName, Map<String, Object> args,
            PyProcessListener listener, int timeout) throws JepException {
        StringBuffer sb = new StringBuffer();
        jep.eval("q = Queue()");
        jep.set("listener", listener);
        jep.set("processState", processState);
        jep.set("errorMessage", errorMessage);
        sb.append("p = Process(name = ");
        sb.append(methodName);
        sb.append(", target=");
        sb.append(methodName);
        sb.append(", kwargs={");
        sb.append("'queue': q,");
        if (args != null && args.size() > 0) {
            Set<String> keys = args.keySet();
            Iterator<String> itr = keys.iterator();
            while (itr.hasNext()) {
                String key = itr.next();
                if (!key.equals("self")) {
                    evaluateArgument(key, args.get(key));
                    sb.append("'");
                    sb.append(key);
                    sb.append("': ");
                    sb.append(key);
                    sb.append(",");
                }
            }
        }
        sb.append("})");
        jep.eval(sb.toString());
        jep.eval("p.daemon = True");
        // JavaImporter forwards sys.meta_path along into python. This creates
        // the issue that if python tries to import something that JavaImporter
        // will try to take over, then python tries to import it as Java and
        // it fails. Resetting sys.meta_path to empty to bypass this issue in
        // the new python process.
        jep.eval("sys.meta_path=[]");
        jep.eval("timeout = " + timeout);
        // adding a timer to the function to determine if it needs killed
        jep.eval("default_time = time.time()");
        jep.eval("p.start()");
        jep.eval(WHILE_PROCESSING);
        jep.eval(PULL_FINISHED);
        if (errorMessage.getMessage() != null) {
            if (errorMessage.getMessage().startsWith("Error")) {
                throw new JepException(errorMessage.getMessage());
            } else {
                System.out.println(errorMessage.getMessage());
            }
        }
    }

    /**
     * Executes the specified method name in a different process using the
     * multiprocessing module
     * 
     * @param methodName
     *            the name of the method to execute
     * @param args
     *            the args to the method
     * @param listener
     *            the Java object to receive the other process's results as they
     *            come in
     * @throws JepException
     */
    public void execute(String methodName, Map<String, Object> args,
            PyProcessListener listener, int timeout) throws JepException {
        internalExecute(methodName, args, listener, timeout);
    }

    /**
     * Informs python to terminate the execute thread.
     */
    public void killProcess() {
        processState.setActive(false);
    }

    @Override
    public void dispose() {
        killProcess();
        super.dispose();
    }

    /**
     * This class allows python to check and clean up when the execute thread is
     * halted.
     */
    private final class ProcessState {
        boolean active = true;

        /**
         * Flag to indicate if process should remain active. Used by the python
         * code.
         * 
         * @return active
         */
        @SuppressWarnings("unused")
        public boolean isActive() {
            return active;
        }

        public void setActive(boolean active) {
            this.active = active;
        }
    }

    private final class ErrorMessage {
        String message = null;

        public String getMessage() {
            return message;
        }

        public void setMessage(String message) {
            this.message = message;
        }
    }
}
