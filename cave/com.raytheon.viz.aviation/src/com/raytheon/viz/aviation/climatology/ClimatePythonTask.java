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
package com.raytheon.viz.aviation.climatology;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.aviation.climatedata.ClimatePythonScript;
import com.raytheon.viz.aviation.monitor.AvnPyUtil;

import jep.JepConfig;
import jep.JepException;
import jep.NamingConventionClassEnquirer;

/**
 * Runs a single climate Python method in a new thread
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------------
 * Jul 30, 2019  7878     tgurney   Initial creation
 * Dec 02, 2019  7986     randerso  Remove redundant setting of shared modules
 * Jan  8, 2020  7878     tgurney   Raise log level of Python exceptions from
 *                                  warn to error
 *
 * </pre>
 *
 * @author tgurney
 */

public class ClimatePythonTask {

    /** Allows client code to receive messages from the running task */
    public interface ClimatePythonListener {
        /**
         * Called from Python code. obj may be any object; depends on the Python
         * code that was called
         */
        void sendObj(Object obj);

        /** Called when the thread begins running */
        default void started() {
        }

        /** Called when the thread is about to die */
        default void finished() {
        }
    }

    /** Methods only called from within Python scripts */
    private abstract class InternalClimatePythonListener {
        public abstract void sendObj(Object obj);

        @SuppressWarnings("unused")
        public final boolean isCanceled() {
            return ClimatePythonTask.this.isCanceled();
        }
    }

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String CLIMATE_ENTRY_SCRIPT_PATH = LocalizationUtil
            .join("aviation", "python", "ClimateEntry.py");

    /** For thread naming only */
    private static AtomicInteger threadCounter = new AtomicInteger();

    private final Object pythonLock;

    private final String methodName;

    private final Map<String, Object> methodKwargs;

    private final ClimatePythonListener listener;

    private final Thread thread;

    private volatile boolean canceled;

    /**
     * Only instantiable via
     * {@link #execute(String, Map, ClimatePythonListener, int, Object)}
     */
    private ClimatePythonTask(String methodName,
            Map<String, Object> methodKwargs, ClimatePythonListener listener,
            Object pythonLock) {
        this.methodName = methodName;
        this.methodKwargs = methodKwargs;
        this.listener = listener;
        String threadName = getClass().getSimpleName() + "-" + methodName + "-"
                + threadCounter.getAndIncrement();
        this.thread = new Thread(this::run, threadName);
        this.pythonLock = pythonLock;
    }

    private JepConfig makeJepConfig(File scriptFile) {
        String includePath = PyUtil.buildJepIncludePath(
                scriptFile.getParentFile().getPath(),
                AvnPyUtil.getLoggingHandlerDir(), AvnPyUtil.getPointDataDir(),
                AvnPyUtil.getCommonPythonDir());
        JepConfig jepConfig = new JepConfig();
        jepConfig.setInteractive(false);
        jepConfig.setIncludePath(includePath);
        jepConfig.setClassLoader(getClass().getClassLoader());
        jepConfig.setClassEnquirer(new NamingConventionClassEnquirer());
        jepConfig.setRedirectOutputStreams(true);
        return jepConfig;
    }

    private void run() {
        listener.started();
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            // force localization sync of entire directory of python scripts
            LocalizationFile parentDir = pm.getStaticLocalizationFile(
                    LocalizationType.COMMON_STATIC,
                    LocalizationUtil.getParent(CLIMATE_ENTRY_SCRIPT_PATH));
            parentDir.getFile(true);
            File scriptFile = pm.getStaticFile(CLIMATE_ENTRY_SCRIPT_PATH);
            JepConfig jepConfig = makeJepConfig(scriptFile);
            String scriptAbsPath = scriptFile.getAbsolutePath();

            synchronized (pythonLock) {
                try (ClimatePythonScript python = new ClimatePythonScript(
                        jepConfig, scriptAbsPath)) {
                    Map<String, Object> newMethodKwargs = new HashMap<>(
                            methodKwargs);
                    InternalClimatePythonListener internalListener;
                    internalListener = new InternalClimatePythonListener() {
                        @Override
                        public void sendObj(Object obj) {
                            listener.sendObj(obj);
                        }
                    };
                    newMethodKwargs.put("listener", internalListener);
                    try {
                        python.execute(methodName, newMethodKwargs);
                    } catch (JepException e) {
                        /*
                         * KeyboardInterrupt is thrown from within the Python
                         * script if it detects that the cancel flag has been
                         * set
                         */
                        if (e.getLocalizedMessage()
                                .contains("KeyboardInterrupt")) {
                            statusHandler.debug(e.getLocalizedMessage(), e);
                        } else {
                            throw e;
                        }
                    }
                }
            }
        } catch (Throwable t) {
            statusHandler.error(t.getLocalizedMessage(), t);
        } finally {
            listener.finished();
        }
    }

    private void cancelAfterTimeout(int timeoutSec) {
        try {
            thread.join(timeoutSec * TimeUtil.MILLIS_PER_SECOND);
        } catch (InterruptedException e) {
            // ignore
        }
        cancel();
    }

    /**
     * Request cancellation of the Python script. This relies on the Python code
     * to check a flag frequently to see if it has been canceled. However, if
     * the thread is in Java code, it will be interrupted immediately.
     *
     * This has no effect if the thread is already dead.
     */
    public void cancel() {
        canceled = true;
        if (thread.isAlive()) {
            thread.interrupt();
        }
    }

    /**
     * @return true if cancellation has been requested. This does not mean the
     *         thread has died yet.
     */
    public boolean isCanceled() {
        return canceled;
    }

    /** Wait for the thread to die */
    public void waitFor() throws InterruptedException {
        thread.join();
    }

    /**
     * Start the thread to run the Python script, and also start the timer
     * thread if timeoutSec is greater than zero
     *
     * @param timeoutSec
     */
    private synchronized void start(final int timeoutSec) {
        if (timeoutSec > 0) {
            Thread timerThread = new Thread(() -> {
                cancelAfterTimeout(timeoutSec);
            }, thread.getName() + "-Timer");
            thread.start();
            timerThread.start();
        } else {
            thread.start();
        }
    }

    /**
     * Execute specified climate Python method in a new thread
     *
     * @param methodName
     *            The method to execute
     * @param methodKwargs
     *            Map of keyword arguments to pass into the method
     * @param listener
     *            Listener that the thread will use to communicate with the
     *            caller of this method
     * @param timeoutSec
     *            Call {@link #cancel} after this many seconds. Zero means no
     *            timeout
     * @param pythonLock
     *            The thread will synchronize on this object while the Python
     *            interpreter is running
     * @return The running task
     */
    public static ClimatePythonTask execute(String methodName,
            Map<String, Object> methodKwargs, ClimatePythonListener listener,
            int timeoutSec, Object pythonLock) {
        ClimatePythonTask python = new ClimatePythonTask(methodName,
                methodKwargs, listener, pythonLock);
        python.start(timeoutSec);
        return python;
    }

    /**
     * Execute specified climate Python method in a new thread
     *
     * @param methodName
     *            The method to execute
     * @param methodKwargs
     *            Map of keyword arguments to pass into the method
     * @param listener
     *            Listener that the thread will use to communicate with the
     *            caller of this method
     * @param timeoutSec
     *            Call {@link #cancel} after this many seconds. Zero means no
     *            timeout
     * @return The running task
     */
    public static ClimatePythonTask execute(String methodName,
            Map<String, Object> methodKwargs, ClimatePythonListener listener,
            int timeoutSec) {
        return execute(methodName, methodKwargs, listener, timeoutSec,
                new Object());
    }
}
