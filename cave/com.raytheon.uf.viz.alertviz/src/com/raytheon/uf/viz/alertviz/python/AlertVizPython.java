package com.raytheon.uf.viz.alertviz.python;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;

/**
 * This is the main class for the alert visualization.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date        Ticket#    Engineer      Description
 * ----------  ---------  ------------  --------------------------
 * 2017-03-24  DR 16985   D. Friedman   Restore Python script functionality
 *
 * </pre>
 *
 */
public class AlertVizPython {

    private static String THREAD_POOL_NAME = "alertviz-python-scripts";
    private static int N_THREADS = 2;

    private static volatile AlertVizPython instance;

    PythonJobCoordinator<PythonScript> pythonJobCoordinator = new PythonJobCoordinator<>(N_THREADS, THREAD_POOL_NAME, new AlertVizPythonScriptFactory());

    public void doRun(StatusMessage statMsg, AlertMetadata alertMetadata, TrayConfiguration gConfig) {
        pythonJobCoordinator.submitJob(new AlertVizPythonExecutor(statMsg, alertMetadata, gConfig));
    }

    private static AlertVizPython getInstance() {
        AlertVizPython result = instance;
        if (result == null) {
            synchronized (AlertVizPython.class) {
                if (instance == null) {
                    result = instance = new AlertVizPython();
                }
            }
        }
        return result;
    }

    public static void run(StatusMessage statMsg, AlertMetadata alertMetadata, TrayConfiguration gConfig) {
        getInstance().doRun(statMsg, alertMetadata, gConfig);

    }

}
