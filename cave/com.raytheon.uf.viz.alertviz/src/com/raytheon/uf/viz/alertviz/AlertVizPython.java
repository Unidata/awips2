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
package com.raytheon.uf.viz.alertviz;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import jep.JepException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 9, 2009             chammack     Initial creation
 * May 5, 2011  9101       cjeanbap     Check Python Script type
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class AlertVizPython extends Job {

    private String alertVizPythonDir;

    private static ConcurrentLinkedQueue<ExecObj> execQueue = new ConcurrentLinkedQueue<ExecObj>();

    private static ConcurrentLinkedQueue<Runnable> runQueue = new ConcurrentLinkedQueue<Runnable>();

    private static AlertVizPython pythonThread;

    public static final String ALERTVIZ_PYTHON_PATH = "alertViz/python";

    private static class ExecObj {
        public StatusMessage statusMessage;

        public AlertMetadata alertMetadata;

        public TrayConfiguration globalConfiguration;

    }

    public static void enqueue(StatusMessage statusMessage,
            AlertMetadata alertMetadata, TrayConfiguration globalConfiguration)
            throws FileNotFoundException {
        File file = PathManagerFactory.getPathManager().getStaticFile(
                ALERTVIZ_PYTHON_PATH + "/" + alertMetadata.getPythonScript());
        if (file == null) {
            throw new FileNotFoundException(
                    "Unable to find file in localization with name: "
                            + alertMetadata.getPythonScript());
        }

        if (!alertMetadata.getPythonScript().endsWith(".py")) {
            statusMessage.setDetails(file.getAbsolutePath());
            alertMetadata.setPythonScript("PyShellScriptWrapper.py");
        }

        ExecObj eo = new ExecObj();
        eo.statusMessage = statusMessage;
        eo.alertMetadata = alertMetadata;
        eo.globalConfiguration = globalConfiguration;
        execQueue.add(eo);

        if (pythonThread == null) {
            pythonThread = new AlertVizPython();
            pythonThread.setSystem(true);
            pythonThread.schedule();
        }

    }

    public static void run(Runnable run) {
        runQueue.add(run);
    }

    private AlertVizPython() {
        super("Alertviz Python Executor");
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);
        File file = pathMgr.getFile(cx, ALERTVIZ_PYTHON_PATH);
        cx = pathMgr.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.SITE);
        File file2 = pathMgr.getFile(cx, ALERTVIZ_PYTHON_PATH);

        cx = pathMgr.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER);
        File file3 = pathMgr.getFile(cx, ALERTVIZ_PYTHON_PATH);

        this.alertVizPythonDir = file.getAbsolutePath() + ":"
                + file2.getAbsolutePath() + ":" + file3.getAbsolutePath();

    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        final String processorPython = ALERTVIZ_PYTHON_PATH
                + "/MasterProcessor.py";
        PythonScript pythonScript = null;
        try {
            File file = PathManagerFactory.getPathManager().getStaticFile(
                    processorPython);
            if (file == null) {
                throw new RuntimeException("Master processor file missing");
            }

            pythonScript = new PythonScript(file.getAbsolutePath(),
                    alertVizPythonDir, this.getClass().getClassLoader());

            while (!pythonThread.getThread().isInterrupted()) {
                while (!runQueue.isEmpty()) {
                    runQueue.remove().run();
                }

                while (!execQueue.isEmpty()) {
                    ExecObj eo = execQueue.remove();

                    try {
                        HashMap<String, Object> args = new HashMap<String, Object>();
                        args.put("statusMessage", eo.statusMessage);
                        args.put("alertMetadata", eo.alertMetadata);
                        args.put("globalConfiguration", eo.globalConfiguration);

                        pythonScript.execute("process", args);
                    } catch (JepException e) {
                        Container.logInternal(Priority.ERROR,
                                "AlertVizPython: JEPP exception executing python script: "
                                        + processorPython, e);
                    }
                }

                try {
                    Thread.sleep(50);
                } catch (InterruptedException e) {
                    return Status.OK_STATUS;
                }
            }
        } catch (JepException e) {
            Container.logInternal(Priority.ERROR,
                    "AlertVizPython: exception creating JEPP python script: "
                            + processorPython, e);
        } finally {
            if (pythonScript != null) {
                pythonScript.dispose();
            }
        }
        return Status.OK_STATUS;
    }

}
