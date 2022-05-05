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
package com.raytheon.viz.gfe.gfeclient;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.concurrent.Semaphore;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.GfeClientRequest;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonEval;
import com.raytheon.uf.common.python.PythonIncludePathUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.core.ProgramArguments;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManagerFactory;
import com.raytheon.viz.gfe.python.GfeCavePyIncludeUtil;
import com.raytheon.viz.ui.personalities.awips.AbstractAWIPSComponent;

import jep.JepConfig;
import jep.JepException;
import jep.NamingConventionClassEnquirer;

/**
 * GFE Client server application component
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 19, 2017  6092     randerso  Initial creation. Adapted from
 *                                  GfeClient.java
 * Feb 20, 2018  6602     dgilling  Update for new text utilities path.
 * Jan 07, 2019  21019    ryu       Fix issue of gfeclient hanging on exit.
 * Jun 03, 2019  7852     dgilling  Update code for jep 3.8.
 *
 * </pre>
 *
 * @author randerso
 */

public class GfeClientServer extends AbstractAWIPSComponent
        implements INotificationObserver {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GfeClientServer.class);

    private class PythonThread extends Thread {

        private Semaphore queueSemaphore = new Semaphore(0);

        private Semaphore awaitSemaphore = new Semaphore(0);

        private GfeClientRequest request;

        private PythonEval pythonEval;

        public PythonThread() {
            super("PythonThread");
            this.start();
        }

        private void initializeJep() {
            try {
                String pyVizDir = new File(FileLocator
                        .resolve(FileLocator.find(
                                Activator.getDefault().getBundle(),
                                new Path(FileUtil.join("python", "pyViz")),
                                null))
                        .getPath()).getPath();

                String utilityDir = new File(FileLocator
                        .resolve(FileLocator.find(
                                Activator.getDefault().getBundle(),
                                new Path(FileUtil.join("python", "utility")),
                                null))
                        .getPath()).getPath();

                boolean includeUser = (!"SITE"
                        .equals(VizApp.getWsId().getUserName()));

                String includePath = PyUtil.buildJepIncludePath(true,
                        utilityDir,
                        PythonIncludePathUtil.getCommonPythonIncludePath("time",
                                "dataaccess"),
                        GfePyIncludeUtil.getCommonGfeIncludePath(),
                        GfePyIncludeUtil.getConfigIncludePath(includeUser),
                        pyVizDir,
                        GfePyIncludeUtil.getUtilitiesIncludePath(includeUser),
                        GfePyIncludeUtil.getIToolIncludePath(),
                        GfePyIncludeUtil.getVtecIncludePath(),
                        GfeCavePyIncludeUtil.getAutotestIncludePath(),
                        GfePyIncludeUtil
                                .getTextUtilitiesIncludePath(includeUser),
                        GfePyIncludeUtil
                                .getTextProductsIncludePath(includeUser),
                        GfePyIncludeUtil
                                .getCombinationsIncludePath(includeUser),
                        GfeCavePyIncludeUtil.getTestsIncludePath(),
                        GfePyIncludeUtil.getProceduresIncludePath(includeUser));

                JepConfig config = new JepConfig().setInteractive(false)
                        .setIncludePath(includePath)
                        .setClassLoader(GfeClientServer.class.getClassLoader())
                        .setClassEnquirer(new NamingConventionClassEnquirer())
                        .setRedirectOutputStreams(true);
                pythonEval = new PythonEval(config);
                pythonEval.eval("import sys");
            } catch (IOException | JepException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }

        public void queueRequest(GfeClientRequest request) {
            this.request = request;
            queueSemaphore.release();
            try {
                awaitSemaphore.acquire();
            } catch (InterruptedException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }

        @Override
        public void run() {
            while (!shutDown) {
                try {
                    queueSemaphore.acquire();
                } catch (InterruptedException e) {
                    statusHandler.error(e.getLocalizedMessage(), e);
                }

                processRequest();
                this.request = null;

                awaitSemaphore.release();
            }

            DataManagerFactory.disposeAll();
        }

        private void processRequest() {
            String script = request.getScript();
            if (GfeClientRequest.SHUTDOWN_CMD.equalsIgnoreCase(script)) {
                shutDown = true;

                if (pythonEval != null) {
                    try {
                        pythonEval.close();
                    } catch (JepException e) {
                        statusHandler
                                .debug("Failed to dispose script instance.", e);
                    }
                }

                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        // wake up the display so we can shut down
                    }
                });
                return;
            }

            Date drt = request.getTime();
            if (drt == null) {
                SimulatedTime.getSystemTime().setRealTime();
            } else {
                SimulatedTime.getSystemTime().setTime(drt);
            }

            if (pythonEval == null) {
                initializeJep();
            }

            try {
                pythonEval.eval("sys.argv = ['']");
                for (String arg : request.getArgs()) {
                    pythonEval.eval("sys.argv.append('"
                            + arg.replaceAll("'", "\\\\'") + "')");
                }
                pythonEval.runScript(script);
            } catch (JepException e) {
                statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(),
                        e);
            }
            return;
        }
    }

    private boolean shutDown = false;

    private PythonThread pythonThread;

    /**
     * Default constructor
     */
    public GfeClientServer() {
        super();
    }

    @Override
    protected void startInternal(String componentName) throws Exception {
        String queue = ProgramArguments.getInstance().getString("-queue");
        if (StringUtils.isEmpty(queue)) {
            statusHandler.fatal(
                    "No -queue parameter specified, GFE Client process terminating.");
            return;
        }

        pythonThread = new PythonThread();

        NotificationManagerJob.addQueueObserver(queue, this);

        statusHandler.info(
                "GFE Client process started and listening on queue: " + queue);

        while (!shutDown) {
            try {
                if (!Display.getCurrent().readAndDispatch()) {
                    Display.getCurrent().sleep();
                }
            } catch (Throwable t) {
                statusHandler.error(t.getLocalizedMessage(), t);
            }
        }

        NotificationManagerJob.removeQueueObserver(queue, null, this);

        statusHandler
                .info("GFE Client process terminating. No longer listening on queue: "
                        + queue);
    }

    @Override
    protected int getRuntimeModes() {
        return (NON_UI | ALERT_VIZ);
    }

    @Override
    public void notificationArrived(NotificationMessage[] messages) {

        for (NotificationMessage message : messages) {
            try {
                Object payload = message.getMessagePayload();
                if (!(payload instanceof GfeClientRequest)) {
                    statusHandler
                            .error("GfeClient received unexpected message of type: "
                                    + payload.getClass().getName());
                    continue;
                }

                GfeClientRequest request = (GfeClientRequest) payload;
                String script = request.getScript();
                if (StringUtil.isEmptyString(script)) {
                    statusHandler.error("No script specified to run - exiting");
                    continue;
                }

                long t0 = System.currentTimeMillis();

                statusHandler.info("GFE Client received request: " + request);

                pythonThread.queueRequest(request);

                long t1 = System.currentTimeMillis();
                statusHandler.info(String.format("Time to run script %s: %d ms",
                        script, (t1 - t0)));
            } catch (NotificationException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }
    }

}
