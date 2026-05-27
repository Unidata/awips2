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
package com.raytheon.viz.aviation.guidance;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.jobs.AbstractQueueJob;
import com.raytheon.uf.viz.core.jobs.IRequestCompleteListener;
import com.raytheon.viz.aviation.activator.Activator;
import com.raytheon.viz.aviation.cachedata.CacheGuidanceRequest;
import com.raytheon.viz.aviation.cachedata.PythonCacheGuidanceJob;
import com.raytheon.viz.aviation.guidance.GuidanceRequest.GuidanceType;
import com.raytheon.viz.aviation.monitor.AvnPyUtil;

import jep.JepConfig;
import jep.JepException;

/**
 * Job for using legacy AvnFPS python to generate guidance. Caches a single
 * python interpreter and keeps the thread around to boost performance.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2009            njensen     Initial creation
 * Apr 14, 2011 8065       rferrel     Implemented enqueue to place
 *                                     Alerts at the front of the queue
 *                                     and work with data caching.
 * Nov 28, 2012 1363       rferrel     No longer add a dispose listener so the
 *                                      creation of instance can be done on
 *                                      any thread.
 * May 14, 2015 4259       njensen     Updates for new JEP API
 * Jan 13, 2017 5959       njensen     Cleaned up warnings
 * Mar 22, 2017 6183       tgurney     Move python files to common_static
 * Jun 03, 2019  7852      dgilling    Update code for jep 3.8.
 *
 * </pre>
 *
 * @author njensen
 */

public class PythonGuidanceJob extends AbstractQueueJob<GuidanceRequest> {

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PythonGuidanceJob.class);

    /**
     * The single instance of this class.
     */
    private static PythonGuidanceJob instance;

    /**
     * List of tabs to send alerts to in order to purge data cache.
     */
    private static final List<ViewerTab> viewerList = new ArrayList<>();

    /**
     * instance of python to execute the python methods.
     */
    private PythonScript python;

    /**
     * Object to synchronize on when changing the suspendJob state.
     */
    private Object suspendMonitor;

    /**
     * flag to indicate job is suspended.
     */
    private boolean suspendJob;

    /**
     * Private constructor to enforce singleton.
     *
     * @param name
     */
    private PythonGuidanceJob(String name) {
        super(name);
        suspendMonitor = new Object();
        suspendJob = false;
    }

    /**
     * Flag to indicate job should go away.
     */
    private boolean shutdown = false;

    /**
     * Initializes the python interpreter necessary to generate the different
     * guidances
     *
     * @throws JepException
     */
    private synchronized void initializePython() throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        File runner = pathMgr.getStaticFile("aviation/python/GuidanceEntry.py");
        LocalizationContext baseCtx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        // need getFile to pull whole python dir from localization server
        pathMgr.getLocalizationFile(baseCtx, "aviation/python").getFile();
        String filePath = runner.getPath();
        String includePath = PyUtil.buildJepIncludePath(
                runner.getParentFile().getPath(), AvnPyUtil.getPointDataDir(),
                AvnPyUtil.getLoggingHandlerDir(),
                AvnPyUtil.getCommonPythonDir());
        JepConfig config = new JepConfig().setIncludePath(includePath)
                .setClassLoader(PythonGuidanceJob.class.getClassLoader());
        python = new PythonScript(config, filePath);
    }

    /**
     * Gets the instance of the job. There should only ever be one
     * PythonGuidanceJob (i.e. one thread)
     *
     * @return
     */
    public static final synchronized PythonGuidanceJob getInstance() {
        if (instance == null) {
            instance = new PythonGuidanceJob("AvnFPS Python Guidance");
            instance.setSystem(true);
            instance.schedule();
        }
        return instance;
    }

    public static final synchronized void dispose() {
        if (instance != null) {
            instance.shutdown();
            instance = null;
        }
    }

    /**
     * Shutdowns the running instance and cleans up any threads.
     */
    private void shutdown() {
        shutdown = true;
        restart();
        instance = null;
    }

    /**
     * Add a tab that needs to be told about alerts.
     *
     * @param viewerTab
     */
    public static final synchronized void addViewerTab(ViewerTab viewerTab) {
        viewerList.add(viewerTab);
        PythonCacheGuidanceJob.getInstance().restart();
        PythonGuidanceJob.getInstance().restart();
    }

    /**
     * Tab is being disposed and no longer needs to receive alerts.
     *
     * @param viewerTab
     */
    public static final synchronized void removeViwerTab(ViewerTab viewerTab) {
        viewerList.remove(viewerTab);
        PythonCacheGuidanceJob.suspend();
        PythonGuidanceJob.suspend();
    }

    /**
     * Stop process of request and clear the queue.
     */
    private static final void suspend() {
        if (instance != null) {
            instance.suspendJob = true;
        }
    }

    /**
     * Restart processing of request.
     */
    private void restart() {
        synchronized (instance.suspendMonitor) {
            instance.suspendJob = false;
            instance.suspendMonitor.notify();
        }
    }

    @Override
    public boolean enqueue(GuidanceRequest req) {
        boolean queueAlert = false;
        GuidanceType type = req.getGuidanceType();
        if (type == GuidanceType.METAR_CACHE
                || type == GuidanceType.TAF_CACHE) {
            queueAlert = true;
        }

        List<GuidanceRequest> queueList = new ArrayList<>();

        if (queueAlert) {
            // Move alert to the top of the queue.
            while (queue.peek() != null) {
                queueList.add(queue.poll());
            }
            boolean value = super.enqueue(req);

            for (GuidanceRequest aReq : queueList) {
                super.enqueue(aReq);
            }

            return value;
        }

        // This assumes the new request is for the current view tab.
        // Therefore non-alert requests on the queue are no longer needed and
        // can be purged.
        while (queue.peek() != null) {
            GuidanceRequest qReq = queue.poll();
            GuidanceType qType = qReq.getGuidanceType();
            if (qType == GuidanceType.METAR_CACHE
                    || qType == GuidanceType.TAF_CACHE) {
                queueList.add(qReq);
            }
        }

        // Put any alerts back on the queue.
        for (GuidanceRequest aReq : queueList) {
            super.enqueue(aReq);
        }

        return super.enqueue(req);
    }

    @SuppressWarnings("unchecked")
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        try {
            initializePython();
        } catch (JepException e) {
            return new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                    "Error initializing guidance python", e);
        }

        try {
            while (!shutdown) {
                if (suspendJob) {
                    synchronized (suspendMonitor) {
                        queue.clear();
                        suspendMonitor.wait();
                    }
                    continue;
                }
                if (queue.peek() != null) {
                    GuidanceRequest request = queue.poll();
                    final IRequestCompleteListener<String[]> listener = request
                            .getListener();
                    Map<String, Object> args = request.getPythonArguments();
                    String methodName = request.getGuidanceType()
                            .getPythonMethod();
                    try {
                        final String[] result = ((List<String>) python
                                .execute(methodName, args))
                                        .toArray(new String[0]);
                        if (request
                                .getGuidanceType() == GuidanceType.METAR_CACHE) {
                            // Update only the METAR tab
                            List<String> siteIDs = request.getSiteIDs();
                            for (ViewerTab viewerTab : PythonGuidanceJob.viewerList) {
                                if (viewerTab instanceof MetarViewer) {
                                    viewerTab.alertSites(siteIDs);
                                    break;
                                }
                            }
                        } else if (request
                                .getGuidanceType() == GuidanceType.TAF_CACHE) {
                            // Update all tabs starting with the current tab.
                            List<String> siteIDs = request.getSiteIDs();

                            ViewerTab currentTab = null;

                            // Find and notify the current view tab.
                            for (ViewerTab viewerTab : PythonGuidanceJob.viewerList) {
                                if (viewerTab.isCurrentTab()) {
                                    currentTab = viewerTab;
                                    currentTab.alertSites(siteIDs);
                                    break;
                                }
                            }

                            // Notify the rest of the view tabs
                            for (ViewerTab viewerTab : PythonGuidanceJob.viewerList) {
                                if (viewerTab != currentTab) {
                                    viewerTab.alertSites(siteIDs);
                                }
                            }
                        }

                        VizApp.runAsync(() -> listener.requestComplete(result));

                    } catch (JepException e) {
                        if (e.getMessage().contains("NoDataException")) {
                            String msg = e.getMessage().split("'")[3];
                            statusHandler.handle(Priority.PROBLEM, msg, e);
                        } else {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Error generating guidance", e);
                        }
                    }
                } else {
                    try {
                        Thread.sleep(20);
                    } catch (InterruptedException e) {
                        break;
                    }
                }
            }
        } catch (InterruptedException e) {
            // Do nothing just go away
        } finally {
            if (python != null) {
                try {
                    python.dispose();
                } catch (JepException e) {
                    statusHandler.debug("Failed to dispose script instance.",
                            e);
                }
                python = null;
            }
        }
        return Status.OK_STATUS;
    }

    /**
     * This method does not return until the list of cache requests are
     * completed.
     *
     * @param cacheRequests
     */
    public void waitForCacheRequests(List<CacheGuidanceRequest> cacheRequests) {
        PythonCacheGuidanceJob.getInstance()
                .waitForCacheRequests(cacheRequests);
    }

}
