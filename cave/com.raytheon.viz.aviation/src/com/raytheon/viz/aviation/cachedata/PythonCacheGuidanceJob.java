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
package com.raytheon.viz.aviation.cachedata;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
import com.raytheon.uf.viz.core.jobs.AbstractQueueJob;
import com.raytheon.viz.aviation.activator.Activator;
import com.raytheon.viz.aviation.monitor.AvnPyUtil;

import jep.JepConfig;
import jep.JepException;

/**
 * Class to handle the cache data jobs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2011 8065       rferrel     Initial creation
 * Nov 11, 2012 1298       rferrel     Non-blocking dialog discovered problem
 *                                      adding dispose listener when not on the
 *                                      UI thread.
 * Aug 26, 2013 #2283      lvenable    Cleaned up some synchronized code.
 * 09Apr2014    #3005      lvenable    Remove waitMonitor, replaced waitList array with a Set,
 *                                     updated queueList to be a LinkedHashSet, added a catch
 *                                     to capture a throwable to prevent the thread from dying
 *                                     prematurely.
 * Jan 13, 2017  5959      njensen     Cleaned up warnings
 * Mar 22, 2017  6183      tgurney     Move python files to common_static
 * Jun 03, 2019  7852      dgilling    Update code for jep 3.8.
 * Jul 31, 2019  7878      tgurney     Handle pickled Python objects as byte[]
 *
 * </pre>
 *
 * @author rferrel
 */
public class PythonCacheGuidanceJob
        extends AbstractQueueJob<CacheGuidanceRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PythonCacheGuidanceJob.class);

    /**
     * The singleton instance of the class.
     */
    private static PythonCacheGuidanceJob instance = null;

    /**
     * Script used to execute requests placed on the queue.
     */
    private PythonScript python;

    /**
     * Current cached pickle string objects retrieved by requests.
     */
    private Map<String, Map<String, byte[]>> siteObjMaps;

    /**
     * Flag to indicate execution thread should shut down.
     */
    private boolean shutdown = false;

    /**
     * Current executing thread or null if none pending.
     */
    private volatile CacheGuidanceRequest request = null;

    /**
     * Set of requests whose results are waiting to be cached.
     */
    private Set<CacheGuidanceRequest> waitSet;

    /**
     * Object to synchronize suspending/restarting the instance of this class.
     */
    private Object suspendMonitor;

    /**
     * True when job suspended otherwise false.
     */
    private boolean suspendJob;

    /**
     * Obtain the singleton instance of this class.
     *
     * @return pythonCacheJob
     */
    public static synchronized PythonCacheGuidanceJob getInstance() {
        if (instance == null) {
            instance = new PythonCacheGuidanceJob(
                    "AvnFPS Cache Python Guidance");
            instance.setSystem(true);
            instance.schedule();
        }

        return instance;
    }

    /**
     * Shutdown and remove instance of this class.
     */
    public static synchronized void dispose() {
        if (instance != null) {
            instance.shutdown();
            instance = null;
        }
    }

    /**
     * A private constructor to force singleton.
     *
     * @param name
     */
    private PythonCacheGuidanceJob(String name) {
        super(name);
        siteObjMaps = new HashMap<>();
        suspendMonitor = new Object();
        suspendJob = false;
        waitSet = new HashSet<>();
    }

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
                .setClassLoader(PythonCacheGuidanceJob.class.getClassLoader());
        python = new PythonScript(config, filePath);
    }

    /**
     * Shutdown queue and clean up.
     */
    private void shutdown() {
        shutdown = true;
        restart();
    }

    /**
     * Indicate the jobs is to be suspended.
     */
    public static final void suspend() {
        if (instance != null) {
            instance.suspendJob = true;
        }
    }

    /**
     * Notify the job it can resume working.
     */
    public void restart() {
        synchronized (instance.suspendMonitor) {
            instance.suspendJob = false;
            instance.suspendMonitor.notify();
        }
    }

    /**
     * A request some thread is waiting for the results to be cached.
     *
     * @param req
     */
    private void waitAdd(CacheGuidanceRequest req) {
        synchronized (waitSet) {
            if (!waitSet.contains(req)) {
                waitSet.add(req);
            }
        }
    }

    /**
     * Remove request from the wait list and notify a waiting thread.
     *
     * @param req
     */
    private void waitRemove(CacheGuidanceRequest req) {
        synchronized (waitSet) {
            waitSet.remove(req);
            waitSet.notifyAll();
        }
    }

    /**
     * List of cache requests to move to the head of the queue.
     *
     * @param cacheRequests
     *            - list of cache requests
     */
    private synchronized void addToQueue(
            List<CacheGuidanceRequest> cacheRequests) {

        Set<CacheGuidanceRequest> queueSet = new LinkedHashSet<>(cacheRequests);

        for (CacheGuidanceRequest req : cacheRequests) {
            waitAdd(req);
        }

        queue.drainTo(queueSet);

        if (request != null) {
            queueSet.remove(request);
        }

        queue.addAll(queueSet);
    }

    /**
     * This method does not return until all the requests are cached.
     *
     * @param cacheRequests
     *            - The list of requests to cache.
     */
    public void waitForCacheRequests(List<CacheGuidanceRequest> cacheRequests) {
        addToQueue(cacheRequests);
        try {
            for (CacheGuidanceRequest req : cacheRequests) {
                synchronized (waitSet) {
                    while (waitSet.contains(req)) {
                        waitSet.wait();
                    }
                }
            }
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred when requested were being cached...", e);
        }
    }

    /**
     * Get cached site object or null if none cached.
     *
     * @param siteID
     *            - site object is for
     * @param tag
     *            - Unique tag
     * @return siteObj
     */
    public byte[] getSiteObj(String siteID, String tag) {
        byte[] siteObj = null;
        synchronized (siteObjMaps) {
            Map<String, byte[]> siteObjs = siteObjMaps.get(siteID);
            if (siteObjs != null) {
                siteObj = siteObjs.get(tag);
            }
        }
        return siteObj;
    }

    /**
     * Remove cached site object.
     *
     * @param siteID
     *            - Site object is for
     * @param tag
     *            - Unique tag
     */
    private void clearSiteObj(String siteID, String tag) {
        Map<String, byte[]> siteObjs = siteObjMaps.get(siteID);
        synchronized (siteObjMaps) {
            if (siteObjs != null) {
                siteObjs.remove(tag);
            }
        }
    }

    /**
     * Clear desired tags for sites.
     *
     * @param tags
     *            A map with key of stites an array of tags to clear for the
     *            site
     */
    public void clearSiteObjs(Map<String, List<String>> tags) {
        for (Object s : tags.keySet().toArray()) {
            String siteID = s.toString();
            for (String tag : tags.get(siteID)) {
                clearSiteObj(siteID, tag);
            }
        }
    }

    /**
     * Cache the site object.
     *
     * @param siteID
     *            - Site object is for
     * @param tag
     *            - Unique tag
     * @param siteObj
     *            - Pickle string to cache
     */
    private void setSiteObj(String siteID, String tag, byte[] siteObj) {
        synchronized (siteObjMaps) {
            Map<String, byte[]> siteObjs = siteObjMaps.get(siteID);
            if (siteObjs == null) {
                siteObjs = new HashMap<>();
                siteObjMaps.put(siteID, siteObjs);
            }
            siteObjs.put(tag, siteObj);
        }
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        try {
            initializePython();
        } catch (JepException e) {
            return new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                    "Error initializing guidance cache python", e);
        }
        try {
            while (!shutdown) {

                try {
                    if (suspendJob) {
                        synchronized (suspendMonitor) {
                            queue.clear();
                            siteObjMaps.clear();
                            suspendMonitor.wait();
                        }
                        continue;
                    }
                    if (queue.peek() != null) {
                        request = queue.poll();
                        Map<String, Object> args = request.getPythonArguments();
                        String methodName = request.getGuidanceType()
                                .getPythonMethod() + "Retrieve";
                        try {
                            byte[] result = (byte[]) python.execute(methodName,
                                    args);
                            String siteID = request.getSiteID();
                            String tag = request.getTag();
                            setSiteObj(siteID, tag, result);
                            waitRemove(request);
                        } catch (JepException e) {
                            if (e.getMessage().contains("NoDataException")) {
                                String msg = e.getMessage().split("'")[3];
                                statusHandler.handle(Priority.PROBLEM, msg, e);
                            } else {
                                statusHandler.handle(Priority.PROBLEM,
                                        "Error generating guidance", e);
                            }
                        } finally {
                            request = null;
                        }
                    } else {
                        try {
                            Thread.sleep(20);
                        } catch (InterruptedException e) {
                            break;
                        }
                    }
                } catch (Throwable t) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error generating guidance", t);
                }
            }
        } finally {
            siteObjMaps.clear();
            if (python != null) {
                try {
                    python.dispose();
                } catch (JepException e) {
                    statusHandler.debug("Failed to dispose script instance.",
                            e);
                }
                python = null;
            }
            synchronized (waitSet) {
                waitSet.clear();
                waitSet.notifyAll();
            }
        }
        return Status.OK_STATUS;
    }

    @Override
    public boolean enqueue(CacheGuidanceRequest req) {
        // Assume the requests cache site object needs to be updated.
        clearSiteObj(req.getSiteID(), req.getTag());
        return super.enqueue(req);
    }
}
