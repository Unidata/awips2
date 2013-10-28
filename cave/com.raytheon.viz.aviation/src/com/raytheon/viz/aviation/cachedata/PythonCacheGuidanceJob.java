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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.jobs.AbstractQueueJob;
import com.raytheon.viz.aviation.activator.Activator;
import com.raytheon.viz.aviation.monitor.AvnPyUtil;

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
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class PythonCacheGuidanceJob extends
        AbstractQueueJob<CacheGuidanceRequest> {
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
    private Map<String, Map<String, String>> siteObjMaps;

    /**
     * Flag to indicate execution thread should shut down.
     */
    private boolean shutdown = false;

    /**
     * Current executing thread or null if none pending.
     */
    private CacheGuidanceRequest request = null;

    /**
     * List of requests whose results are waiting to be cached.
     */
    private List<CacheGuidanceRequest> waitList;

    /**
     * Object to synchronize threads waiting on requests.
     */
    private Object waitMonitor;

    /**
     * Object to synchronize suspending/restarting the instance of this class.
     */
    private Object suspendMonitor;

    /**
     * True when job suspended otherwise false.
     */
    boolean suspendJob;

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
        siteObjMaps = new HashMap<String, Map<String, String>>();
        waitMonitor = new Object();
        suspendMonitor = new Object();
        suspendJob = false;
        waitList = new ArrayList<CacheGuidanceRequest>();
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
        String filePath = runner.getPath();
        String includePath = PyUtil.buildJepIncludePath(runner.getParentFile()
                .getPath(), AvnPyUtil.getPointDataDir(), AvnPyUtil
                .getLoggingHandlerDir(), AvnPyUtil.getCommonPythonDir());
        python = new PythonScript(filePath, includePath,
                PythonCacheGuidanceJob.class.getClassLoader());
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
    private synchronized void waitAdd(CacheGuidanceRequest req) {
        synchronized (waitMonitor) {
            if (waitList.contains(req) == false) {
                waitList.add(req);
            }
        }
    }

    /**
     * Remove request from the wait list and notify a waiting thread.
     * 
     * @param req
     */
    private synchronized void waitRemove(CacheGuidanceRequest req) {
        synchronized (waitMonitor) {
            waitList.remove(req);
            waitMonitor.notify();
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
        ArrayList<CacheGuidanceRequest> queueList = new ArrayList<CacheGuidanceRequest>();
        for (CacheGuidanceRequest req : cacheRequests) {
            waitAdd(req);
        }

        // Get pending request to add after the cacheRequests.
        while (queue.peek() != null) {
            CacheGuidanceRequest qReq = queue.poll();
            if (cacheRequests.contains(qReq) == false) {
                queueList.add(qReq);
            }
        }

        // Add cache request to head of the queue unless it is the current
        // request.
        for (CacheGuidanceRequest req : cacheRequests) {
            if (req.equals(request) == false) {
                queue.add(req);
            }
        }

        // Queue other pending requests.
        for (CacheGuidanceRequest qReq : queueList) {
            queue.add(qReq);
        }
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
                synchronized (waitMonitor) {
                    while (waitList.contains(req)) {
                        waitMonitor.wait();
                        // Notify another waiting thread.
                        waitMonitor.notify();
                    }
                }
            }
        } catch (InterruptedException e) {
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
    public synchronized String getSiteObj(String siteID, String tag) {
        String siteObj = null;
        Map<String, String> siteObjs = siteObjMaps.get(siteID);
        if (siteObjs != null) {
            siteObj = siteObjs.get(tag);
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
    private synchronized void clearSiteObj(String siteID, String tag) {
        Map<String, String> siteObjs = siteObjMaps.get(siteID);
        if (siteObjs != null) {
            siteObjs.remove(tag);
        }
    }

    /**
     * Clear desired tags for sites.
     * 
     * @param tags
     *            A map with key of stites an array of tags to clear for the
     *            site
     */
    public synchronized void clearSiteObjs(Map<String, ArrayList<String>> tags) {
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
    private synchronized void setSiteObj(String siteID, String tag,
            String siteObj) {
        Map<String, String> siteObjs = siteObjMaps.get(siteID);
        if (siteObjs == null) {
            siteObjs = new HashMap<String, String>();
            siteObjMaps.put(siteID, siteObjs);
        }
        siteObjs.put(tag, siteObj);
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        try {
            initializePython();
        } catch (JepException e) {
            return new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                    "Error initializing guidance cache python", e);
        }
        try {
            while (shutdown == false) {
                if (suspendJob == true) {
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
                        // long t0 = System.currentTimeMillis();
                        String result = (String) python.execute(methodName,
                                args);
                        // long t1 = System.currentTimeMillis();
                        String siteID = request.getSiteID();
                        String tag = request.getTag();
                        setSiteObj(siteID, tag, result);
                        // System.out.println("Python cache guidance time: "
                        // + (t1 - t0) + ", " + siteID + " - " + tag);
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
            }
        } catch (InterruptedException e) {
            // Just go away
        } finally {
            siteObjMaps.clear();
            if (python != null) {
                python.dispose();
                python = null;
            }
            synchronized (waitMonitor) {
                waitList.clear();
                waitMonitor.notify();
            }
        }
        return Status.OK_STATUS;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.jobs.AbstractQueueJob#enqueue(com.raytheon.uf
     * .viz.core.jobs.QueueJobRequest)
     */
    public boolean enqueue(CacheGuidanceRequest req) {
        // Assume the requests cache site object needs to be updated.
        clearSiteObj(req.getSiteID(), req.getTag());
        return super.enqueue(req);
    }
}
