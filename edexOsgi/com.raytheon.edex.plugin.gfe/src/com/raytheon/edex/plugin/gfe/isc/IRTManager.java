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

package com.raytheon.edex.plugin.gfe.isc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Manages interactions for the IRT server used with the GFE ISC capability
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/10/09     1995       bphillip    Initial creation
 * 06/13/13     2044       randerso    Refactored to use IFPServer
 * 03/11/15     4128       dgilling    Refactored to use ISCServiceProvider.
 * 11/11/15     5110       dgilling    Fix copy/paste error in log message.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public final class IRTManager {

    /** The logger */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(IRTManager.class);

    /** Map of active IRT connections keyed by site */
    private final ConcurrentMap<String, Future<?>> irtMap;

    /** List of valid ISC sites. */
    private final Set<String> registeredSiteIDs;

    private ExecutorService jobExecutor;

    /**
     * Constructs the singleton instance of the IRT Manager
     */
    public IRTManager() {
        this.irtMap = new ConcurrentHashMap<>();
        this.registeredSiteIDs = new CopyOnWriteArraySet<>();
    }

    /**
     * Determines whether the specified site should continue to (re-) register
     * with IRT.
     * 
     * @param siteID
     *            Site identifier to check for.
     * @return {@code true} if the site should continue registration with IRT.
     *         {@code false} if not.
     */
    public boolean shouldRegister(final String siteID) {
        /*
         * We use this separate Set to hold site IDs to avoid a race condition.
         * While it would be more convenient to use the keys of the irtMap to
         * maintain the list of sites that should be attempting to register with
         * IRT, this will cause a race condition when the Runnable's attempt to
         * call this method. It's likely the job will hit the shouldRegister()
         * check before the Future has been added to the Map and thus fail to
         * ever attempt registration with IRT.
         */
        return registeredSiteIDs.contains(siteID);
    }

    /**
     * Register the given site with the IRT server.
     * 
     * @param siteID
     *            Site identifier for the site to register with the IRT server.
     * @param gfeConfig
     *            The {@code IFPServerConfig} configuration data for the site.
     */
    public void activateSite(String siteID, IFPServerConfig gfeConfig) {
        if (!irtMap.containsKey(siteID)) {
            statusHandler.info("Starting IRT registration thread for site ["
                    + siteID + "]");

            registeredSiteIDs.add(siteID);
            Runnable job = constructJob(siteID, gfeConfig);

            try {
                Future<?> future = jobExecutor.submit(job);
                irtMap.put(siteID, future);
            } catch (RejectedExecutionException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to submit IRT registration job for execution:",
                        e);
                irtMap.remove(siteID);
            }
        }
    }

    private GfeIRT constructJob(final String siteID,
            final IFPServerConfig config) {
        return new GfeIRT(siteID, config, this);
    }

    /**
     * Unregisters the given site with the IRT server.
     * 
     * @param siteID
     *            Site identifier of the site to unregister.
     */
    public void deactivateSite(String siteID) {
        registeredSiteIDs.remove(siteID);
        Future<?> job = irtMap.remove(siteID);
        if (job != null) {
            statusHandler.info("Deactivating IRT registration thread for "
                    + siteID);
            job.cancel(false);
        }
    }

    /**
     * Startup hook for this bean. Initializes job pool.
     */
    public void preStart() {
        statusHandler.info("Initializing IRTManager...");

        jobExecutor = Executors.newCachedThreadPool();
    }

    /**
     * Preliminary shutdown hook for this bean. Stops all running IRT
     * registration jobs and initiates shutdown of the job pool.
     */
    public void preStop() {
        statusHandler.info("Shutting down IRTManager...");

        Collection<String> siteIds = new ArrayList<>(registeredSiteIDs);
        for (String siteId : siteIds) {
            deactivateSite(siteId);
        }

        if (jobExecutor != null) {
            jobExecutor.shutdown();
        }
    }

    /**
     * Shutdown completion hook for this bean. Clears all saved state for this
     * bean.
     */
    public void postStop() {
        jobExecutor = null;
        irtMap.clear();
        registeredSiteIDs.clear();
    }
}
