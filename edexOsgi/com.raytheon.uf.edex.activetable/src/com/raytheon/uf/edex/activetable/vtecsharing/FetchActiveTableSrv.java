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
package com.raytheon.uf.edex.activetable.vtecsharing;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import com.google.common.util.concurrent.MoreExecutors;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.registry.RegistryException;
import com.raytheon.uf.edex.site.ISiteActivationListener;
import com.raytheon.uf.edex.site.SiteAwareRegistry;

/**
 * Service that fetches neighboring sites' active table entries that are
 * relevant to this site using requestAT.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2013            dgilling    Initial creation
 * Feb 20, 2014   #2824    randerso    Changed log level of message when activating FetchAT
 *                                     Registered with SiteAwareRegistry so we can stop
 *                                     fetching when site is deactivated.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class FetchActiveTableSrv implements ISiteActivationListener {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FetchActiveTableSrv.class);

    private final Map<String, FetchATJobConfig> siteConfigMap;

    private final Map<String, ScheduledFuture<?>> siteJobInstanceMap;

    private final ScheduledExecutorService jobExecutor;

    public FetchActiveTableSrv() {
        siteConfigMap = new ConcurrentHashMap<String, FetchATJobConfig>();
        siteJobInstanceMap = new ConcurrentHashMap<String, ScheduledFuture<?>>();
        jobExecutor = MoreExecutors
                .getExitingScheduledExecutorService((ScheduledThreadPoolExecutor) Executors
                        .newScheduledThreadPool(1));

        try {
            SiteAwareRegistry.getInstance().register(this);
        } catch (RegistryException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error registering with SiteAwareRegistry", e);
        }
    }

    public void addSite(Map<String, Object> configData) {
        FetchATJobConfig config = new FetchATJobConfig(configData);
        final String site = config.getSiteId();

        statusHandler.info("Activating FetchAT for " + site);
        statusHandler.debug("Site: " + site + " config: " + config);

        if ((siteConfigMap.containsKey(site))
                && siteConfigMap.get(site).equals(config)) {
            return;
        }

        Runnable job = new Runnable() {

            @Override
            public void run() {
                statusHandler.info("Starting requestAT process for site: "
                        + site);

                // requestAT -H ourHost -P ourPort -L ourServerProto -M mhsid
                // -S ourSite -t irtWebAddr -x transmitScript
                FetchATJobConfig jobConfig = siteConfigMap.get(site);
                List<String> args = new ArrayList<String>(17);
                args.add("requestAT");
                args.add("-H");
                args.add(jobConfig.getServerHost());
                args.add("-P");
                args.add(jobConfig.getPort());
                args.add("-L");
                args.add(jobConfig.getProtocolV());
                args.add("-M");
                args.add(jobConfig.getMhsId());
                args.add("-S");
                args.add(jobConfig.getSiteId());
                args.add("-a");
                args.add(jobConfig.getAncfAddress());
                args.add("-b");
                args.add(jobConfig.getBncfAddress());
                args.add("-x");
                args.add(jobConfig.getTransmitScript());

                // String msg = Joiner.on(' ').join(args);
                // statusHandler.debug("Running command: " + msg);

                try {
                    ProcessBuilder command = new ProcessBuilder(args);
                    command.start();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error executing requestAT: ", e);
                }
            }
        };

        try {
            siteConfigMap.put(site, config);
            ScheduledFuture<?> jobInstance = jobExecutor.scheduleAtFixedRate(
                    job, 10, config.getInterval(), TimeUnit.SECONDS);
            siteJobInstanceMap.put(site, jobInstance);
        } catch (RejectedExecutionException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to submit fetchAT job for execution:", e);
            siteConfigMap.remove(site);
            siteJobInstanceMap.remove(site);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.site.ISiteActivationListener#deactivateSite(java
     * .lang.String)
     */
    @Override
    public void deactivateSite(String siteID) throws Exception {
        ScheduledFuture<?> siteJob = siteJobInstanceMap.remove(siteID);
        if (siteJob != null) {
            statusHandler.info("Deactivating FetchAT for " + siteID);
            siteJob.cancel(false);
        }
        siteConfigMap.remove(siteID);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.site.ISiteActivationListener#activateSite(java.lang
     * .String)
     */
    @Override
    public void activateSite(String siteID) throws Exception {
        return;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.site.ISiteActivationListener#getActiveSites()
     */
    @Override
    public Set<String> getActiveSites() {
        return Collections.emptySet();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.site.ISiteActivationListener#validateConfig(java
     * .lang.String)
     */
    @Override
    public String validateConfig(String siteID) {
        return "";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.site.ISiteActivationListener#registered()
     */
    @Override
    public void registered() {
        return;
    }
}
