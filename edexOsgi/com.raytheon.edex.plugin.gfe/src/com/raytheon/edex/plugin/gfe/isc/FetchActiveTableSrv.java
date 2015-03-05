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
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.edex.core.IContextStateProcessor;

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
 * Feb 26, 2015   #4128    dgilling    Moved to edex.gfe plugin, rewritten as
 *                                     IContextStateProcessor.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class FetchActiveTableSrv implements IContextStateProcessor {

    private static final class FetchATJobConfig {

        private final String siteId;

        private final long interval;

        private final String ancfAddress;

        private final String bncfAddress;

        private final String serverHost;

        private final String port;

        private final String protocolV;

        private final String mhsId;

        private final String transmitScript;

        public FetchATJobConfig(final String siteId,
                final IFPServerConfig gfeConfig) {
            this.siteId = siteId;
            this.interval = gfeConfig.tableFetchTime();
            this.ancfAddress = gfeConfig.iscRoutingTableAddress().get("ANCF");
            this.bncfAddress = gfeConfig.iscRoutingTableAddress().get("BNCF");
            this.serverHost = gfeConfig.getServerHost();
            this.port = Long.toString(gfeConfig.getRpcPort());
            this.protocolV = Long.toString(gfeConfig.getProtocolVersion());
            this.mhsId = gfeConfig.getMhsid();
            this.transmitScript = gfeConfig.transmitScript();
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append("FetchATJobConfig [siteId=");
            builder.append(siteId);
            builder.append(", interval=");
            builder.append(interval);
            builder.append(", ancfAddress=");
            builder.append(ancfAddress);
            builder.append(", bncfAddress=");
            builder.append(bncfAddress);
            builder.append(", serverHost=");
            builder.append(serverHost);
            builder.append(", port=");
            builder.append(port);
            builder.append(", protocolV=");
            builder.append(protocolV);
            builder.append(", mhsId=");
            builder.append(mhsId);
            builder.append(", transmitScript=");
            builder.append(transmitScript);
            builder.append("]");
            return builder.toString();
        }

        public String getSiteId() {
            return siteId;
        }

        public long getInterval() {
            return interval;
        }

        public String getAncfAddress() {
            return ancfAddress;
        }

        public String getBncfAddress() {
            return bncfAddress;
        }

        public String getServerHost() {
            return serverHost;
        }

        public String getPort() {
            return port;
        }

        public String getProtocolV() {
            return protocolV;
        }

        public String getMhsId() {
            return mhsId;
        }

        public String getTransmitScript() {
            return transmitScript;
        }

    }

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FetchActiveTableSrv.class);

    private final ConcurrentMap<String, ScheduledFuture<?>> siteJobInstanceMap;

    private ScheduledExecutorService jobExecutor;

    private volatile boolean activeServiceInstance;

    /**
     * Default constructor.
     */
    public FetchActiveTableSrv() {
        this.activeServiceInstance = false;
        this.siteJobInstanceMap = new ConcurrentHashMap<String, ScheduledFuture<?>>();
    }

    /**
     * Dummy trigger method for the timer in the camel context this bean
     * monitors. Ensures this bean properly fails over between cluster members
     * as needed.
     */
    public void activateService() {
        activeServiceInstance = true;
    }

    /**
     * Removes a site's active table sharing job from the job pool.
     * 
     * @param siteID
     *            Site identifier for the site's job to stop.
     */
    public void deactivateSite(final String siteID) {
        ScheduledFuture<?> siteJob = siteJobInstanceMap.remove(siteID);
        if (siteJob != null) {
            statusHandler.info("Deactivating FetchAT for " + siteID);
            siteJob.cancel(false);
        }
    }

    /**
     * Adds a site's active table sharing job to the job pool.
     * 
     * @param siteID
     *            Site identifier for the site's job to add to job pool.
     * @param gfeConfig
     *            {@code IFPServerConfig} for the site.
     */
    public void activateSite(final String siteID,
            final IFPServerConfig gfeConfig) {
        if (activeServiceInstance && (!siteJobInstanceMap.containsKey(siteID))) {
            FetchATJobConfig jobConfig = new FetchATJobConfig(siteID, gfeConfig);

            statusHandler.info("Activating FetchAT for " + siteID);
            statusHandler.debug("Site: " + siteID + " config: " + jobConfig);

            Runnable job = constructJob(jobConfig);

            try {
                ScheduledFuture<?> jobInstance = jobExecutor
                        .scheduleWithFixedDelay(job, 10,
                                jobConfig.getInterval(), TimeUnit.SECONDS);
                siteJobInstanceMap.put(siteID, jobInstance);
            } catch (RejectedExecutionException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to submit fetchAT job for execution:", e);
                siteJobInstanceMap.remove(siteID);
            }
        }
    }

    private Runnable constructJob(final FetchATJobConfig jobConfig) {
        Runnable job = new Runnable() {

            @Override
            public void run() {
                statusHandler.info("Starting requestAT process for site: "
                        + jobConfig.getSiteId());

                /*
                 * requestAT -H ourHost -P ourPort -L ourServerProto -M mhsid -S
                 * ourSite -t irtWebAddr -x transmitScript
                 */
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

                try {
                    /*
                     * We'll wait for requestAT to finish execution so that we
                     * can't accidentally overlap running instances if the user
                     * configures the run interval too low.
                     */
                    ProcessBuilder command = new ProcessBuilder(args);
                    RunProcess proc = RunProcess.getRunProcess();
                    proc.setProcess(command.start());
                    proc.waitFor();
                } catch (Throwable t) {
                    statusHandler.error(
                            "Unhandled exception thrown during requestAT: ", t);
                }
            }
        };
        return job;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.core.IContextStateProcessor#preStart()
     */
    @Override
    public void preStart() {
        statusHandler.info("Initializing FetchATSrv...");

        activeServiceInstance = true;
        jobExecutor = Executors.newScheduledThreadPool(1);

        for (IFPServer ifpServer : IFPServer.getActiveServers()) {
            IFPServerConfig config = ifpServer.getConfig();
            if ((config.requestISC()) && (config.tableFetchTime() > 0)) {
                activateSite(ifpServer.getSiteId(), config);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.core.IContextStateProcessor#postStart()
     */
    @Override
    public void postStart() {
        // no op
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.core.IContextStateProcessor#preStop()
     */
    @Override
    public void preStop() {
        statusHandler.info("Shutting down FetchATSrv...");

        activeServiceInstance = false;
        if (jobExecutor != null) {
            jobExecutor.shutdown();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.core.IContextStateProcessor#postStop()
     */
    @Override
    public void postStop() {
        jobExecutor = null;
        siteJobInstanceMap.clear();
    }
}
