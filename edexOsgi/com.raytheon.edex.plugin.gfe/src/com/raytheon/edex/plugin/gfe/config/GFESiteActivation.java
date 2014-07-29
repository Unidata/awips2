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
package com.raytheon.edex.plugin.gfe.config;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;

import com.google.common.util.concurrent.MoreExecutors;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.exception.GfeMissingConfigurationException;
import com.raytheon.edex.plugin.gfe.isc.IRTManager;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.site.notify.SiteActivationNotification;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONSTATUS;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONTYPE;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.site.ISiteActivationListener;
import com.raytheon.uf.edex.site.notify.SendSiteActivationNotifications;

/**
 * Activates the GFE server capabilities for a site
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 9, 2009            njensen      Initial creation
 * Oct 26, 2010  #6811    jclark       changed listener type
 * Apr 06, 2012  #457     dgilling     Clear site's ISCSendRecords on
 *                                     site deactivation.
 * Jul 12, 2012  15162    ryu          added check for invalid db at activation
 * Dec 11, 2012  14360    ryu          log a clean message in case of
 *                                     missing configuration (no stack trace).
 * Feb 15, 2013  1638      mschenke    Moved sending of site notification messages to edex plugin
 * Feb 28, 2013  #1447    dgilling     Enable active table fetching on site
 *                                     activation.
 * Mar 20, 2013  #1774    randerso     Changed to use GFED2DDao
 * May 02, 2013  #1969    randerso     Moved updateDbs method into IFPGridDatabase
 * Jun 13, 2013  #2044    randerso     Refactored to use IFPServer
 * Oct 16, 2013  #2475    dgilling     Better error handling for IRT activation.
 * Mar 21, 2014  #2726    rjpeter      Updated wait for running loop.
 * Jul 09, 2014  #3146    randerso     Eliminated redundant evaluation of serverConfig
 *                                     Sent activation failure message to alertViz
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GFESiteActivation implements ISiteActivationListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFESiteActivation.class);

    protected static final String TASK_NAME = "GFESiteActivation";

    private static final String INIT_TASK_DETAILS = "Initialization:";

    private static final int LOCK_TASK_TIMEOUT = 180000;

    private static GFESiteActivation instance = new GFESiteActivation();

    private boolean intialized = false;

    private final ExecutorService postActivationTaskExecutor = MoreExecutors
            .getExitingExecutorService((ThreadPoolExecutor) Executors
                    .newCachedThreadPool());

    /**
     * @return the singleton instance
     */
    public static GFESiteActivation getInstance() {
        return instance;
    }

    /**
     * private constructor for singleton class
     */
    private GFESiteActivation() {
    }

    @Override
    public void registered() {
        this.intialized = true;
    }

    private void sendActivationBeginNotification(String siteID) {

        if (this.intialized) {
            try {
                SiteActivationNotification notification = new SiteActivationNotification(
                        SiteUtil.getSite(), siteID, "gfe",
                        ACTIVATIONTYPE.ACTIVATE, ACTIVATIONSTATUS.BEGIN);
                SendSiteActivationNotifications.send(notification);
            } catch (EdexException e) {
                statusHandler
                        .error("Error sending site activation begin notification message!",
                                e);
            }

        }

    }

    private void sendActivationCompleteNotification(String siteID) {
        if (this.intialized) {
            try {
                SiteActivationNotification notification = new SiteActivationNotification(
                        SiteUtil.getSite(), siteID, "gfe",
                        ACTIVATIONTYPE.ACTIVATE, ACTIVATIONSTATUS.SUCCESS);
                SendSiteActivationNotifications.send(notification);
                IFPServerConfigManager.addActiveSite(siteID);
            } catch (EdexException e) {
                statusHandler
                        .error("Error sending site activation complete notification message!",
                                e);
            }
        }
    }

    private void sendActivationFailedNotification(String siteID) {
        if (this.intialized) {
            try {
                SiteActivationNotification notification = new SiteActivationNotification(
                        SiteUtil.getSite(), siteID, "gfe",
                        ACTIVATIONTYPE.ACTIVATE, ACTIVATIONSTATUS.FAILURE);
                SendSiteActivationNotifications.send(notification);
                IFPServerConfigManager.removeActiveSite(siteID);
            } catch (Exception e) {
                statusHandler
                        .error("Error sending site activation failed notification message!",
                                e);
            }
        }
    }

    private void sendDeactivationBeginNotification(String siteID) {
        if (this.intialized) {
            try {
                SiteActivationNotification notification = new SiteActivationNotification(
                        SiteUtil.getSite(), siteID, "gfe",
                        ACTIVATIONTYPE.DEACTIVATE, ACTIVATIONSTATUS.BEGIN);
                SendSiteActivationNotifications.send(notification);
            } catch (Exception e) {
                statusHandler
                        .error("Error sending site deactivation begin notification message!",
                                e);
            }

        }
    }

    private void sendDeactivationCompleteNotification(String siteID) {
        if (this.intialized) {
            try {
                SiteActivationNotification notification = new SiteActivationNotification(
                        SiteUtil.getSite(), siteID, "gfe",
                        ACTIVATIONTYPE.DEACTIVATE, ACTIVATIONSTATUS.SUCCESS);
                SendSiteActivationNotifications.send(notification);
            } catch (Exception e) {
                statusHandler
                        .error("Error sending site deactivation complete notification message!",
                                e);
            }
        }
    }

    private void sendDeactivationFailedNotification(String siteID) {
        if (this.intialized) {
            try {
                SiteActivationNotification notification = new SiteActivationNotification(
                        SiteUtil.getSite(), siteID, "gfe",
                        ACTIVATIONTYPE.DEACTIVATE, ACTIVATIONSTATUS.FAILURE);
                SendSiteActivationNotifications.send(notification);
                IFPServerConfigManager.addActiveSite(siteID);
            } catch (Exception e) {
                statusHandler
                        .error("Error sending site deactivation failed notification message!",
                                e);
            }
        }
    }

    /**
     * Activates a site by reading its server config and generating maps, topo,
     * and text products for the site
     * 
     * @param siteID
     */
    @Override
    public void activateSite(String siteID) throws Exception {

        sendActivationBeginNotification(siteID);
        if (getActiveSites().contains(siteID)) {
            statusHandler.handle(Priority.EVENTB, "Site " + siteID
                    + " is already activated.");
            sendActivationCompleteNotification(siteID);
            return;
        }

        try {
            internalActivateSite(siteID);
        } catch (GfeMissingConfigurationException e) {
            sendActivationFailedNotification(siteID);
            // Stack trace is not printed per requirement for DR14360
            statusHandler.warn(siteID + " will not be activated: "
                    + e.getLocalizedMessage());
            throw e;
        } catch (Exception e) {
            sendActivationFailedNotification(siteID);
            String message = "Error activating IFPServer for site " + siteID
                    + ".  GFE will be unavailable for this site!";
            statusHandler.error(message, e);

            StringWriter stackTrace = new StringWriter();
            e.printStackTrace(new PrintWriter(stackTrace));
            EDEXUtil.sendMessageAlertViz(Priority.ERROR,
                    "com.raytheon.edex.plugin.gfe", "GFE", "GFE", message,
                    stackTrace.toString(), null);
            throw e;
        }
        sendActivationCompleteNotification(siteID);
    }

    /**
     * Activate site routine for internal use.
     * 
     * Doesn't update the site list so it is preserved when loading sites at
     * start up
     * 
     * @param siteID
     * @throws PluginException
     * @throws GfeException
     * @throws UnknownHostException
     * @throws DataAccessLayerException
     * @throws EdexException
     */
    private void internalActivateSite(final String siteID)
            throws PluginException, GfeException, UnknownHostException,
            DataAccessLayerException {
        ClusterTask ct = null;
        while (!(ct = ClusterLockUtils.lock(TASK_NAME, INIT_TASK_DETAILS
                + siteID, LOCK_TASK_TIMEOUT, false)).getLockState().equals(
                LockState.SUCCESSFUL)) {
            try {
                statusHandler
                        .handle(Priority.EVENTA,
                                "Activation task in progress by another EDEX instance.  Waiting...");
                Thread.sleep(10000);
            } catch (InterruptedException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error waiting for cluster lock", e);
            }
        }

        IFPServerConfig config = null;

        try {
            statusHandler.info("Activating " + siteID + "...");

            statusHandler.info("IFPServerConfigManager initializing...");
            config = IFPServerConfigManager.initializeSite(siteID);
            statusHandler.info("Activating IFPServer...");
            IFPServer.activateServer(siteID, config);
        } finally {
            statusHandler
                    .handle(Priority.INFO,
                            "Cluster locked site activation tasks complete.  Releasing Site Activation lock.");
            ClusterLockUtils.unlock(ct, false);
        }

        // Doesn't need to be cluster locked
        statusHandler.info("Checking ISC configuration...");
        boolean isIscActivated = false;
        if (config.requestISC()) {
            String host = InetAddress.getLocalHost().getCanonicalHostName();
            String gfeHost = config.getServerHost();
            String hostNameToCompare = gfeHost;
            if (gfeHost.endsWith("f")) {
                hostNameToCompare = gfeHost.substring(0, gfeHost.length() - 1);
            }
            // TODO: specific to a host and jvm type, register it independently,
            // but don't hard code request
            if (host.contains(hostNameToCompare)
                    && System.getProperty("edex.run.mode").equals("request")) {
                statusHandler.info("Enabling ISC...");
                try {
                    IRTManager.getInstance().enableISC(siteID, config);
                    isIscActivated = true;
                } catch (Exception e) {
                    statusHandler
                            .error("Error starting GFE ISC. ISC functionality will be unavailable!!",
                                    e);
                }
            } else {
                statusHandler
                        .info("ISC Enabled but will use another EDEX instance");
            }

        } else {
            statusHandler.info("ISC is not enabled.");
        }

        // doesn't need to be cluster locked
        final IFPServerConfig configRef = config;

        if ((config.tableFetchTime() > 0) && isIscActivated) {
            Runnable activateFetchAT = new Runnable() {

                @Override
                public void run() {
                    EDEXUtil.waitForRunning();

                    Map<String, Object> fetchATConfig = new HashMap<String, Object>();
                    fetchATConfig.put("siteId", configRef.getSiteID().get(0));
                    fetchATConfig.put("interval", configRef.tableFetchTime());
                    fetchATConfig.put("ancf", configRef
                            .iscRoutingTableAddress().get("ANCF"));
                    fetchATConfig.put("bncf", configRef
                            .iscRoutingTableAddress().get("BNCF"));
                    fetchATConfig.put("serverHost", configRef.getServerHost());
                    fetchATConfig.put("port", configRef.getRpcPort());
                    fetchATConfig.put("protocolV",
                            configRef.getProtocolVersion());
                    fetchATConfig.put("mhsid", configRef.getMhsid());
                    fetchATConfig.put("transmitScript",
                            configRef.transmitScript());

                    try {
                        EDEXUtil.getMessageProducer().sendAsyncUri(
                                "jms-generic:queue:gfeSiteActivated",
                                fetchATConfig);
                    } catch (EdexException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Could not activate active table sharing for site: "
                                        + siteID, e);
                    }
                }
            };
            postActivationTaskExecutor.submit(activateFetchAT);
        }

        statusHandler.info("Adding " + siteID + " to active sites list.");
        IFPServerConfigManager.addActiveSite(siteID);
        statusHandler.info(siteID + " successfully activated");
    }

    /**
     * Deactivates a site's GFE services
     * 
     * @param siteID
     */
    @Override
    public void deactivateSite(String siteID) throws Exception {

        sendDeactivationBeginNotification(siteID);
        if (!IFPServer.getActiveSites().contains(siteID)) {
            statusHandler.handle(Priority.DEBUG, "Site [" + siteID
                    + "] not active.  Cannot deactivate.");
            sendDeactivationCompleteNotification(siteID);
            return;
        }

        ClusterTask ct = null;
        try {

            while (!(ct = ClusterLockUtils.lock(TASK_NAME, INIT_TASK_DETAILS
                    + siteID, LOCK_TASK_TIMEOUT, true)).getLockState().equals(
                    LockState.SUCCESSFUL)) {
                statusHandler
                        .handle(Priority.EVENTA,
                                "Activation task in progress by another EDEX instance.  Waiting...");
                Thread.sleep(10000);

            }

            IFPServer.deactivateServer(siteID);
            statusHandler.info(siteID + " successfully deactivated");

            // TODO eventually this should go away
            IFPServerConfigManager.removeSite(siteID);
            IFPServerConfigManager.removeActiveSite(siteID);

        } catch (GfeConfigurationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to get server config for site [" + siteID + "]", e);
            sendDeactivationFailedNotification(siteID);
            throw e;
        } finally {
            if (ct != null) {
                ClusterLockUtils.unlock(ct, false);
            }
        }

        sendDeactivationCompleteNotification(siteID);
    }

    /**
     * Returns the currently active GFE sites the server is running
     * 
     * @return the active sites
     */
    @Override
    public Set<String> getActiveSites() {
        return IFPServerConfigManager.getActiveSites();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.site.ISiteActivationListener#validateConfig()
     */
    @Override
    public String validateConfig(String site) {
        String retVal = site + " siteConfig and localConfig ";
        try {
            IFPServerConfig config = IFPServerConfigManager
                    .initializeConfig(site);
            if (config != null) {
                retVal += "validate ok!";
            } else {
                retVal += "failed validation.";
            }
        } catch (GfeConfigurationException e) {
            retVal += "failed validation.\n";
            retVal += e.getMessage() + "\n";
            if (e.getCause() != null) {
                retVal += e
                        .getCause()
                        .toString()
                        .replaceFirst(
                                "jep\\.JepException: jep\\.JepException: ", "");
            }
        }
        return retVal;
    }

}
