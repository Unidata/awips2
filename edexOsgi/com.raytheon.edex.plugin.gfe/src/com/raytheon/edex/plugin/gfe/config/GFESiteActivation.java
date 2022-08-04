/**
7 * This software was developed and / or modified by Raytheon Company,
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
import java.net.UnknownHostException;
import java.time.Duration;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.commons.lang3.concurrent.BasicThreadFactory;
import org.apache.commons.lang3.exception.ExceptionUtils;

import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.exception.GfeMissingConfigurationException;
import com.raytheon.edex.plugin.gfe.isc.IscServiceProvider;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WxDefinition;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonEval;
import com.raytheon.uf.common.site.notify.SiteActivationNotification;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONSTATUS;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONTYPE;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexAsyncStartupBean;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.site.ISiteActivationListener;
import com.raytheon.uf.edex.site.SiteAwareRegistry;
import com.raytheon.uf.edex.site.notify.SendSiteActivationNotifications;

import jep.JepConfig;

/**
 * Activates the GFE server capabilities for a site
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 09, 2009           njensen   Initial creation
 * Oct 26, 2010  6811     jclark    changed listener type
 * Apr 06, 2012  457      dgilling  Clear site's ISCSendRecords on site
 *                                  deactivation.
 * Jul 12, 2012  15162    ryu       added check for invalid db at activation
 * Dec 11, 2012  14360    ryu       log a clean message in case of missing
 *                                  configuration (no stack trace).
 * Feb 15, 2013  1638     mschenke  Moved sending of site notification messages
 *                                  to edex plugin
 * Feb 28, 2013  1447     dgilling  Enable active table fetching on site
 *                                  activation.
 * Mar 20, 2013  1774     randerso  Changed to use GFED2DDao
 * May 02, 2013  1969     randerso  Moved updateDbs method into IFPGridDatabase
 * Jun 13, 2013  2044     randerso  Refactored to use IFPServer
 * Oct 16, 2013  2475     dgilling  Better error handling for IRT activation.
 * Mar 21, 2014  2726     rjpeter   Updated wait for running loop.
 * May 15, 2014  3157     dgilling  Mark getActiveSites() as deprecated.
 * Jul 09, 2014  3146     randerso  Eliminated redundant evaluation of
 *                                  serverConfig Sent activation failure message
 *                                  to alertViz
 * Oct 07, 2014  3684     randerso  Restructured IFPServer start up
 * Dec 10, 2014  4953     randerso  Added requestTCVFiles call at site
 *                                  activation
 * Feb 25, 2015  4128     dgilling  Simplify activation of active table sharing.
 * Mar 11, 2015  4128     dgilling  Refactor activation and management of ISC
 *                                  services.
 * Dec 22, 2015  4262     dgilling  Implement EdexAsyncStartupBean.
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 * May 04, 2020  8151     randerso  Added code to ensure all primary sites are
 *                                  started at startup. Code cleanup.
 * Dec 14, 2020  8306     randerso  Improve logging of errors in validateConfig
 *                                  Added jvm to extrainfo on cluster lock
 * Jan 29, 2021  8304     randerso  Move PythonEval for loading IPFServerConfig
 *                                  to separate thread to avoid multiple Python
 *                                  sub-interpreters on same thread.
 *
 * </pre>
 *
 * @author njensen
 */

public class GFESiteActivation
        implements ISiteActivationListener, EdexAsyncStartupBean {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFESiteActivation.class);

    private static final String SITE_CONFIG_PATH = LocalizationUtil.join("gfe",
            "config", "siteConfig.py");

    protected static final String TASK_NAME = "GFESiteActivation";

    private static final String INIT_TASK_DETAILS = "Initialization:";

    private static final long LOCK_TASK_TIMEOUT = Duration.ofMinutes(3)
            .toMillis();

    private static final BasicThreadFactory factory = new BasicThreadFactory.Builder()
            .namingPattern("initializeIFPConfig-%d").build();

    private static final ExecutorService executor = Executors
            .newCachedThreadPool(factory);

    private volatile boolean intialized;

    private final IscServiceProvider iscServices;

    /**
     * Default constructor. Builds a GFESiteActivation instance with no
     * associated {@code FetchActiveTableSrv} instance.
     */
    public GFESiteActivation() {
        this(null);
    }

    /**
     * Builds a GFESiteActivation instance with an associated
     * {@code IscServiceProvider} instance. Should only be used on request JVM.
     *
     * @param iscServices
     *            {@code IscServiceProvider} instance
     */
    public GFESiteActivation(final IscServiceProvider iscServices) {
        this.intialized = false;
        this.iscServices = iscServices;
    }

    @Override
    public void registered() {
        this.intialized = true;
        for (String siteID : SvcBackupUtil.getPrimarySites(false)) {
            if (!getActiveSites().contains(siteID)) {
                SiteAwareRegistry.getInstance().activateSite(siteID);
            }
        }
    }

    private void sendActivationBeginNotification(String siteID) {

        if (this.intialized) {
            try {
                SiteActivationNotification notification = new SiteActivationNotification(
                        SiteUtil.getSite(), siteID, "gfe",
                        ACTIVATIONTYPE.ACTIVATE, ACTIVATIONSTATUS.BEGIN);
                SendSiteActivationNotifications.send(notification);
            } catch (EdexException e) {
                statusHandler.error(
                        "Error sending site activation begin notification message!",
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
            } catch (EdexException e) {
                statusHandler.error(
                        "Error sending site activation complete notification message!",
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
            } catch (Exception e) {
                statusHandler.error(
                        "Error sending site activation failed notification message!",
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
                statusHandler.error(
                        "Error sending site deactivation begin notification message!",
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
                statusHandler.error(
                        "Error sending site deactivation complete notification message!",
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
            } catch (Exception e) {
                statusHandler.error(
                        "Error sending site deactivation failed notification message!",
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
            statusHandler.handle(Priority.EVENTB,
                    "Site " + siteID + " is already activated.");
            sendActivationCompleteNotification(siteID);
            return;
        }

        try {
            internalActivateSite(siteID);
            sendActivationCompleteNotification(siteID);
        } catch (GfeMissingConfigurationException e) {
            sendActivationFailedNotification(siteID);
            // Stack trace is not printed per requirement for DR14360
            statusHandler.warn(siteID + " will not be activated: "
                    + e.getLocalizedMessage());
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
    }

    /**
     * Initializes a site's serverConfig by reading in the site's localConfig
     *
     * @param siteID
     *            the site
     * @return the site's configuration
     * @throws GfeConfigurationException
     */
    private static IFPServerConfig initializeSite(String siteID)
            throws GfeConfigurationException {
        IFPServerConfig siteConfig = null;
        siteConfig = initializeConfig(siteID);

        WxDefinition wxDef = siteConfig.getWxDefinition();
        WeatherSubKey.setWxDefinition(siteID, wxDef);

        DiscreteDefinition dxDef = siteConfig.getDiscreteDefinition();
        DiscreteKey.setDiscreteDefinition(siteID, dxDef);

        return siteConfig;
    }

    /**
     * Initialize an IFPServerConfig object for a site
     *
     * @param siteID
     *            the desired site
     * @return the IFPServerConfig object
     * @throws GfeConfigurationException
     */
    private static IFPServerConfig initializeConfig(String siteID)
            throws GfeConfigurationException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext siteCtx = pathMgr.getContextForSite(
                LocalizationContext.LocalizationType.COMMON_STATIC, siteID);

        ILocalizationFile lf = pathMgr.getLocalizationFile(siteCtx,
                SITE_CONFIG_PATH);
        if (lf == null || !lf.exists()) {
            throw new GfeMissingConfigurationException(
                    "No siteConfig.py file found for " + siteID);
        }

        String gfeConfigPath = GfePyIncludeUtil.getGfeConfigIncludePath(siteID);
        String gfeCommonPath = GfePyIncludeUtil.getCommonGfeIncludePath();
        String vtecPath = GfePyIncludeUtil.getVtecIncludePath(siteID);

        JepConfig jepConfig = new JepConfig()
                .addIncludePaths(PyUtil.buildJepIncludePath(gfeConfigPath,
                        gfeCommonPath, vtecPath))
                .setClassLoader(IFPServerConfig.class.getClassLoader());

        Callable<IFPServerConfig> callable = new Callable<IFPServerConfig>() {
            @Override
            public IFPServerConfig call() throws Exception {
                try (PythonEval py = new PythonEval(jepConfig)) {
                    py.eval("from serverConfig import *");
                    SimpleServerConfig simpleConfig = (SimpleServerConfig) py
                            .execute("getSimpleConfig", null);
                    IFPServerConfig siteConfig = new IFPServerConfig(
                            simpleConfig);
                    return siteConfig;
                }
            }
        };

        try {
            Future<IFPServerConfig> future = executor.submit(callable);
            return future.get();
        } catch (Throwable e) {
            throw new GfeConfigurationException(
                    "Exception occurred while processing serverConfig for site "
                            + siteID,
                    e);
        }
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
        while (!(ct = ClusterLockUtils.lock(TASK_NAME,
                INIT_TASK_DETAILS + siteID,
                SystemUtil.getHostName() + ':'
                        + System.getProperty("edex.run.mode"),
                LOCK_TASK_TIMEOUT, false)).getLockState()
                        .equals(LockState.SUCCESSFUL)) {
            try {
                statusHandler.handle(Priority.EVENTA, "Activation task for "
                        + siteID
                        + " in progress by another EDEX instance.  Waiting...");
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
            config = initializeSite(siteID);
            statusHandler.info("Activating IFPServer...");
            IFPServer.activateServer(siteID, config);
        } finally {
            statusHandler.handle(Priority.INFO,
                    "Cluster locked site activation tasks for " + siteID
                            + " complete.  Releasing site activation lock.");
            ClusterLockUtils.unlock(ct, false);
        }

        if (iscServices != null) {
            iscServices.activateSite(siteID, config);
        }

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
            statusHandler.handle(Priority.DEBUG,
                    "Site [" + siteID + "] not active.  Cannot deactivate.");
            sendDeactivationCompleteNotification(siteID);
            return;
        }

        ClusterTask ct = null;
        try {

            while (!(ct = ClusterLockUtils.lock(TASK_NAME,
                    INIT_TASK_DETAILS + siteID, LOCK_TASK_TIMEOUT, true))
                            .getLockState().equals(LockState.SUCCESSFUL)) {
                statusHandler.handle(Priority.EVENTA, "Activation task for "
                        + siteID
                        + " in progress by another EDEX instance.  Waiting...");
                Thread.sleep(10000);

            }

            if (iscServices != null) {
                iscServices.deactivateSite(siteID);
            }

            IFPServer.deactivateServer(siteID);
            statusHandler.info(siteID + " successfully deactivated");

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
     *
     */
    @Override
    public Set<String> getActiveSites() {
        return IFPServer.getActiveSites();
    }

    @Override
    public String validateConfig(String site) {
        String retVal = site + " siteConfig and localConfig ";
        try {
            IFPServerConfig config = initializeConfig(site);
            if (config != null) {
                retVal += "validate ok!";
            } else {
                retVal += "failed validation.";
            }
        } catch (GfeConfigurationException e) {
            retVal += "failed validation.\n";
            retVal += e.getMessage() + "\n";
            if (e.getCause() != null) {
                String s = ExceptionUtils.getStackTrace(e.getCause());
                retVal += s.replaceFirst(
                        "jep\\.JepException: jep\\.JepException: ", "");
            }
        }
        return retVal;
    }

    @Override
    public boolean isDone() {
        return intialized;
    }
}
