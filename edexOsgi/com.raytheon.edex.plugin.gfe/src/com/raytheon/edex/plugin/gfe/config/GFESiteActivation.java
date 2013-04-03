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

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.edex.plugin.gfe.cache.d2dparms.D2DParmIdCache;
import com.raytheon.edex.plugin.gfe.cache.gridlocations.GridLocationCache;
import com.raytheon.edex.plugin.gfe.cache.ifpparms.IFPParmIdCache;
import com.raytheon.edex.plugin.gfe.db.dao.GFEDao;
import com.raytheon.edex.plugin.gfe.db.dao.IscSendRecordDao;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.exception.GfeMissingConfigurationException;
import com.raytheon.edex.plugin.gfe.isc.IRTManager;
import com.raytheon.edex.plugin.gfe.reference.MapManager;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.database.D2DSatDatabaseManager;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.edex.plugin.gfe.server.database.NetCDFDatabaseManager;
import com.raytheon.edex.plugin.gfe.server.database.TopoDatabaseManager;
import com.raytheon.edex.plugin.gfe.smartinit.SmartInitRecord;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.site.notify.SendSiteActivationNotifications;
import com.raytheon.uf.common.site.notify.SiteActivationNotification;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONSTATUS;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONTYPE;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.IMessageProducer;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.site.ISiteActivationListener;

/**
 * Activates the GFE server capabilities for a site
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 9, 2009            njensen     Initial creation
 * Oct 26, 2010  #6811    jclark      changed listener type
 * Apr 06, 2012  #457     dgilling    Clear site's ISCSendRecords on
 *                                    site deactivation.
 * Jul 12, 2012  15162    ryu         added check for invalid db at activation
 * Dec 11, 2012  14360    ryu         log a clean message in case of
 *                                    missing configuration (no stack trace).
 *
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

    private static final String SMART_INIT_TASK_DETAILS = "SmartInit:";

    private static final int LOCK_TASK_TIMEOUT = 180000;

    // don't rerun the smart init fire if they have been run in the last 30
    // minutes
    private static final int SMART_INIT_TIMEOUT = 1800000;

    private static GFESiteActivation instance;

    private boolean intialized = false;

    public static synchronized GFESiteActivation getInstance() {
        if (instance == null) {
            instance = new GFESiteActivation();
        }
        return instance;
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

            IFPServerConfig config = IFPServerConfigManager
                    .initializeConfig(siteID);
            if (config == null) {
                throw new GfeConfigurationException(
                        "Error validating configuration for " + siteID);
            }
            internalActivateSite(siteID);
        } catch (GfeMissingConfigurationException e) {
            sendActivationFailedNotification(siteID);
            // Stack trace is not printed per requirement for DR14360
            statusHandler.handle(Priority.PROBLEM, siteID
                    + " will not be activated: "
                    + e.getLocalizedMessage());
            throw e;
        } catch (Exception e) {
            sendActivationFailedNotification(siteID);
            statusHandler.handle(Priority.PROBLEM, siteID
                    + " Error activating site " + siteID, e);
            throw e;
        }
        sendActivationCompleteNotification(siteID);
    }

    public void cycleSite(String siteID) throws Exception {
        this.deactivateSite(siteID);
        this.activateSite(siteID);
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
            statusHandler.handle(Priority.EVENTA, "Activating " + siteID
                    + "...");

            statusHandler.handle(Priority.EVENTA,
                    "IFPServerConfigManager initializing...");
            config = IFPServerConfigManager.initializeSite(siteID);
            statusHandler.handle(Priority.EVENTA,
                    "TopoDatabaseManager initializing...");
            TopoDatabaseManager.initializeTopoDatabase(siteID);
            // statusHandler.handle(Priority.EVENTA,
            // "ClimoDatabaseManager initializing...");
            // ClimoDatabaseManager.initializeClimoDatabase(siteID);
            // statusHandler.handle(Priority.EVENTA,
            // "HLSDatabaseManager initializing...");
            // HLSTopoDatabaseManager.initializeHLSTopoDatabase(siteID);
            // statusHandler.handle(Priority.EVENTA,
            // "D2DSatDatabaseManager initializing...");
            D2DSatDatabaseManager.initializeD2DSatDatabase(siteID, config);

            statusHandler.handle(Priority.EVENTA,
                    "NetCDFDatabaseManager initializing...");
            NetCDFDatabaseManager.initializeNetCDFDatabases(config);

            statusHandler.handle(Priority.EVENTA, "MapManager initializing...");
            // should be cluster locked
            new MapManager(config);

            statusHandler
                    .handle(Priority.EVENTA, "Getting GFE db inventory...");
            List<DatabaseID> inventory = GridParmManager.getDbInventory(siteID)
                    .getPayload();
            Map<String, List<DatabaseID>> ifpInventory = new HashMap<String, List<DatabaseID>>();
            for (DatabaseID dbId : inventory) {
                if (!dbId.getDbType().equals("D2D")) {
                    if (!ifpInventory.keySet().contains(dbId.getSiteId())) {
                        ifpInventory.put(dbId.getSiteId(),
                                new ArrayList<DatabaseID>());
                    }
                    ifpInventory.get(dbId.getSiteId()).add(dbId);
                }
            }
            statusHandler.handle(Priority.EVENTA,
                    "Checking for IFPGridDatabase updates...");
            for (String site : ifpInventory.keySet()) {
                for (int i = 0; i < ifpInventory.get(site).size(); i++) {
                    GridDatabase db = GridParmManager.getDb(ifpInventory.get(
                            site).get(i));
                    // cluster locked since IFPGridDatabase can modify the grids
                    // based on changes to grid size, etc
                    if (db.databaseIsValid())
                        db.updateDbs();
                }
            }
        } finally {
            statusHandler
                    .handle(Priority.INFO,
                            "Cluster locked site activation tasks complete.  Releasing Site Activation lock.");
            ClusterLockUtils.unlock(ct, false);
        }

        // Doesn't need to be cluster locked
        statusHandler.handle(Priority.EVENTA, "Checking ISC configuration...");
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
                statusHandler.handle(Priority.EVENTA, "Enabling ISC...");
                IRTManager.getInstance().enableISC(siteID, config.getMhsid());
            } else {
                statusHandler.handle(Priority.EVENTA,
                        "ISC Enabled but will use another EDEX instance");
            }

        } else {
            statusHandler.handle(Priority.EVENTA, "ISC is not enabled.");
        }

        // doesn't need to be cluster locked
        statusHandler.handle(Priority.EVENTA, "Building the D2DParmIDCache...");
        D2DParmIdCache.getInstance().buildCache(siteID);
        final IFPServerConfig configRef = config;

        // TODO: should only be done once at
        // initial start of the configuration, or at least only once per
        // startup, use a separate cluster lock that won't run if lock
        // within last 5 minutes, move outside of site activation as this
        // just need to be done, doesn't matter that site isn't fully
        // activated, in fact would be best to only be done once site is
        // fully activated.
        Thread smartInit = new Thread("SmartInitLauncher") {
            @Override
            public void run() {
                long startTime = System.currentTimeMillis();
                // wait for system startup or at least 3 minutes
                while (!EDEXUtil.isRunning()
                        || System.currentTimeMillis() > startTime + 180000) {
                    try {
                        Thread.sleep(15000);
                    } catch (InterruptedException e) {

                    }
                }

                ClusterTask ct = ClusterLockUtils.lookupLock(TASK_NAME,
                        SMART_INIT_TASK_DETAILS + siteID);
                if (ct.getLastExecution() + SMART_INIT_TIMEOUT < System
                        .currentTimeMillis()) {
                    ct = ClusterLockUtils.lock(TASK_NAME,
                            SMART_INIT_TASK_DETAILS + siteID,
                            SMART_INIT_TIMEOUT, false);
                    if (LockState.SUCCESSFUL.equals(ct.getLockState())) {
                        boolean clearTime = false;
                        try {
                            GFEDao dao = new GFEDao();
                            List<String> d2dModels = configRef.getD2dModels();
                            List<List<String>> idsByVersion = new ArrayList<List<String>>(
                                    5);
                            for (String d2dModelName : d2dModels) {

                                String gfeModel = configRef
                                        .gfeModelNameMapping(d2dModelName);

                                if ((d2dModelName != null)
                                        && (gfeModel != null)) {
                                    int versions = configRef
                                            .desiredDbVersions(new DatabaseID(
                                                    siteID, DataType.GRID, "",
                                                    gfeModel));
                                    List<DatabaseID> dbIds = dao
                                            .getD2DDatabaseIdsFromDb(
                                                    d2dModelName, gfeModel,
                                                    siteID, versions);

                                    while (versions > idsByVersion.size()) {
                                        idsByVersion.add(new ArrayList<String>(
                                                d2dModels.size()));
                                    }

                                    int index = 0;
                                    for (DatabaseID id : dbIds) {
                                        List<String> ids = idsByVersion
                                                .get(index++);
                                        ids.add(id.toString());
                                    }
                                }
                            }
                            IMessageProducer producer = EDEXUtil
                                    .getMessageProducer();
                            for (List<String> ids : idsByVersion) {
                                for (String id : ids) {
                                    statusHandler.handle(Priority.EVENTA,
                                            "Firing smartinit for " + id);
                                    try {
                                        producer.sendAsyncUri(
                                                "jms-generic:queue:manualSmartInit",
                                                id
                                                        + ":0::"
                                                        + SmartInitRecord.SITE_ACTIVATION_INIT_PRIORITY);
                                    } catch (EdexException e) {
                                        statusHandler.handle(Priority.PROBLEM,
                                                "Failed to fire smart init for: "
                                                        + id);
                                    }
                                }
                            }
                        } catch (Exception e) {
                            statusHandler.handle(Priority.ERROR,
                                    "Error occurred firing Smart Inits", e);
                            clearTime = true;
                        } finally {
                            ClusterLockUtils.unlock(ct, clearTime);
                        }
                    }
                }
            }
        };
        smartInit.start();
        statusHandler.handle(Priority.EVENTA, "Adding " + siteID
                + " to active sites list.");
        IFPServerConfigManager.addActiveSite(siteID);
        statusHandler.handle(Priority.EVENTA, siteID
                + " successfully activated");
    }

    /**
     * Deactivates a site's GFE services
     *
     * @param siteID
     */
    @Override
    public void deactivateSite(String siteID) throws Exception {

        sendDeactivationBeginNotification(siteID);
        if (!IFPServerConfigManager.getActiveSites().contains(siteID)) {
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

            IFPServerConfig config = IFPServerConfigManager
                    .getServerConfig(siteID);
            if (config.requestISC()) {
                IRTManager.getInstance().disableISC(config.getMhsid(), siteID);
            }

            try {
                new IscSendRecordDao().deleteForSite(siteID);
            } catch (DataAccessLayerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not clear IscSendRecords for site " + siteID
                                + " from queue.", e);
            }

            TopoDatabaseManager.removeTopoDatabase(siteID);
            // for (String source : ClimoDatabaseManager.getClimoSources()) {
            // ClimoDatabaseManager.removeClimoDatabase(siteID, source);
            // }

            NetCDFDatabaseManager.removeDatabases(siteID);

            D2DSatDatabaseManager.removeSatDatabase(siteID);
            D2DParmIdCache.getInstance().removeSiteDbs(siteID);
            IFPParmIdCache.getInstance().removeSiteDbs(siteID);
            GridParmManager.purgeDbCache(siteID);
            GridLocationCache.removeGridLocationsForSite(siteID);
            statusHandler.handle(Priority.EVENTA, siteID
                    + " successfully deactivated");

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