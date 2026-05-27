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
package com.raytheon.edex.plugin.gfe.server;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import com.raytheon.edex.plugin.gfe.cache.gridlocations.GridLocationCache;
import com.raytheon.edex.plugin.gfe.config.GridDbConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.db.dao.IscSendRecordDao;
import com.raytheon.edex.plugin.gfe.reference.MapManager;
import com.raytheon.edex.plugin.gfe.server.database.TopoDatabaseManager;
import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceMgr;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * GFE Server Container
 *
 * Contains all server objects for a GFE site.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 30, 2013  2044     randerso  Initial creation
 * Nov 20, 2013  2331     randerso  Added getTopoData method
 * Oct 07, 2014  3684     randerso  Restructured IFPServer start up
 * Mar 11, 2015  4128     dgilling  Remove unregister from IRT to IRTManager.
 * Nov 11, 2015  5110     dgilling  Add initIRT.
 * Sep 12, 2016  5861     randerso  Remove synchronization from activateServer
 *                                  and deactivateServer methods to allow
 *                                  multithreaded activation of multiple sites.
 * Jul 31, 2017  6342     randerso  Added getReferenceMgr()
 * Oct 19  2017  #6126    dgilling  Remove references to NetCDFDatabaseManager.
 *
 * </pre>
 *
 * @author randerso
 */

public class IFPServer {
    /**
     * Wrapper class to call static methods from spring/camel
     */
    public static class Wrapper {
        /**
         * @param pluginName
         */
        public void pluginPurged(String pluginName) {
            IFPServer.pluginPurged(pluginName);
        }

        /**
         * @param msg
         */
        public void processNotification(Object msg) {
            IFPServer.processNotification(msg);
        }

        /**
         * @param message
         * @throws Exception
         */
        public void filterDataURINotifications(
                DataURINotificationMessage message) throws Exception {
            IFPServer.filterDataURINotifications(message);
        }
    }

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IFPServer.class);

    private static Map<String, IFPServer> activeServers = new HashMap<>();

    /**
     * Activate an IFPServer for a site
     *
     * @param siteID
     *            site to be activated
     * @param config
     * @return the IFPServer instance
     * @throws GfeException
     *             if site already active
     */
    public static IFPServer activateServer(String siteID,
            IFPServerConfig config) throws GfeException {
        IFPServer instance;
        synchronized (activeServers) {
            instance = activeServers.get(siteID);
            if (instance == null) {
                instance = new IFPServer(siteID, config);
                activeServers.put(siteID, instance);

            } else {
                throw new GfeException(
                        "IFPServer already active for site: " + siteID);
            }
        }

        try {
            instance.init();
            return instance;
        } catch (Throwable e) {
            // exception occurred during initialization
            // remove the IFPServer from activeServers
            synchronized (activeServers) {
                activeServers.remove(siteID);
            }

            // try to clean up the partially initialized IFPServer
            try {
                instance.dispose();
            } catch (Throwable t) {
                statusHandler.error(
                        "Error disposing of partially initialized IFPServer",
                        t);
            }
            throw new GfeException("Error creating IFPServer for " + siteID, e);
        }
    }

    /**
     * Deactivate the IFPServer for a site
     *
     * @param siteID
     *            site to be activated
     * @throws GfeException
     *             if no IFPServer is active for the site
     */
    public static void deactivateServer(String siteID) throws GfeException {
        IFPServer ifp;
        synchronized (activeServers) {
            ifp = activeServers.remove(siteID);
        }

        if (ifp != null) {
            ifp.dispose();
        } else {
            throw new GfeException("No active IFPServer for site: " + siteID);
        }
    }

    /**
     * Get list of sites with active IFPServers
     *
     * @return list of active sites
     */
    public static Set<String> getActiveSites() {

        Set<String> rval = new HashSet<>();

        synchronized (activeServers) {
            for (Entry<String, IFPServer> entry : activeServers.entrySet()) {
                if (entry.getValue().initialized) {
                    rval.add(entry.getKey());
                }
            }
        }

        return rval;
    }

    /**
     * Returns a list of active IFPServers
     *
     * @return the list of servers
     */
    public static List<IFPServer> getActiveServers() {

        List<IFPServer> rval = new ArrayList<>();

        synchronized (activeServers) {
            for (IFPServer server : activeServers.values()) {
                if (server.initialized) {
                    rval.add(server);
                }
            }
        }

        return rval;
    }

    /**
     * Get the active IFPServer instance for a site
     *
     * @param siteID
     * @return the IFPServer instance or null if there is no active server for
     *         siteID
     */
    public static IFPServer getActiveServer(String siteID) {

        IFPServer server;
        synchronized (activeServers) {
            server = activeServers.get(siteID);
        }

        if ((server != null) && server.initialized) {
            return server;
        } else {
            return null;
        }
    }

    private String siteId;

    private IFPServerConfig config;

    private boolean initialized;

    private GridParmManager gridParmMgr;

    private ReferenceMgr referenceMgr;

    private IFPServer(String siteId, IFPServerConfig config) {
        this.siteId = siteId;
        this.config = config;
        this.initialized = false;
    }

    private void init()
            throws DataAccessLayerException, PluginException, GfeException {
        statusHandler.info("MapManager initializing...");
        this.referenceMgr = new ReferenceMgr(config.dbDomain());
        new MapManager(this);

        this.gridParmMgr = new GridParmManager(siteId, config);

        initIRT();

        this.initialized = true;
    }

    private void dispose() {
        try {
            new IscSendRecordDao().deleteForSite(siteId);
        } catch (DataAccessLayerException e) {
            statusHandler.error("Could not clear IscSendRecords for site "
                    + siteId + " from queue.", e);
        }

        GridLocationCache.removeGridLocationsForSite(siteId);

        if (this.gridParmMgr != null) {
            this.gridParmMgr.dispose();
        }
    }

    /**
     * @return the siteId
     */
    public String getSiteId() {
        return siteId;
    }

    /**
     * @return the config
     */
    public IFPServerConfig getConfig() {
        return config;
    }

    /**
     * @return the referenceMgr
     */
    public ReferenceMgr getReferenceMgr() {
        return referenceMgr;
    }

    /**
     * @return the gridParmMgr
     */
    public GridParmManager getGridParmMgr() {
        return gridParmMgr;
    }

    /**
     * @return the lockMgr
     */
    public LockManager getLockMgr() {
        return this.gridParmMgr.getLockMgr();
    }

    /**
     * @return the topoMgr
     */
    public TopoDatabaseManager getTopoMgr() {
        return this.gridParmMgr.getTopoMgr();
    }

    /**
     * @param msg
     */
    public static void processNotification(Object msg) {
        if (msg instanceof List) {
            for (Object obj : (List<?>) msg) {
                if (obj instanceof GfeNotification) {
                    handleGfeNotification((GfeNotification) obj);
                }
            }
        } else if (msg instanceof GfeNotification) {
            handleGfeNotification((GfeNotification) msg);
        }
    }

    private static void handleGfeNotification(GfeNotification notif) {
        IFPServer ifp = getActiveServer(notif.getSiteID());
        if (ifp != null) {
            ifp.gridParmMgr.handleGfeNotification(notif);
        } else {
            statusHandler.warn("Received " + notif.getClass().getSimpleName()
                    + " for " + notif.getSiteID()
                    + " with no active GridParmManager");
        }
    }

    /**
     * @param pluginName
     */
    public static void pluginPurged(String pluginName) {
        for (IFPServer ifpServer : getActiveServers()) {
            if ("grid".equals(pluginName)) {
                statusHandler.info(
                        "Processing " + pluginName + " purge notification");
                ifpServer.getGridParmMgr().d2dGridDataPurged();
            } else if ("satellite".equals(pluginName)) {
                statusHandler.info(
                        "Processing " + pluginName + " purge notification");
                ifpServer.getGridParmMgr().d2dSatDataPurged();
            }
        }
    }

    /**
     * @param message
     * @throws Exception
     */
    public static void filterDataURINotifications(
            DataURINotificationMessage message) throws Exception {
        List<GridRecord> gridRecords = new LinkedList<>();
        List<SatelliteRecord> satRecords = new LinkedList<>();

        for (String dataURI : message.getDataURIs()) {
            if (dataURI.startsWith("/grid/")) {
                gridRecords.add(new GridRecord(dataURI));
            } else if (dataURI.startsWith("/satellite/")) {
                satRecords.add(new SatelliteRecord(dataURI));
            }
        }

        for (IFPServer ifpServer : getActiveServers()) {
            if (!gridRecords.isEmpty()) {
                ifpServer.getGridParmMgr().filterGridRecords(gridRecords);
            }
            if (!satRecords.isEmpty()) {
                ifpServer.getGridParmMgr().filterSatelliteRecords(satRecords);
            }
        }
    }

    /**
     * Get topo data for a GridLocation
     *
     * @param gloc
     *            GridLocation for topo data
     * @return topo gridslice
     */
    public ServerResponse<ScalarGridSlice> getTopoData(GridLocation gloc) {
        return getTopoMgr().getTopoData(gloc);
    }

    private void initIRT() {
        /*
         * no web address or request not enabled, so no need to verify ISC
         * configuration
         */
        String irtAddress = config.iscRoutingTableAddress().get("ANCF");
        if ((!config.iscRoutingTableAddress().containsKey("ANCF"))
                || (!config.iscRoutingTableAddress().containsKey("BNCF"))
                || (StringUtil.isEmptyString(irtAddress))
                || (!config.requestISC())) {
            StringBuilder oz = new StringBuilder(
                    "IRT (ISC Routing Table) Registration Information");
            Map<String, String> addresses = config.iscRoutingTableAddress();
            SortedSet<String> sorted = new TreeSet<>(addresses.keySet());
            for (String key : sorted) {
                String value = addresses.get(key);
                oz.append("\nIRTAddress:      ").append(key).append(' ')
                        .append(value);
            }
            oz.append("\nRequestISC flag: ").append(config.requestISC());

            statusHandler.info(oz.toString());
        }

        // determine which parms are wanted
        List<String> parmsWanted = config.requestedISCparms();
        if (parmsWanted.isEmpty()) {
            // get known list of databases
            List<DatabaseID> dbs = getGridParmMgr().getDbInventory()
                    .getPayload();

            // find the ISC database
            for (DatabaseID dbId : dbs) {
                if (("ISC".equals(dbId.getModelName()))
                        && (dbId.getDbType().isEmpty())
                        && (dbId.getSiteId().equals(siteId))) {
                    GridDbConfig gdc = config.gridDbConfig(dbId);
                    parmsWanted = gdc.parmAndLevelList();
                    break;
                }
            }

            statusHandler.info("ParmsWanted: " + parmsWanted);
            config.setRequestedISCparms(parmsWanted);
        }

        // determine isc areas that are wanted
        List<String> iscWfosWanted = config.requestedISCsites();
        if (iscWfosWanted.isEmpty()) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext commonStaticConfig = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            commonStaticConfig.setContextName(siteId);

            DirectoryStream.Filter<Path> filter = new DirectoryStream.Filter<Path>() {

                @Override
                public boolean accept(Path entry) throws IOException {
                    Path fileName = entry.getFileName();
                    if (fileName != null) {
                        return fileName.toString()
                                .matches("ISC_\\p{Alnum}{3}\\.xml");
                    }
                    return false;
                }
            };

            Path editAreaDir = pathMgr
                    .getFile(commonStaticConfig,
                            "gfe" + IPathManager.SEPARATOR + "editAreas")
                    .toPath();
            iscWfosWanted = new ArrayList<>();
            try (DirectoryStream<Path> stream = Files
                    .newDirectoryStream(editAreaDir, filter)) {
                List<String> knownSites = config.allSites();

                for (Path editArea : stream) {
                    String name = editArea.getFileName().toString()
                            .replace("ISC_", "").replace(".xml", "");
                    if (knownSites.contains(name)) {
                        iscWfosWanted.add(name);
                    }
                }
            } catch (IOException e) {
                statusHandler
                        .error("Unable to retrieve list of ISC edit areas.", e);
            }
            config.setRequestedISCsites(iscWfosWanted);
        }
    }
}
