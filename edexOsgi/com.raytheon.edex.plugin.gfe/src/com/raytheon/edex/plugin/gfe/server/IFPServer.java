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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.edex.plugin.gfe.cache.gridlocations.GridLocationCache;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.db.dao.IscSendRecordDao;
import com.raytheon.edex.plugin.gfe.isc.IRTManager;
import com.raytheon.edex.plugin.gfe.reference.MapManager;
import com.raytheon.edex.plugin.gfe.server.database.NetCDFDatabaseManager;
import com.raytheon.edex.plugin.gfe.server.database.TopoDatabaseManager;
import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 30, 2013       2044 randerso    Initial creation
 * Nov 20, 2013      #2331 randerso    Added getTopoData method
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class IFPServer {
    /**
     * Wrapper class to call static methods from spring/camel
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Jun 14, 2013            randerso     Initial creation
     * 
     * </pre>
     * 
     * @author randerso
     * @version 1.0
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

    private static Map<String, IFPServer> activeServers = new HashMap<String, IFPServer>();

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
    public static synchronized IFPServer activateServer(String siteID,
            IFPServerConfig config) throws GfeException {
        IFPServer instance = activeServers.get(siteID);
        if (instance == null) {
            try {
                instance = new IFPServer(siteID, config);
                activeServers.put(siteID, instance);
            } catch (Exception e) {
                throw new GfeException(
                        "Error creating IFPServer for " + siteID, e);
            }
        } else {
            throw new GfeException("IFPServer already active for site: "
                    + siteID);
        }
        return instance;
    }

    /**
     * Deactivate the IFPServer for a site
     * 
     * @param siteID
     *            site to be activated
     * @throws GfeException
     *             if no IFPServer is active for the site
     */
    public static synchronized void deactivateServer(String siteID)
            throws GfeException {
        IFPServer ifp = activeServers.remove(siteID);
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
        return new HashSet<String>(activeServers.keySet());
    }

    /**
     * Returns a list of active IFPServers
     * 
     * @return the list of servers
     */
    public static List<IFPServer> getActiveServers() {
        return new ArrayList<IFPServer>(activeServers.values());
    }

    /**
     * Get the active IFPServer instance for a site
     * 
     * @param siteID
     * @return the IFPServer instance or null if there is no active server for
     *         siteID
     */
    public static IFPServer getActiveServer(String siteID) {
        return activeServers.get(siteID);
    }

    String siteId;

    IFPServerConfig config;

    GridParmManager gridParmMgr;

    LockManager lockMgr;

    TopoDatabaseManager topoMgr; // TODO do we need this?

    private IFPServer(String siteId, IFPServerConfig config)
            throws DataAccessLayerException, PluginException, GfeException {
        this.siteId = siteId;
        this.config = config;
        this.lockMgr = new LockManager(siteId, config);
        this.gridParmMgr = new GridParmManager(siteId, config, lockMgr);
        this.topoMgr = new TopoDatabaseManager(siteId, config, gridParmMgr);

        statusHandler.info("MapManager initializing...");
        new MapManager(config);
    }

    private void dispose() {
        if (config.requestISC()) {
            IRTManager.getInstance().disableISC(config.getMhsid(), siteId);
        }

        try {
            new IscSendRecordDao().deleteForSite(siteId);
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not clear IscSendRecords for site " + siteId
                            + " from queue.", e);
        }

        // TODO necessary?
        NetCDFDatabaseManager.removeDatabases(siteId);
        GridLocationCache.removeGridLocationsForSite(siteId);

        this.gridParmMgr.dispose();
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
     * @return the gridParmMgr
     */
    public GridParmManager getGridParmMgr() {
        return gridParmMgr;
    }

    /**
     * @return the lockMgr
     */
    public LockManager getLockMgr() {
        return lockMgr;
    }

    /**
     * @return the topoMgr
     */
    public TopoDatabaseManager getTopoMgr() {
        return topoMgr;
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
        IFPServer ifp = activeServers.get(notif.getSiteID());
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
            if (pluginName.equals("grid")) {
                statusHandler.info("Processing " + pluginName
                        + " purge notification");
                ifpServer.getGridParmMgr().d2dGridDataPurged();
            } else if (pluginName.equals("satellite")) {
                statusHandler.info("Processing " + pluginName
                        + " purge notification");
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
        // ITimer timer = TimeUtil.getTimer();
        // timer.start();
        List<GridRecord> gridRecords = new LinkedList<GridRecord>();
        List<SatelliteRecord> satRecords = new LinkedList<SatelliteRecord>();

        for (String dataURI : message.getDataURIs()) {
            if (dataURI.startsWith("/grid/")) {
                gridRecords.add(new GridRecord(dataURI));
            } else if (dataURI.startsWith("/satellite/")) {
                satRecords.add(new SatelliteRecord(dataURI));
            }
        }

        for (IFPServer ifpServer : getActiveServers()) {
            if (!gridRecords.isEmpty()) {
                // TODO: remove this info before check in
                String msg = "Processing " + gridRecords.size()
                        + " grid DataURINotifications";
                statusHandler.info(msg);

                ifpServer.getGridParmMgr().filterGridRecords(gridRecords);
            }
            if (!satRecords.isEmpty()) {
                // TODO: remove this info before check in
                String msg = "Processing " + satRecords.size()
                        + " satellite DataURINotifications";
                statusHandler.info(msg);

                ifpServer.getGridParmMgr().filterSatelliteRecords(satRecords);
            }
        }
        // timer.stop();
        // perfLog.logDuration(
        // "GfeIngestNotificationFilter: processing DataURINotificationMessage",
        // timer.getElapsedTime());
    }

    /**
     * Get topo data for a GridLocation
     * 
     * @param gloc
     *            GridLocation for topo data
     * @return topo gridslice
     */
    public ServerResponse<ScalarGridSlice> getTopoData(GridLocation gloc) {
        return this.topoMgr.getTopoData(gloc);
    }

}
