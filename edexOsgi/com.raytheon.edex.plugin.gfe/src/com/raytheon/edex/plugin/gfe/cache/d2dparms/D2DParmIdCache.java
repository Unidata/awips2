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

package com.raytheon.edex.plugin.gfe.cache.d2dparms;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.db.dao.GFEDao;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.database.D2DGridDatabase;
import com.raytheon.edex.plugin.gfe.server.database.D2DSatDatabase;
import com.raytheon.edex.plugin.gfe.server.database.D2DSatDatabaseManager;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.parameter.mapping.ParameterMapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.site.SiteAwareRegistry;

/**
 * This class stores D2D parmIDs for quick and efficient access.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/08/09     1674       bphillip    Initial creation
 * 11/05/12     #1310      dgilling    Modify cache to listen to plugin
 *                                     purged topic.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class D2DParmIdCache {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DParmIdCache.class);

    /** The name this cache uses when being stored in the SharedObjectProvider */
    public static transient final String CACHE_NAME = "D2DParmIds";

    private static final Pattern RangeFilter = Pattern
            .compile("(.*?)\\d{1,2}hr");

    /** Map containing the ParmIDs */
    private Map<String, Set<String>> parmIds;

    private static D2DParmIdCache instance;

    public static synchronized D2DParmIdCache getInstance() {
        if (instance == null) {
            instance = new D2DParmIdCache();
        }
        return instance;
    }

    /**
     * Constructs a new D2DParmIdCache
     */
    public D2DParmIdCache() {
        parmIds = new HashMap<String, Set<String>>();
    }

    /**
     * Places a parmId into the cache
     * 
     * @param parmId
     *            The ParmID to add to the cache
     */
    public void putParmID(ParmID parmId) {

        D2DGridDatabase db = null;
        try {

            GridDatabase gridDb = GridParmManager.getDb(parmId.getDbId());
            if (gridDb instanceof D2DSatDatabase) {
                putParmIDInternal(parmId);
                return;
            } else if (gridDb instanceof D2DGridDatabase) {
                db = (D2DGridDatabase) gridDb;
            } else {
                putParmIDInternal(parmId);
                return;
            }
        } catch (GfeException e) {
            statusHandler.error("Error getting D2DGridDatabase for "
                    + parmId.getDbId());
            putParmIDInternal(parmId);
            return;
        }

        if (!db.isParmInfoDefined(parmId)) {
            String abbrev = parmId.getParmName();
            Matcher matcher = RangeFilter.matcher(abbrev);
            if (matcher.matches()) {
                abbrev = matcher.group(1);
                ParmID tempParmID = new ParmID(abbrev, parmId.getDbId(),
                        parmId.getParmLevel());
                if (db.isParmInfoDefined(tempParmID)) {
                    parmId = tempParmID;
                }
            }
        }
        putParmIDInternal(parmId);
    }

    private void putParmIDInternal(ParmID parmId) {
        synchronized (parmIds) {
            // Add the database entry to the map if it does not exist
            if (!parmIds.containsKey(parmId.getDbId().toString())) {
                parmIds.put(parmId.getDbId().toString(), new HashSet<String>());
            }
            // Insert the ParmID into the map
            String parmStr = parmId.toString();
            // Ensure that the staticTopo parameter has the correct
            // capitalization, or the smart init scripts will not be able to
            // find it
            String parmName = parmId.getParmName();
            if (parmName.equalsIgnoreCase("staticTopo")) {
                parmStr = parmStr.replace("statictopo", "staticTopo");
            } else if (parmName.equalsIgnoreCase("staticspacing")) {
                parmStr = parmStr.replace("staticspacing", "staticSpacing");
            } else if (parmName.equalsIgnoreCase("staticXspacing")) {
                parmStr = parmStr.replace("staticxspacing", "staticXspacing");
            } else if (parmName.equalsIgnoreCase("staticYspacing")) {
                parmStr = parmStr.replace("staticyspacing", "staticYspacing");
            } else if (parmName.equalsIgnoreCase("staticCoriolis")) {
                parmStr = parmStr.replace("staticcoriolis", "staticCoriolis");
            } else {
                String gfeParamName = null;
                try {
                    gfeParamName = ParameterMapper.getInstance().lookupAlias(
                            parmId.getParmName(), "gfeParamName");
                } catch (MultipleMappingException e) {
                    statusHandler.handle(Priority.WARN,
                            e.getLocalizedMessage(), e);
                    gfeParamName = e.getArbitraryMapping();
                }
                parmStr = parmStr.replaceFirst(
                        parmId.getParmName(),
                        gfeParamName);
            }
            parmIds.get(parmId.getDbId().toString()).add(parmStr);
        }
    }

    /**
     * Places a parmId into the cache
     * 
     * @param parmId
     *            String representation of a ParmID
     */
    public void putParmID(String parmId) {
        putParmID(new ParmID(parmId));
    }

    /**
     * Places a collection of ParmIDs into the cache
     * 
     * @param parmIds
     *            The parmIDs to add
     */
    public void putParmIDList(Collection<ParmID> parmIds) {
        for (ParmID id : parmIds) {
            putParmID(id);
        }
    }

    /**
     * Retrieves all the ParmIDs for a given DatabaseID
     * 
     * @param dbId
     *            The DatabaseID to retrieve the ParmIDs for
     * @return The ParmIDs in the given DatabaseID
     */
    public List<ParmID> getParmIDs(DatabaseID dbId) {
        Set<String> parmStrings = null;
        synchronized (parmIds) {
            if (parmIds.containsKey(dbId.toString())) {
                parmStrings = new HashSet<String>(parmIds.get(dbId.toString()));
            }
        }

        List<ParmID> parms = Collections.emptyList();
        if (parmStrings != null) {
            parms = new ArrayList<ParmID>(parmStrings.size());
            for (String parmStr : parmStrings) {
                parms.add(new ParmID(parmStr));
            }
        }
        return parms;
    }

    /**
     * Retrieves all the ParmIDs for a given DatabaseID
     * 
     * @param dbId
     *            The String representation of the DatabaseID to retrieve the
     *            ParmIDs for
     * @return The ParmIDs in the given DatabaseID
     */
    public List<ParmID> getParmIDs(String dbId) {
        return getParmIDs(new DatabaseID(dbId));
    }

    /**
     * Retrieves all DatabaseIDs
     * 
     * @return The list of DatabaseIDs
     */
    public List<DatabaseID> getDatabaseIDs() {
        Set<String> dbStrs;
        synchronized (parmIds) {
            dbStrs = new HashSet<String>(parmIds.keySet());
        }

        List<DatabaseID> dbIds = new ArrayList<DatabaseID>(dbStrs.size());
        for (String dbId : dbStrs) {
            dbIds.add(new DatabaseID(dbId));
        }
        return dbIds;
    }

    public void removeSiteDbs(String siteID) {

        statusHandler.handle(Priority.EVENTA, "Purging " + siteID
                + " parmIDs from d2d parmID cache...");

        List<DatabaseID> dbInv;

        if (UFStatus.getHandler().isPriorityEnabled(Priority.DEBUG)) {
            StringBuffer msg = new StringBuffer();
            msg.append("\nRemoving site information from D2DParmIdCache\nInitial Database Inventory:\n");
            dbInv = getDatabaseIDs();
            for (DatabaseID dbId : dbInv) {
                msg.append(dbId.toString()).append("\n");
            }
            statusHandler.handle(Priority.DEBUG, msg.toString());
        }

        List<DatabaseID> dbsToRemove = new ArrayList<DatabaseID>();
        dbInv = getDatabaseIDs();
        for (DatabaseID dbId : dbInv) {
            if (dbId.getSiteId().equalsIgnoreCase(siteID)) {
                dbsToRemove.add(dbId);
            }
        }

        synchronized (this.parmIds) {
            for (DatabaseID dbId : dbsToRemove) {
                this.parmIds.remove(dbId.toString());
                if (UFStatus.getHandler().isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler.handle(Priority.DEBUG,
                            "D2dParmIdCache Removed " + dbId);
                }
            }
        }

        if (UFStatus.getHandler().isPriorityEnabled(Priority.DEBUG)) {
            StringBuffer msg = new StringBuffer();
            msg.append("\nD2DParmIdCache Post-Purge Database Inventory:\n");
            dbInv = getDatabaseIDs();
            for (DatabaseID dbId : dbInv) {
                msg.append(dbId.toString()).append("\n");
            }
            statusHandler.handle(Priority.DEBUG, msg.toString());
        }

        statusHandler.handle(Priority.EVENTA, "Successfully purged all "
                + siteID + " parmIDs from d2d parmID cache...");
    }

    /**
     * Refreshes the cache for the given site. This is called upon site
     * activation. Also, the cache is rebuilt when the grib plugin purges its
     * data. The grib plugin will put a message on a topic so all members of the
     * cluster will know to rebuild their caches with the updated grib
     * inventory.
     * 
     * @param site
     *            The site to rebuild the cache for. If this is null, then that
     *            means this method is being fired off as the result of a grib
     *            purge
     * @throws PluginException
     *             If errors occur when interacting with the database.
     * @throws GfeConfigurationException
     *             If errors occur while retrieving the server config for the
     *             given site
     */
    public void buildCache(String site) throws PluginException,
            GfeConfigurationException {
        String[] activeSites = null;
        if (site == null || site.isEmpty()) {
            activeSites = SiteAwareRegistry.getInstance().getActiveSites();
        } else {
            activeSites = new String[] { site };
        }
        for (String siteID : activeSites) {
            List<DatabaseID> dbsToRemove = this.getDatabaseIDs();
            statusHandler.handle(Priority.EVENTA,
                    "Building D2DParmIdCache for " + siteID + "...");
            IFPServerConfig config = IFPServerConfigManager
                    .getServerConfig(siteID);
            GFEDao dao = new GFEDao();
            Set<ParmID> parmIds = new HashSet<ParmID>();
            long start = System.currentTimeMillis();
            List<String> d2dModels = config.getD2dModels();
            for (String d2dModelName : d2dModels) {
                String gfeModel = config.gfeModelNameMapping(d2dModelName);

                if ((d2dModelName != null) && (gfeModel != null)) {
                    List<DatabaseID> dbIds = null;
                    try {
                        dbIds = dao.getD2DDatabaseIdsFromDb(d2dModelName,
                                gfeModel, siteID);
                    } catch (DataAccessLayerException e) {
                        throw new PluginException(
                                "Unable to get D2D Database Ids from database!",
                                e);
                    }

                    if (!dbIds.isEmpty()) {
                        int versions = Math.min(
                                config.desiredDbVersions(dbIds.get(0)),
                                dbIds.size());

                        for (int i = 0; i < versions; i++) {
                            try {
                                parmIds.addAll(dao.getD2DParmIdsFromDb(
                                        d2dModelName, dbIds.get(i)));
                            } catch (DataAccessLayerException e) {
                                throw new PluginException(
                                        "Error adding parmIds to D2DParmIdCache!!",
                                        e);
                            }
                        }
                    }

                }
            }
            parmIds.addAll(D2DSatDatabaseManager.getSatDatabase(siteID)
                    .getParmList().getPayload());
            removeSiteDbs(siteID);
            putParmIDList(parmIds);
            List<DatabaseID> currentDbInventory = this.getDatabaseIDs();
            dbsToRemove.removeAll(currentDbInventory);
            for (DatabaseID dbId : dbsToRemove) {
                GridParmManager.removeDbFromMap(dbId);
            }
            statusHandler.handle(Priority.EVENTA,
                    "Total time to build D2DParmIdCache for " + siteID
                            + " took " + (System.currentTimeMillis() - start)
                            + " ms");
        }
    }

    /**
     * Counts the number of parmIds currently in the cache
     * 
     * @return The number of parmIds currently in the cache
     */
    public long getSize() {
        long size = 0;
        synchronized (parmIds) {
            for (Set<String> parms : parmIds.values()) {
                size += parms.size();
            }
        }
        return size;
    }

    public void pluginPurged(String pluginName)
            throws GfeConfigurationException, PluginException {
        if (pluginName.equals("grid")) {
            buildCache(null);
        }
    }
}
