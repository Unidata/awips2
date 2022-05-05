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
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.edex.plugin.gfe.config.GridDbConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.db.dao.GFEDao;
import com.raytheon.edex.plugin.gfe.server.database.D2DGridDatabase;
import com.raytheon.edex.plugin.gfe.server.database.D2DSatDatabase;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase;
import com.raytheon.edex.plugin.gfe.server.database.NetCDFDatabaseManager;
import com.raytheon.edex.plugin.gfe.server.database.TopoDatabase;
import com.raytheon.edex.plugin.gfe.server.database.TopoDatabaseManager;
import com.raytheon.edex.plugin.gfe.server.database.VGridDatabase;
import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.edex.plugin.gfe.smartinit.SmartInitQueue;
import com.raytheon.edex.plugin.gfe.smartinit.SmartInitRecord;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.DBInvChangeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.LockNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.request.CommitGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SaveGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.grid.GridPathProvider;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.purge.PurgeLogger;

/**
 * Class used to manage grid parms. Maintains an in memory cache of all known
 * GridDatabase objects
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 08, 0008  875      bphillip  Initial Creation
 * Jun 17, 0008  940      bphillip  Implemented GFE Locking
 * Jul 09, 0009  2590     njensen   Changed from singleton to static
 * Jul 12, 0012  15162    ryu       added check for invalid db
 * Oct 10, 0012  1260     randerso  Added exception handling for domain not
 *                                  overlapping the dataset
 * Feb 10, 0013  1603     randerso  General code cleanup, improved purge
 *                                  logging, fixed a purge inefficiency, fixed
 *                                  error which caused D2D purging to remove
 *                                  smartInit hdf5 data
 * Mar 07, 0013  1773     njensen   Logged commitGrid() times
 * Mar 15, 0013  1795     njensen   Sped up commitGrid()
 * Mar 20, 0013  1774     randerso  Removed dead method, changed to use new
 *                                  D2DGridDatabase constructor
 * Apr 23, 0013  1949     rjpeter   Added inventory retrieval for a given time
 *                                  range.
 * May 02, 0013  1969     randerso  Fixed possible null pointer in getParmList
 *                                  Removed inventory from
 *                                  DBInvChangeNotification
 * May 03, 0013  1974     randerso  Fixed error logging to include stack trace
 * May 14, 0013  2004     randerso  Added methods to synch GridParmManager
 *                                  across JVMs
 * May 30, 0013  2044     randerso  Refactored to better match A1 design.
 *                                  Removed D2DParmIDCache.
 * Jul 30, 0013  2057     randerso  Added support for marking obsoleted
 *                                  databases for removal and eventually purging
 *                                  them
 * Sep 12, 0013  2348     randerso  Added logging when database are
 *                                  added/removed from dbMap Fixed the
 *                                  synchronization of dbMap with the database
 *                                  inventory Changed to call
 *                                  D2DGridDatabase.getDatabase instead of
 *                                  calling the constructor directly to ensure
 *                                  the data exists before creating the
 *                                  D2DGridDatabase object
 * Oct 02, 0013  2444     randerso  Fix error handling when creating
 *                                  IFPGridDatabases. DO NOT ATTEMPT TO MERGE
 *                                  THIS CHANGE INTO 14.2 as the GFE server code
 *                                  has been significantly refactored.
 * Oct 10, 0013  2446     randerso  Added cluster lock to prevent two threads
 *                                  from attempting to publish the same parm
 *                                  simultaneously. Added code to check the
 *                                  purge times when publishing and not publish
 *                                  data that is eligible to be purged.
 * May 22, 0014  3071     randerso  Expand publish time to time constraint
 *                                  quantum after truncating it to the purge
 *                                  time
 * Jun 12, 0014  3244     randerso  Improved error handling
 * Jun 24, 2014  3317     randerso  Send DBInvChangeNotification when database
 *                                  is created, unless it's created in response
 *                                  to another DBInvChangeNotification so
 *                                  IFPServers stay in synch. Cleaned up
 *                                  commented code.
 * Jul 21, 2014  3415     randerso  Fixed d2dGridDataPurged to not purge NetCDF
 *                                  databases.
 * Sep 21, 2014  3648     randerso  Changed to do version purging when new
 *                                  databases are added
 * Oct 07, 2014  3684     randerso  Restructured IFPServer start up. Reordered
 *                                  handling of DbInvChangeNotification Don't
 *                                  process GFENotifications sent by this JVM
 *                                  Prevent createDB from creating databases
 *                                  that are immediately purged Send
 *                                  DBInvChangeNotifications at site activation
 *                                  so new D2D data ingested while deactivated
 *                                  gets recognized
 * Oct 27, 2014  3766     randerso  Fixed return type and javadoc for
 *                                  createNewDb
 * Mar 05, 2015  4169     randerso  Fix error handling in getDatabase
 * Nov 17, 2015  5129     dgilling  Ensure ServerResponse payload is always
 *                                  populated when calling getGridHistory.
 * Dec 03, 2015  5168     randerso  Added check to skip running smartInit for
 *                                  static data at non-zero fcsthr
 * Sep 12, 2016  5861     randerso  Changes to eliminate redundant database
 *                                  queries during D2DGridDatabase
 *                                  initialization.
 * Oct 19, 2017  6126     dgilling  Make NetCDFDatabaseManager objects owned by
 *                                  GridParmManager.
 * Dec 14, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Change clone() to copy(). Code cleanup
 * Jul 01, 2020  21963    dhaines   Added fix for DR#21963 - runIFPText creates 
 *                                  timestamped database
 *
 * </pre>
 *
 * @author bphillip
 */

public class GridParmManager {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridParmManager.class);

    private static final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    private static final String SMART_INIT_TASK_NAME = "GridParmManager";

    private static final String SMART_INIT_TASK_DETAILS = "SmartInit:";

    // don't rerun the smart init fire if they have been run in the last 30
    // minutes
    private static final int SMART_INIT_TIMEOUT = 1_800_000;

    private static final String COMMIT_GRIDS_TASK_NAME = "commitGrids";

    private static int COMMIT_GRIDS_TIMEOUT = 10_000;

    private final String siteID;

    private final IFPServerConfig config;

    private final LockManager lockMgr;

    private final TopoDatabaseManager topoMgr;

    private final NetCDFDatabaseManager netCdfDbManager;

    private final Map<DatabaseID, GridDatabase> dbMap = new ConcurrentHashMap<>();

    /**
     * Construct a GridParmManager
     *
     * @param siteID
     * @param config
     * @throws PluginException
     * @throws DataAccessLayerException
     * @throws GfeException
     */
    public GridParmManager(String siteID, IFPServerConfig config)
            throws PluginException, DataAccessLayerException, GfeException {
        this.siteID = siteID;
        this.config = config;
        this.lockMgr = new LockManager(siteID, config, this);
        this.topoMgr = new TopoDatabaseManager(siteID, config);
        this.netCdfDbManager = new NetCDFDatabaseManager(config);

        initializeManager();
    }

    /**
     * Dispose the GridParmManager
     */
    public void dispose() {

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

    private GridParm gridParm(ParmID id) {
        GridDatabase db = getDatabase(id.getDbId());
        if (db != null) {
            return createParm(id);
        } else {
            return new GridParm();
        }
    }

    private GridParm createParm(ParmID id) {
        GridDatabase db = getDatabase(id.getDbId());
        if (db != null) {
            return new GridParm(id, lockMgr, db);
        } else {
            statusHandler.debug(
                    "No matching GridDatabase for requested ParmID  in createParm()");
            return new GridParm();
        }
    }

    /**
     * Get inventory for a parm
     *
     * @param parmId
     * @return server response containing list of TimeRanges
     */
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID parmId) {

        ServerResponse<List<TimeRange>> sr = new ServerResponse<>();
        try {
            GridParm gp = gridParm(parmId);
            if (gp.isValid()) {
                sr = gp.getGridInventory();
            } else {
                sr.addMessage(
                        "Unknown Parm: " + parmId + " in getGridInventory()");
            }
        } catch (Exception e) {
            sr.addMessage("Unknown Parm: " + parmId + " in getGridInventory()");
            statusHandler.error(
                    "Unknown Parm: " + parmId + " in getGridInventory()", e);
        }

        return sr;
    }

    /**
     * Returns the grid inventory overlapping timeRange for the parmId. Returns
     * the status. Calls gridParm() to look up the parameter. If not found,
     * returns the appropriate error. Calls the grid parm's getGridInventory()
     * to obtain the inventory.
     *
     * @param parmId
     *            The parmID to get the inventory for
     * @param timeRange
     *            The timeRange to get the inventory for
     * @return The server response
     */
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID parmId,
            TimeRange timeRange) {

        ServerResponse<List<TimeRange>> sr = new ServerResponse<>();
        try {
            GridParm gp = gridParm(parmId);
            if (gp.isValid()) {
                sr = gp.getGridInventory(timeRange);
            } else {
                sr.addMessage(
                        "Unknown Parm: " + parmId + " in getGridInventory()");
            }
        } catch (Exception e) {
            sr.addMessage("Unknown Parm: " + parmId + " in getGridInventory()");
            statusHandler.error(
                    "Unknown Parm: " + parmId + " in getGridInventory()", e);
        }

        return sr;
    }

    /**
     * @param parmId
     * @param trs
     * @return map of TimeRanges to Lists of GridDataHistorys
     */
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID parmId, List<TimeRange> trs) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<>();
        Map<TimeRange, List<GridDataHistory>> history = Collections.emptyMap();

        try {
            GridParm gp = gridParm(parmId);
            if (gp.isValid()) {
                ServerResponse<Map<TimeRange, List<GridDataHistory>>> ssr = gp
                        .getGridHistory(trs);
                sr.addMessages(ssr);
                if (ssr.getPayload() != null) {
                    history = ssr.getPayload();
                }
            } else {
                sr.addMessage(
                        "Unknown Parm: " + parmId + " in getGridInventory()");
            }
        } catch (Exception e) {
            sr.addMessage("Unknown Parm: " + parmId + " in getGridInventory()");
            statusHandler.error(
                    "Unknown Parm: " + parmId + " in getGridInventory()", e);
        }

        sr.setPayload(history);
        return sr;
    }

    /**
     * Get GridParmInfo for a parm
     *
     * @param parmId
     * @return the GridParmInfo
     */
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID parmId) {

        ServerResponse<GridParmInfo> sr = new ServerResponse<>();

        try {
            GridParm gp = gridParm(parmId);

            if (gp.isValid()) {
                sr = gp.getGridParmInfo();
            } else {
                sr.addMessage(
                        "Unknown Parm: " + parmId + " in getGridParmInfo()");
            }
        } catch (Exception e) {
            sr.addMessage("Unknown Parm: " + parmId + " in getGridParmInfo()");
            statusHandler.error(
                    "Unknown Parm: " + parmId + " in getGridParmInfo()", e);
        }
        return sr;
    }

    /**
     * Save grid data
     *
     * @param saveRequest
     * @param requestorId
     * @return ServerResponse with status only
     */
    public ServerResponse<?> saveGridData(List<SaveGridRequest> saveRequest,
            WsId requestorId) {

        ServerResponse<?> sr = new ServerResponse<>();

        // process each request
        for (SaveGridRequest req : saveRequest) {
            ServerResponse<?> ssr = null;
            GridParm gp = null;
            gp = gridParm(req.getParmId());
            if (!gp.isValid()) {
                sr.addMessage("Unknown Parm: " + req.getParmId()
                        + " in saveGridData()");
                statusHandler.error("Unknown Parm: " + req.getParmId()
                        + " in saveGridData()");
                continue;
            }

            // save the data
            ssr = gp.saveGridData(req, requestorId);
            sr.addMessages(ssr);
            if (!ssr.isOkay()) {
                sr.addMessage("Save Grid Data Failed for: " + req.getParmId()
                        + " " + req.getReplacementTimeRange() + " "
                        + ssr.message());
                continue;
            }

            // grid update notification is returned without workstation id
            for (GfeNotification notify : sr.getNotifications()) {
                if (notify instanceof GridUpdateNotification) {
                    ((GridUpdateNotification) notify)
                            .setWorkstationID(requestorId);
                }
            }
        }
        return sr;
    }

    /**
     * Get grid data
     *
     * @param getRequest
     * @return list of grid slices
     */
    public ServerResponse<List<IGridSlice>> getGridData(
            List<GetGridRequest> getRequest) {

        ServerResponse<List<IGridSlice>> sr = new ServerResponse<>();
        List<TimeRange> badDataTimes = new ArrayList<>();
        for (GetGridRequest req : getRequest) {
            try {
                GridParm gp = gridParm(req.getParmId());
                if (!gp.isValid()) {
                    sr.addMessage("Unknown Parm: " + req.getParmId()
                            + " in getGridData()");
                    break;
                }
                ServerResponse<List<IGridSlice>> inner = gp.getGridData(req,
                        badDataTimes);
                if (sr.getPayload() == null) {
                    sr.setPayload(new ArrayList<IGridSlice>());
                }
                if (inner != null) {
                    if (inner.getPayload() != null) {
                        sr.getPayload().addAll(inner.getPayload());
                    }
                    sr.addMessages(inner);
                }
            } catch (Exception e) {
                sr.addMessage("Unknown Parm: " + req.getParmId()
                        + " in getGridData()");
                statusHandler.error("Unknown Parm: " + req.getParmId()
                        + " in getGridData()", e);
            }
        }

        return sr;
    }

    /**
     * Commit (Publish) grids
     *
     * @param request
     * @param requestorId
     * @param changes
     * @return list of GridUpdateNotifications for updated grids
     */
    public ServerResponse<List<GridUpdateNotification>> commitGrid(
            List<CommitGridRequest> request, WsId requestorId,
            List<GridUpdateNotification> changes) {

        ServerResponse<List<GridUpdateNotification>> sr = new ServerResponse<>();
        sr.setPayload(new ArrayList<GridUpdateNotification>());
        if (request.isEmpty()) {
            return sr;
        }

        ServerResponse<GridDatabase> ssr1 = getOfficialDB(request.get(0));
        GridDatabase officialDBPtr = ssr1.getPayload();
        DatabaseID officialDBid = officialDBPtr.getDbId();
        sr.addMessages(ssr1);

        if (!sr.isOkay()) {
            return sr;
        }

        // convert to parm type requests
        List<CommitGridRequest> parmReq = convertToParmReq(request)
                .getPayload();
        if (!sr.isOkay()) {
            return sr;
        }

        statusHandler.info("Publish/Commit Grids Request: " + parmReq);
        List<CommitGridRequest> failures = new ArrayList<>();

        ITimer lockTimer = TimeUtil.getTimer();
        ITimer inventoryTimer = TimeUtil.getTimer();
        ITimer retrieveTimer = TimeUtil.getTimer();
        ITimer historyRetrieveTimer = TimeUtil.getTimer();
        ITimer historyUpdateTimer = TimeUtil.getTimer();
        ITimer storeTimer = TimeUtil.getTimer();

        // process each request
        ServerResponse<?> srDetailed = new ServerResponse<>();
        for (int r = 0; r < parmReq.size(); r++) {
            CommitGridRequest req = parmReq.get(r);
            ServerResponse<?> ssr = new ServerResponse<>();
            TimeRange publishTime = req.getTimeRange();

            // for the source data
            ParmID sourceParmId = req.getParmId();
            GridParm sourceGP = gridParm(sourceParmId);
            if (!sourceGP.isValid()) {
                ssr.addMessage("Unknown Source Parm: " + req.getParmId()
                        + " in commitGrid()");
                srDetailed.addMessages(ssr);
                failures.add(req);
                continue;
            }

            // for the destination data
            ParmID destParmId = new ParmID(req.getParmId().getParmName(),
                    officialDBid, req.getParmId().getParmLevel());
            String destParmIdStr = destParmId.toString();
            GridParm destGP = null;
            destGP = gridParm(destParmId);
            if (!destGP.isValid()) {
                ssr.addMessage("Unknown Destination Parm: " + destGP
                        + " in commitGrid()");
                srDetailed.addMessages(ssr);
                failures.add(req);
                continue;
            }

            // verify that the source and destination are matched
            GridParmInfo sourceInfo, destInfo;
            ServerResponse<GridParmInfo> gpiSsr = sourceGP.getGridParmInfo();
            ssr.addMessages(gpiSsr);
            sourceInfo = gpiSsr.getPayload();
            gpiSsr = destGP.getGridParmInfo();
            ssr.addMessages(gpiSsr);
            destInfo = gpiSsr.getPayload();

            ssr.addMessages(compareGridParmInfoForCommit(sourceInfo, destInfo));
            if (!ssr.isOkay()) {
                ssr.addMessage(
                        "GetGridParmInfo for source/dest, or compare for commitGrid() failure: "
                                + ssr.message());
                srDetailed.addMessages(ssr);
                failures.add(req);
                continue;
            }

            // ClusterLock to ensure only one thread is publishing at a time.
            lockTimer.start();
            ClusterTask ct = ClusterLockUtils.lookupLock(COMMIT_GRIDS_TASK_NAME,
                    destParmIdStr);
            ct = ClusterLockUtils.lock(COMMIT_GRIDS_TASK_NAME, destParmIdStr,
                    COMMIT_GRIDS_TIMEOUT, true);
            lockTimer.stop();
            try {

                // TODO: No need to get inventory and then compare for
                // history times, just request the history times directly

                // get later of source and destination purge times
                Date sourcePurge = purgeTime(sourceParmId.getDbId());
                Date startTime = purgeTime(destParmId.getDbId());
                if (sourcePurge.after(startTime)) {
                    startTime = sourcePurge;
                }

                // trim publish time to avoid publishing purgeable data
                if (startTime.after(publishTime.getStart())) {
                    publishTime.setStart(startTime);
                }

                // expand publishTime to TimeContraints so full grids are
                // published
                publishTime = sourceInfo.getTimeConstraints()
                        .expandTRToQuantum(publishTime);

                inventoryTimer.start();
                ServerResponse<List<TimeRange>> invSr = sourceGP
                        .getGridInventory(publishTime);
                List<TimeRange> overlapInventory = invSr.getPayload();
                ssr.addMessages(invSr);
                if (!ssr.isOkay()) {
                    ssr.addMessage(
                            "GetGridInventory for source for commitGrid() failure: "
                                    + ssr.message());
                    srDetailed.addMessages(ssr);
                    failures.add(req);
                }

                // expand publish time to span overlapping inventory
                if (!overlapInventory.isEmpty()) {
                    Date d = overlapInventory.get(0).getStart();
                    if (d.before(publishTime.getStart())) {
                        publishTime.setStart(d);
                    }

                    d = overlapInventory.get(overlapInventory.size() - 1)
                            .getEnd();
                    if (d.after(publishTime.getEnd())) {
                        publishTime.setEnd(d);
                    }
                }

                invSr = destGP.getGridInventory(publishTime);
                inventoryTimer.stop();
                List<TimeRange> destInventory = invSr.getPayload();
                ssr.addMessages(invSr);
                if (!ssr.isOkay()) {
                    ssr.addMessage(
                            "GetGridInventory for destination for commitGrid() failure: "
                                    + ssr.message());
                    srDetailed.addMessages(ssr);
                    failures.add(req);
                    continue;
                }

                // get the source grid data
                List<IGridSlice> sourceData = null;
                List<TimeRange> badGridTR = new ArrayList<>();

                // System.out.println("overlapInventory initial size "
                // + overlapInventory.size());

                historyRetrieveTimer.start();
                ServerResponse<Map<TimeRange, List<GridDataHistory>>> history = sourceGP
                        .getGridHistory(overlapInventory);
                Map<TimeRange, List<GridDataHistory>> currentDestHistory = destGP
                        .getGridHistory(overlapInventory).getPayload();
                historyRetrieveTimer.stop();

                Map<TimeRange, List<GridDataHistory>> historyOnly = new HashMap<>();
                for (TimeRange tr : history.getPayload().keySet()) {
                    // should only ever be one history for source grids
                    List<GridDataHistory> gdhList = history.getPayload()
                            .get(tr);
                    boolean doPublish = false;
                    for (GridDataHistory gdh : gdhList) {
                        // if update time is less than publish time, grid
                        // has not changed since last published,
                        // therefore only update history, do not publish
                        if ((gdh.getPublishTime() == null)
                                || (gdh.getUpdateTime().getTime() > gdh
                                        .getPublishTime().getTime())
                                /*
                                 * in service backup, times on srcHistory could
                                 * appear as not needing a publish, even though
                                 * dest data does not exist
                                 */
                                || (currentDestHistory.get(tr) == null)
                                || (currentDestHistory.get(tr).isEmpty())) {
                            doPublish = true;
                        }
                    }
                    if (!doPublish) {
                        historyOnly.put(tr, gdhList);
                        overlapInventory.remove(tr);
                    }
                }

                retrieveTimer.start();
                ServerResponse<List<IGridSlice>> getSr = sourceGP.getGridData(
                        new GetGridRequest(req.getParmId(), overlapInventory),
                        badGridTR);
                retrieveTimer.stop();
                // System.out.println("Retrieved " + overlapInventory.size()
                // + " grids");
                sourceData = getSr.getPayload();
                ssr.addMessages(getSr);
                if (!ssr.isOkay()) {
                    ssr.addMessage(
                            "GetGridData for source for commitGrid() failure: "
                                    + ssr.message());
                    srDetailed.addMessages(ssr);
                    failures.add(req);
                    continue;
                }

                // get list of official grids that overlap publish range and
                // aren't contained in the publish range, these have to be
                // included in the publish step. Then get the grids, shorten
                // and insert into sourceData.
                List<IGridSlice> officialData = new ArrayList<>();
                List<TimeRange> officialTR = new ArrayList<>();
                for (int t = 0; t < destInventory.size(); t++) {
                    if (!publishTime.contains(destInventory.get(t))) {
                        officialTR.add(destInventory.get(t));
                    }
                }

                if (!officialTR.isEmpty()) {
                    retrieveTimer.start();
                    getSr = destGP.getGridData(
                            new GetGridRequest(destParmId, officialTR),
                            badGridTR);
                    retrieveTimer.stop();
                    officialData = getSr.getPayload();
                    ssr.addMessages(getSr);
                    if (!ssr.isOkay()) {
                        ssr.addMessage(
                                "GetGridData for official for commidtGrid() failure: "
                                        + ssr.message());
                        srDetailed.addMessages(ssr);
                        failures.add(req);
                        continue;
                    }

                    // insert the grid into the "sourceGrid" list
                    for (int t = 0; t < officialTR.size(); t++) {
                        if (officialTR.get(t).getStart()
                                .before(publishTime.getStart())) {

                            IGridSlice tempSlice = officialData.get(t).copy();
                            tempSlice.setValidTime(
                                    new TimeRange(officialTR.get(t).getStart(),
                                            publishTime.getStart()));
                            sourceData.add(0, tempSlice);
                            publishTime.setStart(officialTR.get(t).getStart());
                            overlapInventory.add(tempSlice.getValidTime());
                        }

                        // after
                        if (officialTR.get(t).getEnd()
                                .after(publishTime.getEnd())) {
                            IGridSlice tempSlice = officialData.get(t).copy();
                            tempSlice.setValidTime(
                                    new TimeRange(publishTime.getEnd(),
                                            officialTR.get(t).getEnd()));
                            sourceData.add(tempSlice);
                            publishTime.setEnd(officialTR.get(t).getEnd());
                            overlapInventory.add(tempSlice.getValidTime());
                        }
                    }
                }

                // save off the source grid history, to update the source
                // database modify the source grid data for the dest ParmID and
                // GridDataHistory
                Map<TimeRange, List<GridDataHistory>> histories = new HashMap<>();
                Date nowTime = new Date();

                for (IGridSlice slice : sourceData) {
                    GridDataHistory[] sliceHist = slice.getHistory();
                    for (GridDataHistory hist : sliceHist) {
                        hist.setPublishTime((Date) nowTime.clone());
                    }
                    slice.getGridInfo().resetParmID(destParmId);
                    histories.put(slice.getValidTime(),
                            Arrays.asList(sliceHist));
                }

                // update the history for publish time for grids that are
                // unchanged
                for (TimeRange tr : historyOnly.keySet()) {
                    List<GridDataHistory> histList = historyOnly.get(tr);
                    for (GridDataHistory hist : histList) {
                        hist.setPublishTime((Date) nowTime.clone());
                    }
                    histories.put(tr, histList);
                }

                // update the publish times in the source database,
                // update the notifications
                historyUpdateTimer.start();
                sr.addMessages(sourceGP.updatePublishTime(histories.values(),
                        (Date) nowTime.clone()));
                // System.out.println("Updated " + histories.size() +
                // " histories");
                historyUpdateTimer.stop();

                List<TimeRange> historyTimes = new ArrayList<>(
                        histories.keySet());
                Collections.sort(historyTimes);
                changes.add(new GridUpdateNotification(req.getParmId(),
                        publishTime, histories, requestorId, siteID));

                // update the histories of destination database for ones
                // that are not going to be saved since there hasn't been a
                // change
                List<TimeRange> historyOnlyList = new ArrayList<>();
                historyOnlyList.addAll(historyOnly.keySet());

                historyRetrieveTimer.start();
                Map<TimeRange, List<GridDataHistory>> destHistory = destGP
                        .getGridHistory(historyOnlyList).getPayload();
                historyRetrieveTimer.stop();
                for (TimeRange tr : destHistory.keySet()) {
                    List<GridDataHistory> srcHistList = histories.get(tr);
                    List<GridDataHistory> destHistList = destHistory.get(tr);
                    for (int i = 0; i < srcHistList.size(); i++) {
                        destHistList.get(i).replaceValues(srcHistList.get(i));
                    }
                }

                // only need to update the publish time on the destination
                // histories of grids that are not being saved (due to no
                // changes), because the saveGridSlices() call below will update
                // the publish time of the ones with changes
                historyUpdateTimer.start();
                destGP.updatePublishTime(destHistory.values(),
                        (Date) nowTime.clone());
                historyUpdateTimer.stop();

                // save data directly to the official database (bypassing
                // the checks in Parm intentionally)
                storeTimer.start();
                ssr.addMessages(officialDBPtr.saveGridSlices(destParmId,
                        publishTime, sourceData, requestorId, historyOnlyList));
                storeTimer.stop();

                // System.out.println("Published " + sourceData.size() +
                // " slices");
                if (!ssr.isOkay()) {
                    ssr.addMessage(
                            "SaveGridData for official for commitGrid() failure: "
                                    + ssr.message());
                    srDetailed.addMessages(ssr);
                    failures.add(req);
                    continue;
                }

                // make the notification
                GridUpdateNotification not = new GridUpdateNotification(
                        destParmId, publishTime, histories, requestorId,
                        siteID);
                changes.add(not);
                sr.getPayload().add(not);

            } finally {
                ClusterLockUtils.unlock(ct, false);
            }
        }

        perfLog.logDuration("Publish Grids: Acquiring cluster lock",
                lockTimer.getElapsedTime());
        perfLog.logDuration("Publish Grids: Retrieving inventories",
                inventoryTimer.getElapsedTime());
        perfLog.logDuration("Publish Grids: Retrieving histories",
                historyRetrieveTimer.getElapsedTime());
        perfLog.logDuration("Publish Grids: Retrieving data",
                retrieveTimer.getElapsedTime());
        perfLog.logDuration("Publish Grids: Updating histories",
                historyUpdateTimer.getElapsedTime());
        perfLog.logDuration("Publish Grids: Storing data",
                storeTimer.getElapsedTime());

        // if a problem occurred, log the information
        if (!failures.isEmpty()) {
            if (failures.size() == parmReq.size()) {
                StringBuilder sb = new StringBuilder();
                for (CommitGridRequest cgr : parmReq) {
                    sb.append(cgr.getParmId().toString());
                    sb.append(",");
                }
                sr.addMessage("Publish Failed Completely for parms "
                        + sb.toString() + ": " + srDetailed.message());
            } else {
                sr.addMessage("Publish Partially Failed.");
                for (int i = 0; i < failures.size(); i++) {
                    sr.addMessage("Failed for: " + failures + " "
                            + srDetailed.message());
                }
            }
        }

        return sr;
    }

    /**
     * Get database inventory
     *
     * @return list of available DatabaseIDs
     */
    public ServerResponse<List<DatabaseID>> getDbInventory() {
        ServerResponse<List<DatabaseID>> sr = new ServerResponse<>();

        List<DatabaseID> databases = new ArrayList<>(this.dbMap.keySet());

        sr.setPayload(databases);
        return sr;
    }

    /**
     * Get a database if available. If not, a database with the given id is created.
     *
     * @param dbId
     * @return GridDatabase or null if it could not be created
     */
    public GridDatabase getDatabase(DatabaseID dbId) {
        return getDatabase(dbId, true);
    }

    private GridDatabase getDatabase(DatabaseID dbId, boolean notify) {
        // look up the database in the map
        GridDatabase db = this.dbMap.get(dbId);

        // if db not in map try to create it
        if (db == null) {
            if ("D2D".equals(dbId.getDbType())) {
                String d2dModelName = config
                        .d2dModelNameMapping(dbId.getModelName());
                if (d2dModelName != null) {
                    db = D2DGridDatabase.getDatabase(config, d2dModelName,
                            dbId.getModelDate());
                } else {
                    statusHandler.error("No D2D Mapping for model name: "
                            + dbId.getModelName());
                }
            } else {
                ServerResponse<GridDatabase> status = createDB(dbId);
                if (status.isOkay()) {
                    db = status.getPayload();
                } else {
                    statusHandler.error(status.message());
                }
            }

            if (db != null) {
                this.addDB(db);

                // do version purging
                List<DatabaseID> purged = null;
                if (!db.getDbId().getModelTime()
                        .equals(DatabaseID.NO_MODEL_TIME)) {
                    purged = versionPurge(db.getDbId());
                }

                if (notify) {
                    createDbNotification(Arrays.asList(dbId), purged);
                }
                // always notify on purges
                else {
                    createDbNotification(null, purged);
                }
            }
        }

        return db;
    }

    /**
     * Create a new database
     *
     * @param dbId
     * @return ServerResponse containing status only
     */
    public ServerResponse<?> createNewDb(DatabaseID dbId) {

        ServerResponse<?> sr = new ServerResponse<>();

        if (!dbId.getFormat().equals(DataType.GRID)) {
            sr.addMessage("Invalid database id for createNewDb(): " + dbId);
            return sr;
        }

        //getDatabase creates a new db when called with a dbId that doesn't exist
        GridDatabase db = this.getDatabase(dbId);
        
        if (db != null) {
            // database already exists/was just created
            return sr;
        } else {
            sr.addMessage("Unable to create database: " + dbId);
        }

        return sr;
    }

    /**
     * Delete database
     *
     * @param dbId
     * @return ServerResponse containg status only
     */
    public ServerResponse<?> deleteDb(DatabaseID dbId) {
        ServerResponse<?> sr = new ServerResponse<>();

        GridDatabase db = this.getDatabase(dbId);
        if (db == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Cannot delete database " + dbId + ". It does not exist");
            sr.addMessage(
                    "Cannot delete database " + dbId + ". It does not exist");
            return sr;
        }

        // is it a singleton or official database?
        DatabaseID idWOTime = dbId.stripModelTime();
        if (this.config.getSingletonDatabases().contains(idWOTime)) {
            sr.addMessage("Cannot delete database " + dbId
                    + ". It is a singleton database");
            statusHandler.handle(Priority.PROBLEM, "Cannot delete database "
                    + dbId + ". It is a singleton database");
            return sr;
        }

        if (this.config.getOfficialDatabases().contains(idWOTime)) {
            sr.addMessage("Cannot delete database " + dbId
                    + ". It is an official database");
            statusHandler.handle(Priority.PROBLEM, "Cannot delete database "
                    + dbId + ". It is an official database");
            return sr;
        }

        // delete it
        deallocateDb(dbId, true);
        return sr;
    }

    /**
     * Retrieve the parm list for database
     *
     * @param dbId
     * @return the parm list
     */
    public ServerResponse<List<ParmID>> getParmList(DatabaseID dbId) {

        ServerResponse<List<ParmID>> sr = new ServerResponse<>();
        DatabaseID idWOTime = dbId.stripModelTime();
        GridDatabase db = getDatabase(dbId);

        if (db == null) {
            // check if the database is a singleton
            if (this.config.getSingletonDatabases().contains(idWOTime)) {
                String msg = "Cannot create singleton database " + dbId
                        + " with non-zero modeltime";
                sr.addMessage(msg);
                statusHandler.handle(Priority.PROBLEM, msg);
                return sr; 
            }
            
            sr.addMessage(
                    "Database " + dbId + " does not exist for getParmList()");
            return sr;
        }

        sr = db.getParmList();
        return sr;
    }
    
    /**
     * Purge grids based on time
     *
     * @param gridNotifications
     *            list to add GridUpdateNotifications
     * @param lockNotifications
     *            list to add LockNotifications
     * @return ServerResponse containing status only
     */
    public ServerResponse<?> gridsPurge(
            List<GridUpdateNotification> gridNotifications,
            List<LockNotification> lockNotifications) {

        ServerResponse<List<DatabaseID>> sr = new ServerResponse<>();
        sr = getDbInventory();

        if (!sr.isOkay()) {
            sr.addMessage("GridsPurge failed - couldn't get inventory");
            return sr;
        }

        List<DatabaseID> databases = sr.getPayload();

        for (DatabaseID dbId : databases) {
            Date purgeTime = purgeTime(dbId);
            if (purgeTime == null) {
                continue;
            }

            List<ParmID> parmIds = new ArrayList<>();
            ServerResponse<List<ParmID>> ssr = getParmList(dbId);
            sr.addMessages(ssr);
            if (!ssr.isOkay()) {
                continue;
            }

            parmIds = ssr.getPayload();

            int purgedCount = 0;
            for (ParmID parmId : parmIds) {
                List<GridUpdateNotification> gridNotify = new ArrayList<>();
                List<LockNotification> lockNotify = new ArrayList<>();
                GridParm gp = createParm(parmId);
                if (gp.isValid()) {
                    ServerResponse<Integer> sr1 = gp.timePurge(purgeTime,
                            gridNotify, lockNotify);
                    sr.addMessages(sr1);
                    if (sr1.isOkay()) {
                        purgedCount += sr1.getPayload();
                    }

                    gridNotifications.addAll(gridNotify);
                    lockNotifications.addAll(lockNotify);
                }
            }

            PurgeLogger.logInfo("Purge " + purgedCount + " items from " + dbId,
                    "gfe");
        }

        return sr;
    }

    private ServerResponse<GridDatabase> createDB(DatabaseID dbId) {
        // TODO: consider merging this into getDatabase()
        ServerResponse<GridDatabase> status = new ServerResponse<>();
        if (!dbId.isValid() || !dbId.getFormat().equals(DataType.GRID)) {
            status.addMessage("Database id " + dbId
                    + " is not valid, or is not a grid-type.  Cannot create database.");
            return status;
        }
        
        // is it a singleton database?
        DatabaseID idWOTime = dbId.stripModelTime();
        if (this.config.getSingletonDatabases().contains(idWOTime)
               && !dbId.getModelTime().equals(DatabaseID.NO_MODEL_TIME)) {
           status.addMessage("Cannot create singleton database " + dbId
                   + " with non-zero modeltime");
           return status;
        }
        
        // get the dbConfig
        IFPGridDatabase db = null;
        GridDbConfig dbConfig = this.config.gridDbConfig(dbId);
        if (dbConfig == null) {
            status.addMessage(
                    "Unable to obtain GridDbConfig information for creation"
                            + " in createDB() for " + dbId);
        } else {
            // don't create if this version would be immediately purgeable
            List<DatabaseID> existing = new ArrayList<>();
            for (DatabaseID id : this.dbMap.keySet()) {
                if (id.sameModel(dbId)) {
                    existing.add(id);
                }
            }

            if (!existing.contains(dbId)) {
                existing.add(dbId);
            }

            // sort by model time (most recent first)
            Collections.sort(existing);

            int desiredVersions = this.config.desiredDbVersions(dbId);
            if (existing.indexOf(dbId) >= desiredVersions) {
                status.addMessage("Unable to create " + dbId
                        + " as it would exceed the desired number of versions");
                return status;
            }

            // attempt to create the GridDatabase
            db = new IFPGridDatabase(dbId, dbConfig);
            if (db.databaseIsValid()) {
                // get databaseID object from database
                DatabaseID id = db.getDbId();

                if (id.getRemovedDate() != null) {
                    // mark database as not removed
                    try {
                        GFEDao gfeDao = new GFEDao();
                        gfeDao.setDatabaseRemovedDate(id, null);
                        statusHandler.info("Database " + id + " restored");
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to mark database restored: " + id, e);
                    }
                }
            } else {
                status.addMessage("Database " + dbId + " is not valid.");
                db = null;
            }
        }

        if (db == null) {
            // mark database as removed
            try {
                GFEDao gfeDao = new GFEDao();
                gfeDao.setDatabaseRemovedDate(dbId, new Date());
                statusHandler
                        .warn("Database " + dbId + " marked for removal in "
                                + GFEDao.REMOVED_DB_PURGE_TIME + " days.");
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to mark database removed: " + dbId, e);
            }
        }

        status.setPayload(db);
        return status;
    }

    private void deallocateDb(DatabaseID id, boolean deleteFile) {
        GridDatabase db = this.dbMap.remove(id);

        if (db != null) {
            statusHandler.info("deallocateDb called, removing " + id);
            if (deleteFile) {
                db.deleteDb();
            }
        }
    }

    private void initializeManager()
            throws GfeException, DataAccessLayerException, PluginException {

        // add the topo database
        TopoDatabase topoDb = topoMgr.getTopoDatabase();
        if (topoDb != null) {
            this.addDB(topoDb);
        }

        // get existing list (of just GRIDs)
        GFEDao gfeDao = new GFEDao();
        List<DatabaseID> inventory = gfeDao.getDatabaseInventory(siteID);

        // get list of singleton databases from config
        // and add them if missing from inventory (GRID-type only)
        for (DatabaseID manID : this.config.getSingletonDatabases()) {
            if (manID.getFormat().equals(DataType.GRID)
                    && !inventory.contains(manID)) {
                inventory.add(manID);
            }
        }

        // create the databases (the list should now only contain GRID dbs)
        List<DatabaseID> added = new ArrayList<>(inventory.size());
        for (DatabaseID dbId : inventory) {
            GridDatabase db = getDatabase(dbId, false);
            if (db != null) {
                added.add(db.getDbId());
            }
        }

        for (DatabaseID dbId : netCdfDbManager.getDatabaseIds()) {
            GridDatabase db = netCdfDbManager.getDb(dbId);
            addDB(db);
        }

        // Fire up the sat database.
        D2DSatDatabase satDb = new D2DSatDatabase(config);
        addDB(satDb);

        added.addAll(initD2DDbs());
        createDbNotification(added, null);

        // only fire smartInits if queue is instantiated
        SmartInitQueue queue = SmartInitQueue.getQueue();
        if (queue != null) {
            // acquire cluster lock since only needs to happen once
            ClusterTask ct = ClusterLockUtils.lookupLock(SMART_INIT_TASK_NAME,
                    SMART_INIT_TASK_DETAILS + siteID);

            ct = ClusterLockUtils.lock(SMART_INIT_TASK_NAME,
                    SMART_INIT_TASK_DETAILS + siteID, SMART_INIT_TIMEOUT,
                    false);
            if (LockState.SUCCESSFUL.equals(ct.getLockState())) {
                boolean clearTime = false;
                try {
                    for (DatabaseID dbId : this.dbMap.keySet()) {
                        if ("D2D".equals(dbId.getDbType()) && !config
                                .initModels(dbId.getModelName()).isEmpty()) {
                            statusHandler.info("Firing smartinit for: " + dbId);
                            VGridDatabase db = (VGridDatabase) getDatabase(
                                    dbId);
                            SortedSet<Date> validTimes = db.getValidTimes();
                            for (Date validTime : validTimes) {
                                queue.queue(siteID, config, dbId, validTime,
                                        false,
                                        SmartInitRecord.SITE_ACTIVATION_INIT_PRIORITY);
                            }
                        }
                    }
                } finally {
                    ClusterLockUtils.unlock(ct, clearTime);
                }
            }
            // }
        }
    }

    private List<DatabaseID> initD2DDbs() {
        List<DatabaseID> added = new ArrayList<>();
        for (String d2dModelName : config.getD2dModels()) {
            try {
                // get dbId to get desiredDbVersions (date doesn't matter)
                DatabaseID dbId = D2DGridDatabase.getDbId(d2dModelName,
                        new Date(), config);
                int desiredVersions = config.desiredDbVersions(dbId);

                for (Date refTime : D2DGridDatabase
                        .getModelRunTimes(d2dModelName, desiredVersions)) {
                    GridDatabase db = D2DGridDatabase.createDatabase(config,
                            d2dModelName, refTime);
                    if (db != null) {
                        this.addDB(db);
                        added.add(db.getDbId());
                    }
                }
            } catch (Exception e) {
                statusHandler.error(
                        "Error initializing D2D model: " + d2dModelName, e);
            }
        }
        return (added);
    }

    /**
     * @param gridRecords
     */
    public void filterGridRecords(List<GridRecord> gridRecords) {
        List<GridUpdateNotification> guns = new LinkedList<>();
        for (GridRecord record : gridRecords) {

            // ignore static parameters for non-zero forecast hours
            if ((record.getDataTime().getFcstTime() > 0)
                    && GridPathProvider.STATIC_PARAMETERS.contains(
                            record.getParameter().getAbbreviation())) {
                continue;
            }

            String d2dModelName = record.getDatasetId();
            Date refTime = record.getDataTime().getRefTime();
            DatabaseID dbId = D2DGridDatabase.getDbId(d2dModelName, refTime,
                    config);

            // not a d2d model we care about
            if (dbId == null) {
                continue;
            }

            D2DGridDatabase db = (D2DGridDatabase) getDatabase(dbId, true);
            if (db == null) {
                continue;
            }

            GridUpdateNotification gun = db.update(record);
            if (gun != null) {
                guns.add(gun);

                // only fire smartInits if queue is instantiated
                SmartInitQueue queue = SmartInitQueue.getQueue();
                if (queue != null) {
                    Date validTime = gun.getReplacementTimeRange().getStart();
                    TimeRange validPeriod = record.getDataTime()
                            .getValidPeriod();

                    statusHandler.info("D2D grid received for "
                            + record.getParameter().getAbbreviation() + "_"
                            + record.getLevel() + " at "
                            + (validPeriod.isValid() ? validPeriod
                                    : record.getDataTime().getValidTimeAsDate())
                            + "\nFiring smartInit for " + dbId + " at "
                            + validTime
                            + "\nGridUpdateNotification.replacementTimeRange = "
                            + gun.getReplacementTimeRange());

                    queue.queue(siteID, config, dbId, validTime, false,
                            SmartInitRecord.LIVE_SMART_INIT_PRIORITY);
                }
            }
        }

        // send notifications;
        try {
            SendNotifications.send(guns);
        } catch (Exception e) {
            statusHandler.error("Unable to send grib ingest notifications", e);
        }
    }

    /**
     * @param records
     */
    public void filterSatelliteRecords(List<SatelliteRecord> records) {

        DatabaseID dbId = D2DSatDatabase.getDbId(siteID);
        D2DSatDatabase db = (D2DSatDatabase) getDatabase(dbId, true);

        List<GridUpdateNotification> guns = new LinkedList<>();
        for (SatelliteRecord record : records) {
            GridUpdateNotification gun = db.update(record);
            if (gun != null) {
                guns.add(gun);

                // only fire smartInits if queue is instantiated
                SmartInitQueue queue = SmartInitQueue.getQueue();
                if (queue != null) {
                    Date validTime = gun.getReplacementTimeRange().getStart();
                    queue.queue(siteID, config, dbId, validTime, false,
                            SmartInitRecord.LIVE_SMART_INIT_PRIORITY);
                }
            }
        }

        try {
            SendNotifications.send(guns);
        } catch (Exception e) {
            statusHandler.error("Unable to send satellite ingest notifications",
                    e);
        }
    }

    private Date purgeTime(DatabaseID id) {
        int numHours = this.config.gridPurgeAgeInHours(id);

        if (numHours < 1) {
            // don't perform time based purge
            return null;
        }

        // calculate purge time based on present time
        return new Date(System.currentTimeMillis()
                - (numHours * TimeUtil.MILLIS_PER_HOUR));

    }

    private ServerResponse<GridDatabase> getOfficialDB(CommitGridRequest req) {
        ServerResponse<GridDatabase> sr = new ServerResponse<>();

        GridDatabase db = null;
        DatabaseID officialID = null;

        // Extract out the DatabaseID from the CommitGridRequest
        // Note that this database id is the source and not the destination
        DatabaseID requestID = null;
        if (req.isParmRequest()) {
            requestID = req.getParmId().getDbId();
        } else if (req.isDatabaseRequest()) {
            requestID = req.getDbId();
        }

        // find name of official database corresponding to the Commit Grid
        // Request
        for (DatabaseID off : this.config.getOfficialDatabases()) {
            // for a match, the siteid, type, and format must be the same
            if (requestID.getSiteId().equals(off.getSiteId())
                    && requestID.getDbType().equals(off.getDbType())
                    && requestID.getFormat().equals(off.getFormat())) {
                officialID = off;
                break;
            }
        }
        if (officialID == null) {
            sr.addMessage(
                    "No official database specified in config that matches request Req="
                            + req + " OfficialDBs: "
                            + config.getOfficialDatabases());
            sr.addMessage("Commit Grid Operation aborted");
            return sr;
        }

        // now look up the GridDatabase from the name
        db = this.dbMap.get(officialID);
        if (db == null) {
            sr.addMessage("Official Database [" + officialID
                    + "] does not exist." + "  Commit Grid Operation aborted");
        } else {
            sr.setPayload(db);
        }

        return sr;
    }

    private ServerResponse<List<CommitGridRequest>> convertToParmReq(
            List<CommitGridRequest> in) {
        ServerResponse<List<CommitGridRequest>> sr = new ServerResponse<>();
        List<CommitGridRequest> out = new ArrayList<>();

        for (CommitGridRequest req : in) {
            if (req.isParmRequest()) {
                out.add(req);

            } else if (req.isDatabaseRequest()) {

                // get the parm list for this database
                GridDatabase db = this.getDatabase(req.getDbId(), true);
                if (db != null) {
                    List<ParmID> parmList = db.getParmList().getPayload();
                    for (ParmID pid : parmList) {
                        out.add(new CommitGridRequest(pid, req.getTimeRange(),
                                req.isClientSendStatus()));
                    }
                } else {
                    sr.addMessage("Could not find database for " + req.getDbId()
                            + " in convertToParmReq()");
                }
            } else {
                sr.addMessage("Invalid Commit Grid Request: " + req
                        + " in convertToParmReq()");
                break;
            }
        }

        if (!sr.isOkay()) {
            sr.addMessage("convertToParmReq failure");
            out.clear();
        } else {
            sr.setPayload(out);
        }
        return sr;
    }

    private static ServerResponse<?> compareGridParmInfoForCommit(
            GridParmInfo source, GridParmInfo dest) {
        ServerResponse<?> sr = new ServerResponse<>();

        // TODO why is this commented out?

        // if (!source.getGridLoc().equals(dest.getGridLoc())
        // || source.isTimeIndependentParm() != dest
        // .isTimeIndependentParm()
        // || !source.getGridType().equals(dest.getGridType())
        // || !source.getUnitObject().equals(dest.getUnitObject())
        // || !source.getDescriptiveName().equals(
        // dest.getDescriptiveName())
        // || source.getMinValue() != dest.getMinValue()
        // || source.getMaxValue() != dest.getMaxValue()
        // || source.getPrecision() != dest.getPrecision()
        // || !source.getTimeConstraints().equals(
        // dest.getTimeConstraints())) {
        // sr.addMessage("GridParmInfo not compatible for commit operation: "
        // + " Source: " + source + " Destination: " + dest);
        //
        // }

        return sr;
    }

    private void createDbNotification(List<DatabaseID> additions,
            List<DatabaseID> deletions) {
        if (((additions != null) && !additions.isEmpty())
                || ((deletions != null) && !deletions.isEmpty())) {
            DBInvChangeNotification notify = new DBInvChangeNotification(
                    additions, deletions, siteID);
            SendNotifications.send(notify);
        }
    }

    /**
     * @param notif
     */
    public void handleGfeNotification(GfeNotification notif) {
        // Don't handle notifications sent by this JVM
        if (notif.isLocal()) {
            return;
        }

        if (notif instanceof DBInvChangeNotification) {
            DBInvChangeNotification invChanged = (DBInvChangeNotification) notif;

            // handle additions first to try to get old out-of-synch versions
            // are purged sooner
            for (DatabaseID dbId : invChanged.getAdditions()) {
                this.getDatabase(dbId, false);
            }

            for (DatabaseID dbId : invChanged.getDeletions()) {
                deallocateDb(dbId, false);
            }
        } else if (notif instanceof GridUpdateNotification) {
            GridUpdateNotification gun = (GridUpdateNotification) notif;
            DatabaseID dbid = gun.getParmId().getDbId();

            // get the database as an extra attempt to
            // keep the IFPServers in sync
            GridDatabase db = getDatabase(dbid, false);

            if (db instanceof D2DSatDatabase) {
                ((D2DSatDatabase) db).update(gun);
            }
        }
    }

    /**
     * @param db
     */
    private void addDB(GridDatabase db) {
        DatabaseID dbId = db.getDbId();
        statusHandler.info("addDB called, adding " + dbId);
        this.dbMap.put(dbId, db);
    }

    /**
     * Process D2D grid data purge notification
     */
    public void d2dGridDataPurged() {
        List<DatabaseID> currentInventory = new ArrayList<>(
                this.dbMap.keySet());
        List<DatabaseID> newInventory = new ArrayList<>(
                currentInventory.size());
        List<String> d2dModels = config.getD2dModels();
        for (String d2dModelName : d2dModels) {
            String gfeModel = config.gfeModelNameMapping(d2dModelName);
            if (gfeModel != null) {
                DatabaseID dbId = D2DGridDatabase.getDbId(d2dModelName,
                        new Date(), config);
                int desiredVersions = config.desiredDbVersions(dbId);
                try {
                    List<DatabaseID> dbIds = D2DGridDatabase
                            .getD2DDatabaseIdsFromDb(config, d2dModelName,
                                    desiredVersions);
                    newInventory.addAll(dbIds);
                } catch (DataAccessLayerException e) {
                    statusHandler.error(e.getLocalizedMessage(), e);
                }
            }
        }

        List<DatabaseID> deleted = new ArrayList<>(currentInventory);
        deleted.removeAll(newInventory);

        // don't delete NetCDF and satellite databases.
        deleted.removeAll(netCdfDbManager.getDatabaseIds());
        deleted.remove(D2DSatDatabase.getDbId(siteID));

        Iterator<DatabaseID> iter = deleted.iterator();
        while (iter.hasNext()) {
            DatabaseID dbid = iter.next();
            // don't delete non-D2D databases
            if (!"D2D".equals(dbid.getDbType())) {
                iter.remove();
            } else {
                // remove the database
                deallocateDb(dbid, false);
            }
        }

        if (!deleted.isEmpty()) {
            DBInvChangeNotification changed = new DBInvChangeNotification(null,
                    deleted, siteID);

            SendNotifications.send(changed);
        }
    }

    /**
     * Process D2D satellite data purge notification
     */
    public void d2dSatDataPurged() {
        DatabaseID dbId = D2DSatDatabase.getDbId(siteID);
        D2DSatDatabase db = (D2DSatDatabase) getDatabase(dbId);

        List<GridUpdateNotification> notifs = db.update();

        SendNotifications.send(notifs);
    }

    /**
     * Perform database purge based on versions for the given model
     *
     * @param modelToPurge
     *            DatabaseID for model to be purged
     *
     * @return list of purged databases
     */
    public List<DatabaseID> versionPurge(DatabaseID modelToPurge) {
        int desiredVersions = this.config.desiredDbVersions(modelToPurge);

        List<DatabaseID> currentInv = new ArrayList<>(this.dbMap.keySet());
        // sort the inventory by site, type, model, time (most recent first)
        Collections.sort(currentInv);

        // process the inventory looking for "old" unwanted databases
        List<DatabaseID> purged = new ArrayList<>();
        int count = 0;
        for (DatabaseID dbId : currentInv) {
            if (dbId.sameModel(modelToPurge)) {

                // process the id and determine whether it should be purged
                count++;
                if ((count > desiredVersions) && !dbId.getModelTime()
                        .equals(DatabaseID.NO_MODEL_TIME)) {
                    deallocateDb(dbId, true);
                    purged.add(dbId);
                    PurgeLogger.logInfo("Purging " + dbId, "gfe");
                }
            }
        }

        return purged;
    }
}
