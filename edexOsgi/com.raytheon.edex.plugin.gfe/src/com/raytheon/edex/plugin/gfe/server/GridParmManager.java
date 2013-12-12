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
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.cache.d2dparms.D2DParmIdCache;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.db.dao.GFEDao;
import com.raytheon.edex.plugin.gfe.server.database.D2DGridDatabase;
import com.raytheon.edex.plugin.gfe.server.database.D2DSatDatabase;
import com.raytheon.edex.plugin.gfe.server.database.D2DSatDatabaseManager;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase;
import com.raytheon.edex.plugin.gfe.server.database.NetCDFDatabaseManager;
import com.raytheon.edex.plugin.gfe.server.database.TopoDatabaseManager;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
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
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.purge.PurgeLogger;

/**
 * Class used to manage grid parms
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 07/09/09     #2590      njensen     Changed from singleton to static
 * 07/12/12     15162      ryu         added check for invalid db
 * 10/10/12     #1260      randerso    Added exception handling for domain not 
 *                                     overlapping the dataset
 * 02/10/13     #1603      randerso    General code cleanup, improved purge logging,
 *                                     fixed a purge inefficiency,
 *                                     fixed error which caused D2D purging to remove 
 *                                     smartInit hdf5 data
 * 03/07/13     #1773      njensen     Logged commitGrid() times
 * 03/15/13     #1795      njensen     Sped up commitGrid()
 * 03/20/13     #1774      randerso    Removed dead method, changed to use new 
 *                                     D2DGridDatabase constructor
 * 04/23/13     #1949      rjpeter     Added inventory retrieval for a given time range.
 * 05/02/13     #1969      randerso    Fixed possible null pointer in getParmList
 *                                     Removed inventory from DBInvChangedNotification
 * 05/03/13     #1974      randerso    Fixed error logging to include stack trace
 * 05/14/13     #2004      randerso    Added methods to synch GridParmManager across JVMs
 * 09/12/13     #2348      randerso    Added logging when database are added/removed from dbMap
 *                                     Fixed the synchronization of dbMap with the database inventory
 *                                     Changed to call D2DGridDatabase.getDatabase instead of calling 
 *                                     the constructor directly to ensure the data exists before creating
 *                                     the D2DGridDatabase object
 * 10/02/13     #2444      randerso    Fix error handling when creating IFPGridDatabases. 
 *                                     DO NOT ATTEMPT TO MERGE THIS CHANGE INTO 14.2 as the GFE
 *                                     server code has been significantly refactored.
 * 12/03/13     #2595      randerso    Added check for null update time in commitGrid
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class GridParmManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridParmManager.class);

    private static final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    /** The data access object for retrieving GFE grid records */
    private static GFEDao gfeDao;

    /** The logger */
    private static transient Log logger = LogFactory
            .getLog(GridParmManager.class);

    private static Map<DatabaseID, GridDatabase> dbMap = new ConcurrentHashMap<DatabaseID, GridDatabase>();

    static {
        try {
            gfeDao = (GFEDao) PluginFactory.getInstance().getPluginDao("gfe");
        } catch (PluginException e) {
            logger.error("Unable to get gfe dao", e);
        }
    }

    /**
     * Creates a new GridParm based on a ParmID
     * 
     * @param id
     *            The parmID for the new GridParm
     * @return A grid parm
     * @throws GfeException
     *             If problems occur while accessing the IFPServer config
     */
    private static GridParm gridParm(ParmID id) throws GfeException {
        if (id.getDbId().getModelName().equals("Satellite")) {
            D2DSatDatabase satDb = (D2DSatDatabase) getDb(id.getDbId());
            return satDb.findParm(id);

        }
        return new GridParm(id, getDb(id.getDbId()));
    }

    /**
     * Returns the grid inventory through "trs" for the parmId. Returns the
     * status. Zeros trs. Calls gridParm() to look up the parameter. If not
     * found, returns the appropriate error. Calls the grid parm's
     * getGridInventory() to obtain the inventory.
     * 
     * @param parmId
     *            The parmID to get the inventory for
     * @param trs
     *            The resulting time ranges
     * @return The server response
     */
    public static ServerResponse<List<TimeRange>> getGridInventory(ParmID parmId) {

        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        try {
            GridParm gp = gridParm(parmId);
            if (gp.isValid()) {
                sr = gp.getGridInventory();
            } else {
                sr.addMessage("Unknown Parm: " + parmId
                        + " in getGridInventory()");
            }
        } catch (Exception e) {
            sr.addMessage("Unknown Parm: " + parmId + " in getGridInventory()");
            logger.error("Unknown Parm: " + parmId + " in getGridInventory()",
                    e);
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
    public static ServerResponse<List<TimeRange>> getGridInventory(
            ParmID parmId, TimeRange timeRange) {

        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        try {
            GridParm gp = gridParm(parmId);
            if (gp.isValid()) {
                sr = gp.getGridInventory(timeRange);
            } else {
                sr.addMessage("Unknown Parm: " + parmId
                        + " in getGridInventory()");
            }
        } catch (Exception e) {
            sr.addMessage("Unknown Parm: " + parmId + " in getGridInventory()");
            logger.error("Unknown Parm: " + parmId + " in getGridInventory()",
                    e);
        }

        return sr;
    }

    /**
     * Returns the grid history through "history" for the parmId and specified
     * grids. Returns the status.
     * 
     * Zeros trs. Calls gridParm() to look up the parameter. If not found,
     * returns the appropriate error. Calls the grid parm's getGridHistory() to
     * obtain the histories.
     * 
     * @param parmId
     *            The parmID to get the history for
     * @param trs
     *            The time ranges to get the history for
     * @return The server response
     */
    public static ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID parmId, List<TimeRange> trs) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();

        try {
            GridParm gp = gridParm(parmId);
            if (gp.isValid()) {
                sr = gp.getGridHistory(trs);
            } else {
                sr.addMessage("Unknown Parm: " + parmId
                        + " in getGridInventory()");
            }
        } catch (Exception e) {
            sr.addMessage("Unknown Parm: " + parmId + " in getGridInventory()");
            logger.error("Unknown Parm: " + parmId + " in getGridInventory()",
                    e);
        }

        return sr;
    }

    /**
     * * Returns the grid parameter information through "info" for the parmId.
     * Returns the status. Calls gridParm() to look up the parameter. If not
     * found, resets the GridParmInfo and logs the appropriate error. If found,
     * calls the grid parm's getGridParmInfo().
     * 
     * @param parmId
     *            The parmID for which to get GridParmInfo for
     * @param info
     *            The grid parm info
     * @return The server status
     */
    public static ServerResponse<GridParmInfo> getGridParmInfo(ParmID parmId) {

        ServerResponse<GridParmInfo> sr = new ServerResponse<GridParmInfo>();

        try {
            GridParm gp = gridParm(parmId);

            if (gp.isValid()) {
                sr = gp.getGridParmInfo();
            } else {
                sr.addMessage("Unknown Parm: " + parmId
                        + " in getGridParmInfo()");
            }
        } catch (Exception e) {
            sr.addMessage("Unknown Parm: " + parmId + " in getGridParmInfo()");
            logger.error("Unknown Parm: " + parmId + " in getGridParmInfo()", e);
        }
        return sr;
    }

    /**
     * Request to save grid data. The data is in the saveRequest. The changes
     * are returned as notifications through "changes". Returns the status.
     * 
     * Zeros "changes". Loop through each save request and do the following: 1)
     * get the gridParm() for the request, 2) call the GridParm's saveGridData,
     * 3) call dataTime() on the saveRequest and create a GridUpdateNotification
     * and append it to changes.
     * 
     * @param saveRequest
     *            The save requests
     * @param requestorId
     *            The workstation ID of the requester
     * @param changes
     *            The update notifications
     * @return The server status
     */
    public static ServerResponse<?> saveGridData(
            List<SaveGridRequest> saveRequest, WsId requestorId, String siteID) {

        ServerResponse<List<GridUpdateNotification>> sr = new ServerResponse<List<GridUpdateNotification>>();

        // process each request
        for (SaveGridRequest req : saveRequest) {
            ServerResponse<?> ssr = null;
            GridParm gp = null;
            try {
                gp = gridParm(req.getParmId());
                if (!gp.isValid()) {
                    sr.addMessage("Unknown Parm: " + req.getParmId()
                            + " in saveGridData()");
                    logger.error("Unknown Parm: " + req.getParmId()
                            + " in saveGridData()");
                    continue;
                }
            } catch (GfeException e1) {
                sr.addMessage("Unknown Parm: " + req.getParmId()
                        + " in saveGridData()");
                logger.error("Unknown Parm: " + req.getParmId()
                        + " in saveGridData()", e1);
                continue;
            }

            // save the data
            ssr = gp.saveGridData(req, requestorId, siteID);
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
     * Request for grid data. The data is returned through "data".
     * 
     * Zeroes "data". Loop through each getRequest. For each entry, do the
     * following: 1) get the gridParm(), 2) call the grid parm's getGridData, 3)
     * append the set of returned GridSlices to the returned argument. In case
     * of error, zero data. The changes should always be zero length, unless an
     * invalid grid is retrieved from the database; the GridParm class handles
     * filling up the changes entry.
     * 
     * @param getRequest
     *            The get data requests
     * @param data
     *            The returned data
     * @param changes
     *            The grid update notifications
     * @return The server response
     */
    public static ServerResponse<List<IGridSlice>> getGridData(
            List<GetGridRequest> getRequest) {

        ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();
        List<TimeRange> badDataTimes = new ArrayList<TimeRange>();
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
                logger.error("Unknown Parm: " + req.getParmId()
                        + " in getGridData()", e);
            }
        }

        return sr;
    }

    /**
     * * Request to commit data to the official database. The changes are
     * returned through the calling argument "changes".
     * 
     * Zeros changes. Find the GridDatabase* for the official database.
     * 
     * CommitGridRequests can be of type parm, or of type database. This routine
     * converts all database-type requests to individual parm-type requests.
     * 
     * Loop through each commit request and do the following: 1) get the
     * gridParm*, 2) get the GridParm's inventory, 3) determine the set of time
     * ranges for the grids that overlap but are contained within the commit
     * request, 4) get the grid data from the source, 5) verify that the
     * GridParmInfo for the source and destination are matched (not exactly
     * equal though), 6) modify the retrieved DataSlices GridDataHistory and
     * parmID to match the destination, 7) call the official GridDatabase's
     * saveGridData(), 8) assemble a GridUpdateNotification and append it to
     * changes.
     * 
     * @param request
     *            The commit grid request
     * @param requestorId
     *            The workstation ID of the requester
     * @param changes
     *            The grid update notifications
     * @return The server response
     */
    public static ServerResponse<List<GridUpdateNotification>> commitGrid(
            List<CommitGridRequest> request, WsId requestorId,
            List<GridUpdateNotification> changes, String siteID) {

        ServerResponse<List<GridUpdateNotification>> sr = new ServerResponse<List<GridUpdateNotification>>();
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

        logger.info("Publish/Commit Grids Request: " + parmReq);
        List<CommitGridRequest> failures = new ArrayList<CommitGridRequest>();

        ITimer inventoryTimer = TimeUtil.getTimer();
        ITimer retrieveTimer = TimeUtil.getTimer();
        ITimer historyRetrieveTimer = TimeUtil.getTimer();
        ITimer historyUpdateTimer = TimeUtil.getTimer();
        ITimer storeTimer = TimeUtil.getTimer();

        // process each request
        ServerResponse<?> srDetailed = new ServerResponse<String>();
        for (int r = 0; r < parmReq.size(); r++) {
            CommitGridRequest req = parmReq.get(r);
            ServerResponse<?> ssr = new ServerResponse<String>();
            TimeRange publishTime = req.getTimeRange();

            // for the source data
            GridParm sourceGP = null;
            try {
                sourceGP = gridParm(req.getParmId());
                if (!sourceGP.isValid()) {
                    ssr.addMessage("Unknown Source Parm: " + req.getParmId()
                            + " in commitGrid()");
                    srDetailed.addMessages(ssr);
                    failures.add(req);
                    continue;
                }
            } catch (GfeException e) {
                ssr.addMessage("Unknown Source Parm: " + req.getParmId()
                        + " in commitGrid()");
                srDetailed.addMessages(ssr);
                failures.add(req);
                continue;
            }

            // for the destination data
            ParmID destParmId = new ParmID(req.getParmId().getParmName(),
                    officialDBid, req.getParmId().getParmLevel());
            GridParm destGP = null;
            try {
                destGP = gridParm(destParmId);
                if (!destGP.isValid()) {
                    ssr.addMessage("Unknown Destination Parm: " + destGP
                            + " in commitGrid()");
                    srDetailed.addMessages(ssr);
                    failures.add(req);
                    continue;
                }
            } catch (GfeException e) {
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
                ssr.addMessage("GetGridParmInfo for source/dest, or compare for commitGrid() failure: "
                        + ssr.message());
                srDetailed.addMessages(ssr);
                failures.add(req);
                continue;
            }

            // TODO: No need to get inventory and then compare for history
            // times, just request the history times directly

            // get the source data inventory
            inventoryTimer.start();
            ServerResponse<List<TimeRange>> invSr = sourceGP.getGridInventory();
            List<TimeRange> inventory = invSr.getPayload();
            ssr.addMessages(invSr);
            if (!ssr.isOkay()) {
                ssr.addMessage("GetGridInventory for source for commitGrid() failure: "
                        + ssr.message());
                srDetailed.addMessages(ssr);
                failures.add(req);
            }

            // get the destination data inventory
            invSr = destGP.getGridInventory();
            inventoryTimer.stop();
            List<TimeRange> destInventory = invSr.getPayload();
            ssr.addMessages(invSr);
            if (!ssr.isOkay()) {
                ssr.addMessage("GetGridInventory for destination for commitGrid() failure: "
                        + ssr.message());
                srDetailed.addMessages(ssr);
                failures.add(req);
                continue;
            }

            // determine set of grids that overlap the commit grid request
            List<TimeRange> overlapInventory = new ArrayList<TimeRange>();
            for (TimeRange invTime : inventory) {
                if (invTime.overlaps(publishTime)) {
                    overlapInventory.add(invTime);
                } else if (invTime.getStart().after(publishTime.getEnd())) {
                    break;
                }
            }

            // get the source grid data
            List<IGridSlice> sourceData = null;
            List<TimeRange> badGridTR = new ArrayList<TimeRange>();

            // System.out.println("overlapInventory initial size "
            // + overlapInventory.size());

            historyRetrieveTimer.start();
            ServerResponse<Map<TimeRange, List<GridDataHistory>>> history = sourceGP
                    .getGridHistory(overlapInventory);
            Map<TimeRange, List<GridDataHistory>> currentDestHistory = destGP
                    .getGridHistory(overlapInventory).getPayload();
            historyRetrieveTimer.stop();

            Map<TimeRange, List<GridDataHistory>> historyOnly = new HashMap<TimeRange, List<GridDataHistory>>();
            for (TimeRange tr : history.getPayload().keySet()) {
                // should only ever be one history for source grids
                List<GridDataHistory> gdhList = history.getPayload().get(tr);
                boolean doPublish = false;
                for (GridDataHistory gdh : gdhList) {
                    // if update time is less than publish time, grid has not
                    // changed since last published, therefore only update
                    // history, do not publish
                    if ((gdh.getUpdateTime() == null)
                            || (gdh.getPublishTime() == null)
                            || (gdh.getUpdateTime().getTime() > gdh
                                    .getPublishTime().getTime())
                            // in service backup, times on srcHistory could
                            // appear as not needing a publish, even though
                            // dest data does not exist
                            || (currentDestHistory.get(tr) == null)
                            || (currentDestHistory.get(tr).size() == 0)) {
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
                ssr.addMessage("GetGridData for source for commitGrid() failure: "
                        + ssr.message());
                srDetailed.addMessages(ssr);
                failures.add(req);
                continue;
            }

            // get list of official grids that overlap publish range and
            // aren't contained in the publish range, these have to be
            // included in the publish step. Then get the grids, shorten
            // and insert into sourceData.
            List<IGridSlice> officialData = new ArrayList<IGridSlice>();
            List<TimeRange> officialTR = new ArrayList<TimeRange>();
            for (int t = 0; t < destInventory.size(); t++) {
                if (destInventory.get(t).overlaps(publishTime)
                        && !req.getTimeRange().contains(destInventory.get(t))) {
                    officialTR.add(destInventory.get(t));
                }
                if (destInventory.get(t).getStart().after(publishTime.getEnd())) {
                    break;
                }
            }
            if (!officialTR.isEmpty()) {
                retrieveTimer.start();
                getSr = destGP.getGridData(new GetGridRequest(destParmId,
                        officialTR), badGridTR);
                retrieveTimer.stop();
                officialData = getSr.getPayload();
                ssr.addMessages(getSr);
                if (!ssr.isOkay()) {
                    ssr.addMessage("GetGridData for official for commidtGrid() failure: "
                            + ssr.message());
                    srDetailed.addMessages(ssr);
                    failures.add(req);
                    continue;
                }

                // insert the grid into the "sourceGrid" list
                for (int t = 0; t < officialTR.size(); t++) {
                    // before
                    try {
                        if (officialTR.get(t).getStart()
                                .before(publishTime.getStart())) {

                            IGridSlice tempSlice = officialData.get(t).clone();
                            tempSlice
                                    .setValidTime(new TimeRange(officialTR.get(
                                            t).getStart(), publishTime
                                            .getStart()));
                            sourceData.add(0, tempSlice);
                            publishTime.setStart(officialTR.get(t).getStart());
                            overlapInventory.add(tempSlice.getValidTime());
                        }

                        // after
                        if (officialTR.get(t).getEnd()
                                .after(publishTime.getEnd())) {
                            IGridSlice tempSlice = officialData.get(t).clone();
                            tempSlice.setValidTime(new TimeRange(publishTime
                                    .getEnd(), officialTR.get(t).getEnd()));
                            sourceData.add(tempSlice);
                            publishTime.setEnd(officialTR.get(t).getEnd());
                            overlapInventory.add(tempSlice.getValidTime());
                        }
                    } catch (CloneNotSupportedException e) {
                        sr.addMessage("Error cloning GridSlice "
                                + e.getMessage());
                    }
                }

                // adjust publishTime
                // publishTime = publishTime.combineWith(new
                // TimeRange(officialTR
                // .get(0).getStart(), officialTR.get(
                // officialTR.size() - 1).getEnd()));
            }

            // save off the source grid history, to update the source database
            // modify the source grid data for the dest ParmID and
            // GridDataHistory

            Map<TimeRange, List<GridDataHistory>> histories = new HashMap<TimeRange, List<GridDataHistory>>();
            Date nowTime = new Date();

            for (IGridSlice slice : sourceData) {
                GridDataHistory[] sliceHist = slice.getHistory();
                for (GridDataHistory hist : sliceHist) {
                    hist.setPublishTime((Date) nowTime.clone());
                }
                slice.getGridInfo().resetParmID(destParmId);
                histories.put(slice.getValidTime(), Arrays.asList(sliceHist));
            }

            // update the history for publish time for grids that are unchanged
            for (TimeRange tr : historyOnly.keySet()) {
                List<GridDataHistory> histList = historyOnly.get(tr);
                for (GridDataHistory hist : histList) {
                    hist.setPublishTime((Date) nowTime.clone());
                }
                histories.put(tr, histList);
            }

            // update the publish times in the source database, update the
            // notifications
            historyUpdateTimer.start();
            sr.addMessages(sourceGP.updatePublishTime(histories.values(),
                    (Date) nowTime.clone()));
            // System.out.println("Updated " + histories.size() + " histories");
            historyUpdateTimer.stop();

            changes.add(new GridUpdateNotification(req.getParmId(), req
                    .getTimeRange(), histories, requestorId, siteID));

            // update the histories of destination database for ones that
            // are not going to be saved since there hasn't been a change
            List<TimeRange> historyOnlyList = new ArrayList<TimeRange>();
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

            // only need to update the publish time on the destination histories
            // of grids that are not being saved (due to no changes), because
            // the saveGridSlices() call below will update the publish time
            // of the ones with changes
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
            // System.out.println("Published " + sourceData.size() + " slices");
            if (!ssr.isOkay()) {
                ssr.addMessage("SaveGridData for official for commitGrid() failure: "
                        + ssr.message());
                srDetailed.addMessages(ssr);
                failures.add(req);
                continue;
            }

            // make the notification
            GridUpdateNotification not = new GridUpdateNotification(destParmId,
                    publishTime, histories, requestorId, siteID);
            changes.add(not);
            sr.getPayload().add(not);
        }

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
                StringBuffer sb = new StringBuffer();
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
     * Returns the database inventory in "databases".
     * 
     * Zeros databases. Sequentially goes through _dbs and extracts out the
     * database ids. Returns them. Note that no errors can occur from this
     * routine although a ServerResponse is returned.
     * 
     * @param databases
     *            The database inventory
     * @return The server response
     */
    public static ServerResponse<List<DatabaseID>> getDbInventory(String siteID) {
        ServerResponse<List<DatabaseID>> sr = new ServerResponse<List<DatabaseID>>();
        List<DatabaseID> databases = new ArrayList<DatabaseID>();
        List<DatabaseID> gfeDbs = null;
        List<DatabaseID> singletons = null;
        List<DatabaseID> d2dDbs = null;

        try {
            gfeDbs = gfeDao.getDatabaseInventory(siteID);
        } catch (DataAccessLayerException e) {
            sr.addMessage("Unable to get IFP databases for site: " + siteID);
            logger.error("Unable to get IFP databases for site: " + siteID, e);
            return sr;
        }

        d2dDbs = D2DParmIdCache.getInstance().getDatabaseIDs();

        try {
            singletons = IFPServerConfigManager.getServerConfig(siteID)
                    .getSingletonDatabases();
        } catch (GfeException e) {
            sr.addMessage("Unable to get singleton databases");
            logger.error("Unable to get singleton databases", e);
            return sr;
        }

        if (singletons != null) {
            for (DatabaseID singleton : singletons) {
                if (singleton.getSiteId().equals(siteID)) {
                    databases.add(singleton);
                }
            }
        }

        for (DatabaseID dbId : gfeDbs) {
            if (!databases.contains(dbId)) {
                databases.add(dbId);
            }
        }

        if (d2dDbs != null) {
            for (DatabaseID d2d : d2dDbs) {
                if (d2d.getSiteId().equals(siteID)) {
                    databases.add(d2d);
                }
            }
        }

        DatabaseID topoDbId = TopoDatabaseManager.getTopoDbId(siteID);
        databases.add(topoDbId);
        databases.addAll(NetCDFDatabaseManager.getDatabaseIds(siteID));
        sr.setPayload(databases);
        return sr;
    }

    /**
     * Command to create a new database. This functions as a no-op if the
     * database already exists. A user cannot create a new database which is the
     * singleton type -- since that assumes there is no model time.
     * 
     * Checks if "id" is a Grid-type database. Checks to see if the database
     * already exists. If found, then returns okay. If not found, then calls
     * createDB() to handle the creation.
     * 
     * @param id
     *            The database ID of the database to create
     * @return The server response
     */
    public static ServerResponse<?> createNewDb(DatabaseID id) {
        ServerResponse<?> sr = new ServerResponse<String>();
        if (!id.getFormat().equals(DatabaseID.DataType.GRID)) {
            sr.addMessage("Invalid database id for createNewDb(): " + id);
            return sr;
        }

        // Check if this is a singleton database
        DatabaseID idWOTime = id.stripModelTime();
        List<DatabaseID> dbIds;
        try {
            dbIds = IFPServerConfigManager.getServerConfig(id.getSiteId())
                    .getSingletonDatabases();
        } catch (GfeException e) {
            sr.addMessage("Error retrieving singleton databases from IFPServerConfig");
            logger.error(
                    "Error retrieving singleton databases from IFPServerConfig",
                    e);
            return sr;
        }
        if (dbIds.contains(idWOTime)) {
            sr.addMessage("Cannot create database " + id
                    + ". It is a singleton database [" + idWOTime);
            return sr;
        }

        List<DatabaseID> inv = getDbInventory(id.getSiteId()).getPayload();
        try {
            createDB(id);
            if (!inv.contains(id)) {
                createDbNotification(id.getSiteId(),
                        Arrays.asList(new DatabaseID[] { id }),
                        new ArrayList<DatabaseID>());
            }
        } catch (GfeException e) {
            sr.addMessage("Unable to create database: " + id);
            logger.error("Unable to create database: " + id, e);
            return sr;
        }

        return sr;
    }

    public static ServerResponse<?> deleteDb(DatabaseID id) {
        ServerResponse<?> sr = new ServerResponse<String>();

        List<DatabaseID> inv = getDbInventory(id.getSiteId()).getPayload();
        if (!inv.contains(id)) {
            statusHandler.handle(Priority.PROBLEM, "Cannot delete database "
                    + id + ". It does not exist");
            sr.addMessage("Cannot delete database " + id
                    + ". It does not exist");
            return sr;
        }

        DatabaseID idWOTime = id.stripModelTime();
        IFPServerConfig config = null;
        try {
            config = IFPServerConfigManager.getServerConfig(id.getSiteId());
        } catch (GfeException e) {
            sr.addMessage("Error retrieving serverconfig for " + id.getSiteId());
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving serverconfig for " + id.getSiteId(), e);
            return sr;
        }

        if (config.getSingletonDatabases().contains(idWOTime)) {
            sr.addMessage("Cannot delete database " + id
                    + ". It is a singleton database");
            statusHandler.handle(Priority.PROBLEM, "Cannot delete database "
                    + id + ". It is a singleton database");
            return sr;
        }

        if (config.getOfficialDatabases().contains(idWOTime)) {
            sr.addMessage("Cannot delete database " + id
                    + ". It is an official database");
            statusHandler.handle(Priority.PROBLEM, "Cannot delete database "
                    + id + ". It is an official database");
            return sr;
        }

        deallocateDb(id, true);
        return sr;
    }

    /**
     * Returns the parameter list for the given database.
     * 
     * Zeros parmList. Looks up the database using "id". Asks the GridDatabase
     * for the parm list.
     * 
     * @param id
     *            The databases ID to get the parameter list for
     * @return The server response
     */
    public static ServerResponse<List<ParmID>> getParmList(DatabaseID id) {
        ServerResponse<List<ParmID>> sr = new ServerResponse<List<ParmID>>();
        try {
            GridDatabase db = getDb(id);
            if (db != null) {
                sr = db.getParmList();
            } else {
                sr.addMessage("Database " + id
                        + " does not exist for getParmList()");
            }
        } catch (Exception e) {
            sr.addMessage("Error getting db: " + id);
            logger.error("Error getting db: " + id, e);
        }
        return sr;
    }

    /**
     * Performs a database version purge. Returns the status.
     * 
     * Gets the current database inventory. Sorts it. Uses the server config to
     * get the number of databases required for each category (e.g., Model) and
     * determine which ones to delete. Calls deallocateDB() with the deleteFlag
     * set to true for each db to delete.
     * 
     * @return The server response
     */
    public static ServerResponse<?> versionPurge(String siteID) {

        ServerResponse<List<DatabaseID>> sr = new ServerResponse<List<DatabaseID>>();
        sr = getDbInventory(siteID);
        if (!sr.isOkay()) {
            sr.addMessage("VersionPurge failed - couldn't get inventory");
            return sr;
        }
        List<DatabaseID> currentInv = sr.getPayload();

        // sort the inventory by site, type, model, time (most recent first)
        Collections.sort(currentInv);

        // process the inventory looking for "old" unwanted databases
        String model = null;
        String site = null;
        String type = null;
        int count = 0;
        int desiredVersions = 0;
        for (DatabaseID dbId : currentInv) {
            // new series?
            if (!dbId.getSiteId().equals(site)
                    || !dbId.getDbType().equals(type)
                    || !dbId.getModelName().equals(model)) {
                site = dbId.getSiteId();
                type = dbId.getDbType();
                model = dbId.getModelName();
                count = 0;

                // determine desired number of versions
                try {
                    desiredVersions = IFPServerConfigManager.getServerConfig(
                            siteID).desiredDbVersions(dbId);
                } catch (GfeException e) {
                    logger.error("Error retreiving serverConfig", e);
                }
            }

            // process the id and determine whether it should be purged
            count++;
            if ((count > desiredVersions)
                    && !dbId.getModelTime().equals(DatabaseID.NO_MODEL_TIME)) {
                deallocateDb(dbId, true);
                PurgeLogger.logInfo("Purging " + dbId, "gfe");
            }
        }

        List<DatabaseID> newInv = getDbInventory(siteID).getPayload();
        List<DatabaseID> additions = new ArrayList<DatabaseID>(newInv);
        additions.removeAll(currentInv);

        List<DatabaseID> deletions = new ArrayList<DatabaseID>(currentInv);
        deletions.removeAll(newInv);

        // kludge to keep dbMap in synch until GridParmManager/D2DParmICache
        // merge/refactor
        List<DatabaseID> toRemove = new ArrayList<DatabaseID>(dbMap.keySet());
        toRemove.removeAll(newInv);
        for (DatabaseID dbId : toRemove) {
            statusHandler
                    .info("Synching GridParmManager with database inventory, removing "
                            + dbId);
            dbMap.remove(dbId);

            // add any removals to the deletions list
            // so notifications go to the other JVMs
            if (!deletions.contains(dbId)) {
                deletions.add(dbId);
            }
        }

        createDbNotification(siteID, additions, deletions);

        return sr;
    }

    /**
     * Command to perform grid purging. Returns the status.
     * 
     * Zeroes the notifications. Go sequentially through the _parms sequence and
     * get the purge configuration value from the config file for the database
     * by calling purgeTime(), call the GridParm's timePurge() and append its
     * returned notifications to this functions notifications.
     * 
     * @param gridNotifications
     *            The grid changed notifications
     * @param lockNotifications
     *            The lock notifications
     * @return The server response
     */
    public static ServerResponse<?> gridsPurge(
            List<GridUpdateNotification> gridNotifications,
            List<LockNotification> lockNotifications, String siteID) {

        ServerResponse<List<DatabaseID>> sr = new ServerResponse<List<DatabaseID>>();
        sr = getDbInventory(siteID);

        if (!sr.isOkay()) {
            sr.addMessage("GridsPurge failed - couldn't get inventory");
            return sr;
        }

        List<DatabaseID> databases = sr.getPayload();

        for (DatabaseID dbId : databases) {
            if (dbId.getDbType().equals("D2D")) {
                continue;
            }

            Date purgeTime = purgeTime(dbId);
            if (purgeTime == null) {
                continue;
            }

            List<ParmID> parmIds = new ArrayList<ParmID>();
            ServerResponse<List<ParmID>> ssr = getParmList(dbId);
            sr.addMessages(ssr);
            if (!ssr.isOkay()) {
                continue;
            }

            parmIds = ssr.getPayload();

            int purgedCount = 0;
            for (ParmID parmId : parmIds) {
                List<GridUpdateNotification> gridNotify = new ArrayList<GridUpdateNotification>();
                List<LockNotification> lockNotify = new ArrayList<LockNotification>();
                GridParm gp = null;
                try {
                    gp = gridParm(parmId);
                } catch (GfeException e) {
                    sr.addMessage("Error getting parm for: " + parmId);
                    logger.error("Error getting parm for: " + parmId, e);
                    continue;
                }
                ServerResponse<Integer> sr1 = gp.timePurge(purgeTime,
                        gridNotify, lockNotify, siteID);
                sr.addMessages(sr1);
                purgedCount += sr1.getPayload();

                gridNotifications.addAll(gridNotify);
                lockNotifications.addAll(lockNotify);
            }

            PurgeLogger.logInfo("Purge " + purgedCount + " items from " + dbId,
                    "gfe");
        }

        return sr;
    }

    public static Date purgeTime(DatabaseID id) {
        int numHours = 0;

        try {
            numHours = IFPServerConfigManager.getServerConfig(id.getSiteId())
                    .gridPurgeAgeInHours(id);
        } catch (GfeException e) {
            logger.error("Error calculating purge time", e);
        }

        if (numHours < 1) {
            return null; // don't perform time based purge
        }

        // calculate purge time based on present time
        return new Date(System.currentTimeMillis()
                - (numHours * Util.MILLI_PER_HOUR));

    }

    /**
     * Creates a new database with the given databaseID.<br>
     * This method retrieves the configuration for this database from the
     * IFPServerConfig and creates an hdf5 file
     * 
     * @param dbId
     *            The database to create
     */
    private static GridDatabase createDB(DatabaseID dbId) throws GfeException {

        /*
         * Validate the database ID. Throws an exception if the database ID is
         * invalid
         */
        if (!dbId.isValid() || (dbId.getFormat() != DatabaseID.DataType.GRID)) {
            throw new GfeException(
                    "Database id "
                            + dbId
                            + " is not valid, or is not a grid-type. Cannot create database.");
        }

        /*
         * Create the database (create the hdf5 file)
         */
        GridDatabase db = getDb(dbId);

        if (!db.databaseIsValid()) {
            throw new GfeException("Database invalid with id: " + dbId);
        }
        return db;
    }

    /**
     * Utility method to get the correct type of database
     * 
     * @param dbId
     *            The database ID of the database to retrieve
     * @return The Grid Database or null if database not available
     * @throws GfeException
     */
    public static GridDatabase getDb(DatabaseID dbId) throws GfeException {
        GridDatabase db = dbMap.get(dbId);
        if (db == null) {
            String dbType = dbId.getDbType();
            String siteId = dbId.getSiteId();
            String modelName = dbId.getModelName();
            if ("D2D".equals(dbType)) {
                if (modelName.equals("Satellite")) {
                    db = D2DSatDatabaseManager.getSatDatabase(dbId.getSiteId());

                } else {
                    db = NetCDFDatabaseManager.getDb(dbId);
                }
                if (db == null) {
                    IFPServerConfig serverConfig = IFPServerConfigManager
                            .getServerConfig(siteId);
                    try {
                        // this is still necessary on other JVMs from where
                        // ingested
                        String d2dModelName = serverConfig
                                .d2dModelNameMapping(modelName);
                        db = D2DGridDatabase.getDatabase(serverConfig,
                                d2dModelName, dbId.getModelTimeAsDate());
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                        db = null;
                    }
                }
            } else {
                // Check for topo type
                String topoModel = TopoDatabaseManager.getTopoDbId(siteId)
                        .getModelName();
                if (topoModel.equals(modelName)) {
                    db = TopoDatabaseManager.getTopoDatabase(dbId.getSiteId());

                } else {
                    try {
                        db = new IFPGridDatabase(dbId);
                        if (db.databaseIsValid()) {
                            ((IFPGridDatabase) db).updateDbs();
                        }
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error creating IFPGridDatbase for " + dbId, e);
                        db = null;
                    }
                }
            }

            if ((db != null) && db.databaseIsValid()) {
                statusHandler.info("getDb called, adding " + dbId);
                dbMap.put(dbId, db);
            }
        }
        return db;
    }

    public static void purgeDbCache(String siteID) {
        Iterator<DatabaseID> iter = dbMap.keySet().iterator();
        while (iter.hasNext()) {
            DatabaseID dbId = iter.next();
            if (dbId.getSiteId().equals(siteID)) {
                statusHandler.info("purgeDbCache(" + siteID + "), removing "
                        + dbId);
                iter.remove();
            }
        }
    }

    private static ServerResponse<GridDatabase> getOfficialDB(
            CommitGridRequest req) {
        ServerResponse<GridDatabase> sr = new ServerResponse<GridDatabase>();

        GridDatabase officialDBPtr = null;
        DatabaseID officialID = new DatabaseID();
        DatabaseID requestID = null;
        if (req.isParmRequest()) {
            requestID = req.getParmId().getDbId();
        } else if (req.isDatabaseRequest()) {
            requestID = req.getDbId();
        }

        // find name of official database corresponding to the Commit Grid
        // Request
        IFPServerConfig config = null;
        try {
            config = IFPServerConfigManager.getServerConfig(requestID
                    .getSiteId());
        } catch (GfeException e) {
            sr.addMessage("Unable to IFPServerConfig Instance");
            logger.error("Unable to IFPServerConfig Instance", e);
            return sr;
        }
        for (int i = 0; i < config.getOfficialDatabases().size(); i++) {
            DatabaseID off = config.getOfficialDatabases().get(i);
            // for a match, the siteid, type, and format must be the same
            if (requestID.getSiteId().equals(off.getSiteId())
                    && requestID.getDbType().equals(off.getDbType())
                    && requestID.getFormat().equals(off.getFormat())) {
                officialID = off;
                break;
            }
        }
        if (officialID.equals(new DatabaseID())) {
            sr.addMessage("No official database specified in config that matches request Req="
                    + req + " OfficialDBs: " + config.getOfficialDatabases());
            sr.addMessage("Commit Grid Operation aborted");
            return sr;
        }

        try {
            officialDBPtr = getDb(officialID);
        } catch (GfeException e) {
            sr.addMessage("Unable to create database: " + officialID);
            logger.error("Unable to create database: " + officialID, e);
            return sr;
        }
        sr.setPayload(officialDBPtr);

        return sr;
    }

    private static ServerResponse<List<CommitGridRequest>> convertToParmReq(
            List<CommitGridRequest> in) {
        ServerResponse<List<CommitGridRequest>> sr = new ServerResponse<List<CommitGridRequest>>();
        List<CommitGridRequest> out = new ArrayList<CommitGridRequest>();

        for (int i = 0; i < in.size(); i++) {
            if (in.get(i).isParmRequest()) {
                out.add(in.get(i));
            } else if (in.get(i).isDatabaseRequest()) {

                // get the parm list for this database
                List<ParmID> parmList = getParmList(in.get(0).getDbId())
                        .getPayload();
                for (int p = 0; p < parmList.size(); p++) {
                    out.add(new CommitGridRequest(parmList.get(0), in.get(i)
                            .getTimeRange(), in.get(i).isClientSendStatus()));
                }
            } else {
                sr.addMessage("Invalid Commit Grid Request: " + in.get(i)
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
        ServerResponse<?> sr = new ServerResponse<String>();

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

    private static void createDbNotification(String siteID,
            List<DatabaseID> additions, List<DatabaseID> deletions) {
        if (!additions.isEmpty() || !deletions.isEmpty()) {
            DBInvChangeNotification notify = new DBInvChangeNotification(
                    additions, deletions, siteID);
            SendNotifications.send(notify);
        }
    }

    private static void deallocateDb(DatabaseID id, boolean deleteFile) {
        if (deleteFile) {
            try {
                getDb(id).deleteDb();
            } catch (GfeException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to purge model database: " + id, e);
            }
        }
        statusHandler.info("deallocateDb called, removing " + id);
        dbMap.remove(id);
    }

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
        if (notif instanceof DBInvChangeNotification) {
            DBInvChangeNotification invChanged = (DBInvChangeNotification) notif;

            for (DatabaseID dbId : invChanged.getAdditions()) {
                try {
                    getDb(dbId);
                } catch (GfeException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

            for (DatabaseID dbId : invChanged.getDeletions()) {
                statusHandler
                        .info("DBInvChangeNotification deletion received, removing "
                                + dbId);
                dbMap.remove(dbId);
            }
        }
    }
}
