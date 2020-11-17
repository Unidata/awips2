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
package com.raytheon.edex.plugin.gfe.smartinit;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.RefType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceMgr;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SaveGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.Pair;

/**
 * IFP Weather Element, originally a C++ <--> python bridge, ported to Java
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 07, 2008           njensen   Initial creation
 * Jan 22, 2010  4248     njensen   Better error msgs
 * Jul 25, 2012  957      dgilling  Implement getEditArea().
 * Apr 23, 2013  1937     dgilling  Implement get().
 * Apr 23, 2013  1941     dgilling  Implement put(), add methods to build
 *                                  Scalar/VectorGridSlices, refactor
 *                                  Discrete/WeatherGridSlices builders.
 * Jun 05, 2013  2063     dgilling  Port history() from A1.
 * Jun 13, 2013  2044     randerso  Refactored to use non-singleton
 *                                  GridParmManager and LockManager
 * Nov 11, 2013  2517     randerso  Changed put() to support multiple
 *                                  discontiguous saves Added getKeys(tr) to get
 *                                  grid times overlapping a time range Removed
 *                                  caching of inventory as it was not being
 *                                  updated when grids were updated/deleted
 * Jul 01, 2014  3149     randerso  Changed to use updated GetGridRequest.
 *                                  Cleaned up code
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 * Jun 12, 2017  6298     mapeters  Update references to refactored ReferenceMgr
 * Jul 31, 2017  6342     randerso  Get ReferenceMgr from IFPServer. Code
 *                                  cleanup
 * Jan 15, 2018  6867     dgilling  Throw exception if GPI cannot be retrieved.
 *
 * </pre>
 *
 * @author njensen
 */

public class IFPWE {

    /** The smart init user name */
    public static final String SMART_INIT_USER = "smartInit";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IFPWE.class);

    private final ParmID parmId;

    private final String siteId;

    private final IFPServer ifpServer;

    private final GridParmManager gridParmMgr;

    private final LockManager lockMgr;

    private final GridParmInfo gpi;

    private final WsId wsId;

    /**
     * Constructor
     *
     * @param parmId
     * @param userName
     * @param ifpServer
     */
    public IFPWE(ParmID parmId, String userName, IFPServer ifpServer) {
        this.parmId = parmId;
        this.siteId = parmId.getDbId().getSiteId();
        this.ifpServer = ifpServer;
        this.gridParmMgr = ifpServer.getGridParmMgr();
        this.lockMgr = ifpServer.getLockMgr();
        this.wsId = new WsId(null, userName, "EDEX");

        ServerResponse<GridParmInfo> sr = this.gridParmMgr
                .getGridParmInfo(this.parmId);
        if (sr.isOkay()) {
            this.gpi = sr.getPayload();
        } else {
            throw new IllegalArgumentException(sr.message());
        }
    }

    /**
     * Returns all available times of data for the parm
     *
     * @return the time ranges of all available data for the parm
     */
    public List<TimeRange> getKeys() {
        List<TimeRange> availableTimes;
        ServerResponse<List<TimeRange>> sr = gridParmMgr
                .getGridInventory(parmId);
        if (sr.isOkay()) {
            availableTimes = sr.getPayload();
        } else {
            availableTimes = Collections.emptyList();
        }

        return availableTimes;
    }

    /**
     * Returns available times of data for the parm that overlap a time range
     *
     * @param tr
     *            the desired time range
     * @return the time ranges of data that overlap the desired time range
     */
    public List<TimeRange> getKeys(TimeRange tr) {
        List<TimeRange> overlappingTimes;
        ServerResponse<List<TimeRange>> sr = gridParmMgr
                .getGridInventory(parmId, tr);
        if (sr.isOkay()) {
            overlappingTimes = sr.getPayload();
        } else {
            overlappingTimes = Collections.emptyList();
        }
        return overlappingTimes;
    }

    /**
     * Returns the grid parm info
     *
     * @return the GridParmInfo
     */
    public GridParmInfo getGpi() {
        return gpi;
    }

    /**
     * Returns a time range constrained by the grid parm info
     *
     * @param time
     *            the time in seconds
     * @return the constrained TimeRange
     */
    public TimeRange getTimeRange(long time) {
        return gpi.getTimeConstraints().constraintTime(new Date(time * 1000));
    }

    /**
     * Returns an IGridSlice of data for the specified time range
     *
     * @param timeRange
     *            the time range to retrieve data for
     * @return the IGridSlice
     * @throws GfeException
     */
    public IGridSlice getItem(TimeRange timeRange) throws GfeException {
        GetGridRequest req = new GetGridRequest(parmId,
                Arrays.asList(timeRange));
        ArrayList<GetGridRequest> reqList = new ArrayList<>();
        reqList.add(req);

        ServerResponse<List<IGridSlice>> ssr = gridParmMgr.getGridData(reqList);
        if (!ssr.isOkay()) {
            String msg = "Error getting grid data for " + parmId.toString()
                    + " at time " + timeRange.toString() + ssr.message();
            throw new GfeException(msg);
        }

        IGridSlice slice = ssr.getPayload().get(0);
        return slice;
    }

    /**
     * Retrieves IGridSlices and optionally GridDataHistorys for a list of
     * TimeRanges
     *
     * @param times
     *            the desired TimeRanges
     * @param histories
     *            true if histories should be returned
     * @return list of pairs of IGridSlices and GridDataHistorys for the desired
     *         times
     */
    public List<Pair<IGridSlice, List<GridDataHistory>>> get(
            List<TimeRange> times, boolean histories) {
        GetGridRequest ggr = new GetGridRequest(parmId, times);
        ServerResponse<List<IGridSlice>> sr = gridParmMgr
                .getGridData(Arrays.asList(ggr));

        if (!sr.isOkay()) {
            String msg = "Could not retrieve grid data for parm [" + parmId
                    + "] for times " + times + ": " + sr.message();
            statusHandler.error(msg);
            return Collections.emptyList();
        }

        List<IGridSlice> data = sr.getPayload();
        List<Pair<IGridSlice, List<GridDataHistory>>> rval = new ArrayList<>(
                data.size());
        for (IGridSlice grid : data) {
            List<GridDataHistory> hists = Collections.emptyList();
            if (histories) {
                GridDataHistory[] h = grid.getHistory();
                hists = new ArrayList<>(h.length);
                for (GridDataHistory entry : h) {
                    hists.add(entry);
                }
            }
            rval.add(new Pair<>(grid, hists));
        }

        return rval;
    }

    /**
     * Stores the provided grid slices into this weather element's permanent
     * storage.
     *
     * @param inventory
     *            A Map of TimeRanges to List of IGridSlices. TimeRange is the
     *            replacement time range
     * @throws GfeException
     *             If an error occurs while trying to obtain a lock on the
     *             destination database.
     */
    public void put(Map<TimeRange, List<IGridSlice>> inventory)
            throws GfeException {

        for (Entry<TimeRange, List<IGridSlice>> entry : inventory.entrySet()) {
            TimeRange timeRangeSpan = entry.getKey();
            statusHandler.debug("Getting lock for ParmID: " + parmId + " TR: "
                    + timeRangeSpan);
            ServerResponse<List<LockTable>> lockResponse = lockMgr
                    .requestLockChange(new LockRequest(parmId, timeRangeSpan,
                            LockMode.LOCK), wsId);
            if (lockResponse.isOkay()) {
                statusHandler.debug("LOCKING: Lock granted for: " + wsId
                        + " for time range: " + timeRangeSpan);
            } else {
                statusHandler.error("Could not lock TimeRange " + timeRangeSpan
                        + " for parm [" + parmId + "]: "
                        + lockResponse.message());
                throw new GfeException(
                        "Request lock failed. " + lockResponse.message());
            }

            List<IGridSlice> gridSlices = entry.getValue();
            List<GFERecord> records = new ArrayList<>(gridSlices.size());
            for (IGridSlice slice : gridSlices) {
                GFERecord rec = new GFERecord(parmId, slice.getValidTime());
                rec.setGridHistory(slice.getHistory());
                rec.setMessageData(slice);
                records.add(rec);
            }
            SaveGridRequest sgr = new SaveGridRequest(parmId, timeRangeSpan,
                    records);

            try {
                ServerResponse<?> sr = gridParmMgr
                        .saveGridData(Arrays.asList(sgr), wsId);
                if (sr.isOkay()) {
                    SendNotifications.send(sr.getNotifications());
                } else {
                    statusHandler.error("Unable to save grids for parm ["
                            + parmId + "] over time range " + timeRangeSpan
                            + ": " + sr.message());
                }
            } finally {
                ServerResponse<List<LockTable>> unLockResponse = lockMgr
                        .requestLockChange(new LockRequest(parmId,
                                timeRangeSpan, LockMode.UNLOCK), wsId);
                if (unLockResponse.isOkay()) {
                    statusHandler.debug("LOCKING: Unlocked for: " + wsId
                            + " TR: " + timeRangeSpan);
                } else {
                    statusHandler.error("Could not unlock TimeRange "
                            + timeRangeSpan + " for parm [" + parmId + "]: "
                            + lockResponse.message());
                    throw new GfeException("Request unlock failed. "
                            + unLockResponse.message());
                }
            }
        }
    }

    /**
     * Returns the grid history for a specified time range.
     *
     * @param tr
     *            The time for which the history is being requested.
     * @return The grid history entries for the specified time range in coded
     *         string format.
     */
    public List<String> history(final TimeRange tr) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = gridParmMgr
                .getGridHistory(parmId, Arrays.asList(tr));

        if (!sr.isOkay()) {
            statusHandler.error("Error retrieving grid history for parm ["
                    + parmId + "] at time range " + tr + ": " + sr.message());
            return Collections.emptyList();
        }

        Map<TimeRange, List<GridDataHistory>> payload = sr.getPayload();
        if ((payload == null) || (payload.isEmpty())) {
            statusHandler.error("No grid history returned for parm [" + parmId
                    + "] at time range " + tr);
            return Collections.emptyList();
        }

        List<GridDataHistory> hist = payload.get(tr);
        List<String> retVal = new ArrayList<>(hist.size());
        for (GridDataHistory entry : hist) {
            retVal.add(entry.getCodedString());
        }
        return retVal;
    }

    private void setItem(TimeRange time, IGridSlice gridSlice,
            List<GridDataHistory> gdh) throws GfeException {
        GFERecord rec = new GFERecord(parmId, time);
        rec.setGridHistory(gdh);
        List<GFERecord> records = new ArrayList<>();
        if (gridSlice != null) {
            rec.setMessageData(gridSlice);
            records.add(rec);
        }

        SaveGridRequest req = new SaveGridRequest(parmId, time, records);
        req.setParmId(parmId);
        List<SaveGridRequest> reqList = new ArrayList<>();
        reqList.add(req);
        boolean combineLocks = this.wsId.getUserName().equals(SMART_INIT_USER);
        if (!combineLocks) {
            statusHandler.debug("Getting lock for ParmID: " + parmId + " TR: "
                    + req.getReplacementTimeRange());
        }
        ServerResponse<List<LockTable>> lockResponse = lockMgr
                .requestLockChange(
                        new LockRequest(req.getParmId(),
                                req.getReplacementTimeRange(), LockMode.LOCK),
                        wsId, combineLocks);

        if (!lockResponse.isOkay()) {
            throw new GfeException(
                    "Request lock failed. " + lockResponse.message());
        } else {
            statusHandler.debug("LOCKING: Lock granted for: " + wsId
                    + " for time range: " + req.getReplacementTimeRange());
        }
        try {
            ServerResponse<?> resp = gridParmMgr.saveGridData(reqList, wsId);
            if (resp.isOkay()) {
                try {
                    ServerResponse<?> notifyResponse = SendNotifications
                            .send(resp.getNotifications());
                    if (!notifyResponse.isOkay()) {
                        for (ServerMsg msg : notifyResponse.getMessages()) {
                            resp.addMessage(msg.getMessage());
                        }
                    }
                } catch (Exception e) {
                    statusHandler.error("Error sending save notification", e);
                    resp.addMessage("Error sending save notification - "
                            + e.getMessage());
                }
            } else {
                for (ServerMsg msg : resp.getMessages()) {
                    statusHandler.error(msg.getMessage());
                }
            }

        } finally {
            if (!combineLocks) {
                statusHandler.debug("Releasing lock for ParmID: " + parmId
                        + " TR: " + req.getReplacementTimeRange());
            }
            ServerResponse<List<LockTable>> unLockResponse = lockMgr
                    .requestLockChange(new LockRequest(req.getParmId(),
                            req.getReplacementTimeRange(), LockMode.UNLOCK),
                            wsId, combineLocks);
            if (!unLockResponse.isOkay()) {
                throw new GfeException(
                        "Request unlock failed. " + unLockResponse.message());
            } else {
                statusHandler.debug("LOCKING: Unlocked for: " + wsId + " TR: "
                        + req.getReplacementTimeRange());
            }

        }
    }

    /**
     * @param timeRange
     * @throws GfeException
     */
    public void removeItem(TimeRange timeRange) throws GfeException {
        setItem(timeRange, null, new ArrayList<GridDataHistory>());
    }

    /**
     * @param time
     * @param scalarData
     * @throws GfeException
     */
    public void setItemScalar(TimeRange time, float[] scalarData)
            throws GfeException {
        Calendar lastSentTime = Calendar
                .getInstance(TimeZone.getTimeZone("GMT"));

        List<GridDataHistory> gdhList = new ArrayList<>();
        gdhList.add(new GridDataHistory(GridDataHistory.OriginType.INITIALIZED,
                this.parmId, time, lastSentTime.getTime(),
                lastSentTime.getTime()));
        setItemScalar(time, scalarData, gdhList);
    }

    /**
     * @param time
     * @param scalarData
     * @param gdhList
     * @throws GfeException
     */
    public void setItemScalar(TimeRange time, float[] scalarData,
            List<GridDataHistory> gdhList) throws GfeException {
        IGridSlice gridSlice = new ScalarGridSlice(time, gpi, gdhList,
                new Grid2DFloat(gpi.getGridLoc().getNx(),
                        gpi.getGridLoc().getNy(), scalarData));
        setItem(time, gridSlice, gdhList);
    }

    /**
     * @param time
     * @param magData
     * @param dirData
     * @throws GfeException
     */
    public void setItemVector(TimeRange time, float[] magData, float[] dirData)
            throws GfeException {
        Calendar lastSentTime = Calendar
                .getInstance(TimeZone.getTimeZone("GMT"));

        List<GridDataHistory> gdhList = new ArrayList<>();
        gdhList.add(new GridDataHistory(GridDataHistory.OriginType.INITIALIZED,
                this.parmId, time, lastSentTime.getTime(),
                lastSentTime.getTime()));
        setItemVector(time, magData, dirData, gdhList);
    }

    /**
     * @param time
     * @param magData
     * @param dirData
     * @param gdhList
     * @throws GfeException
     */
    public void setItemVector(TimeRange time, float[] magData, float[] dirData,
            List<GridDataHistory> gdhList) throws GfeException {
        IGridSlice gridSlice = new VectorGridSlice(time, gpi, gdhList,
                new Grid2DFloat(gpi.getGridLoc().getNx(),
                        gpi.getGridLoc().getNy(), magData),
                new Grid2DFloat(gpi.getGridLoc().getNx(),
                        gpi.getGridLoc().getNy(), dirData));
        setItem(time, gridSlice, gdhList);
    }

    /**
     * @param time
     * @param discreteData
     * @param keys
     * @throws GfeException
     */
    public void setItemDiscrete(TimeRange time, byte[] discreteData,
            String keys) throws GfeException {
        Date lastSentTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                .getTime();

        List<GridDataHistory> gdhList = new ArrayList<>();
        gdhList.add(new GridDataHistory(GridDataHistory.OriginType.INITIALIZED,
                this.parmId, time, lastSentTime, lastSentTime));
        setItemDiscrete(time, discreteData, keys, gdhList);
    }

    /**
     * @param time
     * @param discreteData
     * @param keys
     * @param gdhList
     * @throws GfeException
     */
    public void setItemDiscrete(TimeRange time, byte[] discreteData,
            String keys, List<GridDataHistory> gdhList) throws GfeException {
        IGridSlice gridSlice = buildDiscreteSlice(time, discreteData, keys,
                gdhList);
        setItem(time, gridSlice, gdhList);
    }

    /**
     * @param time
     * @param weatherData
     * @param keys
     * @throws GfeException
     */
    public void setItemWeather(TimeRange time, byte[] weatherData, String keys)
            throws GfeException {
        Date lastSentTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                .getTime();

        List<GridDataHistory> gdhList = new ArrayList<>();
        gdhList.add(new GridDataHistory(GridDataHistory.OriginType.INITIALIZED,
                this.parmId, time, lastSentTime, lastSentTime));
        setItemWeather(time, weatherData, keys, gdhList);
    }

    /**
     * @param time
     * @param weatherData
     * @param keys
     * @param gdhList
     * @throws GfeException
     */
    public void setItemWeather(TimeRange time, byte[] weatherData, String keys,
            List<GridDataHistory> gdhList) throws GfeException {
        IGridSlice gridSlice = buildWeatherSlice(time, weatherData, keys,
                gdhList);
        setItem(time, gridSlice, gdhList);
    }

    /**
     * @return the parm's type as a string
     */
    public String getGridType() {
        return gpi.getGridType().toString();
    }

    /**
     * @return the minimum allowed value for the grid
     */
    public float getMinAllowedValue() {
        return gpi.getMinValue();
    }

    /**
     * @return the max allowed value for the grid
     */
    public float getMaxAllowedValue() {
        return gpi.getMaxValue();
    }

    /**
     * @return theDiscreteKeys for the parm
     */
    public List<String> getDiscreteKeys() {
        List<String> keys = gpi.getDiscreteKeys();
        return keys;
    }

    /**
     * Builds a ScalarGridSlice to store.
     *
     * @param time
     *            The valid time of the slice.
     * @param data
     *            A float array that corresponds to the slice's data.
     * @param history
     *            The GridDataHistory for the new slice.
     * @return A ScalarGridSlice based on the provided data, valid for the given
     *         time, with the provided history.
     */
    public ScalarGridSlice buildScalarSlice(TimeRange time, float[] data,
            List<GridDataHistory> history) {
        return new ScalarGridSlice(time, gpi, history, new Grid2DFloat(
                gpi.getGridLoc().getNx(), gpi.getGridLoc().getNy(), data));
    }

    /**
     * Builds a VectorGridSlice to store.
     *
     * @param time
     *            The valid time of the slice.
     * @param magData
     *            A float array that corresponds to the slice's magnitude data.
     * @param dirData
     *            A float array that corresponds to the slice's directional
     *            data.
     * @param history
     *            The GridDataHistory for the new slice.
     * @return A VectorGridSlice based on the provided data, valid for the given
     *         time, with the provided history.
     */
    public VectorGridSlice buildVectorSlice(TimeRange time, float[] magData,
            float[] dirData, List<GridDataHistory> history) {
        return new VectorGridSlice(time, gpi, history,
                new Grid2DFloat(gpi.getGridLoc().getNx(),
                        gpi.getGridLoc().getNy(), magData),
                new Grid2DFloat(gpi.getGridLoc().getNx(),
                        gpi.getGridLoc().getNy(), dirData));
    }

    /**
     * Builds a discrete grid slice to store
     *
     * @param time
     *            The valid time of the data.
     * @param bytes
     *            A byte[] corresponding to discrete
     * @param keyString
     *            Python encoded form of discrete keys.
     * @param history
     *            histories for this grid.
     * @return the discrete grid slice
     */
    public DiscreteGridSlice buildDiscreteSlice(TimeRange time, byte[] bytes,
            String keyString, List<GridDataHistory> history) {
        List<DiscreteKey> discreteKeyList = new ArrayList<>();
        List<String> keys = GfeUtil.discreteKeyStringToList(keyString);

        for (String k : keys) {
            discreteKeyList.add(new DiscreteKey(siteId, k, parmId));
        }
        return new DiscreteGridSlice(time, gpi, history,
                new Grid2DByte(gpi.getGridLoc().getNx(),
                        gpi.getGridLoc().getNy(), bytes),
                discreteKeyList);
    }

    /**
     * Builds a weather grid slice to store
     *
     * @param time
     *            The valid time of the data.
     * @param bytes
     *            A byte[] corresponding to weather
     * @param keyString
     *            Python encoded form of weather keys.
     * @param history
     *            histories for this grid.
     * @return the weather grid slice
     */
    public WeatherGridSlice buildWeatherSlice(TimeRange time, byte[] bytes,
            String keyString, List<GridDataHistory> history) {
        List<WeatherKey> weatherKeyList = new ArrayList<>();
        List<String> keys = GfeUtil.discreteKeyStringToList(keyString);
        for (String k : keys) {
            weatherKeyList.add(new WeatherKey(siteId, k));
        }
        return new WeatherGridSlice(time, gpi, history,
                new Grid2DByte(gpi.getGridLoc().getNx(),
                        gpi.getGridLoc().getNy(), bytes),
                weatherKeyList);
    }

    @Override
    public String toString() {
        return "IFPWE " + parmId.toString();
    }

    /**
     * @return the ParmID
     */
    public ParmID getParmid() {
        return parmId;
    }

    /**
     * Retrieve edit area by name
     *
     * @param name
     * @return the editArea as a bit mask
     */
    public Grid2DBit getEditArea(String name) {
        try {
            ReferenceMgr refMgr = ifpServer.getReferenceMgr();

            ServerResponse<ReferenceData> sr = refMgr
                    .getData(new ReferenceID(name));
            if (sr.isOkay()) {
                ReferenceData data = sr.getPayload();
                if (data.refType() != RefType.POLYGON) {
                    throw new Exception("Edit area is not a polygon");
                }
                return data.getGrid();
            } else {
                statusHandler.error("Unable to retrieve edit area [" + name
                        + "]: " + sr.message());
            }
        } catch (Exception e) {
            statusHandler.error("Unable to retrieve edit area [" + name + "].",
                    e);
        }

        return null;
    }
}
