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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.reference.ReferenceMgr;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
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
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 7, 2008              njensen     Initial creation
 * Jan 22, 2010  4248       njensen     Better error msgs
 * Jul 25, 2012  #957       dgilling    Implement getEditArea().
 * Apr 23, 2013  #1937      dgilling    Implement get().
 * Apr 23, 2013  #1941      dgilling    Implement put(), add methods to build
 *                                      Scalar/VectorGridSlices, refactor
 *                                      Discrete/WeatherGridSlices builders.
 * Jun 05, 2013  #2063      dgilling    Port history() from A1.
 * Jun 13, 2013  #2044      randerso    Refactored to use non-singleton 
 *                                      GridParmManager and LockManager
 * Nov 11, 2013  #2517      randerso    Changed put() to support multiple discontiguous saves
 *                                      Added getKeys(tr) to get grid times overlapping a time range
 *                                      Removed caching of inventory as it was not being updated when 
 *                                      grids were updated/deleted
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class IFPWE {

    /** The smart init user name */
    public static final String SMART_INIT_USER = "smartInit";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IFPWE.class);

    private final ParmID parmId;

    private final String siteId;

    private final GridParmManager gridParmMgr;

    private final LockManager lockMgr;

    private final GridParmInfo gpi;

    private final WsId wsId;

    /**
     * Constructor
     * 
     * @param parm
     * @param userName
     * @param gridParmMgr
     * @param lockMgr
     */
    public IFPWE(ParmID parm, String userName, GridParmManager gridParmMgr,
            LockManager lockMgr) {
        parmId = parm;
        siteId = parm.getDbId().getSiteId();
        this.gridParmMgr = gridParmMgr;
        this.lockMgr = lockMgr;

        gpi = gridParmMgr.getGridParmInfo(parmId).getPayload();
        wsId = new WsId(null, userName, "EDEX");
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
        ServerResponse<List<TimeRange>> sr = GridParmManager.getGridInventory(
                parmId, tr);
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
     * @return
     */
    public GridParmInfo getGpi() {
        return gpi;
    }

    /**
     * Returns a time range constrained by the grid parm info
     * 
     * @param time
     *            the time in seconds
     * @return
     */
    public TimeRange getTimeRange(long time) {
        return gpi.getTimeConstraints().constraintTime(new Date(time * 1000));
    }

    /**
     * Returns an IGridSlice of data for the specified time range
     * 
     * @param timeRange
     *            the time range to retrieve data for
     * @return
     * @throws GfeException
     */
    public IGridSlice getItem(TimeRange timeRange) throws GfeException {
        GetGridRequest req = new GetGridRequest();
        req.setParmId(parmId);
        GFERecord gfeRec = new GFERecord(parmId, timeRange);
        ArrayList<GFERecord> gfeList = new ArrayList<GFERecord>();
        gfeList.add(gfeRec);
        req.setRecords(gfeList);
        ArrayList<GetGridRequest> reqList = new ArrayList<GetGridRequest>();
        reqList.add(req);
        List<IGridSlice> data = new ArrayList<IGridSlice>();

        ServerResponse<List<IGridSlice>> ssr = gridParmMgr.getGridData(reqList);
        data = ssr.getPayload();

        IGridSlice slice = null;
        if ((data == null) || (data.size() == 0)) {
            String msg = "Error getting grid data for " + parmId.toString()
                    + " at time " + timeRange.toString();
            for (ServerMsg smsg : ssr.getMessages()) {
                msg += "\n" + smsg.getMessage();
            }
            throw new GfeException(msg);
        } else if (data.size() > 1) {
            // theoretically should never get here
            String msg = "Retrieved too much data for " + parmId.toString()
                    + "at time " + timeRange.toString();
            for (ServerMsg smsg : ssr.getMessages()) {
                msg += "\n" + smsg.getMessage();
            }
            throw new GfeException(msg);
        } else {
            slice = data.get(0);
        }

        return slice;
    }

    public List<Pair<IGridSlice, List<GridDataHistory>>> get(
            List<TimeRange> times, boolean histories) {
        GetGridRequest ggr = new GetGridRequest(parmId, times);
        ServerResponse<List<IGridSlice>> sr = gridParmMgr.getGridData(Arrays
                .asList(ggr));

        if (!sr.isOkay()) {
            String msg = "Could not retrieve grid data for parm [" + parmId
                    + "] for times " + times + ": " + sr.message();
            statusHandler.error(msg);
            return Collections.emptyList();
        }

        List<IGridSlice> data = sr.getPayload();
        List<Pair<IGridSlice, List<GridDataHistory>>> rval = new ArrayList<Pair<IGridSlice, List<GridDataHistory>>>(
                data.size());
        for (IGridSlice grid : data) {
            List<GridDataHistory> hists = Collections.emptyList();
            if (histories) {
                GridDataHistory[] h = grid.getHistory();
                hists = new ArrayList<GridDataHistory>(h.length);
                for (GridDataHistory entry : h) {
                    hists.add(entry);
                }
            }
            rval.add(new Pair<IGridSlice, List<GridDataHistory>>(grid, hists));
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
    public void put(LinkedHashMap<TimeRange, List<IGridSlice>> inventory)
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
                throw new GfeException("Request lock failed. "
                        + lockResponse.message());
            }

            List<IGridSlice> gridSlices = entry.getValue();
            List<GFERecord> records = new ArrayList<GFERecord>(
                    gridSlices.size());
            for (IGridSlice slice : gridSlices) {
                GFERecord rec = new GFERecord(parmId, slice.getValidTime());
                rec.setGridHistory(slice.getHistory());
                rec.setMessageData(slice);
                records.add(rec);
            }
            SaveGridRequest sgr = new SaveGridRequest(parmId, timeRangeSpan,
                    records);

            try {
            ServerResponse<?> sr = gridParmMgr.saveGridData(Arrays.asList(sgr),
                    wsId);
                if (sr.isOkay()) {
                    SendNotifications.send(sr.getNotifications());
                } else {
                    statusHandler.error("Unable to save grids for parm ["
                            + parmId + "] over time range " + timeRangeSpan
                            + ": " + sr.message());
                }
            } finally {
            ServerResponse<List<LockTable>> unLockResponse = lockMgr
                    .requestLockChange(new LockRequest(parmId, timeRangeSpan,
                            LockMode.UNLOCK), wsId);
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
        List<String> retVal = new ArrayList<String>(hist.size());
        for (GridDataHistory entry : hist) {
            retVal.add(entry.getCodedString());
        }
        return retVal;
    }

    private void setItem(TimeRange time, IGridSlice gridSlice,
            List<GridDataHistory> gdh) throws GfeException {
        GFERecord rec = new GFERecord(parmId, time);
        rec.setGridHistory(gdh.toArray(new GridDataHistory[] {}));
        List<GFERecord> records = new ArrayList<GFERecord>();
        if (gridSlice != null) {
            rec.setMessageData(gridSlice);
            records.add(rec);
        }

        SaveGridRequest req = new SaveGridRequest(parmId, time, records);
        req.setParmId(parmId);
        List<SaveGridRequest> reqList = new ArrayList<SaveGridRequest>();
        reqList.add(req);
        boolean combineLocks = this.wsId.getUserName().equals(SMART_INIT_USER);
        if (!combineLocks) {
            statusHandler.debug("Getting lock for ParmID: " + parmId + " TR: "
                    + req.getReplacementTimeRange());
        }
        ServerResponse<List<LockTable>> lockResponse = lockMgr
                .requestLockChange(
                        new LockRequest(req.getParmId(), req
                                .getReplacementTimeRange(), LockMode.LOCK),
                        wsId, combineLocks);

        if (!lockResponse.isOkay()) {
            throw new GfeException("Request lock failed. "
                    + lockResponse.message());
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
                    .requestLockChange(
                            new LockRequest(req.getParmId(), req
                                    .getReplacementTimeRange(), LockMode.UNLOCK),
                            wsId, combineLocks);
            if (!unLockResponse.isOkay()) {
                throw new GfeException("Request unlock failed. "
                        + unLockResponse.message());
            } else {
                statusHandler.debug("LOCKING: Unlocked for: " + wsId + " TR: "
                        + req.getReplacementTimeRange());
            }

        }
    }

    public void removeItem(TimeRange timeRange) throws GfeException {
        setItem(timeRange, null, new ArrayList<GridDataHistory>());
    }

    public void setItemScalar(TimeRange time, float[] scalarData)
            throws GfeException {
        Calendar lastSentTime = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));

        List<GridDataHistory> gdhList = new ArrayList<GridDataHistory>();
        gdhList.add(new GridDataHistory(GridDataHistory.OriginType.INITIALIZED,
                this.parmId, time, lastSentTime.getTime(), lastSentTime
                        .getTime()));
        setItemScalar(time, scalarData, gdhList);
    }

    public void setItemScalar(TimeRange time, float[] scalarData,
            List<GridDataHistory> gdhList) throws GfeException {
        IGridSlice gridSlice = new ScalarGridSlice(time, gpi,
                gdhList.toArray(new GridDataHistory[gdhList.size()]),
                new Grid2DFloat(gpi.getGridLoc().getNx(), gpi.getGridLoc()
                        .getNy(), scalarData));
        setItem(time, gridSlice, gdhList);
    }

    public void setItemVector(TimeRange time, float[] magData, float[] dirData)
            throws GfeException {
        Calendar lastSentTime = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));

        List<GridDataHistory> gdhList = new ArrayList<GridDataHistory>();
        gdhList.add(new GridDataHistory(GridDataHistory.OriginType.INITIALIZED,
                this.parmId, time, lastSentTime.getTime(), lastSentTime
                        .getTime()));
        setItemVector(time, magData, dirData, gdhList);
    }

    public void setItemVector(TimeRange time, float[] magData, float[] dirData,
            List<GridDataHistory> gdhList) throws GfeException {
        IGridSlice gridSlice = new VectorGridSlice(time, gpi,
                gdhList.toArray(new GridDataHistory[gdhList.size()]),
                new Grid2DFloat(gpi.getGridLoc().getNx(), gpi.getGridLoc()
                        .getNy(), magData), new Grid2DFloat(gpi.getGridLoc()
                        .getNx(), gpi.getGridLoc().getNy(), dirData));
        setItem(time, gridSlice, gdhList);
    }

    public void setItemDiscrete(TimeRange time, byte[] discreteData, String keys)
            throws GfeException {
        Date lastSentTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                .getTime();

        List<GridDataHistory> gdhList = new ArrayList<GridDataHistory>();
        gdhList.add(new GridDataHistory(GridDataHistory.OriginType.INITIALIZED,
                this.parmId, time, lastSentTime, lastSentTime));
        setItemDiscrete(time, discreteData, keys, gdhList);
    }

    public void setItemDiscrete(TimeRange time, byte[] discreteData,
            String keys, List<GridDataHistory> gdhList) throws GfeException {
        IGridSlice gridSlice = buildDiscreteSlice(time, discreteData, keys,
                gdhList);
        setItem(time, gridSlice, gdhList);
    }

    public void setItemWeather(TimeRange time, byte[] weatherData, String keys)
            throws GfeException {
        Date lastSentTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                .getTime();

        List<GridDataHistory> gdhList = new ArrayList<GridDataHistory>();
        gdhList.add(new GridDataHistory(GridDataHistory.OriginType.INITIALIZED,
                this.parmId, time, lastSentTime, lastSentTime));
        setItemWeather(time, weatherData, keys, gdhList);
    }

    public void setItemWeather(TimeRange time, byte[] weatherData, String keys,
            List<GridDataHistory> gdhList) throws GfeException {
        IGridSlice gridSlice = buildWeatherSlice(time, weatherData, keys,
                gdhList);
        setItem(time, gridSlice, gdhList);
    }

    /**
     * Returns the parm's type
     * 
     * @return
     */
    public String getGridType() {
        return gpi.getGridType().toString();
    }

    /**
     * Returns the minimum allowed value for the grid
     * 
     * @return
     */
    public float getMinAllowedValue() {
        return gpi.getMinValue();
    }

    /**
     * Returns the max allowed value for the grid
     * 
     * @return
     */
    public float getMaxAllowedValue() {
        return gpi.getMaxValue();
    }

    public String[] getDiscreteKeys() {
        List<String> keys = gpi.getDiscreteKeys();
        // returning an array allows us to access this return value as a normal
        // python list and not a PyJObject
        return keys.toArray(new String[keys.size()]);
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
        return new ScalarGridSlice(time, gpi, history, new Grid2DFloat(gpi
                .getGridLoc().getNx(), gpi.getGridLoc().getNy(), data));
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
        return new VectorGridSlice(time, gpi, history, new Grid2DFloat(gpi
                .getGridLoc().getNx(), gpi.getGridLoc().getNy(), magData),
                new Grid2DFloat(gpi.getGridLoc().getNx(), gpi.getGridLoc()
                        .getNy(), dirData));
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
     * @return
     */
    public DiscreteGridSlice buildDiscreteSlice(TimeRange time, byte[] bytes,
            String keyString, List<GridDataHistory> history) {
        List<DiscreteKey> discreteKeyList = new ArrayList<DiscreteKey>();
        List<String> keys = GfeUtil.discreteKeyStringToList(keyString);

        for (String k : keys) {
            discreteKeyList.add(new DiscreteKey(siteId, k, parmId));
        }
        return new DiscreteGridSlice(time, gpi, history, new Grid2DByte(gpi
                .getGridLoc().getNx(), gpi.getGridLoc().getNy(), bytes),
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
     * @return
     */
    public WeatherGridSlice buildWeatherSlice(TimeRange time, byte[] bytes,
            String keyString, List<GridDataHistory> history) {
        List<WeatherKey> weatherKeyList = new ArrayList<WeatherKey>();
        List<String> keys = GfeUtil.discreteKeyStringToList(keyString);
        for (String k : keys) {
            weatherKeyList.add(new WeatherKey(siteId, k));
        }
        return new WeatherGridSlice(time, gpi, history, new Grid2DByte(gpi
                .getGridLoc().getNx(), gpi.getGridLoc().getNy(), bytes),
                weatherKeyList);
    }

    @Override
    public String toString() {
        return "IFPWE " + parmId.toString();
    }

    public ParmID getParmid() {
        return parmId;
    }

    public Grid2DBit getEditArea(String name) {
        try {
            IFPServerConfig config = IFPServerConfigManager
                    .getServerConfig(siteId);
            ReferenceMgr refMgr = new ReferenceMgr(config);

            ServerResponse<List<ReferenceData>> sr = refMgr.getData(Arrays
                    .asList(new ReferenceID(name)));
            if (sr.isOkay()) {
                ReferenceData data = sr.getPayload().get(0);
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
