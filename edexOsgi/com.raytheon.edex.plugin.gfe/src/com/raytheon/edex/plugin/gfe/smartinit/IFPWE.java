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
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.lock.LockManager;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
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

/**
 * IFP Weather Element, originally a C++ <--> python bridge, ported to Java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 7, 2008				njensen	    Initial creation
 * Jan 22, 2010      4248   njensen      Better error msgs
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class IFPWE {

    public static final String SMART_INIT_USER = "smartInit";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IFPWE.class);

    private ParmID parmId;

    private String siteId;

    private GridParmInfo gpi;

    private List<TimeRange> availableTimes;

    private WsId wsId;

    /**
     * Constructor
     * 
     * @param parm
     *            the parm the IFPWE corresponds to
     * @throws GfeException
     */
    public IFPWE(ParmID parm) {
        this(parm, SMART_INIT_USER);
    }

    public IFPWE(ParmID parm, String userName) {
        parmId = parm;
        siteId = parm.getDbId().getSiteId();
        gpi = GridParmManager.getGridParmInfo(parmId).getPayload();
        wsId = new WsId(null, userName, "EDEX");
    }

    /**
     * Returns the available times of data for the parm
     * 
     * @return
     */
    public List<TimeRange> getKeys() {
        if (availableTimes == null) {
            availableTimes = new ArrayList<TimeRange>();
            List<TimeRange> times = GridParmManager.getGridInventory(parmId)
                    .getPayload();
            if (times != null) {
                Collections.sort(times);
                availableTimes.addAll(times);
            }
        }
        return availableTimes;
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

        ServerResponse<List<IGridSlice>> ssr = GridParmManager
                .getGridData(reqList);
        data = ssr.getPayload();

        IGridSlice slice = null;
        if (data == null || data.size() == 0) {
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
        String siteID = parmId.getDbId().getSiteId();
        boolean combineLocks = this.wsId.getUserName().equals(SMART_INIT_USER);
        if (!combineLocks) {
            statusHandler.debug("Getting lock for ParmID: " + parmId + " TR: "
                    + req.getReplacementTimeRange());
        }
        ServerResponse<List<LockTable>> lockResponse = LockManager
                .getInstance().requestLockChange(
                        new LockRequest(req.getParmId(),
                                req.getReplacementTimeRange(), LockMode.LOCK),
                        wsId, siteID, combineLocks);

        if (!lockResponse.isOkay()) {
            throw new GfeException("Request lock failed. "
                    + lockResponse.message());
        } else {
            statusHandler.debug("LOCKING: Lock granted for: " + wsId
                    + " for time range: " + req.getReplacementTimeRange());
        }
        try {
            ServerResponse<?> resp = GridParmManager.saveGridData(reqList,
                    wsId, siteID);
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
            ServerResponse<List<LockTable>> unLockResponse = LockManager
                    .getInstance().requestLockChange(
                            new LockRequest(req.getParmId(),
                                    req.getReplacementTimeRange(),
                                    LockMode.UNLOCK), wsId, siteID,
                            combineLocks);
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
                gpi.getGridType());
        gridSlice
                .setHistory(gdhList.toArray(new GridDataHistory[gdhList.size()]));
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
                gpi.getGridType());
        gridSlice
                .setHistory(gdhList.toArray(new GridDataHistory[gdhList.size()]));
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
     * Builds a discrete grid slice to store
     * 
     * @param time
     *            the time of the data
     * @param slice
     *            an Object[] { byte[], String } corresponding to discrete/wx
     *            types
     * @param type
     *            the type of the data
     * @return
     * @throws GfeException
     */
    private IGridSlice buildDiscreteSlice(TimeRange time, byte[] bytes,
            String keyString, GridType type) throws GfeException {
        List<DiscreteKey> discreteKeyList = new ArrayList<DiscreteKey>();
        List<String> keys = GfeUtil.discreteKeyStringToList(keyString);

        for (String k : keys) {
            discreteKeyList.add(new DiscreteKey(siteId, k, parmId));
        }
        return new DiscreteGridSlice(
                time,
                gpi,
                new GridDataHistory[] {},
                new Grid2DByte(gpi.getGridLoc().getNx(), gpi.getGridLoc()
                        .getNy(), bytes),
                discreteKeyList.toArray(new DiscreteKey[discreteKeyList.size()]));
    }

    /**
     * Builds a weather grid slice to store
     * 
     * @param time
     *            the time of the data
     * @param slice
     *            an Object[] { byte[], String } corresponding to weather/wx
     *            types
     * @param type
     *            the type of the data
     * @return
     * @throws GfeException
     */
    private IGridSlice buildWeatherSlice(TimeRange time, byte[] bytes,
            String keyString, GridType type) throws GfeException {
        List<WeatherKey> weatherKeyList = new ArrayList<WeatherKey>();
        List<String> keys = GfeUtil.discreteKeyStringToList(keyString);
        for (String k : keys) {
            weatherKeyList.add(new WeatherKey(siteId, k));
        }
        return new WeatherGridSlice(time, gpi, new GridDataHistory[] {},
                new Grid2DByte(gpi.getGridLoc().getNx(), gpi.getGridLoc()
                        .getNy(), bytes),
                weatherKeyList.toArray(new WeatherKey[weatherKeyList.size()]));
    }

    @Override
    public String toString() {
        return "IFPWE " + parmId.toString();
    }

    public ParmID getParmid() {
        return parmId;
    }
}
