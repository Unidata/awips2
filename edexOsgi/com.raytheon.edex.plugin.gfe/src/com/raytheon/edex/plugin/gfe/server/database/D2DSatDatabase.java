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
package com.raytheon.edex.plugin.gfe.server.database;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedSet;
import java.util.TreeSet;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.server.D2DSatParm;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Database implementation for satellite data in GFE
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2011            bphillip    Initial creation
 * May 04, 2012  #574      dgilling    Add unimplemented methods from GridDatabase.
 * Oct 10  2012  #1260     randerso    Added code to set valid flag
 * May 02  2013  #1969     randerso    Removed unnecessary updateDbs method
 * Jun 13  2013  #2044     randerso    Added getDbId and update methods
 * Nov 17  2015  #5129     dgilling    Ensure ServerResponse payload is always
 *                                     populated when calling getGridHistory.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class D2DSatDatabase extends VGridDatabase {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DSatDatabase.class);

    /**
     * Get the Satellite DatabaseID for a given site
     * 
     * @param siteID
     * @return the Satellite DatabaseID
     */
    public static DatabaseID getDbId(String siteID) {
        return new DatabaseID(siteID, DataType.GRID, "D2D", "Satellite",
                DatabaseID.NO_MODEL_TIME);
    }

    /** map of parmIDs to D2DSatParms */
    private Map<ParmID, D2DSatParm> pidToParm;

    /**
     * Map of product IDs to D2DSatParms<br>
     * <br>
     * A product ID consists of the sector ID and physical element of the
     * satellite product. <br>
     * <br>
     * <b>Examples:</b>
     * 
     * <pre>
     * "East CONUS/Imager Visible"
     * "East CONUS/Imager 11 micron IR"
     * "East CONUS/Imager 13 micron (IR)"
     * "East CONUS/Imager 3.9 micron IR"
     * </pre>
     * 
     */
    private Map<String, D2DSatParm> idToParm;

    /**
     * Creates a new D2DSatDatabase
     * 
     * @param config
     *            The server config for this site
     */
    public D2DSatDatabase(IFPServerConfig config) {
        super(config);
        String siteID = config.getSiteID().get(0);
        this.dbId = getDbId(siteID);
        this.valid = this.dbId.isValid();

        Map<String, String> satData = config.satData();

        pidToParm = new HashMap<ParmID, D2DSatParm>(satData.size(), 1.0f);
        idToParm = new HashMap<String, D2DSatParm>(satData.size(), 1.0f);
        for (Entry<String, String> entry : satData.entrySet()) {
            try {
                String productId = entry.getKey();
                String parmName = entry.getValue();
                D2DSatParm parm = new D2DSatParm(config, productId, this.dbId,
                        parmName);
                pidToParm.put(parm.pid(), parm);
                idToParm.put(productId, parm);
            } catch (GfeException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }
    }

    /**
     * Gets the satellite DatabaseID
     * 
     * @return The DatabaseID of the satellite database
     */
    public DatabaseID id() {
        return this.dbId;
    }

    @Override
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID id) {
        D2DSatParm p = pidToParm.get(id);
        if (p != null) {
            return p.getGridInventory();
        }
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        sr.addMessage("Parm not found: " + id);
        return sr;

    }

    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID id) {
        D2DSatParm p = pidToParm.get(id);
        if (p != null) {
            return p.getGridParmInfo();
        }
        ServerResponse<GridParmInfo> sr = new ServerResponse<GridParmInfo>();
        sr.addMessage("Parm not found: " + id);
        return sr;
    }

    @Override
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID id, List<TimeRange> trs) {
        D2DSatParm p = pidToParm.get(id);
        if (p != null) {
            return p.getGridHistory(trs);
        }
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();
        Map<TimeRange, List<GridDataHistory>> history = Collections.emptyMap();
        sr.setPayload(history);
        sr.addMessage("Parm not found: " + id);
        return sr;
    }

    @Override
    public ServerResponse<List<ParmID>> getParmList() {
        ServerResponse<List<ParmID>> retVal = new ServerResponse<List<ParmID>>();
        List<ParmID> parmIDs = new ArrayList<ParmID>();
        for (ParmID pid : pidToParm.keySet()) {
            parmIDs.add(pid);
        }
        retVal.setPayload(parmIDs);
        return retVal;
    }

    @Override
    public ServerResponse<?> saveGridData(ParmID id,
            TimeRange originalTimeRange, List<GFERecord> records,
            WsId requesterId) {
        ServerResponse<?> sr = new ServerResponse<String>();
        sr.addMessage("Not implemented for D2D Sat databases.");
        return sr;
    }

    @Override
    public ServerResponse<List<IGridSlice>> getGridData(ParmID id,
            List<TimeRange> timeRanges) {

        D2DSatParm p = pidToParm.get(id);
        if (p != null) {
            return p.getGridData(new GetGridRequest(id, timeRanges), null);
        }
        ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();
        sr.addMessage("Parm not found: " + id);
        return sr;
    }

    @Override
    public String getProjectionId() {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.VGridDatabase#getValidTimes
     * ()
     */
    @Override
    public SortedSet<Date> getValidTimes() throws GfeException,
            DataAccessLayerException {
        SortedSet<Date> times = new TreeSet<Date>();
        for (D2DSatParm parm : pidToParm.values()) {
            for (TimeRange tr : parm.getGridInventory().getPayload()) {
                times.add(tr.getStart());
            }
        }

        return times;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.gfe.server.database.GridDatabase#deleteDb()
     */
    @Override
    public void deleteDb() {
        // no-op
    }

    /**
     * Update with newly ingested data
     * 
     * @param record
     * @return GridUpdateNotification or null if none
     */
    public GridUpdateNotification update(SatelliteRecord record) {
        GridUpdateNotification notify = null;

        String productId = record.getSectorID() + "/"
                + record.getPhysicalElement();
        D2DSatParm satParm = idToParm.get(productId);
        if (satParm != null) {
            notify = satParm.update(record);
        }
        return notify;
    }

    /**
     * Update inventory from database after satellite purge
     * 
     * @return list of GridUpdateNotifications to be sent
     */
    public List<GridUpdateNotification> update() {
        List<GridUpdateNotification> notifs = new ArrayList<GridUpdateNotification>();

        for (D2DSatParm parm : pidToParm.values()) {
            notifs.addAll(parm.updateFromDb());
        }

        return notifs;
    }

    /**
     * Update parm inventory based on GridUpdateNotification
     * 
     * @param gun
     *            the GridUpdateNotification
     */
    public void update(GridUpdateNotification gun) {
        D2DSatParm parm = pidToParm.get(gun.getParmId());
        parm.update(gun);
    }

}
