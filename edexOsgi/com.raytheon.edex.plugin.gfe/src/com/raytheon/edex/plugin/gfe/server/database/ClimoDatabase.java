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

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.uf.common.climo.ClimoQuery;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;

/**
 * A complete encapsulation of the Climo data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 19, 2008		#1160	randerso	Initial creation
 * Jul 10, 2009        #2590    njensen      Support for multiple sites
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ClimoDatabase extends IFPGridDatabase {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClimoDatabase.class);

    private Set<ParmID> parmIds;

    private Map<ParmID, GridParmInfo> gpiMap;

    private GridLocation gloc;

    protected ClimoDatabase(String siteID, String source) throws GfeException {
        this.dbId = ClimoDatabaseManager.getClimoDbId(siteID, source);
        this.gridConfig = null;

        this.gloc = IFPServerConfigManager.getServerConfig(siteID).dbDomain();

        ParmID mntParm = new ParmID("mnt", dbId, "SFC");
        ParmID mxtParm = new ParmID("mxt", dbId, "SFC");
        ParmID tpParm = new ParmID("tp", dbId, "SFC");
        parmIds = new TreeSet<ParmID>();
        parmIds.add(mntParm);
        parmIds.add(mxtParm);
        parmIds.add(tpParm);

        gpiMap = new HashMap<ParmID, GridParmInfo>(8);
        GridParmInfo gpi;

        gpi = new GridParmInfo(mntParm, gloc, GridType.SCALAR, "K",
                "Minimum Temperature", 0.0f, 10000.0f, 1, false,
                new TimeConstraints(3600, 3600, 0), false);
        gpiMap.put(mntParm, gpi);

        gpi = new GridParmInfo(mxtParm, gloc, GridType.SCALAR, "K",
                "Maximum Temperature", 0.0f, 10000.0f, 1, false,
                new TimeConstraints(3600, 3600, 0), false);
        gpiMap.put(mxtParm, gpi);

        gpi = new GridParmInfo(tpParm, gloc, GridType.SCALAR, "mm",
                "Total Precipitation", 0.0f, 10000.0f, 1, false,
                new TimeConstraints(3600, 3600, 0), false);
        gpiMap.put(tpParm, gpi);

        for (ParmID parmId : parmIds) {
            try {
                IDataStore dataStore = getDataStore(parmId);
                IDataRecord dr = dataStore.retrieve(GRID_PARM_INFO_GRP,
                        parmId.getCompositeName(), Request.ALL);

                gpi = populateGpi(dr.getDataAttributes());
                if (!gpi.getGridLoc().equals(gloc)) {
                    dataStore.delete(GRID_PARM_INFO_GRP + "/"
                            + parmId.getCompositeName());
                    dataStore.delete(GfeUtil.getHDF5Group(parmId));
                    throw new Exception("Grid spacing has changed");
                }
            } catch (Exception e) {
                String s = e.getMessage();
                String message;
                Priority priority;
                Exception ee = null;
                if (s.contains(this.dbId + ".h5 does not exist")) {
                    statusHandler.handle(Priority.INFO, this.dbId
                            + ".h5 does not exist, creating...");
                    priority = Priority.INFO;
                    message = "No data found for " + parmId + ", creating...";
                } else if (s.contains("No group " + GRID_PARM_INFO_GRP
                        + " found")) {
                    priority = Priority.INFO;
                    message = "No data found for " + parmId + ", creating...";
                } else if (s.contains("Name doesn't exist")) {
                    priority = Priority.INFO;
                    message = "No data found for " + parmId + ", creating...";
                } else if (s.contains("Grid spacing has changed")) {
                    priority = Priority.INFO;
                    message = "Grid spacing has changed, re-initializaing "
                            + parmId + "...";
                } else {
                    priority = Priority.PROBLEM;
                    message = "Problem accessing " + parmId
                            + " re-initializing...";
                    ee = e;
                }
                statusHandler.handle(priority, message, ee);

                ServerResponse<?> sr = saveGridData(parmId,
                        TimeRange.allTimes(), new ArrayList<GFERecord>(),
                        new WsId(null, "initialization", "ClimoDatabase"));
                if (!sr.isOkay()) {
                    StringBuilder tmp = new StringBuilder(200);
                    tmp.append("Error occurred saving climo parm:");
                    for (ServerMsg msg : sr.getMessages()) {
                        tmp.append("\n");
                        tmp.append(msg.getMessage());
                    }
                    statusHandler.handle(Priority.ERROR, tmp.toString());
                }
            }
        }
    }

    @Override
    public void updateDbs() {
        // no op
    }

    /**
     * @see com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#getGridParmInfo(com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID)
     */
    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID id) {
        ServerResponse<GridParmInfo> sr = new ServerResponse<GridParmInfo>();
        GridParmInfo gpi = gpiMap.get(id);
        if (gpi == null) {
            String msg = "Parm '" + (id == null ? id : id.getParmId())
                    + "' is not in the Climo database";
            sr.addMessage(msg);
        } else {
            // Give users a copy so they can't alter the one in the map
            gpi = gpi.clone();
            sr.setPayload(gpi);
        }
        return sr;
    }

    /**
     * @see com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#getParmList()
     */
    @Override
    public ServerResponse<List<ParmID>> getParmList() {
        // Copy the parm ids so user can't alter the originals
        List<ParmID> rtnParmIds = new ArrayList<ParmID>();
        for (ParmID parmId : parmIds) {
            ParmID pidCopy = new ParmID(parmId.getParmName(), parmId.getDbId(),
                    parmId.getParmLevel());
            rtnParmIds.add(pidCopy);
        }
        ServerResponse<List<ParmID>> sr = new ServerResponse<List<ParmID>>();

        sr.setPayload(rtnParmIds);
        return sr;
    }

    /**
     * 
     * @see com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#dbSizeStats
     *      ()
     */
    @Override
    public void dbSizeStats() {
        // do nothing
    }

    /**
     * This method always throws an UnsupportedOperationException.
     * 
     * @see com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#deleteDb()
     * @throws UnsupportedOperationException
     */
    @Override
    public void deleteDb() {
        throw new UnsupportedOperationException("Cannot delete Climo databases");
    }

    /**
     * This method is a no-op.
     * 
     * @see com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#dumpStatistics
     *      ()
     */
    @Override
    public void dumpStatistics() {
        // do nothing
    }

    /**
     * 
     * @see com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#getGridHistory
     *      (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID,
     *      java.util.List)
     */
    @Override
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID id, List<TimeRange> timeRanges) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();

        if (parmIds.contains(id)) {
            ServerResponse<List<TimeRange>> invsr = getGridInventory(id);
            if (invsr.isOkay()) {
                Map<TimeRange, List<GridDataHistory>> histMap = new HashMap<TimeRange, List<GridDataHistory>>();
                List<TimeRange> inventory = invsr.getPayload();
                for (TimeRange tr : timeRanges) {
                    if (inventory.contains(tr)) {
                        GridDataHistory gdh = new GridDataHistory(
                                OriginType.INITIALIZED, id, tr);
                        List<GridDataHistory> gdhRecords = histMap.get(tr);
                        if (gdhRecords == null) {
                            gdhRecords = new ArrayList<GridDataHistory>();
                            histMap.put(tr, gdhRecords);
                        }
                        gdhRecords.add(gdh);
                    } else {
                        // TODO: error for non-matching TR?
                    }

                }
                sr.setPayload(histMap);
            } else {
                sr.addMessages(invsr);
                sr.addMessage("Unable to get grid inventory for: " + id);
            }

        } else {
            sr.addMessage("Unknown ParmID: " + id);
        }
        return sr;
    }

    /**
     * 
     * @see com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#getGridInventory
     *      (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID)
     */
    @Override
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID pid) {
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        List<TimeRange> trs = new ArrayList<TimeRange>();
        sr.setPayload(trs);
        if (parmIds.contains(pid)) {
            ClimoQuery climoQuery = ClimoQuery.getInstance("gfe");
            trs.addAll(climoQuery.getInventory(pid.getParmName(), pid.getDbId()
                    .getModelName()));
        }
        return sr;
    }

    /**
     * 
     * @see com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#saveGridData
     *      (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID,
     *      com.raytheon.uf.common.time.TimeRange, java.util.List,
     *      com.raytheon.uf.common.message.WsId)
     */
    @Override
    public ServerResponse<?> saveGridData(ParmID pid,
            TimeRange originalTimeRange, List<GFERecord> records,
            WsId requesterId) {
        ServerResponse<?> sr = new ServerResponse<String>();
        final long milliSecInLeapYear = 366L * 24 * 60 * 60 * 1000;

        GridParmInfo gpi = gpiMap.get(pid);
        if (gpi == null) {
            sr.addMessage("Unknown parm '" + pid + "'");
        } else if (!originalTimeRange.isValid()) {
            sr.addMessage("Invalid timerange '" + originalTimeRange + "'");
        } else if ((!originalTimeRange.equals(TimeRange.allTimes()))
                && (originalTimeRange.getStart().getTime() < 0L || originalTimeRange
                        .getEnd().getTime() >= milliSecInLeapYear)) {
            sr.addMessage("Disallowed timerange '" + originalTimeRange + "'");
        } else {
            if (records.size() == 0) {
                GridGeometry2D geom = MapUtil.getGridGeometry(gloc);
                Unit<?> unit = gpi.getUnitObject();
                UnitConverter cvt = null;
                // tp is stored in millimeters, tmin and tmax in kelvin
                if ("tp".equals(pid.getParmName())) {
                    cvt = SI.MILLIMETER.getConverterTo(unit);
                } else {
                    cvt = SI.KELVIN.getConverterTo(unit);
                }
                String parmName = pid.getParmName();
                String modelName = pid.getDbId().getModelName();

                ClimoQuery query = ClimoQuery.getInstance("gfe");
                List<TimeRange> times = query.getInventory(parmName, modelName);
                float[] climate = null;
                if (times == null) {
                    sr.addMessage("Climo CONUS data is missing");
                } else {
                    records = new ArrayList<GFERecord>(times.size());
                    for (TimeRange timeRange : times) {
                        climate = null;
                        // Generate an initial data grid from the CONUS HDF5
                        try {
                            climate = query.getClimate(geom, timeRange,
                                    parmName, modelName);
                        } catch (Exception e) {
                            sr.addMessage(e.getMessage());
                            sr.addMessage("Error generating local climo data");
                            return sr;
                        }
                        if (climate != null) {
                            for (int i = 0; i < climate.length; i++) {
                                climate[i] = (float) cvt.convert(climate[i]);
                            }
                            GFERecord record = new GFERecord(pid, timeRange);
                            record.setGridInfo(gpi);
                            FloatDataRecord fdr = new FloatDataRecord("Data",
                                    record.getDataTime().getValidPeriod()
                                            .toString(), climate, 2,
                                    new long[] { gloc.getNx(), gloc.getNy() });
                            record.setMessageData(fdr);
                            records.add(record);
                        }
                    }
                    try {
                        saveGridParmInfo(gpi);
                        saveGridsToHdf5(records);
                    } catch (GfeException e) {
                        sr.addMessage(e.getMessage());
                        sr.addMessage("Error saving climo data to HDF5");
                        return sr;
                    }
                }
            }
        }
        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#
     * updateGridHistory(java.util.List, java.util.List)
     */
    @Override
    public ServerResponse<?> updateGridHistory(ParmID parmId,
            Map<TimeRange, List<GridDataHistory>> history) {
        ServerResponse<?> sr = new ServerResponse<String>();
        sr.addMessage("Can't update Grid History on Climo Database");
        return sr;
    }

    /**
     * Add grid parm info to the hdf5 file we are creating. This method should
     * only be called when first creating the file; it doesn't check whether the
     * record already exists, because it "knows" it does not.
     * 
     * @throws GfeException
     *             if any i/o, thrift, or hdf5 operation fails.
     */
    protected void saveGridParmInfo(GridParmInfo gpi) throws GfeException {
        File gpiFile = GfeUtil.getHDF5File(gfeBaseDataDir, this.dbId);

        IDataStore ds = null;
        try {
            ds = DataStoreFactory.getDataStore(gpiFile);
            String parmNameAndLevel = gpi.getParmID().getCompositeName();
            ByteDataRecord br = new ByteDataRecord(parmNameAndLevel,
                    GRID_PARM_INFO_GRP, new byte[1]);
            br.setDataAttributes(getGpiAsMap(gpi));
            ds.addDataRecord(br);
            ds.store();
        } catch (Exception e) {
            throw new GfeException("", e);
        }
    }
}
