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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
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
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.hlstopo.HLSTopoQuery;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 15, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class HLSTopoDatabase extends IFPGridDatabase {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HLSTopoDatabase.class);

    private Set<ParmID> parmIds;

    private Map<ParmID, GridParmInfo> gpiMap;

    private GridLocation gloc;

    /**
     * @param siteID
     * @param source
     * @throws GfeException
     */
    protected HLSTopoDatabase(String siteID, String source) throws GfeException {
        this.dbId = HLSTopoDatabaseManager.getHLSTopoDbId(siteID, source);
        this.gridConfig = null;

        this.gloc = IFPServerConfigManager.getServerConfig(siteID).dbDomain();

        ParmID minTopoParm = new ParmID("minTopo", dbId, "SFC");
        ParmID maxTopoParm = new ParmID("maxTopo", dbId, "SFC");
        ParmID avgTopoParm = new ParmID("avgTopo", dbId, "SFC");
        parmIds = new TreeSet<ParmID>();
        parmIds.add(minTopoParm);
        parmIds.add(maxTopoParm);
        parmIds.add(avgTopoParm);

        gpiMap = new HashMap<ParmID, GridParmInfo>();
        GridParmInfo gpi;

        gpi = new GridParmInfo(minTopoParm, gloc, GridType.SCALAR, "m",
                "Minimum Topo", -5000.0f, 5000.0f, 1, false,
                new TimeConstraints(), false);
        gpiMap.put(minTopoParm, gpi);

        gpi = new GridParmInfo(maxTopoParm, gloc, GridType.SCALAR, "m",
                "Maximum Topo", -5000.0f, 5000.0f, 1, false,
                new TimeConstraints(), false);
        gpiMap.put(maxTopoParm, gpi);

        gpi = new GridParmInfo(avgTopoParm, gloc, GridType.SCALAR, "m",
                "Average Topo", -5000.0f, 5000.0f, 1, false,
                new TimeConstraints(), false);
        gpiMap.put(avgTopoParm, gpi);

        TimeRange allTimes = TimeRange.allTimes();
        for (ParmID parmId : parmIds) {
            gpi = gpiMap.get(parmId);
            IDataStore dataStore = getDataStore(parmId);
            String group = null;
            try {
                group = GfeUtil.getHDF5Group(parmId, allTimes);
                IDataRecord[] dr = dataStore.retrieve(group);

                FloatDataRecord fdr = (FloatDataRecord) dr[0];
                float[] values = fdr.getFloatData();
                long[] sizes = fdr.getSizes();
                if (sizes[0] != gpi.getGridLoc().getNx()
                        || sizes[1] != gpi.getGridLoc().getNy()) {
                    dataStore.delete(GfeUtil.getHDF5Group(parmId, allTimes));
                    throw new Exception("");
                }

                float min = Float.MAX_VALUE;
                float max = -Float.MAX_VALUE;
                for (float f : values) {
                    min = Math.min(f, min);
                    max = Math.max(f, max);
                }
                gpi.setMinValue((float) Math.floor(min));
                gpi.setMaxValue((float) Math.ceil(max));

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
                } else if (s.contains("No group " + group + " found")) {
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

                ServerResponse<?> sr = saveGridData(parmId, allTimes,
                        new ArrayList<GFERecord>(), new WsId(null,
                                "initialization", "HLSTopoDatabase"));
                if (!sr.isOkay()) {
                    StringBuilder tmp = new StringBuilder();
                    ArrayList<ServerMsg> messages = sr.getMessages();
                    String sep = "";
                    for (ServerMsg msg : messages) {
                        tmp.append(sep).append(msg);
                        sep = "\n";
                    }
                    throw new GfeConfigurationException(tmp.toString(), e);
                }
            }
        }
    }

    @Override
    public void updateDbs() {
        // no op
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#dbSizeStats
     * ()
     */
    @Override
    public void dbSizeStats() {
        return; // do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#deleteDb()
     */
    @Override
    public void deleteDb() {
        throw new UnsupportedOperationException(
                "Cannot delete HLS topo databases.");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#dumpStatistics
     * ()
     */
    @Override
    public void dumpStatistics() {
        return; // do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#getParmList
     * ()
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#getGridInventory
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID)
     */
    @Override
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID id) {
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        List<TimeRange> trs = new ArrayList<TimeRange>();
        sr.setPayload(trs);
        if (parmIds.contains(id)) {
            trs.add(TimeRange.allTimes());
        }
        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#getGridHistory
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID, java.util.List)
     */
    @Override
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID id, List<TimeRange> trs) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();

        if (parmIds.contains(id)) {
            ServerResponse<List<TimeRange>> invsr = getGridInventory(id);
            if (invsr.isOkay()) {
                Map<TimeRange, List<GridDataHistory>> histMap = new HashMap<TimeRange, List<GridDataHistory>>();
                List<TimeRange> inventory = invsr.getPayload();
                for (TimeRange tr : trs) {
                    if (inventory.contains(tr)) {
                        GridDataHistory gdh = new GridDataHistory(
                                OriginType.INITIALIZED, id, tr);
                        List<GridDataHistory> gdhSet = histMap.get(tr);
                        if (gdhSet == null) {
                            gdhSet = new ArrayList<GridDataHistory>();
                            histMap.put(tr, gdhSet);
                        }
                        gdhSet.add(gdh);
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#getGridParmInfo
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID)
     */
    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID id) {
        ServerResponse<GridParmInfo> sr = new ServerResponse<GridParmInfo>();
        GridParmInfo gpi = gpiMap.get(id);
        if (gpi == null) {
            String msg = "Parm '" + (id == null ? id : id.getParmId())
                    + "' is not in the HLS topo database";
            sr.addMessage(msg);
        } else {
            // Give users a copy so they can't alter the one in the map
            gpi = gpi.clone();
            sr.setPayload(gpi);
        }
        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#saveGridData
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID,
     * com.raytheon.uf.common.time.TimeRange, java.util.List,
     * com.raytheon.uf.common.message.WsId)
     */
    @Override
    public ServerResponse<?> saveGridData(ParmID id,
            TimeRange originalTimeRange, List<GFERecord> records,
            WsId requesterId) {

        GridParmInfo gpi = gpiMap.get(id);
        String modelName = id.getDbId().getModelName();
        if (records.size() == 0) {
            float[] heights = new float[0];
            try {
                heights = HLSTopoQuery.getInstance(modelName).getHeight(
                        MapUtil.getGridGeometry(gloc), id.getParmName(),
                        modelName);
            } catch (DataAccessLayerException e) {
                ServerResponse<?> sr = new ServerResponse<String>();
                sr.addMessage("Error generating HLS Topo data");
                sr.addMessage(e.getMessage());
                for (StackTraceElement element : e.getStackTrace()) {
                    sr.addMessage(element.toString());
                }
                return sr;
            }

            // Convert to unit (probably SI.METER too),
            // get max and min values for site
            Unit<?> unit = gpi.getUnitObject();
            UnitConverter cvt = SI.METER.getConverterTo(unit);
            float min = Float.MAX_VALUE;
            float max = -Float.MAX_VALUE;
            for (int i = 0; i < heights.length; i++) {
                if (!Float.isNaN(heights[i])) {
                    heights[i] = (float) cvt.convert(heights[i]);
                    min = Math.min(heights[i], min);
                    max = Math.max(heights[i], max);
                }
            }

            // Store the max and min in gpi.
            // NOTE: gpi is still in dbMap, too.
            // Technically, we _should_ save it back...
            gpi.setMinValue((float) Math.floor(min));
            gpi.setMaxValue((float) Math.ceil(max));

            GFERecord record = new GFERecord(id, TimeRange.allTimes());
            record.setGridInfo(gpi);
            FloatDataRecord fdr = new FloatDataRecord("Data", record
                    .getDataTime().getValidPeriod().toString(), heights, 2,
                    new long[] { gloc.getNx(), gloc.getNy() });
            record.setMessageData(fdr);
            records.add(record);
        }

        // Store the gpi into the datastore
        try {
            saveGridParmInfo(gpi);
        } catch (Exception e) {
            ServerResponse<String> sr = new ServerResponse<String>();
            sr.setPayload(e.toString());
            sr.addMessage(e.getLocalizedMessage());
            sr.addMessage("Error saving GridParmInfo for "
                    + gpi.getDescriptiveName());
            return sr;
        }

        ServerResponse<?> sr = new ServerResponse<String>();
        try {
            saveGridsToHdf5(records);
            ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr1 = getGridHistory(
                    id, Arrays.asList(TimeRange.allTimes()));
            Map<TimeRange, List<GridDataHistory>> histories = sr1.getPayload();
            sr.addNotifications(new GridUpdateNotification(id,
                    originalTimeRange, histories, requesterId, id.getDbId()
                            .getSiteId()));
        } catch (GfeException e) {
            sr.addMessage("Error saving hlsTopo data to HDF5");
            return sr;
        }

        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#
     * updateGridHistory
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID, java.util.List,
     * java.util.List)
     */
    @Override
    public ServerResponse<?> updateGridHistory(ParmID parmId,
            Map<TimeRange, List<GridDataHistory>> history) {
        ServerResponse<?> sr = new ServerResponse<String>();
        sr.addMessage("Can't update Grid History on HLS Topo Database");
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
