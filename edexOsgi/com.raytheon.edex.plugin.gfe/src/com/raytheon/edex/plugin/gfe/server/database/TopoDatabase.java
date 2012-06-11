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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
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
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.topo.TopoQuery;

/**
 * A complete encapsulation of the TopoMgr data
 * 
 * Implements the logic needed to make the TopoMgr data appear like another
 * database.
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

public class TopoDatabase extends IFPGridDatabase {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TopoDatabase.class);

    private static final TimeRange TR = TimeRange.allTimes();

    private ParmID parmId;

    private GridParmInfo gpi;

    private GridLocation gloc;

    protected TopoDatabase(String siteID) throws GfeException {

        this.dbId = TopoDatabaseManager.getTopoDbId(siteID);
        this.gridConfig = null;

        this.parmId = new ParmID("Topo", this.dbId, "SFC");

        this.gloc = IFPServerConfigManager.getServerConfig(siteID).dbDomain();

        this.gpi = new GridParmInfo(this.parmId, this.gloc, GridType.SCALAR,
                "ft", "Topography", -16404.0f, 16404.0f, 1, true,
                new TimeConstraints(0, 0, 0), false);

        // assuming we only have one parm
        List<ParmID> parmList = getParmList().getPayload();
        ParmID parmId = parmList.get(0);

        IDataStore dataStore = getDataStore(parmId);
        String group = null;
        try {
            group = GfeUtil.getHDF5Group(parmId, TR);
            IDataRecord[] dr = dataStore.retrieve(group);

            FloatDataRecord fdr = (FloatDataRecord) dr[0];
            float[] heights = fdr.getFloatData();
            long[] sizes = fdr.getSizes();
            if (sizes[0] != gpi.getGridLoc().getNx()
                    || sizes[1] != gpi.getGridLoc().getNy()) {
                dataStore.delete(GfeUtil.getHDF5Group(parmId, TR));
                throw new Exception("Grid spacing has changed");
            }

            float min = Float.MAX_VALUE;
            float max = -Float.MAX_VALUE;
            for (float f : heights) {
                if (!Float.isNaN(f)) {
                    min = Math.min(f, min);
                    max = Math.max(f, max);
                }
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
                message = "Problem accessing " + parmId + " re-initializing...";
                ee = e;
            }
            statusHandler.handle(priority, message, ee);

            ServerResponse<?> sr = saveGridData(parmId, TR,
                    new ArrayList<GFERecord>(), new WsId(null,
                            "initialization", "TopoDatabase"));
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

    @Override
    public void updateDbs() {
        // no op
    }

    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID id) {
        ServerResponse<GridParmInfo> sr = new ServerResponse<GridParmInfo>();
        sr.setPayload(this.gpi);
        return sr;
    }

    @Override
    public ServerResponse<List<ParmID>> getParmList() {
        List<ParmID> parmIds = new ArrayList<ParmID>();
        ServerResponse<List<ParmID>> sr = new ServerResponse<List<ParmID>>();

        parmIds.add(new ParmID("Topo", this.dbId, "SFC"));

        sr.setPayload(parmIds);
        return sr;
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
        // do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#deleteDb()
     */
    @Override
    public void deleteDb() {
        throw new UnsupportedOperationException("Cannot delete Topo databases");
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
        // do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#getGridHistory
     * (com.raytheon.edex.plugin.gfe.db.objects.ParmID, java.util.List)
     */
    @Override
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID id, List<TimeRange> timeRanges) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();

        if (!this.parmId.equals(id)) {
            sr.addMessage("Unknown ParmID: " + id);
        } else if (timeRanges.size() != 1 || !timeRanges.get(0).equals(TR)) {
            sr.addMessage("Invalid time requested");
        } else {

            Map<TimeRange, List<GridDataHistory>> history = new HashMap<TimeRange, List<GridDataHistory>>();
            List<GridDataHistory> hist = new ArrayList<GridDataHistory>();
            hist.add(new GridDataHistory(OriginType.SCRATCH, id, TR));
            history.put(TR, hist);
            sr.setPayload(history);
        }
        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#getGridInventory
     * (com.raytheon.edex.plugin.gfe.db.objects.ParmID)
     */
    @Override
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID pid) {
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        List<TimeRange> trs = new ArrayList<TimeRange>();
        trs.add(TR);
        sr.setPayload(trs);
        return sr;
    }

    public IGridSlice getTopo() {
        IGridSlice topo = null;
        GetGridRequest req = new GetGridRequest();
        req.setParmId(parmId);
        GFERecord gfeRec = new GFERecord(parmId, TimeRange.allTimes());
        ArrayList<GFERecord> gfeList = new ArrayList<GFERecord>();
        gfeList.add(gfeRec);
        req.setRecords(gfeList);
        ArrayList<GetGridRequest> reqList = new ArrayList<GetGridRequest>();
        reqList.add(req);

        List<IGridSlice> data = GridParmManager.getGridData(reqList)
                .getPayload();
        if (data != null && data.size() == 1) {
            topo = data.get(0);
        }

        return topo;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.IFPGridDatabase#saveGridData
     * (com.raytheon.edex.plugin.gfe.db.objects.ParmID,
     * com.raytheon.uf.common.time.TimeRange, java.util.List)
     */
    @Override
    public ServerResponse<?> saveGridData(ParmID pid,
            TimeRange originalTimeRange, List<GFERecord> records,
            WsId requesterId) {
        ServerResponse<?> sr = new ServerResponse<String>();
        if (records.size() > 1) {
            sr.addMessage("Can (and must) save only one topo grid");
            return sr;
        }

        if (records.size() == 0) {
            float[] heights = TopoQuery.getInstance().getHeight(
                    MapUtil.getGridGeometry(gloc));

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
            gpi.setMinValue((float) Math.floor(min));
            gpi.setMaxValue((float) Math.ceil(max));

            GFERecord record = new GFERecord(pid, TR);
            record.setGridInfo(gpi);
            FloatDataRecord fdr = new FloatDataRecord("Data", record
                    .getDataTime().getValidPeriod().toString(), heights, 2,
                    new long[] { gloc.getNx(), gloc.getNy() });
            record.setMessageData(fdr);
            records.add(record);
        }

        try {
            saveGridsToHdf5(records);
            ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr1 = getGridHistory(
                    pid, Arrays.asList(TR));
            Map<TimeRange, List<GridDataHistory>> histories = sr1.getPayload();
            sr.addNotifications(new GridUpdateNotification(pid,
                    originalTimeRange, histories, requesterId, pid.getDbId()
                            .getSiteId()));
        } catch (GfeException e) {
            sr.addMessage("Error saving topo data to HDF5");
            return sr;
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
        sr.addMessage("Can't update Grid History on TopoDatabase");
        return sr;
    }

}
