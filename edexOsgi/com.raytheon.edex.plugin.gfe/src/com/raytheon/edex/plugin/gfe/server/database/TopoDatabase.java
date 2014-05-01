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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * A complete encapsulation of the TopoMgr data.
 * <p>
 * Implements the logic needed to make the TopoMgr data appear like another
 * database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2008  #1160     randerso     Initial creation
 * Jul 10, 2009  #2590     njensen      Support for multiple sites.
 * May 04, 2012  #574      dgilling     Re-port to better match AWIPS1.
 * Apr 23, 2013  #1949     rjpeter      Removed unused method.
 * Nov 20, 2013  #2331     randerso     Changed return type of getTopoData
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class TopoDatabase extends VGridDatabase {
    private static final TimeRange TR = TimeRange.allTimes();

    private final TopoDatabaseManager topoMgr;

    private final GridLocation gloc;

    private final ParmID pid;

    private final GridParmInfo gpi;

    public TopoDatabase(final IFPServerConfig config,
            TopoDatabaseManager topoMgr) {
        super(config);
        this.topoMgr = topoMgr;
        this.dbId = new DatabaseID(getSiteID(config), DataType.GRID,
                "EditTopo", "Topo");
        this.gloc = this.config.dbDomain();
        this.pid = new ParmID("Topo", dbId, "SFC");
        this.gpi = new GridParmInfo(pid, gloc, GridType.SCALAR, "ft",
                "Topograpy", -32000.0f, 50000.0f, 1, true, new TimeConstraints(
                        0, 0, 0));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.gfe.server.database.GridDatabase#getDbId()
     */
    @Override
    public DatabaseID getDbId() {
        return this.dbId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.GridDatabase#databaseIsValid
     * ()
     */
    @Override
    public boolean databaseIsValid() {
        return dbId.isValid();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.GridDatabase#getProjectionId
     * ()
     */
    @Override
    public String getProjectionId() {
        return gloc.getProjection().getProjectionID();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.GridDatabase#getParmList()
     */
    @Override
    public ServerResponse<List<ParmID>> getParmList() {
        List<ParmID> parmIds = new ArrayList<ParmID>(1);
        parmIds.add(new ParmID("Topo", this.dbId, "SFC"));

        ServerResponse<List<ParmID>> sr = new ServerResponse<List<ParmID>>();
        sr.setPayload(parmIds);
        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.GridDatabase#getGridParmInfo
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID)
     */
    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID id) {
        ServerResponse<GridParmInfo> sr = new ServerResponse<GridParmInfo>();
        if (!pid.equals(id)) {
            sr.addMessage("Unknown ParmID: " + id);
        } else {
            sr.setPayload(this.gpi);
        }

        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.GridDatabase#getGridInventory
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID)
     */
    @Override
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID pid) {
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        if (!this.pid.equals(pid)) {
            sr.addMessage("Unknown ParmID: " + pid);
        } else {
            List<TimeRange> trs = new ArrayList<TimeRange>(1);
            trs.add(TR);
            sr.setPayload(trs);
        }

        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.GridDatabase#getGridHistory
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID, java.util.List)
     */
    @Override
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID id, List<TimeRange> timeRanges) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();

        if (!this.pid.equals(id)) {
            sr.addMessage("Unknown ParmID: " + id);
        } else if ((timeRanges.size() != 1) || !timeRanges.get(0).equals(TR)) {
            sr.addMessage("Invalid time requested");
        } else {

            Map<TimeRange, List<GridDataHistory>> history = new HashMap<TimeRange, List<GridDataHistory>>();
            List<GridDataHistory> hist = new ArrayList<GridDataHistory>(1);
            hist.add(new GridDataHistory(OriginType.SCRATCH, pid, TR));
            history.put(TR, hist);
            sr.setPayload(history);
        }
        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.GridDatabase#getGridData
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID, java.util.List)
     */
    @Override
    public ServerResponse<List<IGridSlice>> getGridData(ParmID id,
            List<TimeRange> timeRanges) {
        ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();
        if (!pid.equals(id)) {
            sr.addMessage("Unknown ParmID: " + id);
        } else if ((timeRanges.size() != 1) || (!timeRanges.get(0).equals(TR))) {
            sr.addMessage("Invalid time requested.");
        } else {
            List<IGridSlice> data = new ArrayList<IGridSlice>(1);
            GridDataHistory gdh = new GridDataHistory(OriginType.SCRATCH, pid,
                    TR);
            ServerResponse<ScalarGridSlice> srRetrieve = topoMgr
                    .getTopoData(gloc);
            sr.addMessages(srRetrieve);
            ScalarGridSlice tempgs = srRetrieve.getPayload();
            IGridSlice gs = new ScalarGridSlice(TR, gpi,
                    new GridDataHistory[] { gdh }, tempgs.getScalarGrid());
            data.add(gs);
            sr.setPayload(data);
        }

        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.gfe.server.database.GridDatabase#saveGridData
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID,
     * com.raytheon.uf.common.time.TimeRange, java.util.List,
     * com.raytheon.uf.common.message.WsId)
     */
    @Override
    public ServerResponse<?> saveGridData(ParmID id,
            TimeRange originalTimeRange, List<GFERecord> records,
            WsId requesterId) {
        ServerResponse<?> sr = new ServerResponse<Object>();

        if (!pid.equals(id)) {
            sr.addMessage("Unknown ParmID: " + id);
        } else if (records.isEmpty()) {
            topoMgr.revertTopoData(gloc);
        } else if (records.size() > 1) {
            sr.addMessage("Can (and must) save only one topo grid");
        } else {
            Object data = records.get(0).getMessageData();
            topoMgr.saveTopoData(gloc, (IGridSlice) data);
        }

        return sr;
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
        return new TreeSet<Date>();
    }
}
