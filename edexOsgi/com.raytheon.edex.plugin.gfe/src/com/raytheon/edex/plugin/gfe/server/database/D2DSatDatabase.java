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
import java.util.List;
import java.util.Map;
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
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.message.WsId;
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
 * Oct 10  2012     #1260  randerso    Added code to set valid flag
 * 05/02/13         #1969  randerso    Removed unnecessary updateDbs method
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class D2DSatDatabase extends VGridDatabase {

    /** The list of parms contained in the satellite database */
    private List<D2DSatParm> parms;

    /**
     * Creates a new D2DSatDatabase
     * 
     * @param config
     *            The server config for this site
     * @param productURIs
     *            URI segments which describe the origins of the data in this
     *            database
     * @param parmNames
     *            The parm names used by GFE to identify this satellite data
     */
    public D2DSatDatabase(IFPServerConfig config, List<String> productURIs,
            List<String> parmNames) {
        super(config);
        this.dbId = new DatabaseID(config.getSiteID().get(0), DataType.GRID,
                "D2D", "Satellite", "00000000_0000");
        this.valid = this.dbId.isValid();
        parms = new ArrayList<D2DSatParm>();
        for (int i = 0; i < productURIs.size(); i++) {
            D2DSatParm parm = new D2DSatParm(config, productURIs.get(i),
                    this.dbId, parmNames.get(i));
            parms.add(parm);
            // D2DParmIdCache.getInstance().putParmID(parm.pid());
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

    /**
     * Finds a parm contained in this database
     * 
     * @param pid
     *            The parm to find
     * @return The D2DSatParm associated with the given parmID. null is returned
     *         if the database does not contain the desired parmID
     */
    public D2DSatParm findParm(ParmID pid) {
        for (D2DSatParm parm : parms) {
            if (pid.equals(parm.pid())) {
                return parm;
            }
        }
        return null;
    }

    @Override
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID id) {
        D2DSatParm p = findParm(id);
        if (p != null) {
            return p.getGridInventory();
        }
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        sr.addMessage("Parm not found: " + id);
        return sr;

    }

    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID id) {
        D2DSatParm p = findParm(id);
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
        D2DSatParm p = findParm(id);
        if (p != null) {
            return p.getGridHistory(trs);
        }
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();
        sr.addMessage("Parm not found: " + id);
        return sr;
    }

    @Override
    public ServerResponse<List<ParmID>> getParmList() {
        ServerResponse<List<ParmID>> retVal = new ServerResponse<List<ParmID>>();
        List<ParmID> parmIDs = new ArrayList<ParmID>();
        for (int i = 0; i < this.parms.size(); i++) {
            parmIDs.add(parms.get(i).pid());
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

        D2DSatParm p = findParm(id);
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
        for (D2DSatParm parm : parms) {
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
}
