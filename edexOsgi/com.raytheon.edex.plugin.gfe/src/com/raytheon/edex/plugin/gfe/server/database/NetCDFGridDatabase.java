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

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Database that reads netCDF files like A1
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class NetCDFGridDatabase extends VGridDatabase {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NetCDFGridDatabase.class);

    private static class NetCDFParm {
        private ParmID pid;

        private GridParmInfo gpi;

        private List<TimeRange> inv;

        private int[] indices;

        private int level;

        private String varName;

        public NetCDFParm(String varName, ParmID pid, GridParmInfo gpi,
                List<TimeRange> inventory, int[] indices, int level) {
            this.pid = pid;
            this.gpi = gpi;
            this.inv = inventory;
            this.indices = indices;
            this.level = level;
            this.varName = varName;
        }

        public ParmID getPid() {
            return pid;
        }

        public GridParmInfo getGpi() {
            return gpi;
        }

        public List<TimeRange> getInv() {
            return inv;
        }

        public int[] getIndices() {
            return indices;
        }

        public int getLevel() {
            return level;
        }

        public String getVarName() {
            return varName;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((pid == null) ? 0 : pid.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            NetCDFParm other = (NetCDFParm) obj;
            if (pid == null) {
                if (other.pid != null) {
                    return false;
                }
            } else if (!pid.equals(other.pid)) {
                return false;
            }
            return true;
        }
    }

    private boolean valid;

    private List<NetCDFParm> parms;

    private NetCDFFile file;

    private GridLocation inputGloc;

    private GridLocation outputGloc;

    private Rectangle subdomain;

    private RemapGrid remap;

    public NetCDFGridDatabase(IFPServerConfig config, NetCDFFile file) {
        super(config);
        this.valid = true;
        this.file = file;
        this.remap = null;

        // Get the database id for this database.
        this.dbId = getDBID(this.file, this.config);
        if (!this.dbId.isValid()) {
            this.valid = false;
        }

        // get the output gloc
        if (this.valid) {
            this.outputGloc = this.config.dbDomain();
            if (!this.outputGloc.isValid()) {
                this.valid = false;
            }
        }

        // get the input (netCDF) gloc
        if (this.valid) {
            this.inputGloc = getInputGloc();
            if (!this.inputGloc.isValid()) {
                this.valid = false;
            }
        }

        // create the remapping class, create the parms
        if (this.valid) {
            this.subdomain = NetCDFUtils.getSubGridDims(this.inputGloc,
                    this.outputGloc);
            this.remap = new RemapGrid(NetCDFUtils.subGridGL(this.inputGloc,
                    this.subdomain), this.outputGloc);
            loadParms();
        }
    }

    public static DatabaseID getDBID(NetCDFFile file, IFPServerConfig config) {
        return new DatabaseID(getSiteID(config), DataType.GRID, "D2D",
                file.getModelName(), file.getModelTime());
    }

    /**
     * Determines the Grid Location to be used for input for this
     * NetCDFGridDatabase.
     * 
     * @return This is the grid location for the netCDF file.
     */
    private GridLocation getInputGloc() {
        return new GridLocation(this.file.getProjection());
    }

    @Override
    public SortedSet<Date> getValidTimes() throws GfeException,
            DataAccessLayerException {
        // do nothing for now, only needed for manual smartInit to run against
        // this database which shouldn't be needed
        return null;
    }

    @Override
    public void deleteDb() {
        // do nothing
    }

    @Override
    public ServerResponse<List<ParmID>> getParmList() {
        ServerResponse<List<ParmID>> sr = new ServerResponse<List<ParmID>>();
        List<ParmID> parmIDs = new ArrayList<ParmID>();
        for (NetCDFParm p : this.parms) {
            parmIDs.add(p.getPid());
        }
        sr.setPayload(parmIDs);

        return sr;
    }

    // Calculates the precision based on the max/min values.
    private int calcPrecision(float minV, float maxV) {
        if (maxV - minV > 250.0) {
            return 0;
        } else if (maxV - minV > 25.0) {
            return 1;
        } else if (maxV - minV > 2.5) {
            return 2;
        } else if (maxV - minV > 0.25) {
            return 3;
        } else if (maxV - minV > 0.025) {
            return 4;
        } else {
            return 5;
        }
    }

    private TimeConstraints getTimeConstraints(List<TimeRange> times) {
        if (times.size() <= 1) {
            return new TimeConstraints(3600, 3600, 0);
        }

        int repeat = (int) ((times.get(1).getStart().getTime() - times.get(0)
                .getStart().getTime()) / 1000);
        int start = (int) (times.get(0).getStart().getTime() / 1000) % 86400;

        for (int i = 1; i < times.size() - 1; i++) {
            if ((times.get(i + 1).getStart().getTime() - times.get(i)
                    .getStart().getTime()) / 1000 != repeat) {
                return new TimeConstraints(3600, 3600, 0);
            }
        }

        return new TimeConstraints(3600, repeat, start);
    }

    private void loadParm(NetCDFFile.ParmAtts atts, int level) {
        List<String> accumParms = this.config.accumulativeD2DElements(this.file
                .getModelName());
        boolean accParm = false;
        if (accumParms.indexOf(atts.getUiname()) != -1) {
            accParm = true;
        }

        ParmID pid = new ParmID(atts.getUiname(), this.dbId, atts
                .getLevelNames().get(level));
        if (!pid.isValid()) {
            return;
        }

        boolean rateParm = false;
        TimeConstraints tc = getTimeConstraints(this.file.getAvailableTimes());
        if (accParm) {
            tc = new TimeConstraints(tc.getRepeatInterval(),
                    tc.getRepeatInterval(), tc.getStartTime());
            rateParm = true;
        }

        float minV = atts.getMinVal();
        float maxV = atts.getMaxVal();
        int precision = calcPrecision(minV, maxV);
        GridParmInfo gpi = new GridParmInfo(pid, this.outputGloc,
                GridType.SCALAR, atts.getUnits(), atts.getLongName(), minV,
                maxV, precision, false, tc, rateParm);

        // if (!gpi.isValid()) {
        // return;
        // }

        List<TimeRange> inventory = new ArrayList<TimeRange>(
                this.file.getAvailableTimes());
        if (accParm) {
            for (int i = 0; i < inventory.size(); i++) {
                inventory.set(i, new TimeRange(new Date(inventory.get(i)
                        .getStart().getTime()
                        - tc.getDuration()), tc.getDuration()));
            }
        } else if ((atts.getName().equals("staticTopo")
                || atts.getName().equals("staticSpacing") || atts.getName()
                .equals("staticCoriolis")) && !inventory.isEmpty()) {
            TimeRange ntr = inventory.get(0).combineWith(
                    inventory.get(inventory.size() - 1));
            inventory.clear();
            inventory.add(ntr);
        }

        List<Integer> indices = new ArrayList<Integer>();
        for (int i = 0; i < atts.getInventory().getYdim(); i++) {
            indices.add(i);
        }

        // TODO: is this necessary?
        // inventory.setLength(atts.getInventory().getYdim());

        for (int time = atts.getInventory().getYdim() - 1; time >= 0; time--) {
            if (atts.getInventory().get(level, time) == 0
                    || ((accParm) && (time > 0 && this.file.getTpSubPrev(time) && atts
                            .getInventory().get(level, time - 1) == 0))) {
                inventory.remove(time);
                indices.remove(time);
            }
        }

        int[] ind = new int[indices.size()];
        for (int i = 0; i < indices.size(); i++) {
            ind[i] = indices.get(i);
        }
        this.parms.add(new NetCDFParm(atts.getName(), pid, gpi, inventory, ind,
                level));
    }

    private void loadParm(NetCDFFile.ParmAtts uatts, NetCDFFile.ParmAtts vatts,
            int level) {
        ParmID pid = new ParmID("wind", this.dbId, uatts.getLevelNames().get(
                level));
        if (!pid.isValid()) {
            return;
        }

        TimeConstraints tc = getTimeConstraints(this.file.getAvailableTimes());

        float maxV = uatts.getMaxVal();
        float minV = 0.0f;
        int precision = calcPrecision(minV, maxV);
        GridParmInfo gpi = new GridParmInfo(pid, this.outputGloc,
                GridType.VECTOR, uatts.getUnits(), "wind", minV, maxV,
                precision, false, tc, false);

        // if (!gpi.isValid()) {
        // return;
        // }

        List<TimeRange> inventory = new ArrayList<TimeRange>(
                (this.file.getAvailableTimes()));
        List<Integer> indices = new ArrayList<Integer>();
        for (int i = 0; i < uatts.getInventory().getYdim(); i++) {
            indices.add(i);
        }

        // TODO: is this necessary? I think it's already this size
        // inventory.setLength(uatts.getInventory().getYdim());

        for (int time = uatts.getInventory().getYdim() - 1; time >= 0; time--) {
            if (uatts.getInventory().get(level, time) == 0
                    || vatts.getInventory().get(level, time) == 0) {
                inventory.remove(time);
                indices.remove(time);
            }
        }

        int[] ind = new int[indices.size()];
        for (int i = 0; i < indices.size(); i++) {
            ind[i] = indices.get(i);
        }
        this.parms.add(new NetCDFParm("", pid, gpi, inventory, ind, level));
    }

    private void loadParms() {
        List<String> parmNames = new ArrayList<String>(this.file.getParmNames());
        this.parms = new ArrayList<NetCDFGridDatabase.NetCDFParm>();

        // First see if we can make wind.
        int uindex = parmNames.indexOf("uw");
        int vindex = parmNames.indexOf("vw");
        if (uindex != -1 && vindex != -1) {
            NetCDFFile.ParmAtts uatts = this.file.getAtts("uw");
            NetCDFFile.ParmAtts vatts = this.file.getAtts("vw");
            if (uatts.getInventory().getXdim() == vatts.getInventory()
                    .getXdim()
                    && uatts.getInventory().getYdim() == vatts.getInventory()
                            .getYdim()) {
                if (uindex < vindex) {
                    int tmp = uindex;
                    uindex = vindex;
                    vindex = tmp;
                }
                parmNames.remove(uindex);
                parmNames.remove(vindex);
                for (int j = 0; j < uatts.getInventory().getXdim(); j++) {
                    loadParm(uatts, vatts, j);
                }
            }
        } else {
            int sindex = parmNames.indexOf("ws");
            int dindex = parmNames.indexOf("wd");
            if (sindex != -1 && dindex != -1) {
                NetCDFFile.ParmAtts satts = this.file.getAtts("ws");
                NetCDFFile.ParmAtts datts = this.file.getAtts("wd");
                if (satts.getInventory().getXdim() == datts.getInventory()
                        .getXdim()
                        && satts.getInventory().getYdim() == datts
                                .getInventory().getYdim()) {
                    if (sindex < dindex) {
                        int tmp = sindex;
                        sindex = dindex;
                        dindex = tmp;
                    }
                    parmNames.remove(sindex);
                    parmNames.remove(dindex);
                    for (int j = 0; j < satts.getInventory().getXdim(); j++) {
                        loadParm(satts, datts, j);
                    }
                }
            }
        }

        // Now do all the scalars
        for (String p : parmNames) {
            NetCDFFile.ParmAtts atts = this.file.getAtts(p);
            for (int j = 0; j < atts.getInventory().getXdim(); j++) {
                loadParm(atts, j);
            }
        }
    }

    @Override
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID id) {
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        for (NetCDFParm p : this.parms) {
            if (id.equals(p.getPid())) {
                List<TimeRange> trs = p.getInv();
                sr.setPayload(trs);
                return sr;
            }
        }

        sr.addMessage("Unknown PID: " + id);
        return sr;
    }

    // Returns a Grid2DFloat for the specified parm name, index, and level.
    // The data will be smoothed and truncated to min and max.
    private Grid2DFloat getGrid(String name, int index, int level, float minv,
            float maxv) {
        Grid2DFloat bdata = new Grid2DFloat(this.file.getGrid(name, index,
                level, this.subdomain));

        // deal with the special tp case, where some of the grids are actually
        // sums of other grids
        if (name.equals("tp") && index > 0 && this.file.getTpSubPrev(index)) {
            Grid2DFloat prev = new Grid2DFloat(this.file.getGrid(name,
                    index - 1, level, this.subdomain));
            for (int x = 0; x < bdata.getXdim(); x++) {
                for (int y = 0; y < bdata.getYdim(); y++) {
                    bdata.set(x, y, bdata.get(x, y) - prev.get(x, y));
                }
            }
        }

        // determine the fill value
        float fillV = Float.MAX_VALUE;
        NetCDFFile.ParmAtts pa = this.file.getAtts(name);
        if (pa != null) {
            fillV = pa.getFillVal();
        }

        try {
            return this.remap.remap(bdata, fillV, maxv, minv, minv);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return null;
        }
    }

    // Returns two Grid2D<float> for the index, and level. Assumes "uw" and "vw"
    // for the grid data.
    // The data will be smoothed and truncated to min and max.
    private void getWindGrid(int index, int level, float min, float max,
            Grid2DFloat mag, Grid2DFloat dir) {
        Grid2DFloat udata = new Grid2DFloat(this.file.getGrid("uw", index,
                level, this.subdomain));
        Grid2DFloat vdata = new Grid2DFloat(this.file.getGrid("vw", index,
                level, this.subdomain));
        if (udata.isValid() && vdata.isValid()) {
            float fillV = Float.MAX_VALUE;
            NetCDFFile.ParmAtts pa = this.file.getAtts("uw");
            if (pa != null) {
                fillV = pa.getFillVal();
            }

            try {
                remap.remapUV(udata, vdata, fillV, max, min, min, true, true,
                        mag, dir);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                mag = null;
                dir = null;
            }
        } else {
            Grid2DFloat sdata = new Grid2DFloat(this.file.getGrid("ws", index,
                    level, this.subdomain));
            Grid2DFloat ddata = new Grid2DFloat(this.file.getGrid("wd", index,
                    level, this.subdomain));
            if (sdata.isValid() && ddata.isValid()) {
                float fillV = Float.MAX_VALUE;
                NetCDFFile.ParmAtts pa = this.file.getAtts("ws");
                if (pa != null) {
                    fillV = pa.getFillVal();
                }

                try {
                    remap.remap(sdata, ddata, fillV, max, min, min, mag, dir);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    mag = null;
                    dir = null;
                }
            } else {
                mag = null;
                dir = null;
            }
        }

    }

    /**
     * Returns a GridSlice for the supplied NetCDFParm and index.
     * 
     * @param p
     *            desired parm
     * @param index
     *            desired index
     * @return GridSlice
     */
    private IGridSlice getGridSlice(NetCDFParm p, int index) {
        IGridSlice gs = null;
        GridDataHistory gdh = new GridDataHistory(OriginType.INITIALIZED,
                p.getPid(), p.getInv().get(index));

        switch (p.getGpi().getGridType()) {
        case SCALAR: {
            Grid2DFloat data = new Grid2DFloat(getGrid(p.getVarName(),
                    p.getIndices()[index], p.getLevel(), p.getGpi()
                            .getMinValue(), p.getGpi().getMaxValue()));
            if (!data.isValid()) {
                return null;
            }
            gs = new ScalarGridSlice(p.getInv().get(index), p.getGpi(),
                    Arrays.asList(gdh), data);
            break;
        }
        case VECTOR: {
            Grid2DFloat mag = new Grid2DFloat(p.getGpi().getGridLoc().getNx(),
                    p.getGpi().getGridLoc().getNy());
            Grid2DFloat dir = new Grid2DFloat(p.getGpi().getGridLoc().getNx(),
                    p.getGpi().getGridLoc().getNy());
            getWindGrid(p.getIndices()[index], p.getLevel(), p.getGpi()
                    .getMinValue(), p.getGpi().getMaxValue(), mag, dir);
            if (!mag.isValid() || !dir.isValid()) {
                return null;
            }
            gs = new VectorGridSlice(p.getInv().get(index), p.getGpi(),
                    Arrays.asList(gdh), mag, dir);
            break;
        }
        default:
            statusHandler.handle(Priority.PROBLEM,
                    "unsupported parm type for: " + p.getGpi());
        }

        return gs;
    }

    @Override
    public ServerResponse<List<IGridSlice>> getGridData(ParmID id,
            List<TimeRange> timeRanges) {
        ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();

        for (NetCDFParm p : this.parms) {
            if (id.equals(p.getPid())) {
                {
                    List<IGridSlice> data = new ArrayList<IGridSlice>();
                    for (int j = 0; j < timeRanges.size(); j++) {
                        int k = p.getInv().indexOf(timeRanges.get(j));
                        if (k != -1) {
                            data.add(getGridSlice(p, k));
                        } else {
                            sr.addMessage("Time range is not in inventory: "
                                    + timeRanges.get(j));
                            return sr;
                        }
                    }
                    sr.setPayload(data);
                    return sr;
                }
            }
        }

        sr.addMessage("Unknown PID: " + id);
        return sr;
    }

    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID id) {
        ServerResponse<GridParmInfo> sr = new ServerResponse<GridParmInfo>();
        for (NetCDFParm p : this.parms) {
            if (id.equals(p.getPid())) {
                GridParmInfo info = p.getGpi();
                sr.setPayload(info);
                return sr;
            }
        }

        sr.addMessage("Unknown PID: " + id);
        return sr;
    }

    @Override
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID id, List<TimeRange> trs) {
        Map<TimeRange, List<GridDataHistory>> history = new HashMap<TimeRange, List<GridDataHistory>>();
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = new ServerResponse<Map<TimeRange, List<GridDataHistory>>>();

        List<TimeRange> inventory = getGridInventory(id).getPayload();

        for (TimeRange time : trs) {
            if (inventory.contains(time)) {
                List<GridDataHistory> hist = new ArrayList<GridDataHistory>();
                hist.add(new GridDataHistory(
                        GridDataHistory.OriginType.INITIALIZED, id, time, null,
                        (WsId) null));
                history.put(time, hist);
            } else {
                sr.addMessage("Time Range is not in inventory: " + time);
                history.clear();
                return sr;
            }
        }
        sr.setPayload(history);
        return sr;
    }

    @Override
    public String getProjectionId() {
        String pid = this.outputGloc.getProjection().getProjectionID();
        return pid;
    }
}
