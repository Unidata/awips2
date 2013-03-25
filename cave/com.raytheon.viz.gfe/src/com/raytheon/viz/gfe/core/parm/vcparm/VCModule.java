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
package com.raytheon.viz.gfe.core.parm.vcparm;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Virtual Calculated Module. Serves as interface between the VCParm and the
 * actual python algorithms.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2011            dgilling     Initial creation
 * Jun 20, 2012  #766      dgilling     Refactor to improve
 *                                      performance.
 * Jan 22, 2013  #1515     dgilling     Fix ClassCastException in 
 *                                      getMethodArgs().
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class VCModule {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VCModule.class);

    public class DepParmInv {

        private ParmID parmID;

        private List<TimeRange> times;

        public DepParmInv(ParmID pid, List<TimeRange> trs) {
            this.parmID = pid;
            this.times = trs;
        }

        public ParmID getParmID() {
            return parmID;
        }

        public List<TimeRange> getTimes() {
            return times;
        }

        @Override
        public String toString() {
            StringBuilder tmp = new StringBuilder();
            tmp.append("(pid=");
            tmp.append(parmID.toString());
            tmp.append(",times=");
            tmp.append(times.toString());
            tmp.append(')');

            return tmp.toString();
        }
    }

    public class VCInventory {

        private TimeRange gridTimeRange;

        private List<DepParmInv> depParmInv;

        public VCInventory(TimeRange gridTimeRange, List<DepParmInv> depParmInv) {
            this.gridTimeRange = gridTimeRange;
            this.depParmInv = depParmInv;
        }

        public TimeRange getGridTimeRange() {
            return gridTimeRange;
        }

        public List<DepParmInv> getDepParmInv() {
            return depParmInv;
        }

        @Override
        public String toString() {
            StringBuilder tmp = new StringBuilder();
            tmp.append("gtr=");
            tmp.append(gridTimeRange.toString());
            tmp.append(",dpi=");
            tmp.append(depParmInv.toString());

            return tmp.toString();
        }
    }

    private Throwable error;

    private GridParmInfo gpi;

    private Collection<ParmID> depParms;

    private DataManager dataMgr;

    private IParmManager parmMgr;

    private String id;

    public VCModule(DataManager dataMgr, IParmManager parmMgr, final File module) {
        this.dataMgr = dataMgr;
        this.parmMgr = parmMgr;
        this.id = module.getName().split("\\.(?=[^\\.]+$)")[0];
        this.depParms = Collections.emptyList();
    }

    public void dispose() {
        // no-op
    }

    public boolean isValid() {
        return (this.error == null);
    }

    public Throwable getErrorString() {
        Throwable rVal = error;
        error = null;
        return rVal;
    }

    private List<String> getMethodArgs(String method) throws Throwable {
        // statusHandler.debug("getMethodArgs: " + id);

        Map<String, Object> args = new HashMap<String, Object>();
        args.put(PyConstants.METHOD_NAME, method);
        VCModuleRequest req = new VCModuleRequest(id, "getMethodArgs", args);
        parmMgr.getVCModulePool().enqueue(req);

        Object result = req.getResult();
        @SuppressWarnings("unchecked")
        List<String> argNames = (List<String>) result;
        return argNames;
    }

    public Collection<ParmID> dependentParms() {
        // this is a derivation from AWIPS1
        // like getGpi(), this should only ever need to be calculated once
        // since VCModule does not support dynamic updates.
        if (!depParms.isEmpty()) {
            return depParms;
        }

        try {
            Collection<String> parameters = getMethodArgs("getInventory");
            depParms = new ArrayList<ParmID>(parameters.size());
            for (String parmName : parameters) {
                ParmID pid = parmMgr.fromExpression(parmName);
                if (pid.isValid()) {
                    depParms.add(pid);
                } else {
                    throw new IllegalArgumentException(
                            "Can't find Weather Element for " + parmName);
                }
            }
        } catch (Throwable t) {
            error = t;
            // statusHandler.handle(Priority.DEBUG, "dependentParms: " + id
            // + " error", t);
            depParms = Collections.emptyList();
        }

        return depParms;
    }

    private long[] encodeTR(final TimeRange tr) {
        long[] ptime = { tr.getStart().getTime(), tr.getEnd().getTime() };
        return ptime;
    }

    private List<long[]> pyInventory(Parm p) {
        IGridData[] gridInv = p.getGridInventory();
        List<long[]> inv = new ArrayList<long[]>(gridInv.length);
        for (IGridData gd : gridInv) {
            inv.add(encodeTR(gd.getGridTime()));
        }

        return inv;
    }

    private Object[] encodeGD(IGridData gd) {
        Object[] item = new Object[3];
        item[0] = encodeTR(gd.getGridTime());

        // since we have to go through a bunch of hoops in VCModuleController to
        // get the IGridData in python-useable format, no need doing anything
        // here but storing the data
        item[1] = gd;

        // add a mask indicating the set of valid points. Note for all data
        // other than ISC data, the mask is all points. ISC data values depend
        // upon the masking of the Grid Data History.
        Grid2DBit validBits;
        if (gd.getParm().isIscParm()) {
            List<String> hs = gd.getHistorySites();
            validBits = dataMgr.getRefManager().siteGridpoints(hs, true);
        } else {
            validBits = dataMgr.getRefManager().fullRefSet().getGrid();
        }
        item[2] = validBits;

        return item;
    }

    @SuppressWarnings("unchecked")
    private IGridSlice decodeGD(Object o, VCInventory invEntry)
            throws GFEException, JepException {
        TimeRange tr = invEntry.getGridTimeRange();
        List<GridDataHistory> gdh = calcHistory(invEntry);
        if (!isValid()) {
            throw new GFEException(getErrorString());
        }

        GridParmInfo gpi = getGpi();
        switch (gpi.getGridType()) {
        case SCALAR:
            return new ScalarGridSlice(tr, gpi, gdh, (Grid2DFloat) o);
        case VECTOR:
            Grid2DFloat[] s = (Grid2DFloat[]) o;
            return new VectorGridSlice(tr, gpi, gdh, s[0], s[1]);
        case WEATHER:
            Object[] wxGrid = (Object[]) o;
            List<WeatherKey> key = new ArrayList<WeatherKey>();
            List<String> pkey = (List<String>) wxGrid[1];
            for (String wKey : pkey) {
                key.add(new WeatherKey(gpi.getParmID().getDbId().getSiteId(),
                        wKey));
            }
            return new WeatherGridSlice(tr, gpi, gdh, (Grid2DByte) wxGrid[0],
                    key);
        case DISCRETE:
            Object[] discGrid = (Object[]) o;
            List<DiscreteKey> keys = new ArrayList<DiscreteKey>();
            List<String> pkeys = (List<String>) discGrid[1];
            for (String dKey : pkeys) {
                keys.add(new DiscreteKey(gpi.getParmID().getDbId().getSiteId(),
                        dKey, gpi.getParmID()));
            }
            return new DiscreteGridSlice(tr, gpi, gdh,
                    (Grid2DByte) discGrid[0], keys);
        default:
            statusHandler.handle(Priority.EVENTB, "Unknown parm type: "
                    + gpi.getGridType().toString());
            break;
        }

        return null;
    }

    @SuppressWarnings("unchecked")
    private TimeRange decodeTR(Object o) {
        List<Long> ptr = (List<Long>) o;
        return new TimeRange(ptr.get(0), ptr.get(1));
    }

    @SuppressWarnings("unchecked")
    public List<VCInventory> getInventory() {
        // statusHandler.debug("getInventory: " + id);
        final List<VCInventory> rval = new ArrayList<VCInventory>();

        try {
            List<String> args = getMethodArgs("getInventory");
            Map<String, Object> cargs = new HashMap<String, Object>(args.size());
            List<Parm> pargs = new ArrayList<Parm>(args.size());

            for (String arg : args) {
                ParmID pid = parmMgr.fromExpression(arg);
                Parm tp = parmMgr.getParm(pid);
                if (tp == null) {
                    throw new IllegalArgumentException(
                            "Can't locate Weather Element: " + arg);
                }
                cargs.put(arg, new GetVcModInventoryArg(pyInventory(tp)));
                pargs.add(tp);
            }

            VCModuleRequest req = new VCModuleRequest(id, "getInventory", cargs);
            parmMgr.getVCModulePool().enqueue(req);
            Object reqResult = req.getResult();

            // what's returned from the script here is a list of tuples.
            // Each tuple contains:
            // 1. A TimeRange.
            // 2. A list of TimeRanges that correspond to the Fcst grids that
            // should be used for that time
            // 3. A list of TimeRanges that correspond to the ISC grids that
            // should be used for that time
            List<List<Object>> result = (List<List<Object>>) reqResult;
            for (List<Object> item : result) {
                List<DepParmInv> dpi = new ArrayList<DepParmInv>();
                for (int i = 1; i < item.size(); i++) {
                    List<Object> ditem = (List<Object>) item.get(i);
                    List<TimeRange> trs = new ArrayList<TimeRange>(ditem.size());
                    for (Object tr : ditem) {
                        trs.add(decodeTR(tr));
                    }
                    dpi.add(new DepParmInv(pargs.get(i - 1).getParmID(), trs));
                }
                rval.add(new VCInventory(decodeTR(item.get(0)), dpi));
            }

        } catch (Throwable t) {
            error = t;
        }

        return rval;
    }

    private List<String> encodeGDH(final List<GridDataHistory> gdh) {
        List<String> l = new ArrayList<String>(gdh.size());
        for (GridDataHistory hist : gdh) {
            l.add(hist.getCodedString());
        }

        return l;
    }

    @SuppressWarnings("unchecked")
    private List<GridDataHistory> decodeGDH(final Object o) {
        List<String> s = (List<String>) o;
        List<GridDataHistory> rval = new ArrayList<GridDataHistory>(s.size());

        for (String entry : s) {
            GridDataHistory gdh = new GridDataHistory(entry);
            rval.add(gdh);
        }

        return rval;
    }

    @SuppressWarnings("unchecked")
    public List<GridDataHistory> calcHistory(final VCInventory invEntry) {
        // statusHandler.debug("calcHistory: " + id + " " +
        // invEntry.toString());

        // commenting out this python call because it is completely
        // superfluous--all the baseline VCMODULE files have a calcHistory
        // method so there's no point in checking and it saves a call into the
        // VCModuleJobPool queue. If at some point there's a desire to support
        // user/site-defined modules, this check should probably return.
        // TODO: Reimplement using a call to BaseGfePyController.hasMethod().

        // boolean hasattr = false;
        // try {
        // hasattr = module.hasAttr("calcHistory");
        // } catch (JepException e) {
        // // swallow exception and move on
        // statusHandler.handle(Priority.EVENTB,
        // "Could not retrieve calcHistory attribute", e);
        // }

        // if (hasattr) {
        try {
            final List<DepParmInv> dpi = invEntry.getDepParmInv();
            List<String> args = getMethodArgs("calcHistory");
            Map<String, Object> cargs = new HashMap<String, Object>(args.size());

            for (String arg : args) {
                Parm tp = parmMgr.getParm(parmMgr.fromExpression(arg));
                if (tp != null) {
                    List<Object[]> l = new ArrayList<Object[]>();
                    for (DepParmInv dpiEntry : dpi) {
                        if (dpiEntry.getParmID().equals(tp.getParmID())) {
                            for (TimeRange tr : dpiEntry.getTimes()) {
                                IGridData gd = tp
                                        .overlappingGrid(tr.getStart());
                                if (gd == null) {
                                    throw new IllegalArgumentException(
                                            "Unable to retrieve overlapping grid for parm: "
                                                    + tp.getParmID().toString()
                                                    + " for time: "
                                                    + tr.toString());
                                }

                                Object[] tl = new Object[2];
                                tl[0] = encodeTR(gd.getGridTime());
                                tl[1] = encodeGDH(Arrays
                                        .asList(gd.getHistory()));
                                l.add(tl);
                            }
                        }
                    }

                    cargs.put(arg, new CalcVcModHistoryArg(l));
                } else {
                    throw new IllegalArgumentException("Unable to find parm "
                            + arg);
                }
            }

            VCModuleRequest req = new VCModuleRequest(id, "calcHistory", cargs);
            parmMgr.getVCModulePool().enqueue(req);
            Object reqResult = req.getResult();

            List<String> result = (List<String>) reqResult;
            return decodeGDH(result);

        } catch (Throwable t) {
            error = t;
        }
        // }

        // the default
        return Arrays.asList(new GridDataHistory(OriginType.CALCULATED,
                getGpi().getParmID(), invEntry.getGridTimeRange()));
    }

    public IGridSlice calcGrid(VCInventory invEntry) {
        // statusHandler.debug("calcGrid: " + id + " " + invEntry.toString());

        final List<DepParmInv> dpi = invEntry.getDepParmInv();

        try {
            List<String> args = getMethodArgs("calcGrid");
            Map<String, Object> cargs = new HashMap<String, Object>(args.size());

            for (String arg : args) {
                ParmID id = parmMgr.fromExpression(arg);
                Parm tp = parmMgr.getParm(id);
                if (tp != null) {
                    List<Object[]> l = new ArrayList<Object[]>();

                    for (DepParmInv dpiEntry : dpi) {
                        if (dpiEntry.getParmID().equals(tp.getParmID())) {
                            for (TimeRange tr : dpiEntry.getTimes()) {
                                IGridData gd = tp
                                        .overlappingGrid(tr.getStart());
                                if (gd == null) {
                                    throw new IllegalArgumentException(
                                            "Unable to retrieve overlapping grid for parm: "
                                                    + tp.getParmID().toString()
                                                    + " for time: "
                                                    + tr.toString());
                                }

                                l.add(encodeGD(gd));
                            }
                        }
                    }

                    cargs.put(arg, new CalcVcModGridArg(l));
                } else {
                    throw new IllegalArgumentException("Unable to find parm "
                            + arg);
                }
            }

            VCModuleRequest req = new VCModuleRequest(id, "calcGrid", cargs,
                    getGpi().getGridType());
            parmMgr.getVCModulePool().enqueue(req);
            Object reqResult = req.getResult();

            return decodeGD(reqResult, invEntry);

        } catch (Throwable t) {
            error = t;
        }

        return null;
    }

    @SuppressWarnings("unchecked")
    public GridParmInfo getGpi() {
        if (this.gpi != null) {
            return this.gpi;
        }
        // statusHandler.debug("getGpi(): " + id);

        try {
            VCModuleRequest req = new VCModuleRequest(id, "getWEInfo", null);
            parmMgr.getVCModulePool().enqueue(req);
            Object reqResult = req.getResult();

            List<List<Object>> result = (List<List<Object>>) reqResult;

            List<Object> s = result.get(1);
            String site = parmMgr.getMutableDatabase().getSiteId();
            DatabaseID dbid = new DatabaseID(site, DataType.GRID, s.get(1)
                    .toString(), s.get(0).toString(), DatabaseID.NO_MODEL_TIME);

            s = result.get(2);
            TimeConstraints tc = new TimeConstraints((Integer) s.get(0),
                    (Integer) s.get(1), (Integer) s.get(2));

            s = result.get(0);
            ParmID pid = new ParmID(s.get(0).toString(), dbid);
            GridType type = GridType.NONE;
            try {
                type = GridType.valueOf(s.get(1).toString().toUpperCase());
            } catch (IllegalArgumentException e) {
                // just want to swallow the exception here as we've already
                // defaulted type to NONE
                statusHandler.handle(Priority.EVENTB,
                        "Invalid GridType specified: " + s.get(1).toString());
            }

            // Double check for all the various valid parm lengths from
            // serverConfig
            GridParmInfo gpi = new GridParmInfo(pid,
                    parmMgr.compositeGridLocation(), type, s.get(2).toString(),
                    s.get(3).toString(), (Float) s.get(5), (Float) s.get(4),
                    (Integer) s.get(6), false, tc, (Boolean) s.get(7));
            this.gpi = gpi;

        } catch (Throwable t) {
            error = t;
        }

        return this.gpi;
    }
}
