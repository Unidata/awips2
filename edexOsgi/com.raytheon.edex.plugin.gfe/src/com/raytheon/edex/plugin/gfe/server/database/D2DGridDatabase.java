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
import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.db.dao.GFEDao;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.paraminfo.GridParamInfoLookup;
import com.raytheon.edex.plugin.gfe.paraminfo.ParameterInfo;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
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
import com.raytheon.uf.common.dataplugin.grid.GridPathProvider;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Singleton that assists with grid data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 05/16/08     875         bphillip    Initial Creation
 * 06/17/08     #940        bphillip    Implemented GFE Locking
 * 07/23/09     2342        ryu         Check for null gridConfig in getGridParmInfo
 * 03/02/12     DR14651     ryu         change time independency of staticTopo, staticCoriolis, staticSpacing
 * 05/04/12     #574        dgilling    Implement missing methods from GridDatabase.
 * 09/12/12     #1117       dgilling    Fix getParmList() so it returns all parms defined
 *                                      in the GribParamInfo file.
 * 10/10/12     #1260       randerso    Changed to only retrieve slab containing overlapping
 *                                      data instead of full grid. Added logging to support
 *                                      GFE performance testing
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class D2DGridDatabase extends VGridDatabase {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DGridDatabase.class);

    // separate logger for GFE performance logging
    private static final IUFStatusHandler gfePerformanceLogger = UFStatus
            .getNamedHandler("GFEPerformanceLogger");

    /** The remap object used for resampling grids */
    private final Map<Integer, RemapGrid> remap = new HashMap<Integer, RemapGrid>();

    /** The destination GridLocation (The local GFE grid coverage) */
    private GridLocation outputLoc;

    private final List<ParmID> parms;

    public static final String GRID_LOCATION_CACHE_KEY = "GfeLocations";

    /**
     * Constructs a new D2DGridDatabase
     * 
     * @param dbId
     *            The database ID of this database
     */
    public D2DGridDatabase(IFPServerConfig config, DatabaseID dbId)
            throws GfeException {
        super(config);
        this.dbId = dbId;
        this.parms = new ArrayList<ParmID>();
        valid = this.dbId.isValid();

        if (valid) {
            loadParms();
            outputLoc = this.config.dbDomain();
        }
    }

    private RemapGrid getOrCreateRemap(GridCoverage awipsGrid) {
        RemapGrid remap = this.remap.get(awipsGrid.getId());
        if (remap == null) {
            String gfeModelName = dbId.getModelName();
            String d2dModelName = this.config.d2dModelNameMapping(gfeModelName);
            GridLocation inputLoc = new GridLocation(d2dModelName, awipsGrid);
            inputLoc.setSiteId(d2dModelName);
            Rectangle subdomain = NetCDFUtils.getSubGridDims(inputLoc,
                    this.outputLoc);

            // fix up coordinates for 0,0 in upper left in A2
            subdomain.y = inputLoc.gridSize().y - subdomain.y
                    - subdomain.height;

            if (subdomain.isEmpty()) {
                statusHandler.warn(this.dbId
                        + ": GFE domain does not overlap dataset domain.");
            } else {
                GridLocation subGloc = new GridLocation(dbId.toString(),
                        inputLoc, subdomain);
                remap = new RemapGrid(subGloc, this.outputLoc);
                this.remap.put(awipsGrid.getId(), remap);
            }

        }
        return remap;
    }

    @Override
    public void updateDbs() {
        // no op
    }

    /**
     * Loads all of the parms it can.
     */
    private void loadParms() {
        String gribModelName = config.d2dModelNameMapping(dbId.getModelName());
        Collection<String> parmNames = new HashSet<String>(GridParamInfoLookup
                .getInstance().getParmNames(gribModelName));

        // first see if we can make wind...
        if ((parmNames.contains("uw")) && (parmNames.contains("vw"))) {
            List<String> uLevels = GridParamInfoLookup.getInstance()
                    .getParameterInfo(gribModelName, "uw").getLevels();
            List<String> vLevels = GridParamInfoLookup.getInstance()
                    .getParameterInfo(gribModelName, "vw").getLevels();
            for (String level : uLevels) {
                if (vLevels.contains(level)) {
                    parms.add(new ParmID("wind", dbId, level));
                }
            }
            parmNames.remove("uw");
            parmNames.remove("vw");
        } else if ((parmNames.contains("ws")) && (parmNames.contains("wd"))) {
            List<String> sLevels = GridParamInfoLookup.getInstance()
                    .getParameterInfo(gribModelName, "ws").getLevels();
            List<String> dLevels = GridParamInfoLookup.getInstance()
                    .getParameterInfo(gribModelName, "wd").getLevels();
            for (String level : sLevels) {
                if (dLevels.contains(level)) {
                    parms.add(new ParmID("wind", dbId, level));
                }
            }
            parmNames.remove("ws");
            parmNames.remove("wd");
        }

        // Now do all the scalars
        for (String parmName : parmNames) {
            List<String> levels = GridParamInfoLookup.getInstance()
                    .getParameterInfo(gribModelName, parmName).getLevels();
            if (levels.isEmpty()) {
                levels = Arrays.asList("Dflt");
            }
            for (String level : levels) {
                parms.add(new ParmID(parmName, dbId, level));
            }
        }
    }

    @Override
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID id) {
        List<TimeRange> trs = new ArrayList<TimeRange>();
        ServerResponse<List<TimeRange>> sr = new ServerResponse<List<TimeRange>>();
        GFEDao dao = null;
        try {
            dao = (GFEDao) PluginFactory.getInstance().getPluginDao("gfe");
        } catch (PluginException e1) {
            statusHandler
                    .handle(Priority.PROBLEM, "Unable to get GFE dao!", e1);
        }
        try {
            List<TimeRange> d2dTimes = dao.getD2DTimes(id);
            try {
                trs = getTimeRange(id, d2dTimes);
            } catch (IllegalArgumentException e) {
                // Ignore
            }
            sr.setPayload(trs);
            return sr;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving D2D grid inventory from Database", e);
            sr.addMessage("Error retrieving D2D grid inventory from Database");
            return sr;
        }
    }

    public boolean isParmInfoDefined(ParmID id) {
        String mappedModel = config.d2dModelNameMapping(id.getDbId()
                .getModelName());
        return GridParamInfoLookup.getInstance().getParameterInfo(mappedModel,
                id.getParmName().toLowerCase()) != null;
    }

    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID id) {

        ServerResponse<GridParmInfo> sr = new ServerResponse<GridParmInfo>();
        GridParmInfo gpi = null;
        String mappedModel = config.d2dModelNameMapping(id.getDbId()
                .getModelName());

        if (id.getParmName().equalsIgnoreCase("wind")) {
            List<TimeRange> modelTimes = GridParamInfoLookup
                    .getInstance()
                    .getParameterTimes(mappedModel, id.getDbId().getModelDate());
            TimeConstraints tc = getTimeConstraints(modelTimes);

            // first try getting u-component attributes
            ParameterInfo atts = GridParamInfoLookup.getInstance()
                    .getParameterInfo(mappedModel, "uw");

            // if not found try wind speed
            if (atts == null) {
                atts = GridParamInfoLookup.getInstance().getParameterInfo(
                        mappedModel, "ws");
            }
            float minV = 0;
            float maxV = atts.getValid_range()[1];
            int precision = calcPrecision(minV, maxV);
            gpi = new GridParmInfo(id, this.outputLoc, GridType.VECTOR,
                    atts.getUnits(), "wind", minV, maxV, precision, false, tc,
                    false);
            sr.setPayload(gpi);
            return sr;

        }

        ParameterInfo atts = GridParamInfoLookup.getInstance()
                .getParameterInfo(mappedModel, id.getParmName());

        if (atts == null) {
            if (gpi == null) {
                TimeConstraints tc = new TimeConstraints(3600, 3600, 0);
                gpi = new GridParmInfo(id, this.outputLoc, GridType.SCALAR, "",
                        "", 0, 10000, 0, false, tc, false);
            }

        } else {
            boolean accParm = false;
            List<String> accumParms = config.accumulativeD2DElements(dbId
                    .getModelName());
            if (accumParms != null) {
                if (accumParms.contains(atts.getShort_name())) {
                    accParm = true;
                }
            }

            boolean rateParm = false;
            // List<TimeRange> times = this.getGridInventory(id).getPayload();
            List<TimeRange> times = GridParamInfoLookup
                    .getInstance()
                    .getParameterTimes(mappedModel, id.getDbId().getModelDate());
            TimeConstraints tc = getTimeConstraints(times);
            if (accParm) {
                tc = new TimeConstraints(tc.getRepeatInterval(),
                        tc.getRepeatInterval(), tc.getStartTime());
                rateParm = true;
            }

            float minV = -30;
            float maxV = 10000;

            if (atts.getValid_range() != null) {
                minV = atts.getValid_range()[0];
                maxV = atts.getValid_range()[1];
            } else {
                // This is the CDF convention. But we can't use
                // it or the GFE will attempt to create billions and
                // billions of contours.
                // min = MINFLOAT;
                // max = MAXFLOAT;
                minV = 0;
                maxV = 10000;
                if (!id.getParmName().equals("staticTopo")
                        && !id.getParmName().equals("staticSpacing")
                        && !id.getParmName().equals("staticCoriolis")) {
                    statusHandler.handle(Priority.VERBOSE,
                            "[valid_range] or [valid_min] or [valid_max] "
                                    + "not found for " + id.toString());
                }
            }

            int precision = calcPrecision(minV, maxV);
            gpi = new GridParmInfo(id, this.outputLoc, GridType.SCALAR,
                    atts.getUnits(), atts.getLong_name(), minV, maxV,
                    precision, false, tc, rateParm);
        }

        sr.setPayload(gpi);
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
    public ServerResponse<List<ParmID>> getParmList() {
        ServerResponse<List<ParmID>> sr = new ServerResponse<List<ParmID>>();
        List<ParmID> parmIds = new ArrayList<ParmID>(parms);
        sr.setPayload(parmIds);
        return sr;
    }

    @Override
    public String getProjectionId() {
        return this.outputLoc.getProjection().getProjectionID();
    }

    @Override
    public ServerResponse<List<IGridSlice>> getGridData(ParmID id,
            List<TimeRange> timeRanges) {

        List<IGridSlice> data = new ArrayList<IGridSlice>(timeRanges.size());
        ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();
        for (TimeRange tr : timeRanges) {
            GridParmInfo gpi = getGridParmInfo(id).getPayload();
            try {
                data.add(getGridSlice(id, gpi, tr, false));
            } catch (GfeException e) {
                sr.addMessage("Error getting grid slice for ParmID: " + id
                        + " TimeRange: " + tr);
                statusHandler.handle(Priority.PROBLEM,
                        "Error getting grid slice for ParmID: " + id
                                + " TimeRange: " + tr, e);
            }
        }
        sr.setPayload(data);
        return sr;
    }

    public ServerResponse<List<IGridSlice>> getGridData(ParmID id,
            List<TimeRange> timeRanges, boolean convertUnit) {

        List<IGridSlice> data = new ArrayList<IGridSlice>(timeRanges.size());
        ServerResponse<List<IGridSlice>> sr = new ServerResponse<List<IGridSlice>>();
        for (TimeRange tr : timeRanges) {
            GridParmInfo gpi = getGridParmInfo(id).getPayload();
            try {
                data.add(getGridSlice(id, gpi, tr, convertUnit));
            } catch (GfeException e) {
                sr.addMessage("Error getting grid slice for ParmID: " + id
                        + " TimeRange: " + tr);
                statusHandler.handle(Priority.PROBLEM,
                        "Error getting grid slice for ParmID: " + id
                                + " TimeRange: " + tr, e);
            }
        }

        sr.setPayload(data);
        return sr;
    }

    /**
     * Gets a grid slice based on Parm information provided
     * 
     * @param parmId
     *            The parmID for the grid slice
     * @param gpi
     *            The associated grid parm info
     * @param time
     *            The time range of the data
     * @return The grid slice containing the data
     * @throws GfeException
     *             If the grid slice cannot be constructed
     */
    private IGridSlice getGridSlice(ParmID parmId, GridParmInfo gpi,
            TimeRange time, boolean convertUnit) throws GfeException {
        IGridSlice gs = null;
        GridDataHistory[] gdh = { new GridDataHistory(
                GridDataHistory.OriginType.INITIALIZED, parmId, time, null,
                (WsId) null) };

        switch (gpi.getGridType()) {
        case SCALAR:
            Grid2DFloat data = null;
            if (this.remap == null) {
                // GFE domain does not overlap D2D grid, return default grid
                data = new Grid2DFloat(gpi.getGridLoc().getNx(), gpi
                        .getGridLoc().getNy(), gpi.getMinValue());
            } else {
                data = getGrid(parmId, time, gpi, convertUnit);
            }
            gs = new ScalarGridSlice(time, gpi, gdh, data);
            break;
        case VECTOR:
            Grid2DFloat mag = new Grid2DFloat(gpi.getGridLoc().getNx(), gpi
                    .getGridLoc().getNy());
            Grid2DFloat dir = new Grid2DFloat(gpi.getGridLoc().getNx(), gpi
                    .getGridLoc().getNy());

            if (this.remap == null) {
                // GFE domain does not overlap D2D grid, return default grid
                mag.setAllValues(gpi.getMinValue());
                dir.setAllValues(0.0f);
            } else {
                getWindGrid(parmId, time, gpi, mag, dir);
            }
            gs = new VectorGridSlice(time, gpi, gdh, mag, dir);
            break;
        default:
            statusHandler.handle(Priority.PROBLEM,
                    "Unsupported parm type for: " + gpi);
            break;

        }

        return gs;
    }

    /**
     * Returns a Grid2DFloat for the specified parm information
     * 
     * @param parmId
     *            The parmID of the data
     * @param time
     *            The time range of the data
     * @param gpi
     *            The grid parm information associated with the data
     * @return The raw data
     * @throws GfeException
     *             If the grid data cannot be retrieved
     */
    private Grid2DFloat getGrid(ParmID parmId, TimeRange time,
            GridParmInfo gpi, boolean convertUnit) throws GfeException {

        Grid2DFloat bdata = null;
        GridRecord d2dRecord = null;

        long t0 = System.currentTimeMillis();
        GFEDao dao = null;
        try {
            dao = (GFEDao) PluginFactory.getInstance().getPluginDao("gfe");
        } catch (PluginException e1) {
            throw new GfeException("Unable to get GFE dao!", e1);
        }

        try {
            // Gets the metadata from the grib metadata database
            d2dRecord = dao.getD2DGrid(parmId, time, gpi);
        } catch (DataAccessLayerException e) {
            throw new GfeException(
                    "Error retrieving D2D Grid record from database", e);
        }
        long t1 = System.currentTimeMillis();

        if (d2dRecord == null) {
            throw new GfeException("No data available for " + parmId
                    + " for time range " + time);
        }

        // Gets the raw data from the D2D grib HDF5 file
        bdata = getRawGridData(d2dRecord);
        long t2 = System.currentTimeMillis();

        float fillV = Float.MAX_VALUE;
        ParameterInfo atts = GridParamInfoLookup.getInstance()
                .getParameterInfo(
                        config.d2dModelNameMapping(parmId.getDbId()
                                .getModelName()), parmId.getParmName());
        if (atts != null) {
            fillV = atts.getFillValue();
        }

        // Resample the data to fit desired region
        Grid2DFloat retVal;
        try {
            RemapGrid remap = getOrCreateRemap(d2dRecord.getLocation());
            retVal = remap.remap(bdata, fillV, gpi.getMaxValue(),
                    gpi.getMinValue(), gpi.getMinValue());
            if (convertUnit && (d2dRecord != null)) {
                convertUnits(d2dRecord, retVal, gpi.getUnitObject());
            }
        } catch (Exception e) {
            throw new GfeException("Unable to get Grid", e);
        }
        long t3 = System.currentTimeMillis();

        if (gfePerformanceLogger.isPriorityEnabled(Priority.DEBUG)) {
            gfePerformanceLogger.handle(Priority.DEBUG,
                    "D2DGridDatabase.getGrid" + //
                            " metaData: " + (t1 - t0) + //
                            " hdf5: " + (t2 - t1) + //
                            " remap: " + (t3 - t2) + //
                            " total: " + (t3 - t0));
        }

        return retVal;

    }

    /**
     * Converts the data from the native unit found in the grib file to the
     * desired unit found in the grid parm info
     * 
     * @param d2dRecord
     *            The grib metadata containing the original unit information
     * @param data
     *            The float data to convert
     * @param gpi
     *            The grid parm info containing the target unit information
     * @throws GfeException
     *             If the source and target units are incompatible
     */
    private void convertUnits(GridRecord d2dRecord, Grid2DFloat data,
            Unit<?> targetUnit) throws GfeException {

        Unit<?> sourceUnit = d2dRecord.getParameter().getUnit();
        if (sourceUnit.equals(targetUnit)) {
            return;
        }

        if (!sourceUnit.isCompatible(targetUnit)) {
            return;
        }

        if (!sourceUnit.equals(targetUnit)) {
            UnitConverter converter = sourceUnit.getConverterTo(targetUnit);

            FloatBuffer dataArray = data.getBuffer();
            for (int i = 0; i < dataArray.capacity(); i++) {
                dataArray.put((float) converter.convert(dataArray.get(i)));
            }
        }
    }

    /**
     * Constructs a magnitude and direction grids for wind data
     * 
     * @param parmId
     *            The parmID of the data
     * @param time
     *            The time range of the data
     * @param gpi
     *            The grid parm info for the data
     * @param mag
     *            The resulting wind magnitude grid
     * @param dir
     *            The resulting wind direction grid
     * @throws GfeException
     *             The the wind grids cannot be constructed
     */
    private void getWindGrid(ParmID parmId, TimeRange time, GridParmInfo gpi,
            Grid2DFloat mag, Grid2DFloat dir) throws GfeException {

        GFEDao dao = null;
        try {
            dao = (GFEDao) PluginFactory.getInstance().getPluginDao("gfe");
        } catch (PluginException e1) {
            throw new GfeException("Unable to get GFE dao!!", e1);
        }
        GridRecord uRecord = null;
        GridRecord vRecord = null;
        GridRecord sRecord = null;
        GridRecord dRecord = null;
        try {

            // Get the metadata from the grib metadata database
            uRecord = dao.getD2DGrid(
                    new ParmID("uW", this.dbId, parmId.getParmLevel()), time,
                    gpi);
            vRecord = dao.getD2DGrid(
                    new ParmID("vW", this.dbId, parmId.getParmLevel()), time,
                    gpi);
        } catch (DataAccessLayerException e) {
            throw new GfeException(
                    "Unable to retrieve wind grids from D2D database", e);
        }

        String mappedModel = config.d2dModelNameMapping(dbId.getModelName());

        if ((uRecord != null) && (vRecord != null)) {
            // Gets the raw grid data from the D2D grib HDF5 files
            Grid2DFloat uData = getRawGridData(uRecord);
            Grid2DFloat vData = getRawGridData(vRecord);

            // Resample the data to fit the desired region
            float fillV = Float.MAX_VALUE;
            ParameterInfo pa = GridParamInfoLookup.getInstance()
                    .getParameterInfo(mappedModel, "uw");
            if (pa != null) {
                fillV = pa.getFillValue();
            }

            try {
                RemapGrid remap = getOrCreateRemap(uRecord.getLocation());
                remap.remapUV(uData, vData, fillV, gpi.getMaxValue(),
                        gpi.getMinValue(), gpi.getMinValue(), true, true, mag,
                        dir);
            } catch (Exception e) {
                throw new GfeException("Unable to remap UV wind grids", e);
            }
            return;
        } else {
            try {

                // Get the metadata from the grib metadata database
                sRecord = dao.getD2DGrid(
                        new ParmID("ws", this.dbId, parmId.getParmLevel()),
                        time, gpi);
                dRecord = dao.getD2DGrid(
                        new ParmID("wd", this.dbId, parmId.getParmLevel()),
                        time, gpi);
            } catch (DataAccessLayerException e) {
                throw new GfeException(
                        "Unable to retrieve wind grids from D2D database", e);
            }

            if ((sRecord != null) && (dRecord != null)) {
                // Gets the raw grid data from the D2D grib HDF5 files
                Grid2DFloat sData = getRawGridData(sRecord);
                Grid2DFloat dData = getRawGridData(dRecord);

                // Resample the data to fit the desired region
                float fillV = Float.MAX_VALUE;
                ParameterInfo pa = GridParamInfoLookup.getInstance()
                        .getParameterInfo(mappedModel, "ws");
                if (pa != null) {
                    fillV = pa.getFillValue();
                }

                try {
                    RemapGrid remap = getOrCreateRemap(sRecord.getLocation());
                    remap.remap(sData, dData, fillV, gpi.getMaxValue(),
                            gpi.getMinValue(), gpi.getMinValue(), mag, dir);
                } catch (Exception e) {
                    throw new GfeException("Unable to remap wind grids", e);
                }
                return;
            }
        }

        String message = "Wind record";
        if (uRecord == null) {
            message += " uW";
        }
        if (vRecord == null) {
            message += " vW";
        }
        if (sRecord == null) {
            message += " ws";
        }
        if (dRecord == null) {
            message += " wd";
        }
        message += " is null for db " + dbId.toString() + ", level "
                + parmId.getParmLevel() + ", time " + time.toString();
        throw new GfeException(message);
    }

    /**
     * Gets the raw data from the D2D HDF5 repository
     * 
     * @param d2dRecord
     *            The grib metadata
     * @return The raw data
     * @throws GfeException
     */
    private Grid2DFloat getRawGridData(GridRecord d2dRecord)
            throws GfeException {
        try {
            GridDao dao = new GridDao();
            // reimplementing this call here with subgrid support
            // dao.getHDF5Data(d2dRecord, -1);
            // TODO should we add subgrid support to GribDao or PluginDao
            IDataStore dataStore = dao.getDataStore(d2dRecord);

            GridLocation gloc = getOrCreateRemap(d2dRecord.getLocation())
                    .getSourceGloc();

            String abbrev = d2dRecord.getParameter().getAbbreviation();
            String group, dataset;
            if (GridPathProvider.STATIC_PARAMETERS.contains(abbrev)) {
                group = "/" + d2dRecord.getLocation().getId();
                dataset = abbrev;
            } else {
                group = d2dRecord.getDataURI();
                dataset = DataStoreFactory.DEF_DATASET_NAME;
            }

            IDataRecord record = dataStore.retrieve(group, dataset, Request
                    .buildSlab(
                            new int[] { (int) Math.floor(gloc.getOrigin().x),
                                    (int) Math.floor(gloc.getOrigin().y), },
                            new int[] {
                                    (int) Math.ceil(gloc.getOrigin().x
                                            + gloc.getExtent().x),
                                    (int) Math.ceil(gloc.getOrigin().y
                                            + gloc.getExtent().y), }));

            FloatDataRecord hdf5Record = (FloatDataRecord) record;
            return new Grid2DFloat((int) hdf5Record.getSizes()[0],
                    (int) hdf5Record.getSizes()[1], hdf5Record.getFloatData());

        } catch (Exception e) {
            throw new GfeException("Error retrieving hdf5 record. "
                    + e.getLocalizedMessage(), e);
        }
    }

    /**
     * Assigns the appropriate time range to the inventory
     * 
     * @param parmName
     *            The parm name
     * @param inventory
     *            The inventory of times to examine
     * @return The inventory with corrected time ranges
     * @throws GfeException
     *             If time ranges cannot be reassigned
     */
    private List<TimeRange> getTimeRange(ParmID id, List<TimeRange> inventory)
            throws GfeException {
        String parmName = id.getParmName();
        List<TimeRange> times = GridParamInfoLookup
                .getInstance()
                .getParameterTimes(
                        config.d2dModelNameMapping(id.getDbId().getModelName()),
                        id.getDbId().getModelDate());
        TimeConstraints tc = this.getTimeConstraints(times);
        if (config.accumulativeD2DElements(dbId.getModelName()).contains(
                parmName)) {
            tc = new TimeConstraints(tc.getRepeatInterval(),
                    tc.getRepeatInterval(), tc.getStartTime());
        }

        if ((inventory.size() > 1)
                && (parmName.equals("staticTopo")
                        || parmName.equals("staticSpacing") || parmName
                        .equals("staticCoriolis"))) {
            TimeRange ntr = new TimeRange(
                    inventory.get(0).getStart().getTime(), inventory
                            .get(inventory.size() - 1).getEnd().getTime()
                            + Util.MILLI_PER_HOUR);
            inventory.clear();
            inventory.add(ntr);
        } else {
            List<TimeRange> newInventory = new ArrayList<TimeRange>(
                    inventory.size());
            for (TimeRange tr : inventory) {
                if (isNonAccumDuration(id, inventory) && tr.isValid()
                        && !GFEDao.isMos(id)) {
                    newInventory.add(tc.constraintTime(tr.getEnd()));
                } else {
                    newInventory.add(tc.constraintTime(tr.getStart()));
                }
            }
            inventory = newInventory;
        }

        return inventory;
    }

    /**
     * Gets time constraints based on an ordered list of times
     * 
     * @param times
     *            The times
     * @return The time constraints
     */
    private TimeConstraints getTimeConstraints(List<TimeRange> times) {

        if (times.size() <= 1) {
            return new TimeConstraints(3600, 3600, 0);
        }

        long repeat = (times.get(1).getStart().getTime() - times.get(0)
                .getStart().getTime()) / 1000;
        long start = (times.get(0).getStart().getTime() / 1000) % 86400;

        for (int i = 1; i < times.size() - 1; i++) {
            if (((times.get(i + 1).getStart().getTime() - times.get(i)
                    .getStart().getTime()) / 1000) != repeat) {
                return new TimeConstraints(3600, 3600, 0);
            }
        }
        return new TimeConstraints(3600, (int) repeat, (int) start);
    }

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
        GFEDao dao = null;
        try {
            dao = (GFEDao) PluginFactory.getInstance().getPluginDao("gfe");
        } catch (PluginException e) {
            throw new GfeException("Unable to get GFE dao!!", e);
        }

        List<Integer> fcstTimes = dao.getD2DForecastTimes(dbId);
        SortedSet<Date> validTimes = new TreeSet<Date>();
        Calendar validTimeCalc = Calendar.getInstance();
        Date refTime = dbId.getModelTimeAsDate();
        for (Integer fcstTime : fcstTimes) {
            validTimeCalc.setTime(refTime);
            validTimeCalc.add(Calendar.SECOND, fcstTime.intValue());
            validTimes.add(validTimeCalc.getTime());
        }
        return validTimes;
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

    public static boolean isNonAccumDuration(ParmID id,
            Collection<TimeRange> times) throws GfeConfigurationException {
        boolean isAccum = false;
        try {
            isAccum = IFPServerConfigManager
                    .getServerConfig(id.getDbId().getSiteId())
                    .accumulativeD2DElements(id.getDbId().getModelName())
                    .contains(id.getParmName());
        } catch (GfeConfigurationException e) {
            throw e;
        }

        if (!isAccum) {
            boolean isDuration = false;
            for (TimeRange time : times) {
                if (time.getDuration() > 0) {
                    isDuration = true;
                    break;
                }
            }
            return isDuration;
        }

        return !isAccum;
    }

}
