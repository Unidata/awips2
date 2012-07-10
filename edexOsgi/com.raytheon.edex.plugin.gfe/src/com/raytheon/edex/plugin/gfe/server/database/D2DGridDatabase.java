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

import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import com.raytheon.edex.plugin.gfe.cache.d2dparms.D2DParmIdCache;
import com.raytheon.edex.plugin.gfe.cache.gridlocations.GridLocationCache;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.db.dao.GFEDao;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.grib.dao.GribDao;
import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.edex.plugin.grib.util.GribParamInfoLookup;
import com.raytheon.edex.plugin.grib.util.ParameterInfo;
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
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Singleton that assists with grid data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 05/16/08     875         bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 07/23/09     2342        ryu        Check for null gridConfig in getGridParmInfo
 * 03/02/12     DR14651     ryu        change time independency of staticTopo, staticCoriolis, staticSpacing
 * 05/04/12     #574        dgilling   Implement missing methods from GridDatabase.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class D2DGridDatabase extends VGridDatabase {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DGridDatabase.class);

    /** The remap object used for resampling grids */
    private RemapGrid remap;

    /** The source GridLocation (The D2D grid coverage) */
    private GridLocation inputLoc;

    /** The destination GridLocation (The local GFE grid coverage) */
    private GridLocation outputLoc;

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
        valid = this.dbId.isValid();

        if (valid) {
            String siteId = dbId.getSiteId();
            GridLocationCache locCache = GridLocationCache.getInstance(dbId
                    .getSiteId());
            String gfeModelName = dbId.getModelName();
            inputLoc = locCache.getGridLocation(gfeModelName);

            if (inputLoc == null) {
                String d2dModelName = this.config
                        .d2dModelNameMapping(gfeModelName);
                GridCoverage awipsGrid = GribSpatialCache.getInstance()
                        .getGrid(d2dModelName);

                if (awipsGrid == null) {
                    throw new GfeException(
                            "Unable to lookup coverage for GFE Model ["
                                    + gfeModelName + "] for site [" + siteId
                                    + "].  GribSpatialCache for d2dModel ["
                                    + d2dModelName + "] returned null");
                }

                inputLoc = GfeUtil.transformGridCoverage(awipsGrid);
                locCache.addGridLocation(gfeModelName, inputLoc);
            }

            outputLoc = this.config.dbDomain();

            remap = new RemapGrid(inputLoc, outputLoc);
        }
    }

    @Override
    public void updateDbs() {
        // no op
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
        return GribParamInfoLookup.getInstance().getParameterInfo(mappedModel,
                id.getParmName().toLowerCase()) != null;
    }

    @Override
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID id) {

        ServerResponse<GridParmInfo> sr = new ServerResponse<GridParmInfo>();
        GridParmInfo gpi = null;
        String mappedModel = config.d2dModelNameMapping(id.getDbId()
                .getModelName());

        if (id.getParmName().equalsIgnoreCase("wind")) {
            List<TimeRange> modelTimes = GribParamInfoLookup
                    .getInstance()
                    .getParameterTimes(mappedModel, id.getDbId().getModelDate());
            TimeConstraints tc = getTimeConstraints(modelTimes);

            // first try getting u-component attributes
            ParameterInfo atts = GribParamInfoLookup.getInstance()
                    .getParameterInfo(mappedModel, "uw");

            // if not found try wind speed
            if (atts == null) {
                atts = GribParamInfoLookup.getInstance().getParameterInfo(
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

        ParameterInfo atts = GribParamInfoLookup.getInstance()
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
            List<TimeRange> times = GribParamInfoLookup
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
        List<ParmID> parmIds = D2DParmIdCache.getInstance().getParmIDs(dbId);

        List<ParmID> uwList = new ArrayList<ParmID>();
        Map<String, ParmID> vwMap = new HashMap<String, ParmID>();
        List<ParmID> finalList = new ArrayList<ParmID>();
        List<ParmID> wsList = new ArrayList<ParmID>();
        Map<String, ParmID> wdMap = new HashMap<String, ParmID>();
        List<ParmID> toRemove = new ArrayList<ParmID>();
        for (ParmID id : parmIds) {
            finalList.add(id);

            if (id.getParmName().equals("uw")) {
                uwList.add(id);
                toRemove.add(id);
            } else if (id.getParmName().equals("vw")) {
                vwMap.put(id.getCompositeName(), id);
                toRemove.add(id);
            } else if (id.getParmName().equals("ws")) {
                wsList.add(id);
            } else if (id.getParmName().equals("wd")) {
                wdMap.put(id.getCompositeName(), id);
            }
        }

        for (ParmID uw : uwList) {
            String name = uw.getCompositeName().replaceFirst("uw", "vw");
            ParmID vwParm = vwMap.get(name);
            if (vwParm != null) {
                ParmID windParm = new ParmID("wind", uw.getDbId(),
                        uw.getParmLevel());
                finalList.add(windParm);
            }
        }

        for (ParmID ws : wsList) {
            String name = ws.getCompositeName().replaceFirst("ws", "wd");
            ParmID wdParm = wdMap.get(name);
            if (wdParm != null) {
                ParmID windParm = new ParmID("wind", ws.getDbId(),
                        ws.getParmLevel());
                finalList.add(windParm);
            }
        }
        finalList.removeAll(toRemove);
        sr.setPayload(finalList);
        return sr;
    }

    @Override
    public String getProjectionId() {
        return this.outputLoc.getProjection().getProjectionID();
    }

    @Override
    public ServerResponse<List<IGridSlice>> getGridData(ParmID id,
            List<TimeRange> timeRanges) {

        List<IGridSlice> data = new ArrayList<IGridSlice>();
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

        List<IGridSlice> data = new ArrayList<IGridSlice>();
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
    public IGridSlice getGridSlice(ParmID parmId, GridParmInfo gpi,
            TimeRange time, boolean convertUnit) throws GfeException {
        IGridSlice gs = null;
        GridDataHistory[] gdh = { new GridDataHistory(
                GridDataHistory.OriginType.INITIALIZED, parmId, time, null,
                (WsId) null) };

        switch (gpi.getGridType()) {
        case SCALAR:
            Grid2DFloat data = getGrid(parmId, time, gpi, convertUnit);
            gs = new ScalarGridSlice(time, gpi, gdh, data);
            break;
        case VECTOR:
            Grid2DFloat mag = new Grid2DFloat(gpi.getGridLoc().getNx(), gpi
                    .getGridLoc().getNy());
            Grid2DFloat dir = new Grid2DFloat(gpi.getGridLoc().getNx(), gpi
                    .getGridLoc().getNy());
            getWindGrid(parmId, time, gpi, mag, dir);
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
    public Grid2DFloat getGrid(ParmID parmId, TimeRange time, GridParmInfo gpi,
            boolean convertUnit) throws GfeException {
        Grid2DFloat bdata = null;
        GribRecord d2dRecord = null;

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

        if (d2dRecord == null) {
            throw new GfeException("No data available for " + parmId
                    + " for time range " + time);
        }

        // Gets the raw data from the D2D grib HDF5 file
        bdata = getRawGridData(d2dRecord);

        float fillV = Float.MAX_VALUE;
        ParameterInfo atts = GribParamInfoLookup.getInstance()
                .getParameterInfo(
                        config.d2dModelNameMapping(parmId.getDbId()
                                .getModelName()), parmId.getParmName());
        if (atts != null) {
            fillV = atts.getFillValue();
        }

        // Resample the data to fit desired region
        Grid2DFloat retVal;
        try {
            retVal = this.remap.remap(bdata, fillV, gpi.getMaxValue(),
                    gpi.getMinValue(), gpi.getMinValue());
            if (convertUnit && d2dRecord != null) {
                long t5 = System.currentTimeMillis();
                convertUnits(d2dRecord, retVal, gpi.getUnitObject());
                long t6 = System.currentTimeMillis();
                statusHandler
                        .info("Time spent converting units on d2d grid data: "
                                + (t6 - t5));
            }
        } catch (Exception e) {
            throw new GfeException("Unable to get Grid", e);
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
    private void convertUnits(GribRecord d2dRecord, Grid2DFloat data,
            Unit<?> targetUnit) throws GfeException {

        Unit<?> sourceUnit = d2dRecord.getModelInfo().getParameterUnitObject();
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
        GribRecord uRecord = null;
        GribRecord vRecord = null;
        GribRecord sRecord = null;
        GribRecord dRecord = null;
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

        if (uRecord != null && vRecord != null) {
            // Gets the raw grid data from the D2D grib HDF5 files
            Grid2DFloat uData = getRawGridData(uRecord);
            Grid2DFloat vData = getRawGridData(vRecord);

            // Resample the data to fit the desired region
            float fillV = Float.MAX_VALUE;
            ParameterInfo pa = GribParamInfoLookup.getInstance()
                    .getParameterInfo(mappedModel, "uw");
            if (pa != null) {
                fillV = pa.getFillValue();
            }

            try {
                this.remap.remapUV(uData, vData, fillV, gpi.getMaxValue(),
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

            if (sRecord != null && dRecord != null) {
                // Gets the raw grid data from the D2D grib HDF5 files
                Grid2DFloat sData = getRawGridData(sRecord);
                Grid2DFloat dData = getRawGridData(dRecord);

                // Resample the data to fit the desired region
                float fillV = Float.MAX_VALUE;
                ParameterInfo pa = GribParamInfoLookup.getInstance()
                        .getParameterInfo(mappedModel, "ws");
                if (pa != null) {
                    fillV = pa.getFillValue();
                }

                try {
                    this.remap.remap(sData, dData, fillV, gpi.getMaxValue(),
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
     */
    private Grid2DFloat getRawGridData(GribRecord d2dRecord) {
        FloatDataRecord hdf5Record;
        try {
            GribDao dao = new GribDao();
            IDataRecord[] hdf5Data = dao.getHDF5Data(d2dRecord, -1);
            hdf5Record = (FloatDataRecord) hdf5Data[0];
        } catch (PluginException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to get grib hdf5 record", e);
            return null;
        }
        return new Grid2DFloat((int) hdf5Record.getSizes()[0],
                (int) hdf5Record.getSizes()[1], hdf5Record.getFloatData());

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
        List<TimeRange> times = GribParamInfoLookup
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

    public static boolean isNonAccumDuration(ParmID id, List<TimeRange> times)
            throws GfeConfigurationException {
        boolean isAccum = false;
        try {
            isAccum = IFPServerConfigManager
                    .getServerConfig(id.getDbId().getSiteId())
                    .accumulativeD2DElements(id.getDbId().getModelName())
                    .contains(id.getParmName());
        } catch (GfeConfigurationException e) {
            throw e;
        }
        boolean isDuration = false;
        for (TimeRange time : times) {
            if (time.getDuration() > 0) {
                isDuration = true;
                break;
            }
        }
        return !isAccum && isDuration;
    }

}
