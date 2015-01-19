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
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.edex.plugin.gfe.config.GridDbConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.SimpleGridParmConfig;
import com.raytheon.edex.plugin.gfe.config.SimpleModelConfig;
import com.raytheon.edex.plugin.gfe.db.dao.GFEDao;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.topo.TopoException;
import com.raytheon.uf.common.topo.TopoQuery;

/**
 * Manages the TopoDatabase instances of active GFE sites
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2009            njensen     Initial creation
 * May 04, 2012  #574      dgilling    Re-port to better match AWIPS1.
 * Feb 12, 2013  #1608     randerso    Changed to use explicit deleteGroups
 * Feb 15, 2013  #1638     mschenke    Deleted topo edex plugin, moved code into common topo
 * Jun 13, 2013  #2044     randerso    Refactored to use non-singleton GridParmManager, 
 *                                     code cleanup
 * Nov 20, 2013  #2331     randerso    Changed return type of getTopoData
 * Feb 11, 2014  #2788     randerso    Set missing data points to 0 to match A1
 * Oct 07, 2014  #3684     randerso    Restructured IFPServer start up
 * Jan 15, 2015  #3955     randerso     Changed TopoDatabase to extend IFPGridDatabase
 *                                      to work with ISC for Standarad Terrain WA
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TopoDatabaseManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TopoDatabaseManager.class);

    private final IFPServerConfig config;

    private IDataStore dataStore;

    private TopoDatabase topoDatabase;

    /**
     * Constructor
     * 
     * @param siteID
     * @param config
     * @throws GfeException
     */
    public TopoDatabaseManager(String siteID, IFPServerConfig config)
            throws GfeException {
        this.config = config;

        statusHandler.info("Topography Manager started for " + siteID);

        // get GridParmInfo configuration
        GridLocation gloc = config.dbDomain();

        try {
            // this.topoDatabase = new TopoDatabase(this.config, this);

            GFEDao dao = new GFEDao();
            DatabaseID dbId = dao.getDatabaseId(new DatabaseID(siteID,
                    DataType.GRID, "EditTopo", "Topo"));

            SimpleModelConfig smc = new SimpleModelConfig(siteID, "GRID",
                    "EditTopo", "Topo", gloc.getProjection().getProjectionID(),
                    true, false, 1, 0);
            smc.grids = Arrays.asList(new SimpleGridParmConfig("Topo",
                    "Scalar", "ft", "Topography", 50000.0f, -32000.0f, 1, true,
                    gloc.gridSize(), gloc.getOrigin(), gloc.getExtent(), 0, 0,
                    0, false));
            GridDbConfig gridDbConfig = new GridDbConfig(smc,
                    config.getWxDefinition(), gloc.getProjection(),
                    config.getDiscreteDefinition(), null);
            this.topoDatabase = new TopoDatabase(dbId, gridDbConfig, this);

        } catch (Exception e) {
            throw new GfeException("Error creating Topo database", e);
        }

        if (this.topoDatabase.databaseIsValid()) {
            File hdf5File = GfeUtil.getHdf5TopoFile(
                    GridDatabase.gfeBaseDataDir, this.topoDatabase.getDbId());
            dataStore = DataStoreFactory.getDataStore(hdf5File);

            // create the disk cache
            createDiskCache(gloc);
        } else {
            throw new GfeException("Invalid Topo database");
        }

        statusHandler.info("Topography Manager ready for " + siteID);
    }

    /**
     * @return the topo database
     */
    public TopoDatabase getTopoDatabase() {
        return this.topoDatabase;
    }

    /**
     * Request for topography data given a grid location. Returns the topo data
     * and the status as a <code>ServerResponse</code>.
     * 
     * @param gloc
     * @return ServerResponse containing the topo grid slice
     */
    public ServerResponse<ScalarGridSlice> getTopoData(final GridLocation gloc) {
        ServerResponse<ScalarGridSlice> sr = new ServerResponse<ScalarGridSlice>();
        ScalarGridSlice data = new ScalarGridSlice();
        Grid2DFloat grid = null;
        try {
            FloatDataRecord fdr = getTopoRecord(gloc)[0];

            grid = new Grid2DFloat(gloc.getNx(), gloc.getNy(),
                    fdr.getFloatData());
            if (!grid.isValid()) {
                sr.addMessage("Invalid topography grid from cache for " + gloc);
            }
        } catch (Exception e) {
            sr.addMessage("Unable to retrieve topo record for " + gloc);
        }

        // convert to IGridSlice
        if (sr.isOkay()) {
            data = makeGridSlice(gloc, grid);
            sr.setPayload(data);
        } else {
            sr.addMessage("Unable to provide topography grid");
        }

        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder o = new StringBuilder("Topography Manager Cache \n");
        o.append(fileList().toString());
        return o.toString();
    }

    /**
     * Converts the <code>Grid2DFloat</code> topogrid into a
     * <code>IGridSlice</code>.
     * 
     * @param gloc
     * @param topoGrid
     * @return
     */
    private ScalarGridSlice makeGridSlice(final GridLocation gloc,
            final Grid2DFloat topoGrid) {
        // find the max/min values in the topography data
        float maxValue = -Float.MAX_VALUE;
        float minValue = Float.MAX_VALUE;

        for (float v : topoGrid.getFloats()) {
            maxValue = Math.max(maxValue, v);
            minValue = Math.min(minValue, v);
        }

        TimeRange validTime = TimeRange.allTimes();
        DatabaseID did = new DatabaseID("Topo", DataType.GRID, "Topo", "Topo");
        ParmID parmID = new ParmID("Topo", did);
        GridParmInfo gpi = new GridParmInfo(parmID, gloc, GridType.SCALAR,
                "Feet MSL", "Topography", minValue, maxValue, 0, true,
                new TimeConstraints(0, 0, 0), false);
        GridDataHistory his = new GridDataHistory(OriginType.OTHER, parmID,
                validTime);

        return new ScalarGridSlice(validTime, gpi,
                new GridDataHistory[] { his }, topoGrid);
    }

    /**
     * This function is called to calculate a topography grid based on the input
     * grid location.
     * 
     * @param gloc
     *            The input grid location.
     * @param allowValuesBelowZero
     *            If set to false, values less than zero in the grid will be set
     *            to 0.
     * @return The topography data.
     * @throws TopoException
     */
    private FloatDataRecord processTopography(final GridLocation gloc,
            boolean allowValuesBelowZero) throws TopoException {
        float[] heights = TopoQuery.getInstance().getHeight(
                MapUtil.getGridGeometry(gloc));
        UnitConverter cvt = SI.METER.getConverterTo(NonSI.FOOT);
        for (int i = 0; i < heights.length; i++) {
            if (!Float.isNaN(heights[i])) {
                heights[i] = (float) cvt.convert(heights[i]);
                if (!allowValuesBelowZero && (heights[i] < 0)) {
                    heights[i] = 0.0f;
                }
            } else {
                heights[i] = 0.0f;
            }
        }

        String name = calcGroupName(gloc);
        return new FloatDataRecord("Data", name, heights, 2, new long[] {
                gloc.getNx().longValue(), gloc.getNy().longValue() });
    }

    /**
     * Creates the disk cache given the <code>GridLocation</code>.
     * 
     * @param gloc
     * @throws GfeException
     */
    private void createDiskCache(final GridLocation gloc) throws GfeException {
        // first clean out the disk cache of old and unnecessary data
        // commenting out call, since this function does nothing in current
        // versions of AWIPS1.
        // cleanDiskCache(gloc);

        // now determine if gloc needs to be created
        boolean needToCreate = false;
        ParmID parmId = this.topoDatabase.getParmList().getPayload().get(0);

        List<String> cachedFiles = fileList();
        if (!cachedFiles.contains(calcGroupName(gloc))) {
            needToCreate = true;
        } else {
            ServerResponse<List<TimeRange>> sr = this.topoDatabase
                    .getGridInventory(parmId);
            if (sr.isOkay()) {
                needToCreate = sr.getPayload().isEmpty();
            } else {
                needToCreate = true;
            }
        }

        if (needToCreate) {
            statusHandler.info("Creating Topography Disk Cache");

            FloatDataRecord fdr = getTopoRecord(gloc)[0];

            TimeRange timeRange = TimeRange.allTimes();
            GridDataHistory history = new GridDataHistory(OriginType.SCRATCH,
                    parmId, timeRange);

            Grid2DFloat topoGrid = new Grid2DFloat(gloc.getNx(), gloc.getNy(),
                    fdr.getFloatData());
            ScalarGridSlice slice = makeGridSlice(gloc, topoGrid);

            GFERecord record = new GFERecord(parmId, timeRange);
            record.setGridHistory(Arrays.asList(history));
            record.setMessageData(slice);

            ServerResponse<?> sr = this.topoDatabase.saveGridData(parmId,
                    timeRange, Arrays.asList(record), new WsId(null, "EDEX",
                            "GFE"));
            if (sr.isOkay()) {
                statusHandler.info("Finished Creating Topography Disk Cache");
            } else {
                throw new GfeException("Error Creating Topography Disk Cache: "
                        + sr.message());
            }
        } else {
            statusHandler.info("Topography Disk Cache exists");
        }
    }

    /**
     * Cleans up unnecessary entries in the disk cache.
     * <p>
     * Based on OB9.6 this function has no effect and should not be called.
     * 
     * @param gloc
     */
    @SuppressWarnings("unused")
    private void cleanDiskCache(final GridLocation gloc) {
        // get fileList of already cached files
        List<String> cachedFiles = fileList();

        // purge cache of old files, or those not in gloc
        for (String cachedFile : cachedFiles) {
            // required cache file (in gloc)?
            boolean needed = false;
            if (calcGroupName(gloc).equals(cachedFile)) {
                needed = true;
                break;
            }
        }
    }

    /**
     * Cleans out the cache file for the specified <code>GridLocation</code> and
     * then recreates the disk cache.
     * 
     * @param gloc
     *            The <code>GridLocation</code> to reset the topography cache
     *            for.
     * @throws GfeException
     */
    public void revertTopoData(final GridLocation gloc) throws GfeException {
        String name = calcGroupName(gloc);
        try {
            dataStore.deleteGroups(name);
        } catch (Exception e) {
            statusHandler.error("Error attempting to remove: " + name, e);
        }
        createDiskCache(gloc);
    }

    /**
     * Returns a list of TOPO records in the disk cache.
     * 
     * @return
     */
    private List<String> fileList() {
        List<String> rval;

        try {
            String[] names = dataStore.getDatasets("/");
            rval = new ArrayList<String>(Arrays.asList(names));
        } catch (Exception e) {
            rval = Collections.emptyList();
        }

        return rval;
    }

    /**
     * Calculates the HDF5 group name based on a <code>GridLocation</code>.
     * 
     * @param gloc
     *            The <code>GridLocation</code> to use to build the group name.
     * @return The group name.
     */
    private String calcGroupName(final GridLocation gloc) {
        String proj = gloc.getProjection().getProjectionID();
        proj = proj.replace(" ", "");

        StringBuilder o = new StringBuilder("TOPO_")
                .append(gloc.getNx().intValue()).append('_')
                .append(gloc.getNy().intValue()).append('_').append(proj)
                .append('_').append(gloc.getOrigin().x).append('_')
                .append(gloc.getOrigin().y).append('_')
                .append(gloc.getExtent().x).append('_')
                .append(gloc.getExtent().y);

        return o.toString();
    }

    /**
     * Outputs topography data to the disk cache.
     * 
     * @param gloc
     * @param slice
     * @throws IllegalArgumentException
     *             If data is not of type <code>ScalarGridSlice</code>.
     * @throws GfeException
     */
    public void saveTopoData(final GridLocation gloc, final IGridSlice slice)
            throws IllegalArgumentException, GfeException {
        if (!(slice instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException(
                    "Attempting to save non-scalar grid slice in saveTopoData");
        }

        float[] data = ((ScalarGridSlice) slice).getScalarGrid().getFloats();
        String name = calcGroupName(gloc);
        FloatDataRecord fdr = new FloatDataRecord(
                "Data",
                name,
                data,
                2,
                new long[] { gloc.getNx().longValue(), gloc.getNy().longValue() });

        writeTopoData(gloc, fdr);
    }

    /**
     * Outputs topography data to the disk cache.
     * 
     * @param gloc
     * @param grid
     */
    private void writeTopoData(final GridLocation gloc,
            final FloatDataRecord fdr) throws GfeException {
        try {
            dataStore.addDataRecord(fdr);
            StorageStatus status = dataStore.store(StoreOp.REPLACE);
            StorageException[] exceptions = status.getExceptions();
            if ((exceptions != null) && (exceptions.length > 0)) {
                throw new GfeException("Error storing topo data for " + gloc,
                        exceptions[0]);
            }
        } catch (StorageException e) {
            throw new GfeException("Error storing topo data for " + gloc, e);
        }
    }

    /**
     * @param gridLoc
     */
    public FloatDataRecord[] getTopoRecord(GridLocation gloc)
            throws GfeException {
        String cacheGroupName = calcGroupName(gloc);

        try {
            IDataRecord[] records = dataStore.retrieve(cacheGroupName);
            return new FloatDataRecord[] { (FloatDataRecord) records[0] };
        } catch (FileNotFoundException | StorageException e) {
            // create new cache since file doesn't exist
            try {
                FloatDataRecord fdr = processTopography(gloc,
                        config.isTopoAllowedBelowZero());
                writeTopoData(gloc, fdr);

                return new FloatDataRecord[] { fdr };
            } catch (TopoException e1) {
                throw new GfeException("Unable to calculate topography for "
                        + gloc, e1);
            }
        }
    }
}
