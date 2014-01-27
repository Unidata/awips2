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
import java.util.Collections;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
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

    private final IDataStore dataStore;

    private DatabaseID getTopoDbId(String siteID) {
        return new DatabaseID(siteID, DataType.GRID, "EditTopo", "Topo");
    }

    /**
     * Constructor
     * 
     * @param siteID
     * @param config
     * @param gridMgr
     */
    public TopoDatabaseManager(String siteID, IFPServerConfig config,
            GridParmManager gridMgr) {
        this.config = config;

        statusHandler.info("Topography Manager started for " + siteID);

        // get GridParmInfo configuration
        GridLocation gloc = config.dbDomain();

        File hdf5File = GfeUtil.getHdf5TopoFile(GridDatabase.gfeBaseDataDir,
                getTopoDbId(siteID));
        dataStore = DataStoreFactory.getDataStore(hdf5File);

        // create the disk cache
        createDiskCache(gloc);

        // Add the topo database.
        TopoDatabase tdb = new TopoDatabase(this.config, this);
        if (tdb.databaseIsValid()) {
            gridMgr.addDB(tdb);
        } else {
            statusHandler.error("Invalid Topo database");
        }

        statusHandler.info("Topography Manager ready for " + siteID);
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
        Grid2DFloat grid;
        String cacheGroupName = calcGroupName(gloc);

        try {
            IDataRecord[] dr = dataStore.retrieve(cacheGroupName);

            // file exists, so read it
            grid = readTopoData((FloatDataRecord) dr[0]);
            if (!grid.isValid()) {
                sr.addMessage("Invalid topography grid from cache for " + gloc);
            }
        } catch (Exception e) {
            // create new cache since file doesn't exist
            statusHandler.handle(Priority.DEBUG, "Calculating Topography for "
                    + gloc);
            grid = processTopography(gloc, config.isTopoAllowedBelowZero());
            if (grid.isValid()) {
                writeTopoData(gloc, grid);
            }
        }

        // convert to IGridSlice
        if (sr.isOkay()) {
            data = makeGridSlice(gloc, grid);
        } else {
            sr.addMessage("Unable to provide topography grid");
        }

        sr.setPayload(data);
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
     * @return The topography grid.
     */
    private Grid2DFloat processTopography(final GridLocation gloc,
            boolean allowValuesBelowZero) {
        float[] heights = TopoQuery.getInstance().getHeight(
                MapUtil.getGridGeometry(gloc));
        UnitConverter cvt = SI.METER.getConverterTo(NonSI.FOOT);
        for (int i = 0; i < heights.length; i++) {
            if (!Float.isNaN(heights[i])) {
                heights[i] = (float) cvt.convert(heights[i]);
                if (!allowValuesBelowZero && (heights[i] < 0)) {
                    heights[i] = 0.0f;
                }
            }
        }

        return new Grid2DFloat(gloc.getNx(), gloc.getNy(), heights);
    }

    /**
     * Creates the disk cache given the <code>GridLocation</code>.
     * 
     * @param gloc
     */
    private void createDiskCache(final GridLocation gloc) {
        statusHandler.info("Creating Topography Disk Cache");

        // first clean out the disk cache of old and unnecessary data
        // commenting out call, since this function does nothing in current
        // versions of AWIPS1.
        // cleanDiskCache(gloc);

        // now determine if gloc needs to be created
        List<String> cachedFiles = fileList();
        if (!cachedFiles.contains(calcGroupName(gloc))) {
            // if not in list, then we need to make one
            statusHandler.debug("Calculating Topography for " + gloc);
            Grid2DFloat grid = processTopography(gloc,
                    config.isTopoAllowedBelowZero());
            if (grid.isValid()) {
                writeTopoData(gloc, grid);
            }
        }

        statusHandler.info("Finished Creating Topography Disk Cache.");
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
     */
    public void revertTopoData(final GridLocation gloc) {
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
     * @param data
     * @throws IllegalArgumentException
     *             If data is not of type <code>ScalarGridSlice</code>.
     */
    public void saveTopoData(final GridLocation gloc, final IGridSlice data)
            throws IllegalArgumentException {
        if (!(data instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException(
                    "Attempting to save non-scalar grid slice in saveTopoData");
        }

        ScalarGridSlice casted = (ScalarGridSlice) data;
        writeTopoData(gloc, casted.getScalarGrid());
    }

    /**
     * Outputs topography data to the disk cache.
     * 
     * @param gloc
     * @param grid
     */
    private void writeTopoData(final GridLocation gloc, final Grid2DFloat grid) {
        String name = calcGroupName(gloc);
        IDataRecord output = new FloatDataRecord("Data", name,
                grid.getFloats(), 2, new long[] { gloc.getNx().longValue(),
                        gloc.getNy().longValue() });
        try {
            dataStore.addDataRecord(output);
            StorageStatus status = dataStore.store(StoreOp.REPLACE);
            StorageException[] exceptions = status.getExceptions();
            if ((exceptions != null) && (exceptions.length > 0)) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Storage exceptions occurred during hdf5 save. "
                                        + " errors occurred. The first failure will be logged.");
                statusHandler.error("Unable to ouput cache data to: " + name,
                        exceptions[0]);
            }
        } catch (StorageException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to ouput cache data to: " + name, e);
        }
    }

    /**
     * Reads topography data from the disk cache.
     * 
     * @param input
     *            The <code>FloatDataRecord</code> from the disk cache.
     * @return The topography grid.
     */
    private Grid2DFloat readTopoData(final FloatDataRecord input) {
        // input the data
        long[] gridSize = input.getSizes();
        int xGridSize = (int) gridSize[0];
        int yGridSize = (int) gridSize[1];

        Grid2DFloat grid = new Grid2DFloat(xGridSize, yGridSize,
                input.getFloatData());
        return grid;
    }
}
