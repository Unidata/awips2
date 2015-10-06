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

package com.raytheon.edex.plugin.grib.spatial;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;

import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.awipstools.GetWfoCenterPoint;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.GridGeometryWrapChecker;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.awipstools.GetWfoCenterHandler;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Cache used for holding GridCoverage objects. This cache is responsible for
 * managing the grids and subgrids stored on the filesystem for grib data. The
 * functions in this class have some overlap with the GridCoverageLookup,
 * methods in this class have some extra functionality for trying to resolve
 * grids off the filesystem.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ --------------------------
 * Apr 07, 2009  1994     bphillip     Initial Creation
 * Mar 07, 2013  1771     bsteffen     make subgridding deterministic.
 * Jan 04, 2013  15653    M.Porricelli Shift subgrid domain westward like
 *                                     AWIPSI
 * Oct 15, 2013  2473     bsteffen     Rewrite deprecated code.
 * Jul 21, 2014  3373     bclement     JAXB managers only live during initializeGrids()
 * Mar 04, 2015  3959     rjpeter      Update for grid based subgridding.
 * Sep 28, 2015  4868     rjpeter      Allow subgrids to be defined per coverage.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GribSpatialCache {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribSpatialCache.class);

    private static final float MIN_SUBGRID_COVERAGE;

    /** The singleton instance */
    private static GribSpatialCache instance;

    /**
     * Map containing the GridCoverages<br>
     * The key for this map is the name field of the GridCoverage object stored
     * as the value of the map. This is only used internally for lookup of a
     * coverage by name aka gridId.
     */
    private Map<String, GridCoverage> spatialNameMap;

    /**
     * Maps a GridCoverage id to a set of grid names. The id should come from a
     * coverage in the database but the names are not necessarily in the
     * database. If a grid file spatially matches a grid in the database it will
     * not be added to the db to avoid confusion, this map can be used to look
     * up those names.
     */
    private Map<Integer, Set<String>> gridNameMap;

    /**
     * Map containing the subGrid coverage based on a subGridKey
     */
    private final Map<String, GridCoverage> subGridCoverageMap;

    /**
     * Map containing the subGrid based on a the subGridKey
     */
    private final Map<String, SubGrid> definedSubGridMap;

    private SubGridMapping subGridDefMap;

    private FileDataList fileDataList;

    private long fileScanTime = 0;

    boolean shiftSubGridWest = false;

    static {
        float minCoveragePercent = 20;
        try {
            minCoveragePercent = Float.parseFloat(System.getProperty(
                    "SUB_GRID_COVERAGE_PERCENT", "20"));
            if (minCoveragePercent < 0) {
                minCoveragePercent = 0;
            } else if (minCoveragePercent > 100) {
                minCoveragePercent = 100;
            }
        } catch (Exception e) {
            statusHandler
                    .error("SUB_GRID_COVERAGE_PERCENT must be a value from 0-100.  Defaulting to 20",
                            e);
        }

        MIN_SUBGRID_COVERAGE = minCoveragePercent / 100;
    }

    /**
     * Gets the singleton instance of GribSpatialCache
     * 
     * @return The singleton instance of the GribSpatialCache
     */
    public static synchronized GribSpatialCache getInstance() {
        if (instance == null) {
            instance = new GribSpatialCache();
        }
        return instance;
    }

    /**
     * Creates a new GribSpatialCache
     */
    private GribSpatialCache() {
        gridNameMap = new HashMap<Integer, Set<String>>();
        spatialNameMap = new HashMap<String, GridCoverage>();
        definedSubGridMap = new HashMap<String, SubGrid>();
        subGridCoverageMap = new HashMap<String, GridCoverage>();
        subGridDefMap = new SubGridMapping();
        scanFiles();
    }

    /**
     * Retrieves a grid from the map. If the grid does not exist, the
     * localization files are scanned for newly defined grids, if it still
     * doesn't exist null is returned
     * 
     * @param coverage
     *            a grid coverage
     * @return The GridCoverage object, null if not present
     * @throws GribException
     * @throws DataAccessLayerException
     */
    public GridCoverage getGrid(GridCoverage coverage) {
        GridCoverage rval = GridCoverageLookup.getInstance().getCoverage(
                coverage, false);
        if (rval == null) {
            scanFiles();
            rval = GridCoverageLookup.getInstance()
                    .getCoverage(coverage, false);
        }
        return rval;
    }

    /**
     * Get a grid coverage by name, first all grid files are searched to find
     * one with a matching name, if none is found the database is checked. The
     * returned coverage may not have the name you are looking for but it will
     * be spatially equivalent to that named grid.
     * 
     * @param name
     * @return
     */
    public GridCoverage getGridByName(String name) {
        GridCoverage coverage = spatialNameMap.get(name);
        if (coverage == null) {
            // rescan and hopefully the coverage shows up
            scanFiles();
            coverage = spatialNameMap.get(name);
        }
        return coverage;
    }

    /**
     * This method provides a way to get the names from the definition files for
     * looking up a grib model. It will return all the names of any coverages
     * defined in the grid definition files that are spatially equivalent to the
     * passed in coverage. This is useful when there are multiple grid
     * definition files with the same spatial attributes but different names or
     * for cases where the name in the definition file does not match what is
     * currently in the db.
     * 
     * @param coverage
     * @return
     */
    public Set<String> getGribCoverageNames(GridCoverage coverage) {
        Set<String> rval = gridNameMap.get(coverage.getId());
        if (rval == null) {
            scanFiles();
            rval = gridNameMap.get(coverage.getId());
            if (rval == null) {
                rval = Collections.emptySet();
            }
        }
        return rval;
    }

    /**
     * For a given modelName and coverage this will return the SubGrid used for
     * slicing data if there is a subGrid file for this model. If this model
     * does not require subgridding this method will return null.
     * 
     * @param modelName
     * @param coverage
     * @return
     */
    public SubGrid getSubGrid(String modelName, GridCoverage coverage) {
        SubGrid subGrid = definedSubGridMap
                .get(subGridKey(modelName, coverage));
        if (subGrid == null) {
            if (loadSubGrid(modelName, coverage)) {
                subGrid = definedSubGridMap
                        .get(subGridKey(modelName, coverage));
            }
        }
        return subGrid;
    }

    /**
     * For a given modelName and coverage this will return the sub-GridCoverage
     * which should be used for this data. If this model does not require
     * subgridding this method will return null.
     * 
     * @param modelName
     * @param coverage
     * @return
     */
    public GridCoverage getSubGridCoverage(String modelName,
            GridCoverage coverage) {
        GridCoverage subGrid = subGridCoverageMap.get(subGridKey(modelName,
                coverage));
        if (subGrid == null) {
            if (loadSubGrid(modelName, coverage)) {
                subGrid = subGridCoverageMap
                        .get(subGridKey(modelName, coverage));
            }
        }
        return subGrid;
    }

    /**
     * If a sub grid area is defined for this model than this will process that
     * definition and populate the subGridCoverageMap and definedSubGridMap.
     * Also checks for grids larger than the world and automatically defines a
     * subgrid to remove overlap data.
     * 
     * @param modelName
     * @param coverage
     * @return true if this model is subgridded, false otherwise
     */
    private boolean loadSubGrid(String modelName, GridCoverage coverage) {
        SubGridDef subGridDef = subGridDefMap
                .getSubGridDef(modelName, coverage);

        try {
            if (subGridDef != null) {
                String referenceGrid = subGridDef.getReferenceGrid();
                GridCoverage referenceCoverage = getGridByName(referenceGrid);
                if (referenceCoverage == null) {
                    statusHandler
                            .error("Failed to generate sub grid, Unable to determine coverage for referenceGrid ["
                                    + referenceGrid + "]");
                    return false;
                }

                Coordinate subGridCenterLatLon = new Coordinate(
                        subGridDef.getCenterLongitude(),
                        subGridDef.getCenterLatitude());

                Coordinate subGridCenterGridCoord = MapUtil
                        .latLonToGridCoordinate(subGridCenterLatLon,
                                PixelOrientation.CENTER, referenceCoverage);

                double shiftX = 0;
                int nx = subGridDef.getNx();
                int ny = subGridDef.getNy();

                /*
                 * Check whether 'shiftWest' flag is set in subgrid definition
                 * xml file
                 */
                boolean shiftThisSubGridWest = this.shiftSubGridWest;
                if (subGridDef.getShiftWest() != null) {
                    shiftThisSubGridWest = subGridDef.getShiftWest();
                }

                if (shiftThisSubGridWest == true) {
                    shiftX = nx / 5;
                }

                double xCenterPoint = subGridCenterGridCoord.x - shiftX;
                double yCenterPoint = subGridCenterGridCoord.y;

                double xDistance = nx / 2;
                double yDistance = ny / 2;
                int leftX = (int) (xCenterPoint - xDistance);
                int upperY = (int) (yCenterPoint - yDistance);

                /*
                 * Trim will handle all validation of the subgrid, this includes
                 * world wrap checking.
                 */
                SubGrid subGrid = new SubGrid(leftX, upperY, nx, ny);
                GridCoverage subGridCoverage = referenceCoverage.trim(subGrid);
                if (((subGrid.getNX() * subGrid.getNY()) / (nx * ny)) < MIN_SUBGRID_COVERAGE) {
                    /* minimum subGrid coverage not available, set nx/ny to 0 */
                    subGrid.setNX(0);
                    subGrid.setNY(0);
                }

                if (!referenceCoverage.equals(coverage)) {
                    /*
                     * need to take reference subGrid and convert to subGrid in
                     * original coverage
                     */
                    subGridCoverage.initialize();
                    Coordinate[] origCoords = subGridCoverage.getGeometry()
                            .getCoordinates();
                    if (origCoords.length != 5) {
                        throw new GridCoverageException(
                                "Coverage geometry is not a quadrilateral, cannot create referenced subgrid");
                    }
                    Coordinate[] corners = new Coordinate[4];
                    System.arraycopy(origCoords, 0, corners, 0, 4);
                    MapUtil.latLonToGridCoordinate(corners,
                            PixelOrientation.CENTER, coverage);

                    /*
                     * Don't depend on given corners to be in a specific
                     * position. Can safely assume the lower 2 values are on the
                     * same side.
                     */
                    double[] xCoords = new double[4];
                    xCoords[0] = corners[0].x;
                    xCoords[1] = corners[1].x;
                    xCoords[2] = corners[2].x;
                    xCoords[3] = corners[3].x;
                    double[] yCoords = new double[4];
                    yCoords[0] = corners[0].y;
                    yCoords[1] = corners[1].y;
                    yCoords[2] = corners[2].y;
                    yCoords[3] = corners[3].y;
                    Arrays.sort(xCoords);
                    Arrays.sort(yCoords);

                    /* Guarantee the subGrid is within the reference impl */
                    leftX = (int) Math.ceil(xCoords[1]);
                    upperY = (int) Math.ceil(yCoords[1]);
                    int rightX = (int) Math.floor(xCoords[2]);
                    int lowerY = (int) Math.floor(yCoords[2]);
                    /* Add 1 for inclusive */
                    nx = (rightX - leftX) + 1;
                    ny = (lowerY - upperY) + 1;
                    subGrid = new SubGrid(leftX, upperY, nx, ny);
                    subGridCoverage = coverage.trim(subGrid);
                }

                insertSubGrib(modelName, coverage, subGridCoverage, subGrid);
            } else {
                int wrapCount = GridGeometryWrapChecker
                        .checkForWrapping(coverage.getGridGeometry());

                if ((wrapCount > 0) && (wrapCount < coverage.getNx())) {
                    /*
                     * make sure that there is data going around the world only
                     * once, if the data starts another iteration around the
                     * world, subgrid it to cut off the extra data. This mostly
                     * hits to remove one redundant column.
                     */
                    SubGrid subGrid = new SubGrid(0, 0, wrapCount,
                            coverage.getNy());
                    GridCoverage subGridCoverage = coverage.trim(subGrid);
                    insertSubGrib(modelName, coverage, subGridCoverage, subGrid);
                } else {
                    return false;
                }
            }
        } catch (Exception e) {
            statusHandler.error("Failed to generate sub grid for model "
                    + modelName, e);
            return false;
        }

        return true;
    }

    /**
     * Inserts the subGridCoverage into the database and adds it to the caches.
     * 
     * @param modelName
     * @param coverage
     * @param subGridCoverage
     * @param subGrid
     * @throws GridCoverageException
     */
    private void insertSubGrib(String modelName, GridCoverage coverage,
            GridCoverage subGridCoverage, SubGrid subGrid)
            throws GridCoverageException {
        if (subGridCoverage != null) {
            subGridCoverage = insert(subGridCoverage);
            subGridCoverageMap.put(subGridKey(modelName, coverage),
                    subGridCoverage);
            definedSubGridMap.put(subGridKey(modelName, coverage), subGrid);
        }
    }

    /**
     * Loads and validates subGridDef pointed to by filePath. If definition
     * empty/invalid returns null.
     * 
     * @param subGridDefJaxb
     * @param filePath
     * @param defaultCenter
     * @return
     */
    private SubGridDef loadSubGridDef(
            final SingleTypeJAXBManager<SubGridDef> subGridDefJaxb,
            final String filePath, final Coordinate defaultCenter) {
        SubGridDef rval = null;
        File f = new File(filePath);

        if (f.length() > 0) {
            try {
                rval = subGridDefJaxb.unmarshalFromXmlFile(f);
                boolean noGrid = StringUtil.isEmptyString(rval
                        .getReferenceGrid());
                if (noGrid) {
                    // sub grid didn't have required definitions
                    rval = null;
                } else {
                    if ((rval.getCenterLatitude() == null)
                            || (rval.getCenterLongitude() == null)) {
                        if (defaultCenter == null) {
                            rval = null;
                        } else {
                            rval.setCenterLatitude(defaultCenter.y);
                            rval.setCenterLongitude(defaultCenter.x);
                        }
                    }
                }
            } catch (SerializationException e) {
                statusHandler.error(
                        "Failed reading sub grid file: " + filePath, e);
            }
        }

        return rval;
    }

    private String subGridKey(String modelName, GridCoverage coverage) {
        return modelName + "&" + coverage.getId();
    }

    /**
     * scan the grib grid definition for changes, when force is false this will
     * only scan if we have not scanne din the last 60 seconds.
     * 
     * @param force
     * @return
     */
    private synchronized void scanFiles() {
        if ((fileScanTime + 60000) > System.currentTimeMillis()) {
            return;
        }
        FileDataList currentFDL = generateFileDataList();
        fileScanTime = System.currentTimeMillis();
        if (!currentFDL.equals(this.fileDataList)) {
            initializeGrids(currentFDL);
            return;
        } else {
            return;
        }
    }

    private void initializeGrids(FileDataList fdl) {
        statusHandler.info("Initializing grib grid coverages");
        long startTime = System.currentTimeMillis();
        ClusterTask ct = null;
        Map<Integer, Set<String>> gridNameMap = new HashMap<Integer, Set<String>>();
        Map<String, GridCoverage> spatialNameMap = new HashMap<String, GridCoverage>();
        SubGridMapping subGridDefMap = new SubGridMapping();

        SingleTypeJAXBManager<GridCoverage> gridCovJaxb;
        SingleTypeJAXBManager<SubGridDef> subGridDefJaxb;
        try {
            subGridDefJaxb = new SingleTypeJAXBManager<SubGridDef>(true,
                    SubGridDef.class);
            gridCovJaxb = new SingleTypeJAXBManager<GridCoverage>(true,
                    GridCoverage.class);
        } catch (JAXBException e) {
            statusHandler.error("Unable to create grid JAXB managers", e);
            return;
        }

        do {
            ct = ClusterLockUtils.lock("grib", "spatialCache", 120000, true);
        } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));

        try {
            for (FileData fd : fdl.getCoverageFileList()) {
                try {
                    GridCoverage grid = gridCovJaxb.unmarshalFromXmlFile(fd
                            .getFilePath());
                    String name = grid.getName();
                    grid = insert(grid);
                    spatialNameMap.put(name, grid);
                    Set<String> names = gridNameMap.get(grid.getId());
                    if (names == null) {
                        names = new HashSet<String>();
                        gridNameMap.put(grid.getId(), names);
                    }
                    names.add(name);
                } catch (Exception e) {
                    // Log error but do not throw exception
                    statusHandler.error("Unable to read default grids file: "
                            + fd.getFilePath(), e);
                }
            }
            Coordinate defaultCenterPoint = null;

            try {
                defaultCenterPoint = getDefaultSubGridCenterPoint();
            } catch (Exception e) {
                statusHandler
                        .error("Failed to generate sub grid definitions.  Unable to lookup WFO Center Point",
                                e);
            }
            for (FileData fd : fdl.getSubGridFileList()) {
                try {
                    SubGridDef subGridDef = loadSubGridDef(subGridDefJaxb,
                            fd.getFilePath(), defaultCenterPoint);

                    if (subGridDef == null) {
                        continue;
                    }

                    subGridDefMap.addSubGridDef(subGridDef);
                } catch (Exception e) {
                    // Log error but do not throw exception
                    statusHandler.error("Unable to read default grids file: "
                            + fd.getFilePath(), e);
                }
            }
            this.gridNameMap = gridNameMap;
            this.spatialNameMap = spatialNameMap;
            this.subGridDefMap = subGridDefMap;
            this.subGridCoverageMap.clear();
            this.definedSubGridMap.clear();
            this.fileDataList = fdl;
        } finally {
            ClusterLockUtils.unlock(ct, false);
        }
        long endTime = System.currentTimeMillis();
        statusHandler.info("Grib grid coverages initialized: "
                + (endTime - startTime) + "ms");
    }

    private GridCoverage insert(GridCoverage coverage)
            throws GridCoverageException {
        coverage.initialize();
        GridCoverageLookup gcl = GridCoverageLookup.getInstance();
        GridCoverage dbCoverage = gcl.getCoverage(coverage, true);
        return dbCoverage;
    }

    private FileDataList generateFileDataList() {
        /*
         * Retrieve the list of files from the localization service
         */
        IPathManager pm = PathManagerFactory.getPathManager();
        FileDataList fileList = new FileDataList();
        LocalizationContext[] contexts = pm
                .getLocalSearchHierarchy(LocalizationType.EDEX_STATIC);
        fileList.addCoverageFiles(pm.listFiles(contexts, "/grib/grids",
                new String[] { "xml" }, true, true));
        fileList.addSubGridFiles(pm.listFiles(contexts, "/grib/subgrids",
                new String[] { "xml" }, true, true));

        return fileList;
    }

    public static void reinitialize() {
        GribSpatialCache newInstance = new GribSpatialCache();
        instance = newInstance;
    }

    private Coordinate getDefaultSubGridCenterPoint() throws Exception {
        Coordinate defaultCenterPoint = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile defaultSubGridLocationFile = pm
                .getStaticLocalizationFile("/grib/defaultSubGridCenterPoint.xml");
        SingleTypeJAXBManager<DefaultSubGridCenterPoint> subGridCenterJaxb = new SingleTypeJAXBManager<DefaultSubGridCenterPoint>(
                DefaultSubGridCenterPoint.class);
        if ((defaultSubGridLocationFile != null)
                && defaultSubGridLocationFile.exists()) {
            try {
                DefaultSubGridCenterPoint defaultSubGridLocation = defaultSubGridLocationFile
                        .jaxbUnmarshal(DefaultSubGridCenterPoint.class,
                                subGridCenterJaxb);
                if ((defaultSubGridLocation != null)
                        && (defaultSubGridLocation.getCenterLatitude() != null)
                        && (defaultSubGridLocation.getCenterLongitude() != null)) {
                    defaultCenterPoint = new Coordinate(
                            defaultSubGridLocation.getCenterLongitude(),
                            defaultSubGridLocation.getCenterLatitude());
                    statusHandler
                            .info("Default sub grid location is overriden as ["
                                    + defaultCenterPoint.y + "/"
                                    + defaultCenterPoint.x + "]");
                }
            } catch (Exception e) {
                statusHandler.error(
                        "Unable to load default sub grid location from file: "
                                + defaultSubGridLocationFile.getFile()
                                        .getAbsolutePath(), e);
            }
        }

        if (defaultCenterPoint == null) {
            // use wfo center point
            String wfo = SiteUtil.getSite();
            GetWfoCenterPoint centerPointRequest = new GetWfoCenterPoint(wfo);
            defaultCenterPoint = new GetWfoCenterHandler()
                    .handleRequest(centerPointRequest);
            statusHandler
                    .info("Default sub grid location is wfo center point ["
                            + defaultCenterPoint.y + "/" + defaultCenterPoint.x
                            + "]");
            /*
             * If we are getting the WFO center as the center point, it means
             * that the site has not defined its own center in the site file
             * defaultSubGridCenterPoint.xml (see previous If block). Therefore,
             * we will be shifting the domain westward to be similar to AWIPSI
             * default behavior, so set a flag here. If the site *has* defined a
             * center in defaultSubGridCenterPoint.xml, we will use that as the
             * true, intended center and will not shift the domain further.
             */
            shiftSubGridWest = true;
        } else {
            shiftSubGridWest = false;
        }

        return defaultCenterPoint;
    }

    public static Corner determineFirstGridPointCorner(int scanMode) {
        if ((scanMode & 128) > 0) {
            // -i
            if ((scanMode & 64) > 0) {
                // +j
                return Corner.LowerRight;
            } else {
                // -j
                return Corner.UpperRight;
            }
        } else {
            // +i
            if ((scanMode & 64) > 0) {
                // +j
                return Corner.LowerLeft;
            } else {
                // -j
                return Corner.UpperLeft;
            }
        }
    }

    private class SubGridMapping {
        Map<String, SubGridDef> subGridsByCoverageName = new HashMap<>();

        Map<String, SubGridModelEntry> subGridsByModel = new HashMap<>();

        public void addSubGridDef(SubGridDef def) {
            String coverageName = def.getReferenceGrid();

            List<String> models = def.getModelNames();
            if (CollectionUtil.isNullOrEmpty(models)) {
                subGridsByCoverageName.put(coverageName, def);
            } else {
                for (String model : models) {
                    SubGridModelEntry modelEntry = subGridsByModel.get(model);
                    if (modelEntry == null) {
                        modelEntry = new SubGridModelEntry();
                        subGridsByModel.put(model, modelEntry);
                    }

                    modelEntry.addSubGridDef(def);
                }
            }
        }

        public SubGridDef getSubGridDef(String modelName, GridCoverage coverage) {
            SubGridDef rval = null;
            SubGridModelEntry modelSubgrids = subGridsByModel.get(modelName);

            if (modelSubgrids != null) {
                rval = modelSubgrids.getSubGridDef(coverage);
            }

            if (rval == null) {
                rval = subGridsByCoverageName.get(coverage.getName());

                if (rval == null) {
                    for (String coverageName : GribSpatialCache.this
                            .getGribCoverageNames(coverage)) {
                        rval = subGridsByCoverageName.get(coverageName);

                        if (rval != null) {
                            break;
                        }
                    }
                }
            }

            return rval;
        }
    }

    private class SubGridModelEntry {
        /**
         * Map of sub grid definitions by coverage name.
         */
        private final Map<String, SubGridDef> subGridsByCoverage = new HashMap<>();

        public void addSubGridDef(SubGridDef def) {
            subGridsByCoverage.put(def.getReferenceGrid(), def);
        }

        public SubGridDef getSubGridDef(GridCoverage coverage) {
            SubGridDef rval = subGridsByCoverage.get(coverage.getName());

            if (rval == null) {
                for (String coverageName : GribSpatialCache.this
                        .getGribCoverageNames(coverage)) {
                    rval = subGridsByCoverage.get(coverageName);

                    if (rval != null) {
                        break;
                    }
                }
            }

            return rval;
        }
    }
}
