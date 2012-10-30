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

package com.raytheon.uf.edex.plugin.grid.topo;

import java.awt.Point;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.media.jai.JAI;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.factory.Hints;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.ReferencingFactoryFinder;
import org.geotools.referencing.operation.AbstractCoordinateOperationFactory;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.geotools.referencing.operation.transform.IdentityTransform;
import org.opengis.geometry.DirectPosition;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.GeographicCRS;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.CoordinateOperationFactory;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.MathTransformFactory;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Class used for accessing static topography information for GFE smart inits
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/27/2010   6394        bphillip    Initial creation
 * 10/08/2010   6394        bphillip    Rewrote sections for optimal reading and writing performance
 * 09/19/2011   10955       rferrel     Use RunProcess
 * 04/18/2012   DR 14694    D. Friedman Fixes for static topography generation
 * 05/09/2012   DR 14939    D. Friedman Fix errors in DR 14694
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class StaticTopoData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StaticTopoData.class);

    private static final float DATA_FILL = -9999;

    private static final float TOPO_FILL = -999999;

    /** The HDF5 data store used for accessing the topo data */
    private IDataStore sTopoDataStore;

    /** The HDF5 data store used for storing the site static topo data */
    private IDataStore siteDataStore;

    private static final String[] TOPO_FILES = new String[] { "us", "ak",
            "carib", "pac", "world" };

    private static final String TASK_NAME = "initStaticTopo";

    /** The HDF dataset name for the static topo information for gfe */
    private static final String STOPO_DATASET = "Topo";

    /** The .dat.gz suffix */
    private static final String DAT_GZ_SUFFIX = ".dat.gz";

    /**
     * Margin of extra data to retrieve when requesting the data slab from the
     * data store
     */
    private static final int DATA_MARGIN = 2;

    /** The base directory in which the topo files reside */
    private static final String FILE_PREFIX = PropertiesFactory.getInstance()
            .getEnvProperties().getEnvValue("HDF5DIR")
            + "/topo/";

    /** The file containing the complete static topo data sets */
    private static final File topoFile = new File(FILE_PREFIX + "staticTopo.h5");

    /** The file containing the site specific static topo data sets */
    private static final File siteTopoFile = new File(FILE_PREFIX
            + "modelStaticTopo.h5");

    /** A GridCoverageFactory instance for creating grid coverages */
    private GridCoverageFactory factory;

    /** HDF5 storage properties used for enabling compression */
    private static final StorageProperties sp;

    /** The singleton instance */
    private static StaticTopoData instance;

    // Initializes the storage properties
    static {
        sp = new StorageProperties();
        sp.setCompression(Compression.LZF);
        sp.setChunked(true);
    }

    /**
     * Gets the singleton instance of StaticTopoData
     * 
     * @return The singleton instance
     */
    public static synchronized StaticTopoData getInstance() {
        if (instance == null) {
            instance = new StaticTopoData();
        }
        return instance;
    }

    /**
     * Creates a new StaticTopoData instance
     */
    private StaticTopoData() {
        this.sTopoDataStore = DataStoreFactory.getDataStore(topoFile);
        this.siteDataStore = DataStoreFactory.getDataStore(siteTopoFile);

        this.factory = new GridCoverageFactory();
        initStopoData();
    }

    /**
     * Initializes the static topo data from the raw data
     */
    private void initStopoData() {
        statusHandler.handle(Priority.INFO, "Initializing static topo data");

        try {
            ClusterTask ct = null;
            do {
                ct = ClusterLockUtils.lock(TASK_NAME, "unpack", 300000, true);
            } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));

            try {
                if (!topoFileExists()) {
                    // TODO: This will fail in a clustered server environment
                    // since
                    // static topo isn't installed to dx3/4
                    statusHandler.handle(Priority.INFO,
                            "Static Topo file not found. Creating it...");

                    // Initializes the World topo data
                    statusHandler.handle(Priority.INFO,
                            "Initializing World Topo Data...");
                    // DR#10955
                    RunProcess
                            .getRunProcess()
                            .exec("gunzip " + FILE_PREFIX + "worldTopo"
                                    + DAT_GZ_SUFFIX).waitFor();
                    readTopoFile("world");
                    statusHandler.handle(Priority.INFO,
                            "World Topo Data Initialized!");

                    // Initializes the US topo data
                    statusHandler.handle(Priority.INFO,
                            "Initializing US Topo Data...");
                    // DR#10955
                    RunProcess
                            .getRunProcess()
                            .exec("gunzip " + FILE_PREFIX + "usTopo"
                                    + DAT_GZ_SUFFIX).waitFor();
                    readTopoFile("us");
                    statusHandler.handle(Priority.INFO,
                            "US Topo Data Initialized!");

                    // Initializes the Carribean topo data
                    statusHandler.handle(Priority.INFO,
                            "Initializing Carribean Topo Data...");
                    // DR#10955
                    RunProcess
                            .getRunProcess()
                            .exec("gunzip " + FILE_PREFIX + "caribTopo"
                                    + DAT_GZ_SUFFIX).waitFor();
                    readTopoFile("carib");
                    statusHandler.handle(Priority.INFO,
                            "Carribean Topo Data Initialized!");

                    // Initializes the Alaska topo data
                    statusHandler.handle(Priority.INFO,
                            "Initializing Alaska Topo Data...");
                    // DR#10955
                    RunProcess
                            .getRunProcess()
                            .exec("gunzip " + FILE_PREFIX + "akTopo"
                                    + DAT_GZ_SUFFIX).waitFor();
                    readTopoFile("ak");
                    statusHandler.handle(Priority.INFO,
                            "Alaska Topo Data Initialized!");

                    // Initializes the Pacific topo data
                    statusHandler.handle(Priority.INFO,
                            "Initializing Pacific Topo Data...");
                    // DR#10955
                    RunProcess
                            .getRunProcess()
                            .exec("gunzip " + FILE_PREFIX + "pacTopo"
                                    + DAT_GZ_SUFFIX).waitFor();
                    readTopoFile("pac");
                    statusHandler.handle(Priority.INFO,
                            "Pacific Topo Data Initialized!");
                    splitPacific();
                }
            } finally {
                if (ct != null) {
                    ClusterLockUtils.unlock(ct, false);
                }
            }

            initAttributes();
        } catch (Exception e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error initializing static topo data!", e);
        }
        statusHandler.handle(Priority.INFO, "Static topo data initialized");
    }

    /**
     * Splits the Pacific grid into east and west hemisphere chunks to reduce
     * memory usage
     * 
     * @throws Exception
     */
    private void splitPacific() throws Exception {
        Map<String, Object> pacAttributes = TopoAttributes.getAttributes("pac");
        int nx = (Integer) pacAttributes.get(TopoAttributes.NX);
        int ny = (Integer) pacAttributes.get(TopoAttributes.NY);

        Map<String, Object> attributes = new HashMap<String, Object>();
        attributes.putAll(pacAttributes);
        attributes.put(TopoAttributes.NX, nx / 2);
        attributes.put(TopoAttributes.UR_LON, new Float(180.0f));
        float[] floatData = this.getSlab("pac", 0, 0, nx / 2, ny);
        FloatDataRecord eastRecord = new FloatDataRecord("pac_east", "/",
                floatData, 2, new long[] { nx / 2, ny });
        sTopoDataStore.addDataRecord(eastRecord, sp);

        FloatDataRecord attributeSet = new FloatDataRecord("attr" + "pac_east",
                "/", new float[] { 0 });
        attributeSet.setDataAttributes(attributes);
        sTopoDataStore.addDataRecord(attributeSet, sp);
        sTopoDataStore.store();

        attributes = new HashMap<String, Object>();
        attributes.putAll(pacAttributes);
        attributes.put(TopoAttributes.NX, nx / 2);
        attributes.put(TopoAttributes.LL_LON, new Float(-180.0f));
        floatData = this.getSlab("pac", nx / 2, 0, nx, ny);
        FloatDataRecord westRecord = new FloatDataRecord("pac_west", "/",
                floatData, 2, new long[] { nx / 2, ny });
        attributeSet = new FloatDataRecord("attr" + "pac_west", "/",
                new float[] { 0 });
        attributeSet.setDataAttributes(attributes);
        sTopoDataStore.addDataRecord(attributeSet, sp);
        sTopoDataStore.addDataRecord(westRecord, sp);
        sTopoDataStore.store();
        sTopoDataStore.delete("pac");
        sTopoDataStore.delete("attrpac");

    }

    /**
     * Checks to see if the topo file exists
     * 
     * @return True if the file exists
     */
    private boolean topoFileExists() {
        try {
            String[] modelArray = sTopoDataStore.getDatasets("/");
            if (modelArray.length == 0) {
                return false;
            }
        } catch (Exception e) {
            return false;
        }
        return true;
    }

    /**
     * Initializes the attributes so they can be easily retrieved
     * 
     * @throws Exception
     */
    private void initAttributes() throws Exception {
        TopoAttributes.attributeMap.remove("world");
        TopoAttributes.attributeMap.remove("pac");
        String[] dataSets = sTopoDataStore.getDatasets("/");

        for (String dataset : dataSets) {
            if (dataset.startsWith("attr")) {
                TopoAttributes.attributeMap.put(
                        dataset.replace("attr", ""),
                        sTopoDataStore.retrieve("/", dataset,
                                Request.buildPointRequest(new Point(0, 0)))
                                .getDataAttributes());
            }
        }
    }

    /**
     * Initializes the topography data for the given site. The data is extracted
     * from the static topo file and resampled according to the site location
     * information
     * 
     * @param modelName
     *            The site for which to initalize the topo data
     * @param config
     *            The site's configuration information
     * @throws StorageException
     * @throws SerializationException
     *             If the topography data cannot be initialized for the given
     *             site
     */
    public void initStopoData(GridCoverage coverage) throws StorageException,
            SerializationException {
        if (coverage.getNx() < 0 || coverage.getNy() < 0) {
            statusHandler.handle(Priority.PROBLEM, coverage.getName()
                    + " is not applicable to " + SiteUtil.getSite()
                    + ". Skipping.");
            return;
        }
        GridGeometry2D inGeom = MapUtil.getGridGeometry(coverage);

        // Gets the location data and extracts it from the static topo file
        float[] finalData = null;
        finalData = getTopoData(inGeom, coverage.getCrs(), coverage.getNx(),
                coverage.getNy());

        // Create an HDF5 data record and store it
        FloatDataRecord outRecord = new FloatDataRecord(STOPO_DATASET,
                coverage.spatialKey(), finalData, 2, new long[] {
                        inGeom.getGridRange().getHigh(0) + 1,
                        inGeom.getGridRange().getHigh(1) + 1 });
        siteDataStore.addDataRecord(outRecord, sp);
        siteDataStore.store(StoreOp.REPLACE);

        statusHandler
                .handle(Priority.INFO,
                        "Stopo data successfully initialized for "
                                + coverage.getName());
    }

    /**
     * Private method used by the initialization code to see if the static topo
     * data for a coverage has been initialized
     */
    public boolean checkModelTopo(GridCoverage coverage) {
        ClusterTask ct = ClusterLockUtils.lock(TASK_NAME,
                coverage.spatialKey(), 120000, false);

        if (LockState.SUCCESSFUL.equals(ct.getLockState())) {
            // set processed to true regardless of successfully
            // update coverage so we don't infinite loop
            try {
                if (!topoExists(coverage)) {
                    statusHandler.handle(
                            Priority.INFO,
                            "Initializing static topo for grid "
                                    + coverage.getName());
                    initStopoData(coverage);
                }
            } catch (Exception e) {
                statusHandler.handle(
                        Priority.INFO,
                        "Error storing static topo data for "
                                + coverage.getName(), e);
            } finally {
                ClusterLockUtils.unlock(ct, false);
            }
            return true;
        }
        return false;
    }

    /**
     * Chekcs if the static topo data for a model exists in the file
     * 
     * @param modelName
     *            The model to check
     * @return True if the data exists, else false
     * @throws GribException
     */
    private boolean topoExists(GridCoverage coverage) {

        List<String> dataSets = null;
        try {
            String[] modelArray = siteDataStore.getDatasets(coverage
                    .spatialKey());
            dataSets = Arrays.asList(modelArray);
        } catch (Exception e) {
            return false;
        }

        if (dataSets.contains(STOPO_DATASET)) {
            return true;
        }

        return false;
    }

    /**
     * Retrieves the static topo data for the given site
     * 
     * @param modelName
     *            The site for which to get the static topo data
     * @return The static topo data
     * @throws GribException
     *             If an error occurs while retrieving the topo data
     */
    public FloatDataRecord getStopoData(GridCoverage coverage) {
        if (!topoExists(coverage)) {
            while (!checkModelTopo(coverage)) {
            }
        }
        FloatDataRecord record = null;
        try {
            record = (FloatDataRecord) siteDataStore.retrieve(
                    coverage.spatialKey(), STOPO_DATASET, Request.ALL);

        } catch (Exception e) {
            statusHandler.handle(
                    Priority.INFO,
                    "Static topo data does not exist for grid "
                            + coverage.getName());
            return null;
        }
        return record;
    }

    /**
     * Extracts the the topo data from each of the topo data sets.
     * 
     * @param inGeom
     *            The geometry of the data to be requested
     * @param inCrs
     *            The coordinate reference system of the desired data
     * @return A float array containing the extracted topo data
     * @throws GribException
     *             If the data cannot be extracted
     */
    private float[] getTopoData(GridGeometry2D inGeom,
            CoordinateReferenceSystem inCrs, int coverageNx, int coverageNy) {

        /*
         * Iterate through each of the data sets until the returned data set is
         * fully populated
         */
        float[] finalData = null;

        for (String topoData : TOPO_FILES) {
            for (String dataSet : TopoAttributes.attributeMap.keySet()) {
                if (dataSet.startsWith(topoData)) {
                    try {
                        finalData = extractTopoData(finalData, inCrs, inGeom,
                                coverageNx, coverageNy, dataSet);
                    } catch (Exception e) {
                        // Ignore
                    }
                }
            }
            if (isPopulated(finalData)) {
                break;
            }
        }

        for (int i = 0; i < finalData.length; i++) {
            float v = finalData[i];
            if (Float.isNaN(v))
                finalData[i] = TOPO_FILL;
            else if (v == DATA_FILL || (v > -0.5 && v < 0.5))
                finalData[i] = 0.0f;
            else
                finalData[i] = v;
        }
        return finalData;

    }

    /**
     * Checks the provided array to see if it has been fully populated
     * 
     * @param data
     *            The data to check
     * @return True if the data is fully populated (i.e. There are not NaN
     *         values in the array)
     */
    private boolean isPopulated(float[] data) {
        if (data == null) {
            return false;
        }
        for (int i = 0; i < data.length; i++) {
            if (Float.isNaN(data[i])) {
                return false;
            }
        }
        return true;
    }

    /**
     * Extracts the topo data from the specified data set. The data is populated
     * into the returned array
     * 
     * @param finalData
     *            The topo data to be returned
     * @param inCrs
     *            The CRS of the requested location
     * @param inGeom
     *            The geometry of the requested location
     * @param name
     *            The name of the data set from which to get the data
     * @return The populated array of topo data
     * @throws Exception
     *             If the data cannot be read
     */
    private float[] extractTopoData(float[] finalData,
            CoordinateReferenceSystem inCrs, GridGeometry2D inGeom,
            int coverageNx, int coverageNy, String name) throws Exception {
        String suffix = "";
        String[] tokens = name.split("-");
        if (tokens.length == 2) {
            name = tokens[0];
            suffix = tokens[1];
        }
        statusHandler.handle(Priority.INFO, "Extracting topo data from " + name
                + suffix);

        // Gets the attributes for the data set
        Map<String, Object> attributes = TopoAttributes.getAttributes(name);
        int nx = (Integer) attributes.get(TopoAttributes.NX);
        int ny = (Integer) attributes.get(TopoAttributes.NY);
        double llLat = MapUtil.correctLat((Float) attributes
                .get(TopoAttributes.LL_LAT));
        double llLon = MapUtil.correctLon((Float) attributes
                .get(TopoAttributes.LL_LON));
        double urLat = MapUtil.correctLat((Float) attributes
                .get(TopoAttributes.UR_LAT));
        double urLon = MapUtil.correctLon((Float) attributes
                .get(TopoAttributes.UR_LON));

        // Create the CRS of the topo data
        CoordinateReferenceSystem topoCrs = CRSCache.getInstance()
                .getCoordinateReferenceSystem(
                        (String) attributes.get(TopoAttributes.CRS));

        // Create the geometry of the topo data
        GridGeometry2D topoGeom = createGridGeometry(topoCrs, new Coordinate(
                llLon, llLat), new Coordinate(urLon, urLat), nx, ny);

        ReferencedEnvelope refEnv = new ReferencedEnvelope(
                inGeom.getEnvelope2D(), inCrs);
        refEnv = refEnv.transform(topoCrs, true);

        DirectPosition upperCorner = topoGeom.getCRSToGrid2D(
                PixelOrientation.UPPER_LEFT).transform(refEnv.getUpperCorner(),
                null);
        DirectPosition lowerCorner = topoGeom.getCRSToGrid2D(
                PixelOrientation.UPPER_LEFT).transform(refEnv.getLowerCorner(),
                null);

        int minx = (int) Math.floor(Math.min(lowerCorner.getOrdinate(0),
                upperCorner.getOrdinate(0)));
        int maxx = (int) Math.ceil(Math.max(upperCorner.getOrdinate(0),
                lowerCorner.getOrdinate(0)));
        int miny = (int) Math.floor(Math.min(upperCorner.getOrdinate(1),
                lowerCorner.getOrdinate(1)));
        int maxy = (int) Math.ceil(Math.max(lowerCorner.getOrdinate(1),
                upperCorner.getOrdinate(1)));

        if ("world".equals(name)) {
            if (minx - DATA_MARGIN < 0 || miny - DATA_MARGIN < 0
                    || maxx + DATA_MARGIN >= nx || maxy + DATA_MARGIN >= ny) {
                /*
                 * TODO: Have to do quite a bit more for minimal world
                 * projection subset. Just load the whole thing for now.
                 */
                minx = miny = 0;
                maxx = nx;
                maxy = ny;
            }
        } else {
            if (minx - DATA_MARGIN >= 0) {
                minx -= DATA_MARGIN;
            } else {
                minx = 0;
            }

            if (miny - DATA_MARGIN >= 0) {
                miny -= DATA_MARGIN;
            } else {
                miny = 0;
            }

            if (maxx + DATA_MARGIN <= nx) {
                maxx += DATA_MARGIN;
            } else {
                maxx = nx;
            }

            if (maxy + DATA_MARGIN <= ny) {
                maxy += DATA_MARGIN;
            } else {
                maxy = ny;
            }
        }

        double[] input = new double[] { minx, miny, maxx, maxy };
        double[] output = new double[input.length];

        topoGeom.getGridToCRS(PixelInCell.CELL_CORNER).transform(input, 0,
                output, 0, input.length / 2);

        DirectPosition dpUpper = new DirectPosition2D(Math.max(output[0],
                output[2]), Math.max(output[1], output[3]));
        DirectPosition dpLower = new DirectPosition2D(Math.min(output[0],
                output[2]), Math.min(output[1], output[3]));

        Coordinate upperCoordinate = new Coordinate(dpUpper.getCoordinate()[0],
                dpUpper.getCoordinate()[1]);
        Coordinate lowerCoordinate = new Coordinate(dpLower.getCoordinate()[0],
                dpLower.getCoordinate()[1]);

        Envelope tempEnv = new Envelope(upperCoordinate, lowerCoordinate);
        ReferencedEnvelope correctedRefEnv = new ReferencedEnvelope(tempEnv,
                topoCrs);

        int slabNx = maxx - minx;
        int slabNy = maxy - miny;

        // The topo data does not intersect the desired region, so return
        if (slabNx < 0 || slabNy < 0) {
            if (finalData == null) {
                finalData = new float[coverageNx * coverageNy];
                Arrays.fill(finalData, Float.NaN);
            }
            return finalData;
        }

        // Add one to each of the maxx and maxy since getting slab is exclusive
        // of the final point
        float[][] slabData = this.to2DArray(slabNx, slabNy,
                this.getSlab(name, minx, miny, maxx, maxy));

        // Create the coverage object from the data and the geometry
        GridCoverage2D topoCoverage = factory.create("coverage", slabData,
                correctedRefEnv);
        // Reproject the data into the requested CRS and geometry
        float[] f1 = null;
        try {
            f1 = simpleResample(topoCoverage, slabData, inGeom, name, minx,
                    miny);
        } catch (Exception e) {
            statusHandler.error("rasample failed", e);
            throw e;
        }

        if (finalData == null) {
            finalData = f1;
        } else {
            for (int idx = 0; idx < f1.length; idx++) {
                if (Float.isNaN(finalData[idx]) || finalData[idx] <= DATA_FILL) {
                    finalData[idx] = f1[idx];
                }
            }
        }
        JAI.getDefaultInstance().getTileCache().flush();
        return finalData;
    }

    float[] simpleResample(GridCoverage2D sourceGC, float[][] sourceData,
            GridGeometry2D targetGG, String pfx, int minx, int miny) {
        int sourceWidth = sourceGC.getGridGeometry().getGridRange2D()
                .getSpan(0);
        int sourceHeight = sourceGC.getGridGeometry().getGridRange2D()
                .getSpan(1);
        int targetWidth = targetGG.getGridRange2D().getSpan(0);
        int targetHeight = targetGG.getGridRange2D().getSpan(1);
        float[] output = new float[targetWidth * targetHeight];
        Arrays.fill(output, Float.NaN);

        ArrayList<MathTransform> transforms = new ArrayList<MathTransform>();
        ArrayList<MathTransform> toGeoXforms = new ArrayList<MathTransform>();
        ArrayList<MathTransform> sourceToGeoXforms = new ArrayList<MathTransform>();

        MathTransform targetGtoCRS = targetGG
                .getGridToCRS(PixelInCell.CELL_CENTER);
        MathTransform sourceCRStoG = sourceGC.getGridGeometry().getCRSToGrid2D(
                PixelOrientation.CENTER);
        CoordinateReferenceSystem targetCRS = targetGG
                .getCoordinateReferenceSystem();
        CoordinateReferenceSystem sourceCRS = sourceGC
                .getCoordinateReferenceSystem();

        transforms.add(targetGtoCRS);
        if (!CRS.equalsIgnoreMetadata(sourceCRS, targetCRS)) {
            GeographicCRS sourceGeoCRS = null;
            GeographicCRS targetGeoCRS = null;
            if (sourceCRS instanceof ProjectedCRS) {
                sourceGeoCRS = ((ProjectedCRS) sourceCRS).getBaseCRS();
            }
            if (targetCRS instanceof ProjectedCRS) {
                targetGeoCRS = ((ProjectedCRS) targetCRS).getBaseCRS();
            }
            try {
                transforms.add(CRS.findMathTransform(targetCRS, targetGeoCRS,
                        true));
                toGeoXforms.addAll(transforms);
                if (CRS.equalsIgnoreMetadata(sourceGeoCRS, targetGeoCRS)) {
                    // nothing...
                } else {
                    transforms.add(CRS.findMathTransform(targetGeoCRS,
                            sourceGeoCRS));
                }
                transforms.add(CRS.findMathTransform(sourceGeoCRS, sourceCRS,
                        true));
                sourceToGeoXforms.add(0,
                        CRS.findMathTransform(sourceCRS, sourceGeoCRS));
            } catch (FactoryException e) {
                // TODO: log
                return output;
            }
        }
        transforms.add(sourceCRStoG);
        sourceToGeoXforms.add(0,
                sourceGC.getGridGeometry()
                        .getGridToCRS(PixelInCell.CELL_CENTER));

        MathTransform mt;
        try {
            mt = concatenateTransforms(transforms);
        } catch (FactoryException e1) {
            // TODO: log
            return output;
        }

        double[] coord = new double[2];

        /*
         * Output index. Assumes we iterate top-left to bottom-right in
         * row-major order.
         */
        int oi = 0;
        for (int y = 0; y < targetHeight; ++y) {
            for (int x = 0; x < targetWidth; ++x, ++oi) {
                coord[0] = x;
                coord[1] = y;
                try {
                    mt.transform(coord, 0, coord, 0, 1);
                } catch (TransformException e) {
                    continue;
                }

                // Integer cell coordinates of upper-left cell of the 2x2 cell
                // sample area
                int sulx = (int) coord[0];
                int suly = (int) coord[1];

                double fx = coord[0] - sulx;
                double fy = coord[1] - suly;

                double tv = 0; // sum of weighted valid values
                double tw = 0; // sum of valid weights
                float v0;
                double w0;
                if (sulx >= 0 && suly >= 0 && sulx < sourceWidth - 1
                        && suly < sourceHeight - 1) {
                    if (valid(v0 = fix(sourceData[suly][sulx]))) {
                        w0 = (1 - fx) * (1 - fy);
                        tv += v0 * w0;
                        tw += w0;
                    }
                    if (valid(v0 = fix(sourceData[suly][sulx + 1]))) {
                        w0 = (fx) * (1 - fy);
                        tv += v0 * w0;
                        tw += w0;
                    }
                    if (valid(v0 = fix(sourceData[suly + 1][sulx]))) {
                        w0 = (1 - fx) * (fy);
                        tv += v0 * w0;
                        tw += w0;
                    }
                    if (valid(v0 = fix(sourceData[suly + 1][sulx + 1]))) {
                        w0 = (fx) * (fy);
                        tv += v0 * w0;
                        tw += w0;
                    }
                } else {
                    if (isValidCoord(sulx, suly, sourceWidth, sourceHeight)
                            && valid(v0 = fix(sourceData[suly][sulx]))) {
                        w0 = (1 - fx) * (1 - fy);
                        tv += v0 * w0;
                        tw += w0;
                    }
                    if (isValidCoord(sulx + 1, suly, sourceWidth, sourceHeight)
                            && valid(v0 = fix(sourceData[suly][sulx + 1]))) {
                        w0 = (fx) * (1 - fy);
                        tv += v0 * w0;
                        tw += w0;
                    }
                    if (isValidCoord(sulx, suly + 1, sourceWidth, sourceHeight)
                            && valid(v0 = fix(sourceData[suly + 1][sulx]))) {
                        w0 = (1 - fx) * (fy);
                        tv += v0 * w0;
                        tw += w0;
                    }
                    if (isValidCoord(sulx + 1, suly + 1, sourceWidth,
                            sourceHeight)
                            && valid(v0 = fix(sourceData[suly + 1][sulx + 1]))) {
                        w0 = (fx) * (fy);
                        tv += v0 * w0;
                        tw += w0;
                    }
                }
                if (tw != 0) {
                    output[oi] = (float) (tv / tw);
                }
            }
        }

        return output;

    }

    private static final boolean isValidCoord(int ulx, int uly, int nx, int ny) {
        return ulx >= 0 && uly >= 0 && ulx < nx && uly < ny;
    }

    private static final boolean valid(float v) {
        return !Float.isNaN(v) && v > DATA_FILL;
    }

    // A1: passes N -9999 to test_grhi_remap, setting
    // all source -9999 values to 0.
    private static float fix(float v) {
        return v > DATA_FILL ? v : 0;
    }

    private MathTransform concatenateTransforms(
            ArrayList<MathTransform> transforms) throws FactoryException {
        Hints hints = new Hints();
        final CoordinateOperationFactory factory = ReferencingFactoryFinder
                .getCoordinateOperationFactory(hints);
        final MathTransformFactory mtFactory;
        if (factory instanceof AbstractCoordinateOperationFactory) {
            mtFactory = ((AbstractCoordinateOperationFactory) factory)
                    .getMathTransformFactory();
        } else {
            mtFactory = ReferencingFactoryFinder.getMathTransformFactory(hints);
        }

        MathTransform mt = null;
        for (MathTransform mti : transforms/*
                                            * int i = 0; i < transforms.size();
                                            * ++i
                                            */) {
            if (mt == null)
                mt = mti;
            else {
                mt = mtFactory.createConcatenatedTransform(mt, mti);
            }
        }

        return mt != null ? mt : IdentityTransform.create(2);
    }

    /**
     * Reads the raw data from the topo files
     * 
     * @param name
     *            The name of the topo data set
     * @param llLat
     *            The lower left latitude
     * @param llLon
     *            The lower left longitude
     * @param urLat
     *            The upper right latitude
     * @param urLon
     *            The upper right longitude
     * @param latCenter
     *            The central latitude
     * @param lonCenter
     *            The central longitude
     * @param nx
     *            The number of x axis points
     * @param ny
     *            The number of y axis points
     * @throws Exception
     *             If the data cannot be read
     */
    private void readTopoFile(String name) throws Exception {

        Map<String, Object> dataAttributes = TopoAttributes.getAttributes(name);
        int nx = (Integer) dataAttributes.get(TopoAttributes.NX);
        int ny = (Integer) dataAttributes.get(TopoAttributes.NY);
        long[] sizes = new long[] { nx, ny };

        // Store the data
        FloatDataRecord dataRec = new FloatDataRecord(name, "/", null, 2, sizes);
        dataRec.setFillValue(Float.NaN);

        sTopoDataStore.createDataset(dataRec);

        // Create the new input stream from which to read
        FileInputStream in = new FileInputStream(FILE_PREFIX + name
                + "Topo.dat");

        // Create a byte buffer to store the read data
        ByteBuffer bb = ByteBuffer.allocate(2);
        bb.order(ByteOrder.BIG_ENDIAN);

        byte[] bbuf = new byte[nx * 2];
        int index = 0;
        int rowIndex = 0;

        // Create an array to hold the data read
        float[] rawData = new float[nx];

        sizes[1] = 1;
        // Read in the data
        while (in.available() > 0) {
            if (in.read(bbuf) == -1) {
                break;
            }
            for (int i = 0; i < nx * 2; i += 2) {
                bb.clear();
                bb.put(bbuf[i]);
                bb.put(bbuf[i + 1]);
                rawData[index] = bb.getShort(0);

                index++;
                if (index == nx) {
                    FloatDataRecord row = new FloatDataRecord(name, "/",
                            rawData, 2, sizes);
                    row.setMinIndex(new long[] { 0, rowIndex });
                    sTopoDataStore.addDataRecord(row, sp);
                    sTopoDataStore.store();
                    index = 0;
                    rowIndex++;
                }
            }
        }

        FloatDataRecord attributeSet = new FloatDataRecord("attr" + name, "/",
                new float[] { 0 });
        attributeSet.setDataAttributes(dataAttributes);
        sTopoDataStore.addDataRecord(attributeSet, sp);
        sTopoDataStore.store();
        in.close();
    }

    /**
     * Creates a grid geometry object from the provided information
     * 
     * @param crs
     *            The coordinate reference system
     * @param llCoord
     *            The lower left coordinate
     * @param urCoord
     *            The upper right coordinate
     * @param nx
     *            The x axis points
     * @param ny
     *            The y axis points
     * @return The generated geometry
     * @throws TransformException
     * @throws MismatchedDimensionException
     * @throws FactoryException
     * @throws GribException
     *             If the geometry cannot be created
     */
    private GridGeometry2D createGridGeometry(CoordinateReferenceSystem crs,
            Coordinate llCoord, Coordinate urCoord, int nx, int ny)
            throws MismatchedDimensionException, TransformException,
            FactoryException {
        MathTransform WGS84toPROJCRS = MapUtil.getTransformFromLatLon(crs);

        GeneralEnvelope envelope = new GeneralEnvelope(2);

        DirectPosition ll = WGS84toPROJCRS.transform(new DirectPosition2D(
                llCoord.x, llCoord.y), null);

        DirectPosition ur = WGS84toPROJCRS.transform(new DirectPosition2D(
                urCoord.x, urCoord.y), null);

        envelope.setRange(0, Math.min(ll.getOrdinate(0), ur.getOrdinate(0)),
                Math.max(ll.getOrdinate(0), ur.getOrdinate(0)));
        envelope.setRange(1, Math.min(ll.getOrdinate(1), ur.getOrdinate(1)),
                Math.max(ll.getOrdinate(1), ur.getOrdinate(1)));

        envelope.setCoordinateReferenceSystem(crs);

        GridToEnvelopeMapper mapper = new GridToEnvelopeMapper();
        mapper.setEnvelope(envelope);
        mapper.setGridRange(new GeneralGridEnvelope(new int[] { 1, 1 },
                new int[] { nx, ny }, false));
        mapper.setPixelAnchor(PixelInCell.CELL_CENTER);
        mapper.setReverseAxis(new boolean[] { false, true });
        MathTransform mt = mapper.createTransform();

        return new GridGeometry2D(PixelInCell.CELL_CORNER, mt, envelope, null);
    }

    /**
     * Retrieves a slab of data from a data set and returns a float array
     * containing the data
     * 
     * @param name
     *            The name of the data set to get the data slab from
     * @param minX
     *            The starting x offset
     * @param minY
     *            The starting y offset
     * @param maxX
     *            The ending x offset
     * @param maxY
     *            The endgin y offset
     * @return The extracted slab of data
     * @throws StorageException
     * @throws FileNotFoundException
     *             If the data slab cannot be read
     */
    private float[] getSlab(String name, int minX, int minY, int maxX, int maxY)
            throws FileNotFoundException, StorageException {
        return ((FloatDataRecord) sTopoDataStore.retrieve(
                "/",
                name,
                Request.buildSlab(new int[] { minX, minY }, new int[] { maxX,
                        maxY }))).getFloatData();

    }

    /**
     * Utility method to flip a 1-D array into a 2-D array
     * 
     * @param nx
     *            The x axis points
     * @param ny
     *            The y axis points
     * @param data
     *            The 1-D data array
     * @return The 2-D data array
     */
    private float[][] to2DArray(int nx, int ny, float[] data) {
        float[][] matrix = new float[ny][nx];

        for (int i = 0; i < nx; i++) {
            for (int j = 0; j < ny; j++) {
                matrix[j][i] = data[j * nx + i];
            }
        }
        return matrix;
    }

}
