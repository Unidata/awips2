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

package com.raytheon.uf.edex.grid.staticdata.topo;

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

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.hibernate.type.SerializationException;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.geometry.DirectPosition;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.NoMetadataIdentifier;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.topo.TiledTopoSource;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * Class used for accessing static topography information for GFE smart inits
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 27, 2010  6394     bphillip    Initial creation
 * Oct 08, 2010  6394     bphillip    Rewrote sections for optimal reading and
 *                                    writing performance
 * Sep 19, 2011  10955    rferrel     Use RunProcess
 * Apr 18, 2012  14694    D. Friedman Fixes for static topography generation
 * May 09, 2012  14939    D. Friedman Fix errors in DR 14694
 * Jan 14, 2013  1469     bkowal      Removed the hdf5 data directory
 * Feb 12, 2013  1608     randerso    Changed to call deleteDatasets
 * Aug 06, 2013  2235     bsteffen    Added Caching version of TopoQuery.
 * Aug 06, 2013  3805     bsteffen    Add timing to logging.
 * Apr 29, 2015  4167     nabowle     Propagate exceptions from
 *                                    #initStopoData(GridCoverage)
 * Nov 02, 2016  5979     njensen     Cast to Number where applicable
 * Sep 23, 2021  8608     mapeters    Add metadata id handling
 *
 * </pre>
 *
 * @author bphillip
 */
public class StaticTopoData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StaticTopoData.class);

    private static final float DATA_FILL = -9999;

    private static final float TOPO_FILL = -999_999;

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

    /** The base directory in which the topo files reside */
    private static final String FILE_PREFIX = "topo/";

    /** The file containing the complete static topo data sets */
    private static final File topoFile = new File(
            FILE_PREFIX + "staticTopo.h5");

    /** The file containing the site specific static topo data sets */
    private static final File siteTopoFile = new File(
            FILE_PREFIX + "modelStaticTopo.h5");

    /** HDF5 storage properties used for enabling compression */
    private static final StorageProperties sp;

    /** The singleton instance */
    private static StaticTopoData instance;

    private List<TiledTopoSource> topoSources;

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
                ct = ClusterLockUtils.lock(TASK_NAME, "unpack", 300_000, true);
            } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));

            try {
                if (!topoFileExists()) {
                    // TODO: This will fail in a clustered server environment
                    // since static topo isn't installed to dx3/4
                    // UPDATE: this doesn't even work in a standalone
                    // environment now because it can't find the gzipped source
                    // files since FILE_PREFIX was changed
                    statusHandler.handle(Priority.INFO,
                            "Static Topo file not found. Creating it...");

                    // Initializes the World topo data
                    statusHandler.handle(Priority.INFO,
                            "Initializing World Topo Data...");
                    // DR#10955
                    RunProcess.getRunProcess().exec("gunzip " + FILE_PREFIX
                            + "worldTopo" + DAT_GZ_SUFFIX).waitFor();
                    readTopoFile("world");
                    statusHandler.handle(Priority.INFO,
                            "World Topo Data Initialized!");

                    // Initializes the US topo data
                    statusHandler.handle(Priority.INFO,
                            "Initializing US Topo Data...");
                    // DR#10955
                    RunProcess.getRunProcess().exec(
                            "gunzip " + FILE_PREFIX + "usTopo" + DAT_GZ_SUFFIX)
                            .waitFor();
                    readTopoFile("us");
                    statusHandler.handle(Priority.INFO,
                            "US Topo Data Initialized!");

                    // Initializes the Carribean topo data
                    statusHandler.handle(Priority.INFO,
                            "Initializing Carribean Topo Data...");
                    // DR#10955
                    RunProcess.getRunProcess().exec("gunzip " + FILE_PREFIX
                            + "caribTopo" + DAT_GZ_SUFFIX).waitFor();
                    readTopoFile("carib");
                    statusHandler.handle(Priority.INFO,
                            "Carribean Topo Data Initialized!");

                    // Initializes the Alaska topo data
                    statusHandler.handle(Priority.INFO,
                            "Initializing Alaska Topo Data...");
                    // DR#10955
                    RunProcess.getRunProcess().exec(
                            "gunzip " + FILE_PREFIX + "akTopo" + DAT_GZ_SUFFIX)
                            .waitFor();
                    readTopoFile("ak");
                    statusHandler.handle(Priority.INFO,
                            "Alaska Topo Data Initialized!");

                    // Initializes the Pacific topo data
                    statusHandler.handle(Priority.INFO,
                            "Initializing Pacific Topo Data...");
                    // DR#10955
                    RunProcess.getRunProcess().exec(
                            "gunzip " + FILE_PREFIX + "pacTopo" + DAT_GZ_SUFFIX)
                            .waitFor();
                    readTopoFile("pac");
                    statusHandler.handle(Priority.INFO,
                            "Pacific Topo Data Initialized!");
                    splitPacific();
                }
            } finally {
                ClusterLockUtils.unlock(ct, false);
            }

            initAttributes();
            initSources();

        } catch (Exception e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error initializing static topo data!", e);
        }
        statusHandler.handle(Priority.INFO, "Static topo data initialized");
    }

    /**
     * Prepare DataSource for interpolation. Since TiledTopoSource loads all
     * data on demand this will inititialize the source but will not request any
     * data.
     *
     * @throws FactoryException
     * @throws MismatchedDimensionException
     * @throws TransformException
     */
    private void initSources() throws FactoryException,
            MismatchedDimensionException, TransformException {
        topoSources = new ArrayList<>();
        for (String topoData : TOPO_FILES) {
            for (String dataSet : TopoAttributes.attributeMap.keySet()) {
                if (dataSet.startsWith(topoData)) {
                    // Gets the attributes for the data set
                    Map<String, Object> attributes = TopoAttributes
                            .getAttributes(dataSet);
                    int nx = ((Number) attributes.get(TopoAttributes.NX))
                            .intValue();
                    int ny = ((Number) attributes.get(TopoAttributes.NY))
                            .intValue();
                    double llLat = MapUtil.correctLat(
                            ((Number) attributes.get(TopoAttributes.LL_LAT))
                                    .doubleValue());
                    double llLon = MapUtil.correctLon(
                            ((Number) attributes.get(TopoAttributes.LL_LON))
                                    .doubleValue());
                    double urLat = MapUtil.correctLat(
                            ((Number) attributes.get(TopoAttributes.UR_LAT))
                                    .doubleValue());
                    double urLon = MapUtil.correctLon(
                            ((Number) attributes.get(TopoAttributes.UR_LON))
                                    .doubleValue());

                    // Create the CRS of the topo data
                    CoordinateReferenceSystem topoCrs = CRSCache.getInstance()
                            .getCoordinateReferenceSystem((String) attributes
                                    .get(TopoAttributes.CRS));

                    // Create the geometry of the topo data
                    GridGeometry2D topoGeom = createGridGeometry(topoCrs,
                            new Coordinate(llLon, llLat),
                            new Coordinate(urLon, urLat), nx, ny);
                    topoSources.add(new TiledTopoSource(256, topoGeom,
                            sTopoDataStore, dataSet));
                }
            }
        }
    }

    /**
     * Splits the Pacific grid into east and west hemisphere chunks to reduce
     * memory usage
     *
     * @throws Exception
     */
    private void splitPacific() throws Exception {
        Map<String, Object> pacAttributes = TopoAttributes.getAttributes("pac");
        int nx = ((Number) pacAttributes.get(TopoAttributes.NX)).intValue();
        int ny = ((Number) pacAttributes.get(TopoAttributes.NY)).intValue();

        Map<String, Object> attributes = new HashMap<>();
        attributes.putAll(pacAttributes);
        attributes.put(TopoAttributes.NX, nx / 2);
        attributes.put(TopoAttributes.UR_LON, new Float(180.0f));
        float[] floatData = this.getSlab("pac", 0, 0, nx / 2, ny);
        FloatDataRecord eastRecord = new FloatDataRecord("pac_east", "/",
                floatData, 2, new long[] { nx / 2, ny });
        sTopoDataStore.addDataRecord(eastRecord, new NoMetadataIdentifier(),
                sp);

        FloatDataRecord attributeSet = new FloatDataRecord("attr" + "pac_east",
                "/", new float[] { 0 });
        attributeSet.setDataAttributes(attributes);
        sTopoDataStore.addDataRecord(attributeSet, new NoMetadataIdentifier(),
                sp);
        sTopoDataStore.store();

        attributes = new HashMap<>();
        attributes.putAll(pacAttributes);
        attributes.put(TopoAttributes.NX, nx / 2);
        attributes.put(TopoAttributes.LL_LON, new Float(-180.0f));
        floatData = this.getSlab("pac", nx / 2, 0, nx, ny);
        FloatDataRecord westRecord = new FloatDataRecord("pac_west", "/",
                floatData, 2, new long[] { nx / 2, ny });
        attributeSet = new FloatDataRecord("attr" + "pac_west", "/",
                new float[] { 0 });
        attributeSet.setDataAttributes(attributes);
        sTopoDataStore.addDataRecord(attributeSet, new NoMetadataIdentifier(),
                sp);
        sTopoDataStore.addDataRecord(westRecord, new NoMetadataIdentifier(),
                sp);
        sTopoDataStore.store();
        sTopoDataStore.deleteDatasets("pac", "attrpac");
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
        } catch (@SuppressWarnings("squid:S1166")
        Exception e) {
            // Assume exception just means that data doesn't exist
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
                TopoAttributes.attributeMap.put(dataset.replace("attr", ""),
                        sTopoDataStore
                                .retrieve("/", dataset,
                                        Request.buildPointRequest(
                                                new Point(0, 0)))
                                .getDataAttributes());
            }
        }
    }

    /**
     * Initializes the topography data for the given site. The data is extracted
     * from the static topo file and resampled according to the site location
     * information
     *
     * @param coverage
     *
     * @throws StorageException
     * @throws SerializationException
     *             If the topography data cannot be initialized for the given
     *             site
     */
    public void initStopoData(GridCoverage coverage)
            throws StorageException, SerializationException {
        if (coverage.getNx() < 0 || coverage.getNy() < 0) {
            statusHandler.handle(Priority.PROBLEM,
                    coverage.getName() + " is not applicable to "
                            + EDEXUtil.getEdexSite() + ". Skipping.");
            return;
        }
        long startTimeMillis = System.currentTimeMillis();
        GridGeometry2D inGeom = MapUtil.getGridGeometry(coverage);

        // Gets the location data and extracts it from the static topo file
        float[] finalData = null;
        finalData = getTopoData(inGeom);

        // Create an HDF5 data record and store it
        FloatDataRecord outRecord = new FloatDataRecord(STOPO_DATASET,
                coverage.spatialKey(), finalData, 2,
                new long[] { inGeom.getGridRange().getHigh(0) + 1,
                        inGeom.getGridRange().getHigh(1) + 1 });
        siteDataStore.addDataRecord(outRecord, new NoMetadataIdentifier(), sp);
        StorageStatus storestatus = siteDataStore.store(StoreOp.REPLACE);
        long endTimeMillis = System.currentTimeMillis();
        if (storestatus.hasExceptions()) {
            throw storestatus.getExceptions()[0];
        }

        statusHandler.handle(Priority.INFO,
                "Stopo data successfully initialized for " + coverage.getName()
                        + " in " + (endTimeMillis - startTimeMillis) + "ms");
    }

    /**
     * Private method used by the initialization code to see if the static topo
     * data for a coverage has been initialized
     *
     * @throws StorageException
     * @throws SerializationException
     *             If the topography data cannot be initialized for the given
     *             site
     */
    public boolean checkModelTopo(GridCoverage coverage)
            throws SerializationException, StorageException {
        ClusterTask ct = ClusterLockUtils.lock(TASK_NAME, coverage.spatialKey(),
                120_000, false);

        if (LockState.SUCCESSFUL.equals(ct.getLockState())) {
            // set processed to true regardless of successfully
            // update coverage so we don't infinite loop
            try {
                if (!topoExists(coverage)) {
                    statusHandler.handle(Priority.INFO,
                            "Initializing static topo for grid "
                                    + coverage.getName());
                    initStopoData(coverage);
                }
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
     */
    private boolean topoExists(GridCoverage coverage) {

        List<String> dataSets = null;
        try {
            String[] modelArray = siteDataStore
                    .getDatasets(coverage.spatialKey());
            dataSets = Arrays.asList(modelArray);
        } catch (@SuppressWarnings("squid:S1166")
        Exception e) {
            // Assume exception just means that data doesn't exist
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
     * @param coverage
     *
     * @return The static topo data
     * @throws StorageException
     * @throws SerializationException
     *             If the topography data cannot be initialized for the given
     *             site
     *
     */
    public FloatDataRecord getStopoData(GridCoverage coverage)
            throws SerializationException, StorageException {
        if (!topoExists(coverage)) {
            while (!checkModelTopo(coverage)) {
            }
        }
        FloatDataRecord record = null;
        try {
            record = (FloatDataRecord) siteDataStore.retrieve(
                    coverage.spatialKey(), STOPO_DATASET, Request.ALL);

        } catch (Exception e) {
            statusHandler.info("Static topo data does not exist for grid "
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
     * @return A float array containing the extracted topo data
     */
    private float[] getTopoData(GridGeometry2D inGeom) {
        FloatBufferWrapper finalDataWrapper = null;
        BilinearInterpolation interp = new BilinearInterpolation();
        interp.setMissingThreshold(0.0001f);

        for (TiledTopoSource source : topoSources) {
            statusHandler
                    .info("Extracting topo data from " + source.getDataset());
            GridReprojection reprojection = new GridReprojection(
                    source.getGridGeometry(), inGeom);
            GridSampler sampler = new GridSampler(source, interp);
            try {
                if (finalDataWrapper == null) {
                    finalDataWrapper = new FloatBufferWrapper(
                            inGeom.getGridRange2D());
                    reprojection.reprojectedGrid(sampler, finalDataWrapper);
                } else {
                    boolean done = true;
                    for (int i = 0; i < inGeom.getGridRange2D().width; i += 1) {
                        for (int j = 0; j < inGeom
                                .getGridRange2D().height; j += 1) {
                            if (Double.isNaN(
                                    finalDataWrapper.getDataValue(i, j))) {
                                done = false;
                                double val = reprojection
                                        .reprojectedGridCell(sampler, i, j);
                                finalDataWrapper.setDataValue(val, i, j);
                            }
                        }
                    }
                    if (done) {
                        break;
                    }
                }
            } catch (FactoryException | TransformException e) {
                statusHandler.warn(e.getLocalizedMessage(), e);
            }
        }
        float[] finalData = finalDataWrapper.getArray();

        for (int i = 0; i < finalData.length; i++) {
            float v = finalData[i];
            if (Float.isNaN(v)) {
                finalData[i] = TOPO_FILL;
            } else if (v == DATA_FILL || (v > -0.5 && v < 0.5)) {
                finalData[i] = 0.0f;
            } else {
                finalData[i] = v;
            }
        }
        return finalData;

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
        int nx = ((Number) dataAttributes.get(TopoAttributes.NX)).intValue();
        int ny = ((Number) dataAttributes.get(TopoAttributes.NY)).intValue();
        long[] sizes = new long[] { nx, ny };

        // Store the data
        FloatDataRecord dataRec = new FloatDataRecord(name, "/", null, 2,
                sizes);
        dataRec.setFillValue(Float.NaN);

        sTopoDataStore.createDataset(dataRec);

        // Create the new input stream from which to read
        try (FileInputStream in = new FileInputStream(
                FILE_PREFIX + name + "Topo.dat")) {

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
                        sTopoDataStore.addDataRecord(row,
                                new NoMetadataIdentifier(), sp);
                        sTopoDataStore.store();
                        index = 0;
                        rowIndex++;
                    }
                }
            }

            FloatDataRecord attributeSet = new FloatDataRecord("attr" + name,
                    "/", new float[] { 0 });
            attributeSet.setDataAttributes(dataAttributes);
            sTopoDataStore.addDataRecord(attributeSet,
                    new NoMetadataIdentifier(), sp);
            sTopoDataStore.store();
        }
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
     */
    private GridGeometry2D createGridGeometry(CoordinateReferenceSystem crs,
            Coordinate llCoord, Coordinate urCoord, int nx, int ny)
            throws MismatchedDimensionException, TransformException,
            FactoryException {
        MathTransform WGS84toPROJCRS = MapUtil.getTransformFromLatLon(crs);

        GeneralEnvelope envelope = new GeneralEnvelope(2);

        DirectPosition ll = WGS84toPROJCRS
                .transform(new DirectPosition2D(llCoord.x, llCoord.y), null);

        DirectPosition ur = WGS84toPROJCRS
                .transform(new DirectPosition2D(urCoord.x, urCoord.y), null);

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
        return ((FloatDataRecord) sTopoDataStore.retrieve("/", name, Request
                .buildSlab(new int[] { minX, minY }, new int[] { maxX, maxY })))
                        .getFloatData();

    }

}
