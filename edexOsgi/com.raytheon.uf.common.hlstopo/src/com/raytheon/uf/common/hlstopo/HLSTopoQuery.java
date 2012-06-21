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
package com.raytheon.uf.common.hlstopo;

import java.awt.image.Raster;
import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.media.jai.Interpolation;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.GeographicCRS;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.vividsolutions.jts.geom.Point;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2011            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class HLSTopoQuery {

    /** the subdirectory in edex/hdf5 in which to find HLS topo files. */
    private static final String HLS_TOPO = "hlsTopo";

    private static final Map<String, HLSTopoQuery> instanceMap;

    private static final String basePath;

    // TODO: Move these constants into a resource bundle
    private static final String OLD_FOLDER = "NED3ARCSTOPO";

    private static final String NEW_FOLDER = "NED3ARCSTOPONEW";

    private static final String OLD_FILE = "20100318_0000.h5";

    private static final String NEW_FILE = "20110607_1200.h5";

    static {
        instanceMap = new HashMap<String, HLSTopoQuery>();
        String bp = "";
        EnvProperties properties = PropertiesFactory.getInstance()
                .getEnvProperties();
        if (properties != null) {
            bp = properties.getEnvValue("HDF5DIR");
        }
        basePath = bp;
    }

    private String module;

    protected HLSTopoQuery(String module) {
        this.module = module;
    }

    /**
     * @return
     */
    public static synchronized HLSTopoQuery getInstance(String module) {
        HLSTopoQuery instance = instanceMap.get(module);
        if (instance == null) {
            instance = new HLSTopoQuery(module);
            instanceMap.put(module, instance);
        }

        return instance;
    }

    /**
     * @param targetGeom
     * @return
     */
    public float[] getHeight(GridGeometry2D geom, String parmName, String dbName)
            throws DataAccessLayerException {

        String h5FilePath = buildFilePath(dbName);
        // get the HDF5 CONUS data store
        File file = new File(h5FilePath);
        IDataStore dataStore = DataStoreFactory.getDataStore(file);

        // Construct the path to the data within the data store
        String dataPath = String.format("%s/SFC", parmName);

        // get the data grid
        IDataRecord irecord = null;
        try {
            irecord = dataStore.retrieve(dataPath, "Data", Request.ALL);
        } catch (Exception e) {
            throw new DataAccessLayerException("Error retrieving \"" + dataPath
                    + "\".", e);
        }

        if (irecord == null) {
            throw new DataAccessLayerException("Data record for \"" + dataPath
                    + "\" is null");
        }

        FloatDataRecord record = (FloatDataRecord) irecord;

        // Get the corner points from the hdf5 attributes group
        float lat00 = getScalarFloat(dataStore, "attributes", "lat00");
        float lon00 = getScalarFloat(dataStore, "attributes", "lon00");
        float latNxNy = getScalarFloat(dataStore, "attributes", "latNxNy");
        float lonNxNy = getScalarFloat(dataStore, "attributes", "lonNxNy");

        // Use the corner points to build a coordinate reference system
        // for the CONUS map.
        GeographicCRS fromCrs = MapUtil.getLatLonProjection();
        Point[] cornerPoints = new Point[2];
        cornerPoints[0] = MapUtil.getPoint(lat00, lon00);
        cornerPoints[1] = MapUtil.getPoint(latNxNy, lonNxNy);

        // convert the 1D float data array to 2D
        float[] floatData = record.getFloatData();
        long[] sizes = record.getSizes();
        int nx = (int) sizes[0];
        int ny = (int) sizes[1];
        float[][] grid = new float[ny][nx];
        int rowOffset = 0;
        for (int row = 0; row < ny; row++) {
            rowOffset = row * nx;
            for (int col = 0; col < nx; col++) {
                grid[row][col] = floatData[rowOffset + col];
            }
        }

        // Create a grid coverage over the CONUS grid.
        GridCoverage2D srcCoverage = null;
        try {
            srcCoverage = MapUtil.constructGridCoverage(dataPath, grid,
                    fromCrs, cornerPoints);
        } catch (Exception e) {
            throw new DataAccessLayerException("Error creating grid coverage "
                    + dataPath + " from HDF5 data", e);
        }

        // Project from srcCoverage into geom to get single-site dstCoverage
        Interpolation interp = Interpolation
                .getInstance(Interpolation.INTERP_NEAREST);
        GridCoverage2D dstCoverage = MapUtil.reprojectCoverage(srcCoverage,
                geom.getCoordinateReferenceSystem(), geom, interp);

        // Extract the projected grid from dstCoverage
        Raster data = dstCoverage.getRenderedImage().getData();
        float[] f1 = null;
        f1 = data.getPixels(data.getMinX(), data.getMinY(), data.getWidth(),
                data.getHeight(), f1);

        return f1;
    }

    /**
     * @return
     */
    public String getModule() {
        return module;
    }

    /**
     * Build the path to the HDF5 file that should provide HLS topo data for the
     * given database name.
     * 
     * @param dbName
     *            The database name. Current options are "NED" and "CRMTopo".
     * @return the path to the HLS Topo file for dbName.
     */
    protected String buildFilePath(String dbName)
            throws DataAccessLayerException {

        // Use dbName to decide if we want the "new" directory
        String folder = NEW_FOLDER;
        String fname = null;
        fname = NEW_FILE;
        if ("CRMTopo".equals(dbName)) {
            folder = OLD_FOLDER;
            fname = OLD_FILE;
        }
        final String FS = File.separator;
        final String dirPath = basePath + FS + "topo" + FS + HLS_TOPO + FS
                + dbName + FS + folder;

        // On deployed systems, dirpath is in pypies and can't be listed.
        // Going back to hardcoded filename temporarily.
        // final File dir = new File(dirPath);
        // if (!dir.exists()) {
        // throw new DataAccessLayerException("Directory " + dirPath
        // + " does not exist.");
        // }
        // String[] fileNames = dir.list(new FilenameFilter() {
        // @Override
        // public boolean accept(File dir, String name) {
        // return name.endsWith(".h5");
        // }
        // });
        // if (fileNames == null || fileNames.length == 0) {
        // throw new DataAccessLayerException("Directory " + dirPath
        // + " does not contain any HDF5 files.");
        // }
        //
        // Arrays.sort(fileNames);
        // fname = fileNames[fileNames.length - 1];

        final String filePath = dirPath + FS + fname;
        return filePath;
    }

    /**
     * @param dataStore
     *            The data store from which to take the value
     * @param group
     *            The path to the group holding the dataset
     * @param dataset
     *            The dataset holding the value
     * @return the data value
     * @throws DataAccessLayerException
     *             if retrieving the data space throws an exception, or if the
     *             data space does not hold a single float value.
     */
    protected float getScalarFloat(IDataStore dataStore, String group,
            String dataset) throws DataAccessLayerException {
        IDataRecord rec = null;
        try {
            rec = dataStore.retrieve(group, dataset, Request.ALL);
        } catch (Exception e) {
            throw new DataAccessLayerException("Error retrieving \"" + group
                    + "\"", e);
        }

        if (rec.getDimension() == 1) {
            Object recObj = rec.getDataObject();
            if (recObj instanceof float[]) {
                float[] recFA = (float[]) recObj;
                if (recFA.length == 1) {
                    return recFA[0];
                }
            }
        }

        throw new DataAccessLayerException("Item \"" + group + File.separator
                + dataset + "\" is not a scalar float");
    }

}
