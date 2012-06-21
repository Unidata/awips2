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
package com.raytheon.uf.common.climo;

import java.awt.image.Raster;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
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
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.TimeRange;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2010            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class ClimoQuery implements IClimoQuery {

    private static Map<String, ClimoQuery> instanceMap = null;

    private static SimpleDateFormat sdf = null;

    private static final String basePath;

    static {
        String bp = "";
        try {
            instanceMap = new HashMap<String, ClimoQuery>();
            sdf = new SimpleDateFormat("yyyy_MM_dd_HH");

            EnvProperties properties = PropertiesFactory.getInstance()
                    .getEnvProperties();
            if (properties != null) {
                bp = properties.getEnvValue("HDF5DIR");
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            basePath = bp;
        }
    }

    private String module;

    protected ClimoQuery(String module) {
        this.module = module;
    }

    /**
     * @return
     */
    public static synchronized ClimoQuery getInstance(String module) {
        ClimoQuery instance = instanceMap.get(module);
        if (instance == null) {
            instance = new ClimoQuery(module);
            instanceMap.put(module, instance);
        }

        return instance;
    }

    /**
     * Get the climate grid specified by the parameters. This is a low-level
     * method. The time range is assumed to be one we obtained previously from
     * the same database through a call to getInventory().
     * 
     * @param geom
     *            The grid geometry describing the area of interest
     * @param timeRange
     *            The time range of interest
     * @param parmName
     *            The climo weather element, for example, "mxt".
     * @return A 1D array of float values retrieved from HDF5.
     */
    @Override
    public float[] getClimate(GridGeometry2D geom, TimeRange timeRange,
            String parmName, String modelName) throws DataAccessLayerException {

        String h5FilePath = buildFilePath(modelName);
        // get the HDF5 data store
        File file = new File(h5FilePath);
        IDataStore dataStore = DataStoreFactory.getDataStore(file);

        // Construct the path to the data within the data store
        String dataPath = String.format("%s/SFC/%s--%s", parmName,
                sdf.format(timeRange.getStart()),
                sdf.format(timeRange.getEnd()));

        IDataRecord[] records = null;
        try {
            records = dataStore.retrieve(dataPath);
        } catch (Exception e) {
            throw new DataAccessLayerException("Error retrieving \"" + dataPath
                    + "\".", e);
        }

        String subMessage = null;
        if (records == null) {
            subMessage = "is null";
        } else if (records.length == 0) {
            subMessage = "is empty";
        } else if (records.length > 1) {
            subMessage = "contains more than one record";
        }
        if (subMessage != null) {
            throw new DataAccessLayerException("Data record array for \""
                    + dataPath + "\" " + subMessage);
        }

        FloatDataRecord record = (FloatDataRecord) records[0];

        float lat00 = getScalarFloat(dataStore, "attributes", "lat00");
        float lon00 = getScalarFloat(dataStore, "attributes", "lon00");
        float latNxNy = getScalarFloat(dataStore, "attributes", "latNxNy");
        float lonNxNy = getScalarFloat(dataStore, "attributes", "lonNxNy");

        GeographicCRS fromCrs = MapUtil.getLatLonProjection();
        Point[] cornerPoints = new Point[2];
        cornerPoints[0] = MapUtil.getPoint(lat00, lon00);
        cornerPoints[1] = MapUtil.getPoint(latNxNy, lonNxNy);

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

        GridCoverage2D srcCoverage = null;
        try {
            srcCoverage = MapUtil.constructGridCoverage(dataPath, grid,
                    fromCrs, cornerPoints);
        } catch (Exception e) {
            throw new DataAccessLayerException("Error creating grid coverage "
                    + dataPath + " from HDF5 data", e);
        }
        Interpolation interp = Interpolation
                .getInstance(Interpolation.INTERP_NEAREST);
        GridCoverage2D dstCoverage = MapUtil.reprojectCoverage(srcCoverage,
                geom.getCoordinateReferenceSystem(), geom, interp);

        Raster data = dstCoverage.getRenderedImage().getData();
        float[] f1 = null;
        f1 = data.getPixels(data.getMinX(), data.getMinY(), data.getWidth(),
                data.getHeight(), f1);

        return f1;
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

    /**
     * Get the list of time ranges for which this climo instance has data.
     * 
     * @return the time ranges
     */
    public synchronized List<TimeRange> getInventory(String parmName,
            String modelName) {
        List<TimeRange> times = null;

        final String filePath = buildFilePath(modelName);

        // Set up the paths for the inventory flags and times in seconds
        final String invPath = "/" + parmName + "/SFC";
        final String invTimePath = "/valtimeMINUSreftime";

        // Create an HDF5 data store to read from the file
        File file = new File(filePath);
        IDataStore dataStore = DataStoreFactory.getDataStore(file);

        StringDataRecord invFlagRec = null;
        IntegerDataRecord timeRec = null;
        try {
            // Get the inventory
            invFlagRec = (StringDataRecord) dataStore.retrieve(invPath,
                    "Inventory", Request.ALL);
            timeRec = (IntegerDataRecord) dataStore.retrieve(invTimePath,
                    "Data", Request.ALL);
        } catch (Exception e) {
            throw new RuntimeException("Error retrieving inventory", e);
        }

        if (invFlagRec == null || timeRec == null) {

        } else {
            String[] flag = invFlagRec.getStringData();

            int[] timeInts = timeRec.getIntData();
            // Convert the inventory to time ranges.
            if (flag.length == timeInts.length) {

                times = new ArrayList<TimeRange>(12);
                for (int dualIdx = 0; dualIdx < flag.length; dualIdx++) {
                    if ("1".equals(flag[dualIdx])) {
                        int seconds = timeInts[dualIdx];
                        Calendar cal = Calendar.getInstance();
                        cal.setTimeInMillis(1000L * seconds);
                        Date startTime = cal.getTime();
                        cal.add(Calendar.HOUR, 1);
                        Date endTime = cal.getTime();
                        TimeRange range = new TimeRange(startTime, endTime);
                        times.add(range);
                    }
                }
            }
        }
        return times;
    }

    /**
     * Build the path to the HDF5 file that should provide climo data for the
     * given model
     * 
     * @param modelName
     *            The string model name: a collection of tokens separated by
     *            underscores.
     * @return the path to the climo HDF5 file for modelName
     */
    protected String buildFilePath(String modelName) {
        String[] mnparts = modelName.split("_");
        String source = "PRISM";
        if (mnparts.length > 3) {
            if ("NCDC".equals(mnparts[3])) {
                source = "NCDC";
            }
        }

        final String filePath = basePath + File.separator + module
                + File.separator + "climo" + File.separator + source
                + "Climo.h5";
        return filePath;
    }
}
