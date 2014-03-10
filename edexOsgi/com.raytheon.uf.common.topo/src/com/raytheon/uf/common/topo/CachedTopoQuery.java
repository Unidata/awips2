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
package com.raytheon.uf.common.topo;

import java.io.File;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Map;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.LatLonGridSampler;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Replacement for TopoQuery that uses cached tiles. If the cache is not
 * populated then requests may take slightly longer as tiles are retrieved but
 * for doing multiple requests in the same area the cache will provide very fast
 * results.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2013            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CachedTopoQuery {

    private static final int TILE_SIZE = 256;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CachedTopoQuery.class);

    private static Map<Integer, CachedTopoQuery> topoQueryMap;

    /**
     * @return Initialized TopoQuery instance
     */
    public static synchronized CachedTopoQuery getInstance() {
        return getInstance(0);
    }

    /**
     * @return Initialized TopoQuery instance
     */
    public static synchronized CachedTopoQuery getInstance(int topoLevel) {
        return getInstance(TopoUtils.getDefaultTopoFile(), topoLevel);
    }

    public static synchronized CachedTopoQuery getInstance(File hdf5File,
            int topoLevel) {
        if (topoQueryMap == null) {
            topoQueryMap = new Hashtable<Integer, CachedTopoQuery>();
        }
        CachedTopoQuery query = topoQueryMap.get(topoLevel);
        if (query == null) {
            try {
                query = new CachedTopoQuery(hdf5File, topoLevel);
                topoQueryMap.put(topoLevel, query);
            } catch (TopoException e) {
                statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
            }
        }
        return query;
    }

    private final TiledTopoSource source;

    private final LatLonGridSampler latLonSampler;

    private CachedTopoQuery(File hdf5File, int level) throws TopoException {
        IDataStore dataStore = DataStoreFactory.getDataStore(hdf5File);
        try {
            String dataset = TopoUtils.getDatasetForLevel(level);
            GridGeometry2D gridGeometry = TopoUtils.getTopoGeometry(
                    dataStore, dataset);
            source = new TiledTopoSource(TILE_SIZE, gridGeometry, dataStore,
                    dataset);
            latLonSampler = new LatLonGridSampler(source.getGridGeometry(), source,
                    new NearestNeighborInterpolation());
        } catch (Exception e) {
            throw new TopoException(e);
        }
    }

    /**
     * Retrieves topo height in meters above mean sea level for the specified
     * coordinate.
     * 
     * @param latLon
     *            should contain lon/lat in degrees
     * @return topo height in meters MSL
     */
    public double getHeight(Coordinate latLon) {
        try {
            return latLonSampler.sample(latLon.x, latLon.y);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retriving topo value for lat/lons", e);
            return Double.NaN;
        }
    }

    /**
     * Retrieves topo height in meters above mean sea level for the specified
     * coordinates.
     * 
     * @param coords
     *            should contain lon/lat in degrees
     * @return topo height in meters MSL
     */
    public double[] getHeight(Coordinate[] coords) {
        double[] result = new double[coords.length];
        for (int i = 0; i < result.length; i += 1) {
            try {
                result[i] = latLonSampler.sample(coords[i].x, coords[i].y);
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retriving topo value for lat/lons", e);
                result[i] = Double.NaN;
            }
        }
        return result;
    }

    /**
     * Retrieves topo height in meters above mean sea level reprojected and
     * interpolated to the specified grid geometry.
     * 
     * @param targetGeom
     * @return the topo data array in row major order
     */
    public float[] getHeight(GridGeometry2D targetGeom) {
        FloatBufferWrapper destination = new FloatBufferWrapper(
                targetGeom.getGridRange2D());
        GridReprojection reprojection = new GridReprojection(source.getGridGeometry(), targetGeom);
        try {
            reprojection.reprojectedGrid(new NearestNeighborInterpolation(), source, destination);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            Arrays.fill(destination.getArray(), Float.NaN);
        }
        return destination.getArray();
    }
}
