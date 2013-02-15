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
package com.raytheon.uf.common.geospatial;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.projection.ProjectionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Convert a {@link GeneralGridGeometry} to Lat/Lon projection, with methods for
 * retrieving just the lats and just the lons
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2013            mnash     Initial creation
 * Feb 15, 2013 1614       bsteffen    Cache LatLonReprojection results.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class LatLonReprojection {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LatLonReprojection.class);

    // The cache is an LRU map that holds a soft reference to LatLonWrapper.
    // Using an LRU map prevents the buildup of empty references and places a
    // reasonable upper limit on the number of objects to cache. Using soft
    // references prevents running out of memory and allows all the memory to be
    // reclaimed if the LatLonReprojection is not used for awhile.
    private static Map<GeneralGridGeometry, Reference<LatLonWrapper>> cache = new LinkedHashMap<GeneralGridGeometry, Reference<LatLonWrapper>>(
            64, 0.8f, true) {

        private static final long serialVersionUID = 1L;

        @Override
        protected boolean removeEldestEntry(
                Entry<GeneralGridGeometry, Reference<LatLonWrapper>> eldest) {
            if (size() > 50) {
                return true;
            }
            return false;
        }

    };

    /**
     * Take a {@link GeneralGridGeometry} and reproject it to lat/lon space
     * 
     * @param source
     * @return float[] of all lat/lon points
     */
    private static float[] reproject(GeneralGridGeometry source) {
        MathTransform gridToCRS = source.getGridToCRS(PixelInCell.CELL_CENTER);
        DefaultMathTransformFactory mtf = new DefaultMathTransformFactory();

        int sourceNx = source.getGridRange().getSpan(0);
        int sourceNy = source.getGridRange().getSpan(1);

        float[] transformTable = new float[sourceNx * sourceNy * 2];
        try {
            // create a concatenated transform with the one above and to
            // lat/lon
            MathTransform finalTransform = null;
            finalTransform = mtf.createConcatenatedTransform(gridToCRS,
                    MapUtil.getTransformToLatLon(source
                            .getCoordinateReferenceSystem()));
            int index = 0;
            for (int j = 0; j < sourceNy; j++) {
                for (int i = 0; i < sourceNx; i++) {
                    transformTable[index++] = i;
                    transformTable[index++] = j;
                }
            }
            finalTransform.transform(transformTable, 0, transformTable, 0,
                    sourceNx * sourceNy);
        } catch (ProjectionException e) {
            // do nothing
        } catch (TransformException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to transform to Lat/Lon projection", e);
        } catch (InvalidGridGeometryException e) {
            statusHandler.handle(Priority.ERROR, "Grid geometry is invalid", e);
        } catch (FactoryException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }

        return transformTable;
    }

    /**
     * Get the latitudes as an array after being reprojected
     * 
     * @param source
     * @return
     */
    public static LatLonWrapper getLatLons(GeneralGridGeometry source) {
        // normalize grid geometry to a GridGeometry2D object since a
        // GeneralGridGeometry and a GridGeometry2D can produce identical
        // results but cannot be interchanged as map keys.
        source = GridGeometry2D.wrap(source);
        LatLonWrapper wrapper = null;
        Reference<LatLonWrapper> wrapperRef = null;
        synchronized (cache) {
            // There are three paths through this sync block.
            // 1) The wrapper was in the cache and can be returned.
            // 2) The wrapperRef was in the cache but it has no wrapper, the
            // wrapperRef can be used as a lock object for synchronizing this
            // geometry.
            // 3) Nothing is in the cache, an empty ref will be placed in the
            // cache to provide a lock object for this geometry.
            wrapperRef = cache.get(source);
            if (wrapperRef != null) {
                wrapper = wrapperRef.get();
            } else {
                wrapperRef = new SoftReference<LatLonWrapper>(null);
                cache.put(source, wrapperRef);
            }
        }
        if (wrapper == null) {
            // wrapperRef is used to provide fine grained locking on an
            // individual geometry. Different geometries can run simultaneously.
            synchronized (wrapperRef) {
                // If 2 threads manage to block on the same wrapperRef then the
                // second thread will be able to pull the wrapper out of the
                // cache and avoid calculation.
                synchronized (cache) {
                    wrapperRef = cache.get(source);
                }
                if (wrapperRef != null) {
                    wrapper = wrapperRef.get();
                }
                if (wrapper == null) {
                    float[] latlons = reproject(source);
                    float[] lats = new float[latlons.length / 2];
                    float[] lons = new float[latlons.length / 2];

                    for (int i = 0; i < lats.length; i++) {
                        int index = i * 2;
                        lons[i] = latlons[index];
                        lats[i] = latlons[index + 1];
                    }

                    wrapper = new LatLonWrapper(lats, lons);
                    synchronized (cache) {
                        cache.put(source, new SoftReference<LatLonWrapper>(
                                wrapper));
                    }
                }
            }
        }
        return wrapper;
    }
}
