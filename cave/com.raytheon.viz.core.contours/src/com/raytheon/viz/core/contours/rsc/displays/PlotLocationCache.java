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
package com.raytheon.viz.core.contours.rsc.displays;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Map;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.transform.ConcatenatedTransform;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.NoninvertibleTransformException;
import org.opengis.referencing.operation.TransformException;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PlotLocationCache {

    private static class CacheKey {

        private final GridGeometry2D gridGeometry;

        private final GridGeometry2D descriptorGeometry;

        public CacheKey(GridGeometry2D gridGeometry,
                GridGeometry2D descriptorGeometry) {
            super();
            this.gridGeometry = gridGeometry;
            this.descriptorGeometry = descriptorGeometry;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime
                    * result
                    + ((descriptorGeometry == null) ? 0 : descriptorGeometry
                            .hashCode());
            result = prime * result
                    + ((gridGeometry == null) ? 0 : gridGeometry.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            CacheKey other = (CacheKey) obj;
            if (descriptorGeometry == null) {
                if (other.descriptorGeometry != null)
                    return false;
            } else if (!descriptorGeometry.equals(other.descriptorGeometry))
                return false;
            if (gridGeometry == null) {
                if (other.gridGeometry != null)
                    return false;
            } else if (!gridGeometry.equals(other.gridGeometry))
                return false;
            return true;
        }

    }

    private static PlotLocationCache instance;

    public static PlotLocationCache getInstance() {
        if (instance == null) {
            instance = new PlotLocationCache();
        }
        return instance;
    }

    private Map<CacheKey, Reference<float[]>> cache = new HashMap<CacheKey, Reference<float[]>>();

    private PlotLocationCache() {

    }

    public synchronized float[] getPlotLocations(GridGeometry2D gridGeometry,
            GridGeometry2D descriptorGeometry) {
        CacheKey key = new CacheKey(gridGeometry, descriptorGeometry);
        float[] result = null;
        Reference<float[]> resultRef = cache.get(key);
        if (resultRef != null) {
            result = resultRef.get();
        }
        if (result == null) {
            int xDim = gridGeometry.getGridRange().getSpan(0);
            int yDim = gridGeometry.getGridRange().getSpan(1);

            result = new float[xDim * yDim * 2];

            for (int i = 0; i < result.length; i += 2) {
                result[i] = (i / 2) / yDim;
                result[i + 1] = (i / 2) % yDim;
            }
            try {
                MathTransform grid2crs = gridGeometry.getGridToCRS();
                MathTransform crs2crs = CRS.findMathTransform(
                        gridGeometry.getCoordinateReferenceSystem(),
                        descriptorGeometry.getCoordinateReferenceSystem());
                MathTransform crs2grid = descriptorGeometry.getGridToCRS()
                        .inverse();
                MathTransform grid2grid = ConcatenatedTransform.create(
                        ConcatenatedTransform.create(grid2crs, crs2crs),
                        crs2grid);

                grid2grid.transform(result, 0, result, 0, xDim * yDim);
            } catch (FactoryException e) {
                throw new RuntimeException(e);
            } catch (InvalidGridGeometryException e) {
                throw new RuntimeException(e);
            } catch (NoninvertibleTransformException e) {
                throw new RuntimeException(e);
            } catch (TransformException e) {
                throw new RuntimeException(e);
            }
            cache.put(key, new SoftReference<float[]>(result));
        }
        return result;
    }
}
