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
package com.raytheon.uf.common.geospatial.interpolation;

import java.awt.geom.Point2D;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.collections.keyvalue.MultiKey;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.operation.projection.ProjectionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

/**
 * A GridReprojection which precomputes the coordinates of all the grid cells so
 * that multiple reprojects will be much faster. This implements a memory/time
 * tradeoff, using much more memory than an ordinary GridReprojection so that it
 * is able to reproject much faster. Because of the high memory usage all
 * instances are cached so they can be shared for identical reprojections.
 * 
 * The current caching implementation uses soft references. When the
 * reprojection is no longer referenced then the memory will be reclaimed by the
 * JVM as needed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2013 2185       bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PrecomputedGridReprojection extends GridReprojection {

    protected float[] transformTable;

    protected PrecomputedGridReprojection(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        super(sourceGeometry, targetGeometry);
    }

    /**
     * This function precomputes the math transform for all grid cells in the
     * target grid range. Precalculating this table takes time and uses more
     * memory but cuts the time to perform interpolation significantly.
     * 
     * @throws TransformException
     */
    protected void computeTransformTable() throws TransformException {
        try {
            initTransforms();
        } catch (FactoryException e) {
            throw new TransformException("Error preparing transform.", e);
        }
        float[] transformTable = new float[targetNy * targetNx * 2];
        int index = 0;
        for (int j = 0; j < targetNy; j++) {
            for (int i = 0; i < targetNx; i++) {
                transformTable[index++] = i;
                transformTable[index++] = j;
            }
        }
        try {
            transform.transform(transformTable, 0, transformTable, 0, targetNy
                    * targetNx);
        } catch (ProjectionException e) {
            ;// Ignore the points in the transformTable that are
             // invalid are set to NaN, no other action is necessary.
        }
        this.transformTable = transformTable;
    }

    @Override
    protected Point2D.Double getReprojectDataPoint(int x, int y)
            throws TransformException, FactoryException {
        if (x >= 0 && x < targetNx && y >= 0 && y < targetNy) {
            int index = (y * targetNx + x) * 2;
            float xVal = transformTable[index];
            float yVal = transformTable[index + 1];
            if (!Float.isNaN(xVal) && !Float.isNaN(yVal)) {
                return new Point2D.Double(xVal, yVal);
            }
        }
        return super.getReprojectDataPoint(x, y);
    }

    private static final Map<MultiKey, Reference<PrecomputedGridReprojection>> cache = new HashMap<MultiKey, Reference<PrecomputedGridReprojection>>();

    /**
     * Get a shared GridReprojection. This reprojection will have the transform
     * table computed.
     * 
     * @param sourceGeometry
     * @param targetGeometry
     * @return
     * @throws FactoryException
     * @throws TransformException
     */
    public static PrecomputedGridReprojection getReprojection(
            GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) throws TransformException {
        PrecomputedGridReprojection reprojection = null;
        boolean created = false;
        MultiKey key = new MultiKey(sourceGeometry, targetGeometry);
        synchronized (cache) {
            Reference<PrecomputedGridReprojection> ref = cache.get(key);
            if (ref != null) {
                reprojection = ref.get();
            }
            if (reprojection == null) {
                reprojection = new PrecomputedGridReprojection(sourceGeometry,
                        targetGeometry);
                created = true;
                cache.put(key, new SoftReference<PrecomputedGridReprojection>(
                        reprojection));
            }
        }
        synchronized (reprojection) {
            if (created) {
                reprojection.computeTransformTable();
            }
        }
        return reprojection;
    }

}
