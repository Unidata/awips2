/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.grid.radar;

import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.tuple.ImmutableTriple;
import org.apache.commons.lang3.tuple.Triple;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.projection.ProjectionException;
import org.geotools.referencing.operation.transform.ConcatenatedTransform;
import org.geotools.api.referencing.FactoryException;
import org.geotools.api.referencing.operation.MathTransform;
import org.geotools.api.referencing.operation.MathTransformFactory;
import org.geotools.api.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.radar.projection.RadialBinMapProjection;
import com.raytheon.uf.common.dataplugin.radar.projection.RadialBinMapProjection.AzRanToRadialBinTransform;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.numeric.dest.DataDestination;

/**
 * Class for reprojecting radar data to grid. This class offers some performance
 * optimizations over the superclass for radar data.
 *
 * Specifically, this class tries to split the reprojection math transform into
 * two halves: a first transform that is common to different radar records and
 * whose results can be cached ({@link #cacheTransform}), and a second, faster
 * transform that cannot be cached ({@link #uniqueTransform}).
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2023 2031675    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarGridReprojection extends GridReprojection {

    private static final MathTransformFactory transformFactory = new DefaultMathTransformFactory();

    private static final Map<Triple<MathTransform, Integer, Integer>, SoftReference<double[]>> transformedPoints = new ConcurrentHashMap<>();

    private MathTransform cacheTransform;

    private MathTransform uniqueTransform;

    public RadarGridReprojection(GeneralGridGeometry sourceGeometry,
            GeneralGridGeometry targetGeometry) {
        super(sourceGeometry, targetGeometry);
    }

    @Override
    protected void initTransforms() throws FactoryException,
            InvalidGridGeometryException, TransformException {
        if (transform == null) {
            super.initTransforms();

            /*
             * Try to split the transform into two: one that is common to a lot
             * of radar records so that we can cache its results, and a second
             * one that is rarely shared by records and shouldn't be cached.
             */
            List<MathTransform> transforms = expandTransform(transform);
            for (int i = 0; i < transforms.size(); ++i) {
                /*
                 * AzRanToRadialBinTransform shouldn't be cached since it uses
                 * the specific angle values that the radar record has data for.
                 * Cache everything up to it.
                 */
                if (transforms.get(i) instanceof AzRanToRadialBinTransform) {
                    cacheTransform = concatenateTransforms(
                            new ArrayList<>(transforms.subList(0, i)));
                    uniqueTransform = concatenateTransforms(new ArrayList<>(
                            transforms.subList(i, transforms.size())));
                    break;
                }
            }
        }
    }

    @Override
    public <T extends DataDestination> T reprojectedGrid(GridSampler sampler,
            T dest) throws FactoryException, TransformException {
        initTransforms();

        if (cacheTransform == null || uniqueTransform == null) {
            /*
             * We weren't able to split the transform into two parts, just do
             * the standard reprojection.
             */
            return super.reprojectedGrid(sampler, dest);
        }

        /*
         * Do the first, cacheable reprojection portion if it's not cached
         * already
         */
        Triple<MathTransform, Integer, Integer> cacheKey = new ImmutableTriple<>(
                cacheTransform, targetNx, targetNy);
        SoftReference<double[]> pointsRef = transformedPoints.get(cacheKey);
        double[] points = pointsRef != null ? pointsRef.get() : null;
        if (points == null) {
            double[] srcPoint = new double[2];
            points = new double[targetNy * targetNx * 2];
            int destPointIndex = 0;
            for (int y = 0; y < targetNy; y++) {
                for (int x = 0; x < targetNx; x++) {
                    try {
                        srcPoint[0] = x;
                        srcPoint[1] = y;
                        cacheTransform.transform(srcPoint, 0, points,
                                destPointIndex, 1);
                    } catch (ProjectionException e) {
                        /*
                         * ProjectionException is thrown when a point is outside
                         * the valid range of the source data, so we will treat
                         * it like other out of range values and set the point
                         * to NaN. The unique transformation below will check
                         * for NaN points and set them to fill value.
                         */
                        points[destPointIndex] = Double.NaN;
                        points[destPointIndex + 1] = Double.NaN;
                    }

                    destPointIndex += 2;
                }
            }
            transformedPoints.put(cacheKey, new SoftReference<>(points));
        }

        // Do the non-cacheable reprojection and sample the data values
        int srcPointIndex = 0;
        double[] destPoint = new double[2];
        for (int y = 0; y < targetNy; y++) {
            for (int x = 0; x < targetNx; x++) {
                double value = Double.NaN;
                if (!Double.isNaN(points[srcPointIndex])
                        && !Double.isNaN(points[srcPointIndex + 1])) {
                    try {
                        uniqueTransform.transform(points, srcPointIndex,
                                destPoint, 0, 1);
                        value = sampler.sample(destPoint[0], destPoint[1]);
                    } catch (ProjectionException e) {
                        // Point is outside valid range, leave as NaN
                    }
                }
                dest.setDataValue(value, x, y);

                srcPointIndex += 2;
            }
        }

        return dest;
    }

    /**
     * Create a concatenated transform from the given list of transforms.
     *
     * @param transforms
     *            the list of transforms to concatenate (modified by this
     *            method)
     * @return the concatenated transform
     * @throws FactoryException
     */
    private static MathTransform concatenateTransforms(
            List<MathTransform> transforms) throws FactoryException {
        if (transforms.isEmpty()) {
            throw new IllegalArgumentException(
                    "Provided transforms list must not be empty");
        } else if (transforms.size() == 1) {
            return transforms.remove(0);
        } else {
            MathTransform transform1 = transforms.remove(0);
            return transformFactory.createConcatenatedTransform(transform1,
                    concatenateTransforms(transforms));
        }
    }

    /**
     * Expand the given transform into a list of transforms that make it up.
     *
     * @param transform
     *            the transform to expand
     * @return the expanded list of transforms
     */
    private static List<MathTransform> expandTransform(
            MathTransform transform) {
        if (transform instanceof ConcatenatedTransform) {
            List<MathTransform> transforms = new ArrayList<>();
            ConcatenatedTransform concatenatedTransform = (ConcatenatedTransform) transform;
            transforms
                    .addAll(expandTransform(concatenatedTransform.transform1));
            transforms
                    .addAll(expandTransform(concatenatedTransform.transform2));
            return Collections.unmodifiableList(transforms);
        } else if (transform instanceof RadialBinMapProjection) {
            /*
             * Split RadialBinMapProjection into 2 transforms so we can cache
             * the first half of it.
             */
            return ((RadialBinMapProjection) transform).splitTransform();
        } else {
            return List.of(transform);
        }
    }
}
