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

import java.awt.image.RenderedImage;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

import javax.media.jai.Interpolation;
import javax.media.jai.InterpolationTable;
import javax.media.jai.PlanarImage;
import javax.media.jai.RenderedOp;

import org.apache.commons.collections.map.LRUMap;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.coverage.grid.ViewType;
import org.geotools.coverage.processing.Operations;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.referencing.crs.DefaultProjectedCRS;
import org.geotools.referencing.cs.DefaultCartesianCS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.DefiningConversion;
import org.opengis.coverage.grid.GridGeometry;
import org.opengis.geometry.DirectPosition;
import org.opengis.geometry.Envelope;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.NoSuchIdentifierException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.GeographicCRS;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.cs.CartesianCS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.NoninvertibleTransformException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.sun.medialib.mlib.Constants;
import com.sun.medialib.mlib.mediaLibImageInterpTable;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * MapUtil provides a convenience wrapper for some common map interactions such
 * as creating a grid coverage from an image, or from a data grid, and
 * reprojecting a coverage into another projection/coordinate system.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    05/16/2012   14993       D. Friedman Add oversampling option to
 *                                         reprojectGeometry.
 *    06/19/2012   14988       D. Friedman Make oversampling more like AWIPS 1
 *    09/18/2012   #1091       randerso    corrected getBoundingEnvelope
 *    11/06/2012   15406       ryu         Added convertToNativeEnvelope()
 * 
 * </pre>
 * 
 * @author chammack
 */
@SuppressWarnings("unchecked")
public class MapUtil {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapUtil.class);

    public static final double AWIPS_EARTH_RADIUS = 6371229.0;

    private static DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();

    public static GeographicCRS LATLON_PROJECTION;

    private static Map<Integer, MathTransform> fromLatLonMap = new LRUMap(250);

    private static Map<Integer, MathTransform> toLatLonMap = new LRUMap(250);

    private static Map<Integer, MathTransform> toLatLonCellMap = new LRUMap(250);

    private static Map<Integer, MathTransform> fromNativeMap = new LRUMap(250);

    private static Map<Integer, GridGeometry2D> gridGeometriesMap = new LRUMap(
            250);

    public static GeometryFactory jtsGeometryFactory = new GeometryFactory();

    static {
        try {
            LATLON_PROJECTION = (GeographicCRS) CRS.decode("EPSG:4326");
        } catch (FactoryException e) {
            // won't happen
        }
    }

    /**
     * AWIPS Lambert conformal projections use this CRS (211, 212, etc). Replace
     * the constructAWIPS211 with a WKT file implementation
     */
    public static ProjectedCRS AWIPS_LAMBERT_NORTHAMERICA = constructLambertConformal(
            AWIPS_EARTH_RADIUS, AWIPS_EARTH_RADIUS, 25, 25, -95);

    public static ProjectedCRS AWIPS_POLARSTEREO_ALASKA = constructNorthPolarStereo(
            AWIPS_EARTH_RADIUS, AWIPS_EARTH_RADIUS, 60, -150);

    public static ProjectedCRS AWIPS_POLARSTEREO_NORTHAMERICA = constructNorthPolarStereo(
            AWIPS_EARTH_RADIUS, AWIPS_EARTH_RADIUS, 60, -105);

    public static ProjectedCRS AWIPS_MERCATOR_HAWAII = constructMercator(
            AWIPS_EARTH_RADIUS, AWIPS_EARTH_RADIUS, 0, -160);

    public static ProjectedCRS AWIPS_MERCATOR_PACIFIC = constructMercator(
            AWIPS_EARTH_RADIUS, AWIPS_EARTH_RADIUS, 0, 150);

    /**
     * Construct an image-based grid coverage
     * 
     * @param name
     *            name of the projection
     * @param image
     *            the java-based image
     * @param crs
     *            the coordinate system/projection of the image
     * @param cornerPoints
     *            corner points of the image (2 or 4)
     * @return a grid coverage that represents the image
     * @throws Exception
     */
    public static GridCoverage2D constructGridCoverage(String name,
            RenderedImage image, CoordinateReferenceSystem crs,
            Point[] cornerPoints) throws Exception {

        GridCoverage2D baseGC = null;
        GridCoverageFactory factory = new GridCoverageFactory();

        MathTransform LLtoPROJ = CRS.findMathTransform(LATLON_PROJECTION, crs,
                true);

        GeneralEnvelope generalEnvelope = extractProjectedEnvelope(crs,
                cornerPoints, LLtoPROJ);

        baseGC = factory.create(name, image, generalEnvelope);

        return baseGC;
    }

    /**
     * Construct a data-based grid coverage
     * 
     * @param name
     *            name of the projection
     * @param grid
     *            the grid of floats in the image
     * @param crs
     *            the coordinate system/projection of the data
     * @param cornerPoints
     *            corner points of the data (2 or 4)
     * @return a grid coverage that represents the image
     * @throws Exception
     */
    public static GridCoverage2D constructGridCoverage(String name,
            float[][] grid, CoordinateReferenceSystem crs, Point[] cornerPoints)
            throws Exception {

        GridCoverage2D baseGC = null;
        GridCoverageFactory factory = new GridCoverageFactory();

        MathTransform LLtoPROJ = CRS.findMathTransform(LATLON_PROJECTION, crs,
                true);

        GeneralEnvelope generalEnvelope = extractProjectedEnvelope(crs,
                cornerPoints, LLtoPROJ);

        baseGC = factory.create(name, grid, generalEnvelope);

        return baseGC;
    }

    /**
     * Internal method
     * 
     * Construct a projected envelope. Directly creating an envelope with a
     * projection does not yield the expected results.
     * 
     * @param crs
     *            the coordinate reference system
     * @param cornerPoints
     *            the points of the image
     * @param LLtoPROJ
     *            the lat lon to projection transform
     * @return a projected envelope
     * @throws TransformException
     */
    public static GeneralEnvelope extractProjectedEnvelope(
            CoordinateReferenceSystem crs, Point[] cornerPoints,
            MathTransform LLtoPROJ) throws TransformException {
        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        generalEnvelope.setCoordinateReferenceSystem(crs);

        double minX = Double.POSITIVE_INFINITY;
        double maxX = Double.NEGATIVE_INFINITY;
        double minY = Double.POSITIVE_INFINITY;
        double maxY = Double.NEGATIVE_INFINITY;

        for (Point p : cornerPoints) {
            DirectPosition ll = new DirectPosition2D(LATLON_PROJECTION,
                    p.getX(), p.getY());
            DirectPosition translated = LLtoPROJ.transform(ll,
                    new DirectPosition2D());
            double x = translated.getOrdinate(0);
            double y = translated.getOrdinate(1);

            if (x < minX) {
                minX = x;
            }
            if (x > maxX) {
                maxX = x;
            }
            if (y < minY) {
                minY = y;
            }
            if (y > maxY) {
                maxY = y;
            }

        }
        generalEnvelope.setRange(0, minX, maxX);
        generalEnvelope.setRange(1, minY, maxY);

        return generalEnvelope;
    }

    /**
     * Construct a native envelope from the grid domain represented by the lower
     * left and the upper right corners.
     * 
     * @param ll
     *            lower left of the grid envelope
     * @param ur
     *            upper right of the grid envelope
     * @param gloc
     *            grid location object
     * @return a native envelope
     * 
     */
    public static GeneralEnvelope convertToNativeEnvelope(Coordinate ll,
            Coordinate ur, ISpatialObject gloc) {
        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        generalEnvelope.setCoordinateReferenceSystem(gloc.getCrs());

        double minX = Double.POSITIVE_INFINITY;
        double maxX = Double.NEGATIVE_INFINITY;
        double minY = Double.POSITIVE_INFINITY;
        double maxY = Double.NEGATIVE_INFINITY;

        for (Coordinate p : new Coordinate[] { ll, ur }) {
            Coordinate translated = gridCoordinateToNative(p,
                    PixelOrientation.CENTER, gloc);
            double x = translated.x;
            double y = translated.y;

            if (x < minX) {
                minX = x;
            }
            if (x > maxX) {
                maxX = x;
            }
            if (y < minY) {
                minY = y;
            }
            if (y > maxY) {
                maxY = y;
            }
        }

        generalEnvelope.setRange(0, minX, maxX);
        generalEnvelope.setRange(1, minY, maxY);

        return generalEnvelope;
    }

    /**
     * Reproject a grid coverage into a different coordinate reference system
     * 
     * @param srcCoverage
     *            the original grid coverage
     * @param targetCRS
     *            the target projection/coordinate system
     * @return a grid coverage in the new projection
     */
    public static GridCoverage2D reprojectCoverage(GridCoverage2D srcCoverage,
            CoordinateReferenceSystem targetCRS) {

        return reprojectCoverage(srcCoverage, targetCRS, null,
                Interpolation.getInstance(Interpolation.INTERP_NEAREST));
    }

    /**
     * Reproject a grid coverage into a different coordinate reference system
     * 
     * @param srcCoverage
     *            the original grid coverage
     * @param targetCRS
     *            the target projection/coordinate system
     * @param targetGeometry
     *            the target grid geometry
     * @param interpolation
     *            String indication desired interpolation type: "nearest",
     *            "bilinear"
     * @return a grid coverage in the new projection
     */
    public static GridCoverage2D reprojectCoverage(GridCoverage2D srcCoverage,
            CoordinateReferenceSystem targetCRS, GridGeometry targetGeometry,
            Interpolation interpolation) {

        GridCoverage2D projected = (GridCoverage2D) Operations.DEFAULT
                .resample(srcCoverage.view(ViewType.GEOPHYSICS), targetCRS,
                        targetGeometry, interpolation);

        return projected;
    }

    /**
     * Builds a new grid geometry that can be used to reproject sourceGeometry
     * into target envelope with about the same number of points.
     * 
     * @param sourceGeometry
     * @param targetEnvelope
     * @return the reprojected grid geometry
     * @throws FactoryException
     *             , TransformException
     */
    public static GeneralGridGeometry reprojectGeometry(
            GeneralGridGeometry sourceGeometry, Envelope targetEnvelope)
            throws FactoryException, TransformException {
        return reprojectGeometry(sourceGeometry, targetEnvelope, false);
    }

    /**
     * Builds a new grid geometry that can be used to reproject sourceGeometry
     * into target envelope with about the same number of points.
     * 
     * @param sourceGeometry
     * @param targetEnvelope
     * @param addBorder
     *            expand envelope to include a 1 grid cell border after
     *            reprojection
     * @return the reprojected grid geometry
     * @throws FactoryException
     *             , TransformException
     */
    public static GeneralGridGeometry reprojectGeometry(
            GeneralGridGeometry sourceGeometry, Envelope targetEnvelope,
            boolean addBorder) throws FactoryException, TransformException {
        return reprojectGeometry(sourceGeometry, targetEnvelope, addBorder, 1);
    }

    /**
     * Builds a new grid geometry that can be used to reproject sourceGeometry
     * into target envelope with about the same number of points (multiplied by
     * an oversample factor.)
     * 
     * @param sourceGeometry
     * @param targetEnvelope
     * @param addBorder
     *            expand envelope to include a 1 grid cell border after
     *            reprojection
     * @param oversampleFactor
     *            oversample factor for new grid
     * @return the reprojected grid geometry
     * @throws FactoryException
     *             , TransformException
     */
    public static GeneralGridGeometry reprojectGeometry(
            GeneralGridGeometry sourceGeometry, Envelope targetEnvelope,
            boolean addBorder, int oversampleFactor) throws FactoryException,
            TransformException {
        CoordinateReferenceSystem targetCRS = targetEnvelope
                .getCoordinateReferenceSystem();
        ReferencedEnvelope targetREnv = null;
        if (targetEnvelope instanceof ReferencedEnvelope) {
            targetREnv = (ReferencedEnvelope) targetEnvelope;
        } else {
            targetREnv = new ReferencedEnvelope(targetEnvelope);
        }

        // Gather variables for grib info
        CoordinateReferenceSystem sourceCRS = sourceGeometry
                .getCoordinateReferenceSystem();
        ReferencedEnvelope sourceEnv = new ReferencedEnvelope(
                sourceGeometry.getEnvelope());

        // Transform source and target envelopes to Lat/Lon Projection.
        ReferencedEnvelope newSourceEnv = sourceEnv.transform(
                LATLON_PROJECTION, false);
        ReferencedEnvelope newTargetREnv = targetREnv.transform(
                LATLON_PROJECTION, false);
        com.vividsolutions.jts.geom.Envelope intersection = newTargetREnv
                .intersection(newSourceEnv);
        // Its possible to get two envelopes that don't intersect in a common
        // space, for example one could have longitude from -200 to -160 and
        // another could have longitude from 160 to 200. Even though these are
        // the same range, they don't intersect. These two loops will shift the
        // data 360 degrees in the x direction until all intersections are
        // found.
        while (newSourceEnv.getMaxX() > newTargetREnv.getMinX()) {
            newSourceEnv.translate(-360, 0);
            intersection.expandToInclude(newTargetREnv
                    .intersection(newSourceEnv));
        }
        while (newSourceEnv.getMinX() < newTargetREnv.getMaxX()) {
            newSourceEnv.translate(360, 0);
            intersection.expandToInclude(newTargetREnv
                    .intersection(newSourceEnv));
        }
        // Get the newEnvelope
        ReferencedEnvelope newEnv = new ReferencedEnvelope(JTS.getEnvelope2D(
                intersection, LATLON_PROJECTION), LATLON_PROJECTION);

        newEnv = newEnv.transform(targetCRS, false, 500);
        // Calculate nx and ny, start with the number of original grid
        // points in the intersection and then adjust to the new aspect
        // ratio
        com.vividsolutions.jts.geom.Envelope intersectingEnv = JTS.transform(
                newEnv.transform(sourceCRS, false), sourceGeometry
                        .getGridToCRS().inverse());
        double aspectRatio = newEnv.getHeight() / newEnv.getWidth();
        double count = intersectingEnv.getWidth() * intersectingEnv.getHeight();
        int nx = (int) Math.sqrt(count / aspectRatio);
        int ny = (int) (nx * aspectRatio);

        if (oversampleFactor > 1) {
            int inCount = sourceGeometry.getGridRange().getSpan(0)
                    * sourceGeometry.getGridRange().getSpan(1);
            double outCount = inCount * newEnv.getArea() / sourceEnv.getArea();
            outCount *= 4;
            nx = (int) Math.sqrt(outCount / aspectRatio);
            ny = (int) (nx * aspectRatio);
        }

        if (addBorder) {
            newEnv.expandBy(newEnv.getWidth() / nx, newEnv.getHeight() / ny);
            nx += 2;
            ny += 2;
        }
        // Create the new Grid Geometry
        return new GridGeometry2D(new GeneralGridEnvelope(new int[] { 0, 0 },
                new int[] { nx, ny }), newEnv);
    }

    /**
     * Provides a transform to use to translate lat lons into any other
     * projection It is recommended that you call this method once, and then
     * cache the transform if possible.
     * 
     * <P>
     * After calling this method, use the transform methods to translate
     * coordinates
     * 
     * @param crs
     *            the coordinate reference system
     * @return the math transform
     * @throws FactoryException
     */
    public static MathTransform getTransformFromLatLon(
            CoordinateReferenceSystem crs) throws FactoryException {
        return CRS.findMathTransform(LATLON_PROJECTION, crs, true);
    }

    /**
     * Provides a transform to use to translate a projection coordinates into
     * lat lon It is recommended that you call this method once, and then cache
     * the transform if possible.
     * 
     * <P>
     * After calling this method, use the transform methods to translate
     * coordinates
     * 
     * @param crs
     *            the coordinate reference system
     * @return the math transform
     * @throws FactoryException
     */
    public static MathTransform getTransformToLatLon(
            CoordinateReferenceSystem crs) throws FactoryException {
        return CRS.findMathTransform(crs, LATLON_PROJECTION, true);
    }

    /**
     * @param name
     * @param parameters
     * @return the constructed projection
     * @throws FactoryException
     * @throws NoSuchIdentifierException
     * @throws NoSuchIdentifierException
     * @throws FactoryException
     */
    public static DefaultProjectedCRS constructProjection(String name,
            ParameterValueGroup parameters) throws NoSuchIdentifierException,
            FactoryException {
        DefaultProjectedCRS projCrs = null;

        Map<String, Object> props = new HashMap<String, Object>();
        props.put("name", name);

        DefiningConversion dc = new DefiningConversion(name, parameters);

        GeographicCRS base = DefaultGeographicCRS.WGS84;

        MathTransform mt = dmtFactory.createParameterizedTransform(parameters);

        CartesianCS cs = DefaultCartesianCS.PROJECTED;

        projCrs = new DefaultProjectedCRS(props, dc, base, mt, cs);
        return projCrs;
    }

    /**
     * Construct a north polar stereographic projection
     * 
     * @param majorAxis
     *            the major axis in meters
     * @param minorAxis
     *            the minor axis in meters
     * @param stdParallel
     *            the standard parallel
     * @param centralMeridian
     *            the central meridian
     * @return the constructed projection
     */
    public static ProjectedCRS constructNorthPolarStereo(double majorAxis,
            double minorAxis, double stdParallel, double centralMeridian) {
        try {
            ParameterValueGroup parameters = dmtFactory
                    .getDefaultParameters("Stereographic_North_Pole");

            parameters.parameter("semi_major").setValue(majorAxis);
            parameters.parameter("semi_minor").setValue(minorAxis);
            parameters.parameter("standard_parallel_1").setValue(stdParallel);
            parameters.parameter("central_meridian").setValue(centralMeridian);
            parameters.parameter("false_easting").setValue(0.0);
            parameters.parameter("false_northing").setValue(0.0);

            String name = "AWIPS Polar Stereographic (SP: " + stdParallel
                    + ", CM: " + centralMeridian + ")";

            return constructProjection(name, parameters);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            return null;
        }
    }

    public static GeographicCRS getLatLonProjection() {
        return LATLON_PROJECTION;
    }

    public static ProjectedCRS contructLambertAzimuthalEqualArea(
            double majorAxis, double minorAxis, double latOfOrigin,
            double centralMeridian, double falseEasting, double falseNorthing) {

        try {
            ParameterValueGroup parameters = dmtFactory
                    .getDefaultParameters("Lambert_Azimuthal_Equal_Area");

            parameters.parameter("semi_major").setValue(majorAxis);
            parameters.parameter("semi_minor").setValue(minorAxis);
            parameters.parameter("latitude_of_center").setValue(latOfOrigin);
            parameters.parameter("longitude_of_center").setValue(
                    centralMeridian);
            parameters.parameter("false_easting").setValue(falseEasting);
            parameters.parameter("false_northing").setValue(falseNorthing);

            String name = "Equidistant_Cylindrical (LO: " + latOfOrigin
                    + ", CM: " + centralMeridian + ")";

            return constructProjection(name, parameters);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            return null;
        }

    }

    /**
     * Construct a lambert conformal crs
     * 
     * @param majorAxis
     *            the major axis in meters
     * @param minorAxis
     *            the minor axis in meters
     * @param stdParallel1
     *            the standard parallel 1
     * @param stdParallel2
     *            the standard parallel 2
     * @param lonOfOrigin
     *            the longitude of origin
     * 
     * @return the constructed projection
     */
    public static ProjectedCRS constructLambertConformal(double majorAxis,
            double minorAxis, double stdParallel1, double stdParallel2,
            double lonOfOrigin) {
        try {
            ParameterValueGroup parameters = null;
            if (stdParallel1 == stdParallel2) {
                parameters = dmtFactory
                        .getDefaultParameters("Lambert_Conformal_Conic_1SP");

                parameters.parameter("semi_major").setValue(majorAxis);
                parameters.parameter("semi_minor").setValue(minorAxis);
                parameters.parameter("latitude_of_origin").setValue(
                        stdParallel1);
                parameters.parameter("longitude_of_origin").setValue(
                        lonOfOrigin);
                parameters.parameter("false_easting").setValue(0.0);
                parameters.parameter("false_northing").setValue(0.0);
            } else {
                parameters = dmtFactory
                        .getDefaultParameters("Lambert_Conformal_Conic_2SP");

                parameters.parameter("semi_major").setValue(majorAxis);
                parameters.parameter("semi_minor").setValue(minorAxis);
                parameters.parameter("latitude_of_origin").setValue(
                        stdParallel1);
                parameters.parameter("standard_parallel_1").setValue(
                        stdParallel1);
                parameters.parameter("standard_parallel_2").setValue(
                        stdParallel2);
                parameters.parameter("longitude_of_origin").setValue(
                        lonOfOrigin);
                parameters.parameter("false_easting").setValue(0.0);
                parameters.parameter("false_northing").setValue(0.0);
            }

            String name = "Lambert Conformal (SP: " + stdParallel1 + "/"
                    + stdParallel2 + ", Origin: " + lonOfOrigin + ")";

            return constructProjection(name, parameters);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            return null;
        }
    }

    /**
     * Creates a projected stereographic coordinate reference system for use
     * with radar.
     * 
     * @param majorAxis
     *            the major axis in meters
     * @param minorAxis
     *            the minor axis in meters
     * @param originLatitude
     *            latitude of origin
     * @param centralMeridian
     *            central meridian
     * 
     * @return A projected coordinate reference system
     */
    public static ProjectedCRS constructStereographic(double majorAxis,
            double minorAxis, double originLatitude, double centralMeridian) {
        try {
            ParameterValueGroup parameters = dmtFactory
                    .getDefaultParameters("Stereographic");
            parameters.parameter("semi_major").setValue(majorAxis);
            parameters.parameter("semi_minor").setValue(minorAxis);
            parameters.parameter("central_meridian").setValue(centralMeridian);
            parameters.parameter("latitude_of_origin").setValue(originLatitude);
            parameters.parameter("false_easting").setValue(0.0);
            parameters.parameter("false_northing").setValue(0.0);

            String name = "AWIPS Stereo Radar";

            return constructProjection(name, parameters);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            return null;
        }
    }

    /**
     * Construct a mercator projection
     * 
     * @param majorAxis
     *            the major axis in meters
     * @param minorAxis
     *            the minor axis in meters
     * @param stdParallel1
     *            the standard parallel 1
     * @param centralMeridian
     *            the central meridian
     * @return the constructed projection
     */
    public static ProjectedCRS constructMercator(double majorAxis,
            double minorAxis, double stdParallel1, double centralMeridian) {
        try {
            ParameterValueGroup parameters = dmtFactory
                    .getDefaultParameters("Mercator_2SP");

            parameters.parameter("semi_major").setValue(majorAxis);
            parameters.parameter("semi_minor").setValue(minorAxis);
            parameters.parameter("standard_parallel_1").setValue(stdParallel1);
            parameters.parameter("false_easting").setValue(0.0);
            parameters.parameter("false_northing").setValue(0.0);
            parameters.parameter("central_meridian").setValue(centralMeridian);

            String name = "Mercator (SP: " + stdParallel1 + ", CM: "
                    + centralMeridian + ")";

            return constructProjection(name, parameters);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            return null;
        }
    }

    public static ProjectedCRS constructEquidistantCylindrical(
            double majorAxis, double minorAxis, double centralMeridian,
            double latOfOrigin) {
        try {
            ParameterValueGroup parameters = dmtFactory
                    .getDefaultParameters("Equidistant_Cylindrical");

            parameters.parameter("semi_major").setValue(majorAxis);
            parameters.parameter("semi_minor").setValue(minorAxis);
            parameters.parameter("central_meridian").setValue(centralMeridian);
            parameters.parameter("latitude_of_origin").setValue(latOfOrigin);
            parameters.parameter("false_easting").setValue(0.0);
            parameters.parameter("false_northing").setValue(0.0);

            String name = "Equidistant_Cylindrical (LO: " + latOfOrigin
                    + ", CM: " + centralMeridian + ")";

            return constructProjection(name, parameters);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            return null;
        }
    }

    /**
     * Adjust longitude to be in the range +/- 180
     * 
     * @param lon
     *            the longitude
     * @return the corrected longitude
     */
    public static double correctLon(double lon) {

        while (lon > 180) {
            lon -= 360;
        }
        while (lon < -180) {
            lon += 360;
        }
        return lon;
    }

    /**
     * Adjust latitude to be in the range +/- 90
     * 
     * @param lat
     *            the latitude
     * @return the corrected latitude
     */
    public static double correctLat(double lat) {
        while (Math.abs(lat) > 90) {
            if (lat > 90) {
                lat = 180 - lat;
            }
            if (lat < -90) {
                lat = -180 - lat;
            }
        }
        return lat;
    }

    public synchronized static GridGeometry2D getGridGeometry(ISpatialObject obj) {
        GridGeometry2D mapGeom = null;
        Integer pk = getGridGeomHash(obj);
        synchronized (gridGeometriesMap) {
            mapGeom = gridGeometriesMap.get(pk);
        }

        if (mapGeom == null) {
            try {
                Point[] points = new Point[((Polygon) obj.getGeometry())
                        .getExteriorRing().getNumPoints()];

                for (int i = 0; i < points.length; i++) {
                    points[i] = ((Polygon) obj.getGeometry()).getExteriorRing()
                            .getPointN(i);
                }

                GeneralEnvelope env2 = MapUtil.extractProjectedEnvelope(
                        obj.getCrs(), points,
                        MapUtil.getTransformFromLatLon(obj.getCrs()));

                mapGeom = new GridGeometry2D(new GeneralGridEnvelope(new int[] {
                        0, 0 }, new int[] { obj.getNx(), obj.getNy() }, false),
                        env2);
                synchronized (gridGeometriesMap) {
                    gridGeometriesMap.put(pk, mapGeom);
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            }
        }
        return mapGeom;
    }

    public static MathTransform getTransformToNative(
            PixelOrientation orientation, ISpatialObject spatialObject) {
        return getGridGeometry(spatialObject).getGridToCRS(orientation);
    }

    public static MathTransform getTransformFromNative(
            PixelOrientation orientation, ISpatialObject spatialObject) {
        MathTransform fromNative = null;
        Integer pk = getSpatialObjectHash(spatialObject, orientation);
        synchronized (fromNativeMap) {
            fromNative = fromNativeMap.get(pk);
        }

        if (fromNative == null) {
            try {
                fromNative = MapUtil.getGridGeometry(spatialObject)
                        .getGridToCRS(orientation).inverse();

                synchronized (fromNativeMap) {
                    fromNativeMap.put(pk, fromNative);
                }
            } catch (NoninvertibleTransformException e) {
                statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            }
        }
        return fromNative;
    }

    public static MathTransform getTransformFromLatLon(
            PixelOrientation orientation, ISpatialObject spatialObject) {
        Integer pk = getSpatialObjectHash(spatialObject, orientation);
        MathTransform fromLatLon = null;
        synchronized (fromLatLonMap) {
            fromLatLon = fromLatLonMap.get(pk);
        }
        if (fromLatLon == null) {
            try {
                MathTransform latLonToProj = MapUtil
                        .getTransformFromLatLon(spatialObject.getCrs());
                MathTransform projToGrid = MapUtil
                        .getGridGeometry(spatialObject)
                        .getGridToCRS(orientation).inverse();
                DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                fromLatLon = dmtf.createConcatenatedTransform(latLonToProj,
                        projToGrid);

                synchronized (fromLatLonMap) {
                    fromLatLonMap.put(pk, fromLatLon);
                }
            } catch (FactoryException e) {
                statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            } catch (NoninvertibleTransformException e) {
                statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            }
        }
        return fromLatLon;
    }

    public static MathTransform getTransformToLatLon(
            PixelOrientation orientation, ISpatialObject spatialObject) {
        Integer pk = getSpatialObjectHash(spatialObject, orientation);
        MathTransform toLatLon = null;
        synchronized (toLatLonMap) {
            toLatLon = toLatLonMap.get(pk);
        }
        if (toLatLon == null) {
            try {
                MathTransform gridToProj = getGridGeometry(spatialObject)
                        .getGridToCRS(orientation);
                MathTransform projToLatLon = MapUtil
                        .getTransformToLatLon(spatialObject.getCrs());
                DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                toLatLon = dmtf.createConcatenatedTransform(gridToProj,
                        projToLatLon);
                synchronized (toLatLonMap) {
                    toLatLonMap.put(pk, toLatLon);
                }
            } catch (FactoryException e) {
                statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            }
        }
        return toLatLon;
    }

    public static MathTransform getTransformToLatLon(PixelInCell orientation,
            ISpatialObject spatialObject) {
        Integer pk = getSpatialObjectHash(spatialObject, orientation);
        MathTransform toLatLon = null;
        synchronized (toLatLonCellMap) {
            toLatLon = toLatLonCellMap.get(pk);
        }
        if (toLatLon == null) {
            try {
                MathTransform gridToProj = getGridGeometry(spatialObject)
                        .getGridToCRS(orientation);
                MathTransform projToLatLon = MapUtil
                        .getTransformToLatLon(spatialObject.getCrs());
                DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                toLatLon = dmtf.createConcatenatedTransform(gridToProj,
                        projToLatLon);
                synchronized (toLatLonCellMap) {
                    toLatLonCellMap.put(pk, toLatLon);
                }
            } catch (FactoryException e) {
                statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            }
        }
        return toLatLon;
    }

    public static void latLonToGridCoordinate(Coordinate[] coords,
            PixelOrientation orientation, ISpatialObject spatialObject) {

        transformCoordinates(
                getTransformFromLatLon(orientation, spatialObject), coords);
    }

    public static Coordinate nativeToGridCoordinate(
            Coordinate nativeCoordinate, PixelOrientation orientation,
            ISpatialObject spatialObject) {
        Coordinate gridCoord = new Coordinate(nativeCoordinate);

        transformCoordinates(
                getTransformFromNative(orientation, spatialObject), gridCoord);

        return gridCoord;
    }

    public static Coordinate latLonToGridCoordinate(Coordinate latLon,
            PixelOrientation orientation, ISpatialObject spatialObject) {

        Coordinate gridCoord = new Coordinate(latLon);

        transformCoordinates(
                getTransformFromLatLon(orientation, spatialObject), gridCoord);

        return gridCoord;
    }

    public static void gridCoordinateToLatLon(Coordinate[] coords,
            PixelOrientation orientation, ISpatialObject spatialObject) {

        transformCoordinates(getTransformToLatLon(orientation, spatialObject),
                coords);
    }

    /**
     * @param coords
     * @param mt
     */
    private static void transformCoordinates(MathTransform mt,
            Coordinate... coords) {
        double[] input = new double[coords.length * 2];
        int i = 0;
        for (Coordinate c : coords) {
            input[i++] = c.x;
            input[i++] = c.y;
        }

        double[] output = new double[input.length];
        try {
            mt.transform(input, 0, output, 0, input.length / 2);
        } catch (TransformException e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
        }

        i = 0;
        for (Coordinate c : coords) {
            c.x = output[i++];
            c.y = output[i++];
        }
    }

    public static Coordinate gridCoordinateToLatLon(Coordinate gridCoord,
            PixelOrientation orientation, ISpatialObject spatialObject) {

        Coordinate latLon = new Coordinate(gridCoord);

        transformCoordinates(getTransformToLatLon(orientation, spatialObject),
                latLon);

        return latLon;
    }

    public static Coordinate gridCoordinateToNative(Coordinate gridCoord,
            PixelOrientation orientation, ISpatialObject spatialObject) {

        Coordinate latLon = new Coordinate(gridCoord);

        transformCoordinates(getTransformToNative(orientation, spatialObject),
                latLon);

        return latLon;
    }

    public static void gridCoordinateToNative(Coordinate[] coords,
            PixelOrientation orientation, ISpatialObject spatialObject) {

        transformCoordinates(getTransformToNative(orientation, spatialObject),
                coords);
    }

    /**
     * Returns the rotation from true north (in degrees) of the specified point
     * (in latLon). A positive number indicates that north is to the right of
     * UP. A negative number indicates that north is to the left of UP. To
     * convert from UP to true NORTH, you will add the rotation value. The
     * rotation angle from UP indicates the direction of true NORTH.A
     * 
     * For example, if rotation = 10, then UP is 350 degrees and 10 degrees to
     * the right of UP is north (or 360) degrees.
     * 
     * @param latLon
     * @return rotation angle
     */
    public static double rotation(Coordinate latLon,
            ISpatialObject spatialObject) {

        double newLatLonY = latLon.y + 0.05;
        if (newLatLonY > 90) {
            newLatLonY -= 180;
        }
        if (newLatLonY < -90) {
            newLatLonY += 180;
        }

        Coordinate latLon2 = new Coordinate(latLon.x, newLatLonY);

        Coordinate a = latLonToGridCoordinate(latLon, PixelOrientation.CENTER,
                spatialObject);
        Coordinate a2 = latLonToGridCoordinate(latLon2,
                PixelOrientation.CENTER, spatialObject);

        double dx = a2.x - a.x;
        double dy = a2.y - a.y;
        return Math.toDegrees(Math.atan2(dx, dy));
    }

    /**
     * Returns the rotation from true north (in degrees) of the specified point
     * (in latLon). A positive number indicates that north is to the right of
     * UP. A negative number indicates that north is to the left of UP. To
     * convert from UP to true NORTH, you will add the rotation value. The
     * rotation angle from UP indicates the direction of true NORTH.A
     * 
     * For example, if rotation = 10, then UP is 350 degrees and 10 degrees to
     * the right of UP is north (or 360) degrees.
     * 
     * @param latLon
     * @return rotation angle
     */
    public static double rotation(Coordinate latLon, GridGeometry2D geometry) {

        double newLatLonY = latLon.y + 0.05;
        if (newLatLonY > 90) {
            newLatLonY -= 180;
        }
        if (newLatLonY < -90) {
            newLatLonY += 180;
        }

        Coordinate a = new Coordinate(latLon);
        Coordinate a2 = new Coordinate(latLon.x, newLatLonY);

        try {
            transformCoordinates(
                    getTransformFromLatLon(geometry
                            .getCoordinateReferenceSystem()),
                    a, a2);
            transformCoordinates(geometry.getGridToCRS().inverse(), a, a2);
        } catch (InvalidGridGeometryException e) {
            throw new RuntimeException(e);
        } catch (FactoryException e) {
            throw new RuntimeException(e);
        } catch (NoninvertibleTransformException e) {
            throw new RuntimeException(e);
        }

        double dx = a2.x - a.x;
        double dy = a2.y - a.y;
        return Math.toDegrees(Math.atan2(dx, dy));
    }

    /**
     * Get the bounding envelope of a spatialObject
     * 
     * @param spatialObject
     * @return the bounding envelope of the spatialObject in lat/lon
     * @throws TransformException
     * @throws FactoryException
     */
    public static ReferencedEnvelope getBoundingEnvelope(
            ISpatialObject spatialObject) throws TransformException,
            FactoryException {

        int nx = spatialObject.getNx();
        int ny = spatialObject.getNy();

        Coordinate[] coords = new Coordinate[] { new Coordinate(0, ny - 1),
                new Coordinate(nx, -1) };
        gridCoordinateToNative(coords, PixelOrientation.LOWER_LEFT,
                spatialObject);

        ReferencedEnvelope env = new ReferencedEnvelope(coords[0].x,
                coords[1].x, coords[0].y, coords[1].y, spatialObject.getCrs());

        ReferencedEnvelope boundingEnv;
        boundingEnv = env.transform(MapUtil.LATLON_PROJECTION, true);

        return boundingEnv;
    }

    public static Geometry getBoundingGeometry(ISpatialObject spatialObject) {
        int nx = spatialObject.getNx();
        int ny = spatialObject.getNy();
        Coordinate[] coordinates = new Coordinate[2 * (nx + ny) + 1];
        int i = 0;
        for (int x = 0; x <= nx; x++) {
            coordinates[i++] = new Coordinate(x, -1);
        }
        for (int y = 0; y < ny; y++) {
            coordinates[i++] = new Coordinate(nx, y);
        }
        for (int x = nx - 1; x > 0; x--) {
            coordinates[i++] = new Coordinate(x, ny - 1);
        }
        for (int y = ny - 1; y >= 0; y--) {
            coordinates[i++] = new Coordinate(0, y);
        }
        coordinates[i++] = coordinates[0];

        LinearRing shell = jtsGeometryFactory.createLinearRing(coordinates);
        Geometry g = jtsGeometryFactory.createPolygon(shell, null);

        try {
            g = JTS.transform(
                    g,
                    getTransformToLatLon(PixelOrientation.LOWER_LEFT,
                            spatialObject));
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error computing bounding geometry", e);
        }

        return g;
    }

    public static Point getPoint(double latitude, double longitude) {
        return jtsGeometryFactory.createPoint(new Coordinate(MapUtil
                .correctLon(longitude), MapUtil.correctLat(latitude)));
    }

    public static Coordinate getCoordinate(double longitude, double latitude) {
        return new Coordinate(MapUtil.correctLon(longitude),
                MapUtil.correctLat(latitude));
    }

    public static Polygon getPolygon(Coordinate[] coordinates) {
        LinearRing ring = jtsGeometryFactory.createLinearRing(coordinates);
        return jtsGeometryFactory.createPolygon(ring, new LinearRing[] {});
    }

    private static int getSpatialObjectHash(ISpatialObject spatialObject,
            PixelInCell orientation) {
        HashCodeBuilder hashBuilder = new HashCodeBuilder();
        hashBuilder.append(spatialObject.getNx());
        hashBuilder.append(spatialObject.getNy());
        hashBuilder.append(spatialObject.getGeometry());
        hashBuilder.append(spatialObject.getCrs());
        hashBuilder.append(orientation);
        return hashBuilder.toHashCode();
    }

    private static int getSpatialObjectHash(ISpatialObject spatialObject,
            PixelOrientation orientation) {
        HashCodeBuilder hashBuilder = new HashCodeBuilder();
        hashBuilder.append(spatialObject.getNx());
        hashBuilder.append(spatialObject.getNy());
        hashBuilder.append(spatialObject.getGeometry());
        hashBuilder.append(spatialObject.getCrs());
        hashBuilder.append(orientation);
        return hashBuilder.toHashCode();
    }

    private static int getGridGeomHash(ISpatialObject spatialObject) {
        HashCodeBuilder hashBuilder = new HashCodeBuilder();
        hashBuilder.append(spatialObject.getNx());
        hashBuilder.append(spatialObject.getNy());
        hashBuilder.append(spatialObject.getGeometry());
        hashBuilder.append(spatialObject.getCrs());
        return hashBuilder.toHashCode();
    }

    public static Polygon createGeometry(
            com.vividsolutions.jts.geom.Envelope env) {

        return createGeometry(env.getMinY(), env.getMinX(), env.getMaxY(),
                env.getMaxX());
    }

    public static Polygon createGeometry(Double la1, Double lo1, Double la2,
            Double lo2) {
        double[] corners = new double[5 * 2];
        corners[0] = lo1;
        corners[1] = la1;

        corners[2] = lo2;
        corners[3] = la1;

        corners[4] = lo2;
        corners[5] = la2;

        corners[6] = lo1;
        corners[7] = la2;

        corners[8] = lo1;
        corners[9] = la1;

        return createPolygon(corners);
    }

    public static Polygon createGeometry(CoordinateReferenceSystem crs,
            Double la1, Double lo1, Double dx, Double dy, Integer nx, Integer ny)
            throws Exception {

        double[] d = new double[5 * 2];

        // Get the transforms to be used to convert between meters and
        // lat/lon
        MathTransform fromLatLon = MapUtil.getTransformFromLatLon(crs);
        MathTransform toLatLon = fromLatLon.inverse();

        // Use la1 and lo1 to specify the first point
        d[0] = lo1;
        d[1] = la1;
        fromLatLon.transform(d, 0, d, 0, 1);

        // move firstPosition from cell center to cell corner
        d[0] -= 0.5 * dx;
        d[1] -= 0.5 * dy;

        // Determine the 3 other corner points using the given dx,dy,nx, and
        // ny in meters
        d[2] = d[0] + dx * nx;
        d[3] = d[1];
        d[4] = d[0] + dx * nx;
        d[5] = d[1] + dy * ny;
        d[6] = d[0];
        d[7] = d[1] + dy * ny;

        // fifth point to close the polygon
        d[8] = d[0];
        d[9] = d[1];

        // Convert the corner points from meters to lat/lon
        toLatLon.transform(d, 0, d, 0, 5);

        // Create the geometry from the constructed String
        return createPolygon(d);
    }

    private static Polygon createPolygon(double[] points) {
        GeometryFactory gf = new GeometryFactory();

        Coordinate[] coordinates = new Coordinate[points.length / 2];
        for (int i = 0; i < coordinates.length; i++) {
            coordinates[i] = new Coordinate(points[i * 2], points[i * 2 + 1]);
        }
        LinearRing shell = gf.createLinearRing(coordinates);
        Polygon polygon = gf.createPolygon(shell, null);

        return polygon;
    }

    /**
     * Find the intersection between source envelope and target envelope in the
     * target envelope CRS. The simplified idea of what happens is that
     * sourceEnvelope is projected to the targetEnvelope CRS and a simple
     * rectangle intersection is performed. There is quite a bit of special
     * handling code for projections that do not line up well and therefore do
     * not work using the simple case but the idea is conceptually the same.
     * 
     * @param sourceEnvelope
     * @param targetEnvelope
     * @return
     * @throws TransformException
     */
    public static ReferencedEnvelope reprojectAndIntersect(
            Envelope sourceEnvelope, Envelope targetEnvelope)
            throws TransformException {
        try {
            // Use referenced envelope to go from source crs into target crs.
            ReferencedEnvelope sourceRefEnvelope = new ReferencedEnvelope(
                    sourceEnvelope);
            ReferencedEnvelope targetRefEnvelope = new ReferencedEnvelope(
                    targetEnvelope);
            ReferencedEnvelope sourceInTargetCRS = null;
            try {
                sourceInTargetCRS = sourceRefEnvelope.transform(
                        targetEnvelope.getCoordinateReferenceSystem(), true,
                        200);
            } catch (TransformException e) {
                // If the corners of the source envelope are invalid in the
                // target crs then the referenced envelope fails so this is the
                // backup plan. This is known to hit when projecting ab
                // Equidistant cyclindrical envelope that extends to the south
                // pole onto a north polar stereographic CRS.

                // transform target to source space.
                ReferencedEnvelope targetInSourceCRS = targetRefEnvelope
                        .transform(
                                sourceEnvelope.getCoordinateReferenceSystem(),
                                true, 200);
                // intersect the transformed envelope with the source envelope
                // Hopefully this intersection will eliminate the
                // invalid points in the original source envelope.
                sourceInTargetCRS = new ReferencedEnvelope(
                        sourceRefEnvelope.intersection(targetInSourceCRS),
                        sourceRefEnvelope.getCoordinateReferenceSystem());
                // transform the intersection back to target space, now it is
                // hopefully a subgrid.
                sourceInTargetCRS = sourceInTargetCRS.transform(
                        targetEnvelope.getCoordinateReferenceSystem(), true,
                        200);

            }
            // with both enenvelopes in target space, perform a simple
            // intersection.
            ReferencedEnvelope result = new ReferencedEnvelope(
                    sourceInTargetCRS.intersection(targetRefEnvelope),
                    targetEnvelope.getCoordinateReferenceSystem());

            // Attempt to add extra space to the interesection near the border
            // of the source envelope. This is similar to what
            // ReferencedEnvelope does when it uses CRS.transform where it
            // checks the extrema of each axis. For the projected CRSs that we
            // use we don't set an axis axtrema so those checks don't accomplish
            // anything. Checking the target envelope extrema
            // accomplishes the same thing. This is known to expand the envelope
            // when a polar stereographic source is projected onto a worldwide
            // equidistant cylindrical target.
            MathTransform mt = CRS.findMathTransform(
                    targetEnvelope.getCoordinateReferenceSystem(),
                    sourceEnvelope.getCoordinateReferenceSystem());
            double[] xTestPoints = { targetEnvelope.getMinimum(0),
                    targetEnvelope.getMedian(0), targetEnvelope.getMaximum(0) };
            double[] yTestPoints = { targetEnvelope.getMinimum(1),
                    targetEnvelope.getMedian(1), targetEnvelope.getMaximum(1) };

            for (double xTestPoint : xTestPoints) {
                for (double yTestPoint : yTestPoints) {
                    DirectPosition2D edge = new DirectPosition2D(xTestPoint,
                            yTestPoint);
                    if (!result.contains(edge)) {
                        try {
                            DirectPosition2D tmp = new DirectPosition2D();
                            mt.transform(edge, tmp);
                            if (sourceRefEnvelope.contains(tmp)) {
                                result.expandToInclude(edge.x, edge.y);
                            }
                        } catch (Exception ex) {
                            ;// ignore, can't expand the envelope.
                        }
                    }
                }
            }

            return result;
        } catch (FactoryException e) {
            throw new TransformException(
                    "Error performing envelope transformation", e);
        }
    }

    /**
     * Work around until MlibWarpPolynomialTableOpImage is fixed in jai-core.
     * jai-core defect number 144 REMOVE THIS CODE when bug is fixed
     * 
     * @param renderedImage
     *            renderedImage
     * @throws Exception
     */
    public static void jaiMlibWarpPolynomialTableOpImageWorkAround(
            RenderedImage renderedImage) throws Exception {
        if (renderedImage instanceof RenderedOp) {
            Field theImageField = RenderedOp.class.getDeclaredField("theImage");
            theImageField.setAccessible(true);
            PlanarImage theImage = (PlanarImage) theImageField
                    .get(renderedImage);

            String name = theImage.getClass().getSimpleName();
            if ("MlibWarpPolynomialTableOpImage".equals(name)) {
                Field mlibInterpTableDField = theImage.getClass()
                        .getDeclaredField("mlibInterpTableD");
                mlibInterpTableDField.setAccessible(true);
                mediaLibImageInterpTable mlibInterpTableD = (mediaLibImageInterpTable) mlibInterpTableDField
                        .get(theImage);

                Field interpField = theImage.getClass().getSuperclass()
                        .getSuperclass().getDeclaredField("interp");
                interpField.setAccessible(true);
                InterpolationTable interp = (InterpolationTable) interpField
                        .get(theImage);

                InterpolationTable jtable = interp;
                mlibInterpTableD = new mediaLibImageInterpTable(
                        Constants.MLIB_FLOAT, jtable.getWidth(),
                        jtable.getHeight(), jtable.getLeftPadding(),
                        jtable.getTopPadding(), jtable.getSubsampleBitsH(),
                        jtable.getSubsampleBitsV(), jtable.getPrecisionBits(),
                        jtable.getHorizontalTableDataFloat(),
                        jtable.getVerticalTableDataFloat());
                mlibInterpTableDField.set(theImage, mlibInterpTableD);

            }
        }

    }

    public static GridGeometry2D createFineIntersectingGeometry(
            org.opengis.geometry.Envelope sourceEnvelope,
            org.opengis.geometry.Envelope targetEnvelope, long[] sizes)
            throws Exception {
        ReferencedEnvelope targetREnv = new ReferencedEnvelope(targetEnvelope);
        CoordinateReferenceSystem targetCRS = targetREnv
                .getCoordinateReferenceSystem();
        ReferencedEnvelope sourceEnv = new ReferencedEnvelope(sourceEnvelope);

        ReferencedEnvelope newEnv = new ReferencedEnvelope(JTS.getEnvelope2D(
                targetREnv.intersection(sourceEnv.transform(targetCRS, false)),
                targetCRS), targetCRS);
        GridGeometry2D newTarget = new GridGeometry2D(new GeneralGridEnvelope(
                new int[] { 0, 0 }, new int[] { (int) (sizes[0]),
                        (int) (sizes[1]) }), newEnv);
        return newTarget;
    }

    public static ReferencedEnvelope getBoundingEnvelope(
            CoordinateReferenceSystem crs, Polygon crs84Shape) {
        try {
            // the polygons aren't truly projected, they are just the projected
            // points
            Coordinate[] coords = crs84Shape.getExteriorRing().getCoordinates();
            MathTransform mt = CRS.findMathTransform(MapUtil.LATLON_PROJECTION,
                    crs);
            // here we project the points back to the native crs

            double maxX = Double.NEGATIVE_INFINITY;
            double maxY = Double.NEGATIVE_INFINITY;
            double minX = Double.POSITIVE_INFINITY;
            double minY = Double.POSITIVE_INFINITY;
            for (int i = 0; i < coords.length; ++i) {
                DirectPosition2D to = new DirectPosition2D();
                mt.transform(new DirectPosition2D(coords[i].x, coords[i].y), to);
                maxX = Math.max(maxX, to.x);
                maxY = Math.max(maxY, to.y);
                minX = Math.min(minX, to.x);
                minY = Math.min(minY, to.y);
            }

            return new ReferencedEnvelope(minX, maxX, minY, maxY, crs);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
            // FIXME actually handle error
            throw new RuntimeException(e);
        }
    }

    public static ReferencedEnvelope getNativeEnvelope(ISpatialObject spatial)
            throws FactoryException {
        CoordinateReferenceSystem crs = spatial.getCrs();
        Geometry geom = spatial.getGeometry();
        return MapUtil.getBoundingEnvelope(crs, (Polygon) geom);
    }
}
