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

package com.raytheon.edex.util.satellite;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.jts.JTS;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.edex.plugin.satellite.dao.SatMapCoverageDao;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 
 * Retrieves or creates a SatMapCoverage object
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/19/07     439         bphillip    Initial creation
 * - AWIPS2 Baseline Repository --------
 * 07/12/2012    798        jkorman     Changed projection "magic" numbers
 * 09/30/2013   2333        mschenke    Refactored to store points in crs space
 * </pre>
 */
public class SatSpatialFactory {

    // TODO: These constants should be in the GINI decoder since they are only
    // related to the GINI format and should not be stored in SatMapCoverage.
    // Can't do this now as ncep has code that checks projection number to
    // determine how code should flow.
    public static final int PROJ_MERCATOR = 1;

    public static final int PROJ_LAMBERT = 3;

    public static final int PROJ_POLAR = 5;

    public static final int PROJ_CYLIN_EQUIDISTANT = 7;

    public static final int UNDEFINED = -1;

    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    /** The singleton instance */
    private static SatSpatialFactory instance;

    private SatMapCoverageDao satDao = new SatMapCoverageDao();

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public static synchronized SatSpatialFactory getInstance() {
        if (instance == null) {
            instance = new SatSpatialFactory();
        }
        return instance;
    }

    /**
     * Retrieves or generates a satellite map coverage object
     * 
     * @param mapProjection
     *            The projection
     * @param nx
     *            The number of columns
     * @param ny
     *            The number of rows
     * @param dx
     *            The distance between x points
     * @param dy
     *            The distance between y points
     * @param lov
     *            The orientation of the grid
     * @param latin
     *            The latitude at which the Lambert projection cone is tangent
     *            to the earth
     * @param la1
     *            Latitude of first point
     * @param lo1
     *            Longitude of first point
     * @param la2
     *            Latitude of last point
     * @param lo2
     *            Longitude of last point
     * @return A SatMapCoverage object with the given values
     * @throws Exception
     *             If errors occur during db interaction or creation of the
     *             coverage object
     */
    public synchronized SatMapCoverage getMapCoverage(Integer mapProjection,
            Integer nx, Integer ny, Float dx, Float dy, Float lov, Float latin,
            Float la1, Float lo1, Float la2, Float lo2) throws Exception {
        try {
            SatMapCoverage mapCoverage = createMapCoverage(mapProjection, nx,
                    ny, dx, dy, lov, latin, la1, lo1, la2, lo2);
            SatMapCoverage persisted = satDao
                    .queryByMapId(mapCoverage.getGid());
            if (persisted == null) {
                persisted = mapCoverage;
                satDao.persist(persisted);
            }
            return persisted;
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to retrieve or construct valid Satellite Map Coverage",
                    e);
        }
    }

    /**
     * Creates a new SatMapCoverage object from scratch with the given
     * parameters
     * 
     * @param mapProjection
     *            The projection
     * @param nx
     *            The number of columns
     * @param ny
     *            The number of rows
     * @param dx
     *            The distance between x points
     * @param dy
     *            The distance between y points
     * @param lov
     *            The orientation of the grid
     * @param latin
     *            The latitude at which the Lambert projection cone is tangent
     *            to the earth
     * @param la1
     *            Latitude of first point
     * @param lo1
     *            Longitude of first point
     * @param la2
     *            Latitude of last point
     * @param lo2
     *            Longitude of last point
     * @return A SatMapCoverage object with the given values
     * @throws Exception
     *             If errors occur during generation of the coverage object
     */
    private synchronized SatMapCoverage createMapCoverage(
            Integer mapProjection, Integer nx, Integer ny, Float dx, Float dy,
            Float lov, Float latin, Float la1, Float lo1, Float la2, Float lo2)
            throws Exception {

        logger.debug("Creating map coverage object");

        ProjectedCRS crs = null;
        // Get the correct CRS
        if (mapProjection == PROJ_MERCATOR) {
            double cm = 0.0;
            if ((lo1 > 0.0) && (lo2 < 0.0)) {
                cm = 180.0;
            }
            crs = MapUtil.constructMercator(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, latin, cm);
        } else if (mapProjection == PROJ_LAMBERT) {
            crs = MapUtil.constructLambertConformal(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, latin, latin, lov);
        } else if (mapProjection == SatSpatialFactory.PROJ_CYLIN_EQUIDISTANT) {
            crs = MapUtil.constructEquidistantCylindrical(
                    MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                    lov, latin);
        } else {
            crs = MapUtil.constructNorthPolarStereo(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, 60, lov);
        }

        DirectPosition2D firstPosition = null;
        DirectPosition2D secondPosition = null;
        DirectPosition2D thirdPosition = null;
        DirectPosition2D fourthPosition = null;
        DirectPosition2D corner1 = new DirectPosition2D();
        DirectPosition2D corner2 = new DirectPosition2D();
        DirectPosition2D corner3 = new DirectPosition2D();
        DirectPosition2D corner4 = new DirectPosition2D();

        /*
         * Projection is Mercator. Determine corner points from la1,lo1,la2,lo2
         * provided in the satellite file
         */
        if (mapProjection == PROJ_MERCATOR) {
            logger.debug("Determining corner points for Mercator projection");
            corner1.x = lo1;
            corner1.y = la1;

            corner3.x = lo2;
            corner3.y = la2;

            corner2.x = lo2;
            corner2.y = la1;

            corner4.x = lo1;
            corner4.y = la2;
        }
        /*
         * Projection is Lambert Conformal or Polar Stereographic. Therefore,
         * the corner points must be calculated
         */
        else {
            logger.debug("Determining corner points for Lambert Conformal or Polar Stereographic projection");

            // Get the transforms to be used to convert between meters and
            // lat/lon
            MathTransform fromLatLon = MapUtil.getTransformFromLatLon(crs);
            MathTransform toLatLon = fromLatLon.inverse();

            // Use la1 and lo1 to specifyt the first point
            firstPosition = new DirectPosition2D();
            fromLatLon.transform(new DirectPosition2D(lo1, la1), firstPosition);

            // Determine the 3 other corner points using the given dx,dy,nx, and
            // ny in meters
            secondPosition = new DirectPosition2D(firstPosition.x + (dx * nx),
                    firstPosition.y);
            thirdPosition = new DirectPosition2D(secondPosition.x,
                    firstPosition.y + (dy * ny));
            fourthPosition = new DirectPosition2D(firstPosition.x,
                    thirdPosition.y);

            // Convert the corner points from meters to lat/lon
            toLatLon.transform(firstPosition, corner1);
            toLatLon.transform(secondPosition, corner2);
            toLatLon.transform(thirdPosition, corner3);
            toLatLon.transform(fourthPosition, corner4);
        }

        double[] c = corner1.getCoordinate();
        Coordinate c1 = new Coordinate(c[0], c[1]);
        c = corner2.getCoordinate();
        Coordinate c2 = new Coordinate(c[0], c[1]);
        c = corner3.getCoordinate();
        Coordinate c3 = new Coordinate(c[0], c[1]);
        c = corner4.getCoordinate();
        Coordinate c4 = new Coordinate(c[0], c[1]);

        // Go from lat/lon to crs space to get minX,minY in crs space
        GeometryFactory gf = new GeometryFactory();
        Polygon polygon = gf.createPolygon(
                gf.createLinearRing(new Coordinate[] { c1, c2, c3, c4, c1 }),
                null);
        MathTransform fromLatLon = MapUtil.getTransformFromLatLon(crs);

        polygon = (Polygon) JTS.transform(polygon, fromLatLon);
        Envelope env = polygon.getEnvelopeInternal();
        if (mapProjection == PROJ_MERCATOR) {
            // Calculate dx/dy in mercator crs space
            dx = (float) (env.getWidth() / nx);
            dy = (float) (env.getHeight() / ny);
        }
        return new SatMapCoverage(mapProjection, env.getMinX(), env.getMinY(),
                nx, ny, dx, dy, crs);
    }

}
