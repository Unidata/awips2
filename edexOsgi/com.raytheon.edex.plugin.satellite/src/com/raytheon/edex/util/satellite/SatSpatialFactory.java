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
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.edex.plugin.satellite.dao.SatMapCoverageDao;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTReader;

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
 * 
 * </pre>
 */
public class SatSpatialFactory {

    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    /** The singleton instance */
    private static SatSpatialFactory instance;

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
    public synchronized SatMapCoverage getMapCoverage(
            Integer mapProjection, Integer nx, Integer ny, Float dx, Float dy,
            Float lov, Float latin, Float la1, Float lo1, Float la2, Float lo2)
            throws Exception {

        SatMapCoverage mapCoverage = null;
        SatMapCoverageDao satDao = new SatMapCoverageDao();

        try {
            // Check the database to see if a coverage already exists
            mapCoverage = satDao.getSatCoverage(mapProjection, nx, ny, dx, dy,
                    lov, latin, la1, lo1, la2, lo2);

            // If the database does not contain an existing sat map coverage for
            // the given values, create one
            if (mapCoverage == null) {
                mapCoverage = createMapCoverage(mapProjection, nx, ny, dx, dy,
                        lov, latin, la1, lo1, la2, lo2);
                // Persist the new coverage to the database
                satDao.persist(mapCoverage);
            }
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to retrieve or construct valid Satellite Map Coverage",
                    e);
        }

        if (mapCoverage == null) {
            throw new DataAccessLayerException(
                    "Unable to retrieve or construct valid Satellite Map Coverage");
        }

        return mapCoverage;
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
        if (mapProjection == SatMapCoverage.PROJ_MERCATOR) {
            double cm = 0.0;
            if ((lo1 > 0.0) && (lo2 < 0.0)) {
                cm = 180.0;
            }
            crs = MapUtil.constructMercator(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, latin, cm);
        } else if (mapProjection == 3) {
            crs = MapUtil.constructLambertConformal(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, latin, latin, lov);
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
        if (mapProjection == SatMapCoverage.PROJ_MERCATOR) {
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
            logger
                    .debug("Determining corner points for Lambert Conformal or Polar Stereographic projection");

            // Get the transforms to be used to convert between meters and
            // lat/lon
            MathTransform fromLatLon = MapUtil.getTransformFromLatLon(crs);
            MathTransform toLatLon = fromLatLon.inverse();

            // Use la1 and lo1 to specifyt the first point
            firstPosition = new DirectPosition2D();
            fromLatLon.transform(new DirectPosition2D(lo1, la1), firstPosition);

            // Determine the 3 other corner points using the given dx,dy,nx, and
            // ny in meters
            secondPosition = new DirectPosition2D(dx * nx + firstPosition.x,
                    firstPosition.y);
            thirdPosition = new DirectPosition2D(dx * nx + firstPosition.x, dy
                    * ny + firstPosition.y);
            fourthPosition = new DirectPosition2D(firstPosition.x, dx * ny
                    + firstPosition.y);

            // Convert the corner points from meters to lat/lon
            toLatLon.transform(firstPosition, corner1);
            toLatLon.transform(secondPosition, corner2);
            toLatLon.transform(thirdPosition, corner3);
            toLatLon.transform(fourthPosition, corner4);
        }

        // Construct the polygon constructor String
        StringBuffer buffer = new StringBuffer();
        buffer.append("POLYGON((");
        buffer.append(corner1.x + " " + corner1.y + ",");
        buffer.append(corner2.x + " " + corner2.y + ",");
        buffer.append(corner3.x + " " + corner3.y + ",");
        buffer.append(corner4.x + " " + corner4.y + ",");
        buffer.append(corner1.x + " " + corner1.y);
        buffer.append("))");

        // Create the geometry from the constructed String
        Geometry geometry = new WKTReader().read(buffer.toString());

        SatMapCoverage mapCoverage = new SatMapCoverage(mapProjection,
                nx, ny, dx, dy, lov, latin, la1, lo1, la2, lo2, crs, geometry);

        return mapCoverage;
    }

}
