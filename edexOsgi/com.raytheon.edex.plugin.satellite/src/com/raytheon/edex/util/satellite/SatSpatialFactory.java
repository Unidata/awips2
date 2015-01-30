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

import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.edex.plugin.satellite.SatelliteDecoderException;
import com.raytheon.edex.plugin.satellite.dao.SatMapCoverageDao;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * Retrieves or creates a SatMapCoverage object
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 19, 2007  439      bphillip    Initial creation
 * Jul 12, 2012  798      jkorman     Changed projection "magic" numbers
 * Sep 30, 2013  2333     mschenke    Refactored to store points in crs space
 * Apr 15, 2014  3017     bsteffen    Add new getCoverage methods to support
 *                                    either one corner + dx/dy or two corners.
 * Jun 05, 2014  3243     bsteffen    Remove deprecated lambert conformal call.
 * Sep 15, 2014  17303    jgerth      Support for second standard latitude
 * Nov 05, 2014  2714     bclement    replaced DecoderException with SatelliteDecoderException
 * Nov 05, 2014  3788     bsteffen    use getOrCreateCoverage in place of queryByMapId
 * 
 * 
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
     * @deprecated use either
     *             {@link #getCoverageSingleCorner(int, int, int, double, double, double, double, double, double)}
     *             or
     *             {@link #getCoverageTwoCorners(int, int, int, double, double, double, double, double, double)}
     *             depending on which parameters are considered more accurate.
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
    @Deprecated
    public synchronized SatMapCoverage getMapCoverage(Integer mapProjection,
            Integer nx, Integer ny, Float dx, Float dy, Float lov, Float latin,
            Float la1, Float lo1, Float la2, Float lo2) throws Exception {
        if (mapProjection == PROJ_MERCATOR) {
            return getCoverageTwoCorners(mapProjection, nx, ny, lov, latin,
                    la1, lo1, la2, lo2);
        } else {
            return getCoverageSingleCorner(mapProjection, nx, ny, lov, latin,
                    la1, lo1, dx, dy);
        }
    }

    /**
     * Create a {@link SatMapCoverage} with an area defined by only one corner
     * and using dx/dy and nx/by to derive the rest of the area. If dx and dy
     * are positive than la1 and lo1 are the upper left corner.
     * 
     * @param crsType
     *            the type of CRS, must be one of
     *            {@link #PROJ_CYLIN_EQUIDISTANT}, {@link #PROJ_LAMBERT},
     *            {@link #PROJ_MERCATOR}, {@link #PROJ_POLAR}.
     * @param nx
     *            the number of columns of data.
     * @param ny
     *            the number of rows of data.
     * @param lov
     *            the longitude orientation, used by
     *            {@link #PROJ_CYLIN_EQUIDISTANT}, {@link #PROJ_LAMBERT},
     *            {@link #PROJ_POLAR}.
     * @param latin
     *            the latitude at which the projection is tangent to the earths
     *            surface, used by {@link #PROJ_CYLIN_EQUIDISTANT},
     *            {@link #PROJ_LAMBERT}, {@link #PROJ_MERCATOR}.
     * @param latin2
     *            the second standard latitude, used by {@link #PROJ_LAMBERT}.
     * @param la1
     *            the latitude of a corner of the grid, if dy is positive this
     *            is an upper corner.
     * @param lo1
     *            the longitude of a corner of the grid, if dx is positive this
     *            is a left corner
     * @param dx
     *            the distance between columns measured in CRS meters.
     * @param dy
     *            the distance between rows measured in CRS meters.
     * @return a {@link SatMapCoverage} matching these parameters that has been
     *         loaded from or persisted to the database.
     * @throws DecoderException
     */
    public SatMapCoverage getCoverageSingleCorner(int crsType, int nx, int ny,
            double lov, double latin, double latin2, double la1, double lo1, double dx,
 double dy) throws SatelliteDecoderException {
        try {
            ProjectedCRS crs = createCRS(crsType, lov, latin, latin2, 0.0);
            DirectPosition2D corner = new DirectPosition2D(lo1, la1);
            MathTransform fromLatLon = MapUtil.getTransformFromLatLon(crs);
            fromLatLon.transform(corner, corner);
            Envelope e = new Envelope(corner.x, corner.x, corner.y, corner.y);
            e.expandToInclude(corner.x + dx * nx, corner.y + dy * ny);
            SatMapCoverage coverage = createCoverageFromEnvelope(crsType, crs,
                    e, nx, ny);
            return checkPersisted(coverage);
        } catch (Exception e) {
            StringBuilder buf = new StringBuilder();
            buf.append(
                    "Error getting or constructing SatMapCoverage for values: ")
                    .append("\n\t");
            buf.append("crsType=" + crsType).append("\n\t");
            buf.append("nx=" + nx).append("\n\t");
            buf.append("ny=" + ny).append("\n\t");
            buf.append("lov=" + lov).append("\n\t");
            buf.append("latin=" + latin).append("\n\t");
            buf.append("la1=" + la1).append("\n\t");
            buf.append("lo1=" + lo1).append("\n\t");
            buf.append("dx=" + dx).append("\n\t");
            buf.append("dy=" + dy).append("\n");
            throw new SatelliteDecoderException(buf.toString(), e);
        }
    }

    /**
     * @see {@link #getCoverageSingleCorner(int, int, int, double, double, double, double, double, double, double)}
     */
    public SatMapCoverage getCoverageSingleCorner(int crsType, int nx, int ny,
            double lov, double latin, double la1, double lo1, double dx,
            double dy) throws SatelliteDecoderException {
        return getCoverageSingleCorner(crsType, nx, ny, lov, latin, latin, la1, lo1, dx, dy);
    }

    /**
     * 
     * Create a {@link SatMapCoverage} with an area defined by two corners. The
     * two corners must be opposite (diagonal) from each other. They can be either
     * the upper left and lower right or the upper right and lower left corners.
     * 
     * @param crsType
     *            the type of CRS, must be one of
     *            {@link #PROJ_CYLIN_EQUIDISTANT}, {@link #PROJ_LAMBERT},
     *            {@link #PROJ_MERCATOR}, {@link #PROJ_POLAR}.
     * @param lov
     *            the longitude orientation, used by
     *            {@link #PROJ_CYLIN_EQUIDISTANT}, {@link #PROJ_LAMBERT},
     *            {@link #PROJ_POLAR}.
     * @param latin
     *            the latitude at which the projection is tangent to the earths
     *            surface, used by {@link #PROJ_CYLIN_EQUIDISTANT},
     *            {@link #PROJ_LAMBERT}, {@link #PROJ_MERCATOR}.
     * @param latin2
     *            the second standard latitude, used by {@link #PROJ_LAMBERT}.
     * @param la1
     *            the latitude of a corner of the grid.
     * @param lo1
     *            the longitude of a corner of the grid.
     * @param la2
     *            the latitude of a corner of the grid., should be opposite
     *            corner from la1.
     * @param lo2
     *            the longitude of a corner of the grid, should be opposite
     *            corner from lo1
     * @return a {@link SatMapCoverage} matching these parameters that has been
     *         loaded from or persisted to the database.
     * @throws DecoderException
     */
    public SatMapCoverage getCoverageTwoCorners(int crsType, int nx, int ny,
            double lov, double latin, double latin2, double la1, double lo1, double la2,
 double lo2) throws SatelliteDecoderException {
        try {
            double cm = 0.0;
            if ((lo1 > 0.0) && (lo2 < 0.0)) {
                cm = 180.0;
            }
            ProjectedCRS crs = createCRS(crsType, lov, latin, latin2, cm);
            DirectPosition2D corner1 = new DirectPosition2D(lo1, la1);
            DirectPosition2D corner2 = new DirectPosition2D(lo2, la2);
            MathTransform fromLatLon = MapUtil.getTransformFromLatLon(crs);
            fromLatLon.transform(corner1, corner1);
            fromLatLon.transform(corner2, corner2);
            Envelope e = new Envelope(corner1.x, corner2.x, corner1.y,
                    corner2.y);
            SatMapCoverage coverage = createCoverageFromEnvelope(crsType, crs,
                    e, nx, ny);
            return checkPersisted(coverage);
        } catch (Exception e) {
            StringBuilder buf = new StringBuilder();
            buf.append(
                    "Error getting or constructing SatMapCoverage for values: ")
                    .append("\n\t");
            buf.append("crsType=" + crsType).append("\n\t");
            buf.append("nx=" + nx).append("\n\t");
            buf.append("ny=" + ny).append("\n\t");
            buf.append("lov=" + lov).append("\n\t");
            buf.append("latin=" + latin).append("\n\t");
            buf.append("la1=" + la1).append("\n\t");
            buf.append("lo1=" + lo1).append("\n\t");
            buf.append("la2=" + la2).append("\n\t");
            buf.append("lo2=" + lo2).append("\n");
            throw new SatelliteDecoderException(buf.toString(), e);
        }
    }

    /**
     * @see {@link #getCoverageTwoCorners(int, int, int, double, double, double, double, double, double, double)}
     */
    public SatMapCoverage getCoverageTwoCorners(int crsType, int nx, int ny,
            double lov, double latin, double la1, double lo1, double la2,
            double lo2) throws SatelliteDecoderException {
        return getCoverageTwoCorners(crsType, nx, ny, lov, latin, latin, la1, lo1, la2, lo2);
    }

    /** Load or persist a {@link SatMapCoverage} */
    private synchronized SatMapCoverage checkPersisted(
            SatMapCoverage mapCoverage) {
        SatMapCoverage persisted = satDao.getOrCreateCoverage(mapCoverage);
        return persisted;
    }

    /**
     * Create a SatMapCoverage from an envelope and additional metadata. The
     * minX and minY from the envelope are used and dx/dy are derived useing the
     * envelope dimensions and nx/ny.
     */
    private static SatMapCoverage createCoverageFromEnvelope(int crsType,
            ProjectedCRS crs, Envelope envelope, int nx, int ny) {
        float dx = (float) (envelope.getWidth() / nx);
        float dy = (float) (envelope.getWidth() / nx);
        return new SatMapCoverage(crsType, envelope.getMinX(),
                envelope.getMinY(), nx, ny, dx, dy, crs);
    }

    /**
     * Create a {@link ProjectedCRS} from a crsType and some parameters.
     * 
     * @param crsType
     *            the type of CRS, must be one of
     *            {@link #PROJ_CYLIN_EQUIDISTANT}, {@link #PROJ_LAMBERT},
     *            {@link #PROJ_MERCATOR}, {@link #PROJ_POLAR}. * @param lov
     * @param lov
     *            the longitude orientation, used by
     *            {@link #PROJ_CYLIN_EQUIDISTANT}, {@link #PROJ_LAMBERT},
     *            {@link #PROJ_POLAR}.
     * @param latin
     *            the latitude at which the projection is tangent to the earths
     *            surface, used by {@link #PROJ_CYLIN_EQUIDISTANT},
     *            {@link #PROJ_LAMBERT}, {@link #PROJ_MERCATOR}.
     * @param latin2
     *            the second standard latitude, used by {@link #PROJ_LAMBERT}.
     * @param cm
     *            the central meridian of the projection, only used by
     *            {@link #PROJ_MERCATOR}.
     * @return
     */
    private static ProjectedCRS createCRS(int crsType, double lov,
            double latin, double latin2, double cm) {
        switch (crsType) {
        case PROJ_MERCATOR:
            return createMercatorCrs(latin, cm);
        case PROJ_LAMBERT:
            return createLambertCrs(latin, latin2, lov);
        case PROJ_CYLIN_EQUIDISTANT:
            return createEqCylCrs(latin, lov);
        default:
            return createNorthPolarStereoCrs(lov);
        }
    }

    private static ProjectedCRS createMercatorCrs(double latin, double cm) {
        return MapUtil.constructMercator(MapUtil.AWIPS_EARTH_RADIUS,
                MapUtil.AWIPS_EARTH_RADIUS, latin, cm);
    }

    private static ProjectedCRS createLambertCrs(double latin, double latin2, double lov) {
        return MapUtil.constructLambertConformal(MapUtil.AWIPS_EARTH_RADIUS,
                MapUtil.AWIPS_EARTH_RADIUS, latin, latin2, lov, latin);
    }

    private static ProjectedCRS createEqCylCrs(double latin, double lov) {
        return MapUtil.constructEquidistantCylindrical(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, lov,
                latin);
    }

    private static ProjectedCRS createNorthPolarStereoCrs(double lov) {
        return MapUtil.constructNorthPolarStereo(MapUtil.AWIPS_EARTH_RADIUS,
                MapUtil.AWIPS_EARTH_RADIUS, 60, lov);
    }

}
