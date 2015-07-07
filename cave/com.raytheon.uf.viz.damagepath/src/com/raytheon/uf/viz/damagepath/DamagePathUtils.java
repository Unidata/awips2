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
package com.raytheon.uf.viz.damagepath;

import java.awt.geom.Point2D;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.awipstools.ui.layer.InteractiveBaselinesLayer.Baseline;
import com.raytheon.viz.radar.util.StationUtils;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Utility class for Damage Paths.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2015 3977       nabowle     Initial creation
 * Mar 26, 2015 3977       nabowle     Limit distance to avoid polar errors. Log
 *                                     skipped points.
 * Apr 01, 2015 3977       nabowle     rename the status handler.
 * Jun 18, 2015 3977       nabowle     Fix buffering. Renamed mehtods to
 *                                     specify that they're for Tornados.
 * Jun 25, 2015 3977       nabowle     Redo inner-point buffering for use with
 *                                     Interactive Baselines.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
public class DamagePathUtils {

    /**
     * The number of degrees between points at the ends of a Tornado Damage Path
     * Buffer.
     */
    private static final double END_DEGREE_DIFF = 30.0D;

    /** Upper degree boundary to define a sharp angle. */
    private static final double SHARP_ANGLE = 100;

    /** Lower degree boundary when a sharp angle straddles 0 degrees. */
    private static final double SHARP_ANGLE_INV = 360 - SHARP_ANGLE;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DamagePathUtils.class);

    /** Convert miles for the algorithm to meters. */
    private static UnitConverter TO_METERS = NonSI.MILE
            .getConverterTo(SI.METER);

    /*
     * Pre-convert algorithm constants to meters to prevent having to convert
     * back and forth.
     */
    private static final double ONE_TENTH_MILE = TO_METERS.convert(0.1);

    private static final double THREE_TENTHS_MILE = TO_METERS.convert(0.3);

    private static final double FORTY_MILES = TO_METERS.convert(40.0);

    private static final double EIGHTY_MILES = TO_METERS.convert(80.0);

    /**
     * Maximum distance of a point to the radar station to use when estimating a
     * damage path polygon. Any farther point will be ignored. Based on radar
     * max extent
     */
    private static final double MAX_DISTANCE = TO_METERS.convert(300.0);

    private DamagePathUtils() {
        super();
    }


    /**
     * Estimates a Tornado damage path polygon for a baseline.
     *
     * @param baseline
     *            The baseline to create a tornado damage path for.
     * @return The estimated tornado damage path polygon for a baseline.
     */
    public static Polygon estimateTornadoDamagePath(Baseline baseline) {

        Coordinate[] coords = baseline.line.getCoordinates();
        RadarStation station = StationUtils.getInstance().getHomeRadarStation();
        GeometryFactory gf = new GeometryFactory();

        List<Coordinate> skippedCoords = new ArrayList<>();
        List<Coordinate> validCoords = new ArrayList<>();
        GeodeticCalculator gc = new GeodeticCalculator();
        filterCoords(coords, station, skippedCoords, validCoords,
                gc);

        if (!skippedCoords.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append(
                    "Skipped the following Coordinates for Baseline "
                            + baseline.name + " because they ")
                    .append("are out of range of the radar station. ");
            for (Coordinate coord : skippedCoords) {
                sb.append("(").append(coord.x).append(", ").append(coord.y)
                        .append(") ");
            }
            statusHandler.info(sb.toString());
        }

        if (validCoords.isEmpty()) {
            String suggestion;
            if (skippedCoords.isEmpty()) {
                suggestion = "Make sure the baseline is initialized.";
            } else {
                suggestion = "Make sure the baseline is within range of the radar station.";
            }
            statusHandler
                    .warn("Could not create a Damage Path polygon for Baseline "
                            + baseline.name + ". " + suggestion);
            return null;
        }

        Coordinate[] damagePathCoords = createTornadoBuffer(validCoords, station, gc,
                gf);

        Polygon polygon = gf.createPolygon(damagePathCoords);

        return polygon;
    }


    /**
     * Filter the coordinates based on their range to the given radar station.
     *
     * @param stormCoords
     *            The coordinates.
     * @param station
     *            The radar station.
     * @param skippedCoords
     *            A list where coordinates that are out of range of the station
     *            will be added to.
     * @param validCoords
     *            A list where coordinates that are in range of the station will
     *            be added to.
     * @param gc
     *            A GeodeticCalculator.
     */
    private static void filterCoords(Coordinate[] stormCoords,
            RadarStation station, List<Coordinate> skippedCoords,
            List<Coordinate> validCoords, GeodeticCalculator gc) {
        Coordinate stormCoord;
        double distance;
        if (stormCoords != null) {
            for (int i = 0; i < stormCoords.length; i++) {
                stormCoord = stormCoords[i];
                gc.setStartingGeographicPoint(stormCoord.x, stormCoord.y);
                gc.setDestinationGeographicPoint(station.getLon(),
                        station.getLat());
                distance = gc.getOrthodromicDistance();

                if (distance > MAX_DISTANCE) {
                    skippedCoords.add(stormCoords[i]);
                } else {
                    validCoords.add(stormCoords[i]);
                }
            }
        }
    }

    /**
     * Creates a buffer around the line defined by the storm coordinates using
     * Doug Speheger's tornado damage path algorithm.
     *
     * @param stormCoords
     *            The storm coordinates to create a damage path around.
     * @param station
     *            The radar station to use.
     * @param gc
     *            A GeodeticCalculator.
     * @param gf
     *            A GeometryFactory.
     * @return A Geometry representing an estimated Tornado Damage Path for the
     *         given storm coordinates. If the no coordinates are provided, null
     *         is returned.
     */
    private static Coordinate[] createTornadoBuffer(List<Coordinate> stormCoords,
            RadarStation station, GeodeticCalculator gc, GeometryFactory gf) {
        // left hand side points
        List<Point2D> lhsPoints = new ArrayList<>();
        // right hand side points
        Deque<Point2D> rhsPoints = new ArrayDeque<>();
        double uncertainty;
        Coordinate stormCoord;
        /* Create a concave hull for a linear set of coordinates. */
        for (int i = 0; i < stormCoords.size(); i++) {
            stormCoord = stormCoords.get(i);
            uncertainty = calculateUncertainty(station, gc, stormCoord);

            if (stormCoords.size() == 1) {
                /*
                 * In the case that there's only one coordinate, draw a circle
                 * around the point and break out.
                 */
                for (double d = -180; d < 180; d += END_DEGREE_DIFF) {
                    gc.setDirection(d, uncertainty);
                    lhsPoints.add(gc.getDestinationGeographicPoint());
                }
                break;
            }

            if (i == 0) {
                createStartCap(stormCoords, gc, lhsPoints, uncertainty, i);
            } else if (i < stormCoords.size() - 1) {
                bufferInnerPoint(stormCoord, stormCoords, gc, lhsPoints,
                        rhsPoints, uncertainty, i, station);
            } else {
                createEndCap(stormCoords, gc, rhsPoints, uncertainty, i);
            }

        }
        lhsPoints.addAll(rhsPoints);

        Coordinate[] coordinates = new Coordinate[lhsPoints.size() + 1];
        Point2D point;
        for (int i = 0; i < lhsPoints.size(); i++) {
            point = lhsPoints.get(i);
            coordinates[i] = new Coordinate(point.getX(), point.getY());
        }
        coordinates[coordinates.length - 1] = coordinates[0];

        return coordinates;
    }


    /**
     * Based off of research done by Doug Speheger comparing surveyed tornado
     * paths to manually identified radar tornadic vortex signatures in
     * 2008-2012. In the initial dataset, 87% of tornadoes were within this
     * range of uncertainty.
     *
     * Note: All units are in meters. Constants have been pre-converted from
     * miles to meters.
     *
     * @param station
     * @param gc
     * @param coord
     * @return
     */
    private static double calculateUncertainty(RadarStation station,
            GeodeticCalculator gc, Coordinate coord) {
        gc.setStartingGeographicPoint(coord.x, coord.y);
        gc.setDestinationGeographicPoint(station.getLon(), station.getLat());
        double distance = gc.getOrthodromicDistance();

        double uncertainty;
        if (distance < FORTY_MILES) {
            uncertainty = THREE_TENTHS_MILE + distance * 0.005;
        } else if (distance < EIGHTY_MILES) {
            uncertainty = ONE_TENTH_MILE + distance * 0.01;
        } else {
            uncertainty = distance * 0.015 - THREE_TENTHS_MILE;
        }
        return uncertainty;
    }

    /**
     * Creates a buffer around a non-end point. If the path is on a generally
     * straight path, just create two points on opposite sides of the path
     * point. If the path coordinate is the vertex of an angle is sharp, create
     * a couple points around the outside of the angle and create a single point
     * on the inside that's uncertainty-meters from the point and both lines
     * creating the angle.
     *
     * @param stormCoord
     * @param stormCoords
     * @param gc
     * @param lhsPoints
     * @param rhsPoints
     * @param uncertainty
     * @param i
     * @param station
     */
    private static void bufferInnerPoint(Coordinate stormCoord,
            List<Coordinate> stormCoords, GeodeticCalculator gc,
            List<Point2D> lhsPoints, Deque<Point2D> rhsPoints,
            double uncertainty, int i, RadarStation station) {
        gc.setStartingGeographicPoint(stormCoord.x, stormCoord.y);

        Coordinate nextCoord = stormCoords.get(i + 1);
        gc.setDestinationGeographicPoint(nextCoord.x, nextCoord.y);
        double azimuthToNext = nonNegativeAzimuth(gc.getAzimuth());

        Coordinate prevCoord = stormCoords.get(i - 1);
        gc.setDestinationGeographicPoint(prevCoord.x, prevCoord.y);
        double azimuthToPrev = nonNegativeAzimuth(gc.getAzimuth());

        double angle = Math.abs(azimuthToPrev - azimuthToNext);
        boolean sharpAngle = (angle <= SHARP_ANGLE && angle > 0)
                || (angle >= SHARP_ANGLE_INV && angle < 360);

        if (sharpAngle) {
            boolean rhsOnInside = angle <= SHARP_ANGLE ? azimuthToNext < azimuthToPrev
                    : azimuthToPrev < azimuthToNext;
            double bisectionAngle = Math
                    .toRadians(angle >= SHARP_ANGLE_INV ? (360.0 - angle) / 2
                            : angle / 2);

            gc.setDirection(geodeticAzimuth(azimuthToNext + 180), uncertainty);
            if (rhsOnInside) {
                lhsPoints.add(gc.getDestinationGeographicPoint());

                createOpposingPoints(stormCoord, gc, lhsPoints,
                        new ArrayDeque<Point2D>(), uncertainty, azimuthToNext,
                        azimuthToPrev);
                double innerUncertainty = Math.abs(uncertainty
                        / Math.sin(bisectionAngle));
                if (innerUncertainty < uncertainty) {
                    innerUncertainty = uncertainty;
                }
                createOpposingPoints(stormCoord, gc, new ArrayList<Point2D>(),
                        rhsPoints, innerUncertainty, azimuthToNext,
                        azimuthToPrev);

                gc.setDirection(geodeticAzimuth(azimuthToPrev + 180),
                        uncertainty);
                lhsPoints.add(gc.getDestinationGeographicPoint());

            } else {
                rhsPoints.push(gc.getDestinationGeographicPoint());

                createOpposingPoints(stormCoord, gc, new ArrayList<Point2D>(),
                        rhsPoints, uncertainty, azimuthToNext, azimuthToPrev);
                double innerUncertainty = Math.abs(uncertainty
                        / Math.sin(bisectionAngle));
                if (innerUncertainty < uncertainty) {
                    innerUncertainty = uncertainty;
                }
                createOpposingPoints(stormCoord, gc, lhsPoints,
                        new ArrayDeque<Point2D>(), innerUncertainty,
                        azimuthToNext, azimuthToPrev);

                gc.setDirection(geodeticAzimuth(azimuthToPrev + 180),
                        uncertainty);
                rhsPoints.push(gc.getDestinationGeographicPoint());
            }
        } else {
            createOpposingPoints(stormCoord, gc, lhsPoints, rhsPoints, uncertainty,
                    azimuthToNext, azimuthToPrev);
        }
    }

    /**
     * Create two points at 180 degrees from each other on the imaginary line
     * that bisects the angle created at this point.
     *
     * @param coord
     * @param gc
     * @param lhsPoints
     * @param rhsPoints
     * @param uncertainty
     * @param azimuthToNext
     * @param azimuthToPrev
     */
    private static void createOpposingPoints(Coordinate coord,
            GeodeticCalculator gc,
            List<Point2D> lhsPoints, Deque<Point2D> rhsPoints,
            double uncertainty, double azimuthToNext, double azimuthToPrev) {
        gc.setStartingGeographicPoint(coord.x, coord.y);
        double pointAzimuth = geodeticAzimuth(Math.min(azimuthToNext,
                azimuthToPrev)
                + Math.abs(azimuthToPrev - azimuthToNext) / 2);

        if (azimuthToNext <= azimuthToPrev) {
            pointAzimuth = geodeticAzimuth(pointAzimuth + 180);
        }

        gc.setDirection(pointAzimuth, uncertainty);
        lhsPoints.add(gc.getDestinationGeographicPoint());

        pointAzimuth = geodeticAzimuth(pointAzimuth + 180);

        gc.setDirection(pointAzimuth, uncertainty);
        rhsPoints.push(gc.getDestinationGeographicPoint());
    }


    /**
     * Create's a curved half-circle buffer around the starting coordinate.
     *
     * @param stormCoords
     * @param gc
     * @param lhsPoints
     * @param uncertainty
     * @param i
     */
    private static void createStartCap(List<Coordinate> stormCoords,
            GeodeticCalculator gc, List<Point2D> lhsPoints, double uncertainty,
            int i) {
        Coordinate nextCoord = stormCoords.get(i + 1);
        gc.setDestinationGeographicPoint(nextCoord.x, nextCoord.y);
        double azimuthToNext = gc.getAzimuth();

        double pointAzimuth = geodeticAzimuth(azimuthToNext + 90);
        gc.setDirection(pointAzimuth, uncertainty);
        lhsPoints.add(gc.getDestinationGeographicPoint());

        for (double d = END_DEGREE_DIFF; d <= 180; d += END_DEGREE_DIFF) {
            pointAzimuth = geodeticAzimuth(pointAzimuth + END_DEGREE_DIFF);
            gc.setDirection(pointAzimuth, uncertainty);
            lhsPoints.add(gc.getDestinationGeographicPoint());
        }
    }

    /**
     * Create's a curved half-circle buffer around the ending coordinate.
     *
     * @param stormCoords
     * @param gc
     * @param rhsPoints
     * @param uncertainty
     * @param i
     */
    private static void createEndCap(List<Coordinate> stormCoords,
            GeodeticCalculator gc, Deque<Point2D> rhsPoints,
            double uncertainty, int i) {
        Coordinate prevCoord = stormCoords.get(i - 1);
        gc.setDestinationGeographicPoint(prevCoord.x, prevCoord.y);
        double azimuthToPrev = gc.getAzimuth();

        double pointAzimuth = geodeticAzimuth(azimuthToPrev - 90);
        gc.setDirection(pointAzimuth, uncertainty);
        rhsPoints.push(gc.getDestinationGeographicPoint());

        for (double d = END_DEGREE_DIFF; d <= 180; d += END_DEGREE_DIFF) {
            pointAzimuth = geodeticAzimuth(pointAzimuth - END_DEGREE_DIFF);
            gc.setDirection(pointAzimuth, uncertainty);
            rhsPoints.push(gc.getDestinationGeographicPoint());
        }
    }

    /**
     * Clamps the azimuth to [-180, 180] for use with the GeodeticCalculator.
     *
     * @param azimuth
     *            The azimuth.
     * @return An equivalent azimuth in [-180, 180].
     */
    private static double geodeticAzimuth(double azimuth) {
        double az = azimuth;
        while (az < -180.0) {
            az += 360.0;
        }

        while (az > 180.0) {
            az -= 360.0;
        }

        return az;
    }

    /**
     * Clamps the azimuth to [0, 360] to simplify the azimath.
     *
     * @param azimuth
     *            The azimuth.
     * @return An equivalent azimuth in [0, 360].
     */
    private static double nonNegativeAzimuth(double azimuth) {
        double az = azimuth;
        while (az < 0) {
            az += 360.0;
        }

        while (az > 360.0) {
            az -= 360.0;
        }

        return az;
    }
}
