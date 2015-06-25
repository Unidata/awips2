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
import com.raytheon.viz.awipstools.common.stormtrack.AbstractStormTrackResource;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState;
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
     * Estimates a Tornado damage path polygon for a storm track.
     *
     * @param stormTrack
     *            The storm track to create a tornado damage path for.
     * @return The estimated tornado damage path polygon for a storm track.
     */
    public static Polygon estimateTornadoDamagePath(
            AbstractStormTrackResource stormTrack) {

        StormTrackState stState = stormTrack.getStormTrackState();

        RadarStation station = StationUtils.getInstance().getHomeRadarStation();
        GeometryFactory gf = new GeometryFactory();

        List<StormTrackState.StormCoord> skippedCoords = new ArrayList<>();
        List<Coordinate> validCoords = new ArrayList<>();
        GeodeticCalculator gc = new GeodeticCalculator();
        filterCoords(stState.timePoints, station, skippedCoords, validCoords,
                gc);
        filterCoords(stState.futurePoints, station, skippedCoords, validCoords,
                gc);

        Coordinate[] damagePathCoords = createTornadoBuffer(validCoords, station, gc,
                gf);

        if (!skippedCoords.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append("Skipped the following Storm Coordinates because they ")
                    .append("are out of range of the radar station. ");
            for (StormTrackState.StormCoord coord : skippedCoords) {
                sb.append("(").append(coord.time.toString()).append(", ")
                        .append(coord.coord.x).append(", ")
                        .append(coord.coord.y).append(") ");
            }
            statusHandler.info(sb.toString());
        }

        /*
         * user likely tried to import before creating track or entire track is
         * outside of the max range.
         */
        if (damagePathCoords == null) {
            String suggestion;
            if (skippedCoords.isEmpty()) {
                suggestion = "Make sure the storm track is initialized.";
            } else {
                suggestion = "Make sure the storm track is within range of the radar station.";
            }
            statusHandler
                    .warn("Could not create a Damage Path polygon for the storm track. "
                            + suggestion);
            return null;
        }

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
    private static void filterCoords(StormTrackState.StormCoord[] stormCoords,
            RadarStation station,
            List<StormTrackState.StormCoord> skippedCoords,
            List<Coordinate> validCoords, GeodeticCalculator gc) {
        Coordinate stormCoord;
        double distance;
        if (stormCoords != null) {
            for (int i = 0; i < stormCoords.length; i++) {
                stormCoord = stormCoords[i].coord;
                gc.setStartingGeographicPoint(stormCoord.x, stormCoord.y);
                gc.setDestinationGeographicPoint(station.getLon(),
                        station.getLat());
                distance = gc.getOrthodromicDistance();

                if (distance > MAX_DISTANCE) {
                    skippedCoords.add(stormCoords[i]);
                } else if (validCoords.isEmpty()
                            || !validCoords.get(validCoords.size() - 1).equals(
                                    stormCoords[i].coord)) {
                    validCoords.add(stormCoords[i].coord);
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

        if (stormCoords.isEmpty()) {
            return null;
        }

        // left hand side points
        List<Point2D> lhsPoints = new ArrayList<>();
        // right hand side points
        Deque<Point2D> rhsPoints = new ArrayDeque<>();
        double distance;
        double uncertainty;
        double azimuth;
        double pointAzimuth;
        Coordinate stormCoord;
        Coordinate otherCoord;
        /* Create a concave hull for a linear set of coordinates. */
        for (int i = 0; i < stormCoords.size(); i++) {
            stormCoord = stormCoords.get(i);
            gc.setStartingGeographicPoint(stormCoord.x, stormCoord.y);
            gc.setDestinationGeographicPoint(station.getLon(), station.getLat());
            distance = gc.getOrthodromicDistance();

            /*
             * Based off of research done by Doug Speheger comparing surveyed
             * tornado paths to manually identified radar tornadic vortex
             * signatures in 2008-2012. In the initial dataset, 87% of tornadoes
             * were within this range of uncertainty.
             *
             * Note: All units are in meters. Constants have been pre-converted
             * from miles to meters.
             */
            if (distance < FORTY_MILES) {
                uncertainty = THREE_TENTHS_MILE + distance * 0.005;
            } else if (distance < EIGHTY_MILES) {
                uncertainty = ONE_TENTH_MILE + distance * 0.01;
            } else {
                uncertainty = distance * 0.015 - THREE_TENTHS_MILE;
            }

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

            if (i < stormCoords.size() - 1) {
                otherCoord = stormCoords.get(i + 1);
                gc.setDestinationGeographicPoint(otherCoord.x, otherCoord.y);
                azimuth = gc.getAzimuth();

                if (i == 0) {
                    // create start cap
                    pointAzimuth = clampAzimuth(azimuth + 90);
                    gc.setDirection(pointAzimuth, uncertainty);
                    lhsPoints.add(gc.getDestinationGeographicPoint());
                    for (double d = END_DEGREE_DIFF; d <= 180; d += END_DEGREE_DIFF) {
                        pointAzimuth = clampAzimuth(pointAzimuth
                                + END_DEGREE_DIFF);
                        gc.setDirection(pointAzimuth, uncertainty);
                        lhsPoints.add(gc.getDestinationGeographicPoint());
                    }
                } else {
                    /*
                     * Create two points at 90 degrees from the direction of the
                     * path.
                     */
                    pointAzimuth = clampAzimuth(azimuth - 90);
                    gc.setDirection(pointAzimuth, uncertainty);
                    lhsPoints.add(gc.getDestinationGeographicPoint());
                    pointAzimuth = clampAzimuth(azimuth + 90);
                    gc.setDirection(pointAzimuth, uncertainty);
                    rhsPoints.push(gc.getDestinationGeographicPoint());
                }
            } else {
                // create end cap
                otherCoord = stormCoords.get(i - 1);
                gc.setDestinationGeographicPoint(otherCoord.x, otherCoord.y);
                azimuth = gc.getAzimuth();
                pointAzimuth = clampAzimuth(azimuth - 90);
                gc.setDirection(pointAzimuth, uncertainty);
                rhsPoints.push(gc.getDestinationGeographicPoint());
                for (double d = END_DEGREE_DIFF; d <= 180; d += END_DEGREE_DIFF) {
                    pointAzimuth = clampAzimuth(pointAzimuth - END_DEGREE_DIFF);
                    gc.setDirection(pointAzimuth, uncertainty);
                    rhsPoints.push(gc.getDestinationGeographicPoint());
                }
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
     * Clamps the azimuth to [-180, 180]
     *
     * @param azimuth
     *            The azimuth.
     * @return An equivalent azimuth in [-180, 180]
     */
    private static double clampAzimuth(double azimuth) {
        double az = azimuth;
        while (az < -180.0) {
            az += 360.0;
        }

        while (az > 180.0) {
            az -= 360.0;
        }

        return az;
    }
}
