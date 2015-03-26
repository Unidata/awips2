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

import java.util.ArrayList;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Length;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.awipstools.common.stormtrack.AbstractStormTrackResource;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.StormCoord;
import com.raytheon.viz.radar.util.StationUtils;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
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
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
public class DamagePathUtils {

    private static final IUFStatusHandler STATUS_HANDLER = UFStatus
            .getHandler(DamagePathUtils.class);

    /** The unit for the uncertainty algorithm. */
    private static Unit<Length> TARGET_UNIT = NonSI.MILE;

    /** Convert meters returned the GeodeticCalculator to the desired unit. */
    private static UnitConverter METERS_TO = SI.METER
            .getConverterTo(TARGET_UNIT);

    /**
     * Maximum distance of a point to the radar station to use when estimating a
     * damage path polygon. Any farther point will be ignored.
     */
    private static final double MAX_DISTANCE = NonSI.MILE.getConverterTo(
            TARGET_UNIT).convert(300.0); // Based on radar max extent

    private DamagePathUtils() {
        super();
    }


    /**
     * Estimates the damage path polygon for a storm track.
     *
     * @param stormTrack
     *            The storm track to create a damage path for.
     * @return The estimated damage path polygon for a storm track.
     */
    public static Polygon estimateDamagePath(
            AbstractStormTrackResource stormTrack) {

        StormTrackState stState = stormTrack.getStormTrackState();

        RadarStation station = StationUtils.getInstance().getHomeRadarStation();
        GeometryFactory gf = new GeometryFactory();

        List<StormTrackState.StormCoord> skippedCoords = new ArrayList<>();
        Geometry damagePathBuffer = createBuffer(stState.timePoints, station,
                gf, null, skippedCoords);
        damagePathBuffer = createBuffer(stState.futurePoints, station, gf,
                damagePathBuffer, skippedCoords);

        if (!skippedCoords.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append("Skipped the following Storm Coordinates because they ")
                    .append("are out of range of the radar station. ");
            for (StormTrackState.StormCoord coord : skippedCoords) {
                sb.append("(").append(coord.time.toString()).append(", ")
                        .append(coord.coord.x).append(", ")
                        .append(coord.coord.y).append(") ");
            }
            STATUS_HANDLER.info(sb.toString());
        }

        /*
         * user likely tried to import before creating track or entire track is
         * outside of the max range.
         */
        if (damagePathBuffer == null) {
            String suggestion;
            if (skippedCoords.isEmpty()) {
                suggestion = "Make sure the storm track is initialized.";
            } else {
                suggestion = "Make sure the storm track is within range of the radar station.";
            }
            STATUS_HANDLER
                    .warn("Could not create a Damage Path polygon for the storm track. "
                            + suggestion);
            return null;
        }

        Polygon polygon = gf.createPolygon(damagePathBuffer.convexHull()
                .getCoordinates());

        return polygon;
    }


    /**
     * Creates a buffers a buffer around the storm coordinates. If
     * damagePathBuffer is non null, the created buffer will be the union of the
     * two buffers.
     *
     * @param stormCoords
     *            The storm track coordinates.
     * @param station
     *            The station to base distance on.
     * @param gf
     *            The geometry factory.
     * @param damagePathBuffer
     *            The current damage path buffer. May be null.
     * @param farCoords
     *            The list to add any skipped coordinates to.
     * @return The created buffer. If damagePathBuffer is not null, the created
     *         buffer will included damagePathBuffer.
     */
    private static Geometry createBuffer(
            StormTrackState.StormCoord[] stormCoords, RadarStation station,
            GeometryFactory gf, Geometry damagePathBuffer,
            List<StormCoord> farCoords) {
        if (stormCoords == null || stormCoords.length == 0) {
            return damagePathBuffer;
        }

        GeodeticCalculator gc = new GeodeticCalculator();
        Point point;
        Geometry buffer;
        double distanceMeters; // distance in meters
        double distance; // distance in the desired unit
        double uncertainty;
        Coordinate stormCoord;
        for (int i = 0; i < stormCoords.length; i++) {
            stormCoord = stormCoords[i].coord;
            gc.setStartingGeographicPoint(stormCoord.x, stormCoord.y);
            gc.setDestinationGeographicPoint(station.getLon(), station.getLat());
            distanceMeters = gc.getOrthodromicDistance();
            distance = METERS_TO.convert(distanceMeters);

            if (distance > MAX_DISTANCE) {
                farCoords.add(stormCoords[i]);
                continue;
            }

            /*
             * Based off of research done by Doug Speheger comparing surveyed
             * tornado paths to manually identified radar tornadic vortex
             * signatures in 2008-2012. In the initial dataset, 87% of tornadoes
             * were within this range of uncertainty.
             */
            if (distance < 40.0) {
                uncertainty = 0.3 + distance * 0.005;
            } else if (distance < 80.0) {
                uncertainty = 0.1 + distance * 0.01;
            } else {
                uncertainty = distance * 0.015 - 0.3;
            }

            point = gf.createPoint(stormCoord);
            buffer = point.buffer(uncertainty);
            if (damagePathBuffer == null) {
                damagePathBuffer = buffer;
            } else {
                damagePathBuffer = damagePathBuffer.union(buffer);
            }
        }
        return damagePathBuffer;
    }
}
