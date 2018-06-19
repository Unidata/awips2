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
package com.raytheon.viz.aviation.monitor;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * Cache of the times of recent lightning strikes within a certain distance of a
 * coordinate.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class LtgData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LtgData.class);

    private static final int NUM_VERTICES = 32; // 32 sided polygon

    private static final double ANGLE_STEP = 360.0 / NUM_VERTICES;

    private long age;

    private PreparedGeometry geometry;

    private ArrayList<Long> strikeTimes = new ArrayList<Long>();

    /**
     * Constructor
     * 
     * @param siteID
     *            the site ID
     * @param coord
     *            the lat/lon of the site
     * @param distance
     *            the distance from the site in miles to check for lightning
     *            strikes
     * @param age
     *            the age in minutes of data to check
     */
    public LtgData(Coordinate coord, float distance, int age) {
        this.age = age * 60 * 1000;
        createIcaoRing(coord, distance);
    }

    /**
     * Creates a circular(ish) polygon around a coordinate
     * 
     * @param coord
     *            the center of the circle
     * @param distance
     *            the radius of the circle in miles
     */
    private void createIcaoRing(Coordinate coord, float distance) {
        UnitConverter milesToMeters = NonSI.MILE.getConverterTo(SI.METER);
        double range = milesToMeters.convert(distance);

        GeometryFactory geomFactory = new GeometryFactory();
        GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

        try {
            gc.setStartingGeographicPoint(coord.x, coord.y);
            Coordinate c[] = new Coordinate[NUM_VERTICES + 1];
            for (int i = 0; i < NUM_VERTICES; i++) {
                double azimuth = -180.0 + i * ANGLE_STEP;
                gc.setDirection(azimuth, range);
                Point2D p = gc.getDestinationGeographicPoint();
                c[i] = new Coordinate(p.getX(), p.getY());
            }
            c[NUM_VERTICES] = c[0];

            LinearRing lr = geomFactory.createLinearRing(c);
            Polygon poly = (geomFactory.createPolygon(lr, null));
            geometry = PreparedGeometryFactory.prepare(poly);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating ICAO ring for lightning data", e);
        }
    }

    /**
     * Removes all the lightning strikes from the cache
     */
    public void clearStrikes() {
        strikeTimes.clear();
    }

    /**
     * Removes strikes that are older than the configured time
     */
    private void pruneOldStrikes() {
        Collections.sort(strikeTimes);
        Iterator<Long> itr = strikeTimes.iterator();
        long currentTime = SimulatedTime.getSystemTime().getTime().getTime();
        while (itr.hasNext()) {
            long strike = itr.next();
            if (currentTime - strike > age) {
                itr.remove();
            } else {
                // since they're in order, the first time we find one that is
                // new enough
                // to keep around, we can stop checking
                break;
            }
        }
    }

    /**
     * Adds a strike to the tracked sites if it is within the configured time
     * and distance constraints
     * 
     * @param time
     *            the time of the strike
     * @param point
     *            the lat/lon of the strike
     */
    public void addStrikeIfInRange(long time, Point point) {
        long currentTime = SimulatedTime.getSystemTime().getTime().getTime();
        if (currentTime - time < age) {
            if (geometry.contains(point)) {
                strikeTimes.add(time);
            }
        }
    }

    /**
     * Prunes on the configured time and returns the number of strikes in the
     * cache
     * 
     * @return
     */
    public int getNumberOfStrikes() {
        pruneOldStrikes();
        return strikeTimes.size();
    }

}
