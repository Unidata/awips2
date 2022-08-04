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

//*****************************************************************************
// FILE: mil.af.jutl.geopoint.LatLonPoint.java
//
// CLASSIFICATION: Unclassified
//*****************************************************************************
//
package com.raytheon.uf.edex.decodertools.core;

import java.awt.geom.Point2D;
import java.io.Serializable;

import javax.measure.UnitConverter;

import org.geotools.referencing.GeodeticCalculator;
import org.locationtech.jts.geom.Coordinate;

import si.uom.SI;
import systems.uom.common.USCustomary;

/**
 * The LatLonPoint encapsulates latitude and longitude information by providing
 * constructors that allow constructing an instance using latitude and longitude
 * values in degrees.
 *
 * A set of utility methods allow the calculation of various functions such as
 * great circle distances, bearing from a point, or both.
 *
 * NOTE : All set functions validate the input Latitude and Longitude to be in
 * the range Latitude -90 .. 0 .. 90 where negative latitude is in the Southern
 * Hemisphere, and Longitude -180 .. 0 .. 180 where negative longitude is in the
 * Western Hemisphere.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * 20080103      384      jkorman   Ported from JET JUTL classes.
 * Sep 18, 2014  3627     mapeters  Removed unused methods/fields.
 * Jul 15, 2020  8191     randerso  Changed all inputs and outputs to be in
 *                                  degrees and/or Nautical Miles. Added
 *                                  midPoint method. Removed unused methods.
 *
 * </pre>
 *
 * @deprecated use {@link GeodeticCalculator} and {@link Coordinate}
 * @author jkorman
 */
@Deprecated
public class LatLonPoint implements Cloneable, Serializable {
    private static final long serialVersionUID = 1L;

    private static UnitConverter mToNM = SI.METRE
            .getConverterTo(USCustomary.NAUTICAL_MILE);

    private static UnitConverter NMTom = USCustomary.NAUTICAL_MILE
            .getConverterTo(SI.METRE);

    /** the latitude value for this point in degrees. */
    private double theLatitude;

    /** the longitude value for this point in degrees. */
    private double theLongitude;

    /**
     * Construct a new LatLonPoint with the given data.
     *
     * @param aLatitude
     *            the value of the latitude
     * @param aLongitude
     *            the value of the longitude
     */
    public LatLonPoint(double aLatitude, double aLongitude) {
        validateLatLon(aLatitude, 0);

        theLatitude = aLatitude;
        theLongitude = aLongitude;
    }

    /**
     * Construct a new LatLonPoint from a given LatLonPoint. This is a copy
     * constructor for LatLonPoint.
     *
     * @param thePoint
     *            The LatLonPoint to copy.
     */
    public LatLonPoint(LatLonPoint thePoint) {
        this.theLatitude = thePoint.theLatitude;
        this.theLongitude = thePoint.theLongitude;
    }

    /**
     * Deep-copy clone of the LatLonPoint class. This method implements the
     * contract for Object.clone() in that a new instance of the class is
     * created.
     *
     * @return A clone of this LatLonPoint object.
     */
    @Override
    public Object clone() {
        return new LatLonPoint(this);
    }

    /**
     * Validate an input Latitude and Longitude according to the input units.
     *
     * @param aLatitude
     *            The value of the latitude.
     * @param aLongitude
     *            The value of the latitude.
     * @throws LatLonRangeException
     *             if either data is out of range.
     */
    private static void validateLatLon(double aLatitude, double aLongitude) {
        if ((aLatitude < -90.0) || (aLatitude > 90.0)) {
            throw new LatLonRangeException(
                    "Latitude out of range " + aLatitude);
        }
        if ((aLongitude < -180.0) || (aLongitude > 180.0)) {
            throw new LatLonRangeException(
                    "Longitude out of range " + aLongitude);
        }
    }

    /**
     * Get the latitude of this point.
     *
     * @return the latitude of this point in degrees.
     */
    public double getLatitude() {
        return theLatitude;
    }

    /**
     * Get the longitude of this point.
     *
     * @return the longitude of this point in degrees.
     */
    public double getLongitude() {
        return theLongitude;
    }

    /**
     * Set the latitude of this point.
     *
     * @param aLatitude
     *            value of the latitude in degrees
     */
    public final void setLatitude(double aLatitude) {
        // Validate latitude, longitude set to 0
        validateLatLon(aLatitude, 0);

        theLatitude = aLatitude;
    }

    /**
     * Set the longitude of this point.
     *
     * @param aLongitude
     *            Value of the longitude in degrees.
     */
    public final void setLongitude(double aLongitude) {
        // Validate longitude, latitude set to 0
        validateLatLon(0, aLongitude);

        theLongitude = aLongitude;
    }

    /**
     * Compute the great circle distance in nautical miles between this point
     * and another point located on the ellipsoid.
     *
     * @param aLatitude
     *            latitude of the second point
     * @param aLongitude
     *            longitude of the second point
     * @return The great circle distance in nautical miles.
     */
    public double distanceTo(double aLatitude, double aLongitude) {
        validateLatLon(aLatitude, aLongitude);

        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(theLongitude, theLatitude);
        gc.setDestinationGeographicPoint(aLongitude, aLatitude);

        double distance = gc.getOrthodromicDistance();

        return mToNM.convert(distance);
    }

    /**
     * Compute the latitude/longitude position of a point at a specified bearing
     * and distance from this point.
     *
     * @param aBearing
     *            the initial bearing, clockwise from north in degrees, of the
     *            point to calculate
     * @param aDistance
     *            the distance from the reference point, in nautical miles, of
     *            point to calculate
     * @return The computed location.
     */
    public LatLonPoint positionOf(double aBearing, double aDistance) {

        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(theLongitude, theLatitude);
        gc.setDirection(aBearing, NMTom.convert(aDistance));
        Point2D toPoint = gc.getDestinationGeographicPoint();

        return new LatLonPoint(toPoint.getY(), toPoint.getX());
    }

    /**
     * Compute the midway point from this point and a second point
     *
     * @param toPoint
     *            the second point
     * @return the midPoint
     */
    public LatLonPoint midPoint(LatLonPoint toPoint) {

        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(theLongitude, theLatitude);
        gc.setDestinationGeographicPoint(toPoint.theLongitude,
                toPoint.theLatitude);

        double distance = gc.getOrthodromicDistance() / 2;
        double bearing = gc.getAzimuth();

        gc.setDirection(bearing, distance);
        Point2D midPoint = gc.getDestinationGeographicPoint();

        return new LatLonPoint(midPoint.getY(), midPoint.getX());
    }
}
