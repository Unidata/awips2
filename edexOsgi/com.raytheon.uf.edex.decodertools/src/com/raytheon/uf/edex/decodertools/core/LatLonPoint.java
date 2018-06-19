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

import java.io.Serializable;

/**
 * The LatLonPoint encapsulates latitude and longitude information by providing
 * constructors that allow constructing an instance using latitude and longitude
 * values using either degrees or Radians. Utility methods to set and/or get
 * latitude and longitude allow the use of specified units also. A separate set
 * of utility methods allow the calculation of various spherical functions such
 * as great circle distances, bearing from a point, or both.
 * 
 * NOTE : All set functions validate the input Latitude and Longitude to be in
 * the range Latitude -90 .. 0 .. 90 where negative latitude is in the Southern
 * Hemisphere, and Longitude -180 .. 0 .. 180 where negative longitude is in the
 * Western Hemisphere.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080103            384 jkorman     Ported from JET JUTL classes.
 * Sep 18, 2014       3627 mapeters    Removed unused methods/fields.
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public class LatLonPoint implements Cloneable, Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * Input angular measurement is in Degrees.
     */
    public static final int INDEGREES = 1;

    /**
     * Input angular measurement is in Radians.
     */
    public static final int INRADIANS = 2;

    // the latitude value for this point in radians.
    private double theLatitude;

    // the longitude value for this point in radians.
    private double theLongitude;

    // the latitude value for this point in degrees.
    private double theLatitudeInDegrees;

    // the longitude value for this point in degrees.
    private double theLongitudeInDegrees;

    /**
     * Construct a new LatLonPoint with the given data.
     * 
     * @param aLatitude
     *            the value of the latitude in radians
     * @param aLongitude
     *            the value of the latitude in radians
     */
    public LatLonPoint(double aLatitude, double aLongitude) {
        this(aLatitude, aLongitude, INRADIANS);
    } // LatLonPoint

    /**
     * Construct a new LatLonPoint with the given data.
     * 
     * @param aLatitude
     *            the value of the latitude
     * @param aLongitude
     *            the value of the longitude
     * @param units
     *            The units of the input latitude/longitude.
     */
    public LatLonPoint(double aLatitude, double aLongitude, int units) {
        setLatLon(aLatitude, aLongitude, units);
    } // LatLonPoint

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
        this.theLatitudeInDegrees = thePoint.theLatitudeInDegrees;
        this.theLongitudeInDegrees = thePoint.theLongitudeInDegrees;
    }

    /**
     * Deep-copy clone of the LatLonPoint class. This method implements the
     * contract for Object.clone() in that a new instance of the class is
     * created.
     * 
     * @return A clone of this LatLonPoint object.
     */
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
     * @param units
     *            The units for the value.
     * @see #INRADIANS
     * @see #INDEGREES
     * @throws LatLonRangeException
     *             if either data is out of range.
     */
    private static void validateLatLon(double aLatitude, double aLongitude,
            int units) {
        switch (units) {
        case INDEGREES:
            if ((aLatitude < -90.0) || (aLatitude > 90.0)) {
                throw new LatLonRangeException("Latitude out of range "
                        + aLatitude);
            }
            if ((aLongitude < -180.0) || (aLongitude > 180.0)) {
                throw new LatLonRangeException("Longitude out of range "
                        + aLongitude);
            }
            break;

        default: // assume radians
            if ((aLatitude < -(Math.PI / 2)) || (aLatitude > (Math.PI / 2))) {
                throw new LatLonRangeException("Latitude out of range "
                        + aLatitude);
            }
            if ((aLongitude < -Math.PI) || (aLongitude > Math.PI)) {
                throw new LatLonRangeException("Longitude out of range "
                        + aLongitude);
            }
        } // switch
    }

    // ***
    // get and set methods.
    // ***

    /**
     * Set both the latitude and longitude for this LatLonPoint. This private
     * method is meant for use by the constructors so that Latitude and
     * Longitude may be set without setting the quadrant.
     * 
     * @param aLatitude
     *            The value of the latitude.
     * @param aLongitude
     *            The value of the latitude.
     * @param units
     *            The units for the value.
     * @see #INRADIANS
     * @see #INDEGREES
     */
    private void setLatLon(double aLatitude, double aLongitude, int units) {
        validateLatLon(aLatitude, 0, units);

        switch (units) {
        case INDEGREES:
            theLatitude = Math.toRadians(aLatitude);
            theLatitudeInDegrees = aLatitude;
            theLongitude = Math.toRadians(aLongitude);
            theLongitudeInDegrees = aLongitude;
            break;

        // assume radians
        default:
            theLatitude = aLatitude;
            theLatitudeInDegrees = Math.toDegrees(aLatitude);
            theLongitude = aLongitude;
            theLongitudeInDegrees = Math.toDegrees(aLongitude);
        } // switch
    }

    /**
     * Get the latitude of this point.
     * 
     * @return The latitude of this point in Radians.
     */
    public double getLatitude() {
        return theLatitude;
    }

    /**
     * Get the longitude of this point.
     * 
     * @return The longitude of this point in Radians.
     */
    public double getLongitude() {
        return theLongitude;
    }

    /**
     * Get the latitude of this point.
     * 
     * @param units
     *            The output units.
     * @see #INRADIANS
     * @see #INDEGREES
     * @return the latitude of this point.
     */
    public double getLatitude(int units) {
        switch (units) {
        case INDEGREES:
            return theLatitudeInDegrees;

            // assume radians
        default:
            return theLatitude;
        } // switch
    } // getLatitude

    /**
     * Get the longitude of this point.
     * 
     * @param units
     *            The output units.
     * @see #INRADIANS
     * @see #INDEGREES
     * @return the longitude of this point.
     */
    public double getLongitude(int units) {
        switch (units) {
        case INDEGREES:
            return theLongitudeInDegrees;

            // assume radians
        default:
            return theLongitude;
        } // switch
    }

    /**
     * Set the latitude of this point.
     * 
     * @param aLatitude
     *            value of the latitude
     * @param units
     *            the units for the value
     * @see #INRADIANS
     * @see #INDEGREES
     */
    public final void setLatitude(double aLatitude, int units) {
        // Validate latitude, longitude set to 0
        validateLatLon(aLatitude, 0, units);

        switch (units) {
        case INDEGREES:
            theLatitude = Math.toRadians(aLatitude);
            theLatitudeInDegrees = aLatitude;
            break;

        // assume radians
        default:
            theLatitude = aLatitude;
            theLatitudeInDegrees = Math.toDegrees(aLatitude);
        } // switch
    }

    /**
     * Set the longitude of this point.
     * 
     * @param aLongitude
     *            Value of the longitude.
     * @param units
     *            The units for the value.
     * @see #INRADIANS
     * @see #INDEGREES
     */
    public final void setLongitude(double aLongitude, int units) {
        // Validate longitude, latitude set to 0
        validateLatLon(0, aLongitude, units);

        switch (units) {
        case INDEGREES:
            theLongitude = Math.toRadians(aLongitude);
            theLongitudeInDegrees = aLongitude;
            break;

        // assume radians
        default: // assume radians
            theLongitude = aLongitude;
            theLongitudeInDegrees = Math.toDegrees(aLongitude);

        } // switch
    }

    // ***
    //
    // ***

    /**
     * This is a special Modulus function for floating point computations. It is
     * required for trigometric functions.
     * 
     * @param x
     * @param y
     * @return The decimal part of the
     */
    private static double mod(double x, double y) {
        if (x >= 0) {
            return (x - y * ((long) (x / y)));
        } else {
            return (x + y * (((long) (-x / y)) + 1));
        }
    }

    /**
     * Compute the great circle distance (in radians) between two points located
     * on a spherical surface.
     * 
     * @param fromLat
     *            Latitude of first point.
     * @param fromLon
     *            Longitude of first point.
     * @param toLat
     *            Latitude of second point.
     * @param toLon
     *            Longitude of second point. All of the above
     *            latitudes/longitudes are measured in radians.
     * @return The great circle distance in radians.
     */
    public static double GreatCircle(double fromLat, double fromLon,
            double toLat, double toLon) {
        //
        // gc1 and gc2 are temporary computed terms.
        //
        double gc1 = Math.pow(Math.sin((fromLat - toLat) / 2), 2);

        double gc2 = Math.cos(fromLat) * Math.cos(toLat)
                * Math.pow(Math.sin((fromLon - toLon) / 2), 2);

        return (2 * Math.asin(Math.sqrt(gc1 + gc2)));
    }

    /**
     * Compute the great circle distance (in radians) between this point and
     * another point located on a spherical surface.
     * 
     * @param aLatitude
     *            latitude of the second point
     * @param aLongitude
     *            longitude of the second point
     * @param units
     *            input units of the Latitude and Longitude
     * @see #INRADIANS
     * @see #INDEGREES
     * @return The great circle distance in radians.
     */
    public double distanceTo(double aLatitude, double aLongitude, int units) {

        validateLatLon(aLatitude, aLongitude, units);

        if (units == INDEGREES) {
            aLatitude = Math.toRadians(aLatitude);
            aLongitude = Math.toRadians(aLongitude);
        }

        return GreatCircle(this.theLatitude, this.theLongitude, aLatitude,
                aLongitude);
    }

    /**
     * Compute the great circle distance (in radians) between this point and
     * another point located on a spherical surface.
     * 
     * @param toPoint
     *            A LatLongPoint
     * @return The great circle distance in radians.
     */
    public double distanceTo(LatLonPoint toPoint) {
        return GreatCircle(this.theLatitude, this.theLongitude, toPoint
                .getLatitude(), toPoint.getLongitude());
    }

    /**
     * Compute the initial bearing (in radians) to travel from point 1 to point
     * 2.
     * 
     * @param fromLat
     *            Latitude of first point.
     * @param fromLon
     *            Longitude of first point.
     * @param toLat
     *            Latitude of second point.
     * @param toLon
     *            Longitude of second point.
     * @return The bearing (from north in radians) to the second point.
     */
    public static double bearingTo(double fromLat, double fromLon,
            double toLat, double toLon) {
        // Compute the angular great circle distance between the points.
        double distance = GreatCircle(fromLat, fromLon, toLat, toLon);

        // numer computes the numerator for the bearing.
        double numer = Math.sin(toLat) - Math.sin(fromLat) * Math.cos(distance);

        double bearing = Math.acos(numer
                / (Math.cos(fromLat) * Math.sin(distance)));

        //
        // Adjust the final bearing so that it is in the range 0 <= b < (2 * PI)
        //
        // return ((Math.sin(toLon-fromLon) < 0.0) ? bearing
        // : (Math.PI * 2) - bearing);
        double temp = 0.0;

        if (Math.sin(toLon - fromLon) < 0.0) {
            temp = bearing;
        } else {
            temp = (Math.PI * 2) - bearing;
        }
        return temp;

    }

    /**
     * Compute the initial bearing (in radians) to travel from this point to a
     * second LatLonPoint.
     * 
     * @param toPoint
     *            The latitude/longitude of the second point.
     * @return The bearing (from north in radians) to the second point.
     */
    public double bearingTo(LatLonPoint toPoint) {
        return bearingTo(theLatitude, theLongitude, toPoint.getLatitude(),
                toPoint.getLongitude());
    }

    /**
     * Calculate the latitude/longitude of a point based on its bearing and
     * distance from a given latitude/longitude point.
     * 
     * @param fromLatitude
     *            the latitude (in Radians) of the reference point
     * @param fromLongitude
     *            the longitude (in Radians) of the reference point
     * @param aBearing
     *            the bearing from north in radians, of the point to calculate
     * @param aDistance
     *            the distance (in radians) from the reference point
     * @return The computed location.
     */
    public static LatLonPoint positionOf(double fromLatitude,
            double fromLongitude, double aBearing, double aDistance) {
        // Compute the latitude of the point
        double toLatitude = Math.asin(Math.sin(fromLatitude)
                * Math.cos(aDistance) + Math.cos(fromLatitude)
                * Math.sin(aDistance) * Math.cos(aBearing));

        // compute the longitude term for the final longitude.
        double numer = Math.sin(aBearing) * Math.sin(aDistance)
                * Math.cos(fromLatitude);

        double denom = Math.cos(aDistance) - Math.sin(fromLatitude)
                * Math.sin(toLatitude);

        double dlon = Math.atan2(numer, denom);

        //
        // Compute the longitude of the point. The mod function constrains the
        // longitude to the range {0 <= longitude < (2*PI)}
        //
        double toLongitude = mod(fromLongitude - dlon + Math.PI, 2 * Math.PI)
                - Math.PI;

        return (new LatLonPoint(toLatitude, toLongitude));
    }

    /**
     * Compute the latitude/longitude position of a point at a specified bearing
     * and distance from this point.
     * 
     * @param aBearing
     *            the bearing, from north in radians, of the point to calculate
     * @param aDistance
     *            the distance, in radians, of point to calculate from the
     *            reference point
     * @return The computed location.
     */
    public LatLonPoint positionOf(double aBearing, double aDistance) {
        return positionOf(theLatitude, theLongitude, aBearing, aDistance);
    }
}
