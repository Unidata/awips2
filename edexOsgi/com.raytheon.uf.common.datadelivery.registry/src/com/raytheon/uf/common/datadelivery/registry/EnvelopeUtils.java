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
package com.raytheon.uf.common.datadelivery.registry;

import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.projection.MapProjection;
import org.geotools.referencing.operation.projection.MapProjection.AbstractProvider;
import org.opengis.geometry.Envelope;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Utilities to make it easier to work with envelopes. Most of the methods
 * provide easy interoperability between envelopes and LatLon coordinates.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 5, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class EnvelopeUtils {

    /**
     * Envelope in LatLon CRS that defines the whole world
     */
    public static final ReferencedEnvelope WORLD_WIDE_LATLON = new ReferencedEnvelope(
            -180, 180, -90, 90, MapUtil.LATLON_PROJECTION);

    /**
     * Envelope representing the whole world defined from 0 to 360.
     */
    public static final ReferencedEnvelope WORLD_WIDE_EQ_CYLINDRICAL_CENTER180;

    static {
        ProjectedCRS crs = MapUtil.constructEquidistantCylindrical(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, 180, 0);
        // Best way to do this is to construct an envelope that covers half the
        // world and then double the width. Directly creating an envelope from 0
        // to 360 doesn't work because 0 and 360 normalize to the same point
        // which is an empty envelope.
        WORLD_WIDE_EQ_CYLINDRICAL_CENTER180 = createEnvelopeFromLatLon(crs,
                new Coordinate(-90, -90), new Coordinate(90, 90));
        WORLD_WIDE_EQ_CYLINDRICAL_CENTER180.expandBy(
                WORLD_WIDE_EQ_CYLINDRICAL_CENTER180.getWidth() / 2, 0);
    }

    /**
     * Get the upper left corner of envelope converted to the LatLon crs.
     * 
     * @param envelope
     * @return upper left corner of envelope in LatLon CRS.
     */
    public static Coordinate getUpperLeftLatLon(Envelope envelope) {
        try {
            DirectPosition2D ul = new DirectPosition2D(envelope.getMinimum(0),
                    envelope.getMaximum(1));
            MapUtil.getTransformToLatLon(
                    envelope.getCoordinateReferenceSystem()).transform(ul, ul);
            Coordinate latLon = new Coordinate(ul.x, ul.y);
            normalizeLongitude(envelope, latLon);
            return latLon;
        } catch (FactoryException e) {
            return new Coordinate(Double.NaN, Double.NaN);
        } catch (TransformException e) {
            return new Coordinate(Double.NaN, Double.NaN);
        }
    }

    /**
     * Get the upper right corner of envelope converted to the LatLon crs.
     * 
     * @param envelope
     * @return upper right corner of envelope in LatLon CRS.
     */
    public static Coordinate getUpperRightLatLon(Envelope envelope) {
        try {
            DirectPosition2D ur = new DirectPosition2D(envelope.getMaximum(0),
                    envelope.getMaximum(1));
            MapUtil.getTransformToLatLon(
                    envelope.getCoordinateReferenceSystem()).transform(ur, ur);
            Coordinate latLon = new Coordinate(ur.x, ur.y);
            normalizeLongitude(envelope, latLon);
            return latLon;
        } catch (FactoryException e) {
            return new Coordinate(Double.NaN, Double.NaN);
        } catch (TransformException e) {
            return new Coordinate(Double.NaN, Double.NaN);
        }
    }

    /**
     * Get the lower right corner of envelope converted to the LatLon crs.
     * 
     * @param envelope
     * @return lower right corner of envelope in LatLon CRS.
     */
    public static Coordinate getLowerRightLatLon(Envelope envelope) {
        try {
            DirectPosition2D lr = new DirectPosition2D(envelope.getMaximum(0),
                    envelope.getMinimum(1));
            MapUtil.getTransformToLatLon(
                    envelope.getCoordinateReferenceSystem()).transform(lr, lr);
            Coordinate latLon = new Coordinate(lr.x, lr.y);
            normalizeLongitude(envelope, latLon);
            return latLon;
        } catch (FactoryException e) {
            return new Coordinate(Double.NaN, Double.NaN);
        } catch (TransformException e) {
            return new Coordinate(Double.NaN, Double.NaN);
        }
    }

    /**
     * Get the lower left corner of envelope converted to the LatLon crs.
     * 
     * @param envelope
     * @return lower left corner of envelope in LatLon CRS.
     */
    public static Coordinate getLowerLeftLatLon(Envelope envelope) {
        try {
            DirectPosition2D ll = new DirectPosition2D(envelope.getMinimum(0),
                    envelope.getMinimum(1));
            MapUtil.getTransformToLatLon(
                    envelope.getCoordinateReferenceSystem()).transform(ll, ll);
            Coordinate latLon = new Coordinate(ll.x, ll.y);
            normalizeLongitude(envelope, latLon);
            return latLon;
        } catch (FactoryException e) {
            return new Coordinate(Double.NaN, Double.NaN);
        } catch (TransformException e) {
            return new Coordinate(Double.NaN, Double.NaN);
        }
    }

    /**
     * Get the center point of envelope converted to the LatLon crs.
     * 
     * @param envelope
     * @return center point of envelope in LatLon CRS.
     */
    public static Coordinate getCenterLatLon(Envelope envelope) {
        try {
            DirectPosition2D c = new DirectPosition2D(envelope.getMedian(0),
                    envelope.getMedian(1));
            MapUtil.getTransformToLatLon(
                    envelope.getCoordinateReferenceSystem()).transform(c, c);
            Coordinate latLon = new Coordinate(c.x, c.y);
            normalizeLongitude(envelope, latLon);
            return latLon;
        } catch (FactoryException e) {
            return new Coordinate(Double.NaN, Double.NaN);
        } catch (TransformException e) {
            return new Coordinate(Double.NaN, Double.NaN);
        }
    }

    /**
     * Convert the longitude value of a LatLon coordinate to be within 180
     * degrees of the central meridian of the envelope crs.
     * 
     * @param envelope
     * @param latLon
     * @return
     */
    private static Coordinate normalizeLongitude(Envelope envelope,
            Coordinate latLon) {
        MapProjection worldProjection = CRS.getMapProjection(envelope
                .getCoordinateReferenceSystem());
        double centralMeridian = 0.0;
        if (worldProjection != null) {
            ParameterValueGroup group = worldProjection.getParameterValues();
            centralMeridian = group.parameter(
                    AbstractProvider.CENTRAL_MERIDIAN.getName().getCode())
                    .doubleValue();
        }
        while (latLon.x < centralMeridian - 180.0) {
            latLon.x += 360.0;
        }
        while (latLon.x > centralMeridian + 180.0) {
            latLon.x -= 360.0;
        }
        return latLon;
    }

    /**
     * Determine if an envelope contains a latLon coordinate.
     * 
     * @param envelope
     * @param latLon
     * @return
     */
    public static boolean envelopeContainsLatLon(Envelope envelope,
            Coordinate latLon) {
        return envelopeContainsLatLon(envelope, latLon, 0.0);
    }

    /**
     * Determine if an envelope contains a latLon coordinate. This version
     * allows you to specify extra tolerance for points just outside the
     * envelope, which can be useful when dealing with inaccurate transforms
     * and/or rounded values.
     * 
     * @param envelope
     * @param latLon
     * @param tolerance
     *            percentage by which the height and width of the envelope are
     *            expanded before performing contains. 0 performs no expansion,
     *            1 will double the height and width.
     * @return
     */
    public static boolean envelopeContainsLatLon(Envelope envelope,
            Coordinate latLon, double tolerance) {
        ReferencedEnvelope rEnvelope = new ReferencedEnvelope(envelope);
        try {
            DirectPosition2D dp = new DirectPosition2D(latLon.x, latLon.y);
            MathTransform transform = MapUtil.getTransformFromLatLon(envelope
                    .getCoordinateReferenceSystem());
            transform.transform(dp, dp);
            if (rEnvelope.contains(dp)) {
                return true;
            } else {
                ReferencedEnvelope expandedEnvelope = new ReferencedEnvelope(
                        envelope);
                expandedEnvelope.expandBy(expandedEnvelope.getWidth()
                        * tolerance, expandedEnvelope.getHeight() * tolerance);
                return expandedEnvelope.contains(dp);
            }
        } catch (FactoryException e) {
            return false;
        } catch (TransformException e) {
            // assume all invalid coordinates are outside the valid crs space
            // so if the envelope is within valid crs space then it must not
            // contain the point.
            return false;
        }
    }

    /**
     * Create an envelope in the provided crs which contains latLon1 and
     * latLon2.
     * 
     * @param crs
     * @param latLon1
     * @param latLon2
     * @return
     */
    public static ReferencedEnvelope createEnvelopeFromLatLon(
            CoordinateReferenceSystem crs, Coordinate latLon1,
            Coordinate latLon2) {
        try {
            ReferencedEnvelope e = new ReferencedEnvelope(crs);

            DirectPosition2D dp1 = new DirectPosition2D(latLon1.x, latLon1.y);
            DirectPosition2D dp2 = new DirectPosition2D(latLon2.x, latLon2.y);
            MathTransform transform = MapUtil.getTransformFromLatLon(crs);
            try {
                transform.transform(dp1, dp1);
                e.expandToInclude(dp1.x, dp1.y);
            } catch (TransformException e1) {
                // latlon is not in valid area of crs so do not include it in
                // env.
            }
            try {
                transform.transform(dp2, dp2);
                e.expandToInclude(dp2.x, dp2.y);
            } catch (TransformException e1) {
                // latlon is not in valid area of crs so do not include it in
                // env.
            }
            return e;
        } catch (FactoryException e) {
            return new ReferencedEnvelope(crs);
        }
    }

    /**
     * performs same functionality as createEnvelopeFromLatLon except uses the
     * envelope crs and performs an intersection to guarantee that the return
     * value is completely within the envelope.
     * 
     * @param crs
     * @param latLon1
     * @param latLon2
     * @return
     */
    public static ReferencedEnvelope createSubenvelopeFromLatLon(
            Envelope envelope, Coordinate latLon1,
            Coordinate latLon2) {
        ReferencedEnvelope result = createEnvelopeFromLatLon(envelope.getCoordinateReferenceSystem(), latLon1, latLon2);
        return new ReferencedEnvelope(result.intersection(reference(envelope)), envelope.getCoordinateReferenceSystem());
    }

    /**
     * Create an envelope in the latLon crs that contains latLon1 and latLon2.
     * 
     * @param latLon1
     * @param latLon2
     * @return
     */
    public static ReferencedEnvelope createLatLonEnvelope(Coordinate latLon1,
            Coordinate latLon2) {
        ReferencedEnvelope e = new ReferencedEnvelope(MapUtil.LATLON_PROJECTION);
        e.expandToInclude(latLon1);
        e.expandToInclude(latLon2);
        return e;
    }

    /**
     * Helper method for converting an envelope into a ReferencedEnvelope.
     * 
     * @param envelope
     * @return
     */
    public static ReferencedEnvelope reference(Envelope envelope) {
        if (envelope instanceof ReferencedEnvelope) {
            return (ReferencedEnvelope) envelope;
        } else {
            return new ReferencedEnvelope(envelope);
        }
    }
}
