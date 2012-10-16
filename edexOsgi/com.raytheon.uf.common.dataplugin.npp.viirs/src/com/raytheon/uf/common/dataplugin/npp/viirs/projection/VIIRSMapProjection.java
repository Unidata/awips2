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
package com.raytheon.uf.common.dataplugin.npp.viirs.projection;

import static java.lang.Math.abs;
import static java.lang.Math.acos;
import static java.lang.Math.atan;
import static java.lang.Math.atan2;
import static java.lang.Math.cos;
import static java.lang.Math.sin;
import static java.lang.Math.sqrt;
import static java.lang.Math.tan;

import java.awt.geom.Point2D;
import java.util.Arrays;

import org.geotools.parameter.DefaultParameterDescriptor;
import org.geotools.parameter.DefaultParameterDescriptorGroup;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.operation.MathTransformProvider;
import org.geotools.referencing.operation.projection.MapProjection;
import org.geotools.referencing.operation.projection.ProjectionException;
import org.opengis.parameter.InvalidParameterNameException;
import org.opengis.parameter.InvalidParameterValueException;
import org.opengis.parameter.ParameterDescriptor;
import org.opengis.parameter.ParameterDescriptorGroup;
import org.opengis.parameter.ParameterNotFoundException;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineSegment;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2012            mschenke     Initial creation
 * Feb 21, 2012  #30       mschenke     Fixed world wrapping issues with projection
 *                                      calculations. Switched projection extrapolation
 *                                      algorithm to use great circle intersections for
 *                                      more accurate results
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSMapProjection extends MapProjection {

    private static final long serialVersionUID = -6994495607972402257L;

    public static final String VIIRS_MAP_PROJECTION_GROUP = "VIIRS_CENTER_MAPPING";

    public static final String PROJECTION_NAME = "NPP VIIRS";

    public static final String CENTER_LATITUDES = "center_lats";

    public static final String CENTER_LONGITUDES = "center_lons";

    public static final String DIRECTIONS = "directions";

    public static final String RESOLUTION = "resolution";

    public static final String CENTER_LENGTH = "center_length";

    public static final String SEMI_MAJOR = Provider.SEMI_MAJOR.getName()
            .getCode();

    public static final String SEMI_MINOR = Provider.SEMI_MINOR.getName()
            .getCode();

    private static final double DETIC2CENTRIC = 9.93305529e-1;

    private static final double DELTA = 6.73950125e-3;

    private static final double EQUITORIAL_RADIUS_KM = 6.3781370e+3;

    public static final double TWOPI = Math.PI * 2;

    public static final double PI = Math.PI;

    public static final double PIO2 = Math.PI / 2;

    private float[] centerLats;

    private float[] centerLons;

    private float[] directions;

    private double resolution;

    private int actualHeight;

    /**
     * @param values
     * @throws ParameterNotFoundException
     */
    protected VIIRSMapProjection(ParameterValueGroup values)
            throws ParameterNotFoundException {
        super(values);
        this.centerLats = Provider.getValue(Provider.CENTER_LATS, values);
        this.centerLons = Provider.getValue(Provider.CENTER_LONS, values);
        this.directions = Provider.getValue(Provider.CENTER_DIRECTIONS, values);
        this.resolution = Provider.getValue(Provider.RESOLUTION, values);
        this.actualHeight = Provider.getValue(Provider.CENTER_LENGTH, values);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.geotools.referencing.operation.projection.MapProjection#
     * getParameterDescriptors()
     */
    @Override
    public ParameterDescriptorGroup getParameterDescriptors() {
        return Provider.PARAMETERS;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.geotools.referencing.operation.projection.MapProjection#
     * inverseTransformNormalized(double, double, java.awt.geom.Point2D)
     */
    @Override
    protected Point2D inverseTransformNormalized(double x, double y,
            Point2D ptDst) throws ProjectionException {
        double xi = x / resolution;
        double yi = y / resolution;
        double lon = Math.toRadians(getInterpolatedValue(xi - 0.5, actualHeight
                - yi - 0.5, 0));
        double lat = Math.toRadians(getInterpolatedValue(xi - 0.5, actualHeight
                - yi - 0.5, 1));
        Point2D point = ptDst != null ? ptDst : new Point2D.Double();
        point.setLocation(lon, lat);
        return point;
    }

    protected float getInterpolatedValue(double xd, double yd, int dim) {
        float baseVal = Float.NaN;
        float value = 0.0f;
        float missing = 1.0f;

        float x = (float) xd;
        float y = (float) yd;

        int xi = (int) x;
        int yi = (int) y;
        // Upper left corner
        float xWeight = 1 - x + xi;
        float yWeight = 1 - y + yi;
        float weight = xWeight * yWeight;
        float val = getRawDataValue(xi, yi, dim);
        if (Float.isNaN(val)) {
            missing -= weight;
        } else {
            value += weight * val;
            baseVal = val;
        }
        // upper right corner
        xi = xi + 1;
        xWeight = 1 - xi + x;
        yWeight = 1 - y + yi;
        weight = xWeight * yWeight;
        val = getRawDataValue(xi, yi, dim);
        if (Float.isNaN(val)) {
            missing -= weight;
        } else {
            if (Float.isNaN(baseVal)) {
                baseVal = val;
            } else {
                val = correct(baseVal, val, dim);
            }
            value += weight * val;
        }
        // lower right corner
        yi = yi + 1;
        xWeight = 1 - xi + x;
        yWeight = 1 - yi + y;
        weight = xWeight * yWeight;
        val = getRawDataValue(xi, yi, dim);
        if (Float.isNaN(val)) {
            missing -= weight;
        } else {
            if (Float.isNaN(baseVal)) {
                baseVal = val;
            } else {
                val = correct(baseVal, val, dim);
            }
            value += weight * val;
        }
        // lower left corner
        xi = xi - 1;
        xWeight = 1 - x + xi;
        yWeight = 1 - yi + y;
        weight = xWeight * yWeight;
        val = getRawDataValue(xi, yi, dim);
        if (Float.isNaN(val)) {
            missing -= weight;
        } else {
            if (Float.isNaN(baseVal) == false) {
                val = correct(baseVal, val, dim);
            }
            value += weight * val;
        }

        value /= missing;
        if (dim == 0) {
            // If we did any normalization, reverse it here
            if (value > 180.0f) {
                value -= 360.0f;
            } else if (value <= -180.0f) {
                value += 360.0f;
            }
        }
        return value / missing;
    }

    private float correct(double baseVal, double val, int dim) {
        if (dim == 0 && Math.abs(baseVal - val) > 180.0f) {
            if (baseVal > val) {
                val += 360.0f;
            } else {
                val -= 360.0f;
            }
        }
        return (float) val;
    }

    /**
     * @param xi
     * @param yi
     * @param dim
     * @return
     */
    private float getRawDataValue(int xi, int yi, int dim) {
        Coordinate c = null;
        if (yi >= 0 && yi < centerLats.length) {
            c = index(xi, yi);
        } else {
            int closestY = yi;
            int closestY2;
            if (closestY < 0) {
                closestY = 0;
                closestY2 = closestY + 1;
            } else {
                closestY = centerLats.length - 1;
                closestY2 = closestY - 1;
            }

            int yDiff = closestY - yi;

            Coordinate a = index(xi, closestY);
            Coordinate b = index(xi, closestY2);
            LineSegment ls = new LineSegment(a, b);
            c = ls.pointAlong(-Math.abs(yDiff));

        }
        return (float) (dim == 0 ? c.x : c.y);
    }

    private Coordinate index(int xi, int yi) {
        Coordinate nadirCoord = new Coordinate(centerLons[yi], centerLats[yi]);
        double az = directions[yi];
        if (xi < 0) {
            az += PIO2;
        } else {
            az -= PIO2;
        }

        double latitude = Math.toRadians(nadirCoord.y);
        double earthRadius = getRadius(latitude);
        double[] latLon = target_pt(latitude, Math.toRadians(nadirCoord.x),
                (resolution * abs(xi)) / earthRadius, az);
        return new Coordinate(latLon[0], latLon[1]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.geotools.referencing.operation.projection.MapProjection#
     * transformNormalized(double, double, java.awt.geom.Point2D)
     */
    @Override
    protected Point2D transformNormalized(double aLonR, double aLatR,
            Point2D ptDst) throws ProjectionException {
        double bestY = Double.NaN, bestX = 1;

        Coordinate c1 = new Coordinate(Math.toRadians(centerLons[0]),
                Math.toRadians(centerLats[0]));
        Coordinate c2 = new Coordinate(Math.toRadians(centerLons[1]),
                Math.toRadians(centerLats[1]));

        // Extrapolate from the bottom of the image
        double[] bottom = extrapolate(c1, c2, aLonR, aLatR);

        c1 = new Coordinate(Math.toRadians(centerLons[centerLons.length - 2]),
                Math.toRadians(centerLats[centerLats.length - 2]));
        c2 = new Coordinate(Math.toRadians(centerLons[centerLons.length - 1]),
                Math.toRadians(centerLats[centerLats.length - 1]));

        // Extrapolate from the top of the image
        double[] top = extrapolate(c1, c2, aLonR, aLatR);

        double dist1 = bottom[0], potentialY1;
        double angleDiff1 = bottom[1];
        double potentialX1 = bottom[2];

        // Check to see if intersection point used is going same direction
        // from c1 to c2 or if opposite
        if (angleDiff1 > 90.0) {
            potentialY1 = actualHeight + dist1;
        } else {
            potentialY1 = actualHeight - dist1;
        }

        double dist2 = top[0], potentialY2;
        double angleDiff2 = top[1];
        double potentialX2 = top[2];

        // Check to see if intersection point used is going same direction
        // from c1 to c2 or if opposite
        if (angleDiff2 > 90.0) {
            potentialY2 = actualHeight + dist2 - centerLons.length - 2;
        } else {
            potentialY2 = actualHeight - dist2 - centerLons.length - 2;
        }

        int validHeight = centerLats.length;
        if ((potentialY1 >= 0 && potentialY1 < validHeight)
                || (potentialY2 >= 0 && potentialY2 < validHeight)) {
            potentialY1 = validHeight - potentialY1;
            potentialY2 = validHeight - potentialY2;
            int startI = (int) Math.floor(Math.min(potentialY1, potentialY2));
            if (startI < 0) {
                startI = 0;
            }
            int endI = (int) Math.ceil(Math.max(potentialY1, potentialY2));
            if (endI >= validHeight) {
                endI = validHeight - 1;
            }

            // Lat/Lon to CRS, within our known area
            double alat = Math.toDegrees(aLatR);
            double alon = Math.toDegrees(aLonR);
            double bestDiff = Double.NaN;
            double bestDist = Double.NaN;
            double bestFactor = 1;
            for (int i = startI; i <= endI; ++i) {
                double clon = centerLons[i];
                double clat = centerLats[i];

                double[] azm_sidb = azm_sidb(Math.toRadians(clat),
                        Math.toRadians(clon), aLatR, aLonR);
                double actual = azm_sidb[0];
                double expected = directions[i];
                // Correct for world wrapping
                if (Math.abs(actual - expected) > Math.PI) {
                    if (actual > expected) {
                        expected += TWOPI;
                    } else {
                        expected -= TWOPI;
                    }
                }
                double factor = 1;
                if (actual < expected) {
                    expected -= PIO2;
                } else {
                    expected += PIO2;
                    factor = -1;
                }
                double diff = Math.abs(actual - expected);
                double dist = azm_sidb[1] * getRadius(Math.toRadians(clat));
                if (Double.isNaN(bestDiff) || diff <= bestDiff) {
                    bestFactor = factor;
                    bestDist = dist;
                    bestDiff = diff;
                    bestY = actualHeight - i - 0.5;
                }
            }

            // We found a point which meets our threshold criteria, this means
            // that the point is somewhere between our valid Y range
            bestDist = Math.cos(bestDiff) * bestDist;
            bestX = 0.5 + (bestFactor * (bestDist / resolution));

            int yIdx = (int) bestY;
            int otherYIdx;
            if (yIdx < centerLats.length - 1) {
                otherYIdx = yIdx + 1;
            } else {
                otherYIdx = yIdx - 1;
            }

            try {
                Point2D latLon = new Point2D.Double(), otherLatLon = new Point2D.Double();
                inverse().transform(
                        new Point2D.Double(bestX * resolution, (yIdx + 0.5)
                                * resolution), latLon);
                inverse().transform(
                        new Point2D.Double(bestX * resolution,
                                (otherYIdx + 0.5) * resolution), otherLatLon);

                Coordinate a = new Coordinate(latLon.getX(), latLon.getY());
                Coordinate b = new Coordinate(otherLatLon.getX(),
                        otherLatLon.getY());

                // Correct for world wrapping a, b and alon
                if (Math.abs(a.x - b.x) > 180.0) {
                    if (a.x > b.x) {
                        b.x += 360.0;
                    } else {
                        b.x -= 360.0;
                    }
                }
                if (Math.abs(a.x - alon) > 180.0) {
                    if (a.x > alon) {
                        alon += 360.0;
                    } else {
                        alon -= 360.0;
                    }
                }

                LineSegment ls = new LineSegment(a, b);
                if (yIdx < centerLats.length - 1) {
                    bestY += ls.projectionFactor(new Coordinate(alon, alat));
                } else {
                    bestY -= ls.projectionFactor(new Coordinate(alon, alat));
                }
            } catch (TransformException e) {
                return null;
            }
        } else {
            // Outside range, use closest distance for values
            if (dist1 < dist2) {
                bestX = potentialX1;
                bestY = potentialY1;
            } else {
                bestX = potentialX2;
                bestY = potentialY2;
            }
        }
        Point2D point = ptDst != null ? ptDst : new Point2D.Double();
        point.setLocation(bestX * resolution, bestY * resolution);
        return point;
    }

    /**
     * Given two coordinates (radians), intersect the great circle for those two
     * coordinates with the great circle perpendicular that goes through point
     * (aLonR, aLatR)
     * 
     * @param c1
     * @param c2
     * @param aLonR
     * @param aLatR
     * @return the minimum dist value to c1 [0], the angle difference of the
     *         intersecting point and c1-c2 angle [1] and the x value [2]
     */
    private double[] extrapolate(Coordinate c1, Coordinate c2, double aLonR,
            double aLatR) {
        // Create GeodeticCalculator for calculations...
        GeodeticCalculator gc = new GeodeticCalculator();

        // Set start/end points as c1/c2
        gc.setStartingGeographicPoint(Math.toDegrees(c1.x),
                Math.toDegrees(c1.y));
        gc.setDestinationGeographicPoint(Math.toDegrees(c2.x),
                Math.toDegrees(c2.y));

        // Get the angle of
        double c12Dir = gc.getAzimuth();
        double c12Dist = gc.getOrthodromicDistance();

        // Set starting point (c1) and calculate another point on the great
        // circle which we will call (c3)
        gc.setStartingGeographicPoint(Math.toDegrees(c1.x),
                Math.toDegrees(c1.y));
        gc.setDirection(c12Dir, 2 * c12Dist);
        Point2D dest = gc.getDestinationGeographicPoint();
        Coordinate c3 = new Coordinate(Math.toRadians(dest.getX()),
                Math.toRadians(dest.getY()));

        // Get angle from c2 to c3
        gc.setStartingGeographicPoint(Math.toDegrees(c2.x),
                Math.toDegrees(c2.y));
        gc.setDestinationGeographicPoint(Math.toDegrees(c3.x),
                Math.toDegrees(c3.y));
        double c23Dir = gc.getAzimuth();
        double c23Dist = gc.getOrthodromicDistance();

        // Get point perpendicular to c1 (c1_90)
        gc.setStartingGeographicPoint(Math.toDegrees(c1.x),
                Math.toDegrees(c1.y));
        gc.setDirection(adjustAngle(c12Dir + 90.0), c12Dist);
        dest = gc.getDestinationGeographicPoint();
        Coordinate c1_90 = new Coordinate(Math.toRadians(dest.getX()),
                Math.toRadians(dest.getY()));

        // Get point perpendicular to c2 (c2_90)
        gc.setStartingGeographicPoint(Math.toDegrees(c2.x),
                Math.toDegrees(c2.y));
        gc.setDirection(adjustAngle(c23Dir + 90.0), c23Dist);
        dest = gc.getDestinationGeographicPoint();
        Coordinate c2_90 = new Coordinate(Math.toRadians(dest.getX()),
                Math.toRadians(dest.getY()));

        // Find intersecting point from c1-c1_90 and c2-c2_90, this will give us
        // one of our pole locations
        Coordinate interA = findIntersection(c1, c1_90, c2, c2_90);
        // Now intersect great circle of c1-c2 and pole-(aLonR,aLatR). This will
        // give us closest/farthest intersecting point
        interA = findIntersection(c1, c2, interA, new Coordinate(aLonR, aLatR));
        Coordinate interB = new Coordinate(Math.toRadians(adjustAngle(Math
                .toDegrees(interA.x + PI))), -interA.y);

        // Next we are going to get the angle and distance of interA and interB
        // from point c1 to make sure we use the closest point
        double radius = getRadius(c1.y);
        double[] azm_sidb = azm_sidb(c1.y, c1.x, interA.y, interA.x);
        double azimuthA = azm_sidb[0];
        double distA = (azm_sidb[1] * radius) / resolution;

        azm_sidb = azm_sidb(c1.y, c1.x, interB.y, interB.x);
        double azimuthB = azm_sidb[0];
        double distB = (azm_sidb[1] * radius) / resolution;

        // Now we use the closest intersection point to c1
        double azimuth, dist;
        Coordinate inter;
        if (distA < distB) {
            // Closest point is A
            azimuth = azimuthA;
            dist = distA;
            inter = interA;
        } else {
            // Closest point is B
            azimuth = azimuthB;
            dist = distB;
            inter = interB;
        }

        // Get angle and distance from intersection point used to initial
        // point aLonR, aLatR for bestX value
        azm_sidb = azm_sidb(inter.y, inter.x, aLatR, aLonR);
        // Convert side_b into projection space
        double bestX = (azm_sidb[1] * getRadius(inter.y)) / resolution;

        // Figure if bestX is negative or positive
        double actual = Math.toRadians(c12Dir);
        double expected = azm_sidb[0];
        // Correct for world wrapping
        if (Math.abs(actual - expected) > Math.PI) {
            if (actual > expected) {
                expected += TWOPI;
            } else {
                expected -= TWOPI;
            }
        }
        if (actual < expected) {
            bestX = -bestX;
        }

        // Return {y distance in projection space, difference between angle of
        // closest intersecting point and c1 and c1-c2, and our x value in
        // projection space}
        return new double[] { dist,
                Math.abs(adjustAngle(Math.toDegrees(azimuth)) - c12Dir), bestX };
    }

    /**
     * Find the first intersecting point using great circle algorithm for the
     * two great circles defined by c1-c2 and c3-c4. Other intersecting point
     * will be exactly 180 degrees and -lat on the other side of the world
     * 
     * @param c1
     * @param c2
     * @param c3
     * @param c4
     * @return
     */
    private static Coordinate findIntersection(Coordinate c1, Coordinate c2,
            Coordinate c3, Coordinate c4) {
        // Find intersecting points using great circle algorithm...
        Coordinate e1Xe2 = correctiveCrossProduct(c1, c2);
        Coordinate e3Xe4 = correctiveCrossProduct(c3, c4);

        Coordinate ea = normalize(e1Xe2);
        Coordinate eb = normalize(e3Xe4);

        Coordinate eaXeb = crossProduct(ea, eb);
        return new Coordinate(atan2(-eaXeb.y, eaXeb.x), atan2(eaXeb.z,
                sqrt(eaXeb.x * eaXeb.x + eaXeb.y * eaXeb.y)));
    }

    /**
     * Adjusts an angle to ensure it is between -180/180
     * 
     * @param angle
     * @return
     */
    private static double adjustAngle(double angle) {
        double newVal = angle % 360;
        if (newVal > 180) {
            newVal -= 360;
        } else if (newVal < -180) {
            newVal += 360;
        }
        return newVal;
    }

    private static Coordinate correctiveCrossProduct(Coordinate c1,
            Coordinate c2) {
        Coordinate e1Xe2 = new Coordinate();
        double lat1 = c1.y;
        double lat2 = c2.y;
        double lon1 = c1.x;
        double lon2 = c2.x;
        e1Xe2.x = sin(lat1 - lat2) * sin((lon1 + lon2) / 2)
                * cos((lon1 - lon2) / 2) - sin(lat1 + lat2)
                * cos((lon1 + lon2) / 2) * sin((lon1 - lon2) / 2);
        e1Xe2.y = sin(lat1 - lat2) * cos((lon1 + lon2) / 2)
                * cos((lon1 - lon2) / 2) + sin(lat1 + lat2)
                * sin((lon1 + lon2) / 2) * sin((lon1 - lon2) / 2);
        e1Xe2.z = cos(lat1) * cos(lat2) * sin(lon1 - lon2);
        return e1Xe2;
    }

    private static Coordinate crossProduct(Coordinate e1, Coordinate e2) {
        Coordinate e1Xe2 = new Coordinate();
        e1Xe2.x = e1.y * e2.z - e2.y * e1.z;
        e1Xe2.y = e1.z * e2.x - e2.z * e1.x;
        e1Xe2.z = e1.x * e2.y - e1.y * e2.x;
        return e1Xe2;
    }

    /**
     * @param e1Xe2
     * @param e1
     * @param e2
     * @return
     */
    private static Coordinate normalize(Coordinate e1Xe2) {
        Coordinate ea = new Coordinate();
        double length = Math.sqrt(e1Xe2.x * e1Xe2.x + e1Xe2.y * e1Xe2.y
                + e1Xe2.z * e1Xe2.z);
        ea.x = e1Xe2.x / length;
        ea.y = e1Xe2.y / length;
        ea.z = e1Xe2.z / length;
        return ea;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + actualHeight;
        result = prime * result + Arrays.hashCode(directions);
        result = prime * result + Arrays.hashCode(centerLats);
        result = prime * result + Arrays.hashCode(centerLons);
        long temp;
        temp = Double.doubleToLongBits(resolution);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        VIIRSMapProjection other = (VIIRSMapProjection) obj;
        if (actualHeight != other.actualHeight)
            return false;
        if (!Arrays.equals(directions, other.directions))
            return false;
        if (!Arrays.equals(centerLats, other.centerLats))
            return false;
        if (!Arrays.equals(centerLons, other.centerLons))
            return false;
        if (Double.doubleToLongBits(resolution) != Double
                .doubleToLongBits(other.resolution))
            return false;
        return true;
    }

    public static double[] target_pt(double clat, double clon, double side_b,
            double azimuth) {
        double angle_B; /* angle at the north pole */
        double angle_C; /* angle C of the triangle */
        double angle_XY; /* angle used when distance is small */
        double small_b = 2.0e-4; /* definition of a small distance */
        double side_a; /* side a of the triangle */
        double side_c; /* side c of the triangle */
        double val_trig; /*
                          * variable containing a cosine for a trig function
                          */
        double xcoor; /* X coordinate */
        double ycoor; /* Y coordinate */
        double NPlat = 1.553343e0; /* 89 North, in radians */
        double SPlat = -1.553343e0; /* -89 South, in radians */
        double x1, y1, x2, y2; /* cartesian coordinates */
        double delta_x, delta_y; /* change in x,y coordinates */
        double radius, rlon; /* polar coordinates */
        double ajd_direc; /* adjusted direction */

        double alat, alon; // rval;

        if (azimuth > PI) {
            angle_C = azimuth - TWOPI;
        } else {
            angle_C = azimuth;
        }

        if (clat > NPlat) {
            radius = PIO2 - clat;
            if (radius < 0.e0)
                radius = 0.e0;
            x1 = radius * cos(clon);
            y1 = radius * sin(clon);
            ajd_direc = (PI - azimuth) + clon;
            if (ajd_direc < -PI)
                ajd_direc = ajd_direc + TWOPI;
            if (ajd_direc > PI)
                ajd_direc = ajd_direc - TWOPI;
            delta_x = side_b * cos(ajd_direc);
            delta_y = side_b * sin(ajd_direc);
            x2 = x1 + delta_x;
            y2 = y1 + delta_y;
            radius = sqrt(x2 * x2 + y2 * y2);
            alat = PIO2 - radius;
            alon = atan2(y2, x2);
        } else if (clat < SPlat) {
            radius = PIO2 + clat;
            rlon = -clon;
            x1 = radius * cos(rlon);
            y1 = radius * sin(rlon);
            ajd_direc = -azimuth - clon;
            if (ajd_direc < -PI)
                ajd_direc = ajd_direc + TWOPI;
            if (ajd_direc > PI)
                ajd_direc = ajd_direc - TWOPI;
            delta_x = side_b * cos(ajd_direc);
            delta_y = side_b * sin(ajd_direc);
            x2 = x1 + delta_x;
            y2 = y1 + delta_y;
            radius = sqrt(x2 * x2 + y2 * y2);
            alat = radius - PIO2;
            alon = -atan2(y2, x2);
        } else if (side_b < small_b) {
            angle_XY = PIO2 - azimuth;
            if (angle_XY < -PI) {
                angle_XY = angle_XY + TWOPI;
            }
            xcoor = side_b * cos(angle_XY);
            ycoor = side_b * sin(angle_XY);
            xcoor = xcoor / cos(clat);
            alat = clat + ycoor;
            alon = clon + xcoor;
            if (alon > PI)
                alon = alon - TWOPI;
            if (alon < -PI)
                alon = alon + TWOPI;
        } else if (abs(angle_C) < 1.e-5) {
            alon = clon;
            alat = clat + side_b;
            if (alat > PIO2) {
                alon = alon + PI;
                if (alon > PI)
                    alon = alon - TWOPI;
                alat = PI - alat;
            }
        } else if (abs(azimuth - PI) < 1.e-5) {
            alon = clon;
            alat = clat - side_b;
            if (alat < -PIO2) {
                alon = alon + PI;
                if (alon > PI)
                    alon = alon - TWOPI;
                alat = -(PI + alat);
            }
        } else {
            side_a = PIO2 - clat;
            val_trig = (cos(side_a) * cos(side_b))
                    + (sin(side_a) * sin(side_b) * cos(angle_C));
            if (val_trig > 1.e0) {
                side_c = 0.e0;
            } else if (val_trig < -1.e0) {
                side_c = PI;
            } else {
                side_c = acos(val_trig);
            }

            alat = PIO2 - side_c;
            if (alat < -PIO2)
                alat = -PIO2;

            if ((side_c < 1.e-6) || (side_c == PI)) {
                angle_B = 0.e0;
            } else {
                val_trig = (cos(side_b) - (cos(side_c) * cos(side_a)))
                        / (sin(side_c) * sin(side_a));
                if (val_trig > 1.e0) {
                    angle_B = 0.e0;
                } else if (val_trig < -1.e0) {
                    angle_B = PI;
                } else {
                    angle_B = acos(val_trig);
                }
            }

            if (angle_C > 0.e0) {
                alon = clon + angle_B;
                if (alon > PI)
                    alon = alon - TWOPI;
            } else {
                alon = clon - angle_B;
                if (alon < -PI)
                    alon = alon + TWOPI;
            }
        }

        return new double[] { Math.toDegrees(alon), Math.toDegrees(alat) };
    }

    public static double getRadius(double latitude) {
        return 1000.0 * earth_radius_D(latitude);
    }

    public static double earth_radius_D(double rlat) {
        double sine_lat, clat;
        clat = atan(DETIC2CENTRIC * tan(rlat));
        sine_lat = sin(clat);
        return EQUITORIAL_RADIUS_KM
                / sqrt(1.e0 + (DELTA * sine_lat * sine_lat));
    }

    public static double[] azm_sidb(double clat, double clon, double alat,
            double alon) {
        double small_b = 6.0e-4; /* small distance algorithm option */
        double angle_B; /* Angle B of the triangle */
        double angle_C; /* Angle C of the triangle */
        double cos_angle_C; /* cosine of Angle C */
        double angle_XY; /* angle XY is used for short distances */
        double side_a; /* side a of the triangle */
        double side_c; /* side c of the triangle */
        double xcoor; /* X coordinate for small distances */
        double ycoor; /* Y coordinate for small distances */

        double azimuth, side_b; // return values;

        xcoor = alon - clon;
        if (xcoor > PI)
            xcoor = xcoor - TWOPI;
        if (xcoor < -PI)
            xcoor = xcoor + TWOPI;
        xcoor = xcoor * cos((alat + clat) / 2.e0);
        ycoor = alat - clat;
        side_b = sqrt(xcoor * xcoor + ycoor * ycoor);

        if (side_b < small_b) {
            if (side_b < -1.e-5) {
                azimuth = 0.e0;
            } else {
                if (ycoor >= 0.e+0) {
                    angle_XY = acos(xcoor / side_b);
                } else {
                    angle_XY = -acos(xcoor / side_b);
                }
                azimuth = PIO2 - angle_XY;
                if (azimuth < 0.e0) {
                    azimuth = azimuth + TWOPI;
                }
            }
        } else {
            side_a = PIO2 - clat;
            side_c = PIO2 - alat;
            angle_B = alon - clon;
            if (angle_B > PI)
                angle_B = angle_B - TWOPI;
            if (angle_B < -PI)
                angle_B = angle_B + TWOPI;

            side_b = acos((cos(side_c) * cos(side_a))
                    + (sin(side_c) * sin(side_a) * cos(angle_B)));
            cos_angle_C = (cos(side_c) - (cos(side_a) * cos(side_b)))
                    / (sin(side_a) * sin(side_b));
            if (cos_angle_C < -9.9999999e-1) {
                angle_C = PI;
            } else if (cos_angle_C > 9.9999999e-1) {
                angle_C = 0.e0;
            } else {
                angle_C = acos(cos_angle_C);
            }

            if (abs(angle_C) < 1.e-5) {
                azimuth = 0.e0;
            } else if (angle_B < 0.e0) {
                azimuth = TWOPI - angle_C;
            } else {
                azimuth = angle_C;
            }
        }
        return new double[] { azimuth, side_b };
    }

    public static class Provider extends AbstractProvider {

        private static final long serialVersionUID = 1266944341235566642L;

        static final ParameterDescriptor<float[]> CENTER_LATS = DefaultParameterDescriptor
                .create(VIIRSMapProjection.CENTER_LATITUDES,
                        "Latitude locations of center points", float[].class,
                        null, true);

        static final ParameterDescriptor<float[]> CENTER_LONS = DefaultParameterDescriptor
                .create(VIIRSMapProjection.CENTER_LONGITUDES,
                        "Longitude locations of center points", float[].class,
                        null, true);

        static final ParameterDescriptor<float[]> CENTER_DIRECTIONS = DefaultParameterDescriptor
                .create(VIIRSMapProjection.DIRECTIONS,
                        "Direction track is moving at each center lat/lon point in radians",
                        float[].class, null, true);

        static final ParameterDescriptor<Double> RESOLUTION = DefaultParameterDescriptor
                .create(VIIRSMapProjection.RESOLUTION,
                        "Spacing of cells in meters", Double.class, 750.0, true);

        static final ParameterDescriptor<Integer> CENTER_LENGTH = DefaultParameterDescriptor
                .create(VIIRSMapProjection.CENTER_LENGTH,
                        "Full size of center data", Integer.class, 0, true);

        static final ParameterDescriptorGroup PARAMETERS = new DefaultParameterDescriptorGroup(
                VIIRS_MAP_PROJECTION_GROUP, new ParameterDescriptor[] {
                        CENTER_LATS, CENTER_LONS, CENTER_DIRECTIONS,
                        CENTER_LENGTH, RESOLUTION, SEMI_MAJOR, SEMI_MINOR,
                        CENTRAL_MERIDIAN });

        public Provider() {
            super(PARAMETERS);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.geotools.referencing.operation.MathTransformProvider#
         * createMathTransform (org.opengis.parameter.ParameterValueGroup)
         */
        @Override
        protected MathTransform createMathTransform(ParameterValueGroup values)
                throws InvalidParameterNameException,
                ParameterNotFoundException, InvalidParameterValueException,
                FactoryException {
            return new VIIRSMapProjection(values);
        }

        static <T> T getValue(ParameterDescriptor<T> descriptor,
                ParameterValueGroup group) {
            return MathTransformProvider.value(descriptor, group);
        }
    }

}
