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

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineSegment;

/**
 * VIIRS map projection
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
 * Aug 27, 2013  #2190     mschenke     Sped up transform functions
 * Mar 27, 2014  #2015     njensen      Overrode getParameterValues()
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

    private final float[] centerLats;

    private final float[] centerLons;

    private final float[] directions;

    private final double resolution;

    private final int validHeight;

    private final int actualHeight;

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
        if (centerLats.length != centerLons.length
                || centerLats.length != directions.length) {
            throw new IllegalArgumentException(
                    "Center lat/lon and direction arrays must be same length");
        }
        this.validHeight = centerLats.length;
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

    @Override
    public ParameterValueGroup getParameterValues() {
        final ParameterDescriptorGroup descriptor = getParameterDescriptors();
        final ParameterValueGroup values = descriptor.createValue();
        values.parameter(CENTER_LATITUDES).setValue(centerLats);
        values.parameter(CENTER_LONGITUDES).setValue(centerLons);
        values.parameter(DIRECTIONS).setValue(directions);
        values.parameter(CENTER_LENGTH).setValue(actualHeight);
        values.parameter(RESOLUTION).setValue(resolution);
        values.parameter(SEMI_MAJOR).setValue(
                VIIRSMapProjectionFactory.SEMI_MINOR_MAJOR_VALUE);
        values.parameter(SEMI_MINOR).setValue(
                VIIRSMapProjectionFactory.SEMI_MINOR_MAJOR_VALUE);
        values.parameter(Provider.CENTRAL_MERIDIAN.getName().getCode())
                .setValue(Provider.CENTRAL_MERIDIAN.getDefaultValue());
        return values;
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
        Coordinate latLon = getInterpolatedCoordinate(xi - 0.5, actualHeight
                - yi - 0.5);

        Point2D point = ptDst != null ? ptDst : new Point2D.Double();
        point.setLocation(Math.toRadians(latLon.x), Math.toRadians(latLon.y));
        return point;
    }

    /**
     * @param d
     * @param e
     * @return
     */
    private Coordinate getInterpolatedCoordinate(double xd, double yd) {
        double baseLon = Double.NaN;
        float latValue = 0.0f;
        float lonValue = 0.0f;
        float latMissing = 1.0f;
        float lonMissing = 1.0f;

        float x = (float) xd;
        float y = (float) yd;

        int xi = (int) xd;
        int yi = (int) yd;

        // Upper left corner
        float xWeight = 1 - x + xi;
        float yWeight = 1 - y + yi;
        float weight = xWeight * yWeight;
        Coordinate c = getRawDataCoordinate(xi, yi);
        if (Double.isNaN(c.x)) {
            lonMissing -= weight;
        } else {
            lonValue += weight * c.x;
            baseLon = c.x;
        }
        if (Double.isNaN(c.y)) {
            latMissing -= weight;
        } else {
            latValue += weight * c.y;
        }

        // upper right corner
        xi = xi + 1;
        xWeight = 1 - xi + x;
        yWeight = 1 - y + yi;
        weight = xWeight * yWeight;
        c = getRawDataCoordinate(xi, yi);
        if (Double.isNaN(c.x)) {
            lonMissing -= weight;
        } else {
            if (Double.isNaN(baseLon)) {
                baseLon = c.x;
            } else {
                c.x = correct(baseLon, c.x);
            }
            lonValue += weight * c.x;
        }
        if (Double.isNaN(c.y)) {
            latMissing -= weight;
        } else {
            latValue += weight * c.y;
        }

        // lower right corner
        yi = yi + 1;
        xWeight = 1 - xi + x;
        yWeight = 1 - yi + y;
        weight = xWeight * yWeight;
        c = getRawDataCoordinate(xi, yi);
        if (Double.isNaN(c.x)) {
            lonMissing -= weight;
        } else {
            if (Double.isNaN(baseLon)) {
                baseLon = c.x;
            } else {
                c.x = correct(baseLon, c.x);
            }
            lonValue += weight * c.x;
        }
        if (Double.isNaN(c.y)) {
            latMissing -= weight;
        } else {
            latValue += weight * c.y;
        }

        // lower left corner
        xi = xi - 1;
        xWeight = 1 - x + xi;
        yWeight = 1 - yi + y;
        weight = xWeight * yWeight;
        c = getRawDataCoordinate(xi, yi);
        if (Double.isNaN(c.x)) {
            lonMissing -= weight;
        } else {
            if (Double.isNaN(baseLon) == false) {
                c.x = correct(baseLon, c.x);
            }
            lonValue += weight * c.x;
        }
        if (Double.isNaN(c.y)) {
            latMissing -= weight;
        } else {
            latValue += weight * c.y;
        }

        lonValue /= lonMissing;
        latValue /= latMissing;

        // If we did any normalization, reverse it here
        if (lonValue > 180.0f) {
            lonValue -= 360.0f;
        } else if (lonValue <= -180.0f) {
            lonValue += 360.0f;
        }

        return new Coordinate(lonValue, latValue);
    }

    private double correct(double baseLon, double lon) {
        if (Math.abs(baseLon - lon) > 180.0f) {
            if (baseLon > lon) {
                lon += 360.0f;
            } else {
                lon -= 360.0f;
            }
        }
        return lon;
    }

    /**
     * @param xi
     * @param yi
     * @return
     */
    private Coordinate getRawDataCoordinate(int xi, int yi) {
        Coordinate c = null;
        if (yi >= 0 && yi < validHeight) {
            c = index(xi, yi);
        } else {
            int closestY = yi;
            int closestY2;
            if (closestY < 0) {
                closestY = 0;
                closestY2 = closestY + 1;
            } else {
                closestY = validHeight - 1;
                closestY2 = closestY - 1;
            }

            int yDiff = closestY - yi;

            Coordinate a = index(xi, closestY);
            Coordinate b = index(xi, closestY2);
            LineSegment ls = new LineSegment(a, b);
            c = ls.pointAlong(-Math.abs(yDiff));
        }
        return c;
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
        return new Coordinate(Math.toDegrees(latLon[0]),
                Math.toDegrees(latLon[1]));
    }

    private static class DistanceResult {

        private final int index;

        private final double xDist;

        private final double yDist;

        public DistanceResult(int index, double xDist, double yDist) {
            this.index = index;
            this.xDist = xDist;
            this.yDist = yDist;
        }

    }

    private DistanceResult distance(int index, double aLonR, double aLatR) {
        double clon = centerLons[index];
        double clat = centerLats[index];

        // Compute azm_sidb for center point to lat/lon point
        double[] azm_sidb = azm_sidb(Math.toRadians(clat),
                Math.toRadians(clon), aLatR, aLonR);

        double actual = azm_sidb[0];
        double expected = directions[index];

        // Correct for world wrapping
        if (Math.abs(actual - expected) > Math.PI) {
            if (actual > expected) {
                expected += TWOPI;
            } else {
                expected -= TWOPI;
            }
        }
        double xFactor = 1;
        double yFactor = 1;
        if (actual < expected) {
            expected -= PIO2;
            yFactor = -1;
        } else {
            expected += PIO2;
            xFactor = -1;
        }

        // Get angle difference between actual and expected as well as
        // multiplication factor for whether left or right of center points
        double diff = actual - expected;

        // Because diff is always [0,90], we must account for what quadrant it
        // is in and adjust sign appropriately based on factor
        double yDist = yFactor * azm_sidb[1] * Math.sin(diff);
        double xDist = xFactor * azm_sidb[1] * Math.cos(diff);

        return new DistanceResult(index, xDist, yDist);
    }

    /**
     * Intelligently searches for the best index for the given lat/lon in the
     * range of startIdx->endIdx
     * 
     * @param startIdx
     * @param endIdx
     * @param aLonR
     * @param aLatR
     * @return
     */
    private DistanceResult findBest(int startIdx, int endIdx, double aLonR,
            double aLatR) {
        // Get search results for start/end idx
        DistanceResult below = distance(startIdx, aLonR, aLatR);
        DistanceResult above = distance(endIdx, aLonR, aLatR);
        DistanceResult best = null;
        boolean done = false;
        do {
            double aboveDist = above.yDist;
            double belowDist = below.yDist;
            int aboveIndex = above.index;
            int belowIndex = below.index;

            // get range of y distances
            double range = aboveDist - belowDist;
            // Calculate best index to search when looking for yDist of 0
            double testIdx = belowIndex + (-belowDist / range)
                    * (aboveIndex - belowIndex);
            // Ensure index within search bounds
            int index = (int) testIdx;
            if (index < belowIndex) {
                index = belowIndex;
            } else if (index > aboveIndex) {
                index = aboveIndex;
            }
            // Get other index to check which will be 1 greater than index due
            // to casting of testIdx
            int otherIndex = index + 1;
            if (otherIndex >= validHeight) {
                otherIndex = validHeight - 1;
            }
            if (index == belowIndex || index == aboveIndex
                    || otherIndex == aboveIndex) {
                // Exit case, one of the indices to search is same as our bounds
                if (index == belowIndex) {
                    best = below;
                } else if (index == aboveIndex) {
                    best = above;
                } else {
                    // otherIndex is equal to above index, swap index and
                    // otherIndex and search index before other
                    index = otherIndex;
                    otherIndex = index - 1;
                    best = above;
                }
                // Search other index, set best result
                DistanceResult other = distance(otherIndex, aLonR, aLatR);
                double otherDist = Math.abs(other.yDist);
                if (otherDist < Math.abs(best.yDist)) {
                    best = other;
                }
                // Mark done
                done = true;
            } else {
                // Search the test index and determine new above/below indices
                DistanceResult test = distance(index, aLonR, aLatR);
                double testDist = test.yDist;
                if (testDist < 0) {
                    if (aboveDist > 0) {
                        above = test;
                    } else {
                        below = test;
                    }
                } else if (testDist > 0) {
                    if (aboveDist < 0) {
                        below = test;
                    } else {
                        above = test;
                    }
                } else {
                    // Exactly 0!!! we are done!!
                    best = test;
                    done = true;
                }
            }
        } while (!done);
        return best;
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
        DistanceResult best = findBest(0, validHeight - 1, aLonR, aLatR);
        int idxToUse = best.index;
        // Get radius to convert xDist/yDist into meters
        double radius = getRadius(Math.toRadians(centerLats[idxToUse]));

        // Compute bestX value based on hypotenuse and angle diff
        bestX = resolution * 0.5 + best.xDist * radius;

        // Compute potential bestY grid value
        bestY = actualHeight - idxToUse - 0.5;
        // Compute bestY value based on hypotenuse and angle diff in grid space
        bestY = bestY * resolution - best.yDist * radius;

        Point2D point = ptDst != null ? ptDst : new Point2D.Double();
        point.setLocation(bestX, bestY);
        return point;
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
            double cos_side_a = cos(side_a);
            double sin_side_a = sin(side_a);
            double cos_side_b = cos(side_b);
            double sin_side_b = sin(side_b);

            val_trig = (cos_side_a * cos_side_b)
                    + (sin_side_a * sin_side_b * cos(angle_C));
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
                val_trig = (cos_side_b - (cos(side_c) * cos_side_a))
                        / (sin(side_c) * sin_side_a);
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

        return new double[] { alon, alat };
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

            double cos_side_c = cos(side_c);
            double cos_side_a = cos(side_a);
            double sin_side_c = sin(side_c);
            double sin_side_a = sin(side_a);

            double acos_in = (cos_side_c * cos_side_a)
                    + (sin_side_c * sin_side_a * cos(angle_B));
            side_b = acos(acos_in);
            cos_angle_C = (cos_side_c - (cos_side_a * cos(side_b)))
                    / (sin_side_a * sin(side_b));
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

    private static double acos(double a) {
        double atan = atan(sqrt(1 - a * a) / a);
        if (atan < 0) {
            atan += PI;
        }
        return atan;
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
