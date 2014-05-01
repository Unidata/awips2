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
package com.raytheon.viz.core.map;

import java.awt.geom.Point2D;
import java.util.ArrayList;

import org.geotools.referencing.GeodeticCalculator;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Utility methods for geographic calculations
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 13, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GeoUtil {

    private GeoUtil() {

    }

    /**
     * Rounds an azimuth to a 45 degree increment
     * 
     * @param azimuth
     *            the azimuth to round
     * @return the rounded azimuth
     */
    public static final int roundAzimuth(double azimuth) {
        double intAzimuth = (Math.round(azimuth / 45)) * 45;

        while (intAzimuth < 0)
            intAzimuth += 360;

        return (int) intAzimuth;
    }

    /**
     * Rounds an azimuth to an approximately 22.5 degree increment
     * 
     * @param azimuth
     *            the azimuth to round
     * @return the rounded azimuth
     */
    public static final int roundAzimuth16Directions(double azimuth) {
        double intAzimuth = Math.round((Math.round(azimuth / 22.5)) * 22.5);

        while (intAzimuth < 0)
            intAzimuth += 360;

        return (int) intAzimuth;
    }

    /**
     * Converts an azimuth to a common String representation
     * 
     * @param azimuth
     *            the azimuth to express as a String
     * @param abbrev
     *            whether to use the abbreviation, e.g. S instead of South
     * @return the String representation of the azimuth
     */
    public static final String azimuthToString(int azimuth, boolean abbrev) {
        String result = null;
        switch (azimuth) {
        case 22:
        case 23:
            if (abbrev) {
                result = "NNE";
            } else {
                result = "NORTHNORTHEAST";
            }
            break;
        case 45:
            if (abbrev) {
                result = "NE";
            } else {
                result = "NORTHEAST";
            }
            break;
        case 67:
        case 68:
            if (abbrev) {
                result = "ENE";
            } else {
                result = "EASTNORTHEAST";
            }
            break;
        case 90:
            if (abbrev) {
                result = "E";
            } else {
                result = "EAST";
            }
            break;
        case 112:
        case 113:
            if (abbrev) {
                result = "ESE";
            } else {
                result = "EASTSOUTHEAST";
            }
            break;
        case 135:
            if (abbrev) {
                result = "SE";
            } else {
                result = "SOUTHEAST";
            }
            break;
        case 157:
        case 158:
            if (abbrev) {
                result = "SSE";
            } else {
                result = "SOUTHSOUTHEAST";
            }
            break;
        case 180:
            if (abbrev) {
                result = "S";
            } else {
                result = "SOUTH";
            }
            break;
        case 202:
        case 203:
            if (abbrev) {
                result = "SSW";
            } else {
                result = "SOUTHSOUTHWEST";
            }
            break;
        case 225:
            if (abbrev) {
                result = "SW";
            } else {
                result = "SOUTHWEST";
            }
            break;
        case 247:
        case 248:
            if (abbrev) {
                result = "WSW";
            } else {
                result = "WESTSOUTHWEST";
            }
            break;
        case 270:
            if (abbrev) {
                result = "W";
            } else {
                result = "WEST";
            }
            break;
        case 292:
        case 293:
            if (abbrev) {
                result = "WNW";
            } else {
                result = "WESTNORTHWEST";
            }
            break;
        case 315:
            if (abbrev) {
                result = "NW";
            } else {
                result = "NORTHWEST";
            }
            break;
        case 337:
        case 338:
            if (abbrev) {
                result = "NNW";
            } else {
                result = "NORTHNORTHWEST";
            }
            break;
        default:
            if (abbrev) {
                result = "N";
            } else {
                result = "NORTH";
            }
            break;

        }

        return result;
    }

    /**
     * Splits a line that is composed to two endpoints into multiple points
     * 
     * @param lineBegin
     *            the first point on the line
     * @param lineEnd
     *            the last point on the line
     * @param nPoints
     *            the number of points the line should be composed of
     * @return the line as multiple points
     */
    public static Coordinate[] splitLine(Coordinate lineBegin,
            Coordinate lineEnd, int nPoints) {
        Coordinate[] result = null;
        if (nPoints < 3) {
            result = new Coordinate[] { lineBegin, lineEnd };
        } else {
            ArrayList<Coordinate> list = new ArrayList<Coordinate>();
            GeodeticCalculator gc = new GeodeticCalculator();
            gc.setStartingGeographicPoint(lineBegin.x, lineBegin.y);
            gc.setDestinationGeographicPoint(lineEnd.x, lineEnd.y);
            double azimuth = gc.getAzimuth();
            double totalDistance = gc.getOrthodromicDistance();
            double interval = totalDistance / (nPoints - 1);

            list.add(lineBegin);
            double distance = 0.0;
            for (int i = 1; i < nPoints - 1; i++) {
                distance += interval;
                gc.setDirection(azimuth, distance);
                Point2D point = gc.getDestinationGeographicPoint();
                list.add(new Coordinate(point.getX(), point.getY()));
            }
            list.add(lineEnd);

            result = list.toArray(new Coordinate[list.size()]);
        }

        return result;
    }

    public static Coordinate[] splitLine(int numPoints, Coordinate... coords) {
        double totalDistance = 0.0;
        double distances[] = new double[coords.length - 1];
        GeodeticCalculator gc = new GeodeticCalculator();
        for (int j = 0; j < distances.length; j++) {
            gc.setStartingGeographicPoint(coords[j].x, coords[j].y);
            gc.setDestinationGeographicPoint(coords[j + 1].x, coords[j + 1].y);
            distances[j] = gc.getOrthodromicDistance();
            totalDistance += distances[j];
        }
        double distancePerSegment = new Float(totalDistance / (numPoints - 1));
        Coordinate[] lineData = new Coordinate[numPoints];
        int index = 0;
        double remaining = 0;
        for (int j = 0; j < distances.length; j++) {
            remaining += distances[j];
            gc.setStartingGeographicPoint(coords[j].x, coords[j].y);
            gc.setDestinationGeographicPoint(coords[j + 1].x, coords[j + 1].y);
            double azimuth = gc.getAzimuth();
            while (remaining > 0) {
                gc.setDirection(azimuth, distances[j] - remaining);
                Point2D p = gc.getDestinationGeographicPoint();
                lineData[index++] = new Coordinate(p.getX(), p.getY());
                remaining -= distancePerSegment;
            }
        }
        lineData[numPoints - 1] = coords[distances.length];
        return lineData;
    }

    /**
     * Determines the lat/lon of a distance along a line
     * 
     * @param lineStart
     *            the start of the line
     * @param lineEnd
     *            the end of the line
     * @param distance
     *            the distance down the line of the new coordinate
     * @return the new coordinate along the line
     */
    public static Coordinate determineCoordinateAlongLine(Coordinate lineStart,
            Coordinate lineEnd, double distance) {
        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(lineStart.x, lineStart.y);
        gc.setDestinationGeographicPoint(lineEnd.x, lineEnd.y);
        double azimuth = gc.getAzimuth();
        gc.setDirection(azimuth, distance);
        Point2D p = gc.getDestinationGeographicPoint();
        return new Coordinate(p.getX(), p.getY());
    }

    /**
     * Formats a coordinate into a standard string
     * 
     * @param coord
     *            the coordinate to format
     * @return a String representation of the coordinate
     */
    public static String formatCoordinate(Coordinate coord) {
        double y = coord.y;
        String yPrefix = "N";
        if (y < 0) {
            y = Math.abs(y);
            yPrefix = "S";
        }

        double x = coord.x;
        String xPrefix = "E";
        if (x < 0) {
            x = Math.abs(x);
            xPrefix = "W";
        }

        return String.format("%.2f%s %.2f%s", y, yPrefix, x, xPrefix);
    }

    public static void main(String[] args) {
        Coordinate start = new Coordinate(-95.9, 41.3);
        Coordinate end = new Coordinate(-92.39, 33.88);

        Coordinate[] line = GeoUtil.splitLine(start, end, 5);
        for (Coordinate c : line) {
            System.out.println(c);
        }

    }

}
