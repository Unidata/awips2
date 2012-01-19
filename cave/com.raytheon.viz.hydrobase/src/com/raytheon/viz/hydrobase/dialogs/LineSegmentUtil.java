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
package com.raytheon.viz.hydrobase.dialogs;

import java.util.ArrayList;

import com.raytheon.viz.hydrobase.data.HrapBinList;
import com.raytheon.viz.hydrobase.data.LineSegment;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.util.HrapUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Line Segment Utility Class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2009 2772       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class LineSegmentUtil {
    private static final int LATLON_NORTHWEST_CORNER = 0;

    private static final int LATLON_NORTHEAST_CORNER = 1;

    private static final int LATLON_SOUTHEAST_CORNER = 2;

    private static final int LATLON_SOUTHWEST_CORNER = 3;

    private static final double MIN_DOUBLE_ERROR = 0.00000001;

    /**
     * Constructs an array from segments from an array of n points. the i th and
     * i+1 th points are used to initialize n-1 segments.
     * 
     * @param points
     *            ArrayList of Point objects
     */
    public static ArrayList<LineSegment> getSegmentsFromPoints(
            ArrayList<Coordinate> points) {
        ArrayList<LineSegment> lineSegments = new ArrayList<LineSegment>();

        for (int i = 0; i < points.size() - 1; i++) {
            LineSegment segment = initLineSegment(points.get(i), points
                    .get(i + 1));
            lineSegments.add(segment);
        }

        return lineSegments;
    }

    /**
     * Checks two Points for equality.
     * 
     * @param p1
     *            Point 1
     * @param p2
     *            Point 2
     * @return true if points are equal
     */
    public static boolean pointsEqual(Coordinate p1, Coordinate p2) {
        boolean equal = false;

        if ((p1.x == p2.x) && (p1.y == p2.y)) {
            equal = true;
        }

        return equal;
    }

    /**
     * Given 2 points, initializes the LineSegment structure.
     * 
     * Determines if the segment is a vertical line. Calculates slope and base
     * (if not vertical) Stores the x_value if it is vertical.
     * 
     * Calculates the max and min x and y values of the segment.
     * 
     * @param p1
     *            Point 1
     * @param p2
     *            Point 2
     * @return LineSegment object
     */
    public static LineSegment initLineSegment(Coordinate p1, Coordinate p2) {
        LineSegment seg = new LineSegment();
        double rise;
        double run;

        /* copy the points to the LineSegment structure */
        seg.setPoint1(p1);
        seg.setPoint2(p2);

        /* calculate rise and run, set slope and base of line */
        rise = p2.y - p1.y;
        run = p2.x - p1.x;

        if (run == 0) {
            seg.setVertical(true);
            seg.setXValue(p1.x);
        } else {
            seg.setVertical(false);
            seg.setXValue(p1.x);

            seg.setSlope(rise / run);
            seg.setBase(p1.y - (seg.getSlope() * p1.x));
        }

        /* set the max and min box */
        Coordinate min = new Coordinate();
        seg.setMin(min);
        seg.getMin().y = (
                minDouble(seg.getPoint1().y, seg.getPoint2().y));
        seg.getMin().x = (
                minDouble(seg.getPoint1().x, seg.getPoint2().x));
        
        Coordinate max = new Coordinate();
        seg.setMax(max);
        seg.getMax().y = (
                maxDouble(seg.getPoint1().y, seg.getPoint2().y));
        seg.getMax().x = (
                maxDouble(seg.getPoint1().x, seg.getPoint2().x));

        return seg;
    }

    /**
     * Given an array of line segments, this function fills the HrapBinList
     * variable with the bins inside the area defined by the segments.
     * 
     * Algorithm:
     * 
     * Find the max and min lat-lon corners of the area. Convert the lat-lon to
     * HRAP coords. Widen the HRAP box to search, to make sure it starts outside
     * the polygon.
     * 
     * For each row For each column
     * 
     * Draw a segment from the previous column. If that segment intersects one
     * of the polygon's segments, then add the number of intersections to the
     * total number of intersections for the row. If the row intersections is
     * odd, then the hrap bin is in the polygon, otherwise it is outside.
     * 
     * Compute area for each bin that is within the polygon. Add it to total for
     * area.
     * 
     * @param segments
     *            ArrayList of LineSegment objects
     */
    public static HrapBinList getHrapBinListFromSegments(
            ArrayList<LineSegment> segments) {
        HrapBinList binList = new HrapBinList();
        Coordinate maxLatLon;
        Coordinate minLatLon;
        Coordinate startLatLon;
        Coordinate endLatLon;
        ArrayList<Coordinate> points = new ArrayList<Coordinate>();
        double r;
        double c;
        double maxCol;
        double minCol;
        double maxRow;
        double minRow;
        double singleBinArea = 0;
        boolean inside = false;
        int index = 0;
        int numIntersections = 0;
        LineSegment segment;

        /* init HrapBinList */
        binList.setNumBins(0);
        binList.setNumRows(0);
        binList.setArea(0);

        /* get the max and min points of the segments */
        maxLatLon = LineSegmentUtil.getMaxXY(segments);
        minLatLon = LineSegmentUtil.getMinXY(segments);

        /*
         * Determine the HRAP box that will completely enclose the latitude /
         * longitude box defined by the max lat/lon and the min lat/lon pairs
         * retrieved above.
         */
        Coordinate hrap = HrapUtil.latLonToHrap(new Coordinate(minLatLon.x, maxLatLon.y));
        minRow = hrap.y;
        maxRow = hrap.y;
        minCol = hrap.x;
        maxCol = hrap.x;

        for (int i = LATLON_NORTHEAST_CORNER; i <= LATLON_SOUTHWEST_CORNER; i++) {
            switch (i) {
            case LATLON_NORTHEAST_CORNER:
                hrap = HrapUtil
                        .latLonToHrap(new Coordinate(maxLatLon.x, maxLatLon.y));
                break;

            case LATLON_SOUTHEAST_CORNER:
                hrap = HrapUtil
                        .latLonToHrap(new Coordinate(maxLatLon.x, minLatLon.y));
                break;

            case LATLON_SOUTHWEST_CORNER:
                hrap = HrapUtil
                        .latLonToHrap(new Coordinate(minLatLon.x, minLatLon.y));
                break;

            default:
                /* Execution should never reach this point. */
                break;
            }
            r = hrap.y;
            c = hrap.x;

            if (c < minCol) {
                minCol = c;
            } else if (c > maxCol) {
                maxCol = c;
            } else if (r < minRow) {
                minRow = r;
            } else if (r > maxRow) {
                maxRow = r;
            }
        }

        maxRow = Math.floor(maxRow);
        maxCol = Math.floor(maxCol);
        minRow = Math.floor(minRow);
        minCol = Math.floor(minCol);

        /* expand the box to make sure polygon has been covered */
        minRow -= 2;
        minCol -= 2;
        maxRow += 2;
        maxCol += 2;

        for (r = minRow + 0.5; r <= maxRow; r++) {
            inside = false;
            numIntersections = 0;

            /* init the first lat lon point */
            startLatLon = new Coordinate(HrapUtil.hrapToLatLon(new Coordinate(minCol - 0.5, r)));

            for (c = minCol + 0.5; c <= maxCol; c++) {
                /* get the lat lon coordinate from the hrap row and column */
                endLatLon = new Coordinate(HrapUtil.hrapToLatLon(new Coordinate(c, r)));

                /* create a segment from start to end */
                segment = initLineSegment(startLatLon, endLatLon);

                points = getIntersectionPoints(segment, segments);

                index = (int) binList.getNumRows();

                if (points.size() > 0) {
                    numIntersections += points.size();
                }

                /*
                 * key of algorithm: if the number of intersections is odd, then
                 * you are inside the polygon, else outside
                 */
                if ((numIntersections % 2) == 1) {
                    /* if previous bin was inside */
                    if (inside) {
                        binList.getEndCols().set(index, (long) c);
                        binList.setNumBins(binList.getNumBins() + 1);
                        singleBinArea = HrapUtil.getHrapBinArea(new Coordinate(c, r));
                        binList.setArea(binList.getArea() + singleBinArea);
                    } else {
                        /* previous bin was outside */
                        binList.getRows().add((long) r);
                        binList.getBeginCols().add((long) c);
                        binList.getEndCols().add((long) c);

                        binList.setNumBins(binList.getNumBins() + 1);
                        singleBinArea = HrapUtil.getHrapBinArea(new Coordinate(c, r));
                        binList.setArea(binList.getArea() + singleBinArea);

                        inside = true;
                    }
                } else {
                    /*
                     * else if previous bin was inside and, since this one is
                     * not, increment the row counter
                     */
                    /* intersections is even */
                    if (inside) {
                        inside = false;
                        binList.setNumRows(binList.getNumRows() + 1);
                    }
                }
                
                startLatLon = endLatLon;
            }
        }
        
        return binList;
    }

    /**
     * returns the minimum of two doubles.
     * 
     * @param num1
     *            first value to check
     * @param num2
     *            second value to check
     * @return the smaller of the two, num1 or num2
     */
    public static double minDouble(double num1, double num2) {
        if (num1 < num2) {
            return num1;
        } else {
            return num2;
        }
    }

    /**
     * returns the maximum of two doubles.
     * 
     * @param num1
     *            first value to check
     * @param num2
     *            second value to check
     * @return the larger of the two, num1 or num2
     */
    public static double maxDouble(double num1, double num2) {
        if (num1 > num2) {
            return num1;
        } else {
            return num2;
        }
    }

    /**
     * Determines the max x and y values for a list of segments.
     * 
     * @param segments
     *            ArrayList<LineSegment>
     * @return Max Point of the segment list
     */
    public static Coordinate getMaxXY(ArrayList<LineSegment> segments) {
        Coordinate p = segments.get(0).getMax();

        for (int i = 1; i < segments.size(); i++) {
            p.x = (maxDouble(segments.get(i).getMax().x, p.x));
            p.y = (maxDouble(segments.get(i).getMax().y, p.y));
        }

        return p;
    }

    /**
     * Determines the min x and y values for a list of segments.
     * 
     * @param segments
     *            ArrayList<LineSegment>
     * @return Min Point of the segment list
     */
    public static Coordinate getMinXY(ArrayList<LineSegment> segments) {
        Coordinate p = new Coordinate();

        p.x = (segments.get(0).getMin().x);
        p.y = (segments.get(0).getMin().y);

        for (int i = 1; i < segments.size(); i++) {
            p.x = (minDouble(segments.get(i).getMin().x, p.x));
            p.y = (minDouble(segments.get(i).getMin().y, p.y));
        }

        return p;
    }

    /**
     * Returns the points of intersection between one segment and an array of
     * segments
     * 
     * @param segment
     *            Single LineSegment
     * @param segments
     *            List of LineSegments
     * @param numSegments
     *            Number of segments
     */
    public static ArrayList<Coordinate> getIntersectionPoints(LineSegment segment,
            ArrayList<LineSegment> segments) {
        Coordinate intersectPoint;
        ArrayList<Coordinate> points = new ArrayList<Coordinate>();

        for (int i = 0; i < segments.size(); i++) {
            intersectPoint = getIntersectionOfSegments(segment, segments.get(i));

            if (intersectPoint != null) {
                points.add(intersectPoint);
            }
        }

        return points;
    }

    /**
     * Determines the point of intersection (if any) between two segments
     * 
     * @param s1
     *            LineSegment 1
     * @param s2
     *            LineSegment 2
     * @return The intersection point, null if no intersection
     */
    public static Coordinate getIntersectionOfSegments(LineSegment s1, LineSegment s2) {
        Coordinate p = new Coordinate(0, 0);
        double x;
        double y;
        double y1;
        double y2;

        LineSegment tempVertical;
        LineSegment tempNonVertical;

        boolean intersect = false;

        if (s1.isVertical() && s2.isVertical()) {
            intersect = false;
        } else if (s1.isVertical() || s2.isVertical()) {
            /* if one is a vertical line and one is not */

            /* assign to tempVertical and tempNonVertical */
            if (s1.isVertical()) {
                tempVertical = s1;
                tempNonVertical = s2;
            } else {
                tempVertical = s2;
                tempNonVertical = s1;
            }

            /*
             * see if vertical segment is in the x range of the non vertical
             * segment
             */
            if (isBetweenInclusive(tempVertical.getXValue(), tempNonVertical
                    .getMin().x, tempNonVertical.getMax().x)) {
                y = evaluateLineSegmentAtX(tempNonVertical, tempVertical
                        .getXValue());

                if ((y != HydroConstants.MISSING_VALUE)
                        && isBetweenInclusive(y, tempVertical.getMin().y,
                                tempVertical.getMax().y)) {
                    p.x = (tempVertical.getXValue());
                    p.y = (y);
                    intersect = true;
                } else {
                    intersect = false;
                }
            }
        } else {
            /* the lines are both non vertical */
            x = (s1.getBase() - s2.getBase()) / (s2.getSlope() - s1.getSlope());

            y1 = evaluateLineSegmentAtX(s1, x);
            y2 = evaluateLineSegmentAtX(s2, x);

            if ((y1 == HydroConstants.MISSING_VALUE)
                    || (y2 == HydroConstants.MISSING_VALUE)) {
                intersect = false;
            } else if ((Math.abs(y1 - y2) > MIN_DOUBLE_ERROR)) {
                intersect = false;
            } else {
                p.x = x;
                p.y = y1;
                intersect = true;
            }
        }

        if (intersect) {
            return p;
        }

        return null;
    }

    /**
     * Determine the Y value of a segment, given x. If the line is Vertical or X
     * is not in the domain, then the returned value is invalid.
     * 
     * @param segment
     *            The line segment
     * @param x
     *            The x value
     * @return The y value
     */
    public static double evaluateLineSegmentAtX(LineSegment segment, double x) {
        double y = 0;

        if (segment.isVertical()) {
            y = segment.getMin().y;
        } else if (isBetweenInclusive(x, segment.getMin().x, segment
                .getMax().x)) {
            y = (segment.getSlope() * x) + segment.getBase();
        } else {
            y = HydroConstants.MISSING_VALUE;
        }

        return y;
    }

    /**
     * Returns whether (x is >= start) and (x >= end)
     * 
     * @param x
     *            The value to check
     * @param start
     *            The start value
     * @param end
     *            The end value
     * @return true if (x is >= start) and (x >= end)
     */
    public static boolean isBetweenInclusive(double x, double start, double end) {
        boolean isBetween = false;

        if ((x >= start) && (x <= end)) {
            isBetween = true;
        }

        return isBetween;
    }
}
