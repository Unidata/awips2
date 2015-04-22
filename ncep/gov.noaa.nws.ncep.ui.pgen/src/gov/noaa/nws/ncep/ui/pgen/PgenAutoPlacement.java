/*
 * gov.noaa.nws.ncep.ui.pgen
 * 
 * November 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;

/**
 * A utility class used to place a text box and its associated point arrow
 * in/around its associated polygon without intersecting the polygon and other
 * geometries on the visible screen.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 4, 2013  TTR 752       jwu     Initial creation
 * 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 */

public class PgenAutoPlacement {

    private static final double START_ANGLE = 45.0;;

    private PgenRangeRecord textBox;

    private PgenRangeRecord polygonBox;

    private PgenRangeRecord arrowBox;

    private List<PgenRangeRecord> activeRanges;

    private PgenRangeRecord screenBox;

    private boolean checkCenter;

    private double angleIncrement;

    private double distIncrement;

    private GeometryFactory gf;

    /**
     * @param textBox
     * @param arrowBox
     * @param activeRanges
     * @param screenBox
     * @param checkCenter
     * @param angleIncrement
     * @param distIncrement
     */
    public PgenAutoPlacement(PgenRangeRecord textBox,
            PgenRangeRecord polygonBox, PgenRangeRecord arrowBox,
            List<PgenRangeRecord> activeRanges, PgenRangeRecord screenBox,
            boolean checkCenter, double angleIncrement, double distIncrement) {
        super();
        this.textBox = textBox;
        this.polygonBox = polygonBox;
        this.arrowBox = arrowBox;
        this.activeRanges = activeRanges;
        this.screenBox = screenBox;
        this.checkCenter = checkCenter;
        this.angleIncrement = angleIncrement;
        this.distIncrement = distIncrement;

        gf = new GeometryFactory();
    }

    /**
     * @param textBox
     *            the textBox to set
     */
    public void setTextBox(PgenRangeRecord textBox) {
        this.textBox = textBox;
    }

    /**
     * @param polygonBox
     *            the polygonBox to set
     */
    public void setPolygonBox(PgenRangeRecord polygonBox) {
        this.polygonBox = polygonBox;
    }

    /**
     * @param activeRanges
     *            the activeRanges to set
     */
    public void setActiveRanges(List<PgenRangeRecord> activeRanges) {
        this.activeRanges = activeRanges;
    }

    /**
     * @param screenBox
     *            the screenBox to set
     */
    public void setScreenBox(PgenRangeRecord screenBox) {
        this.screenBox = screenBox;
    }

    /**
     * @param checkCenter
     *            the checkCenter to set
     */
    public void setCheckCenter(boolean checkCenter) {
        this.checkCenter = checkCenter;
    }

    /**
     * @param angleIncrement
     *            the angleIncrement to set
     */
    public void setAngleIncrement(double angleIncrement) {
        this.angleIncrement = angleIncrement;
    }

    /**
     * @param distIncrement
     *            the distIncrement to set
     */
    public void setDistIncrement(double distIncrement) {
        this.distIncrement = distIncrement;
    }

    /**
     * Generate a JTS polygon from a set of points.
     * 
     * @param points
     *            array of points
     * @return
     */
    public static Geometry pointsToGeometry(Coordinate[] points,
            boolean closed, GeometryFactory gf) {

        Geometry geom;
        if (points == null || points.length == 0) {
            geom = null;
        } else if (points.length == 1) {
            CoordinateArraySequence cas = new CoordinateArraySequence(points);
            geom = new Point(cas, new GeometryFactory());
        } else if (points.length == 2) {
            geom = pointsToLineString(points, gf);
        } else {
            if (closed) {
                geom = pointsToPolygon(points, gf);
            } else {
                geom = pointsToLineString(points, gf);
            }
        }

        return geom;
    }

    /**
     * Generate a JTS polygon from a set of points.
     * 
     * @param pots
     *            list of points
     * @return
     */
    public static Geometry pointsToGeometry(List<Coordinate> pts,
            boolean closed, GeometryFactory gf) {

        if (pts == null || pts.size() == 0)
            return null;

        Coordinate[] points = new Coordinate[pts.size()];
        pts.toArray(points);

        return pointsToGeometry(points, closed, gf);
    }

    /**
     * Generate a JTS LineString from a set of points.
     * 
     * @param points
     *            array of points
     * @return
     */
    public static Geometry pointsToLineString(Coordinate[] points,
            GeometryFactory gf) {

        CoordinateArraySequence cas = new CoordinateArraySequence(points);

        return new LineString(cas, gf);
    }

    /**
     * Generate a JTS polygon from a set of points.
     * 
     * It is assumed that the first point is not repeated at the end in the
     * input array of point.
     * 
     * @param points
     *            array of points
     * @return
     */
    public static Polygon pointsToPolygon(Coordinate[] points,
            GeometryFactory gf) {

        Coordinate[] coords = Arrays.copyOf(points, points.length + 1);
        coords[coords.length - 1] = coords[0];

        CoordinateArraySequence cas = new CoordinateArraySequence(coords);
        LinearRing ring = new LinearRing(cas, gf);

        Polygon polygon = new Polygon(ring, null, gf);

        return polygon;
    }

    /**
     * Find a location to place the text box so the box won't intersect its
     * linked polygon and all other geometries.
     * 
     * @param points
     *            array of points
     * @return PgenRangeRecord
     */
    public PgenRangeRecord[] placeTextBox() {

        PgenRangeRecord[] newrr = new PgenRangeRecord[2];

        Geometry textgeo = pointsToGeometry(textBox.getExtent(), true, gf);
        Geometry textgeo1 = pointsToGeometry(textBox.getExtentWithoutBuffer(),
                true, gf);
        // Geometry ccfpbox = pointsToGeometry(polygonBox.getExtent(), gf);
        Geometry ccfpgeo = pointsToGeometry(polygonBox.getPoints(), true, gf);

        GeometryCollection gc = buildGC(activeRanges, gf);

        // Place the text in the centroid of the CCFP polygon.
        newrr[0] = this.textBox;
        newrr[1] = new PgenRangeRecord(); // no arrow needed.
        if (checkCenter && textgeo1.within(ccfpgeo)) {

            if (gc == null || (gc != null && !gc.intersects(textgeo1))) {
                // newrr[0] = this.textBox;
                // newrr[1] = new PgenRangeRecord(); // no arrow needed.
                return newrr;
            }
        }

        // Start from the center direction to find the first location.
        double screenWidth = screenBox.getExtent().get(1).x
                - screenBox.getExtent().get(0).x;

        // remember the start location (center)
        PgenRangeRecord startLoc = textBox.copy();

        // angleIncrement = 5.0;
        // distIncrement = 1.25;
        double unit = screenWidth * distIncrement / 100;

        // Find how many round of horizontal increments needed to reach the
        // farest of the screen.
        double maxDist = textBox.maxExtention(screenBox);
        int nrounds = (int) (maxDist / unit) + 1;

        // Find the number of trials in one round.
        int numOfRotations = (int) (360 / angleIncrement + 0.05);

        double radius = 0;
        double ang, xdir, ydir;
        boolean firstAvailable = false;

        for (int jj = 0; jj < nrounds; jj++) {
            radius = unit + jj * unit; // distance increment
            for (int ii = 0; ii < numOfRotations; ii++) {
                ang = Math.toRadians(START_ANGLE - ii * angleIncrement); // anti-clockwise
                xdir = radius * Math.cos(ang);
                ydir = radius * Math.sin(ang);

                // Move box and make sure it is visible in screen.
                PgenRangeRecord newBox = moveTextBox(startLoc, xdir, ydir);
                PgenRangeRecord newArrow = new PgenRangeRecord(); // no arrow
                                                                  // needed.;
                if (!newBox.within(screenBox))
                    continue;

                /*
                 * check if the box intersects the polygon and then check if it
                 * intersects all other geometries. Remember the first box that
                 * found outside of the polygon.
                 */
                textgeo = pointsToGeometry(newBox.getExtent(), true, gf);
                boolean insideCcfp = textgeo.within(ccfpgeo);
                if (insideCcfp || !textgeo.intersects(ccfpgeo)) {
                    // if ( !textgeo.intersects(ccfpgeo)) {

                    if (!insideCcfp) {
                        newArrow = findArrow(newBox, ccfpgeo, gc);
                    }

                    // Remember the first one.
                    if (!firstAvailable) {
                        newrr[0] = newBox.copy();
                        newrr[1] = newArrow.copy();
                        firstAvailable = true;
                    }

                    if (gc == null || (gc != null && !gc.intersects(textgeo))) {
                        /*
                         * to do - let's check if the arrow does not intersect
                         * with other geometries, except the CCFP polygon and
                         * the text box?
                         */
                        if (!insideCcfp
                                && (gc != null && gc.getNumGeometries() > 0)) {
                            Geometry arrowgeo = pointsToGeometry(
                                    newArrow.getPoints(), false, gf);
                            boolean validArrow = true;
                            for (int kk = 0; kk < gc.getNumGeometries(); kk++) {
                                if (arrowgeo.intersects(gc.getGeometryN(kk))) {
                                    validArrow = false;
                                    break;
                                }
                            }

                            if (!validArrow) {
                                continue;
                            }
                        }

                        newrr[0] = newBox;
                        newrr[1] = newArrow;
                        return newrr;
                    }
                }
            }
        }

        return newrr;

    }

    /*
     * Move the text box (range record) as specified.
     */
    private PgenRangeRecord moveTextBox(PgenRangeRecord rr, double xinc,
            double yinc) {

        PgenRangeRecord newrr = rr.copy();
        for (Coordinate pt : newrr.getExtent()) {
            pt.x += xinc;
            pt.y += yinc;
        }

        for (Coordinate pt : newrr.getPoints()) {
            pt.x += xinc;
            pt.y += yinc;
        }

        return newrr;

    }

    /*
     * Put all geometries that need to be checked agaist into a
     * GeometryCollection.
     */
    private GeometryCollection buildGC(List<PgenRangeRecord> ranges,
            GeometryFactory gf) {
        GeometryCollection ngc = null;
        List<Geometry> rangeGeos = new ArrayList<Geometry>();
        for (PgenRangeRecord rr : ranges) {
            if (rr.getPoints() != null && rr.getPoints().size() > 0) {
                if (rr.getPoints().size() > 1) {
                    rangeGeos.add(pointsToGeometry(rr.getPoints(),
                            rr.isClosed(), gf));
                } else {
                    Geometry ego = pointsToGeometry(rr.getExtent(), true, gf);
                    if (ego != null) {
                        rangeGeos
                                .add(pointsToGeometry(rr.getExtent(), true, gf));
                    }
                }
            }
        }

        if (rangeGeos.size() > 0) {
            ngc = new GeometryCollection(
                    rangeGeos.toArray(new Geometry[rangeGeos.size()]), gf);
        }

        return ngc;

    }

    /*
     * Find an arrow that will not intersect with a list of geometries.
     */
    private PgenRangeRecord findArrow(PgenRangeRecord textBox,
            Geometry polygonGeo, GeometryCollection gc) {

        // First use arrow from text centroid to polygon centroid.
        List<Coordinate> arrow = new ArrayList<Coordinate>();
        Point polyCenter = polygonGeo.getCentroid();
        Coordinate textCenter = textBox.getPoints().get(0);

        arrow.add(textCenter);
        arrow.add(new Coordinate(polyCenter.getX(), polyCenter.getY()));

        // Now starts from the intersection point of the arrow with the text
        Geometry arrowGeo = pointsToGeometry(arrow, false, gf);
        Geometry textGeo = pointsToGeometry(textBox.getExtentWithoutBuffer(),
                true, gf);

        Geometry interPt = arrowGeo.intersection(textGeo);

        arrow.remove(0);
        if (interPt.getNumPoints() > 1) {
            arrow.add(0, interPt.getCoordinates()[1]);
        } else {
            arrow.add(0, interPt.getCoordinates()[0]);
        }
        Geometry firstArrowGeo = pointsToGeometry(arrow, false, gf);

        return new PgenRangeRecord(firstArrowGeo.getCoordinates(), false);

        // if (gc == null
        // || (gc != null && gc.getNumGeometries() > 0 && firstArrowGeo
        // .intersects(gc))) {
        // return new PgenRangeRecord(firstArrowGeo.getCoordinates());
        // } else {
        // return new PgenRangeRecord(firstArrowGeo.getCoordinates());
        // }

    }

}
