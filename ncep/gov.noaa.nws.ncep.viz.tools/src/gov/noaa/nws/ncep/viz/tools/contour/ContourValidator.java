/*
 * ContourValidator
 * 
 * Date created 15 JUNE 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.tools.contour;

import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;

import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateArrays;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.operation.IsSimpleOp;
import com.vividsolutions.jts.operation.valid.IsValidOp;
import com.vividsolutions.jts.operation.valid.TopologyValidationError;

/**
 * Contains static methods used to check whether the LineStrings and Polygons
 * are valid JTS geometries. If an invalid Geometry is found, an attempt is made
 * to create one or more valid ones from the invalid geometry.
 * 
 * @author sgilbert
 * 
 */
public class ContourValidator {

    // Log the invalid polygons that cannot be fixed.
    private static NcepLogger logger = NcepLoggerManager
            .getNcepLogger(ContourValidator.class);

    /**
     * Validates each Geometry in a GeometryCollection
     * 
     * @param contours
     *            GeometryCollection containing Polygons and LineStrings
     * @return
     */
    public static Geometry validateContours(Geometry contours) {

        List<Geometry> geoms = new ArrayList<Geometry>();
        GeometryFactory gf = new GeometryFactory();

        for (int i = 0; i < contours.getNumGeometries(); i++) {
            Geometry curr = contours.getGeometryN(i);
            geoms.addAll(validateContour(curr));
        }

        return gf.createGeometryCollection(geoms.toArray(new Geometry[] {}));
    }

    /**
     * Checks to see if a given Polygon or LineString is a valid JTS Geometry.
     * If not, an attempt is made to create valid Geometrys from the invalid
     * one.
     * 
     * @param cntr
     * @return
     */
    public static List<Geometry> validateContour(Geometry cntr) {

        List<Geometry> geoms = new ArrayList<Geometry>();

        if (cntr instanceof Polygon) {

            if (!cntr.isValid()) {
                /*
                 * Polygon is not valid, check why
                 */
                IsValidOp op = new IsValidOp(cntr);
                // System.out.println("NUM OF RINGS = "+((Polygon)curr).getNumInteriorRing());
                // System.out.println(op.getValidationError().getMessage());
                // System.out.println(op.getValidationError().toString());

                switch (op.getValidationError().getErrorType()) {

                case TopologyValidationError.TOO_FEW_POINTS:
                    // degenerate poly. discard
                    break;

                case TopologyValidationError.SELF_INTERSECTION:
                case TopologyValidationError.RING_SELF_INTERSECTION:
                    /*
                     * self intersection. try to separate into two diff polygons
                     * at pt of intersection
                     */
                    List<Geometry> subs = splitPolygonAt(cntr, op
                            .getValidationError().getCoordinate());

                    // The polygon cannot be fixed
                    if (subs.size() == 1 && cntr.equalsExact(subs.get(0))) {
                        logger.info("Invalid polygon");
                        logger.info(cntr);
                    } else {
                        geoms.addAll(subs);
                    }
                    break;

                }

            } else {
                // keep original
                geoms.add(cntr);
            }

        } else if (cntr instanceof LineString) {

            if (!cntr.isValid()) {
                /*
                 * invalid LineString. find out why...
                 */
                IsValidOp op = new IsValidOp(cntr);
                // System.out.println(op.getValidationError().getMessage());
                // System.out.println(op.getValidationError().toString());

                // discard if too_few_points error
                if (op.getValidationError().getErrorType() != TopologyValidationError.TOO_FEW_POINTS) {
                    geoms.add(cntr);
                }
                // else System.out.println("BAD_LINE "+cntr);

            } else {
                // keep original
                geoms.add(cntr);
            }

        }
        // System.out.println("Validation DONE!!!");

        return geoms;
    }

    /*
     * Not useful at this point private static List<Geometry>
     * splitLineString(Geometry cntr) {
     * 
     * Coordinate[] tempCoords = null; Coordinate[] coords =
     * cntr.getCoordinates(); List<Geometry> tempList = new
     * ArrayList<Geometry>(); List<Geometry> newCntrs = new
     * ArrayList<Geometry>(); GeometryFactory gf = new GeometryFactory();
     * LineString ls = gf.createLineString(coords);
     * 
     * IsSimpleOp op = new IsSimpleOp(ls); if ( op.isSimple() ) {
     * tempList.add(cntr); return tempList; }
     * 
     * Coordinate intx = op.getNonSimpleLocation();
     * System.out.println("BreakApartLSAt: "+intx);
     * 
     * ArrayList<Integer> indexes = new ArrayList<Integer>(); for ( int j=0; j <
     * coords.length; j++ ) { if ( coords[j].equals2D( intx ) ) indexes.add(j);
     * }
     * 
     * if ( indexes.size() < 2 ) { CoordinateList clist = new CoordinateList();
     * clist.add(coords[0], true); for ( int j=0; j < coords.length-1; j++ ) {
     * //clist.add( coords[j], true); if ( !coords[j].equals2D(intx) &&
     * !coords[j+1].equals2D(intx)) { if ( CGAlgorithms.isOnLine(intx, new
     * Coordinate[]{ coords[j], coords[j+1]} )) clist.add(intx, true); }
     * clist.add( coords[j+1], true); }
     * 
     * tempCoords = clist.toCoordinateArray(); indexes.clear(); for ( int j=0; j
     * < tempCoords.length; j++ ) { if ( tempCoords[j].equals2D( intx ) )
     * indexes.add(j); }
     * 
     * } else { tempCoords = coords; }
     * 
     * 
     * CoordinateList c1 = new CoordinateList( CoordinateArrays.extract(
     * tempCoords, 0, indexes.get(0))); c1.add( CoordinateArrays.extract(
     * tempCoords, indexes.get(1)+1, tempCoords.length-1), false);
     * //c1.closeRing(); Geometry g =
     * gf.createLineString(c1.toCoordinateArray()); if (g!=null) tempList.add( g
     * );
     * 
     * CoordinateList c2 = new CoordinateList( CoordinateArrays.extract(
     * tempCoords, indexes.get(0), indexes.get(1) ) ); g =
     * createPolygon(c2.toCoordinateArray()); if (g!=null) tempList.add( g );
     * 
     * for ( Geometry geom : tempList ) { newCntrs.addAll( validateContour(geom)
     * ); }
     * 
     * return newCntrs;
     * 
     * }
     */

    /*
     * Try to split up a polygon into two separate polygons at (or near) the
     * given point
     */
    private static List<Geometry> splitPolygonAt(Geometry geom, Coordinate point) {

        ArrayList<Integer> indexes = new ArrayList<Integer>();
        List<Geometry> tempList = new ArrayList<Geometry>();
        List<Geometry> newPolys = new ArrayList<Geometry>();

        Coordinate[] coords = CoordinateArrays.removeRepeatedPoints(geom
                .getCoordinates());

        /*
         * save indexes where given point appears in the polygon
         */
        for (int j = 0; j < coords.length; j++) {
            if (coords[j].equals2D(point))
                indexes.add(j);
        }

        if (indexes.size() == 1) {
            /*
             * if point appears only once, check to see if polygon self
             * intersects at a different point
             */
            List<Geometry> glist = trySplitPolygon(coords);
            if (!glist.isEmpty()) {
                tempList.addAll(glist);
            } else {
                /*
                 * remove the bad point from original to avoid infinite loop
                 */
                CoordinateList clist = new CoordinateList();

                for (int jj = 0; jj < coords.length; jj++) {
                    if (!coords[jj].equals2D(point))
                        clist.add(coords[jj], false);
                }
                clist.closeRing();

                Geometry g = createPolygon(clist.toCoordinateArray());

                if (g != null)
                    tempList.add(g);
            }
        } else if (indexes.size() > 1) {
            /*
             * point occurs more than once
             */
            if (indexes.get(0) == 0 && indexes.get(1) == coords.length - 1) { // point
                                                                              // is
                                                                              // at
                                                                              // endpoints
                List<Geometry> glist = trySplitPolygon(coords);
                if (!glist.isEmpty())
                    tempList.addAll(glist);
            } else {
                /*
                 * create two polygons from original
                 */
                CoordinateList c1 = new CoordinateList(
                        CoordinateArrays.extract(coords, 0, indexes.get(0)));
                c1.add(CoordinateArrays.extract(coords, indexes.get(1) + 1,
                        coords.length - 1), false);
                c1.closeRing();
                Geometry g = createPolygon(c1.toCoordinateArray());
                if (g != null)
                    tempList.add(g);

                CoordinateList c2 = new CoordinateList(
                        CoordinateArrays.extract(coords, indexes.get(0),
                                indexes.get(1)));
                g = createPolygon(c2.toCoordinateArray());
                if (g != null)
                    tempList.add(g);
            }
        }

        /*
         * validate all new polygons as well
         */
        for (Geometry g : tempList) {
            newPolys.addAll(validateContour(g));
        }

        return newPolys;
    }

    /*
	 * 
	 */
    private static List<Geometry> trySplitPolygon(Coordinate[] coords) {

        Coordinate[] tempCoords = null;
        List<Geometry> tempList = new ArrayList<Geometry>();
        GeometryFactory gf = new GeometryFactory();
        LineString ls = gf.createLineString(coords);

        IsSimpleOp op = new IsSimpleOp(ls);
        if (op.isSimple())
            return tempList; // does not self intersect

        Coordinate intx = op.getNonSimpleLocation();
        // System.out.println("BreakApartAt: "+intx);

        /*
         * save indexes where non simple location appears in the polygon
         */
        ArrayList<Integer> indexes = new ArrayList<Integer>();
        for (int j = 0; j < coords.length; j++) {
            if (coords[j].equals2D(intx))
                indexes.add(j);
        }

        if (indexes.size() < 2) {
            /*
             * intersection point appears only once in coordinate list. check
             * each segment, and add intersection point to that segment if they
             * are collinear.
             */
            CoordinateList clist = new CoordinateList();
            clist.add(coords[0], true);
            for (int j = 0; j < coords.length - 1; j++) {
                if (!coords[j].equals2D(intx) && !coords[j + 1].equals2D(intx)) {
                    if (CGAlgorithms.isOnLine(intx, new Coordinate[] {
                            coords[j], coords[j + 1] }))
                        clist.add(intx, true);
                }
                clist.add(coords[j + 1], true);
            }

            tempCoords = clist.toCoordinateArray();
            indexes.clear();
            for (int j = 0; j < tempCoords.length; j++) {
                if (tempCoords[j].equals2D(intx))
                    indexes.add(j);
            }

        } else {
            tempCoords = coords;
        }

        if (indexes.size() < 2)
            return tempList;

        /*
         * create two polygons from list of coordinates in tempCoords at
         * intersection point.
         */
        CoordinateList c1 = new CoordinateList(CoordinateArrays.extract(
                tempCoords, 0, indexes.get(0)));
        c1.add(CoordinateArrays.extract(tempCoords, indexes.get(1) + 1,
                tempCoords.length - 1), false);
        c1.closeRing();
        Geometry g = createPolygon(c1.toCoordinateArray());
        if (g != null)
            tempList.add(g);

        CoordinateList c2 = new CoordinateList(CoordinateArrays.extract(
                tempCoords, indexes.get(0), indexes.get(1)));
        g = createPolygon(c2.toCoordinateArray());
        if (g != null)
            tempList.add(g);

        return tempList;
    }

    /*
     * create a JTS Polygon from the array of Coordinates, if possible
     */
    private static Geometry createPolygon(Coordinate[] coords) {

        if (coords.length < 4)
            return null;

        GeometryFactory gf = new GeometryFactory();
        LinearRing lr = gf.createLinearRing(coords);

        Envelope env = lr.getEnvelopeInternal();
        if ((env.getHeight() == 0) || (env.getWidth() == 0))
            return null; // collinear points

        return gf.createPolygon(lr, null);
    }

}
