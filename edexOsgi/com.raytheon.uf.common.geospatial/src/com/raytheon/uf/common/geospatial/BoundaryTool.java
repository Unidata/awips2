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
package com.raytheon.uf.common.geospatial;

import java.util.LinkedList;
import java.util.List;

import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Utility for doing boundary calculations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class BoundaryTool {

    /**
     * Creates a boundary geometry object in the projection specified by
     * crsToConvertTo from the startX,startY to endX,endY
     * 
     * @param geom
     * @param crsToConvertTo
     * @param startX
     * @param startY
     * @param endX
     * @param endY
     * @param inclusive
     * @return
     * @throws Exception
     */
    public static Polygon calculateBoundaryGeometry(
            MathTransform gridToDesired, int startX, int startY, int endX,
            int endY, boolean inclusive, int xInc, int yInc) throws Exception {
        if (startX > endX) {
            int tmp = startX;
            startX = endX;
            endX = tmp;
        }
        if (startY > endY) {
            int tmp = startY;
            startY = endX;
            endY = tmp;
        }
        if (inclusive) {
            ++endX;
            ++endY;
        }
        List<Coordinate> coordinateList = new LinkedList<Coordinate>();
        double[] in = new double[3];
        double[] out = new double[3];

        // y = startY, across x
        for (int x = startX; x < endX - 1; x += xInc) {
            in[0] = x;
            in[1] = startY;
            try {
                gridToDesired.transform(in, 0, out, 0, 1);
                coordinateList.add(new Coordinate(out[0], out[1]));
            } catch (TransformException e) {
                // ignore;
            }
        }

        if (in[0] != (endX - 1)) {
            in[0] = endX - 1;
            in[1] = startY;
            try {
                gridToDesired.transform(in, 0, out, 0, 1);
                coordinateList.add(new Coordinate(out[0], out[1]));
            } catch (TransformException e) {
                // ignore;
            }
        }

        // across y, x = endX-1
        for (int y = startY; y < endY - 1; y += yInc) {
            in[0] = endX - 1;
            in[1] = y;
            try {
                gridToDesired.transform(in, 0, out, 0, 1);
                coordinateList.add(new Coordinate(out[0], out[1]));
            } catch (TransformException e) {
                // ignore;
            }
        }

        if (in[1] != (endY - 1)) {
            in[0] = endX - 1;
            in[1] = endY - 1;
            try {
                gridToDesired.transform(in, 0, out, 0, 1);
                coordinateList.add(new Coordinate(out[0], out[1]));
            } catch (TransformException e) {
                // ignore;
            }
        }

        // y = endY-1, reverse across x
        for (int x = endX - 1; x > startX; x -= xInc) {
            in[0] = x;
            in[1] = endY - 1;
            try {
                gridToDesired.transform(in, 0, out, 0, 1);
                coordinateList.add(new Coordinate(out[0], out[1]));
            } catch (TransformException e) {
                // ignore;
            }
        }

        if (in[0] != startX) {
            in[0] = startX;
            in[1] = endY - 1;
            try {
                gridToDesired.transform(in, 0, out, 0, 1);
                coordinateList.add(new Coordinate(out[0], out[1]));
            } catch (TransformException e) {
                // ignore;
            }
        }

        // reverse across y, x = 0
        for (int y = endY - 1; y >= startY; y -= yInc) {
            in[0] = startX;
            in[1] = y;
            try {
                gridToDesired.transform(in, 0, out, 0, 1);
                coordinateList.add(new Coordinate(out[0], out[1]));
            } catch (TransformException e) {
                // ignore;
            }
        }

        if (in[1] != startY) {
            in[0] = startX;
            in[1] = startY;
            try {
                gridToDesired.transform(in, 0, out, 0, 1);
                coordinateList.add(new Coordinate(out[0], out[1]));
            } catch (TransformException e) {
                // ignore;
            }
        }

        // Create the polygon
        GeometryFactory gf = new GeometryFactory();
        Polygon rval = gf.createPolygon(gf.createLinearRing(coordinateList
                .toArray(new Coordinate[coordinateList.size()])), null);

        return rval;
    }

    /**
     * Checks to see if g1 intersects g2
     * 
     * @param g1
     * @param g2
     * @return
     */
    public static boolean intersects(Geometry g1, Geometry g2) {
        if (g1 instanceof GeometryCollection) {
            for (int i = 0; i < g1.getNumGeometries(); ++i) {
                if (intersects(g1.getGeometryN(i), g2)) {
                    return true;
                }
            }
            return false;
        } else if (g2 instanceof GeometryCollection) {
            for (int i = 0; i < g2.getNumGeometries(); ++i) {
                if (intersects(g1, g2.getGeometryN(i))) {
                    return true;
                }
            }
            return false;
        } else {
            return g1.intersects(g2);
        }
    }

    /**
     * Checks to see if g1 intersects g2
     * 
     * @param g1
     * @param g2
     * @return
     */
    public static boolean contains(Geometry g1, Geometry g2) {
        if (g1 instanceof GeometryCollection) {
            for (int i = 0; i < g1.getNumGeometries(); ++i) {
                if (!contains(g1.getGeometryN(i), g2)) {
                    return false;
                }
            }
            return true;
        } else if (g2 instanceof GeometryCollection) {
            for (int i = 0; i < g2.getNumGeometries(); ++i) {
                if (!contains(g1, g2.getGeometryN(i))) {
                    return false;
                }
            }
            return true;
        } else {
            return g1.contains(g2);
        }
    }

    /**
     * Checks to see if g1 intersects the envelope
     * 
     * @param g1
     * @param g2
     * @return
     */
    public static boolean intersects(Geometry g1, Envelope envelope) {
        Coordinate[] bounds = new Coordinate[5];
        bounds[0] = new Coordinate(envelope.getMinX(), envelope.getMinY());
        bounds[bounds.length - 1] = new Coordinate(bounds[0]);
        bounds[2] = new Coordinate(envelope.getMaxX(), envelope.getMaxY());
        bounds[1] = new Coordinate(bounds[2].x, bounds[0].y);
        bounds[3] = new Coordinate(bounds[0].x, bounds[2].y);
        GeometryFactory gf = new GeometryFactory();
        Geometry envPoly = gf.createPolygon(gf.createLinearRing(bounds), null);
        return intersects(g1, envPoly);
    }

    /**
     * Get the total area of intersection for 2 geometries
     * 
     * @param g1
     * @param g2
     * @return
     */
    public static double areaOfIntersection(Geometry g1, Geometry g2) {
        double area = 0;
        if (g1 instanceof GeometryCollection) {
            for (int i = 0; i < g1.getNumGeometries(); ++i) {
                area += areaOfIntersection(g1.getGeometryN(i), g2);
            }
        } else if (g2 instanceof GeometryCollection) {
            for (int i = 0; i < g2.getNumGeometries(); ++i) {
                area += areaOfIntersection(g1, g2.getGeometryN(i));
            }
        } else {
            area += g1.intersection(g2).getArea();
        }
        return area;
    }
}
