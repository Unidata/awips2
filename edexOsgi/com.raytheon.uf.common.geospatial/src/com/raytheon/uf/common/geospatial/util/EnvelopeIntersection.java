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
package com.raytheon.uf.common.geospatial.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Utility class capable of computing geometric intersection of one
 * {@link Envelope} into another. Resulting {@link Geometry} may consist of
 * multiple {@link Polygon}s
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 14, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class EnvelopeIntersection {

    private static final GeometryFactory gf = new GeometryFactory();

    /**
     * Computes an intersection {@link Geometry} between sourceEnvelope and
     * targetEnvelope in targetEnvelope's CRS space. The resulting
     * {@link Geometry} may contain multiple Geometries within it
     * 
     * @param sourceEnvelope
     * @param targetEnvelope
     * @param threshold
     * @return
     * @throws TransformException
     * @throws FactoryException
     */
    public static Geometry createEnvelopeIntersection(Envelope sourceEnvelope,
            Envelope targetEnvelope, double threshold, int maxHorDivisions,
            int maxVertDivisions) throws TransformException, FactoryException {
        long t0 = System.currentTimeMillis();
        ReferencedEnvelope sourceREnvelope = reference(sourceEnvelope);
        ReferencedEnvelope targetREnvelope = reference(targetEnvelope);
        Geometry border = null;
        WorldWrapCorrector corrector = new WorldWrapCorrector(targetREnvelope);
        MathTransform sourceCRSToTargetCRS = CRS.findMathTransform(
                sourceREnvelope.getCoordinateReferenceSystem(),
                targetREnvelope.getCoordinateReferenceSystem());
        MathTransform targetCRSToLatLon = MapUtil
                .getTransformToLatLon(targetREnvelope
                        .getCoordinateReferenceSystem());

        double midY = sourceREnvelope.getMinimum(1)
                + (sourceREnvelope.getSpan(1) / 2.0);
        double[] UL = new double[] { sourceREnvelope.getMinimum(0),
                sourceREnvelope.getMinimum(1) };
        UL = findNearestValidPoint(UL,
                new double[] { sourceREnvelope.getMinimum(0), midY }, null,
                sourceCRSToTargetCRS, true);

        double[] UR = new double[] { sourceREnvelope.getMaximum(0),
                sourceREnvelope.getMinimum(1) };
        UR = findNearestValidPoint(UR,
                new double[] { sourceREnvelope.getMaximum(0), midY }, null,
                sourceCRSToTargetCRS, true);

        double[] LR = new double[] { sourceREnvelope.getMaximum(0),
                sourceREnvelope.getMaximum(1) };
        LR = findNearestValidPoint(LR,
                new double[] { sourceREnvelope.getMaximum(0), midY }, null,
                sourceCRSToTargetCRS, true);

        double[] LL = new double[] { sourceREnvelope.getMinimum(0),
                sourceREnvelope.getMaximum(1) };
        LL = findNearestValidPoint(LL,
                new double[] { sourceREnvelope.getMinimum(0), midY }, null,
                sourceCRSToTargetCRS, true);

        List<Coordinate> borderPoints = new ArrayList<Coordinate>(
                maxVertDivisions * 2 + maxHorDivisions * 2);
        double[] out = new double[2];

        // UL to UR
        sourceCRSToTargetCRS.transform(UL, 0, out, 0, 1);
        borderPoints.add(new Coordinate(out[0], out[1]));
        calculateBorder(borderPoints, UL, null, UR, null, maxHorDivisions,
                threshold, sourceCRSToTargetCRS);

        // UR to LR
        sourceCRSToTargetCRS.transform(UR, 0, out, 0, 1);
        borderPoints.add(new Coordinate(out[0], out[1]));
        calculateBorder(borderPoints, UR, null, LR, null, maxVertDivisions,
                threshold, sourceCRSToTargetCRS);

        // LR to LL
        sourceCRSToTargetCRS.transform(LR, 0, out, 0, 1);
        borderPoints.add(new Coordinate(out[0], out[1]));
        calculateBorder(borderPoints, LR, null, LL, null, maxHorDivisions,
                threshold, sourceCRSToTargetCRS);

        // LL to UL
        sourceCRSToTargetCRS.transform(LL, 0, out, 0, 1);
        borderPoints.add(new Coordinate(out[0], out[1]));
        calculateBorder(borderPoints, LL, null, UL, null, maxVertDivisions,
                threshold, sourceCRSToTargetCRS);

        // Add start point to complete linear ring
        sourceCRSToTargetCRS.transform(UL, 0, out, 0, 1);
        borderPoints.add(new Coordinate(out[0], out[1]));

        // Create valid continuous LineStrings for source border
        List<LineString> lineStrings = new ArrayList<LineString>();
        List<Coordinate> currString = new ArrayList<Coordinate>();
        boolean foundValid = false;
        for (Coordinate c : borderPoints) {
            if (!Double.isNaN(c.x) && !Double.isNaN(c.y)) {
                if (foundValid == false) {
                    foundValid = true;
                }
                currString.add(c);
            } else if (foundValid) {
                if (currString.size() > 1) {
                    lineStrings.add(gf.createLineString(currString
                            .toArray(new Coordinate[0])));
                    currString.clear();
                }
                foundValid = false;
            }
        }
        if (currString.size() > 1) {
            lineStrings.add(gf.createLineString(currString
                    .toArray(new Coordinate[0])));
        }

        MathTransform latLonToTargetCRS = targetCRSToLatLon.inverse();

        int numStrings = lineStrings.size();
        if (numStrings == 1) {
            // Check for one continuous line string that starts and ends at same
            // point, if so, attempt to create single polygon and correct it
            LineString ls = lineStrings.get(0);
            Coordinate[] coords = ls.getCoordinates();
            if (coords[0].equals(coords[coords.length - 1])) {
                border = gf.createPolygon(gf.createLinearRing(coords), null);
                border = JTS.transform(corrector.correct(JTS.transform(border,
                        targetCRSToLatLon)), latLonToTargetCRS);
            }
        }
        if ((border == null || border.isEmpty() || border.isValid() == false)
                && numStrings > 0) {
            // This may happen if more than one valid line string found or
            // correcting the single line string produced invalid geometries and
            // therefore the border is empty
            MathTransform targetCRSToSourceCRS = sourceCRSToTargetCRS.inverse();
            // Here we check for a simple case where entire target CRS is within
            // source CRS making border equivalent to target CRS border
            Coordinate ul = new Coordinate(targetREnvelope.getMinimum(0),
                    targetREnvelope.getMinimum(1));
            Coordinate ur = new Coordinate(targetREnvelope.getMaximum(0),
                    targetREnvelope.getMinimum(1));
            Coordinate lr = new Coordinate(targetREnvelope.getMaximum(0),
                    targetREnvelope.getMaximum(1));
            Coordinate ll = new Coordinate(targetREnvelope.getMinimum(0),
                    targetREnvelope.getMaximum(1));

            Polygon targetBorder = gf.createPolygon(gf
                    .createLinearRing(new Coordinate[] { ul, ur, lr, ll, ul }),
                    null);

            // Convert corner points of target envelope into source envelope
            // space
            boolean bad = false;
            try {
                double[] in = new double[] { ul.x, ul.y, ur.x, ur.y, lr.x,
                        lr.y, ll.x, ll.y };
                out = new double[in.length];
                targetCRSToSourceCRS.transform(in, 0, out, 0, 4);
                for (int i = 0, idx = 0; i < 4 && !bad; i++, idx += 2) {
                    if (sourceREnvelope.contains(out[idx], out[idx + 1]) == false) {
                        // if any point not within source envelope, this case is
                        // bad
                        bad = true;
                    }
                }
            } catch (TransformException e) {
                bad = true;
            }
            if (!bad) {
                // base case, use entire target border polygon
                border = targetBorder;
            } else {
                // Complicated case, take valid line strings and compute
                // intersection area with target border and combine all
                // intersections in collection
                LineSegment top = new LineSegment(ul, ur);
                LineSegment right = new LineSegment(ur, lr);
                LineSegment bottom = new LineSegment(ll, lr);
                LineSegment left = new LineSegment(ul, ll);
                LineSegment[] borderSegments = new LineSegment[] { top, right,
                        bottom, left };

                // First, world wrap correct the line strings
                List<LineString> corrected = new ArrayList<LineString>();
                for (LineString ls : lineStrings) {
                    extractLineStrings(corrected, JTS.transform(corrector
                            .correct(JTS.transform(ls, targetCRSToLatLon)),
                            latLonToTargetCRS));
                }

                // Second, connect any line strings that are continuous (start
                // point in one is end point in another)
                List<LineString> connected = new ArrayList<LineString>();
                // Start with first in corrected in connected
                connected.add(corrected.get(corrected.size() - 1));
                corrected.remove(connected.get(0));
                boolean done = false;
                while (!done) {
                    LineString geomA = null, geomB = null;
                    LineString newGeom = null;
                    // For each LineString in connected, check if any in
                    // corrected connect to it. If it does, remove from
                    // corrected, connected with it and add back into connected
                    // for more processing
                    for (LineString ls1 : connected) {
                        geomA = ls1;
                        for (LineString ls2 : corrected) {
                            geomB = ls2;
                            Coordinate[] c1 = ls1.getCoordinates();
                            Coordinate c1_0 = c1[0];
                            Coordinate c1_l = c1[c1.length - 1];
                            Coordinate[] c2 = ls2.getCoordinates();
                            Coordinate c2_0 = c2[0];
                            Coordinate c2_l = c2[c2.length - 1];
                            if (c1_0.equals(c2_l) || c1_l.equals(c2_0)) {
                                // ls1 and ls2 are connected, create new geom
                                if (c1_0.equals(c2_l)) {
                                    Coordinate[] tmp = c1;
                                    c1 = c2;
                                    c2 = tmp;
                                }
                                // These line strings can be connected
                                Coordinate[] newCoords = new Coordinate[c1.length
                                        + c2.length - 1];
                                for (int i = 0; i < c1.length; ++i) {
                                    newCoords[i] = c1[i];
                                }
                                for (int i = 1; i < c2.length; ++i) {
                                    newCoords[i + c1.length - 1] = c2[i];
                                }
                                newGeom = gf.createLineString(newCoords);
                                break;
                            }
                        }
                        if (newGeom != null) {
                            break;
                        }
                    }
                    if (newGeom != null) {
                        connected.remove(geomA);
                        corrected.remove(geomB);
                        connected.add(newGeom);
                        newGeom = null;
                    } else {
                        // Nothing found that can be connected, we are done
                        done = true;
                    }
                }
                corrected = connected;

                // Process all connected-corrected LineStrings into polygonal
                // intersections with the target border
                List<Geometry> borders = new ArrayList<Geometry>();
                for (Geometry correctedLs : corrected) {
                    if (targetBorder.intersects(correctedLs)) {
                        // Here we want to make sure there are points in the
                        // line string that intersects the tile border. This
                        // algorithm looks at first 2 and last 2 points in
                        // correctedLS and extrapolates to find intersections
                        // point with one of the LineSegments in the
                        // targetBorder. Those points are then added to the
                        // LineString
                        Coordinate[] lsCoords = correctedLs.getCoordinates();
                        List<Coordinate> lsACoords = new ArrayList<Coordinate>(
                                Arrays.asList(lsCoords));
                        LineSegment one = new LineSegment(lsCoords[1],
                                lsCoords[0]);
                        LineSegment two = new LineSegment(
                                lsCoords[lsCoords.length - 2],
                                lsCoords[lsCoords.length - 1]);
                        double bestProjectionFactorOne = 1.0;
                        double bestProjectionFactorTwo = 1.0;
                        Coordinate cOne = null, cTwo = null;
                        for (LineSegment ls : borderSegments) {
                            double factor = one.projectionFactor(ls
                                    .getCoordinate(0));
                            if (factor > bestProjectionFactorOne) {
                                cOne = one.pointAlong(factor);
                                bestProjectionFactorOne = factor;
                            }
                            factor = two.projectionFactor(ls.getCoordinate(0));
                            if (factor > bestProjectionFactorTwo) {
                                cTwo = two.pointAlong(factor);
                                bestProjectionFactorTwo = factor;
                            }
                        }
                        if (cOne != null) {
                            lsACoords.add(0, cOne);
                        }
                        if (cTwo != null) {
                            lsACoords.add(cTwo);
                        }
                        if (lsACoords.size() > lsCoords.length) {
                            // Points were added, recreate correctedLs
                            lsCoords = lsACoords.toArray(new Coordinate[0]);
                            correctedLs = gf.createLineString(lsCoords);
                        }

                        // Intersect with targetBorder to trim LineString
                        correctedLs = targetBorder.intersection(correctedLs);
                    } else {
                        System.err
                                .println("LineString lives completely outside target extent");
                        continue;
                    }

                    // Intersection of correctedLS with targetBorder might
                    // result in MultiLineString if correctedLS entered and
                    // exited targetBorder multiple times so we extract them
                    // here
                    List<LineString> correctedLsArray = new ArrayList<LineString>();
                    extractLineStrings(correctedLsArray, correctedLs);

                    for (LineString ls : correctedLsArray) {
                        // For each LineString, we know first and last
                        // coordinates are on the targetBorder so we difference
                        // targetBorder with the LineString to get a border with
                        // those points included. Then we can walk the border
                        // and create a polygon of the LineString and the
                        // border. At that point, we will either have a polygon
                        // where an interior point is within the sourceEnvelope
                        // or not. If not, we will use the targetBorder
                        // differenced with it to get the part that represents
                        // the sourceEnvelopes intersection
                        Coordinate[] boundaryCoords = targetBorder.difference(
                                ls).getCoordinates();
                        Coordinate[] lsCoords = ls.getCoordinates();
                        List<Coordinate> lsACoords = new ArrayList<Coordinate>(
                                Arrays.asList(lsCoords));
                        Coordinate first = lsCoords[0];
                        Coordinate last = lsCoords[lsCoords.length - 1];
                        // Find index of last in boundaryCoords
                        int idx = 0;
                        for (; idx < boundaryCoords.length; ++idx) {
                            if (last.equals(boundaryCoords[idx])) {
                                break;
                            }
                        }
                        int startIdx = idx;
                        done = idx == boundaryCoords.length;
                        if (done) {
                            System.err
                                    .println("Could not find intersection point in polygon "
                                            + "boundry for last point in LineString");
                            continue;
                        }
                        while (!done) {
                            // Append points to the LineString until we find
                            // first coordinate which indicates we are done
                            idx = (idx + 1) % boundaryCoords.length;
                            if (idx != startIdx) {
                                lsACoords.add(boundaryCoords[idx]);
                                if (boundaryCoords[idx].equals(first)) {
                                    done = true;
                                }
                            } else {
                                done = true;
                            }
                        }
                        if (idx == startIdx) {
                            System.err
                                    .println("Could not find intersection point in polygon "
                                            + "boundry for first point in LineString");
                            continue;
                        }

                        // Create polygon out of LineString points and check if
                        // it is the half that is within the source CRS
                        Polygon lsA = gf.createPolygon(gf
                                .createLinearRing(lsACoords
                                        .toArray(new Coordinate[0])), null);
                        Coordinate pointA = lsA.getInteriorPoint()
                                .getCoordinate();
                        targetCRSToSourceCRS.transform(new double[] { pointA.x,
                                pointA.y }, 0, out, 0, 1);
                        if (sourceREnvelope.contains(out[0], out[1])) {
                            borders.add(lsA);
                        } else {
                            borders.add(targetBorder.difference(lsA));
                        }
                    }
                }

                // Combine any borders that intersect to create large
                // intersecting areas
                done = false;
                while (!done) {
                    Geometry newGeom = null;
                    Geometry oldA = null, oldB = null;
                    for (Geometry g1 : borders) {
                        oldA = g1;
                        for (Geometry g2 : borders) {
                            if (g2 != oldA) {
                                oldB = g2;
                                if (oldB.intersects(oldA)) {
                                    newGeom = oldB.intersection(oldA);
                                    break;
                                }
                            }
                        }
                        if (newGeom != null) {
                            break;
                        }
                    }
                    if (newGeom != null) {
                        borders.remove(oldA);
                        borders.remove(oldB);
                        borders.add(newGeom);
                        newGeom = null;
                    } else {
                        done = true;
                    }
                }

                // We are done!
                if (borders.size() == 1) {
                    border = borders.get(0);
                } else {
                    border = gf.createGeometryCollection(borders
                            .toArray(new Geometry[0]));
                }
            }
        }

        System.out.println("Time to create EnvelopeIntersection: "
                + (System.currentTimeMillis() - t0) + "ms");
        return border;
    }

    private static void extractLineStrings(List<LineString> lines, Geometry geom) {
        if (geom instanceof GeometryCollection) {
            for (int n = 0; n < geom.getNumGeometries(); ++n) {
                extractLineStrings(lines, geom.getGeometryN(n));
            }
        } else if (geom instanceof LineString) {
            lines.add((LineString) geom);
        }
    }

    private static double[] findNearestValidPoint(double[] maxPoint,
            double[] point, double[] prevPoint, MathTransform mt,
            boolean checkMax) {
        if (checkMax) {
            try {
                double[] tmp = new double[maxPoint.length];
                mt.transform(maxPoint, 0, tmp, 0, 1);
                return maxPoint;
            } catch (TransformException e) {
                // Ignore
            }
            checkMax = false;
        }
        try {
            double[] tmp = new double[point.length];
            mt.transform(point, 0, tmp, 0, 1);
            // point is valid, keep looking
            double deltaX = (maxPoint[0] - point[0]) / 2.0;
            double deltaY = (maxPoint[1] - point[1]) / 2.0;
            double[] newPoint = new double[] { point[0] + deltaX,
                    point[1] + deltaY };
            return findNearestValidPoint(maxPoint, newPoint, point, mt,
                    checkMax);
        } catch (TransformException e) {
            // Ignore
        }
        return prevPoint;
    }

    private static int calculateBorder(List<Coordinate> borderList,
            double[] point1, double[] transformedPoint1, double[] point3,
            double[] transformedPoint3, double maxNumDivs, double threshold,
            MathTransform transform) throws TransformException {
        if (transformedPoint1 == null) {
            transformedPoint1 = new double[point1.length];
            for (int i = 0; i < point1.length; ++i) {
                transformedPoint1[i] = Double.NaN;
            }
            try {
                transform.transform(point1, 0, transformedPoint1, 0, 1);
            } catch (TransformException e) {
                // Eat exception, NaNs will do work for us
            }
        }
        if (transformedPoint3 == null) {
            transformedPoint3 = new double[point3.length];
            for (int i = 0; i < point3.length; ++i) {
                transformedPoint3[i] = Double.NaN;
            }
            try {
                transform.transform(point3, 0, transformedPoint3, 0, 1);
            } catch (TransformException e) {
                // Eat exception, NaNs will do work for us
            }
        }

        double[] point2 = { (point1[0] + point3[0]) / 2,
                (point1[1] + point3[1]) / 2 };
        double[] transformedPoint2 = new double[point2.length];
        transform.transform(point2, 0, transformedPoint2, 0, 1);
        double[] interp2 = { (transformedPoint1[0] + transformedPoint3[0]) / 2,
                (transformedPoint1[1] + transformedPoint3[1]) / 2 };
        double dX = transformedPoint2[0] - interp2[0];
        double dY = transformedPoint2[1] - interp2[1];
        double d = Math.hypot(dX, dY);
        if (d < threshold || maxNumDivs < 1) {
            return 1;
        } else {
            int nd1 = calculateBorder(borderList, point1, transformedPoint1,
                    point2, transformedPoint2, maxNumDivs / 2, threshold,
                    transform);
            borderList.add(new Coordinate(transformedPoint2[0],
                    transformedPoint2[1]));
            if (nd1 * 2 >= maxNumDivs) {
                nd1 = (int) Math.ceil(maxNumDivs);
            }
            int nd2 = calculateBorder(borderList, point2, transformedPoint2,
                    point3, transformedPoint3, maxNumDivs / 2, threshold,
                    transform);
            if (nd2 * 2 >= maxNumDivs) {
                nd2 = (int) Math.ceil(maxNumDivs);
            }
            return (Math.max(nd1, nd2) * 2);
        }
    }

    private static ReferencedEnvelope reference(Envelope envelope) {
        if (envelope instanceof ReferencedEnvelope) {
            return (ReferencedEnvelope) envelope;
        }
        return new ReferencedEnvelope(envelope);
    }
}
