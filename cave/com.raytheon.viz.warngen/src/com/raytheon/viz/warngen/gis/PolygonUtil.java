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
package com.raytheon.viz.warngen.gis;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.warning.util.GeometryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.contours.util.ContourContainer;
import com.raytheon.viz.core.contours.util.FortConBuf;
import com.raytheon.viz.core.contours.util.FortConConfig;
import com.raytheon.viz.warngen.gui.WarngenLayer;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryCollection;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineSegment;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.PrecisionModel;
import org.locationtech.jts.geom.TopologyException;
import org.locationtech.jts.geom.prep.PreparedGeometry;
import org.locationtech.jts.geom.prep.PreparedGeometryFactory;
import org.locationtech.jts.precision.SimpleGeometryPrecisionReducer;

/**
 * Utility for polygon operations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2010            mschenke     Initial creation
 * 12/06/2012   DR 15559  Qinglu Lin   Added round() methods.
 * 04/16/2013   DR 16045  Qinglu Lin   Relocated removeDuplicateCoordinate(), computeSlope(), 
 *                                     computeCoordinate(), and adjustPolygon from WarngenUIState.
 * 05/23/2013  DR 16169   D. Friedman  Improve redraw-from-hatched-area polygons.
 * 06/17/2013  DR 15787   Qinglu Lin   Added removeOverlaidLinesegments() and removeTriplyOverlaidLinesegments().
 * 07/11/2013  DR 16376   Qinglu Lin   Removed removeTriplyOverlaidLinesegments() and updated computeSlope()
 *                                     and removeOverlaidLinesegments().
 * 07/25/2013  DR 16376   Qinglu Lin   Move adjustVertex() and computeSlope() here from WarngenLayer; replaced
 *                                     the call to removeIntersectedSeg() with a call to adjustVertex(); updated 
 *                                     removeDuplicateCoordinate(), computeCoordinate(), adjustPolygon() prolog, and
 *                                     removeOverlaidLinesegments(); added alterVertexes() and calcShortestDistance().
 * 10/01/2013  DR 16632   Qinglu Lin   Fixed the bug in for loop range.
 * 10/17/2013  DR 16632   Qinglu Lin   Updated removeOverlaidLinesegments().
 * 10/18/2013  DR 16632   Qinglu Lin   Catch exception thrown when coords length is less than 4 and doing createLinearRing(coords).
 * 12/17/2013  DR 16567   Qinglu Lin   Added createPolygonByPoints().
 * 01/09/2014  DR 16974   D. Friedman  Improve followup redraw-from-hatched-area polygons.
 * 04/15/2014  DR 17247   D. Friedman  Prevent some invalid coordinates in adjustVertex.
 * 05/16/2014  DR 17365   D. Friedman  Prevent some Coordinate reuse. Add reducePrecision.
 * 06/27/2014  DR 17443   D. Friedman  Fix some odd cases in which parts of a polygon not covering a
 *                                     hatched area would be retained after redrawing.
 * 07/22/2014  DR 17475   Qinglu Lin   Updated createPolygonByPoints() and created second createPolygonByPoints().
 * 04/02/2015  DR 4353    dgilling     Fix bad exception handler in hatchWarningArea.
 * 04/29/2015  DR 17310   D. Friedman  Use Geometry.buffer() to fix self-intersections.  Fix bug in alterVertexes.
 * 05/07/2015  DR 17438   D. Friedman  Clean up debug and performance logging.
 * 05/08/2015  DR 17310   D. Friedman  Prevent reducePoints from generating invalid polygons.
 * 09/22/2015  DR 18033   Qinglu Lin   Updated removeOverlaidLinesegments(), removed one computeSlope().
 * 12/09/2015  DR 18209   D. Friedman  Support cwaStretch.
 * 12/21/2015  DCS 17942  D. Friedman  Support extension area.  Work around glitch in contour adjustment.
 * 01/11/2016  DR 18474   D. Friedman  Do not allow expansion of polygon into extension area on followup.
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PolygonUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PolygonUtil.class);

    private WarngenLayer layer;

    private int nx, ny;

    private int maxVertices;

    private MathTransform latLonToContour, contourToLatLon;

    private static final PrecisionModel REDUCED_PRECISION = new PrecisionModel(
            10000000000.0);

    public PolygonUtil(WarngenLayer layer, int nx, int ny, int maxVertices,
            IExtent localExtent, MathTransform localToLatLon) throws Exception {
        this.layer = layer;
        this.nx = nx;
        this.ny = ny;
        this.maxVertices = maxVertices;

        GeneralEnvelope ge = new GeneralEnvelope(new double[] {
                localExtent.getMinX(), localExtent.getMaxY() }, new double[] {
                localExtent.getMaxX(), localExtent.getMinY() });
        GeneralGridEnvelope range = new GeneralGridEnvelope(new int[] { 0, 0 },
                new int[] { nx, ny }, false);
        contourToLatLon = new DefaultMathTransformFactory()
                .createConcatenatedTransform(new GridGeometry2D(range, ge)
                        .getGridToCRS(PixelOrientation.CENTER), localToLatLon);
        latLonToContour = contourToLatLon.inverse();
    }

    public Polygon hatchWarningArea(Polygon origPolygon,
            Geometry origWarningArea, Geometry extensionArea,
            Polygon oldWarningPolygon, boolean cwaStretch)
            throws VizException {
        float[][] contourAreaData = toFloatData(origWarningArea);
        if (extensionArea != null) {
            if (oldWarningPolygon != null) {
                extensionArea = GeometryUtil.intersection(extensionArea, oldWarningPolygon);
            }
            toFloatData(extensionArea, contourAreaData);
        }

        /*
         * If we have an oldWarningPolygon, we can take a shortcut and see if
         * the intersection of the current polygon and the old polygon generates
         * the same input to the contouring algorithm as the current hatched
         * area. If it does, that intersection can be used instead of generating
         * a new contour.
         */
        if (oldWarningPolygon != null && oldWarningPolygon.isValid()
                && origPolygon.isValid()) {
            try {
                /*
                 * Create a clone to ensure we do not use a Coordinate from
                 * oldWarningPolygon.
                 */
                Geometry intersection = (Geometry) origPolygon.intersection(
                        oldWarningPolygon).clone();
                if (intersection instanceof Polygon) {
                    Polygon polygonIntersection = (Polygon) intersection;
                    if (polygonIntersection.isValid()
                            && polygonIntersection.getNumInteriorRing() == 0
                            && polygonIntersection.getNumPoints() - 1 <= maxVertices) {
                        /*
                         * Use buildIdealArea to clip the current polygon
                         * against the old polygon (actually oldWarningArea) and
                         * the CWA, using the same coordinate transformations
                         * that are used to generate origWarningArea.
                         */
                        Geometry comparableIntersection = layer
                                .buildIdealArea(origPolygon, cwaStretch);
                        float[][] interAreaData = toFloatData(comparableIntersection);
                        if (areasEqual(interAreaData, contourAreaData)) {
                            return polygonIntersection;
                        }
                    }
                }
            } catch (Exception e) {
                statusHandler
                        .handle(Priority.WARN,
                                "Error while using simple polygon redraw method.  Will continue using contouring method.",
                                e);
            }
        }

        // Create contouring configuration
        FortConConfig config = new FortConConfig();
        config.generateMaxes = false;
        config.generateMins = false;
        config.labelFormat = null;
        config.seed = new float[] { 0.49999f };
        config.mode = 1;
        config.badlo = 99999;
        config.badhi = 99998;

        // Contour the image
        ContourContainer container = FortConBuf
                .contour(contourAreaData, config);
        List<float[]> pts = container.xyContourPoints;
        List<Coordinate[]> areaContours = toCoordinateList(pts);

        if (areaContours.size() > 1) {
            // More than one contour generated, connect them
            connectA1Port(areaContours, contourAreaData, config.seed[0] * 1.1f,
                    ny);

            // Recontour with disconnects
            container = FortConBuf.contour(contourAreaData, config);
            pts = container.xyContourPoints;
            areaContours = toCoordinateList(pts);
        }

        // Find longest contour...
        Coordinate[] contour = null;
        int size = 0;
        for (Coordinate[] f : areaContours) {
            if (f.length > size) {
                contour = f;
                size = f.length;
            }
        }

        Polygon rval = null;
        // Flag for debuging to see generated contour before point reduction
        boolean showContour = false;
        if (contour != null && !showContour) {
            rval = awips1PointReduction(contour, origPolygon, origWarningArea,
                    extensionArea, config, oldWarningPolygon, contourAreaData);
            if (rval == null) {
                return (Polygon) origPolygon.clone();
            }
        } else if (contour != null) {
            // Create a polygon from the contour
            GeometryFactory gf = new GeometryFactory();
            Coordinate[] coords = contour;
            coords = Arrays.copyOf(coords, coords.length + 1);
            coords[coords.length - 1] = new Coordinate(coords[0]);
            truncate(coords, 2);
            rval = gf.createPolygon(gf.createLinearRing(coords), null);
        }
        // Convert back to lat/lon
        return layer.convertGeom(rval, contourToLatLon);
    }

    /**
     * @return null if the original warningPolygon should be used
     */
    private Polygon awips1PointReduction(Coordinate[] longest,
            Polygon warningPolygon, Geometry warningArea,
            Geometry extensionArea, FortConConfig config,
            Polygon oldWarningPolygon, float[][] warningAreaData)
            throws VizException {
        if (extensionArea != null) {
            /*
             * Attempt to avoid a glitch in the code below in which it chooses
             * an inappropriate side of the polygon on which to project an
             * unmatched contour point. The glitch is likely to occur when a
             * polygon point is outside the contour space, so clip the polygon
             * to it.
             */
            Polygon wpc = WarngenLayer.convertGeom(warningPolygon, latLonToContour);
            GeometryFactory gf = new GeometryFactory();
            Coordinate[] coords = new Coordinate[5];
            coords[0] = new Coordinate(0, 0);
            coords[1] = new Coordinate(nx, 0);
            coords[2] = new Coordinate(nx, ny);
            coords[3] = new Coordinate(0, ny);
            coords[4] = new Coordinate(0, 0);
            Polygon clip = gf.createPolygon(gf.createLinearRing(coords), null);
            Geometry g = clip.intersection(wpc);
            if (g instanceof Polygon) {
                warningPolygon = WarngenLayer.convertGeom((Polygon) g, contourToLatLon);
            }
        }
        Coordinate[] vertices = warningPolygon.getCoordinates();
        vertices = Arrays.copyOf(vertices, vertices.length - 1);

        // Extract data
        float[][] contourPolyData = toFloatData(warningPolygon);
        float[][] currentPolyData = warningAreaData;

        // If same area is hatched, just use the current polygon.
        if (areasEqual(contourPolyData, currentPolyData)) {
            /*
             * If the polygon is an intersection between what the user drew and
             * the original polygon from a previous product, we may still want
             * to reduce the number of points...
             */
            if (oldWarningPolygon != null) {
                Polygon p = removeCollinear(warningPolygon);
                return layer.convertGeom(p, latLonToContour);
            } else {
                return null;
            }
        } else if (oldWarningPolygon != null
                && areasEqual(toFloatData(oldWarningPolygon), currentPolyData)) {
            return layer.convertGeom(oldWarningPolygon, latLonToContour);
        }

        // Contour the polygon
        ContourContainer container = FortConBuf
                .contour(contourPolyData, config);
        int npoints = longest.length;
        char[] fixed = new char[npoints];
        List<Coordinate[]> polyContours = toCoordinateList(container.xyContourPoints);
        if (polyContours.size() > 0) {
            Coordinate[] polyContour = polyContours.get(0);
            double xAvg = 0, yAvg = 0;
            for (Coordinate c : polyContour) {
                xAvg += c.x;
                yAvg += c.y;
            }
            Coordinate avg = new Coordinate(xAvg / polyContour.length, yAvg
                    / polyContour.length);

            // Index the raw polygon points into a hash table with 50
            // elements in
            // each octant of angle around the centroid
            int[] ph = new int[400];
            Arrays.fill(ph, -1);
            int[] nexth = new int[polyContour.length];
            Arrays.fill(nexth, -1);
            double dx, dy, da, dn;
            double hm = -49.999;
            double hp = 49.999;
            int i = 0;
            for (int k = 0; k < polyContour.length; ++k) {
                Coordinate c = polyContour[k];
                dx = c.x - avg.x;
                dy = c.y - avg.y;
                if (dx == 0 && dy == 0) {
                    i = 0;
                } else if (dx < 0) {
                    if (dy < 0) {
                        i = dx < dy ? (int) (hp * dy / dx) : 50 + (int) (hp
                                * dx / dy);
                    } else {
                        i = -dx > dy ? 100 + (int) (hm * dy / dx)
                                : 150 + (int) (hm * dx / dy);
                    }
                } else {
                    if (dy < 0) {
                        i = dx > -dy ? 200 + (int) (hm * dy / dx)
                                : 250 + (int) (hm * dx / dy);
                    } else {
                        i = dx > dy ? 300 + (int) (hp * dy / dx)
                                : 350 + (int) (hp * dx / dy);
                    }
                }
                nexth[k] = ph[i];
                ph[i] = k;
            }

            // Use the hash table as a fast way to record those points
            // in the
            // detailed warned area that are also in the detailed
            // polygon.
            // -1 means no match, vertices.length() means ambiguous
            // match.
            int nv = vertices.length;
            int[] match = new int[npoints];
            int j, k;
            for (j = k = 0; k < npoints; k++) {
                Coordinate kPt = longest[k];
                dx = kPt.x - avg.x;
                dy = kPt.y - avg.y;
                match[k] = -1;
                if (dx == 0 && dy == 0) {
                    i = 0;
                } else if (dx < 0) {
                    if (dy < 0) {
                        i = dx < dy ? (int) (hp * dy / dx) : 50 + (int) (hp
                                * dx / dy);
                    } else {
                        i = -dx > dy ? 100 + (int) (hm * dy / dx)
                                : 150 + (int) (hm * dx / dy);
                    }
                } else {
                    if (dy < 0) {
                        i = dx > -dy ? 200 + (int) (hm * dy / dx)
                                : 250 + (int) (hm * dx / dy);
                    } else {
                        i = dx > dy ? 300 + (int) (hp * dy / dx)
                                : 350 + (int) (hp * dx / dy);
                    }
                }
                i = ph[i];
                for (; i >= 0; i = nexth[i]) {
                    if (kPt.x == polyContour[i].x && kPt.y == polyContour[i].y) {
                        match[k] = nv;
                        j++;
                        break;
                    }
                }
            }

            if (j > 0) {
                // Compute each vertex in the base polygon in contour
                // index space; also
                // compute the length and unit vector along each side of
                // base polygon.
                Coordinate[] vert = new Coordinate[nv * 2];
                for (i = 0, j = nv; i < nv; i++, j++) {
                    vert[i] = vert[j] = layer.convertGeom(vertices[i],
                            latLonToContour);
                }

                double[] ux = new double[nv];
                double[] uy = new double[nv];
                double[] vlen = new double[nv];
                double[] totlen = new double[nv + 1];
                totlen[0] = 0;
                for (i = 0; i < nv; i++) {
                    dx = vert[i + 1].x - vert[i].x;
                    dy = vert[i + 1].y - vert[i].y;
                    vlen[i] = Math.sqrt(dx * dx + dy * dy);
                    ux[i] = dx / vlen[i];
                    uy[i] = dy / vlen[i];
                    totlen[i + 1] = totlen[i] + vlen[i];
                }
                double len2 = totlen[nv] / 2;

                // For each match between the detailed warned area and
                // the detailed
                // raw polygon, note the base polygon side it belongs to
                // if unambiguous.
                // Ambiguous points next to unmatched points are
                // considered unmatched.
                for (k = 0; k < npoints; k++) {
                    if (match[k] < 0) {
                        continue;
                    }
                    for (i = 0; i < nv; ++i) {
                        dx = longest[k].x - vert[i].x;
                        dy = longest[k].y - vert[i].y;
                        da = dx * ux[i] + dy * uy[i];
                        if (da < 0 || da > vlen[i]) {
                            continue;
                        }
                        dn = dy * ux[i] - dx * uy[i];
                        if (dn < -1 || dn > 1) {
                            continue;
                        }
                        if (match[k] < nv) {
                            match[k] = nv;
                            break;
                        }
                        match[k] = i;
                    }
                }

                for (j = npoints - 1, k = 0; k < npoints; j = k++) {
                    if (match[j] < 0 && match[k] == nv) {
                        match[k] = -1;
                    }
                }
                for (j = 0, k = npoints - 1; k >= 0; j = k--) {
                    if (match[j] < 0 && match[k] == nv) {
                        match[k] = -1;
                    }
                }

                // Now we replace matched points with ones in the
                // original base polygon
                int p1 = npoints - 1;
                while (p1 >= 0 && match[p1] == nv) {
                    p1--;
                }

                if (p1 >= 0) {
                    Coordinate last = new Coordinate();
                    int n, best1, best2;
                    for (n = k = 0; k < npoints; k++) {
                        if (match[k] == nv) {
                            continue;
                        }
                        best1 = match[p1];
                        best2 = match[k];
                        last.x = longest[p1].x;
                        last.y = longest[p1].y;
                        p1 = k;
                        if (best1 < 0 && best2 < 0) {
                            if (k == n) {
                                continue;
                            }
                            fixed[n] = 0;
                            longest[n++] = new Coordinate(longest[k]);
                            continue;
                        }
                        if (best1 == best2) {
                            continue;
                        }
                        if (best1 < 0 || best2 < 0) {
                            i = best1 > best2 ? best1 : best2;
                            dx = longest[k].x - vert[i].x;
                            dy = longest[k].y - vert[i].y;
                            da = dx * ux[i] + dy * uy[i];
                            fixed[n] = 1;
                            longest[n++] = new Coordinate(vert[i].x + da
                                    * ux[i], vert[i].y + da * uy[i]);
                            continue;
                        }
                        if (best2 - best1 == 1 || best1 - best2 == nv - 1) {
                            i = best2;
                        } else if (best1 - best2 == 1
                                || best2 - best1 == nv - 1) {
                            i = best1;
                        } else {
                            dx = longest[k].x - vert[best1].x;
                            dy = longest[k].y - vert[best1].y;
                            da = dx * ux[best1] + dy * uy[best1]
                                    + totlen[best1];
                            dx = longest[k].x - vert[best2].x;
                            dy = longest[k].y - vert[best2].y;
                            dn = dx * ux[best2] + dy * uy[best2]
                                    + totlen[best2];
                            if (dn - da > len2) {
                                best1 += nv;
                            } else if (dn - da < -len2) {
                                best2 += nv;
                            }

                            /*
                             * We have apparently jumped from side best1 to side
                             * best2. Should we add all of the original
                             * vert[best1] -> vert[best2] vertices? If this is
                             * significantly longer then the distance from the
                             * last contour point to this one, it is probably
                             * the wrong thing to do.
                             * 
                             * The factor of 3 assumes that all points along the
                             * contour we ware redrawing are fairly close so
                             * that three times the length from any one point to
                             * another is not very long.
                             */
                            double maxPatchLen = last.distance(longest[k]) * 3;
                            double patchLen = 0;

                            int va = Math.min(best1, best2);
                            int vb = Math.max(best1, best2);
                            if (va < nv) {
                                patchLen = totlen[Math.min(nv, vb)]
                                        - totlen[va];
                                if (vb >= nv) {
                                    patchLen += totlen[vb % nv];
                                }
                            } else {
                                patchLen = totlen[vb % nv] - totlen[va % nv];
                            }

                            if (patchLen >= maxPatchLen) {
                                /*
                                 * Adding all of the other vertices would be
                                 * going of the rails, so just add a vertex for
                                 * the last point (since it may be far from
                                 * vert[best1] and the current point. Only add
                                 * one point if adding two would cause the
                                 * output in longest to overtake the input.
                                 */
                                if (n + 1 < k) {
                                    fixed[n] = 1;
                                    longest[n++] = new Coordinate(last);
                                }
                                fixed[n] = 1;
                                longest[n++] = longest[k];
                            } else if (best1 < best2) {
                                for (best1++; best1 <= best2; best1++) {
                                    fixed[n] = 1;
                                    longest[n++] = new Coordinate(vert[best1]);
                                }
                            } else {
                                for (; best1 > best2; best1--) {
                                    fixed[n] = 1;
                                    longest[n++] = new Coordinate(vert[best1]);
                                }
                            }
                            continue;
                        }
                        fixed[n] = 1;
                        longest[n++] = new Coordinate(vert[i]);
                    }
                    if (n > 0) {
                        npoints = n;
                    }
                }

            }
        }

        longest = Arrays.copyOf(longest, npoints);

        // Fill in largest contour
        List<Coordinate> points = new ArrayList<Coordinate>(
                Arrays.asList(longest));
        reducePoints(points, maxVertices);
        while (points.size() > vertices.length && reducePoints2(points)) {
            reducePoints(points, maxVertices);
        }

        GeometryFactory gf = new GeometryFactory();
        points.add(new Coordinate(points.get(0)));
        Polygon rval = gf.createPolygon(gf.createLinearRing(points
                .toArray(new Coordinate[points.size()])), null);

        if (!rval.isValid()) {
            statusHandler.handle(Priority.DEBUG, String.format(
                    "Polygon %s is invalid.  Attempting to fix...", rval));
            String resultMessage = null;
            try {
                Polygon p2 = (Polygon) rval.buffer(0.0);
                rval = gf.createPolygon((LinearRing) p2.getExteriorRing());
                resultMessage = String.format("  ...fixed.  Result: %s", rval);
            } catch (TopologyException e) {
                resultMessage = "  ...fix failed";
            } catch (ClassCastException e) {
                resultMessage = "  ...resulted in something other than a polygon";
            }
            statusHandler.handle(Priority.DEBUG, resultMessage);
        }

        if (rval.isValid() == false) {
            statusHandler.handle(Priority.DEBUG, "Fixing intersected segments");
            Coordinate[] coords = rval.getCoordinates();
            adjustVertex(coords);
            PolygonUtil.round(coords, 2);
            coords = PolygonUtil.removeDuplicateCoordinate(coords);
            coords = PolygonUtil.removeOverlaidLinesegments(coords);
            rval = gf.createPolygon(gf.createLinearRing(coords), null);
        }
        return rval;
    }

    private boolean areasEqual(float[][] a, float[][] b) {
        if (a.length != b.length) {
            return false;
        }
        for (int r = 0; r < a.length; ++r) {
            if (!Arrays.equals(a[r], b[r])) {
                return false;
            }
        }
        return true;
    }

    private List<Coordinate[]> toCoordinateList(List<float[]> pts) {
        List<Coordinate[]> contours = new ArrayList<Coordinate[]>(pts.size());
        for (float[] f : pts) {
            Coordinate[] coords = new Coordinate[f.length / 2];
            for (int i = 0, j = 0; i < f.length; i += 2, j++) {
                coords[j] = new Coordinate(f[i], f[i + 1]);
            }
            contours.add(coords);
        }
        return contours;
    }

    private void connectA1Port(List<Coordinate[]> polygons, float[][] data,
            float cnctval, int ny) {
        Coordinate[] avgs = new Coordinate[polygons.size()];
        Coordinate[] largest = null;
        for (int i = 0; i < polygons.size(); ++i) {
            Coordinate[] p = polygons.get(i);
            if (largest == null || p.length > largest.length) {
                largest = p;
            }
            double xAvg = 0;
            double yAvg = 0;
            for (Coordinate c : p) {
                xAvg += c.x;
                yAvg += c.y;
            }

            xAvg /= p.length;
            yAvg /= p.length;

            avgs[i] = new Coordinate(xAvg, yAvg);
        }

        List<Coordinate[]> newAreas = new ArrayList<Coordinate[]>();
        newAreas.add(largest);
        int i, j, k, i1, i2, j1, j2, dc;
        double dbest, d2, dx, dy;
        // Need connecting...
        for (int a = 0; a < polygons.size(); ++a) {
            Coordinate[] polyCoords = polygons.get(a);
            if (polyCoords == largest) {
                continue;
            }
            Coordinate avg = avgs[a];
            j = k = 0;
            dbest = d2 = 1e37;
            Coordinate c = largest[k];
            dx = c.x - avg.x;
            dy = c.y - avg.y;
            d2 = dx * dx + dy * dy;
            dbest = Math.sqrt(d2);
            for (k++; k < largest.length; ++k) {
                c = largest[k];
                dx = c.x - avg.x;
                if (dx < -dbest || dx > dbest) {
                    continue;
                }
                dy = c.y - avg.y;
                if (dy < -dbest || dy > dbest) {
                    continue;
                }
                dx = dx * dx + dy * dy;
                if (dx > d2) {
                    continue;
                }
                d2 = dx;
                dbest = Math.sqrt(d2);
                j = k;
            }
            dbest = d2 = 1e37;
            // j is now closest largest index to avgs[a]
            Coordinate closest = largest[j];
            i = k = 0;
            c = polyCoords[k];
            dx = c.x - closest.x;
            dy = c.y - closest.y;
            d2 = dx * dx + dy * dy;
            dbest = Math.sqrt(d2);
            for (k++; k < polyCoords.length; ++k) {
                c = polyCoords[k];
                dx = c.x - closest.x;
                if (dx < -dbest || dx > dbest) {
                    continue;
                }
                dy = c.y - closest.y;
                if (dy < -dbest || dy > dbest) {
                    continue;
                }
                dx = dx * dx + dy * dy;
                if (dx > d2) {
                    continue;
                }
                d2 = dx;
                dbest = Math.sqrt(d2);
                i = k;
            }
            dx = largest[j].x - polyCoords[i].x;
            if (dx > 0) {
                i1 = (int) (largest[j].x + 0.5);
                i2 = (int) (polyCoords[i].x - 0.5);
            } else {
                i1 = (int) (largest[j].x - 0.5);
                i2 = (int) (polyCoords[i].x + 0.5);
            }

            dx = largest[j].y - polyCoords[i].y;
            if (dy < 0) {
                j1 = (int) (largest[j].y + 0.5);
                j2 = (int) (polyCoords[i].y - 0.5);
            } else {
                j1 = (int) (largest[j].y - 0.5);
                j2 = (int) (polyCoords[i].y + 0.5);
            }

            if (j1 < j2) {
                if (i1 < i2) {
                    if (i2 - i1 > j2 - j1) {
                        disconnectX(i1, i2, j1, j2, data, cnctval);
                    } else {
                        disconnectY(i1, i2, j1, j2, data, cnctval);
                    }
                    continue;
                } else if (i1 > i2) {
                    if (i1 - i2 > j2 - j1) {
                        disconnectX(i2, i1, j2, j1, data, cnctval);
                    } else {
                        disconnectY(i1, i2, j1, j2, data, cnctval);
                    }
                    continue;
                }
                dc = 1;
            } else if (j1 > j2) {
                if (i1 < i2) {
                    if (i2 - i1 > j1 - j2) {
                        disconnectX(i1, i2, j1, j2, data, cnctval);
                    } else {
                        disconnectY(i2, i1, j2, j1, data, cnctval);
                    }
                    continue;
                } else if (i1 > i2) {
                    if (i1 - i2 > j1 - j2) {
                        disconnectX(i2, i1, j2, j1, data, cnctval);
                    } else {
                        disconnectY(i2, i1, j2, j1, data, cnctval);
                    }
                    continue;
                }
                dc = -1;
            } else if (i2 > i1) {
                dc = ny;
            } else {
                dc = -ny;
            }

            int c1 = 11 * ny + j1;
            int c2 = 12 * ny + j2 + dc;
            for (; c1 != c2; c1 += dc) {
                i = c1 / ny;
                j = c1 % ny;
                if (data[i][j] == 0.0f) {
                    data[i][j] = cnctval;
                }
            }
        }
    }

    /**
     * Digitizes an angular line longer in x than y into a float array.
     * 
     * @param i1
     * @param i2
     * @param j1
     * @param j2
     * @param array
     * @param val
     */
    private void disconnectX(int i1, int i2, int j1, int j2, float[][] array,
            float val) {
        float slope = (float) (j2 - j1) / (float) (i2 - i1);
        float intcp = 0.5f + j1 - slope * i1;
        array[i1][j1] = val;
        for (int i = i1; i <= i2; i++) {
            if (array[i][j1] == 0) {
                array[i][j1] = val;
            }
            j2 = (int) (slope * i + intcp);
            if (j2 == j1) {
                continue;
            }
            j1 = j2;
            if (array[i][j1] == 0) {
                array[i][j1] = val;
            }
        }
        array[i2][j2] = val;
    }

    /**
     * Digitizes an angular line longer in y than x into a float array.
     * 
     * @param i1
     * @param i2
     * @param j1
     * @param j2
     * @param array
     * @param val
     */
    private void disconnectY(int i1, int i2, int j1, int j2, float[][] array,
            float val) {
        float slope = (float) (i2 - i1) / (float) (j2 - j1);
        float intcp = 0.5f + i1 - slope * j1;
        array[i1][j1] = val;
        for (int j = j1; j <= j2; j++) {
            if (array[i1][j] == 0) {
                array[i1][j] = val;
            }
            i2 = (int) (slope * j + intcp);
            if (i2 == i1) {
                continue;
            }
            i1 = i2;
            if (array[i1][j] == 0) {
                array[i1][j] = val;
            }
        }
        array[i2][j2] = val;
    }

    /**
     * A1 ported point reduction method
     * 
     * @param points
     * @param maxNpts
     * @return
     */
    private void reducePoints(List<Coordinate> points, int maxNpts) {
        Coordinate[] pts = points.toArray(new Coordinate[points.size()]);
        // Find the mean, the point furthest from mean, and the point furthest
        // from that.
        int npts = pts.length;
        double xavg = 0, yavg = 0;
        int[] yesList = new int[npts];
        boolean[] excludeList = new boolean[npts];
        int nyes = 0;
        int k, k1, k2, kn, y, simple;
        double bigDis, maxDis, dis, dx, dy, dx0, dy0, bas;
        for (k = 0; k < npts; k++) {
            xavg += pts[k].x;
            yavg += pts[k].y;
        }
        xavg /= npts;
        yavg /= npts;
        k1 = -1;
        maxDis = 0;
        for (k = 0; k < npts; k++) {
            dx = pts[k].x - xavg;
            dy = pts[k].y - yavg;
            dis = dx * dx + dy * dy;
            if (dis < maxDis) {
                continue;
            }
            maxDis = dis;
            k1 = k;
        }
        k2 = -1;
        maxDis = 0;
        for (k = 0; k < npts; k++) {
            dx = pts[k].x - pts[k1].x;
            dy = pts[k].y - pts[k1].y;
            dis = dx * dx + dy * dy;
            if (dis < maxDis) {
                continue;
            }
            maxDis = dis;
            k2 = k;
        }
        nyes = 2;
        if (k1 < k2) {
            yesList[0] = k1;
            yesList[1] = k2;
        } else {
            yesList[0] = k2;
            yesList[1] = k1;
        }
        dx = pts[k2].x - xavg;
        dy = pts[k2].y - yavg;
        bigDis = Math.sqrt(dx * dx + dy * dy);

        // In each pass we will include that point furthest off the midline
        // of the two points. We will always first consider those that are
        // not simple bends.
        while (nyes < maxNpts && nyes < npts) {
            simple = 1;
            maxDis = 0;
            kn = -1;
            k1 = yesList[nyes - 1];
            for (y = 0; y < nyes; y++) {
                k2 = yesList[y];
                dx0 = pts[k2].x - pts[k1].x;
                dy0 = pts[k2].y - pts[k1].y;
                bas = Math.sqrt(dx0 * dx0 + dy0 * dy0);
                dx0 /= bas;
                dy0 /= bas;
                k = k1;
                while (true) {
                    if (++k >= npts) {
                        k = 0;
                    }
                    if (k == k2) {
                        break;
                    }

                    if (excludeList[k])
                        continue;
                    dx = pts[k].x - pts[k1].x;
                    dy = pts[k].y - pts[k1].y;
                    dis = dx * dx0 + dy * dy0;
                    if (dis < 0) {
                        dis = -dis;
                    } else {
                        dis -= bas;
                    }
                    double newMaxDis = maxDis;
                    int newSimple = simple;
                    if (dis <= 0) {
                        if (simple == 0) {
                            continue;
                        }
                        dis = dx * dy0 - dy * dx0;
                        if (dis < 0) {
                            dis = -dis;
                        }
                    } else if (simple != 0) {
                        newMaxDis = newSimple = 0;
                    }
                    if (dis < newMaxDis) {
                        maxDis = newMaxDis;
                        continue;
                    }
                    if (! checkReducePointsValid(pts, yesList, nyes, k)) {
                        excludeList[k] = true;
                        continue;
                    }
                    simple = newSimple;
                    maxDis = dis;
                    kn = k;
                }
                k1 = k2;
            }

            Arrays.fill(excludeList, false);
            if (kn < 0) {
                statusHandler.debug(
                        String.format("reducePoints(..., %d): Unable to find a valid point\npoints: %s",
                                maxNpts, points));
                break;
            }

            if (simple != 0 && nyes > 2) {
                if (maxDis * 40 < bigDis) {
                    break;
                }
                dis = (4.0 * nyes / maxNpts) - 1;
                if (maxDis < dis) {
                    break;
                }
            }

            for (y = nyes - 1; y >= 0 && kn < yesList[y]; y--) {
                yesList[y + 1] = yesList[y];
            }
            nyes++;
            yesList[y + 1] = kn;
        }

        for (y = 0; y < nyes; y++) {
            k = yesList[y];
            pts[y] = new Coordinate(pts[k]);
        }
        npts = nyes;
        points.clear();
        points.addAll(Arrays.asList(Arrays.copyOf(pts, npts)));
    }

    private boolean checkReducePointsValid(Coordinate[] pts, int[] yesList, int nyes, int k) {
        Coordinate[] verts = new Coordinate[nyes + 2];
        int vi = 0;
        for (int i = 0; i < nyes; ++i) {
            if (k >= 0 && k < yesList[i]) {
                verts[vi++] = pts[k];
                k = -1;
            }
            verts[vi++] = pts[yesList[i]];
        }
        if (k >= 0) {
            verts[vi++] = pts[k];
        }
        verts[verts.length - 1] = new Coordinate(verts[0]);
        GeometryFactory gf = new GeometryFactory();
        return gf.createPolygon(verts).isValid();
    }

    /**
     * A1 ported point reduction method 2
     * 
     * @param points
     * @param maxNpts
     * @return
     */
    private boolean reducePoints2(List<Coordinate> points) {
        Coordinate[] pts = points.toArray(new Coordinate[points.size()]);
        int npts = pts.length;
        int i, j, k;
        int best = 0;
        double bestx = -1e10;
        double besty = -1e10;

        // First, determine if the points are ordered in CW or CCW order
        for (i = 0; i < npts; ++i) {
            if (pts[i].y < besty || (pts[i].y == besty && pts[i].x > bestx)) {
                best = i;
                bestx = pts[i].x;
                besty = pts[i].y;
            }
        }

        i = best;

        if (--i < 0) {
            i = npts - 1;
        }
        if ((j = i + 1) >= npts) {
            j -= npts;
        }
        if ((k = j + 1) >= npts) {
            k -= npts;
        }

        double crs = (pts[j].x - pts[i].x) * (pts[k].y - pts[j].y)
                - (pts[j].y - pts[i].y) * (pts[k].x - pts[j].x);

        int orient = crs < 0 ? -1 : (crs > 0 ? 1 : 0);
        if (orient == 0) {
            return false;
        }

        best = -1;
        double besta = 1e10;
        i = 0;

        // find smallest kink
        while (i < npts) {
            if ((j = i + 1) >= npts) {
                j -= npts;
            }
            if ((k = j + 1) >= npts) {
                k -= npts;
            }
            crs = (pts[j].x - pts[i].x) * (pts[k].y - pts[j].y)
                    - (pts[j].y - pts[i].y) * (pts[k].x - pts[j].x);
            if (orient < 0) {
                crs = -crs;
            }

            if (crs < 0) {
                crs = (pts[i].x - pts[j].x) * (pts[k].y - pts[j].y)
                        - (pts[i].y - pts[j].y) * (pts[k].x - pts[j].x);
                if (crs < 0) {
                    crs = -crs;
                }
                double area = 0.5 * crs;

                double dx = pts[k].x - pts[i].x;
                double dy = pts[k].y - pts[i].y;
                double len = Math.sqrt(dx * dx + dy * dy);

                if (area < besta && (area < 1 || area < (0.64 * len))) {
                    best = j;
                    besta = area;
                }
            }
            ++i;
        }

        if (best > 0) {
            points.remove(best);
            --npts;
        }

        return best > 0;

    }

    /**
     * Remove vertices that are (very close) to being collinear with both
     * adjacent vertices.
     */
    private static Polygon removeCollinear(Polygon polygon) {
        ArrayList<Coordinate> coords = new ArrayList<Coordinate>(
                Arrays.asList(polygon.getExteriorRing().getCoordinates()));
        boolean changed = false;
        if (coords.size() <= 4) {
            return polygon;
        }
        coords.remove(coords.size() - 1);

        for (int i = 0; i < coords.size() && coords.size() > 3; ++i) {
            int j = (i + 1) % coords.size();
            Coordinate pi = coords.get(i);
            Coordinate pj = coords.get(j);
            Coordinate pk = coords.get((j + 1) % coords.size());

            double ux = pj.x - pi.x;
            double uy = pj.y - pi.y;
            double vx = pk.x - pj.x;
            double vy = pk.y - pj.y;
            double crs = ux * vy - vx * uy; // cross product
            double ul = Math.sqrt(ux * ux + uy * uy);
            double vl = Math.sqrt(vx * vx + vy * vy);
            if (ul != 0) {
                crs /= ul;
            }
            if (vl != 0) {
                crs /= vl;
            }

            if (Math.abs(crs) <= 0.01) {
                coords.remove(j);
                --i;
                changed = true;
            }
        }

        if (changed) {
            coords.add(new Coordinate(coords.get(0)));
            GeometryFactory gf = polygon.getFactory();
            try {
                Polygon p = gf.createPolygon(gf.createLinearRing(coords
                        .toArray(new Coordinate[coords.size()])), null);
                if (p.isValid()) {
                    return p;
                } else {
                    return polygon;
                }
            } catch (IllegalArgumentException e) {
                /*
                 * An invalid ring can be created when the original has an
                 * "orphan vertex." Just return the original.
                 */
                return polygon;
            }
        } else {
            return polygon;
        }
    }

    public float[][] toFloatData(Geometry warningArea) throws VizException {
        float[][] contourAreaData = new float[nx][ny];
        toFloatData(warningArea, contourAreaData);
        return contourAreaData;
    }

    public void  toFloatData(Geometry warningArea, float[][] contourAreaData) throws VizException {
        Geometry contoured = layer.convertGeom(warningArea, latLonToContour);
        List<Geometry> geomList = new ArrayList<Geometry>(
                contoured.getNumGeometries());
        GeometryUtil.buildGeometryList(geomList, contoured);
        List<PreparedGeometry> prepped = new ArrayList<PreparedGeometry>(
                geomList.size());
        for (Geometry g : geomList) {
            prepped.add(PreparedGeometryFactory.prepare(g));
        }

        GeometryFactory gf = warningArea.getFactory();
        Point point = gf.createPoint(new Coordinate(0, 0));
        CoordinateSequence pointCS = point.getCoordinateSequence();

        for (PreparedGeometry geom : prepped) {
            Envelope env = geom.getGeometry().getEnvelopeInternal();
            int startX = (int) env.getMinX();
            int startY = (int) env.getMinY();
            int width = (int) env.getMaxX();
            int height = (int) env.getMaxY();

            startX = Math.max(0, startX - 1);
            startY = Math.max(0, startY - 1);
            width = Math.min(nx, width + 1);
            height = Math.min(ny, height + 1);
            if (width < 0 || startX >= nx || height < 0 || startY >= ny) {
                continue;
            }

            for (int x = startX; x < width; ++x) {
                for (int y = startY; y < height; ++y) {
                    pointCS.setOrdinate(0, 0, x);
                    pointCS.setOrdinate(0, 1, y);
                    point.geometryChanged();
                    if (contourAreaData[x][y] == 0.0f && geom.intersects(point)) {
                        contourAreaData[x][y] = 1.0f;
                    }
                }
            }
        }
    }

    /**
     * @param points
     */
    public static void removeIntersectedSeg(List<Coordinate> points) {
        if (points.size() > 3) {
            boolean changes = true;
            while (changes) {
                changes = false;
                int maxDelta = (points.size() / 2) - 1;
                for (int deltaK = 0; deltaK < maxDelta && !changes;) {
                    for (int k1 = 0; !changes && k1 < points.size(); ++k1) {
                        int k2 = (k1 + 1) % points.size();
                        int k3 = (k2 + deltaK + 1) % points.size();
                        int k4 = (k3 + 1) % points.size();
                        LineSegment ls1 = new LineSegment(points.get(k1),
                                points.get(k2));
                        LineSegment ls2 = new LineSegment(points.get(k3),
                                points.get(k4));
                        if (ls1.intersection(ls2) != null) {
                            if (ls1.p0.equals(ls2.p0) || ls1.p0.equals(ls2.p1)
                                    || ls1.p1.equals(ls2.p0)
                                    || ls1.p1.equals(ls2.p1)) {
                                continue;
                            }

                            // flip flop k2 and k3
                            Coordinate c3 = points.get(k3);
                            Coordinate c2 = points.get(k2);
                            Coordinate tmp = new Coordinate(c2);
                            c2.x = c3.x;
                            c2.y = c3.y;
                            c3.x = tmp.x;
                            c3.y = tmp.y;
                            changes = true;
                        }
                    }

                    if (!changes) {
                        ++deltaK;
                    }
                }
            }
        }
    }

    public static void truncate(List<Coordinate> coordinates, int decimalPlaces) {
        for (Coordinate coordinate : coordinates) {
            truncate(coordinate, decimalPlaces);
        }
    }

    public static void truncate(Coordinate[] coordinates, int decimalPlaces) {
        for (Coordinate coordinate : coordinates) {
            truncate(coordinate, decimalPlaces);
        }
    }

    public static void truncate(Coordinate coordinate, int decimalPlaces) {
        double x = coordinate.x * Math.pow(10, decimalPlaces);
        double y = coordinate.y * Math.pow(10, decimalPlaces);

        x = x >= 0 ? Math.floor(x) : Math.ceil(x);
        y = y >= 0 ? Math.floor(y) : Math.ceil(y);

        coordinate.x = x / Math.pow(10, decimalPlaces);
        coordinate.y = y / Math.pow(10, decimalPlaces);
    }

    public static void round(List<Coordinate> coordinates, int decimalPlaces) {
        for (Coordinate coordinate : coordinates) {
            round(coordinate, decimalPlaces);
        }
    }

    public static void round(Coordinate[] coordinates, int decimalPlaces) {
        for (Coordinate coordinate : coordinates) {
            round(coordinate, decimalPlaces);
        }
    }

    /**
     * round() Rounding coordinates, instead of truncating them.
     * 
     * History 12/06/2012 DR 15559 Qinglu Lin Created.
     */
    public static void round(Coordinate coordinate, int decimalPlaces) {
        double x = coordinate.x * Math.pow(10, decimalPlaces);
        double y = coordinate.y * Math.pow(10, decimalPlaces);

        if (Double.isNaN(x) || Double.isNaN(y)) {
            throw new IllegalArgumentException("Invalid coordinate "
                    + coordinate);
        }

        x = Math.round(x);
        y = Math.round(y);

        coordinate.x = x / Math.pow(10, decimalPlaces);
        coordinate.y = y / Math.pow(10, decimalPlaces);
    }

    /**
     * removeDuplicateCoordinate remove duplicate intermediate coordinates in
     * warningPolygon. History 10-26-2012 Qinglu Lin DR15479 Created.
     */
    public static Polygon removeDuplicateCoordinate(Polygon polygon) {
        if (polygon == null) {
            return null;
        }
        if (polygon.getNumPoints() <= 4) {
            return polygon;
        }
        Coordinate[] coords = removeDuplicateCoordinate(polygon
                .getCoordinates());
        GeometryFactory gf = new GeometryFactory();
        try {
            polygon = gf.createPolygon(gf.createLinearRing(coords), null);
        } catch (Exception e) {
            ;
        }
        return polygon;
    }

    public static Coordinate[] removeDuplicateCoordinate(Coordinate[] verts) {
        if (verts == null) {
            return null;
        }
        if (verts.length <= 4) {
            return verts;
        }

        Set<Coordinate> coords = new LinkedHashSet<Coordinate>();
        for (Coordinate c : verts) {
            coords.add(c);
        }
        if ((verts.length - coords.size()) < 2) {
            return verts;
        }
        Coordinate[] vertices = new Coordinate[coords.size() + 1];
        Iterator<Coordinate> iter = coords.iterator();
        int i = 0;
        while (iter.hasNext()) {
            vertices[i] = new Coordinate(iter.next());
            i += 1;
        }
        vertices[i] = new Coordinate(vertices[0]);
        if (vertices.length <= 3) {
            return verts;
        } else {
            return vertices;
        }
    }

    /**
     * computeSlope compute the slope of a line.
     * 
     * History 12/06/2012 DR 15559 Qinglu Lin Created.
     */
    private static double computeSlope(Coordinate[] coords, int i) {
        double min = 1.0E-08;
        double slope = 1.0E08;
        double dx = coords[i].x - coords[i + 1].x;
        if (Math.abs(dx) > min) {
            slope = (coords[i].y - coords[i + 1].y) / dx;
        }
        return slope;
    }

    /**
     * computeCoordinate Compute the x component of a coordinate after its y
     * component is adjusted.
     * 
     * History 12/06/2012 DR 15559 Qinglu Lin Created.
     */
    private static void computeCoordinate(Coordinate[] c, int i, int j) {
        double slope;
        slope = computeSlope(c, i);
        int iPlus1 = i + 1;
        if (c[j].x >= c[i].x && c[j].x <= c[iPlus1].x || c[j].x >= c[iPlus1].x
                && c[j].x <= c[i].x) {

            double x, y;
            double min1 = 0.005d;
            y = slope * (c[j].x - c[i].x) + c[i].y;
            double d = Math.abs(y - c[j].y);
            if (d > min1) {
                return;
            }

            double min2 = 1.0E-8d;
            double delta = 0.005d; // empirical value
            double dyMin = 0.01d;
            int jMinus1 = j - 1;
            if (jMinus1 < 0) {
                jMinus1 = c.length - 2;
            }
            int jPlus1 = j + 1;
            if (Math.abs(y - c[j].y) < min1) {
                double dy1, dy2;
                dy1 = Math.abs(c[jMinus1].y - y);
                dy2 = Math.abs(c[jPlus1].y - y);
                if (dy1 >= dy2
                        && (Math.abs(dy1) > dyMin || Math.abs(dy2) > dyMin)) {
                    // attempt to use l2 for computation
                    if (c[j].y == c[jMinus1].y
                            && Math.abs(c[j].x - c[jMinus1].x) > min2) {
                        // l2 is a horizontal line, use l3 for computation
                        if (c[jPlus1].y < c[j].y) {
                            delta = -delta;
                        }
                        slope = computeSlope(c, j);
                        if (Math.abs(slope) > min2) {
                            y = c[j].y + delta;
                            x = (y - c[jPlus1].y) / slope + c[jPlus1].x;
                        } else {
                            // l3 is a vertical line
                            y = c[j].y + delta;
                            x = c[j].x;
                        }
                    } else {
                        // use l2 for computation
                        if (c[jMinus1].y < c[j].y) {
                            delta = -delta;
                        }
                        slope = computeSlope(c, jMinus1);
                        if (Math.abs(slope) > min2) {
                            y = c[j].y + delta;
                            x = (y - c[jMinus1].y) / slope + c[jMinus1].x;
                        } else {
                            // l2 is a vertical line
                            y = c[j].y + delta;
                            x = c[j].x;
                        }
                    }
                } else {
                    if (Math.abs(dy1) > dyMin || Math.abs(dy2) > dyMin) {
                        // attempt to use l3 for computation
                        if (c[j].y == c[jPlus1].y
                                && Math.abs(c[j].x - c[jPlus1].x) > min2) {
                            // l3 is a horizontal line, use l2 for computation
                            if (c[jMinus1].y < c[j].y) {
                                delta = -delta;
                            }
                            slope = computeSlope(c, jMinus1);
                            if (Math.abs(slope) > min2) {
                                y = c[j].y + delta;
                                x = (y - c[jMinus1].y) / slope + c[jMinus1].x;
                            } else {
                                // l2 is a vertical line
                                y = c[j].y + delta;
                                x = c[j].x;
                            }
                        } else {
                            // use l3 for computation
                            if (c[jPlus1].y < c[j].y) {
                                delta = -delta;
                            }
                            slope = computeSlope(c, j);
                            if (Math.abs(slope) > min2) {
                                y = c[j].y + delta;
                                x = (y - c[jPlus1].y) / slope + c[jPlus1].x;
                            } else {
                                // l3 is a vertical line
                                y = c[j].y + delta;
                                x = c[j].x;
                            }
                        }
                    } else {
                        x = c[j].x;
                        y = c[j].y;
                    }
                }
                c[j].x = x;
                c[j].y = y;
                if (j == 0) {
                    c[c.length - 1] = c[j];
                }
                if (j == c.length - 1) {
                    c[0] = c[j];
                }
            }
        }
    }

    /**
     * adjustPolygon When a point is very close to a line in the initial warning
     * polygon, the resulting coordinates cause the failure of polygon drawing
     * in follow-up. The method move that kind of points away from the line.
     * 
     * History 12/06/2012 DR 15559 Qinglu Lin Created.
     */
    public static void adjustPolygon(Coordinate[] coords) {
        int n = coords.length;
        for (int i = 0; i < n - 1; ++i) {
            int j;
            for (j = i + 2; j <= n - 2; j++) {
                computeCoordinate(coords, i, j);
            }
            if (i <= n - 3) {
                for (j = 0; j < i; j++) {
                    computeCoordinate(coords, i, j);
                }
            } else {
                for (j = 1; j < i; j++) {
                    computeCoordinate(coords, i, j);
                }
            }
        }
    }

    public static Coordinate[] removeOverlaidLinesegments(Coordinate[] coords) {
        if (coords.length <= 4) {
            return coords;
        }
        Coordinate[] expandedCoords = null;
        boolean flag = true;
        while (flag) {
            if (coords.length <= 4) {
                return coords;
            }
            expandedCoords = new Coordinate[coords.length + 1];
            flag = false;
            for (int i = 0; i < coords.length; i++) {
                expandedCoords[i] = new Coordinate(coords[i]);
            }
            expandedCoords[expandedCoords.length - 1] = new Coordinate(
                    coords[1]);
            double min = 1.0E-8;
            int m = expandedCoords.length;
            int count = 0;
            double slope = 0.0, slope1 = 0.0;
            for (int i = 0; i < m - 1; i++) {
                slope = computeSlope(expandedCoords, i);
                if (count == 0) {
                    slope1 = slope;
                    count += 1;
                } else {
                    if (Math.abs(slope - slope1) <= min) {
                        count += 1;
                    } else {
                        count = 0;
                        slope1 = slope;
                        count += 1;
                    }
                }
                if (count == 2) {
                    // remove the middle point, i.e., that has index of i, of
                    // the three that either form two
                    // overlaid/partial overlaid line segments or is in the
                    // middle
                    // of a straight line segment
                    coords = new Coordinate[coords.length - 1];
                    if (i == m - 2) {
                        for (int j = 1; j <= m - 2; j++) {
                            coords[j - 1] = new Coordinate(expandedCoords[j]);
                        }
                        coords[coords.length - 1] = new Coordinate(coords[0]);
                    } else {
                        for (int j = 0; j < i; j++) {
                            coords[j] = new Coordinate(expandedCoords[j]);
                        }
                        for (int j = i + 1; j < expandedCoords.length - 2; j++) {
                            coords[j - 1] = new Coordinate(expandedCoords[j]);
                        }
                        coords[coords.length - 1] = new Coordinate(coords[0]);
                    }
                    flag = true;
                    break;
                }
            }
        }
        return coords;
    }

    /**
     * Adjust the location of one vertex that cause polygon self-crossing.
     */
    static public Coordinate[] adjustVertex(Coordinate[] coord) {
        GeometryFactory gf = new GeometryFactory();
        LinearRing lr;
        Polygon p;
        int length = coord.length;
        Coordinate intersectCoord = null;
        int index[] = new int[6];
        LineSegment ls1, ls2;
        double d[] = new double[6];
        int indexOfTheOtherEnd[] = new int[2];
        boolean isPolygonValid = false;
        outerLoop: for (int skippedSegment = 1; skippedSegment < length - 3; skippedSegment++) {
            for (int i = 0; i < length - 1; i++) {
                index[0] = i;
                index[1] = index[0] + 1;
                index[2] = index[1] + skippedSegment;
                if (index[2] >= length) {
                    index[2] = index[2] - length + 1;
                }
                index[3] = index[2] + 1;
                if (index[3] >= length) {
                    index[3] = index[3] - length + 1;
                }
                ls1 = new LineSegment(coord[index[0]], coord[index[1]]);
                ls2 = new LineSegment(coord[index[2]], coord[index[3]]);
                intersectCoord = ls1.intersection(ls2);
                if (intersectCoord != null) {
                    for (int j = 0; j < index.length - 2; j++) {
                        d[j] = intersectCoord.distance(coord[index[j]]);
                    }
                    if (d[0] < d[1]) {
                        index[4] = index[0];
                        d[4] = d[0];
                        indexOfTheOtherEnd[0] = index[1];
                    } else {
                        index[4] = index[1];
                        d[4] = d[1];
                        indexOfTheOtherEnd[0] = index[0];
                    }
                    if (d[2] < d[3]) {
                        index[5] = index[2];
                        d[5] = d[2];
                        indexOfTheOtherEnd[1] = index[3];
                    } else {
                        index[5] = index[3];
                        d[5] = d[3];
                        indexOfTheOtherEnd[1] = index[2];
                    }
                    // index of the vertex on a line segment (line segment A),
                    // which will be moved along line segment A.
                    int replaceIndex;
                    // index of the vertex at the other end of line segment A.
                    int theOtherIndex;
                    Coordinate b0, b1;
                    if (d[4] < d[5]) {
                        replaceIndex = index[4];
                        theOtherIndex = indexOfTheOtherEnd[0];
                        b0 = coord[index[2]];
                        b1 = coord[index[3]];
                    } else {
                        replaceIndex = index[5];
                        theOtherIndex = indexOfTheOtherEnd[1];
                        b0 = coord[index[0]];
                        b1 = coord[index[1]];
                    }

                    /*
                     * Move the bad vertex (coord[replaceIndex]), which is on
                     * line segment A and has the shortest distance to
                     * intersectCoord, along line segment A to the other side of
                     * line segment B (b0, b1) which intersects with line
                     * segment A.
                     * 
                     * The point is actually moved to the 0.01 grid point
                     * closest to intersectCoord. That point may not actually be
                     * on line segment A.
                     */
                    Coordinate c = adjustVertex2(intersectCoord,
                            coord[theOtherIndex], b0, b1);
                    if (c != null) {
                        coord[replaceIndex].x = c.x;
                        coord[replaceIndex].y = c.y;
                    }

                    // PolygonUtil.round(coord, 2);
                    PolygonUtil.round(coord[replaceIndex], 2);
                    if (replaceIndex == 0) {
                        coord[length - 1] = new Coordinate(coord[replaceIndex]);
                    } else if (replaceIndex == length - 1) {
                        coord[0] = new Coordinate(coord[replaceIndex]);
                    }
                    lr = gf.createLinearRing(coord);
                    p = gf.createPolygon(lr, null);
                    isPolygonValid = p.isValid();
                    if (isPolygonValid) {
                        break outerLoop;
                    }
                }
            }
        }
        return coord;
    }

    private static final double SIDE_OF_LINE_THRESHOLD = 1e-9;

    /**
     * Returns 1, -1, or 0 if p is on the left of, on the right of, or on pa ->
     * pb
     */
    private static int sideOfLine(Coordinate p, Coordinate pa, Coordinate pb) {
        double cp = (pb.x - pa.x) * (p.y - pa.y) - (p.x - pa.x) * (pb.y - pa.y); // Cross
                                                                                 // product
        return Math.abs(cp) > SIDE_OF_LINE_THRESHOLD ? (cp < 0 ? -1
                : (cp > 0 ? 1 : 0)) : 0;
    }

    /** Returns the angle between p -> pa and p -> pb */
    private static double angleBetween(Coordinate p, Coordinate pa,
            Coordinate pb) {
        double ax = pa.x - p.x;
        double ay = pa.y - p.y;
        double bx = pb.x - p.x;
        double by = pb.y - p.y;

        double m = Math.sqrt((ax * ax + ay * ay) * (bx * bx + by * by));
        return m != 0 ? Math.acos((ax * bx + ay * by) / m) : 0;
    }

    private static int N_CANDIDATE_POINTS = 8;

    private static byte[] CANDIDATE_DX = { 1, 1, 1, 0, -1, -1, -1, 0 };

    private static byte[] CANDIDATE_DY = { 1, 0, -1, -1, -1, 0, 1, 1 };

    /**
     * Returns the coordinate within one grid point on the 0.01 grid next to
     * intersectCoord that is on the same side of (b0,b1) as 'destination' which
     * has the smallest angle to (inserectCoord,destination). The result may not
     * be exact so it should be passed to round(Coordinate) if used.
     * 
     * If intersectCoord is on a grid point, there are eight candidate points.
     * Otherwise there are four candidates.
     * 
     * Returns null if no point can be found.
     */
    private static Coordinate adjustVertex2(Coordinate intersectCoord,
            Coordinate destination, Coordinate b0, Coordinate b1) {
        int sideOfTheOther = sideOfLine(destination, b0, b1);
        if (sideOfTheOther == 0) {
            return null;
        }

        double pxh = intersectCoord.x * 100;
        double pyh = intersectCoord.y * 100;

        double cx = Math.ceil(pxh);
        double fx = Math.floor(pxh);
        double cy = Math.ceil(pyh);
        double fy = Math.floor(pyh);

        double ox, oy;
        if (Math.abs(cx - pxh) < SIDE_OF_LINE_THRESHOLD
                || Math.abs(fx - pxh) < SIDE_OF_LINE_THRESHOLD) {
            cx = fx = pxh;
        }
        if (Math.abs(cy - pyh) < SIDE_OF_LINE_THRESHOLD
                || Math.abs(fy - pyh) < SIDE_OF_LINE_THRESHOLD) {
            cy = fy = pyh;
        }

        Coordinate best = null;
        double bestAngle = Math.PI * 2;

        for (int ci = 0; ci < N_CANDIDATE_POINTS; ++ci) {
            int dx = CANDIDATE_DX[ci];
            int dy = CANDIDATE_DY[ci];

            if (dx == 0) {
                if (cx != fx) {
                    continue;
                }
                ox = pxh;
            } else {
                if (dx > 0) {
                    ox = cx == fx ? pxh + 1 : cx;
                } else {
                    ox = cx == fx ? pxh - 1 : fx;
                }
            }
            if (dy == 0) {
                if (cy != fy) {
                    continue;
                }
                oy = pyh;
            } else {
                if (dy > 0) {
                    oy = cy == fy ? pyh + 1 : cy;
                } else {
                    oy = cy == fy ? pyh - 1 : fy;
                }
            }
            Coordinate c = new Coordinate(ox / 100.0, oy / 100.0);
            if (c != null && sideOfLine(c, b0, b1) == sideOfTheOther) {
                double a = angleBetween(intersectCoord, c, destination);
                if (a < bestAngle) {
                    best = c;
                    bestAngle = a;
                }
            }
        }

        return best;
    }

    /**
     * Alter the location of two vertexes that cause polygon self-crossing. This
     * method would be used if polygon is still invalid after using
     * adjustVertex().
     */
    static public Coordinate[] alterVertexes(Coordinate[] coord) {
        GeometryFactory gf = new GeometryFactory();
        LinearRing lr;
        Polygon p;
        int length = coord.length;
        Coordinate intersectCoord = null;
        int index[] = new int[6];
        LineSegment ls1, ls2;
        boolean isPolygonValid = false;
        int index1, index2;
        outerLoop: for (int skippedSegment = 1; skippedSegment < length - 3; skippedSegment++) {
            for (int i = 0; i < length - 1; i++) {
                index[0] = i;
                index[1] = index[0] + 1;
                index[2] = index[1] + skippedSegment;
                if (index[2] >= length) {
                    index[2] = index[2] - length + 1;
                }
                index[3] = index[2] + 1;
                if (index[3] >= length) {
                    index[3] = index[3] - length + 1;
                }
                ls1 = new LineSegment(coord[index[0]], coord[index[1]]);
                ls2 = new LineSegment(coord[index[2]], coord[index[3]]);
                intersectCoord = null;
                intersectCoord = ls1.intersection(ls2);
                if (intersectCoord != null) {
                    index1 = calcShortestDistance(intersectCoord, ls1);
                    index2 = calcShortestDistance(intersectCoord, ls2);
                    Coordinate c = new Coordinate(0.5*(coord[index[index1]].x + coord[index[2+index2]].x),
                            0.5*(coord[index[index1]].y + coord[index[2+index2]].y));
                    PolygonUtil.round(c, 2);
                    coord[index[index1]] = new Coordinate(c);
                    coord[index[2 + index2]] = new Coordinate(c);
                    if (index[index1] == 0) {
                        coord[coord.length - 1] = new Coordinate(c);
                    } else if (index[index1] == coord.length - 1) {
                        coord[0] = new Coordinate(c);
                    }
                    if (index[2 + index2] == 0) {
                        coord[coord.length - 1] = new Coordinate(c);
                    } else if (index[2 + index2] == coord.length - 1) {
                        coord[0] = new Coordinate(c);
                    }
                    lr = gf.createLinearRing(coord);
                    p = gf.createPolygon(lr, null);
                    isPolygonValid = p.isValid();
                    if (isPolygonValid) {
                        break outerLoop;
                    }
                }
            }
        }
        return coord;
    }

    static public int calcShortestDistance(Coordinate p, LineSegment ls) {
        double d1 = p.distance(ls.p0);
        double d2 = p.distance(ls.p1);
        if (d1 <= d2) {
            return 0;
        } else {
            return 1;
        }
    }

    /**
     * Create a polygon whose two diagonal coordinates are a and b.
     **/
    static public Geometry createPolygonByPoints(GeometryFactory gf,
            Coordinate a, Coordinate b) {
        double maxX, minX, maxY, minY;
        maxX = Math.max(a.x, b.x);
        minX = Math.min(a.x, b.x);
        maxY = Math.max(a.y, b.y);
        minY = Math.min(a.y, b.y);
        Coordinate[] coord = new Coordinate[5];
        coord[0] = new Coordinate(minX, minY);
        coord[1] = new Coordinate(maxX, minY);
        coord[2] = new Coordinate(maxX, maxY);
        coord[3] = new Coordinate(minX, maxY);
        coord[4] = new Coordinate(coord[0]);
        LinearRing lr = gf.createLinearRing(coord);
        return gf.createPolygon(lr, null);
    }

    static public Geometry createPolygonByPoints(GeometryFactory gf,
            Coordinate a, double shift) {
        Coordinate b = new Coordinate(a.x + shift, a.y + shift);
        return createPolygonByPoints(gf, a, b);
    }

    /**
     * Creates a copy of a Geometry with reduced precision to reduce the chance
     * of topology errors when used in intersection operations.
     * 
     * @param g
     * @return a new Geometry that is a copy of given Geometry with reduced
     *         precision. References to user data are copied. If there are
     *         GeometryCollection objects, user data is copied for each element.
     */
    static public Geometry reducePrecision(Geometry g) {
        Geometry result;
        if (g instanceof GeometryCollection) {
            Geometry[] list = new Geometry[g.getNumGeometries()];
            for (int i = 0; i < list.length; ++i) {
                list[i] = reducePrecision(g.getGeometryN(i));
            }
            GeometryFactory gf = new GeometryFactory();
            result = gf.createGeometryCollection(list);
        } else {
            result = SimpleGeometryPrecisionReducer
                    .reduce(g, REDUCED_PRECISION);
        }
        result.setUserData(g.getUserData());
        return result;
    }
}
