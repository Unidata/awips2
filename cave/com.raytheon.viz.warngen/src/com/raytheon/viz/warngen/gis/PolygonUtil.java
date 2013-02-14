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
import java.util.List;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.warning.util.GeometryUtil;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.contours.util.ContourContainer;
import com.raytheon.viz.core.contours.util.FortConBuf;
import com.raytheon.viz.core.contours.util.FortConConfig;
import com.raytheon.viz.warngen.gui.WarngenLayer;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

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
 *
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PolygonUtil {

    private WarngenLayer layer;

    private int nx, ny;

    private int maxVertices;

    private MathTransform latLonToContour, contourToLatLon;

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
            Geometry origWarningArea) throws VizException {
        float[][] contourAreaData = toFloatData(origWarningArea);

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
                    config);
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
     * @return
     */
    private Polygon awips1PointReduction(Coordinate[] longest,
            Polygon warningPolygon, Geometry warningArea, FortConConfig config)
            throws VizException {
        Coordinate[] vertices = warningPolygon.getCoordinates();
        vertices = Arrays.copyOf(vertices, vertices.length - 1);

        // Extract data
        float[][] contourPolyData = toFloatData(warningPolygon);

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
                    if (dy < 0)
                        i = dx < dy ? (int) (hp * dy / dx) : 50 + (int) (hp
                                * dx / dy);
                    else
                        i = -dx > dy ? 100 + (int) (hm * dy / dx)
                                : 150 + (int) (hm * dx / dy);
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
                if (dx == 0 && dy == 0)
                    i = 0;
                else if (dx < 0) {
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
                    if (match[k] < 0)
                        continue;
                    for (i = 0; i < nv; ++i) {
                        dx = longest[k].x - vert[i].x;
                        dy = longest[k].y - vert[i].y;
                        da = dx * ux[i] + dy * uy[i];
                        if (da < 0 || da > vlen[i])
                            continue;
                        dn = dy * ux[i] - dx * uy[i];
                        if (dn < -1 || dn > 1)
                            continue;
                        if (match[k] < nv) {
                            match[k] = nv;
                            break;
                        }
                        match[k] = i;
                    }
                }

                for (j = npoints - 1, k = 0; k < npoints; j = k++)
                    if (match[j] < 0 && match[k] == nv)
                        match[k] = -1;
                for (j = 0, k = npoints - 1; k >= 0; j = k--)
                    if (match[j] < 0 && match[k] == nv)
                        match[k] = -1;

                // Now we replace matched points with ones in the
                // original base polygon
                int p1 = npoints - 1;
                while (p1 >= 0 && match[p1] == nv)
                    p1--;

                if (p1 >= 0) {
                    int n, best1, best2;
                    for (n = k = 0; k < npoints; k++) {
                        if (match[k] == nv)
                            continue;
                        best1 = match[p1];
                        best2 = match[k];
                        p1 = k;
                        if (best1 < 0 && best2 < 0) {
                            if (k == n)
                                continue;
                            fixed[n] = 0;
                            longest[n++] = new Coordinate(longest[k]);
                            continue;
                        }
                        if (best1 == best2)
                            continue;
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
                        if (best2 - best1 == 1 || best1 - best2 == nv - 1)
                            i = best2;
                        else if (best1 - best2 == 1 || best2 - best1 == nv - 1)
                            i = best1;
                        else {
                            dx = longest[k].x - vert[best1].x;
                            dy = longest[k].y - vert[best1].y;
                            da = dx * ux[best1] + dy * uy[best1]
                                    + totlen[best1];
                            dx = longest[k].x - vert[best2].x;
                            dy = longest[k].y - vert[best2].y;
                            dn = dx * ux[best2] + dy * uy[best2]
                                    + totlen[best2];
                            if (dn - da > len2)
                                best1 += nv;
                            else if (dn - da < -len2)
                                best2 += nv;
                            if (best1 < best2)
                                for (best1++; best1 <= best2; best1++) {
                                    fixed[n] = 1;
                                    longest[n++] = new Coordinate(vert[best1]);
                                }
                            else
                                for (; best1 > best2; best1--) {
                                    fixed[n] = 1;
                                    longest[n++] = new Coordinate(vert[best1]);
                                }
                            continue;
                        }
                        fixed[n] = 1;
                        longest[n++] = new Coordinate(vert[i]);
                    }
                    if (n > 0)
                        npoints = n;
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
        truncate(points, 2);
        Polygon rval = gf.createPolygon(gf.createLinearRing(points
                .toArray(new Coordinate[points.size()])), null);

        if (rval.isValid() == false) {
            System.out.println("Fixing intersected segments");
            points.remove(points.size() - 1);
            removeIntersectedSeg(points);
            points.add(new Coordinate(points.get(0)));
            rval = gf.createPolygon(gf.createLinearRing(points
                    .toArray(new Coordinate[points.size()])), null);
        }

        return rval;
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
                if (dx < -dbest || dx > dbest)
                    continue;
                dy = c.y - avg.y;
                if (dy < -dbest || dy > dbest)
                    continue;
                dx = dx * dx + dy * dy;
                if (dx > d2)
                    continue;
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
                if (dx < -dbest || dx > dbest)
                    continue;
                dy = c.y - closest.y;
                if (dy < -dbest || dy > dbest)
                    continue;
                dx = dx * dx + dy * dy;
                if (dx > d2)
                    continue;
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
            if (array[i][j1] == 0)
                array[i][j1] = val;
            j2 = (int) (slope * i + intcp);
            if (j2 == j1)
                continue;
            j1 = j2;
            if (array[i][j1] == 0)
                array[i][j1] = val;
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
            if (array[i1][j] == 0)
                array[i1][j] = val;
            i2 = (int) (slope * j + intcp);
            if (i2 == i1)
                continue;
            i1 = i2;
            if (array[i1][j] == 0)
                array[i1][j] = val;
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
            if (dis < maxDis)
                continue;
            maxDis = dis;
            k1 = k;
        }
        k2 = -1;
        maxDis = 0;
        for (k = 0; k < npts; k++) {
            dx = pts[k].x - pts[k1].x;
            dy = pts[k].y - pts[k1].y;
            dis = dx * dx + dy * dy;
            if (dis < maxDis)
                continue;
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
                    if (++k >= npts)
                        k = 0;
                    if (k == k2)
                        break;
                    dx = pts[k].x - pts[k1].x;
                    dy = pts[k].y - pts[k1].y;
                    dis = dx * dx0 + dy * dy0;
                    if (dis < 0)
                        dis = -dis;
                    else
                        dis -= bas;
                    if (dis <= 0) {
                        if (simple == 0)
                            continue;
                        dis = dx * dy0 - dy * dx0;
                        if (dis < 0)
                            dis = -dis;
                    } else if (simple != 0)
                        maxDis = simple = 0;
                    if (dis < maxDis)
                        continue;
                    maxDis = dis;
                    kn = k;
                }
                k1 = k2;
            }

            if (simple != 0 && nyes > 2) {
                if (maxDis * 40 < bigDis)
                    break;
                dis = (4.0 * nyes / (float) maxNpts) - 1;
                if (maxDis < dis)
                    break;
            }

            for (y = nyes - 1; y >= 0 && kn < yesList[y]; y--)
                yesList[y + 1] = yesList[y];
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
        for (i = 0; i < npts; ++i)
            if (pts[i].y < besty || (pts[i].y == besty && pts[i].x > bestx)) {
                best = i;
                bestx = pts[i].x;
                besty = pts[i].y;
            }

        i = best;

        if (--i < 0)
            i = npts - 1;
        if ((j = i + 1) >= npts)
            j -= npts;
        if ((k = j + 1) >= npts)
            k -= npts;

        double crs = (pts[j].x - pts[i].x) * (pts[k].y - pts[j].y)
                - (pts[j].y - pts[i].y) * (pts[k].x - pts[j].x);

        int orient = crs < 0 ? -1 : (crs > 0 ? 1 : 0);
        if (orient == 0)
            return false;

        best = -1;
        double besta = 1e10;
        i = 0;

        // find smallest kink
        while (i < npts) {
            if ((j = i + 1) >= npts)
                j -= npts;
            if ((k = j + 1) >= npts)
                k -= npts;
            crs = (pts[j].x - pts[i].x) * (pts[k].y - pts[j].y)
                    - (pts[j].y - pts[i].y) * (pts[k].x - pts[j].x);
            if (orient < 0)
                crs = -crs;

            if (crs < 0) {
                crs = (pts[i].x - pts[j].x) * (pts[k].y - pts[j].y)
                        - (pts[i].y - pts[j].y) * (pts[k].x - pts[j].x);
                if (crs < 0)
                    crs = -crs;
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

    private float[][] toFloatData(Geometry warningArea) throws VizException {
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
        float[][] contourAreaData = new float[nx][ny];
        Geometry[][] points = new Geometry[nx][ny];
        for (int x = 0; x < nx; ++x) {
            for (int y = 0; y < ny; ++y) {
                points[x][y] = gf.createPoint(new Coordinate(x, y));
            }
        }
        for (PreparedGeometry geom : prepped) {
            Envelope env = geom.getGeometry().getEnvelopeInternal();
            int startX = (int) env.getMinX();
            int startY = (int) env.getMinY();
            int width = (int) env.getMaxX();
            int height = (int) env.getMaxY();
            if (startX < 0 || width > nx || startY < 0 || height > ny) {
                continue;
            }
            startX = Math.max(0, startX - 1);
            startY = Math.max(0, startY - 1);
            width = Math.min(nx, width + 1);
            height = Math.min(ny, height + 1);

            for (int x = startX; x < width; ++x) {
                for (int y = startY; y < height; ++y) {
                    if (contourAreaData[x][y] == 0.0f
                            && geom.intersects(points[x][y])) {
                        contourAreaData[x][y] = 1.0f;
                    }
                }
            }
        }
        return contourAreaData;
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
    
    public static void truncate(List<Coordinate >coordinates, int decimalPlaces) {
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

    public static void round(List<Coordinate >coordinates, int decimalPlaces) {
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
     * round()
     *     Rounding coordinates, instead of truncating them.     
     *
     * History
     * 12/06/2012   DR 15559  Qinglu Lin   Created. 
     */
    public static void round(Coordinate coordinate, int decimalPlaces) {
        double x = coordinate.x * Math.pow(10, decimalPlaces);
        double y = coordinate.y * Math.pow(10, decimalPlaces);
        
        x = Math.round(x);
        y = Math.round(y);
        
        coordinate.x = x / Math.pow(10, decimalPlaces);
        coordinate.y = y / Math.pow(10, decimalPlaces);
    }
}
