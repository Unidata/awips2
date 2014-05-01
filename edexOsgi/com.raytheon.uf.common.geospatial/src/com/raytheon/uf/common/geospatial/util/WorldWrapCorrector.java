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
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.geometry.Envelope;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * This class uses the WorldWrapChecker to correct wrapping of geometries that
 * are in normalized lat/lon (-180-180) coordinates for the projection on the
 * descriptor passed in
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Oct 12, 2011           mschenke    Initial creation
 * May 30, 2013  2028     randerso    Changed to return simple geometry or
 *                                    multi-geometry if possible
 * Dec 11, 2013  2619     bsteffen    Fix rare dateline bug in flattenGeometry.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WorldWrapCorrector {

    private WorldWrapChecker checker;

    /**
     * Constructs of world wrap corrector for the specified world
     * 
     * @param descriptor
     */
    public WorldWrapCorrector(GeneralGridGeometry worldGeometry) {
        this(worldGeometry.getEnvelope());
    }

    /**
     * Constructs a world wrap corrector for the specified world CRS Envelope
     * 
     * @param worldEnvelope
     */
    public WorldWrapCorrector(Envelope worldEnvelope) {
        checker = new WorldWrapChecker(worldEnvelope);
    }

    /**
     * Corrects the Geometry passed in to not wrap if geometry does wrap.
     * Geometry must be in normalized lat/lon space (-180 to 180)
     * 
     * @param geom
     * @return A Geometry that has been corrected for wrapping. The good
     *         geometries will be a geometry collection.
     */
    public Geometry correct(Geometry geom) {
        List<Geometry> geoms = new ArrayList<Geometry>();
        if (checker.needsChecking() == false) {
            geoms.add(geom);
        } else {
            wrapCorrect(geom, geoms);
        }

        Geometry retVal;
        if (geoms.size() == 1) {
            retVal = geoms.get(0);
        } else {
            retVal = geom.getFactory().buildGeometry(geoms);
        }

        return retVal;
    }

    /**
     * Recursive function to preprocess primitive geometries (non
     * collections/non points) and look for wrapping that will occur on the
     * descriptor's map
     * 
     * @param g
     * @param geomList
     */
    private void wrapCorrect(Geometry g, List<Geometry> geomList) {
        if (g instanceof GeometryCollection) {
            for (int n = 0; n < g.getNumGeometries(); ++n) {
                wrapCorrect(g.getGeometryN(n), geomList);
            }
        } else if (g.isEmpty() == false) {
            // Algorithm:
            // Process primitive geometry type (non collection). Algorithm works
            // in that it walks the geometry, when two points cross, it adds or
            // subtracts 360 to the offset to flatten out the geometry. When
            // first part is done, geometries will be continuous and not limited
            // to -180 to 180, they will be technically be > neg infinitive, <
            // pos infinity given that the algorithm supports wrapping around
            // the world multiple times. When we have the continuous geometry,
            // we split it up into sections by intersecting with a 360 deg
            // inverse central meridian. We then normalize the points for each
            // section back to -180 to 180
            List<Geometry> geoms = new ArrayList<Geometry>();
            if (g instanceof Polygon) {
                GeometryFactory gf = g.getFactory();
                Polygon p = (Polygon) g;
                LineString extRing = p.getExteriorRing();
                Polygon extPolygon = gf.createPolygon(
                        gf.createLinearRing(extRing.getCoordinates()), null);
                // World wrap correct exterior ring and extract polygons
                double[] offsets = flattenGeometry(extPolygon);
                if (offsets != null) {
                    // This polygon needs to be corrected, process
                    List<Geometry> extRings = new ArrayList<Geometry>();
                    correct(extRings, extPolygon, offsets);
                    List<Polygon> polygons = new ArrayList<Polygon>();
                    for (Geometry geom : extRings) {
                        extractPolygons(polygons, geom);
                    }

                    // World wrap correct each interior ring
                    List<Geometry> intRings = new ArrayList<Geometry>(
                            p.getNumInteriorRing());
                    for (int n = 0; n < p.getNumInteriorRing(); ++n) {
                        Polygon intRing = gf.createPolygon(gf
                                .createLinearRing(p.getInteriorRingN(n)
                                        .getCoordinates()), null);
                        offsets = flattenGeometry(intRing);
                        correct(intRings, intRing, offsets);
                    }

                    // Extract polygons and "preprare" them for intersections
                    List<Polygon> interiorPolygons = new LinkedList<Polygon>();
                    for (Geometry geom : intRings) {
                        extractPolygons(interiorPolygons, geom);
                    }
                    List<PreparedGeometry> preparedInteriorPolygons = new LinkedList<PreparedGeometry>();
                    for (Polygon intPoly : interiorPolygons) {
                        preparedInteriorPolygons.add(PreparedGeometryFactory
                                .prepare(intPoly));
                    }

                    // Final polygon list (may create multipolygon out of)
                    List<Polygon> finalPolys = new ArrayList<Polygon>(
                            polygons.size());
                    for (Polygon polygon : polygons) {
                        // For each polygon, check if it intersects any interior
                        // polygons. If so, add them to interior ring list so we
                        // can reconstruct with them in place
                        List<LinearRing> interiorRings = new ArrayList<LinearRing>();
                        Iterator<PreparedGeometry> preparedIntPolys = preparedInteriorPolygons
                                .iterator();
                        while (preparedIntPolys.hasNext()) {
                            PreparedGeometry prepIntPoly = preparedIntPolys
                                    .next();
                            boolean intersects = prepIntPoly
                                    .intersects(polygon);
                            if (intersects) {
                                preparedIntPolys.remove();
                                interiorRings
                                        .add(gf.createLinearRing(((Polygon) prepIntPoly
                                                .getGeometry())
                                                .getExteriorRing()
                                                .getCoordinates()));
                            }
                        }

                        if (interiorRings.size() > 0) {
                            // add holes to polygon
                            polygon = gf.createPolygon(gf
                                    .createLinearRing(polygon.getExteriorRing()
                                            .getCoordinates()), interiorRings
                                    .toArray(new LinearRing[0]));
                        }
                        finalPolys.add(polygon);
                    }

                    if (finalPolys.size() > 1) {
                        // More than one polygon resulting, create MultiPolygon
                        geoms.add(gf.createMultiPolygon(finalPolys
                                .toArray(new Polygon[0])));
                    } else {
                        // 1 or 0 polygons, just add to list
                        for (Polygon polygon : finalPolys) {
                            geoms.add(polygon);
                        }
                    }
                } else {
                    // offsets were null, polygon can be added as is
                    geoms.add(g);
                }
            } else {
                double[] offsets = flattenGeometry(g);
                correct(geoms, g, offsets);
            }
            for (Geometry geom : geoms) {
                rollGeometry(geom);
            }
            geomList.addAll(geoms);
        } else {
            geomList.add(g);
        }
    }

    private void correct(List<Geometry> geoms, Geometry flattenedGeom,
            double[] offsets) {
        if (offsets == null) {
            // no offsets to apply, add and return
            geoms.add(flattenedGeom);
            return;
        } else if (flattenedGeom.isValid()) {
            // Only apply world wrap correcting to valid geometries, otherwise
            // throw them out since we can't guarantee integrity
            GeometryFactory gf = flattenedGeom.getFactory();
            double delta = 0.00001;
            // Because Geometries are within bounds -180-180, ensure our initial
            // start/end range will fully cover it
            double lowInverseCentral = checker.getLowInverseCentralMeridian();
            if (lowInverseCentral > -180.0) {
                lowInverseCentral -= 360.0;
            }
            double highInverseCentral = checker.getHighInverseCentralMeridian();
            if (highInverseCentral < 180.0) {
                highInverseCentral += 360.0;
            }
            double start = lowInverseCentral + offsets[0];
            double end = highInverseCentral + offsets[1];
            double minY = -90, maxY = 90;

            while (start < end) {
                double useStart = start;
                double useEnd = start + 360;
                double minX = useStart + delta;
                double maxX = useEnd - delta;

                Geometry section = gf.createPolygon(
                        gf.createLinearRing(new Coordinate[] {
                                new Coordinate(minX, maxY),
                                new Coordinate(maxX, maxY),
                                new Coordinate(maxX, minY),
                                new Coordinate(minX, minY),
                                new Coordinate(minX, maxY) }), null);
                section = section.intersection(flattenedGeom);
                if (section.isEmpty() == false) {
                    geoms.add(section);
                }
                start = useEnd;
            }
        }
    }

    /**
     * Inverse of {@link #flattenGeometry(Geometry)}, converts geometry
     * coordinates so they are between -180/180
     * 
     * @param geom
     */
    private void rollGeometry(Geometry geom) {
        for (Coordinate c : geom.getCoordinates()) {
            while (c.x < -180.0) {
                c.x += 360.0;
            }
            while (c.x > 180.0) {
                c.x -= 360.0;
            }
        }
    }

    /**
     * Flattens a geometries coordinates so they are continuous if they wrap
     * over the 180/-180 line. Returns the min/max offset used for flattening.
     * min/max offset of 0.0 means no flattening occurred, all points were
     * continuous between -180/180
     * 
     * @param geom
     * @return null if geometry does not need to be corrected
     */
    private double[] flattenGeometry(Geometry geom) {
        boolean handle = false;
        double currOffset = 0.0;
        double minOffset = 0.0, maxOffset = 0.0;
        Coordinate[] coords = geom.getCoordinates();
        int length = coords.length;
        // Never check last point because we dont' ever want to offset first
        // point (i+1)
        for (int i = 0; i < length - 1; ++i) {
            int ip1 = i + 1;
            Coordinate a = coords[i];
            Coordinate b = coords[ip1];

            double testX = b.x + currOffset;

            Boolean low = null;
            if (a.x - testX > 180.0) {
                low = false;
            } else if (testX - a.x > 180.0) {
                low = true;
            } else if (checker.check(a.x, testX)) {
                handle = true;
            }

            if (low != null) {
                handle = true;
                // we wrap either low end or high
                if (low) {
                    currOffset -= 360;
                    if (currOffset < minOffset) {
                        minOffset = currOffset;
                    }
                } else {
                    currOffset += 360;
                    if (currOffset > maxOffset) {
                        maxOffset = currOffset;
                    }
                }
                b.x += currOffset;
            } else {
                b.x = testX;
            }

        }
        return handle ? new double[] { minOffset, maxOffset } : null;
    }

    private static void extractPolygons(List<Polygon> polygons, Geometry geom) {
        if (geom instanceof Polygon) {
            polygons.add((Polygon) geom);
        } else if (geom instanceof GeometryCollection) {
            for (int n = 0; n < geom.getNumGeometries(); ++n) {
                extractPolygons(polygons, geom.getGeometryN(n));
            }
        }
    }
}
