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
import java.util.List;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * This class uses the WorldWrapChecker to correct wrapping of geometries that
 * are in normalized lat/lon (-180-180) coordinates for the projection on the
 * descriptor passed in
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2011            mschenke     Initial creation
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
        checker = new WorldWrapChecker(worldGeometry);
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
            wrapCorrect(geom, geoms, checker.getInverseCentralMeridian());
        }
        return geom.getFactory().createGeometryCollection(
                geoms.toArray(new Geometry[geoms.size()]));
    }

    /**
     * Recursive function to preprocess primitive geometries (non
     * collections/non points) and look for wrapping that will occur on the
     * descriptor's map
     * 
     * @param g
     * @param geomList
     */
    private void wrapCorrect(Geometry g, List<Geometry> geomList,
            double inverseCentralMeridian) {
        if (g instanceof GeometryCollection) {
            for (int n = 0; n < g.getNumGeometries(); ++n) {
                wrapCorrect(g.getGeometryN(n), geomList, inverseCentralMeridian);
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
            if (checker.needsChecking()) {
                List<Geometry> geoms = new ArrayList<Geometry>();
                if (g instanceof Polygon) {
                    GeometryFactory gf = g.getFactory();
                    Polygon p = (Polygon) g;
                    LineString extRing = p.getExteriorRing();
                    Polygon extPolygon = gf
                            .createPolygon(gf.createLinearRing(extRing
                                    .getCoordinates()), null);
                    double[] offsets = flattenGeometry(extPolygon);
                    List<Geometry> extRings = new ArrayList<Geometry>();
                    correct(extRings, extPolygon, inverseCentralMeridian,
                            offsets[0], offsets[1]);
                    List<Geometry> intRings = new ArrayList<Geometry>(
                            p.getNumInteriorRing());
                    for (int n = 0; n < p.getNumInteriorRing(); ++n) {
                        Polygon intRing = gf.createPolygon(gf
                                .createLinearRing(p.getInteriorRingN(n)
                                        .getCoordinates()), null);
                        offsets = flattenGeometry(intRing);
                        correct(intRings, intRing, inverseCentralMeridian,
                                offsets[0], offsets[1]);
                    }
                    for (Geometry ext : extRings) {
                        for (int n1 = 0; n1 < ext.getNumGeometries(); ++n1) {
                            Geometry geom = ext.getGeometryN(n1);
                            for (Geometry intRing : intRings) {
                                for (int n2 = 0; n2 < intRing
                                        .getNumGeometries(); ++n2) {
                                    geom = geom.difference(intRing
                                            .getGeometryN(n2));
                                }
                            }
                            geoms.add(geom);
                        }
                    }
                } else {
                    double[] offsets = flattenGeometry(g);
                    double minOffset = offsets[0];
                    double maxOffset = offsets[1];
                    correct(geoms, g, inverseCentralMeridian, minOffset,
                            maxOffset);
                }
                for (Geometry geom : geoms) {
                    rollGeometry(geom);
                }
                geomList.addAll(geoms);
            } else {
                geomList.add(g);
            }
        }
    }

    private void correct(List<Geometry> geoms, Geometry flattenedGeom,
            double inverseCentralMeridian, double minOffset, double maxOffset) {
        GeometryFactory gf = flattenedGeom.getFactory();
        double delta = 0.00001;
        double start = inverseCentralMeridian + minOffset - 360;
        double end = inverseCentralMeridian + maxOffset + 360;
        double minY = -90, maxY = 90;

        while (start < end) {
            double useStart = start;
            double useEnd = start + 360;
            double minX = useStart + delta;
            double maxX = (useEnd) - delta;

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
            start += 360.0;
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
            while (c.x <= -180.0) {
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
     */
    private double[] flattenGeometry(Geometry geom) {
        double currOffset = 0.0;
        double minOffset = 0.0, maxOffset = 0.0;
        Coordinate[] coords = geom.getCoordinates();
        int length = coords.length;
        for (int i = 0; i < length; ++i) {
            int ip1 = (i + 1) % length;
            Coordinate a = coords[i];
            Coordinate b = coords[ip1];

            if (ip1 != 0) {
                b.x += currOffset;
            }

            Boolean low = null;
            if (a.x - b.x > 180.0) {
                low = false;
            } else if (b.x - a.x > 180.0) {
                low = true;
            }

            if (low != null) {
                // we wrap either low end or high
                if (low) {
                    currOffset -= 360;
                    b.x -= 360.0;
                    if (currOffset < minOffset) {
                        minOffset = currOffset;
                    }
                } else {
                    currOffset += 360;
                    b.x += 360;
                    if (currOffset > maxOffset) {
                        maxOffset = currOffset;
                    }
                }
            }
        }
        return new double[] { minOffset, maxOffset };
    }

}
