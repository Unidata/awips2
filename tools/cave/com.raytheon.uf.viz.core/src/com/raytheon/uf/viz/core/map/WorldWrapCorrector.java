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
package com.raytheon.uf.viz.core.map;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
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
     * Constructs of world wrap corrector for the specified descriptor
     * 
     * @param descriptor
     */
    public WorldWrapCorrector(IMapDescriptor descriptor) {
        checker = new WorldWrapChecker(descriptor);
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
            boolean handle = false;
            if (checker.needsChecking()) {
                boolean polygon = g instanceof Polygon;
                Coordinate[] coords = g.getCoordinates();
                if (polygon) {
                    // remove duplicate last point for polygon
                    coords = Arrays.copyOf(coords, coords.length - 1);
                }
                int length = coords.length + (polygon ? 0 : -1);
                int truLen = coords.length;
                double currOffset = 0.0;
                double minOffset = 0.0, maxOffset = 0.0;
                for (int i = 0; i < length; ++i) {
                    int ip1 = (i + 1) % truLen;
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
                    } else if (checker.check(a.x, b.x)) {
                        handle = true;
                    }

                    if (low != null) {
                        handle = true;
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
                if (handle) {
                    // All coords in geometry should be denormalized now, get
                    // adjusted envelope, divide envelope into sections, for
                    // each section, intersect with geometry and add to
                    // geom list
                    List<Geometry> sections = new ArrayList<Geometry>();
                    List<Double> rolls = new ArrayList<Double>();
                    GeometryFactory gf = g.getFactory();
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
                        section = section.intersection(g);
                        if (section.isEmpty() == false) {
                            sections.add(section);
                            rolls.add(useEnd);
                        }
                        start += 360;
                    }

                    // We need to roll the geometries back into the -180 to 180
                    // range. That is why we kept track of the meridian used
                    for (int i = 0; i < sections.size(); ++i) {
                        Geometry section = sections.get(i);
                        double rollVal = rolls.get(i);
                        rollLongitudes(section, rollVal, inverseCentralMeridian);
                        geomList.add(section);
                    }
                }
            }

            if (!handle) {
                geomList.add(g);
            }
        }
    }

    private void rollLongitudes(Geometry g, double inverseCentralMeridianUsed,
            double inverseCentralMeridian) {
        double diff = inverseCentralMeridian - inverseCentralMeridianUsed;
        if (diff != 0) {
            for (Coordinate c : g.getCoordinates()) {
                c.x += diff;
            }
        }
    }
}
