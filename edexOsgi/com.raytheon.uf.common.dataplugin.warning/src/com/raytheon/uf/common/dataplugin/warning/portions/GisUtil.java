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

package com.raytheon.uf.common.dataplugin.warning.portions;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;

import org.geotools.referencing.GeodeticCalculator;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * 
 * GisUtil - Utilities for Warngen GIS
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Dec 7, 2007  #601        chammack    Initial Creation.
 *    Oct 31, 2011 #11077      Qinglu Lin  Added convertAlaskaLons(), which
 *                                         convert longitudes for Alaska when
 *                                         they're in Eastern Hemisphere; invoke
 *                                         it in convertCoords().
 *    Feb 29, 2012 #13596      Qinglu Lin  Added restoreAlaskaLon().
 *    May  9, 2012 #14887      Qinglu Lin  Change 0.1 to 0.16875f for PORTION_OF_CENTER; 
 *                                         0.10 to 0.0625 for EXTREME_DELTA; Added/modified code.
 *    May  1, 2013  1963       jsanchez    Refactored calculatePortion to match A1. Do not allow 'Central' to be included if East and West is included.
 *    Jun  3, 2013  2029       jsanchez    Updated A1 special case for calculating a central portion. Allowed East Central and West Central.
 *    Dec  4, 2013  2604       jsanchez    Moved out of viz.warngen.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class GisUtil {

    private static final float DIRECTION_DELTA = 15;

    public static final ThreadLocal<GeodeticCalculator> gc = new ThreadLocal<GeodeticCalculator>() {
        @Override
        protected GeodeticCalculator initialValue() {
            GeodeticCalculator gc = new GeodeticCalculator();
            return gc;
        }
    };

    public static enum Direction {
        CENTRAL, NORTH, SOUTH, EAST, WEST, EXTREME
    };

    /**
     * Ported getAreaDesc from A1 code method GeoEntityLookupTable.getAreaDesc
     * 
     * @param parentGeom
     *            - the parent geometry such as the impacted county/zone
     * @param warnedArea
     *            - the intersection geometry of the hatched warned geometry and
     *            the parent geometry
     * @param useCentral
     *            - boolean flag to allow CENTRAL portion to be included.
     * @param useExtreme
     *            - boolean flag to allow EXTREME portion to be included.
     * @return
     */
    public static EnumSet<Direction> calculatePortion(Geometry parentGeom,
            Geometry warnedArea, boolean useCentral, boolean useExtreme) {
        EnumSet<Direction> portions = EnumSet.noneOf(Direction.class);
        ImpactedQuadrants iQuad = ImpactedQuadrants.findImpactedQuadrants(
                parentGeom, warnedArea, useCentral);

        // Test for case where we cannot do portions or if the warnedArea covers
        // the parentGeom
        if (parentGeom == null || warnedArea == null
                || parentGeom.equals(warnedArea)) {
            return EnumSet.noneOf(Direction.class);
        }

        // Test for central by not being near adjacent borders.
        if (useCentral && iQuad.centralGeom != null
                && iQuad.centralGeom.intersects(warnedArea) && !iQuad.north
                && !iQuad.south && !iQuad.east && !iQuad.west) {
            portions.add(Direction.CENTRAL);
            return portions;
        }

        // If the parentGeom is oddly shaped (i.e. BROOMFIELD), the intended
        // special cases won't work. A basic comparison can only be applied.
        Coordinate centre = parentGeom.getEnvelopeInternal().centre();
        GeometryFactory gf = new GeometryFactory();
        if (!parentGeom.intersects(gf.createPoint(centre))) {
            return getPointDesc(iQuad, useExtreme);
        }

        // Possible case of a stripe across the middle
        if (iQuad.q == 0) {
            // Only one direction encoded
            ; // <-- Ported A1 code
        } else if ((iQuad.q == 2 && iQuad.nw == iQuad.se)
                || (iQuad.q == 2 && iQuad.ne == iQuad.sw)
                || (iQuad.qq == 2 && iQuad.nn == iQuad.ss)
                || (iQuad.qq == 2 && iQuad.ee == iQuad.ww)) {
            return getPointDesc(iQuad, useExtreme);
        }

        // All quadrants in use.
        if (iQuad.q == 4 && iQuad.qq == 4) {
            if (useCentral
                    && ((iQuad.north && iQuad.south && !iQuad.east && !iQuad.west) || (iQuad.east
                            && iQuad.west && !iQuad.north && !iQuad.south))) {
                // Add CENTRAL if north and south are impacted, but not east and
                // west. Apply vice versa
                portions.add(Direction.CENTRAL);
                return portions;
            }
            return EnumSet.noneOf(Direction.class);
        }
        // Only one typical quadrant in use.
        if (iQuad.q == 1) {
            return getPointDesc(iQuad, useExtreme);
        }

        // No more than two quadrants of any kind in us, or all quadrants.
        if (iQuad.q < 3 && iQuad.qq < 3) {
            if (iQuad.nnx != iQuad.ssx
                    && iQuad.wwx != iQuad.eex
                    || (iQuad.centralGeom != null && iQuad.centralGeom
                            .intersects(warnedArea))) {
                return getPointDesc(iQuad, useExtreme);
            }
        }

        // Three typical quadrants in use.
        if (iQuad.q == 3 && iQuad.qq != 3) {
            if (iQuad.ne != 1 && (iQuad.ssw || iQuad.wsw)) {
                portions.add(Direction.SOUTH);
                portions.add(Direction.WEST);
            } else if (iQuad.se != 1 && (iQuad.nnw || iQuad.wnw)) {
                portions.add(Direction.NORTH);
                portions.add(Direction.WEST);
            } else if (iQuad.nw != 1 && (iQuad.sse || iQuad.ese)) {
                portions.add(Direction.SOUTH);
                portions.add(Direction.EAST);
            } else if (iQuad.sw != 1 && (iQuad.nne || iQuad.ene)) {
                portions.add(Direction.NORTH);
                portions.add(Direction.EAST);
            }
        }

        // add extreme for three quadrant case.
        if (!portions.isEmpty()) {
            if (useExtreme && iQuad.xxx > 0) {
                portions.add(Direction.EXTREME);
                return portions;
            }
        }

        // All of either type of quadrant in use.
        if (iQuad.q == 4 && iQuad.qq == 4) {
            return EnumSet.noneOf(Direction.class);
        }

        // Case of a pure simple direction.
        if (iQuad.ss == 1 && iQuad.nn == 1 || iQuad.q == 0) {
            if (iQuad.nn == 0 && iQuad.ww == 1) {
                portions.add(Direction.WEST);
            }
            if (iQuad.ww == 0 && iQuad.ee == 1) {
                portions.add(Direction.EAST);
            }
        } else if (iQuad.ee == 1 && iQuad.ww == 1 || iQuad.q == 0) {
            if (iQuad.nn == 0 && iQuad.ss == 1) {
                portions.add(Direction.SOUTH);
            }
            if (iQuad.ss == 0 && iQuad.nn == 1) {
                portions.add(Direction.NORTH);
            }
        }

        // add extreme for simple direction case.
        if (portions.isEmpty() == false) {
            if (useExtreme && iQuad.xxx > 0) {
                portions.add(Direction.EXTREME);
            }
            return portions;
        }

        // Catch with the point descriptor one last time
        return getPointDesc(iQuad, useExtreme);
    }

    /**
     * Determines the portion of an area based on the ImpactedQuadrants object
     * 
     * @param iQuad
     *            - ImpactedQuadrants object
     * @param useExtrme
     *            - boolean flag to allow EXTREME portion to be included.
     * @return
     */
    private static EnumSet<Direction> getPointDesc(ImpactedQuadrants iQuad,
            boolean useExtrme) {
        EnumSet<Direction> portions = EnumSet.noneOf(Direction.class);
        int counter = 0;

        if (iQuad.north && !iQuad.south) {
            portions.add(Direction.NORTH);
            counter++;
        } else if (iQuad.south && !iQuad.north) {
            portions.add(Direction.SOUTH);
            counter++;
        }

        if (iQuad.east && !iQuad.west) {
            portions.add(Direction.EAST);
            counter++;
        } else if (iQuad.west && !iQuad.east) {
            portions.add(Direction.WEST);
            counter++;
        }

        // Only add CENTRAL if only one portion was set. For example, NORTH EAST
        // CENTRAL is not allowed.
        if (iQuad.cc && counter < 2) {
            portions.add(Direction.CENTRAL);
        }

        if (!portions.isEmpty() && useExtrme && iQuad.xxx > 0) {
            portions.add(Direction.EXTREME);
        }
        return portions;
    }

    public static List<String> asStringList(EnumSet<Direction> set) {
        List<String> list = new ArrayList<String>();
        Iterator<Direction> dirIter = set.iterator();
        while (dirIter.hasNext()) {
            list.add(dirIter.next().toString());
        }

        if (list.isEmpty()) {
            return null;
        }
        return list;
    }

    public static Coordinate d2dCoordinate(Coordinate oldCoord) {
        Coordinate coord = new Coordinate();
        if (oldCoord.x > 150. && oldCoord.y > 45.) {
            coord.x = oldCoord.x - 360.;
        } else {
            coord.x = oldCoord.x;
        }
        coord.y = oldCoord.y;

        return coord;

    }

    public static Coordinate[] d2dCoordinates(Coordinate[] oldCoords) {
        int length = oldCoords.length;
        Coordinate[] coords = new Coordinate[length];
        for (int i = 0; i < length; i++) {
            coords[i] = d2dCoordinate(oldCoords[i]);
        }
        return coords;
    }

    public static Point2D[] convertCoords(Coordinate[] oldCoords) {
        Coordinate[] coords = d2dCoordinates(oldCoords);
        ArrayList<Point2D> pts = new ArrayList<Point2D>();
        int firstX = (int) (coords[0].x * 100);
        int firstY = (int) (coords[0].y * 100);
        pts.add(new Point2D.Double(coords[0].x, coords[0].y));
        for (int i = 0; i < coords.length - 1; i++) {
            if (!((int) (coords[i].x * 100) == firstX && (int) (coords[i].y * 100) == firstY)) {
                pts.add(new Point2D.Double(coords[i].x, coords[i].y));
            }
        }
        Point2D[] rval = new Point2D.Double[pts.size()];
        for (int i = 0; i < pts.size(); i++) {
            rval[i] = pts.get(i);
        }
        return rval;
    }

    public static boolean equivalent(Geometry oldGeom, Geometry newGeom) {
        Point2D[] old = GisUtil.convertCoords(oldGeom.getCoordinates());
        Point2D[] neww = GisUtil.convertCoords(newGeom.getCoordinates());
        return Arrays.equals(old, neww);
    }

    /**
     * restoreAlaskaLon()
     * 
     * Feb 28, 2012 DR13596 Qinglu Lin Created.
     * 
     * If the longitude of a Coordinate is less than -180 and corresponding
     * latitude is larger than 45 degree North, convert it to a value equivalent
     * to (360 + the longitude).
     */
    public static Coordinate restoreAlaskaLon(Coordinate oldCoords) {
        Coordinate coord = new Coordinate();
        if (oldCoords.x < -180. && oldCoords.y > 45.) {
            coord.x = 360. + oldCoords.x;
        } else {
            coord.x = oldCoords.x;
        }
        coord.y = oldCoords.y;
        return coord;
    }

    /**
     * Calculates the cardinal directions of a location.
     * 
     * @param locationGeom
     * @param reference
     * @param gc
     * @return
     */
    public static EnumSet<Direction> calculateLocationPortion(
            Geometry locationGeom, Geometry reference, boolean useExtreme) {
        for (int i = 0; i < locationGeom.getNumGeometries(); i++) {
            Geometry geom = locationGeom.getGeometryN(i);
            if (geom.intersects(reference)) {

                Coordinate geomCentroid = geom.getEnvelope().getCentroid()
                        .getCoordinate();
                Coordinate refCentroid = reference.getCentroid()
                        .getCoordinate();

                EnumSet<Direction> portions = EnumSet.noneOf(Direction.class);

                gc.get().setStartingGeographicPoint(geomCentroid.x,
                        geomCentroid.y);
                gc.get().setDestinationGeographicPoint(refCentroid.x,
                        refCentroid.y);
                double azimuth = gc.get().getAzimuth();

                if (azimuth < (180 - DIRECTION_DELTA)
                        && azimuth > DIRECTION_DELTA) {
                    portions.add(Direction.EAST);
                } else if (azimuth < 0 - DIRECTION_DELTA
                        && azimuth > DIRECTION_DELTA - 180) {
                    portions.add(Direction.WEST);
                }

                if (Math.abs(azimuth) < (90 - DIRECTION_DELTA)) {
                    portions.add(Direction.NORTH);
                } else if (Math.abs(azimuth) > (90 + DIRECTION_DELTA)) {
                    portions.add(Direction.SOUTH);
                }

                boolean isExtreme = false;
                if (!portions.isEmpty() && useExtreme && isExtreme) {
                    portions.add(Direction.EXTREME);
                }

                return portions;
            }
        }

        return EnumSet.noneOf(Direction.class);

    }

}
