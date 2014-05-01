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

import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.PackedCoordinateSequence;

/**
 * Solves the problem of intersecting two envelopes in different coordinate
 * systems by reprojecting a grid of points and building polygons out of
 * adjacent grid cells. This can be significantly slower than other algorithms
 * but will always produce the best result possible for a given width and
 * height. Increasing the width/height will slow the algorithm but give better
 * results.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 14, 2013  2528     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
class BruteForceEnvelopeIntersection {

    private final ReferencedEnvelope sourceEnvelope;

    private final ReferencedEnvelope targetEnvelope;

    private final WorldWrapChecker wwc;

    private final int width;

    private final int height;

    private final MathTransform latLonToTargetCRS;

    private final CoordinateSequence latLonCoords;

    private final CoordinateSequence targetCoords;

    /**
     * Construct a new intersection. This will calculate all fields including
     * reprojecting all points in the grid.
     */
    private BruteForceEnvelopeIntersection(Envelope sourceEnvelope,
            Envelope targetEnvelope, int width, int height)
            throws FactoryException, TransformException {
        if (sourceEnvelope instanceof ReferencedEnvelope) {
            this.sourceEnvelope = (ReferencedEnvelope) sourceEnvelope;
        } else {
            this.sourceEnvelope = new ReferencedEnvelope(sourceEnvelope);
        }
        if (targetEnvelope instanceof ReferencedEnvelope) {
            this.targetEnvelope = (ReferencedEnvelope) targetEnvelope;
        } else {
            this.targetEnvelope = new ReferencedEnvelope(targetEnvelope);
        }
        this.width = width;
        this.height = height;
        wwc = new WorldWrapChecker(this.targetEnvelope);

        double[] latLonCoords = buildLatLonCoords();
        this.latLonCoords = new PackedCoordinateSequence.Double(latLonCoords, 2);

        latLonToTargetCRS = MapUtil
                .getTransformFromLatLon(targetEnvelope
                        .getCoordinateReferenceSystem());
        double[] targetCoords = new double[latLonCoords.length];
        latLonToTargetCRS.transform(latLonCoords, 0, targetCoords, 0, width
                * height);
        this.targetCoords = new PackedCoordinateSequence.Double(targetCoords, 2);
    }

    /**
     * Construct a grid of coordinates and reproject to lat/lon CRS.
     */
    private double[] buildLatLonCoords() throws FactoryException,
            TransformException {
        MathTransform sourceCRSToLatLon = MapUtil
                .getTransformToLatLon(sourceEnvelope
                        .getCoordinateReferenceSystem());
        double worldMinX = this.sourceEnvelope.getMinX();
        double worldMinY = this.sourceEnvelope.getMinY();
        double dXWorld = this.sourceEnvelope.getWidth() / (width - 1);
        double dYWorld = this.sourceEnvelope.getHeight() / (height - 1);

        int index = 0;
        double[] coordinates = new double[width * height * 2];

        for (int j = 0; j < height; ++j) {
            double y = worldMinY + j * dYWorld;
            for (int i = 0; i < width; ++i) {
                coordinates[index++] = worldMinX + i * dXWorld;
                coordinates[index++] = y;
            }
        }
        sourceCRSToLatLon.transform(coordinates, 0, coordinates, 0, width
                * height);
        return coordinates;
    }

    /**
     * Perform the actual intersection operation by merging all {@link Cell}s
     * into {@link SimplePolygon} and finally converting those into Polygons.
     */
    public Geometry reproject() throws MismatchedDimensionException,
            TransformException {
        /*
         * Loop over each cell and split into two groups. First group is the
         * "simple" cases that do not world wrap and can all be merged into one
         * or more polygons. Second group contains the cells that need to be
         * world wrap corrected.
         */
        List<SimplePolygon> simpleCases = new ArrayList<SimplePolygon>();
        List<Cell> worldWrappCells = new ArrayList<Cell>();
        for (int j = 1; j < height; ++j) {
            for (int i = 1; i < width; ++i) {
                Cell cell = new Cell(i, j);
                if (!cell.isValid()) {
                    // skip it
                } else if (cell.worldWrapCheck(wwc)) {
                    worldWrappCells.add(cell);
                } else {
                    for (SimplePolygon poly : simpleCases) {
                        if (poly.merge(cell)) {
                            cell = null;
                            break;
                        }
                    }
                    if (cell != null) {
                        simpleCases.add(new SimplePolygon(cell));
                    }
                }
            }
        }
        /* Convert all simple case polygons into JTS polygons */
        GeometryFactory gf = new GeometryFactory();
        List<Geometry> geoms = new ArrayList<Geometry>(simpleCases.size() + worldWrappCells.size() *2);
        for (SimplePolygon poly : simpleCases) {
            geoms.add(poly.asGeometry(gf));
        }
        if(!worldWrappCells.isEmpty()){
            /*
             * World wrap correct the cells that need it and transform the
             * corrected geometries.
             */
            WorldWrapCorrector corrector = new WorldWrapCorrector(
                    targetEnvelope);
            for (Cell cell : worldWrappCells) {
                Geometry geom = cell.asLatLonGeometry(gf);
                geom = corrector.correct(geom);
                geom = JTS.transform(geom, latLonToTargetCRS);
                if (geom.isValid() && !geom.isEmpty()) {
                    geoms.add(geom);
                }
            }
        }
        return gf.createGeometryCollection(geoms.toArray(new Geometry[0]))
                .buffer(0);
    }

    /**
     * A cell represents four connected grid points that form a continous piece
     * of an intersecting polygons. Most of the time a cell represents a
     * quadrilateral but for cases where one of the corners is invalid in the
     * target CRS a cell may represent a triangle. If any more points are
     * invalid then the cell is invalid.
     */
    private final class Cell {

        private final int[] indices;

        public Cell(int x, int y) {
            int[] indices = new int[] { y * width + x - 1,
                    (y - 1) * width + x - 1,
                    (y - 1) * width + x, y * width + x};
            int nanCount = 0;
            for (int index : indices) {
                if (isNaN(index)) {
                    nanCount += 1;
                }
            }
            if (nanCount == 0) {
                this.indices = indices;
            } else if (nanCount == 1) {
                this.indices = new int[3];
                int i = 0;
                for (int index : indices) {
                    if (!isNaN(index)) {
                        this.indices[i++] = index;
                    }
                }
            } else {
                this.indices = null;
            }
        }

        private boolean isNaN(int index){
            return Double.isNaN(targetCoords.getOrdinate(index, 0))
                    || Double.isNaN(targetCoords.getOrdinate(index, 1));
        }

        public boolean isValid() {
            return indices != null;
        }

        public boolean worldWrapCheck(WorldWrapChecker wwc) {
            double prevLon = latLonCoords.getOrdinate(
                    indices[indices.length - 1], 0);
            for (int index : indices) {
                double lon = latLonCoords.getOrdinate(index, 0);
                if (wwc.check(prevLon, lon)) {
                    return true;
                }
                prevLon = lon;
            }
            return false;

        }

        public List<Coordinate> getTargetCoords() {
            List<Coordinate> result = new ArrayList<Coordinate>(4);
            for (int index : indices) {
                result.add(targetCoords.getCoordinate(index));
            }
            return result;
        }

        public Polygon asLatLonGeometry(GeometryFactory gf) {
            Coordinate[] coordinates = new Coordinate[indices.length + 1];
            for (int i = 0; i < indices.length; i += 1) {
                coordinates[i] = latLonCoords.getCoordinate(indices[i]);
            }
            coordinates[coordinates.length - 1] = coordinates[0];
            return gf.createPolygon(gf.createLinearRing(coordinates), null);
        }

        public Coordinate getTargetCoord(int index) {
            return targetCoords.getCoordinate(indices[index % indices.length]);
        }

        public int indexOfTarget(Coordinate c) {
            for (int i = 0; i < targetCoords.size(); i += 1) {
                if (targetCoords.getOrdinate(indices[i], 0) == c.x
                        && targetCoords.getOrdinate(indices[i], 1) == c.y) {
                    return i;
                }
            }
            return -1;
        }

        public int size() {
            return indices.length;
        }
    }

    /**
     * This class is used to represent a Polygon with no holes. Additionally it
     * provides fast method for merging a cell because it can safely assume that
     * any points it shares with the cell are identical.
     */
    private static final class SimplePolygon {

        private List<Coordinate> coords;

        public SimplePolygon(Cell cell) {
            this.coords = cell.getTargetCoords();
        }

        public boolean merge(Cell cell) {
            List<Coordinate> toCheck = cell.getTargetCoords();
            /*
             * Walk coords in order, if any identical coords are found we can
             * ceck nearby points to eliminate the duplicates
             */
            for (int i = 0; i < coords.size() - 1; i += 1) {
                if (toCheck.remove(coords.get(i))) {
                    int lastFoundIndex = -1;
                    if (i == 0 && toCheck.remove(coords.get(coords.size() - 1))) {
                        /*
                         * For the 0 index the end of coords must be checked,
                         * all other indices do not need to check previous
                         * coords.
                         */
                        while (toCheck.remove(coords.get(coords.size() - 2))) {
                            coords.remove(coords.size() - 1);
                        }
                        lastFoundIndex = 0;
                    } else if (i + 1 < coords.size()
                            && toCheck.remove(coords.get(i + 1))) {
                        /* This check ensures 2 points match. */
                        lastFoundIndex = i + 1;
                    }
                    if (lastFoundIndex != -1) {
                        while (lastFoundIndex + 1 < coords.size()
                                && toCheck.remove(coords
                                        .get(lastFoundIndex + 1))) {
                            /*
                             * If more than two points match, remove the common
                             * interior points.
                             */
                            coords.remove(lastFoundIndex);
                        }
                        if (!toCheck.isEmpty()) {
                            /*
                             * Add any exterior remaining points from the cell
                             * into this.
                             */
                            int cellLastFoundIndex = cell.indexOfTarget(coords
                                    .get(lastFoundIndex));
                            int prevIndex = cellLastFoundIndex + cell.size() - 1;
                            int nextIndex = cellLastFoundIndex + 1;
                            if (toCheck
                                    .contains(cell.getTargetCoord(nextIndex))) {
                                coords.add(lastFoundIndex,
                                        cell.getTargetCoord(nextIndex));
                                for (int j = nextIndex + 1; j < prevIndex; j += 1) {
                                    Coordinate c = cell.getTargetCoord(j);
                                    if (toCheck.contains(c)) {
                                        coords.add(lastFoundIndex, c);
                                    } else {
                                        break;
                                    }
                                }
                            } else if (toCheck.contains(cell
                                    .getTargetCoord(prevIndex))) {
                                coords.add(lastFoundIndex,
                                        cell.getTargetCoord(prevIndex));
                                for (int j = prevIndex - 1; j > nextIndex; j -= 1) {
                                    Coordinate c = cell.getTargetCoord(j);
                                    if (toCheck.contains(c)) {
                                        coords.add(lastFoundIndex, c);
                                    } else {
                                        break;
                                    }
                                }
                            }
                        }
                        return true;
                    } else {
                        return false;
                    }
                }
            }
            return false;
        }

        public Polygon asGeometry(GeometryFactory gf) {
            Coordinate[] coordinates = new Coordinate[this.coords.size() + 1];
            this.coords.toArray(coordinates);
            coordinates[coordinates.length - 1] = coordinates[0];
            return gf.createPolygon(gf.createLinearRing(coordinates), null);
        }

    }

    /**
     * Computes an intersection {@link Geometry} between sourceEnvelope and
     * targetEnvelope in targetEnvelope's CRS space. The resulting
     * {@link Geometry} may contain multiple Geometries within it. But all
     * geometries will be {@link Polygon}s
     * 
     * @param sourceEnvelope
     * @param targetEnvelope
     * @param maxHorDivisions
     * @param maxVertDivisions
     * @return
     * @throws FactoryException
     * @throws TransformException
     */
    public static Geometry createEnvelopeIntersection(Envelope sourceEnvelope,
            Envelope targetEnvelope, int maxHorDivisions, int maxVertDivisions)
            throws FactoryException, TransformException {
        return new BruteForceEnvelopeIntersection(sourceEnvelope, targetEnvelope,
                maxHorDivisions, maxVertDivisions).reproject();
    }
}
