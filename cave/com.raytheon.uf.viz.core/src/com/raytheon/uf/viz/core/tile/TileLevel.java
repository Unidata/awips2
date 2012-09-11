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
package com.raytheon.uf.viz.core.tile;

import java.util.ArrayList;
import java.util.List;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.util.WorldWrapCorrector;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * This object represents a single tile level. It does this by containing a 2
 * dimensional array of {@link Tile} objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 8, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TileLevel {

    /** Tile level's GridGeometry */
    private GridGeometry2D levelGeometry;

    /** GridGeometry tile level was created for */
    private GeneralGridGeometry targetGeometry;

    // Cached MathTransforms
    private MathTransform crsToGrid;

    private MathTransform gridToCRS;

    private MathTransform tileCRSToTargetGrid;

    /** Target grid to lat/lon for world wrap correcting */
    private MathTransform targetGridToLatLon;

    /** World wrap corrector, corrects Tile borders */
    private WorldWrapCorrector corrector;

    /** Level of this TileLevel */
    private int tileLevel;

    /** size for tiles in this level */
    private int tileSize;

    /** Pixel density of the tile level */
    private double pixelDensity;

    /** Tile array */
    private Tile[][] tiles;

    TileLevel(GridGeometry2D levelGeometry, GeneralGridGeometry targetGeometry,
            int tileLevel, int tileSize) {
        this.levelGeometry = levelGeometry;
        this.targetGeometry = targetGeometry;
        this.tileLevel = tileLevel;
        this.tileSize = tileSize;
        intialize(levelGeometry.getGridRange(), levelGeometry.getEnvelope());
    }

    private void intialize(GridEnvelope range, Envelope envelope) {
        int width = range.getSpan(0);
        int height = range.getSpan(1);
        int fullTileDimX = width / tileSize;
        int tileRemainderX = width % tileSize;
        int fullTileDimY = height / tileSize;
        int tileRemainderY = height % tileSize;

        int totalTilesX = fullTileDimX;
        if (tileRemainderX > 0) {
            totalTilesX++;
        }

        int totalTilesY = fullTileDimY;
        if (tileRemainderY > 0) {
            totalTilesY++;
        }

        tiles = new Tile[totalTilesY][totalTilesX];

        try {
            gridToCRS = levelGeometry.getGridToCRS(PixelInCell.CELL_CORNER);
            crsToGrid = gridToCRS.inverse();

            DefaultMathTransformFactory factory = new DefaultMathTransformFactory();
            tileCRSToTargetGrid = factory.createConcatenatedTransform(
                    CRSCache.getInstance().findMathTransform(
                            levelGeometry.getCoordinateReferenceSystem(),
                            targetGeometry.getCoordinateReferenceSystem()),
                    targetGeometry.getGridToCRS(PixelInCell.CELL_CORNER)
                            .inverse());
            targetGridToLatLon = factory.createConcatenatedTransform(
                    targetGeometry.getGridToCRS(PixelInCell.CELL_CORNER),
                    CRSCache.getInstance().findMathTransform(
                            targetGeometry.getCoordinateReferenceSystem(),
                            DefaultGeographicCRS.WGS84));
            corrector = new WorldWrapCorrector(targetGeometry);

            // Calculate pixel density
            // Grab the center x, 3/4 y of the map
            double mapXCenter = targetGeometry.getGridRange().getSpan(0) * 0.5;
            double mapYCenter = targetGeometry.getGridRange().getSpan(1) * 0.75;

            double[] input = new double[] { mapXCenter, mapYCenter,
                    mapXCenter + 1, mapYCenter + 1 };
            double[] output = new double[input.length];

            tileCRSToTargetGrid.inverse().transform(input, 0, output, 0, 2);
            levelGeometry.getGridToCRS(PixelInCell.CELL_CORNER).inverse()
                    .transform(output, 0, input, 0, 2);
            pixelDensity = 1.0 / Math.abs(new Coordinate(input[0], input[1],
                    0.0).distance(new Coordinate(input[2], input[3], 0.0)));
        } catch (Exception e) {
            throw new RuntimeException(
                    "Cannot tranform tile CRS into target CRS", e);
        }

    }

    /**
     * Returns the pixel density for the tile level. This is the approximate
     * number of target grid pixels a single tile grid pixel takes up
     * 
     * @return
     */
    public double getPixelDensity() {
        return pixelDensity;
    }

    /**
     * The number of tiles in the x direction (number of columns)
     * 
     * @return
     */
    public int getNumXTiles() {
        return tiles[0].length;
    }

    /**
     * The number of tiles in the y direction (number of rows)
     * 
     * @return
     */
    public int getNumYTiles() {
        return tiles.length;
    }

    /**
     * Level of this TileLevel
     * 
     * @return
     */
    public int getLevel() {
        return tileLevel;
    }

    /**
     * Get the Tile at the specified x/y index in the tile set
     * 
     * @param x
     * @param y
     * @return
     */
    public Tile getTile(int x, int y) {
        Tile tile = tiles[y][x];
        if (tile == null) {
            synchronized (tiles) {
                // Double check tile to see if another thread created it
                tile = tiles[y][x];
                if (tile == null) {
                    tiles[y][x] = tile = createTile(x, y);
                }
            }
        }
        return tile;
    }

    /**
     * Gets the Tile for the specified tile grid location
     * 
     * @param x
     * @param y
     * @return
     */
    public Tile getTile(double x, double y) {
        double xIdx = x / tileSize;
        double yIdx = y / tileSize;
        if (xIdx >= 0 && yIdx >= 0 && xIdx < getNumXTiles()
                && yIdx < getNumYTiles()) {
            Tile tile = getTile((int) xIdx, (int) yIdx);
            if (tile.gridContains((int) x, (int) y)) {
                return tile;
            }
        }
        return null;
    }

    /**
     * Transforms TileLevel crs x,y into tile level grid space x,y
     * 
     * @param x
     * @param y
     * @return
     */
    public double[] crsToGrid(double x, double y) throws TransformException {
        double[] out = new double[2];
        crsToGrid.transform(new double[] { x, y }, 0, out, 0, 1);
        return out;
    }

    /**
     * Creates a Tile for the specified x/y {@link #tiles} index
     * 
     * @param x
     * @param y
     * @return
     */
    private Tile createTile(int x, int y) {
        GridEnvelope range = levelGeometry.getGridRange();
        // Get grid range ranges and calculate grid range for the tile
        int startX = range.getLow(0);
        int startY = range.getLow(1);
        int endX = range.getHigh(0) + 1;
        int endY = range.getHigh(1) + 1;

        int tileY = startY + y * tileSize;
        int tileX = startX + x * tileSize;

        int tileEndX = Math.min(endX, tileX + tileSize);
        int tileEndY = Math.min(endY, tileY + tileSize);

        // Convert grid range into crs envelope range
        double[] in = new double[] { tileX, tileY, tileEndX, tileEndY };
        double[] out = new double[in.length];
        try {
            gridToCRS.transform(in, 0, out, 0, 2);
        } catch (TransformException e) {
            throw new RuntimeException("Error getting tile envelope from grid",
                    e);
        }
        double envTileX = out[0];
        double envTileY = out[1];
        double envTileEndX = out[2];
        double envTileEndY = out[3];

        // Create tile GridGeometry
        range = new GeneralGridEnvelope(new int[] { tileX, tileY }, new int[] {
                tileEndX, tileEndY }, false);
        GridGeometry2D tileGridGeom = new GridGeometry2D(range, new Envelope2D(
                levelGeometry.getCoordinateReferenceSystem(), out[0], envTileY,
                envTileEndX - envTileX, envTileEndY - envTileY));

        // Calculate the border in target grid space for the Tile
        Geometry border = null;
        try {
            double[] UL = new double[] { envTileX, envTileY };
            double[] UR = new double[] { envTileEndX, envTileY };
            double[] LR = new double[] { envTileEndX, envTileEndY };
            double[] LL = new double[] { envTileX, envTileEndY };

            // Create tile border based on pixel density tile level 0 should
            // always have threshold of 1.0
            border = createTileBorder(UL, UR, LR, LL,
                    Math.max(range.getSpan(0) / 4, 1),
                    Math.max(range.getSpan(1) / 4, 1),
                    tileLevel > 0 ? Math.max(pixelDensity, 1.0) : 1.0);
        } catch (TransformException e) {
            // Invalid geometry, don't add a border
        }

        // Create the Tile object
        return new Tile(tileLevel, tileGridGeom, border);
    }

    /**
     * Ensures all Tile objects are created for the level. This method can takes
     * lots of time depending on grid resolution of level and {@link #tileSize}.
     * Tiles are created dynamically as requested otherwise
     */
    public void populateTiles() {
        int totalTilesY = getNumYTiles();
        int totalTilesX = getNumXTiles();
        for (int y = 0; y < totalTilesY; ++y) {
            for (int x = 0; x < totalTilesX; ++x) {
                if (tiles[y][x] == null) {
                    tiles[y][x] = createTile(x, y);
                }
            }
        }
    }

    private Geometry createTileBorder(double[] UL, double[] UR, double[] LR,
            double[] LL, int maxHorDivisions, int maxVertDivisions,
            double threshold) throws TransformException {
        List<Coordinate> borderPoints = new ArrayList<Coordinate>(
                maxVertDivisions * 2 + maxHorDivisions * 2);
        double[] out = new double[2];

        // UL to UR
        tileCRSToTargetGrid.transform(UL, 0, out, 0, 1);
        borderPoints.add(new Coordinate(out[0], out[1]));
        calculateBorder(borderPoints, UL, null, UR, null, maxHorDivisions,
                threshold);

        // UR to LR
        tileCRSToTargetGrid.transform(UR, 0, out, 0, 1);
        borderPoints.add(new Coordinate(out[0], out[1]));
        calculateBorder(borderPoints, UR, null, LR, null, maxVertDivisions, 1.0);

        // LR to LL
        tileCRSToTargetGrid.transform(LR, 0, out, 0, 1);
        borderPoints.add(new Coordinate(out[0], out[1]));
        calculateBorder(borderPoints, LR, null, LL, null, maxHorDivisions, 1.0);

        // LL to UL
        tileCRSToTargetGrid.transform(LL, 0, out, 0, 1);
        borderPoints.add(new Coordinate(out[0], out[1]));
        calculateBorder(borderPoints, LL, null, UL, null, maxVertDivisions, 1.0);

        // Add start point to complete linear ring
        tileCRSToTargetGrid.transform(UL, 0, out, 0, 1);
        borderPoints.add(new Coordinate(out[0], out[1]));

        // Create Geometry and world wrap correct (need to be in lat/lon)
        return JTS.transform(corrector.correct(JTS.transform(TileSet.gf
                .createPolygon(TileSet.gf.createLinearRing(borderPoints
                        .toArray(new Coordinate[borderPoints.size()])), null),
                targetGridToLatLon)), targetGridToLatLon.inverse());
    }

    private int calculateBorder(List<Coordinate> borderList, double[] point1,
            double[] transformedPoint1, double[] point3,
            double[] transformedPoint3, double maxNumDivs, double threshold)
            throws TransformException {
        if (transformedPoint1 == null) {
            transformedPoint1 = new double[point1.length];
            tileCRSToTargetGrid.transform(point1, 0, transformedPoint1, 0, 1);
        }
        if (transformedPoint3 == null) {
            transformedPoint3 = new double[point3.length];
            tileCRSToTargetGrid.transform(point3, 0, transformedPoint3, 0, 1);
        }
        if (transformedPoint1 == null || transformedPoint3 == null) {
            // if the image has some points outside the valid range of the
            // screen then give up optimizing and assume the max number of
            // points.
            return (int) Math.ceil(maxNumDivs);
        }
        double[] point2 = { (point1[0] + point3[0]) / 2,
                (point1[1] + point3[1]) / 2 };
        double[] transformedPoint2 = new double[point2.length];
        tileCRSToTargetGrid.transform(point2, 0, transformedPoint2, 0, 1);
        double[] interp2 = { (transformedPoint1[0] + transformedPoint3[0]) / 2,
                (transformedPoint1[1] + transformedPoint3[1]) / 2 };
        double dX = transformedPoint2[0] - interp2[0];
        double dY = transformedPoint2[1] - interp2[1];
        double d = Math.hypot(dX, dY);
        if (d < threshold || maxNumDivs < 1) {
            return 1;
        } else {
            int nd1 = calculateBorder(borderList, point1, transformedPoint1,
                    point2, transformedPoint2, maxNumDivs / 2, threshold);
            borderList.add(new Coordinate(transformedPoint2[0],
                    transformedPoint2[1]));
            if (nd1 * 2 >= maxNumDivs) {
                nd1 = (int) Math.ceil(maxNumDivs);
            }
            int nd2 = calculateBorder(borderList, point2, transformedPoint2,
                    point3, transformedPoint3, maxNumDivs / 2, threshold);
            if (nd2 * 2 >= maxNumDivs) {
                nd2 = (int) Math.ceil(maxNumDivs);
            }
            return (Math.max(nd1, nd2) * 2);
        }
    }
}
