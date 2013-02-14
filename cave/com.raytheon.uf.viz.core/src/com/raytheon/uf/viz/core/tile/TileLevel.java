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

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.util.EnvelopeIntersection;
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

            Envelope levelEnv = levelGeometry.getEnvelope();
            double[] in = new double[] {
                    levelEnv.getMinimum(0) + (levelEnv.getSpan(0) / 2),
                    levelEnv.getMinimum(1) + (levelEnv.getSpan(1) / 2) };
            double[] out = new double[in.length];
            tileCRSToTargetGrid.transform(in, 0, out, 0, 1);

            double mapPointX = out[0];
            double mapPointY = out[1];
            GridEnvelope targetEnv = targetGeometry.getGridRange();
            if (targetEnv.getLow(0) > mapPointX
                    || targetEnv.getHigh(0) < mapPointX
                    || targetEnv.getLow(1) > mapPointY
                    || targetEnv.getHigh(1) < mapPointY) {
                // Center of tile level outside target grid, use something on
                // target grid for calculations
                mapPointX = targetEnv.getLow(0) + targetEnv.getSpan(0) * 0.5;
                mapPointY = targetEnv.getLow(1)
                        + targetGeometry.getGridRange().getSpan(1) * 0.75;
            }

            double[] input = new double[] { mapPointX, mapPointY,
                    mapPointX + 1, mapPointY + 1 };
            double[] output = new double[input.length];

            tileCRSToTargetGrid.inverse().transform(input, 0, output, 0, 2);
            crsToGrid.transform(output, 0, input, 0, 2);
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
            // Create tile border based on pixel density tile level 0 should
            // always have threshold of 1.0
            double pixelDensity = this.pixelDensity;
            if (tileLevel == 0 || pixelDensity < 1.0) {
                pixelDensity = 1.0;
            }
            // Convert grid pixel density into CRS pixel density
            pixelDensity *= (targetGeometry.getEnvelope().getSpan(0) / targetGeometry
                    .getGridRange().getSpan(0));
            // Compute border and convert into target grid space from crs
            border = JTS.transform(EnvelopeIntersection
                    .createEnvelopeIntersection(tileGridGeom.getEnvelope(),
                            targetGeometry.getEnvelope(), pixelDensity,
                            Math.max(range.getSpan(0) / 4, 1),
                            Math.max(range.getSpan(1) / 4, 1)), targetGeometry
                    .getGridToCRS(PixelInCell.CELL_CENTER).inverse());
        } catch (TransformException e) {
            // Invalid geometry, don't add a border
        } catch (FactoryException e) {
            // Invalid transforms, don't add border
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

}
