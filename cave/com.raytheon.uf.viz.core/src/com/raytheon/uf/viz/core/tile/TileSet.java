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
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.keyvalue.MultiKey;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;

import com.raytheon.uf.viz.core.IExtent;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * A TileSet is an object that consists of multiple tile levels. Each tile level
 * is represented by a {@link TileLevel} object. These objects are only accessed
 * via static funtion
 * {@link #getTileSet(GridGeometry2D, GeneralGridGeometry, int, int)} to ensure
 * tile set sharing. When a TileSet is no longer needed, dispose must be called
 * on it to make sure it is cleaned up
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

public class TileSet {

    /** Each tile level is LEVEL_FACTOR much smaller than the previous level */
    private static final int LEVEL_FACTOR = 2;

    static final GeometryFactory gf = new GeometryFactory();

    /** Static cache for TileSets */
    private static Map<MultiKey, TileSet> tileSetCache = new HashMap<MultiKey, TileSet>();

    /**
     * Creates a {@link TileSet} object with the specified tile size and levels
     * 
     * @param tileSetGeometry
     * @param targetGeometry
     * @param levels
     * @param tileSize
     * @return
     */
    public static TileSet getTileSet(GridGeometry2D tileSetGeometry,
            GeneralGridGeometry targetGeometry, int levels, int tileSize) {
        synchronized (TileSet.class) {
            MultiKey key = new MultiKey(tileSetGeometry, targetGeometry,
                    levels, tileSize);
            TileSet tileSet = tileSetCache.get(key);
            if (tileSet == null) {
                // No tileset for key, create one
                tileSet = new TileSet(tileSetGeometry, targetGeometry, levels,
                        tileSize, key);
                tileSetCache.put(key, tileSet);
            }
            ++tileSet.refCount;
            return tileSet;
        }
    }

    /** GridGeometry tileSet is created for */
    private GeneralGridGeometry targetGeometry;

    /** {@link TileLevel} array, one per tile level */
    private TileLevel[] tileLevels;

    private int tileSize;

    private int refCount = 0;

    private MultiKey cacheKey;

    private TileSet(GridGeometry2D tileSetGeometry,
            GeneralGridGeometry targetGeometry, int levels, int tileSize,
            MultiKey cacheKey) {
        this.targetGeometry = targetGeometry;
        this.tileSize = tileSize;
        this.cacheKey = cacheKey;
        initialize(tileSetGeometry, levels);
    }

    /**
     * @param tileSetGeometry
     * @param levels
     */
    private void initialize(GridGeometry2D tileSetGeometry, int levels) {
        Envelope envelope = tileSetGeometry.getEnvelope();
        GridEnvelope range = tileSetGeometry.getGridRange();
        int startX = range.getLow(0);
        int startY = range.getLow(1);
        int width = range.getSpan(0);
        int height = range.getSpan(1);

        tileLevels = new TileLevel[levels];
        for (int i = 0; i < levels; ++i) {
            if (i > 0) {
                startX /= LEVEL_FACTOR;
                startY /= LEVEL_FACTOR;
                width /= LEVEL_FACTOR;
                height /= LEVEL_FACTOR;
                tileSetGeometry = new GridGeometry2D(new GeneralGridEnvelope(
                        new int[] { startX, startY },
                        new int[] { width, height }, false), envelope);
            }
            tileLevels[i] = new TileLevel(tileSetGeometry, targetGeometry, i,
                    tileSize);
        }

        // Fully populate lowest resolution tile level
        tileLevels[tileLevels.length - 1].populateTiles();
    }

    /**
     * Disposes of the TileSet, needs to be called when tile set is no longer
     * needed
     */
    public void dispose() {
        synchronized (TileSet.class) {
            --refCount;
            if (refCount == 0) {
                tileSetCache.remove(cacheKey);
            }
        }
    }

    /**
     * Returns the {@link TileLevel} object for the specified level. Lower level
     * = higher resolution
     * 
     * @param level
     * @return
     */
    public TileLevel getTileLevel(int level) {
        return tileLevels[level];
    }

    /**
     * Returns all intersecting tiles in the level for the given extent. Lower
     * tile level = higher resolution with 0 being the highest
     * 
     * @param level
     *            level to get tiles at
     * @param extent
     *            area requesting tiles at in targetGeometry space
     * @return
     */
    public Collection<Tile> getIntersectingTiles(int level, IExtent extent) {
        // Start with level 0 and work way up to requested level
        Geometry extentGeom = gf.createPolygon(
                gf.createLinearRing(new Coordinate[] {
                        new Coordinate(extent.getMinX(), extent.getMinY()),
                        new Coordinate(extent.getMaxX(), extent.getMinY()),
                        new Coordinate(extent.getMaxX(), extent.getMaxY()),
                        new Coordinate(extent.getMinX(), extent.getMaxY()),
                        new Coordinate(extent.getMinX(), extent.getMinY()) }),
                null);
        TileLevel tileLevel = getTileLevel(tileLevels.length - 1);
        return getIntersectingTiles(tileLevel, level, 0, 0,
                tileLevel.getNumXTiles(), tileLevel.getNumYTiles(), extentGeom);
    }

    /**
     * Returns intersecting tiles for the desired level. Recursive function that
     * checks intersection by looking at previous tile level tiles
     * 
     * @param tileLevel
     * @param level
     * @param startX
     * @param startY
     * @param endX
     * @param endY
     * @param extent
     * @return
     */
    private Collection<Tile> getIntersectingTiles(TileLevel tileLevel,
            int level, int startX, int startY, int endX, int endY,
            Geometry extent) {
        TileLevel nextTileLevel = null;
        if (tileLevel.getLevel() != level) {
            // If we are not at desired level, get next tile level
            nextTileLevel = getTileLevel(tileLevel.getLevel() - 1);
        }
        List<Tile> intersecting = new ArrayList<Tile>();
        for (int y = startY; y < endY; ++y) {
            for (int x = startX; x < endX; ++x) {
                Tile tile = tileLevel.getTile(x, y);
                if (tile.intersects(extent)) {
                    if (nextTileLevel != null) {
                        // nextTileLevel is not null meaning we are not at
                        // desired level. Calculate index into next tile level
                        // and call recursively
                        int nextStartX = x * LEVEL_FACTOR;
                        int nextStartY = y * LEVEL_FACTOR;
                        int nextEndX = Math.min(nextStartX + LEVEL_FACTOR,
                                nextTileLevel.getNumXTiles());
                        int nextEndY = Math.min(nextStartY + LEVEL_FACTOR,
                                nextTileLevel.getNumYTiles());
                        intersecting.addAll(getIntersectingTiles(nextTileLevel,
                                level, nextStartX, nextStartY, nextEndX,
                                nextEndY, extent));
                    } else if (tile.tileBorder != null) {
                        // This is desired tile level, add to list if we
                        // actually have a valid border that intersects
                        intersecting.add(tile);
                    }
                }
            }
        }

        return intersecting;
    }

    /**
     * Gets the GridGeometry the tileSet is created for
     * 
     * @return
     */
    public GeneralGridGeometry getTargetGeometry() {
        return targetGeometry;
    }
}
