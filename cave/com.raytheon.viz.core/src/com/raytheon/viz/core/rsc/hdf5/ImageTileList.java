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
package com.raytheon.viz.core.rsc.hdf5;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;

/**
 * 
 * Tilesets can be shared and use a reference count to determine when to
 * dispose. When the constructor is called the tileset has a refcount of one,
 * anyone sharing it should call the use() method to increment the refcount.
 * When a tileset is done it should call dispose to stop decrement the refcount
 * and dispose if it is no longer in use.
 * 
 * Construct new ImageTileList objects using the TileListFactory
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ImageTileList implements Iterable<ImageTile[][]> {

    private List<ImageTile[][]> tileSet;

    public ImageTileList() {
        this.tileSet = new ArrayList<ImageTile[][]>();
    }

    /**
     * Dispose of the tile list
     * 
     */
    public void dispose() {
        for (ImageTile[][] tileGrid : tileSet) {
            for (ImageTile[] tileArr : tileGrid) {
                for (ImageTile tile : tileArr) {
                    if (tile != null) {
                        tile.dispose();
                    }
                }
            }
        }
        tileSet.clear();
    }

    public List<ImageTile[][]> getTileSet() {
        return tileSet;
    }

    public ImageTile[][] getTileGrid(int level) {
        if (tileSet.size() <= level) {
            return null;
        }
        return tileSet.get(level);
    }

    public ImageTile getTile(int level, int i, int j) {
        ImageTile[][] grid = getTileGrid(level);
        if (grid == null || grid.length <= i) {
            return null;
        }
        ImageTile[] row = grid[i];
        if (row == null || row.length <= j) {
            return null;
        }
        return row[j];
    }

    @Override
    public Iterator<ImageTile[][]> iterator() {
        return tileSet.iterator();
    }

}