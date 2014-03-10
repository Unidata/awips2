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
package com.raytheon.uf.common.topo;

import java.awt.Point;
import java.io.FileNotFoundException;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Map;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.numeric.source.AbstractTiledDataSource;
import com.raytheon.uf.common.numeric.source.DataSource;

/**
 * Tiled data source for loading topo tiles and caching tiles as soft reference
 * objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 11, 2012           bsteffen    Initial creation
 * Aug 06, 2013  2235     bsteffen    Added Caching version of TopoQuery.
 * Mar 07, 2014  2791     bsteffen    Move Data Source/Destination to numeric
 *                                    plugin.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class TiledTopoSource extends AbstractTiledDataSource {

    private final GridGeometry2D gridGeometry;

    private final IDataStore dataStore;

    private final String dataset;

    private Map<Point, Reference<DataSource>> tiles = new HashMap<Point, Reference<DataSource>>();

    public TiledTopoSource(int tileSize, GridGeometry2D gridGeometry,
            IDataStore dataStore, String dataset) {
        super(tileSize, gridGeometry.getGridRange2D().width, gridGeometry
                .getGridRange2D().height);
        this.gridGeometry = gridGeometry;
        this.dataStore = dataStore;
        this.dataset = dataset;
    }

    public GridGeometry2D getGridGeometry() {
        return gridGeometry;
    }

    public String getDataset() {
        return dataset;
    }

    @Override
    public double getDataValue(int x, int y) {
        double val = super.getDataValue(x, y);
        if (val <= -9999) {
            val = 0;
        }
        return val;

    }

    @Override
    protected DataSource getTile(int startX, int startY, int width, int height) {
        Point key = new Point(startX, startY);
        DataSource tile = null;
        Reference<DataSource> ref = tiles.get(key);
        if (ref != null) {
            tile = ref.get();
        }
        if (tile == null) {
            tile = requestTile(startX, startY, width, height);
            tiles.put(key, new SoftReference<DataSource>(tile));
        }
        return tile;

    }

    protected DataSource requestTile(int startX, int startY, int width,
            int height) {
        try {
            Request req = Request.buildSlab(new int[] { startX, startY },
                    new int[] { startX + width, startY + height });
            IDataRecord record = dataStore.retrieve("/", dataset, req);
            return BufferWrapper.wrapArray(record.getDataObject(), width,
                    height);
        } catch (FileNotFoundException e) {
            throw new DataRetrievalException(e);
        } catch (StorageException e) {
            throw new DataRetrievalException(e);
        }
    }

    public static class DataRetrievalException extends RuntimeException {

        private static final long serialVersionUID = 2292460511295837321L;

        public DataRetrievalException(Throwable cause) {
            super(cause);
        }

        public DataRetrievalException(String message) {
            super(message);
        }

    }

}
