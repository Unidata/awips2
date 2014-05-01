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

import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.BufferSlicer;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.prep.HDF5DataRetriever;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage.Status;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * {@link TileSetRenderable} for 2D {@link PluginDataObject}s. Groups adjacent
 * tiles when retrieving data. Default retrieval mechanism uses
 * {@link HDF5DataRetriever}. If other mechanism desired, extend class and
 * override {@link #retrieveRecordData(Tile)}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 19, 2013  2122     mschenke    Initial creation.
 * Oct 16, 2013  2333     mschenke    Added method for auto-unit conversion
 *                                    interrogating
 * Nov 20, 2013  2492     bsteffen    Move unit converting interrogate into
 *                                    TileSetRenderable.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RecordTileSetRenderable extends TileSetRenderable {

    private class RecordTileDataCallback implements
            IColorMapDataRetrievalCallback {

        private final Tile tile;

        private ColorMapData data;

        private RecordTileDataCallback(Tile tile) {
            this.tile = tile;
        }

        private void setRetrievedData(ColorMapData data) {
            this.data = data;
        }

        @Override
        public ColorMapData getColorMapData() throws VizException {
            ColorMapData rval = data;
            if (rval != null) {
                // Once retrieved, null out
                data = null;
            } else {
                rval = data = retrieveRecordData(tile);
            }
            return rval;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((tile == null) ? 0 : tile.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            RecordTileDataCallback other = (RecordTileDataCallback) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (tile == null) {
                if (other.tile != null)
                    return false;
            } else if (!tile.equals(other.tile))
                return false;
            return true;
        }

        private RecordTileSetRenderable getOuterType() {
            // Only compares outer type class to ensure calls to
            // retrieveRecordData will be the same
            return RecordTileSetRenderable.this;
        }

    }

    private class RecordTileImageCreatorTask implements Runnable {

        private final IGraphicsTarget target;

        private final Tile bigTile;

        private final List<Tile> subTiles;

        public RecordTileImageCreatorTask(IGraphicsTarget target, Tile bigTile,
                List<Tile> subTiles) {
            this.target = target;
            this.bigTile = bigTile;
            this.subTiles = subTiles;
        }

        @Override
        public void run() {
            int numTiles = subTiles.size();
            int numNeedStaging = 0;

            List<RecordTileDataCallback> callbacks = new ArrayList<RecordTileDataCallback>(
                    numTiles);
            List<DrawableImage> images = new ArrayList<DrawableImage>(numTiles);

            for (Tile tile : subTiles) {
                RecordTileDataCallback callback = new RecordTileDataCallback(
                        tile);
                callbacks.add(callback);
                DrawableImage image = null;
                try {
                    image = createTileImage(target, tile, callback);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
                images.add(image);

                if (image != null
                        && image.getImage().getStatus() == Status.UNLOADED) {
                    numNeedStaging += 1;
                }
            }

            if (numNeedStaging == numTiles) {
                // All the images need staging, do bulk request
                ColorMapData data = retrieveRecordData(bigTile);

                if (data != null) {
                    Rectangle bigTileRect = bigTile.getRectangle();
                    for (int i = 0; i < numTiles; i += 1) {
                        Tile tile = subTiles.get(i);
                        DrawableImage image = images.get(i);
                        if (image != null) {
                            if (image.getImage().getStatus() == Status.UNLOADED) {
                                Rectangle tileRect = tile.getRectangle();
                                ColorMapData subData = new ColorMapData(
                                        BufferSlicer.slice(data.getBuffer(),
                                                tileRect, bigTileRect),
                                        new int[] { tileRect.width,
                                                tileRect.height },
                                        data.getDataType(), data.getDataUnit());

                                callbacks.get(i).setRetrievedData(subData);
                                try {
                                    image.getImage().stage();
                                } catch (VizException e) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            e.getLocalizedMessage(), e);
                                }
                            }
                        }
                    }
                }
            }

            for (int i = 0; i < numTiles; i += 1) {
                Tile tile = subTiles.get(i);
                DrawableImage image = images.get(i);
                addTileImage(tile, image);
            }
            issueRefresh(target);
        }

        public DrawableImage createTileImage(IGraphicsTarget target, Tile tile,
                RecordTileDataCallback callback) throws VizException {
            IColormappedImage image = target.getExtension(
                    IColormappedImageExtension.class).initializeRaster(
                    callback, colormapping.getColorMapParameters());
            IMesh mesh = target.getExtension(IMapMeshExtension.class)
                    .constructMesh(tile.tileGeometry,
                            tileSet.getTargetGeometry());
            return new DrawableImage(image, new PixelCoverage(mesh),
                    RasterMode.SYNCHRONOUS);
        }

    }

    protected final ColorMapCapability colormapping;

    protected final PluginDataObject record;

    public RecordTileSetRenderable(AbstractVizResource<?, ?> resource,
            PluginDataObject record, ISpatialObject spatialObject,
            int tileLevels) {
        this(resource, record, MapUtil.getGridGeometry(spatialObject),
                tileLevels);
    }

    public RecordTileSetRenderable(AbstractVizResource<?, ?> resource,
            PluginDataObject record, GridGeometry2D tileSetGeometry,
            int tileLevels) {
        this(resource, record, tileSetGeometry, tileLevels, 512);
    }

    public RecordTileSetRenderable(AbstractVizResource<?, ?> resource,
            PluginDataObject record, GridGeometry2D tileSetGeometry,
            int tileLevels, int tileSize) {
        super(resource.getCapability(ImagingCapability.class), tileSetGeometry,
                null, tileLevels, tileSize);
        this.record = record;
        this.colormapping = resource.getCapability(ColorMapCapability.class);
    }

    @Override
    protected void createTileImages(IGraphicsTarget target,
            Collection<Tile> tilesToCreate) {
        Map<Integer, List<Tile>> mapped = new HashMap<Integer, List<Tile>>();
        for (Tile tile : tilesToCreate) {
            // Ensure no job already scheduled for tile
            if (jobMap.get(tile) == null) {
                List<Tile> tiles = mapped.get(tile.tileLevel);
                if (tiles == null) {
                    tiles = new ArrayList<Tile>();
                    mapped.put(tile.tileLevel, tiles);
                }
                tiles.add(tile);
            }
        }

        for (Integer tileLevel : mapped.keySet()) {
            List<Tile> tiles = mapped.get(tileLevel);
            List<GridEnvelope2D> rectangles = new ArrayList<GridEnvelope2D>();
            List<Envelope2D> envelopes = new ArrayList<Envelope2D>();

            for (Tile tile : tiles) {
                rectangles.add(tile.tileGeometry.getGridRange2D());
                envelopes.add(tile.tileGeometry.getEnvelope2D());
            }

            // Join together any adjacent rectangles
            for (int i = 0; i < rectangles.size(); i++) {
                Rectangle r1 = rectangles.get(i);
                Envelope2D e1 = envelopes.get(i);
                for (int j = i + 1; j < rectangles.size(); j++) {
                    Rectangle r2 = rectangles.get(j);
                    Envelope2D e2 = envelopes.get(j);
                    boolean joinable = true;
                    if (r1.x == r2.x && r1.width == r2.width) {
                        if (r1.getMaxY() == r2.getMinY()
                                || r1.getMinY() == r2.getMaxY()) {
                            joinable = true;
                        }
                    } else if (r1.y == r2.y && r1.height == r2.height) {
                        if (r1.getMaxX() == r2.getMinX()
                                || r1.getMinX() == r2.getMaxX()) {
                            joinable = true;
                        }
                    }
                    if (joinable) {
                        // Join the rectangles
                        rectangles.remove(r1);
                        rectangles.remove(r2);
                        Rectangle2D joined = new GridEnvelope2D();
                        Rectangle2D.union(r1, r2, joined);
                        rectangles.add((GridEnvelope2D) joined);

                        // Join the envelopes
                        envelopes.remove(e1);
                        envelopes.remove(e2);
                        joined = new Envelope2D();
                        Rectangle2D.union(e1, e2, joined);
                        envelopes.add((Envelope2D) joined);

                        // start all over.
                        i = -1;
                        break;
                    }
                }
            }

            // start a separate job for every big rectangle
            for (int i = 0; i < rectangles.size(); i++) {
                Tile joinedTile = new Tile(tileLevel, new GridGeometry2D(
                        (GridEnvelope) rectangles.get(i), envelopes.get(i)));
                RecordTileImageCreatorTask task = new RecordTileImageCreatorTask(
                        target, joinedTile, tiles);
                for (Tile tile : tiles) {
                    jobMap.put(tile, task);
                }
                tileCreationPool.schedule(task);
            }
        }
    }

    /**
     * @param tile
     * @return
     */
    protected ColorMapData retrieveRecordData(Tile tile) {
        return new HDF5DataRetriever(HDF5Util.findHDF5Location(record),
                DataStoreFactory.createDataSetName(record.getDataURI(),
                        DataStoreFactory.DEF_DATASET_NAME, tile.tileLevel),
                tile.getRectangle()).getColorMapData();
    }

    @Override
    public double interrogate(Coordinate coordinate) throws VizException {
        ColorMapParameters parameters = colormapping.getColorMapParameters();
        return interrogate(coordinate, parameters.getNoDataValue());
    }

    @Override
    public double interrogate(Coordinate coordinate, Unit<?> resultUnit)
            throws VizException {
        ColorMapParameters parameters = colormapping.getColorMapParameters();
        return super.interrogate(coordinate, resultUnit,
                parameters.getNoDataValue());
    }

    /**
     * @param target
     */
    protected void issueRefresh(IGraphicsTarget target) {
        target.setNeedsRefresh(true);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((record == null) ? 0 : record.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        RecordTileSetRenderable other = (RecordTileSetRenderable) obj;
        if (record == null) {
            if (other.record != null)
                return false;
        } else if (!record.equals(other.record))
            return false;
        return true;
    }

}
