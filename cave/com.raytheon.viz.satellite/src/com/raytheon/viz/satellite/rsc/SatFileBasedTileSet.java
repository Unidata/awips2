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
package com.raytheon.viz.satellite.rsc;

import java.awt.Point;
import java.awt.Rectangle;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.collections.keyvalue.MultiKey;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.units.generic.GenericPixel;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;
import com.raytheon.viz.core.rsc.hdf5.CreateTileJob;
import com.raytheon.viz.core.rsc.hdf5.FileBasedTileSet;
import com.raytheon.viz.satellite.data.prep.SatDataRetriever;

public class SatFileBasedTileSet extends FileBasedTileSet {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatFileBasedTileSet.class);

    private static class CreateTileGroupJob extends CreateTileJob {

        private Rectangle rectangle;

        private List<MultiKey> keys;

        public CreateTileGroupJob(String name, Rectangle rectangle,
                List<MultiKey> keys) {
            super(name);
            this.rectangle = rectangle;
            this.keys = keys;
        }

        @Override
        public void run() {
            canceled = false;
            ((SatFileBasedTileSet) tileSet).createTileGroup(target, level,
                    rectangle, keys);

            canceled = true;
        }

    }

    protected PluginDataObject pdo;

    public SatFileBasedTileSet(PluginDataObject pdo, String dataset,
            AbstractTileSet sharedGeometryTileset) throws VizException {
        super(HDF5Util.findHDF5Location(pdo).getPath(), pdo
                .getDataURI(), dataset, sharedGeometryTileset);
        this.pdo = pdo;
    }

    public SatFileBasedTileSet(PluginDataObject pdo, String dataset,
            int levels, int tileSize, GridGeometry2D gridGeometry,
            AbstractVizResource<?, ?> rsc, String viewType) throws VizException {
        super(HDF5Util.findHDF5Location(pdo).getPath(), pdo
                .getDataURI(), dataset, levels, tileSize, gridGeometry, rsc,
                viewType);
        this.pdo = pdo;
    }

    public SatFileBasedTileSet(PluginDataObject pdo, String dataset,
            int levels, int tileSize, GridGeometry2D gridGeometry,
            AbstractVizResource<?, ?> rsc, PixelInCell pixelOrientation,
            String viewType) throws VizException {
        super(HDF5Util.findHDF5Location(pdo).getPath(), pdo
                .getDataURI(), dataset, levels, tileSize, gridGeometry, rsc,
                pixelOrientation, viewType);
        this.pdo = pdo;
    }

    protected void createTileGroup(IGraphicsTarget target, Integer level,
            Rectangle rectangle, List<MultiKey> keys) {
        ColorMapParameters cmp = rsc.getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        boolean signed = false;
        if (cmp != null && cmp.getDataUnit() instanceof GenericPixel) {
            signed = true;
        }
        SatDataRetriever retriever = new SatDataRetriever(pdo, level,
                rectangle, signed, null);
        byte[] rawData = retriever.getRawData();
        if (rawData == null) {
            for (MultiKey key : keys) {
                remove(key);
            }
            return;
        }
        ByteBuffer fullData = ByteBuffer.wrap(rawData);
        for (MultiKey key : keys) {
            int i = (Integer) key.getKey(1);
            int j = (Integer) key.getKey(2);
            ImageTile tile = this.tileSet.getTile(level, i, j);
            if (tile != null) {
                Rectangle tileRect = tile.getRectangle();
                ByteBuffer tileData = ByteBuffer.allocate(tileRect.height
                        * tileRect.width);

                byte[] row = new byte[tileRect.width];

                int srcIndex = (tileRect.y - rectangle.y) * rectangle.width
                        + tileRect.x - rectangle.x;

                for (int c = 0; c < tileRect.getHeight(); c++) {
                    fullData.position(srcIndex);
                    fullData.get(row);
                    tileData.put(row);
                    srcIndex += rectangle.width;
                }

                try {
                    IImage raster = target.getExtension(
                            IColormappedImageExtension.class).initializeRaster(
                            new SatDataRetriever(pdo, level, tileRect, signed,
                                    tileData), cmp);
                    if (raster != null) {
                        raster.stage();
                        addImage(raster, level, i, j);
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error getting extension for colormapped images: "
                                    + e.getLocalizedMessage(), e);
                }

            }
            remove(key);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.tiling.AbstractTileSet#createTile(com.raytheon
     * .viz.core.IGraphicsTarget, int, java.awt.Rectangle)
     */
    @Override
    protected IImage createTile(IGraphicsTarget target, int level, int i, int j)
            throws VizException {
        ColorMapParameters cmp = rsc.getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        boolean signed = false;
        if (cmp != null && cmp.getDataUnit() instanceof GenericPixel) {
            signed = true;
        }
        return target
                .getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new SatDataRetriever(pdo, level, this.tileSet.getTile(
                                level, i, j).getRectangle(), signed, null), cmp);
    }

    @Override
    protected void startCreateTileJobs(int lvl, IGraphicsTarget target,
            List<Point> tilesToCreate) {
        // We are going to request as much data as possible in one request by
        // joining adjacent requests.
        List<Rectangle> rectangles = new ArrayList<Rectangle>();
        for (Point p : tilesToCreate) {
            rectangles.add(tileSet.getTile(lvl, p.x, p.y).getRectangle());
        }
        // Join together any adjacent rectangles
        for (int i = 0; i < rectangles.size(); i++) {
            Rectangle r1 = rectangles.get(i);
            for (int j = i + 1; j < rectangles.size(); j++) {
                Rectangle r2 = rectangles.get(j);
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
                    rectangles.remove(r1);
                    rectangles.remove(r2);
                    rectangles.add(r1.union(r2));
                    // start all over.
                    i = -1;
                    break;
                }
            }
        }
        // start a separate job for every big rectangle
        for (Rectangle bigRectangle : rectangles) {
            List<MultiKey> keys = new ArrayList<MultiKey>();
            for (Point p : tilesToCreate) {
                int i = p.x;
                int j = p.y;
                Rectangle tileRectangle = tileSet.getTileSet().get(lvl)[i][j]
                        .getRectangle();
                MultiKey key = new MultiKey(lvl, i, j);
                if (bigRectangle.equals(tileRectangle)) {
                    // This tile is my big rectangle, schedule single tile
                    // request
                    if (jobMap.get(key) == null) {
                        CreateTileJob job = new CreateTileJob("tileset");
                        job.setI(i);
                        job.setJ(j);
                        job.setLevel(lvl);
                        job.setTarget(target);
                        job.setTileSet(this);
                        job.setSystem(true);
                        jobMap.put(key, job);
                        tileCreationPool.schedule(job);
                        break;
                    }
                } else if (bigRectangle.contains(tileRectangle)) {
                    if (jobMap.get(key) == null) {
                        keys.add(key);
                    }
                }
            }

            // For all keys in the big rectangle, create a request job
            if (!keys.isEmpty()) {
                CreateTileGroupJob job = new CreateTileGroupJob(
                        "GroupTileRequest", bigRectangle, keys);
                job.setLevel(lvl);
                job.setTarget(target);
                job.setTileSet(this);
                job.setSystem(true);
                for (MultiKey key : keys) {
                    jobMap.put(key, job);
                }
                tileCreationPool.schedule(job);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.tiling.AbstractTileSet#preloadDataObject(int)
     */
    @Override
    protected void preloadDataObject(int level) throws StorageException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.tiling.AbstractTileSet#hasDataPreloaded(int)
     */
    @Override
    public boolean hasDataPreloaded(int level) {
        return true;
    }

    @Override
    public void cancelRequest(int level, int i, int j) {

    }

    public synchronized List<DrawableImage> getImages(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        return do2D(target, paintProps);
    }
}
