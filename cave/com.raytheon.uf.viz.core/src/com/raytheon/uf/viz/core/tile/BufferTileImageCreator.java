package com.raytheon.uf.viz.core.tile;

import java.awt.Rectangle;
import java.nio.Buffer;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.BufferSlicer;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.tile.TileSetRenderable.TileImageCreator;

/**
 * {@link TileImageCreator} for {@link TileSetRenderable} that creates image
 * tiles for Buffers. Only supports single level tiling
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class BufferTileImageCreator implements TileImageCreator {

    private class BufferColorMapRetrievalCallback implements
            IColorMapDataRetrievalCallback {

        private Rectangle slice;

        private BufferColorMapRetrievalCallback(Rectangle slice) {
            this.slice = slice;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback#
         * getColorMapData()
         */
        @Override
        public ColorMapData getColorMapData() throws VizException {
            return new ColorMapData(BufferSlicer.slice(buffer, slice,
                    bufferBounds), new int[] { slice.width, slice.height });
        }

    }

    private Buffer buffer;

    private Rectangle bufferBounds;

    private ColorMapCapability cmapCapability;

    public BufferTileImageCreator(Buffer buffer, Rectangle bufferBounds,
            ColorMapCapability cmapCapability) {
        this.buffer = buffer;
        this.bufferBounds = bufferBounds;
        this.cmapCapability = cmapCapability;
    }

    @Override
    public DrawableImage createTileImage(IGraphicsTarget target, Tile tile,
            GeneralGridGeometry targetGeometry) throws VizException {
        if (tile.tileLevel != 0) {
            throw new VizException(getClass().getSimpleName()
                    + " only supports single level tiled data");
        }

        IImage image = target
                .getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new BufferColorMapRetrievalCallback(tile.getRectangle()),
                        cmapCapability.getColorMapParameters());
        IMesh mesh = target.getExtension(IMapMeshExtension.class)
                .constructMesh(tile.tileGeometry, targetGeometry);
        return new DrawableImage(image, new PixelCoverage(mesh),
                RasterMode.ASYNCHRONOUS);
    }
}
