package com.raytheon.uf.viz.core.tile;

import java.awt.Rectangle;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.numeric.DataUtilities;
import com.raytheon.uf.common.numeric.buffer.ByteBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.IntBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ShortBufferWrapper;
import com.raytheon.uf.common.numeric.dest.DataDestination;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.numeric.source.OffsetDataSource;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.tile.TileSetRenderable.TileImageCreator;

/**
 * 
 * Create imagery from any source of numeric data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 25, 2014  2791     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DataSourceTileImageCreator implements TileImageCreator {

    private class DataSourceColorMapRetrievalCallback implements
            IColorMapDataRetrievalCallback {

        private final Rectangle slice;

        private DataSourceColorMapRetrievalCallback(Rectangle slice) {
            this.slice = slice;
        }

        @Override
        public ColorMapData getColorMapData() throws VizException {
            ColorMapData data = new ColorMapData(dataType, new int[] {
                    slice.width, slice.height });
            Buffer buffer = data.getBuffer();
            DataDestination dest = null;
            if (buffer instanceof ByteBuffer) {
                dest = new ByteBufferWrapper((ByteBuffer) buffer, slice.width,
                        slice.height);
            } else if (buffer instanceof ShortBuffer) {
                dest = new ShortBufferWrapper((ShortBuffer) buffer,
                        slice.width, slice.height);
            } else if (buffer instanceof IntBuffer) {
                dest = new IntBufferWrapper((IntBuffer) buffer, slice.width,
                        slice.height);
            } else if (buffer instanceof FloatBuffer) {
                dest = new FloatBufferWrapper((FloatBuffer) buffer,
                        slice.width, slice.height);
            } else {
                throw new VizException("Unsupported data type: "
                        + dataType.toString());
            }
            DataSource sliceSource = source;
            if (slice.x != 0 || slice.y != 0) {
                sliceSource = new OffsetDataSource(source, slice.x, slice.y);
            }
            DataUtilities.copy(sliceSource, dest, slice.width, slice.height);
            /* Add units */
            return new ColorMapData(buffer, new int[] { slice.width,
                    slice.height }, dataType, unit);
        }

    }

    private final DataSource source;

    private final Unit<?> unit;

    private final ColorMapCapability cmapCapability;

    private final ColorMapDataType dataType;

    public DataSourceTileImageCreator(DataSource source, Unit<?> unit,
            ColorMapDataType dataType, ColorMapCapability cmapCapability) {
        this.source = source;
        this.unit = unit;
        this.dataType = dataType;
        this.cmapCapability = cmapCapability;
    }

    @Override
    public DrawableImage createTileImage(IGraphicsTarget target, Tile tile,
            GeneralGridGeometry targetGeometry) throws VizException {
        if (tile.tileLevel != 0) {
            throw new VizException(getClass().getSimpleName()
                    + " only supports single level tiled data");
        }

        IImage image = target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new DataSourceColorMapRetrievalCallback(
                                tile.getRectangle()),
                        cmapCapability.getColorMapParameters());
        IMesh mesh = target.getExtension(IMapMeshExtension.class)
                .constructMesh(tile.tileGeometry, targetGeometry);
        return new DrawableImage(image, new PixelCoverage(mesh),
                RasterMode.ASYNCHRONOUS);
    }
}
