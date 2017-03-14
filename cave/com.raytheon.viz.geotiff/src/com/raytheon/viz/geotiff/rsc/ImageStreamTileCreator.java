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
package com.raytheon.viz.geotiff.rsc;

import java.awt.image.RenderedImage;
import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageReader;
import javax.imageio.stream.FileImageInputStream;
import javax.imageio.stream.ImageInputStream;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.tile.Tile;
import com.raytheon.uf.viz.core.tile.TileSetRenderable.TileImageCreator;

/**
 * Tile image creator that gets tiles from a FileImageInputStream. Must be
 * closed after use.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2014 3522       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ImageStreamTileCreator implements TileImageCreator, Closeable {

    private final ImageInputStream imgIn;

    private final ImageReader imgReader;

    /**
     * @param in
     * @throws IOException
     * @see {@link #close()}
     */
    public ImageStreamTileCreator(FileImageInputStream in) throws IOException {
        this.imgIn = in;
        Iterator<ImageReader> readers = ImageIO.getImageReaders(imgIn);
        if (readers.hasNext()) {
            this.imgReader = readers.next();
            imgReader.setInput(imgIn);
        } else {
            imgIn.close();
            throw new IOException("No image readers found for stream");
        }
    }

    /**
     * @param imageFile
     * @throws FileNotFoundException
     * @throws IOException
     * @see {@link #close()}
     */
    public ImageStreamTileCreator(File imageFile) throws FileNotFoundException,
            IOException {
        this(new FileImageInputStream(imageFile));
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.core.tile.TileSetRenderable.TileImageCreator#createTileImage(com.raytheon.uf.viz.core.IGraphicsTarget, com.raytheon.uf.viz.core.tile.Tile, org.geotools.coverage.grid.GeneralGridGeometry)
     */
    @Override
    public DrawableImage createTileImage(IGraphicsTarget target, Tile tile,
            GeneralGridGeometry targetGeometry) throws VizException {
        if (tile.tileLevel != 0) {
            throw new VizException(getClass().getSimpleName()
                    + " only supports single level tiled data");
        }
        final GridEnvelope2D env = tile.tileGeometry.getGridRange2D();
        final ImageReadParam param = imgReader.getDefaultReadParam();
        param.setSourceRegion(env);
        IImage img = target.initializeRaster(new IRenderedImageCallback() {

            @Override
            public RenderedImage getImage() throws VizException {
                try {
                    synchronized (imgReader) {
                        return imgReader.read(0, param);
                    }
                } catch (IOException e) {
                    throw new VizException(
                            "Unable to read tile from image stream for bounds: "
                                    + env, e);
                }
            }
        });

        IMesh mesh = target.getExtension(IMapMeshExtension.class)
                .constructMesh(tile.tileGeometry, targetGeometry);
        return new DrawableImage(img, new PixelCoverage(mesh));
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.io.Closeable#close()
     */
    @Override
    public void close() throws IOException {
        imgIn.close();
        imgReader.dispose();
    }
    
}
