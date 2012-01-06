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

import java.awt.image.RenderedImage;
import java.awt.image.renderable.ParameterBlock;
import java.util.LinkedList;
import java.util.List;

import javax.media.jai.JAI;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/* 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2009            mschenke     Initial creation
 *
 * </pre>
 *
 * @author mschenke
 */
/**
 * This class is used to tile images in memory, ie large geotiffs off the fiels
 * system
 * 
 * @version 1.0
 */
public class PureMemoryBasedTileSet extends AbstractTileSet {

    /**
     * The full resolution image
     */
    private RenderedImage image;

    /**
     * Tile size for images
     */
    private static final int TILE_SIZE = 256;

    /**
     * List of scaled images, indexed by
     */
    private List<RenderedImage> imageLevels;

    /**
     * The name of the image
     */
    private String name;

    /**
     * Creates a new memory based tile set
     * 
     * @param name
     *            name of set
     * @param rsc
     *            resource set is for
     * @param geom
     *            the geometry of the set
     * @param viewType
     *            2D or 3D
     * @throws VizException
     */
    public PureMemoryBasedTileSet(String name, AbstractVizResource<?, ?> rsc,
            GridGeometry2D geom, int levels, String viewType)
            throws VizException {
        super(levels, TILE_SIZE, geom, rsc, viewType);
        this.name = name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.tiling.AbstractTileSet#hasDataPreloaded(int)
     */
    @Override
    public boolean hasDataPreloaded(int level) {
        boolean rval = (imageLevels.get(level) != null);
        return rval;
    }

    @Override
    protected IImage createTile(IGraphicsTarget target, int level, int i, int j)
            throws VizException {
        IImage img = null;
        RenderedImage scaledImage = (RenderedImage) imageLevels.get(level);
        if (scaledImage == null) {
            try {
                preloadDataObject(level);
            } catch (StorageException e) {
            }
            scaledImage = (RenderedImage) imageLevels.get(level);
        }
        RenderedImage croppedImage = cropImage(i, j, scaledImage);
        // the 0 is number of pixels overlapping between tiles
        img = target.initializeRaster(new IODataPreparer(croppedImage, name
                + "_" + i + "_" + j, 0), null);

        return img;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.tiling.AbstractTileSet#preloadDataObject(int)
     */
    @Override
    protected void preloadDataObject(int level) throws StorageException {
        RenderedImage image = scaleImage(level);
        imageLevels.add(level, image);
    }

    /**
     * Scales the images for zooming
     */
    private void scaleImages() {
        imageLevels = new LinkedList<RenderedImage>();
        imageLevels.add(0, image);
        for (int i = 1; i < levels; ++i) {
            imageLevels.add(i, null);
        }
    }

    /**
     * Scales an image using the formula scale is equal to 1/2^level
     * 
     * @param level
     * @return
     */
    private RenderedImage scaleImage(int level) {
        ParameterBlock pb = new ParameterBlock();
        pb.addSource(image);
        float scale = (float) (1 / Math.pow(2, (level)));
        pb.add(scale);
        pb.add(scale);
        RenderedImage rval = JAI.create("scale", pb);
        pb = null;
        return rval;
    }

    /**
     * Crops an image into a tile
     * 
     * @param i
     *            row to crop
     * @param j
     *            col to crop
     * @param image
     *            image to crop from
     * @return
     */
    private RenderedImage cropImage(int i, int j, RenderedImage image) {
        ParameterBlock pb = new ParameterBlock();
        pb.addSource(image);
        pb.add((float) (i * TILE_SIZE));
        pb.add((float) (j * TILE_SIZE));

        if (TILE_SIZE * (i + 1) <= image.getWidth()) {
            pb.add(new Float(TILE_SIZE));
        } else {
            pb.add(new Float(TILE_SIZE
                    - ((TILE_SIZE * (i + 1)) - image.getWidth())));
        }

        if (TILE_SIZE * (j + 1) <= image.getHeight()) {
            pb.add(new Float(TILE_SIZE));
        } else {
            pb.add(new Float(TILE_SIZE
                    - ((TILE_SIZE * (j + 1)) - image.getHeight())));
        }

        RenderedImage rval = JAI.create("crop", pb);
        pb = null;
        return rval;
    }

    public RenderedImage getImage() {
        return image;
    }

    /**
     * Set the image to be tiled
     * 
     * @param image
     */
    public void setImage(RenderedImage image) {
        this.image = image;
        scaleImages();
    }

    /*
     * Calculates the number of levels needed
     */
    public static int calculateLevels(RenderedImage image) {
        int width = image.getWidth();
        int height = image.getHeight();

        int numLevels = 1;
        while (width >= 2048 || height >= 2048) {
            width /= 2;
            height /= 2;
            numLevels++;
        }
        return numLevels;
    }

    @Override
    public void dispose() {
        super.dispose();
        image = null;
        imageLevels.clear();
        System.gc();
    }

    @Override
    public void cancelRequest(int level, int i, int j) {
        // TODO Auto-generated method stub

    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }
}