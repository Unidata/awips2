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

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.collections.keyvalue.MultiKey;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.IDataPreparer;
import com.raytheon.uf.viz.core.data.prep.HDF5DataRetriever;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;

/**
 * 
 * Implements a file based tileset.
 * 
 * A file based tileset pulls the tile directly out of an hdf5 file directly on
 * demand. This assumes that the hdf5 file contains a renderable raster.
 * 
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Feb 15, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */

public class FileBasedTileSet extends AbstractTileSet {

    protected String hdf5File;

    protected String group;

    protected String dataset;

    protected Map<MultiKey, IDataPreparer> prepMap = new HashMap<MultiKey, IDataPreparer>();

    public FileBasedTileSet(String hdf5File, String group, String dataset,
            AbstractTileSet sharedGeometryTileset) throws VizException {
        super(sharedGeometryTileset);
        this.hdf5File = hdf5File;
        this.group = group;
        this.dataset = dataset;
    }

    public FileBasedTileSet(String hdf5File, String group, String dataset,
            int levels, int tileSize, GridGeometry2D gridGeometry,
            AbstractVizResource<?, ?> rsc, String viewType) throws VizException {
        super(levels, tileSize, gridGeometry, rsc, viewType);
        this.hdf5File = hdf5File;
        this.group = group;
        this.dataset = dataset;
    }

    public FileBasedTileSet(String hdf5File, String group, String dataset,
            int levels, int tileSize, GridGeometry2D gridGeometry,
            AbstractVizResource<?, ?> rsc, PixelInCell pixelOrientation,
            String viewType) throws VizException {
        super(levels, tileSize, gridGeometry, rsc, pixelOrientation, viewType);
        this.hdf5File = hdf5File;
        this.group = group;
        this.dataset = dataset;
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
        IImage raster = target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new HDF5DataRetriever(new File(this.hdf5File), "/"
                                + this.group + "/" + this.dataset
                                + "-interpolated/" + level, this.tileSet
                                .getTile(level, i, j).getRectangle()),
                        rsc.getCapability(ColorMapCapability.class)
                                .getColorMapParameters());
        return raster;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.tiling.AbstractTileSet#preloadDataObject(int)
     */
    @Override
    protected void preloadDataObject(int level) throws StorageException {
        // No preloading -- Load during createTile

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
}
