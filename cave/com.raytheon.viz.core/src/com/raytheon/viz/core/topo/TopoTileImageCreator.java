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
package com.raytheon.viz.core.topo;

import java.io.File;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.prep.HDF5DataRetriever;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.tile.Tile;
import com.raytheon.uf.viz.core.tile.TileSetRenderable.TileImageCreator;

/**
 * Image creator for topo tiles, uses hdf5 data file for retrieval
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TopoTileImageCreator implements TileImageCreator {

    protected TopoResource resource;

    private File dataFile;

    public TopoTileImageCreator(TopoResource resource, File dataFile) {
        this.resource = resource;
        this.dataFile = dataFile;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.tile.TileSetRenderable.TileImageCreator#
     * createTileImage(com.raytheon.uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.tile.Tile,
     * org.geotools.coverage.grid.GeneralGridGeometry)
     */
    @Override
    public DrawableImage createTileImage(IGraphicsTarget target, Tile tile,
            GeneralGridGeometry targetGeometry) throws VizException {
        IColormappedImage image = target.getExtension(
                IColormappedImageExtension.class).initializeRaster(
                createColormapImageCallback(tile),
                resource.getCapability(ColorMapCapability.class)
                        .getColorMapParameters());
        IMesh mesh = target.getExtension(IMapMeshExtension.class)
                .constructMesh(tile.tileGeometry, targetGeometry);
        return new DrawableImage(image, new PixelCoverage(mesh),
                RasterMode.ASYNCHRONOUS);
    }

    protected IColorMapDataRetrievalCallback createColormapImageCallback(
            Tile tile) {
        int level = tile.tileLevel;
        String dataset = "/full";
        if (level > 0) {
            dataset = "/interpolated/" + level;
        }
        return new HDF5DataRetriever(dataFile, dataset, tile.getRectangle());
    }
}
