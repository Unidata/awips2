package gov.noaa.nws.ncep.viz.rsc.mosaic.rsc;

import gov.noaa.nws.ncep.edex.plugin.mosaic.uengine.MosaicTiler;

import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;

/**
 * Defines a tileset for radar mosaic data
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  01/2001		 204		 M. Li       Initial Creation.
 *  09/2014                  sgilbert    Clean up unused method
 * 
 * </pre>
 * 
 * @author mli
 * @version 1
 */
public class RadarTileSet extends AbstractTileSet {
    protected Object loadedData;

    protected int[] dims;

    private MosaicTiler tiler;

    public RadarTileSet(MosaicTiler tiler,
            AbstractTileSet sharedGeometryTileset,
            AbstractVizResource<?, ?> rsc, String viewType) throws VizException {

        super(sharedGeometryTileset, rsc);

        this.tiler = tiler;
    }

    public RadarTileSet(MosaicTiler tiler, AbstractVizResource<?, ?> rsc,
            String viewType) throws VizException {

        super(tiler.getLevels(), tiler.getTileSize(), tiler
                .constructGridGeometry(), rsc, PixelInCell.CELL_CORNER,
                viewType);

        this.tiler = tiler;
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
     * com.raytheon.viz.core.rsc.tiling.AbstractTileSet#createTile(com.raytheon
     * .viz.core.IGraphicsTarget, int, int, int)
     */
    @Override
    protected IImage createTile(IGraphicsTarget target, int level, int i, int j)
            throws VizException {
        return RadarLoaderJob.getInstance().requestLoad(
                i,
                j,
                level,
                tileSize,
                target,
                tiler,
                this.rsc.getCapability(ColorMapCapability.class)
                        .getColorMapParameters(), dims);
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

    public CoordinateReferenceSystem getCRS() {
        return this.originalGridGeometry.getCoordinateReferenceSystem();
    }

    @Override
    public void cancelRequest(int level, int i, int j) {
        // TODO Auto-generated method stub

    }

}
