/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.satellite.rsc;

import java.awt.Rectangle;
import java.util.Map;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;
import com.raytheon.viz.satellite.rsc.SatFileBasedTileSet;

/**
 * 
 * Basically same as SatFileBasedTileSet except that it overrides some methods
 * so that all tiles are created (and maybe displayed) instead of only creating
 * tiles within the given pixel extent.
 * 
 * @author sgilbert
 * 
 */
public class McidasFileBasedTileSet extends SatFileBasedTileSet {
    // private static final transient IUFStatusHandler statusHandler = UFStatus
    // .getHandler(McidasFileBasedTileSet.class);

    public McidasFileBasedTileSet(PluginDataObject pdo, String dataset,
            AbstractTileSet sharedGeometryTileset) throws VizException {
        super(pdo, dataset, sharedGeometryTileset);
    }

    public McidasFileBasedTileSet(PluginDataObject pdo, String dataset,
            int levels, int tileSize, GridGeometry2D gridGeometry,
            AbstractVizResource<?, ?> rsc, PixelInCell pixelOrientation,
            String viewType) throws VizException {
        super(pdo, dataset, levels, tileSize, gridGeometry, rsc,
                pixelOrientation, viewType);
    }

    /*
     * Almost the same as method createTileLevel in AbstractTileSet. This method
     * was copied from class AbstractTileSet. A check was added to create a tile
     * when either the map envelope or tile envelope contains a NaN. In the
     * AbstractTileSet version, the tile would not have been created because the
     * intesects() method fails.
     */
    protected void createTileLevel(int level) throws VizException {

        // Get the h/w of the image we have to work with
        int h = gridGeometry[level].getGridRange().getSpan(1);
        int w = gridGeometry[level].getGridRange().getSpan(0);

        int fullTileDimX = w / tileSize;
        int tileRemainderX = w % tileSize;
        int fullTileDimY = h / tileSize;
        int tileRemainderY = h % tileSize;

        int totalTilesX = fullTileDimX;
        if (tileRemainderX > 0) {
            totalTilesX++;
        }

        int totalTilesY = fullTileDimY;
        if (tileRemainderY > 0) {
            totalTilesY++;
        }

        ImageTile[][] tiles = new ImageTile[totalTilesX][totalTilesY];

        this.tileSet.getTileSet().add(tiles);
        int startX;
        int startY;

        int effectiveWidth = 0;
        int effectiveHeight = 0;

        try {
            ReferencedEnvelope mapEnv = new ReferencedEnvelope(
                    this.mapDescriptor.getGridGeometry().getEnvelope());
            mapEnv = mapEnv.transform(MapUtil.LATLON_PROJECTION, false);
            GeneralEnvelope generalMapEnv = new GeneralEnvelope(mapEnv);
            generalMapEnv.normalize(false);
            startY = 0;
            for (int j = 0; j < totalTilesY; j++) {
                startY = j * (tileSize);
                for (int i = 0; i < totalTilesX; i++) {
                    startX = i * (tileSize);

                    effectiveWidth = tileSize;

                    if ((w - (startX + effectiveWidth) < 0)) {
                        effectiveWidth = (w - startX);
                    }

                    effectiveHeight = tileSize;
                    if ((h - (startY + effectiveHeight) < 0)) {
                        effectiveHeight = (h - startY);
                    }

                    double[] in = new double[2];
                    double[] ul = new double[2];
                    double[] lr = new double[2];

                    in[0] = startX;
                    in[1] = startY;
                    mathTransform[level].transform(in, 0, ul, 0, 1);
                    in[0] = startX + effectiveWidth;
                    in[1] = startY + effectiveHeight;

                    mathTransform[level].transform(in, 0, lr, 0, 1);

                    ReferencedEnvelope env = new ReferencedEnvelope(ul[0],
                            lr[0], ul[1], lr[1],
                            gridGeometry[0].getCoordinateReferenceSystem());
                    GeneralEnvelope generalEnv = new GeneralEnvelope(
                            env.transform(MapUtil.LATLON_PROJECTION, false));
                    // tiles which cross the dateline will almost always be
                    // created since normalizing changes their range to
                    // -180,180. If this is causing problems then instead of
                    // normalizing we should split envelopes that cross the
                    // dateline.
                    generalEnv.normalize(false);
                    // System.out.println("  ENV   =" + generalEnv);
                    // System.out.println("  MAPENV=" + generalMapEnv);
                    // only create the tile if env overlaps the map or if either
                    // envelope contains a NaN
                    if (generalEnv.intersects(generalMapEnv, true)
                            || generalMapEnv.isNull() || generalEnv.isNull()) {

                        tiles[i][j] = new ImageTile();
                        tiles[i][j].setGridGeometry(new Rectangle(startX,
                                startY, effectiveWidth, effectiveHeight), env);
                    }
                }

            }
        } catch (Exception e) {
            throw new VizException("Error creating tile level", e);
        }

    }

    /**
     * @return the originalGridGeometry
     */
    public GridGeometry2D getOriginalGridGeometry() {
        return originalGridGeometry;
    }

    public MathTransform[] getMathTransform() {
        return this.mathTransform;
    }

    public MathTransform[] getInverseMathTransform() {
        return this.inverseMathTransform;
    }

    public int getLastPaintedLevel() {
        return this.lastPaintedLevel;
    }

    public Map<ImageTile, IImage> getImageMap() {
        return this.imageMap;
    }

    public int getTileSize() {
        return this.tileSize;
    }

    public PluginDataObject getPdo() {
        return this.pdo;
    }
}
