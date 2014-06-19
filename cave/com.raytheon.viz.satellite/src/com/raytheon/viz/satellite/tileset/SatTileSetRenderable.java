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
package com.raytheon.viz.satellite.tileset;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.tile.RecordTileSetRenderable;
import com.raytheon.uf.viz.core.tile.Tile;
import com.raytheon.uf.viz.core.tile.TileLevel;

/**
 * Satellite tile set renderable, uses {@link SatDataRetriever} for {@link Tile}
 * data retrieval
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2013            mschenke     Initial creation
 * Jun 19, 2014 3238       bsteffen     Add method to create a DataSource for
 *                                      a tile level.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SatTileSetRenderable extends RecordTileSetRenderable {

    private final AbstractVizResource<?, ?> resource;

    /**
     * Create satellite tile set renderable
     * 
     * @param resource
     * @param record
     * @param signed
     */
    public SatTileSetRenderable(AbstractVizResource<?, ?> resource,
            SatelliteRecord record) {
        // Total levels = Number of interpolation levels + base level
        super(resource, record, record.getGridGeometry(), record
                .getInterpolationLevels() + 1);
        this.resource = resource;
    }

    @Override
    protected ColorMapData retrieveRecordData(Tile tile) {
        return new SatDataRetriever((SatelliteRecord) record, tile.tileLevel,
                tile.getRectangle()).getColorMapData();
    }

    @Override
    protected void issueRefresh(IGraphicsTarget target) {
        super.issueRefresh(target);
        resource.issueRefresh();
    }

    public SatelliteRecord getSatelliteRecord() {
        return (SatelliteRecord) record;
    }

    public GeographicDataSource getCurrentLevelDataSource() {
        TileLevel level = tileSet.getTileLevel(lastPaintedLevel);
        DataSource tile = new TileLevelDataSource(level);
        GridEnvelope range = tileSetGeometry.getGridRange();
        int levelFactor = 2 * level.getLevel();
        if (levelFactor == 0) {
            levelFactor = 1;
        }
        int startX = range.getLow(0) / levelFactor;
        int startY = range.getLow(1) / levelFactor;
        int width = range.getSpan(0) / levelFactor;
        int height = range.getSpan(1) / levelFactor;

        GridGeometry2D levelGeometry = new GridGeometry2D(
                new GeneralGridEnvelope(new int[] { startX, startY },
                        new int[] { width, height }, false),
                tileSetGeometry.getEnvelope());

        return new GeographicDataSource(tile, levelGeometry);
    }

    private class TileLevelDataSource implements DataSource {

        private final TileLevel level;

        public TileLevelDataSource(TileLevel level) {
            this.level = level;
        }

        @Override
        public double getDataValue(final int x, final int y) {
            Tile tile = level.getTile((double) x, (double) y);
            IColormappedImage cmapImage = null;
            if (tile != null) {
                DrawableImage di = imageMap.get(tile);
                if (di != null) {
                    IImage image = di.getImage();
                    if (image instanceof IColormappedImage) {
                        cmapImage = (IColormappedImage) image;
                    }
                }
            }
            if (cmapImage != null) {
                final IColormappedImage theImage = cmapImage;
                final double[] result = new double[1];
                VizApp.runSync(new Runnable() {

                    @Override
                    public void run() {
                        result[0] = theImage.getValue(x % tileSize, y
                                % tileSize);
                    }

                });
                return result[0];
            }
            return Double.NaN;
        }

    }
}
