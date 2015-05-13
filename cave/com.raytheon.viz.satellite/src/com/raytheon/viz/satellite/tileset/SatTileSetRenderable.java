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

import java.awt.Rectangle;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.eclipse.swt.widgets.Display;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ByteBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ShortBufferWrapper;
import com.raytheon.uf.common.numeric.filter.UnsignedFilter;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IImage.Status;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.tile.RecordTileSetRenderable;
import com.raytheon.uf.viz.core.tile.Tile;
import com.raytheon.uf.viz.core.tile.TileLevel;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Satellite tile set renderable, uses {@link SatDataRetriever} for {@link Tile}
 * data retrieval
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 19, 2013           mschenke    Initial creation
 * Jun 19, 2014  3238     bsteffen    Add method to create a DataSource for
 *                                    a tile level.
 * Oct 15, 2014  3681     bsteffen    Allow asynchronous interrogation.
 * May 04, 2015  4426     bsteffen    Fix unsigned interrogation.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SatTileSetRenderable extends RecordTileSetRenderable {

    private final AbstractVizResource<?, ?> resource;

    /**
     * Provides a temporary cache of data so that if multiple interrogations
     * and/or rendering will not retrieve data that has been recently used. The
     * references in this map that have been cleared are ignored since the
     * maximum size is limited to the number of tiles which should be fairly
     * reasonable.
     */
    private final Map<Tile, Reference<ColorMapData>> dataCache = new ConcurrentHashMap<>();

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
        Reference<ColorMapData> dataRef = dataCache.get(tile);
        ColorMapData data = null;
        if (dataRef != null) {
            data = dataRef.get();
        }
        if (data == null) {
            data = new SatDataRetriever((SatelliteRecord) record,
                    tile.tileLevel, tile.getRectangle()).getColorMapData();
        }
        if (data != null) {
            dataCache.put(tile, new SoftReference<ColorMapData>(data));
        }
        return data;
    }

    @Override
    protected void issueRefresh(IGraphicsTarget target) {
        super.issueRefresh(target);
        resource.issueRefresh();
    }

    public SatelliteRecord getSatelliteRecord() {
        return (SatelliteRecord) record;
    }

    /**
     * @return a {@link GeographicDataSource} that can be used for interrogating
     *         any point on the renderable for the most recently painted tile
     *         level.
     */
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

    @Override
    public double interrogate(Coordinate coordinate, Unit<?> resultUnit)
            throws VizException {
        ColorMapParameters parameters = colormapping.getColorMapParameters();
        /*
         * RecordTileSetRenderable is nearly identical but it calls
         * super.interrogate here which ignores the override of that specific
         * interrogate method so we must override this method to ensure this
         * classes interrogate methods are used.
         */
        return interrogate(coordinate, resultUnit, parameters.getNoDataValue());
    }

    @Override
    public double interrogate(Coordinate coordinate, Unit<?> resultUnit,
            double nanValue) throws VizException {
        /* Overriden to provide accurate results asynchronously */
        TileLevel level = tileSet.getTileLevel(lastPaintedLevel);

        double[] grid = null;
        try {
            double[] local = new double[2];
            llToLocalProj
                    .transform(new double[] { coordinate.x, coordinate.y }, 0,
                            local, 0, 1);
            grid = level.crsToGrid(local[0], local[1]);
        } catch (TransformException e) {
            throw new VizException("Error interrogating ", e);
        }

        return getDataValue(level, grid[0], grid[1], resultUnit, nanValue);
    }

    /**
     * Get a single data value for a specific 2d index. This method will load
     * data from the image if it is calle don the appropriate thread and the
     * image has data. Otherwise it requests the data from the datastore(with
     * some caching).
     * 
     * @param level
     *            the level to get the data for.
     * @param x
     *            the x index in the level.
     * @param y
     *            the y index in the level
     * @param resultUnit
     *            the unit the data should be in.
     * @param nanValue
     *            a special value that will return nan.
     * @return a data value in the specified unit.
     */
    protected double getDataValue(TileLevel level, double x, double y,
            Unit<?> resultUnit, double nanValue) {
        double dataValue = Double.NaN;
        IColormappedImage cmapImage = null;
        Tile tile = level.getTile(x, y);
        /**
         * If we are not on the UI thread then do not use the image for the data
         * value.
         */
        if (tile != null && Display.getCurrent() != null) {
            DrawableImage di = imageMap.get(tile);
            if (di != null) {
                IImage image = di.getImage();
                if (image instanceof IColormappedImage) {
                    cmapImage = (IColormappedImage) image;
                }
            }
        }
        int tilex = (int) x % tileSize;
        int tiley = (int) y % tileSize;
        Unit<?> dataUnit = null;
        /**
         * In some implementations of IColormappedImage NaN is returned if the
         * data is not loaded. So for those cases fall back to retrieving the
         * tile.
         */
        if (cmapImage != null && (cmapImage.getStatus() == Status.STAGED || cmapImage.getStatus() == Status.LOADED)) {
            dataValue = cmapImage.getValue(tilex, tiley);
            if (dataValue == nanValue) {
                dataValue = Double.NaN;
            } else {
                ColorMapParameters parameters = cmapImage
                        .getColorMapParameters();
                dataUnit = cmapImage.getDataUnit();
                if (parameters.getDataMapping() != null) {
                    dataUnit = parameters.getColorMapUnit();
                }
            }
        } else if (tile != null) {
            ColorMapData data = retrieveRecordData(tile);
            Rectangle rect = tile.getRectangle();
            DataSource source = BufferWrapper.wrap(data.getBuffer(),
                    rect.width, rect.height);
            if (data.getDataType() == ColorMapDataType.BYTE) {
                source = UnsignedFilter.apply((ByteBufferWrapper) source);
            } else if (data.getDataType() == ColorMapDataType.UNSIGNED_SHORT) {
                source = UnsignedFilter.apply((ShortBufferWrapper) source);
            }
            dataValue = source.getDataValue(tilex, tiley);
            dataUnit = data.getDataUnit();
        }

        /** Reconcile any unit discrepencies. */
        if (resultUnit != null && dataUnit != null
                && dataUnit.equals(resultUnit) == false) {
            if (resultUnit.isCompatible(dataUnit)) {
                dataValue = dataUnit.getConverterTo(resultUnit).convert(
                        dataValue);
            } else {
                UnitFormat uf = UnitFormat.getUCUMInstance();
                String message = String
                        .format("Unable to interrogate tile set.  Desired unit (%s) is not compatible with data unit (%s).",
                                uf.format(resultUnit), uf.format(dataUnit));
                throw new IllegalArgumentException(message);
            }
        }

        return dataValue;
    }

    private class TileLevelDataSource implements DataSource {

        private final TileLevel level;

        public TileLevelDataSource(TileLevel level) {
            this.level = level;
        }

        @Override
        public double getDataValue(final int x, final int y) {
            ColorMapParameters parameters = colormapping
                    .getColorMapParameters();
            return SatTileSetRenderable.this.getDataValue(level, x, y, null,
                    parameters.getNoDataValue());
        }

    }
}
