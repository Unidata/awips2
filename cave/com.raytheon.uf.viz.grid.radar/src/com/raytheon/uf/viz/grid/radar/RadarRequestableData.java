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
package com.raytheon.uf.viz.grid.radar;

import java.lang.ref.SoftReference;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableFuture;

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.UnitConverter;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.GridRequestableData;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.SliceUtil;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.projection.RadarProjectionFactory;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.Request.Type;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.data.UnitConvertingDataFilter;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ByteBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.filter.InverseFillValueFilter;
import com.raytheon.uf.common.numeric.filter.UnsignedFilter;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.IPerformanceTimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.radar.DefaultVizRadarRecord;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

/**
 * A requestable data record which wraps a RadarRecord and can convert radar
 * radial data into the expected radar projection and units.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 18, 2010  4473     rjpeter   Initial creation
 * Aug 30, 2013  2298     rjpeter   Make getPluginName abstract
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * Aug 15, 2017  6332     bsteffen  Move to viz.grid.radar plugin
 * Jan 24, 2018  6907     bsteffen  Replace RadarMapper with more accurate
 *                                  GridReprojection
 * Feb 06, 2018  6747     bsteffen  Speed up slab requests by finding subset
 *                                  before reprojection.
 * Apr 15, 2019  7596     lsingh    Upgraded javax.measure to JSR-363.
 *                                  Simplified unit assignment.
 * Jul 07, 2021  8576     randerso  Changed RadarAdapter to support multiple
 *                                  local radars as defined in radarsInUse.txt
 * Jul 28, 2021  8611     randerso  Refactored to use more of
 *                                  GridRequestableData caching. Changed no data
 *                                  value to NaN to look more like normal radar
 *                                  products.
 * Jan 14, 2022  8741     njensen   Refactored caching to also support caching
 *                                  requests that are not Request.ALL
 * Jan 26, 2022  8741     njensen   Ensure notifyAll() is called by calling
 *                                  cacheDataValue() and add performance logging
 * Feb 22, 2023  9021     mapeters  Cache data as Futures, add getRadialData and
 *                                  dispose methods to support SRM subclass
 * Mar 14, 2023  2031675  mapeters  Improve performance of unit conversion and
 *                                  reprojection (use RadarGridReprojection)
 * Dec 20, 2023  2036519  mapeters  Remove dispose(), use DefaultVizRadarRecord
 * May 22, 2024  2037092  mapeters  Pass grid record to super constructor
 *
 * </pre>
 *
 * @author rjpeter
 */
public class RadarRequestableData extends GridRequestableData {

    private static final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler(RadarRequestableData.class.getSimpleName());

    protected final RadarRecord radarSource;

    public RadarRequestableData(RadarRecord source, String parameterAbbrev)
            throws VizException {
        this(source,
                buildGridRecord(source, parameterAbbrev, source.getDataTime()));
    }

    protected RadarRequestableData(RadarRecord source, GridRecord gridSource)
            throws VizException {
        super(gridSource);
        // Wrap record so that the raw data is cached/shared
        source.setAddSpatial(false);
        if (!(source instanceof DefaultVizRadarRecord)) {
            source = new DefaultVizRadarRecord(source);
        }
        this.radarSource = source;
    }

    /**
     * Build a grid record representing the given radar record.
     *
     * @param source
     *            radar record source
     * @param paramAbbrev
     *            parameter abbreviation to set on the grid record
     * @param time
     *            time to set on the grid record
     * @return the grid record
     * @throws VizException
     */
    protected static GridRecord buildGridRecord(RadarRecord source,
            String paramAbbrev, DataTime time) throws VizException {
        if (time.isSpatial()) {
            /*
             * Clear level from time, since it's set separately on the grid
             * record
             */
            time = time.clone();
            time.clearLevel();
        }
        String icao = source.getIcao().toLowerCase();
        try {
            GridRecord record = new GridRecord();
            record.setDatasetId(RadarAsGridUtil.getModelNameForIcao(icao));
            record.setLocation(RadarAdapter.getInstance().getCoverage(icao));
            record.setLevel(LevelFactory.getInstance().getLevel(RadarUtil.TILT,
                    source.getPrimaryElevationAngle()));
            Parameter parameter = new Parameter(paramAbbrev, "",
                    RadarAdapter.getColorMap(source).getDisplayUnit());
            record.setParameter(parameter);
            record.setDataTime(time);
            return record;
        } catch (Exception e) {
            throw new VizException(e);
        }
    }

    /**
     * Determine the radial data to reproject to grid, based on the raw radial
     * data in the radar record.
     *
     * Suppress warning about declared exception not being thrown, as overrides
     * do throw it.
     *
     * @return the radial data to reproject to grid
     * @throws DataCubeException
     */
    @SuppressWarnings("unused")
    protected byte[] getRadialData() throws DataCubeException {
        return radarSource.getRawData();
    }

    @Override
    public IDataRecord[] getDataValue(Object arg) throws DataCubeException {
        Request request = arg instanceof Request ? (Request) arg : Request.ALL;
        request = request.shallowCopy();
        boolean fullRecord = Request.ALL.equals(request);

        Future<IDataRecord[]> recordFuture;

        synchronized (cache) {
            recordFuture = getCachedValue(request);

            if (recordFuture == null && !fullRecord) {
                /*
                 * in case the request was not ALL, check to see if the full
                 * record has been cached
                 */
                recordFuture = getCachedValue(Request.ALL);
                if (recordFuture != null) {
                    fullRecord = true;
                }
            }

            if (recordFuture == null) {
                GridGeometry2D destGeom = gridSource.getLocation()
                        .getGridGeometry();
                GridGeometry2D slabGeom;
                try {
                    slabGeom = handleSlabRequest(request, destGeom);
                } catch (TransformException e) {
                    throw new DataCubeException(
                            "Error retrieving data from radar record "
                                    + radarSource,
                            e);
                }
                if (slabGeom != null) {
                    destGeom = slabGeom;
                    fullRecord = false;
                } else {
                    fullRecord = true;
                }
                GridGeometry2D finalDestGeom = destGeom;
                recordFuture = new FutureTask<>(
                        () -> getDataValueInternal(finalDestGeom));

                if (fullRecord) {
                    cache.put(Request.ALL, new SoftReference<>(recordFuture));
                } else {
                    // put the sliced result in the cache
                    cache.put(request, new SoftReference<>(recordFuture));
                }
            }
        }

        IDataRecord[] record;
        try {
            if (recordFuture instanceof RunnableFuture) {
                ((RunnableFuture<IDataRecord[]>) recordFuture).run();
            }
            record = recordFuture.get();
        } catch (InterruptedException | ExecutionException e) {
            throw new DataCubeException(
                    "Error retrieving data from radar record " + radarSource,
                    e);
        }

        FloatDataRecord fdr = (FloatDataRecord) record[0];
        // only slice full records, otherwise they've already been sliced
        if (fullRecord) {
            fdr = SliceUtil.slice(fdr, request);
        }

        return new IDataRecord[] { fdr };
    }

    private IDataRecord[] getDataValueInternal(GridGeometry2D destGeom)
            throws DataCubeException {
        try {
            UnitConverter converter = radarSource.getDataUnit()
                    .getConverterToAny(getUnit());

            GridEnvelope2D destRange = destGeom.getGridRange2D();

            FloatBufferWrapper reprojectedData = new FloatBufferWrapper(
                    destRange);

            Coordinate centerLatLon = new Coordinate(radarSource.getLongitude(),
                    radarSource.getLatitude());

            GridGeometry2D sourceGeom = RadarProjectionFactory
                    .constructGridGeometry(centerLatLon,
                            radarSource.getAngleData(),
                            radarSource.getGateResolution(),
                            radarSource.getTrueElevationAngle(),
                            radarSource.getNumBins(), true);

            ByteBufferWrapper byteSource = new ByteBufferWrapper(
                    getRadialData(), sourceGeom.getGridRange2D());

            DataSource source = UnsignedFilter.apply(byteSource);

            source = UnitConvertingDataFilter.apply(source, converter);

            source = InverseFillValueFilter.apply(source, Float.NaN);

            IPerformanceTimer timer = TimeUtil.getPerformanceTimer();
            timer.start();

            int numSourcePoints = byteSource.getNx() * byteSource.getNy();
            int numDestPoints = reprojectedData.getNx()
                    * reprojectedData.getNy();
            if (numSourcePoints < numDestPoints) {
                /*
                 * For performance, do unit conversions on source if it has
                 * fewer data points than the destination.
                 */
                BufferWrapper processedSource = new FloatBufferWrapper(
                        byteSource.getNx(), byteSource.getNy());
                for (int x = 0; x < byteSource.getNx(); ++x) {
                    for (int y = 0; y < byteSource.getNy(); ++y) {
                        processedSource.setDataValue(source.getDataValue(x, y),
                                x, y);
                    }
                }
                source = processedSource;

                timer.lap("units");
            }

            RadarGridReprojection reproj = new RadarGridReprojection(sourceGeom,
                    destGeom);
            reproj.reprojectedGrid(new NearestNeighborInterpolation(), source,
                    reprojectedData);

            timer.lap("projection");

            timer.logLaps("Converting radar to grid", perfLog);
            FloatDataRecord fdr = new FloatDataRecord();
            fdr.setFloatData(reprojectedData.getArray());
            fdr.setSizes(new long[] { destRange.width, destRange.height });
            fdr.setDimension(2);

            return new IDataRecord[] { fdr };
        } catch (FactoryException | TransformException | UnconvertibleException
                | IncommensurableException e) {
            throw new DataCubeException(
                    "Error retrieving data from radar record " + radarSource,
                    e);
        }
    }

    /**
     * Helper for {@link #getDataValue(Object)} that tries to shrink the
     * destination grid geometry if possible, which can be useful for reducing
     * the amount of reprojection necessary to fulfill the request.
     *
     * @param request
     *            the request type
     * @param destGeom
     *            the full grid geometry of the data this object is supposed to
     *            produce
     * @return a geometry describing a subset of the destGeom based on the given
     *         request, or null if that isn't possible.
     * @throws TransformException
     */
    private static GridGeometry2D handleSlabRequest(Request request,
            GridGeometry2D destGeom) throws TransformException {
        if (request.getType() != Type.SLAB) {
            return null;
        }
        int[] min = request.getMinIndexForSlab();
        int[] max = request.getMaxIndexForSlab();
        int x = min[0];
        int y = min[1];
        int width = max[0] - x;
        int height = max[1] - y;
        GridEnvelope2D gridEnv = new GridEnvelope2D(x, y, width, height);
        Envelope worldEnv = destGeom.gridToWorld(gridEnv);
        gridEnv.x = 0;
        gridEnv.y = 0;
        return new GridGeometry2D(gridEnv, worldEnv);
    }
}
