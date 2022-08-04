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

import java.io.File;
import java.lang.ref.WeakReference;

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.GridRequestableData;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.SliceUtil;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.projection.RadarProjectionFactory;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.Request.Type;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.data.UnitConvertingDataFilter;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.numeric.buffer.ByteBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.filter.InverseFillValueFilter;
import com.raytheon.uf.common.numeric.filter.UnsignedFilter;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.viz.core.exception.VizException;

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
 * Feb 06 2018   6747     bsteffen  Speed up slab requests by finding subset
 *                                  before reprojection.
 * Apr 15, 2019  7596     lsingh    Upgraded javax.measure to JSR-363. 
 *                                  Simplified unit assignment.
 * 
 * </pre>
 * 
 * @author rjpeter
 */
public class RadarRequestableData extends GridRequestableData {

    private final RadarRecord radarSource;

    private WeakReference<FloatDataRecord> cache = null;

    public RadarRequestableData(RadarRecord source, String parameterAbbrev)
            throws VizException {
        this.radarSource = source;
        source.setAddSpatial(false);
        // set unit converter here
        ColorMapParameters cMapParams = RadarAdapter.getColorMap(radarSource);
        Unit<?> unit = cMapParams.getDisplayUnit();

        this.source = "radar";
        this.dataTime = source.getDataTime();
        this.space = RadarAdapter.getInstance().getCoverage();
        this.level = LevelFactory.getInstance().getLevel("TILT",
                source.getPrimaryElevationAngle());

        this.parameter = parameterAbbrev;
        this.parameterName = "";
        this.unit = unit;

        try {
            GridRecord record = new GridRecord();
            record.setDatasetId(this.source);
            record.setLocation(RadarAdapter.getInstance().getCoverage());
            record.setLevel(this.level);
            Parameter parameter = new Parameter(parameterAbbrev,
                    this.parameterName, unit);
            record.setParameter(parameter);
            record.setDataTime(source.getDataTime());
            setGridSource(record);
        } catch (Exception e) {
            throw new VizException(e);
        }
    }

    @Override
    public IDataRecord[] getDataValue(Object arg) throws DataCubeException {
        FloatDataRecord fdr = null;
        boolean fullRecord = true;
        if (cache != null) {
            fdr = cache.get();
        }
        if (fdr == null) {
            File loc = HDF5Util.findHDF5Location(radarSource);
            IDataStore dataStore = DataStoreFactory.getDataStore(loc);
            try {
                RadarDataRetriever.populateRadarRecord(dataStore, radarSource);
            } catch (Exception e) {
                throw new DataCubeException(
                        "Error Retrieving Data from Radar Record", e);
            }
            try {
                UnitConverter converter = radarSource.getDataUnit()
                        .getConverterToAny(getUnit());
                GridGeometry2D destGeom = gridSource.getLocation()
                        .getGridGeometry();
                GridGeometry2D slabGeom = handleSlabRequest(arg, destGeom);
                if (slabGeom != null) {
                    destGeom = slabGeom;
                    fullRecord = false;
                }
                GridEnvelope2D destRange = destGeom.getGridRange2D();

                FloatBufferWrapper reprojectedData = new FloatBufferWrapper(
                        destRange);

                Coordinate centerLatLon = new Coordinate(
                        radarSource.getLongitude(), radarSource.getLatitude());

                GridGeometry2D sourceGeom = RadarProjectionFactory
                        .constructGridGeometry(centerLatLon,
                                radarSource.getAngleData(),
                                radarSource.getGateResolution(),
                                radarSource.getTrueElevationAngle(),
                                radarSource.getNumBins(), true);

                ByteBufferWrapper byteSource = new ByteBufferWrapper(
                        radarSource.getRawData(), sourceGeom.getGridRange2D());

                DataSource source = UnsignedFilter.apply(byteSource);

                source = UnitConvertingDataFilter.apply(source, converter);

                /*
                 * Based off looking at Awips I Col Max reflectivity it looks
                 * like they use -10 when there is no data.
                 */
                source = InverseFillValueFilter.apply(source, -10);
                GridReprojection reproj = new GridReprojection(sourceGeom,
                        destGeom);
                reproj.reprojectedGrid(new NearestNeighborInterpolation(),
                        source, reprojectedData);
                fdr = new FloatDataRecord();
                fdr.setFloatData(reprojectedData.getArray());
                fdr.setSizes(new long[] { destRange.width, destRange.height });
                fdr.setDimension(2);

            } catch (FactoryException | TransformException | UnconvertibleException | IncommensurableException e) {
                throw new DataCubeException(e);
            }

            if (fullRecord) {
                cache = new WeakReference<>(fdr);
            }
        }
        if (fullRecord && arg instanceof Request) {
            fdr = SliceUtil.slice(fdr, (Request) arg);
        }
        return new IDataRecord[] { fdr };
    }

    /**
     * Helper for {@link #getDataValue(Object)} that can determine if the arg
     * can be converted into a subset of the destination grid geometry, which
     * can be useful for reducing the amount of reprojection necessary to
     * fulfill the request.
     * 
     * @param arg
     *            whatever was passed into getData Value
     * @param destGeom
     *            The full grid geometry of the data this object is supposed to
     *            rpduce.
     * @return a geometry describing a subset of the destGeom based on arg, or
     *         null if that isn't possible.
     * @throws TransformException
     */
    private static GridGeometry2D handleSlabRequest(Object arg,
            GridGeometry2D destGeom) throws TransformException {
        if (!(arg instanceof Request)) {
            return null;
        }
        Request request = (Request) arg;
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
