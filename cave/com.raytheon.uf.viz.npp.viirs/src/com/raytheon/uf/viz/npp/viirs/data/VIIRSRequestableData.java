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
package com.raytheon.uf.viz.npp.viirs.data;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.geometry.Envelope;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSSpatialCoverage;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.data.UnitConvertingDataFilter;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridDownscaler;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.numeric.DataUtilities;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ShortBufferWrapper;
import com.raytheon.uf.common.numeric.dest.DataDestination;
import com.raytheon.uf.common.numeric.filter.FillValueFilter;
import com.raytheon.uf.common.numeric.filter.InverseFillValueFilter;
import com.raytheon.uf.common.numeric.filter.UnsignedFilter;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;

/**
 * Object capable of requesting VIIRS data for base or derived displays. Can
 * also reproject into different coverages
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 19, 2012           mschenke    Initial creation
 * Mar 07, 2014  2791     bsteffen    Move Data Source/Destination to numeric
 *                                    plugin.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSRequestableData extends AbstractRequestableData {

    /**
     * The data request object, bounds of request, full coverage, and dataset to
     * request
     */
    public static class VIIRSRequest {
        public final Request request;

        public final String dataset;

        public final VIIRSSpatialCoverage coverage;

        public VIIRSRequest(Request request, String dataset,
                VIIRSSpatialCoverage coverage) {
            this.request = request;
            this.dataset = dataset;
            this.coverage = coverage;
        }
    }

    /** Data record this object requests for */
    private VIIRSDataRecord dataRecord;

    public VIIRSRequestableData(VIIRSDataRecord dataRecord, Level level) {
        this.dataRecord = dataRecord;
        setDataTime(dataRecord.getDataTime());
        setLevel(level);
        setParameter(dataRecord.getParameter());
        setParameterName(VIIRSDynamicParameters.createParameter(
                dataRecord.getWavelength(), dataRecord.getParameter()));
        setSource(dataRecord.getChannelType());
        setUnit(Unit.ONE);
    }

    /**
     * Gets the raw data records without converting to floats/applying scale and
     * offset. Will ensure returned data is projected into request's coverage.
     * Can be directly called if data is desired to stay in "short" format.
     * Otherwise calling {@link #getDataValue(Object)} will cause data to be
     * converted to "float" by having scale/offset applied
     * 
     * @param request
     * @return
     * @throws VizException
     */
    public IDataRecord[] getRawDataValue(VIIRSRequest request)
            throws VizException {
        IDataStore store = DataStoreFactory.getDataStore(HDF5Util
                .findHDF5Location(dataRecord));
        try {
            VIIRSSpatialCoverage recordCoverage = dataRecord.getCoverage();
            VIIRSSpatialCoverage requestCoverage = request.coverage;
            if (recordCoverage.equals(requestCoverage) == false) {
                // Data coverages are different, reprojection is required
                GridGeometry2D requestGeometry = requestCoverage
                        .getGridGeometry();
                GridGeometry2D recordGeometry = recordCoverage
                        .getGridGeometry();

                // Grab the downscale rectangles for the geomerties
                Rectangle[] requestSizes = GridDownscaler
                        .getDownscaleSizes(requestGeometry);
                Rectangle[] recordSizes = GridDownscaler
                        .getDownscaleSizes(recordGeometry);

                // Figure out what level we are requesting for
                int length = Math.min(requestSizes.length, recordSizes.length);
                int level;
                for (level = 0; level < length; ++level) {
                    if (VIIRSDataRecord.getDataSet(level).equals(
                            request.dataset)) {
                        break;
                    }
                }
                Rectangle requestLevelRect = requestSizes[level];
                Rectangle recordLevelRect = recordSizes[level];

                // Calculate ratio based on request level sizes
                double diffRatioX = recordLevelRect.getWidth()
                        / requestLevelRect.getWidth();
                double diffRatioY = recordLevelRect.getHeight()
                        / requestLevelRect.getHeight();

                GridGeometry2D requestSliceGeometry = null;
                GridGeometry2D recordSliceGeometry = null;

                Request req = request.request;
                Request recordRequest = req;
                switch (request.request.getType()) {
                case POINT:
                    Point[] points = req.getPoints();
                    Point[] newPoints = new Point[points.length];
                    for (int i = 0; i < points.length; ++i) {
                        newPoints[i] = new Point((int) Math.max(points[i].x
                                * diffRatioX, requestLevelRect.width - 1),
                                (int) Math.max(points[i].x * diffRatioX,
                                        requestLevelRect.height - 1));
                    }
                    recordRequest = Request.buildPointRequest(newPoints);
                    break;
                case SLAB:
                    int[] min = req.getMinIndexForSlab();
                    int[] max = req.getMaxIndexForSlab();
                    GridEnvelope2D reqGrid = new GridEnvelope2D(0, 0, max[0]
                            - min[0], max[1] - min[1]);
                    requestSliceGeometry = new GridGeometry2D(reqGrid,
                            (Envelope) requestGeometry.gridToWorld(reqGrid));
                    recordRequest = Request.buildSlab(
                            new int[] { (int) (min[0] * diffRatioX),
                                    (int) (min[1] * diffRatioY) },
                            new int[] {
                                    (int) Math.min(max[0] * diffRatioX,
                                            recordLevelRect.getMaxX()),
                                    (int) Math.min(max[1] * diffRatioY,
                                            recordLevelRect.getMaxY()) });
                    min = recordRequest.getMinIndexForSlab();
                    max = recordRequest.getMaxIndexForSlab();
                    GridEnvelope2D recGrid = new GridEnvelope2D(0, 0, max[0]
                            - min[0], max[1] - min[1]);
                    recordSliceGeometry = new GridGeometry2D(recGrid,
                            (Envelope) recordGeometry.gridToWorld(recGrid));
                    break;
                case ALL:
                    requestSliceGeometry = requestGeometry;
                    recordSliceGeometry = recordGeometry;
                    break;
                }

                IDataRecord record = store.retrieve(dataRecord.getDataURI(),
                        request.dataset, recordRequest);
                if (requestSliceGeometry != null && recordSliceGeometry != null) {
                    // Slice geometries are set, we need to reproject into
                    // request space
                    double noData = Double.NaN;
                    if (record.getDataAttributes().containsKey(
                            VIIRSDataRecord.MISSING_VALUE_ID)) {
                        noData = ((Number) record.getDataAttributes().get(
                                VIIRSDataRecord.MISSING_VALUE_ID))
                                .doubleValue();
                    }

                    GridReprojection reprojection = new GridReprojection(
                            recordSliceGeometry, requestSliceGeometry);
                    ShortBufferWrapper rawDest = new ShortBufferWrapper(
                            requestSliceGeometry.getGridRange2D());
                    DataDestination dest = InverseFillValueFilter.apply(
                            (DataDestination) rawDest, noData);
                    ShortBufferWrapper rawSource = new ShortBufferWrapper(
                            ((ShortDataRecord) record).getShortData(),
                            recordSliceGeometry.getGridRange2D());
                    DataSource source = UnsignedFilter.apply(rawSource);

                    source = FillValueFilter.apply(source, noData);
                    reprojection.reprojectedGrid(new BilinearInterpolation(),
                            source, dest);

                    ShortDataRecord scaled = new ShortDataRecord(
                            record.getName(), record.getGroup(),
                            rawDest.getArray());
                    copyRecord(scaled, record);
                    // set correct sizes after copying attributes
                    scaled.setIntSizes(new int[] {
                            requestSliceGeometry.getGridRange().getSpan(0),
                            requestSliceGeometry.getGridRange().getSpan(1) });
                    record = scaled;
                }
                return new IDataRecord[] { record };
            } else {
                return new IDataRecord[] { store.retrieve(
                        dataRecord.getDataURI(), request.dataset,
                        request.request) };
            }
        } catch (Exception e) {
            throw new VizException("Error retrieving viirs data: "
                    + e.getLocalizedMessage(), e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractRequestableData#getDataValue
     * (java.lang.Object)
     */
    @Override
    public IDataRecord[] getDataValue(Object arg) throws VizException {
        VIIRSRequest request = (VIIRSRequest) arg;
        // Get raw data
        IDataRecord[] records = getRawDataValue(request);
        for (int i = 0; i < records.length; ++i) {
            IDataRecord record = records[i];
            Map<String, Object> attrs = record.getDataAttributes();
            double noDataValue = Double.NaN;
            if (attrs.containsKey(VIIRSDataRecord.MISSING_VALUE_ID)) {
                // Replace no data value with NaN while assigning noDataValue
                noDataValue = ((Number) attrs.put(
                        VIIRSDataRecord.MISSING_VALUE_ID, noDataValue))
                        .doubleValue();
            }

            Unit<?> dataUnit = Unit.ONE;
            // Remove scale/offset since we are applying them now
            Float offset = (Float) attrs.remove(VIIRSDataRecord.OFFSET_ID);
            Float scale = (Float) attrs.remove(VIIRSDataRecord.SCALE_ID);

            if (offset != null && offset != 0.0) {
                dataUnit = dataUnit.plus(offset);
            }
            if (scale != null && scale != 0.0) {
                dataUnit = dataUnit.times(scale);
            }

            long[] sizes = record.getSizes();
            int width = (int) sizes[0], height = 1;
            if (sizes.length > 1) {
                height = (int) sizes[1];
            }
            float[] floatData = new float[width * height];
            final UnitConverter converter = dataUnit.getConverterTo(Unit.ONE);
            DataDestination destination = new FloatBufferWrapper(floatData,
                    width, height);
            destination = UnitConvertingDataFilter
                    .apply(destination, converter);
            ShortBufferWrapper shortData = new ShortBufferWrapper(
                    ((ShortDataRecord) record).getShortData(), width, height);
            DataSource source = UnsignedFilter.apply(shortData);
            source = FillValueFilter.apply(source, noDataValue);

            DataUtilities.copy(source, destination, width, height);

            // Create float data record from converted data
            FloatDataRecord fdr = new FloatDataRecord(record.getName(),
                    record.getGroup(), floatData);
            copyRecord(fdr, record);
            records[i] = fdr;
        }
        return records;
    }

    /**
     * @return the dataRecord
     */
    public VIIRSDataRecord getDataRecord() {
        return dataRecord;
    }

    /**
     * Copies one {@link IDataRecord} into another
     * 
     * @param to
     * @param from
     */
    public static void copyRecord(IDataRecord to, IDataRecord from) {
        to.setCorrelationObject(from.getCorrelationObject());
        to.setDataAttributes(from.getDataAttributes());
        to.setDimension(from.getDimension());
        to.setFillValue(from.getFillValue());
        to.setMaxChunkSize(from.getMaxChunkSize());
        to.setMaxSizes(from.getMaxSizes());
        to.setMinIndex(from.getMinIndex());
        to.setProperties(from.getProperties());
        to.setSizes(from.getSizes());
    }
}
