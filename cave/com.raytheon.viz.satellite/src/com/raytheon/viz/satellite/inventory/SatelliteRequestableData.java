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
package com.raytheon.viz.satellite.inventory;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.Map;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.interpolation.GridDownscaler;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.PrecomputedGridReprojection;
import com.raytheon.uf.common.geospatial.util.SubGridGeometryCalculator;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.datacube.CubeUtil;
import com.raytheon.viz.satellite.tileset.SatDataRetriever;

/**
 * Object capable of requesting Satellite data for base or derived displays. Can
 * also reproject into different coverages
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 09, 2014  2947     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SatelliteRequestableData extends AbstractRequestableData {

    private final SatelliteRecord record;

    public SatelliteRequestableData(SatelliteRecord record, Level level) {
        this.record = record;
        this.dataTime = record.getDataTime();
        this.level = level;
        this.parameter = record.getPhysicalElement();
        this.parameterName = record.getPhysicalElement();
        this.source = record.getSectorID();
        this.space = new ComparableSatMapCoverage(record.getCoverage());
    }

    public SatelliteRecord getRecord() {
        return record;
    }

    @Override
    public Object getDataValue(Object arg) throws DataCubeException {
        if (arg instanceof GridGeometry2D) {
            GridGeometry2D requestGeom = (GridGeometry2D) arg;
            GridGeometry2D recordGeom = record.getGridGeometry();
            validateCRSMatch(requestGeom, recordGeom);
            /* Figure out what level is needed */
            Envelope2D requestEnv = requestGeom.getEnvelope2D();
            GridEnvelope2D requestRange = requestGeom.getGridRange2D();
            double requestDx = requestEnv.width / requestRange.width;
            double requestDy = requestEnv.height / requestRange.height;
            Envelope2D recordEnv = recordGeom.getEnvelope2D();
            Rectangle[] levels = GridDownscaler.getDownscaleSizes(recordGeom);
            int bestLevel = 0;
            for (int level = levels.length - 1; level >= 0; level -= 1) {
                double levelDx = recordEnv.width / levels[level].width;
                double levelDy = recordEnv.height / levels[level].height;
                if (levelDx <= requestDx || levelDy <= requestDy) {
                    bestLevel = Math.max(0, level);
                    break;
                }
            }
            /* figure out what area of the level is needed. */
            GridGeometry2D levelGeom = new GridGeometry2D(
                    (GridEnvelope) new GridEnvelope2D(levels[bestLevel]),
                    recordEnv);
            Request request;
            SubGridGeometryCalculator subGrid;
            try {
                subGrid = new SubGridGeometryCalculator(requestEnv, levelGeom);
            } catch (TransformException e) {
                throw new DataCubeException(e);
            }
            if (subGrid.isEmpty()) {
                /* The SpaceTimeMatcher should prevent this from ever happening */
                throw new DataCubeException(
                        "Request Area does not intersect data area.");
            } else if (subGrid.isFull()) {
                request = Request.ALL;
            } else {
                request = Request.buildSlab(subGrid.getGridRangeLow(true),
                        subGrid.getGridRangeHigh(false));
            }
            String dataset = DataStoreFactory.createDataSetName(null,
                    SatelliteRecord.SAT_DATASET_NAME, bestLevel);
            IDataRecord dataRecord = CubeUtil.retrieveData(record,
                    record.getPluginName(), request, dataset);
            unit = SatDataRetriever.getDataUnit(
                    SatDataRetriever.getRecordUnit(record), dataRecord);
            /* Reproject the data to match the request. */
            GridGeometry2D subGeom = subGrid.getSubGridGeometry2D();
            BufferWrapper source = BufferWrapper.wrapArray(
                    dataRecord.getDataObject(), subGeom.getGridRange2D().width,
                    subGeom.getGridRange2D().height);
            BufferWrapper dest = BufferWrapper.create(
                    source.getPrimitiveType(), requestRange.width,
                    requestRange.height);
            try {
                GridReprojection reproj = PrecomputedGridReprojection
                        .getReprojection(subGeom, requestGeom);
                reproj.reprojectedGrid(new NearestNeighborInterpolation(),
                        source, dest);
            } catch (TransformException e) {
                throw new DataCubeException(e);
            } catch (FactoryException e) {
                throw new DataCubeException(e);
            }
            Map<String, Object> attrs = dataRecord.getDataAttributes();
            dataRecord = DataStoreFactory.createStorageRecord(
                    dataRecord.getName(), dataRecord.getGroup(),
                    dest.getArray(), 2, new long[] { requestRange.width,
                            requestRange.height });
            dataRecord.setFillValue(0);
            if (attrs != null) {
                Number fill = (Number) attrs
                        .get(SatelliteRecord.SAT_FILL_VALUE);

                if (fill != null) {
                    dataRecord.setFillValue(fill);
                }
            }
            return dataRecord;
        } else {
            return CubeUtil.retrieveData(record, record.getPluginName(),
                    Request.ALL, SatelliteRecord.SAT_DATASET_NAME);
        }
    }

    private static void validateCRSMatch(GridGeometry2D requestGeom,
            GridGeometry2D recordGeom) throws DataCubeException {
        CoordinateReferenceSystem requestCRS = requestGeom
                .getCoordinateReferenceSystem();
        CoordinateReferenceSystem recordCRS = recordGeom
                .getCoordinateReferenceSystem();

        if (!requestCRS.equals(recordCRS)) {
            /* The SpaceTimeMatcher should prevent this from ever happening */
            throw new DataCubeException("Incompatible CRSs\n"
                    + requestCRS.toWKT() + "\n" + recordCRS.toWKT());
        }
    }

    @Override
    public Unit<?> getUnit() {
        if (this.unit == null) {
            /*
             * Normally a data request occurs before anything needs the unit so
             * this should never happen.
             */
            try {
                IDataRecord dataRecord = CubeUtil.retrieveData(record,
                        record.getPluginName(),
                        Request.buildPointRequest(new Point(0, 0)),
                        SatelliteRecord.SAT_DATASET_NAME);
                unit = SatDataRetriever.getDataUnit(
                        SatDataRetriever.getRecordUnit(record), dataRecord);
            } catch (DataCubeException e) {
                UFStatus.getHandler(getClass()).error("Cannot find data unit.",
                        e);

            }
        }
        return super.getUnit();
    }

}
