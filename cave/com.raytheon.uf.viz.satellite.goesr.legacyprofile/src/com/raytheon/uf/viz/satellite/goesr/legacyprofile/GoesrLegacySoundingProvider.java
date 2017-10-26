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
package com.raytheon.uf.viz.satellite.goesr.legacyprofile;

import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;

import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.sounding.providers.AbstractVerticalSoundingProvider;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Provider which allows GOESR Legacy Moisture/Temperature profiles to be used
 * for popup skewt
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 30, 2015  4335     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GoesrLegacySoundingProvider extends
        AbstractVerticalSoundingProvider<SatelliteRecord[]> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GoesrLegacySoundingProvider.class);

    @Override
    public String getSoundingSource() {
        return "GOES";
    }

    @Override
    protected SatelliteRecord[] queryForData(
            Map<String, RequestConstraint> constraints, DataTime time,
            Coordinate location) {
        DbQueryRequest request = new DbQueryRequest(constraints);

        RequestConstraint timeConstraint = new RequestConstraint(
                time.toString());
        request.addConstraint(PluginDataObject.DATATIME_ID, timeConstraint);
        DbQueryResponse response;
        try {
            response = (DbQueryResponse) RequestRouter.route(request);
        } catch (Exception e) {
            statusHandler.error("Unable to request satellite data.", e);
            return new SatelliteRecord[0];
        }
        SatelliteRecord[] records = response
                .getEntityObjects(SatelliteRecord.class);
        for (SatelliteRecord record : records) {
            IDataStore dataStore = DataStoreFactory.getDataStore(HDF5Util
                    .findHDF5Location(record));
            try {
                IDataRecord dataRecord = dataStore.retrieve(
                        record.getDataURI(), SatelliteRecord.SAT_DATASET_NAME,
                        Request.ALL);
                record.setMessageData(dataRecord);
            } catch (FileNotFoundException | StorageException e) {
                statusHandler.error("Unable to retrieve satellite data.", e);
            }

        }
        return records;
    }

    @Override
    protected VerticalSounding createSounding(DataTime time,
            SatelliteRecord[] records, Coordinate location) {
        GoesrProfileBuilder builder = new GoesrProfileBuilder();
        /* This should be of size 1 in normal circumstances. */
        Map<SatMapCoverage, Integer> indexMap = new HashMap<SatMapCoverage, Integer>(
                2);
        for (SatelliteRecord record : records) {
            IDataRecord dataRecord = (IDataRecord) record.getMessageData();
            if (dataRecord == null) {
                continue;
            }
            SatMapCoverage coverage = record.getCoverage();
            Integer index = indexMap.get(coverage);
            if (index == null) {
                try {
                    index = getDataIndex(coverage, location);
                } catch (Exception e) {
                    statusHandler.error(
                            "Error determining satellite data location", e);
                    continue;
                }
                indexMap.put(coverage, index);
            }
            if (index >= 0) {
                builder.addRecord(record, dataRecord, index);
            }

        }
        if (builder.isEmpty()) {
            return null;
        }
        VerticalSounding sounding = builder.toVerticalSounding();
        sounding.setStationId(getSoundingSource());
        return sounding;
    }

    private int getDataIndex(SatMapCoverage coverage, Coordinate coordinate)
            throws FactoryException, TransformException {
        MathTransform ll2crs = CRS.findMathTransform(
                DefaultGeographicCRS.WGS84, coverage.getCrs(), true);
        MathTransform crs2grid = coverage.getGridGeometry().getCRSToGrid2D();
        DirectPosition2D point = new DirectPosition2D(coordinate.x,
                coordinate.y);
        ll2crs.transform(point, point);
        crs2grid.transform(point, point);
        int nx = coverage.getNx();
        int ny = coverage.getNy();
        if (point.y < 0 || point.y >= ny || Double.isNaN(point.y)) {
            return -1;
        } else if (point.x < 0 || point.x >= nx || Double.isNaN(point.x)) {
            return -1;
        } else {
            return nx * (int) point.y + (int) point.x;
        }
    }

}
