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

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube.QueryStatus;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;

import java.awt.Point;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

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
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.d2d.nsharp.rsc.D2DNSharpResourceData;
import com.raytheon.uf.viz.points.PointsDataManager;

/**
 * Resource data which allows loading GOESR Legacy Moisture/Temperature profiles
 * into NSharp in the D2D perspective.
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
@XmlAccessorType(XmlAccessType.NONE)
public class GoesrLegacyProfileResourceData extends D2DNSharpResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GoesrLegacyProfileResourceData.class);

    public GoesrLegacyProfileResourceData() {
        super();
    }

    @Override
    protected void preparePointInfo() {
        if (coordinate == null && pointName != null) {
            coordinate = PointsDataManager.getInstance().getCoordinate(
                    pointName);
        }

    }

    @Override
    protected NcSoundingCube getSoundingCube(NsharpStationInfo stnInfo) {
        DataTime time = new DataTime(stnInfo.getReftime());
        DbQueryRequest pdoRequest = new DbQueryRequest(getMetadataMap());
        pdoRequest.addConstraint(PluginDataObject.DATATIME_ID,
                new RequestConstraint(time.toString()));
        pdoRequest.setEntityClass(SatelliteRecord.class);
        try {
            DbQueryResponse response = (DbQueryResponse) RequestRouter
                    .route(pdoRequest);
            /* This should be of size 1 in all normal circumstances. */
            Map<SatMapCoverage, Request> requestMap = new HashMap<SatMapCoverage, Request>(
                    2);
            GoesrProfileBuilder builder = new GoesrProfileBuilder();
            for (SatelliteRecord record : response
                    .getEntityObjects(SatelliteRecord.class)) {
                SatMapCoverage coverage = record.getCoverage();
                Request request = requestMap.get(coverage);
                if (request == null) {
                    request = getRequest(coverage);
                    requestMap.put(coverage, request);
                }
                if (request != null) {
                    IDataStore dataStore = DataStoreFactory
                            .getDataStore(HDF5Util.findHDF5Location(record));
                    IDataRecord dataRecord = dataStore.retrieve(
                            record.getDataURI(),
                            SatelliteRecord.SAT_DATASET_NAME, request);
                    builder.addRecord(record, dataRecord, 0);
                }
            }
            if (builder.isEmpty()) {
                return null;
            }
            NcSoundingProfile profile = builder.toNcSoundingProfile();
            NcSoundingCube cube = new NcSoundingCube(Arrays.asList(profile));
            cube.setRtnStatus(QueryStatus.OK);
            return cube;
        } catch (Exception e) {
            statusHandler.error("Error occured retrieving GOESR profile data.",
                    e);
        }
        return null;
    }

    private Request getRequest(SatMapCoverage coverage)
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
            return null;
        } else if (point.x < 0 || point.x >= nx || Double.isNaN(point.x)) {
            return null;
        } else {
            return Request.buildPointRequest(new Point((int) Math
                    .round(point.x), (int) Math.round(point.y)));
        }
    }


}
