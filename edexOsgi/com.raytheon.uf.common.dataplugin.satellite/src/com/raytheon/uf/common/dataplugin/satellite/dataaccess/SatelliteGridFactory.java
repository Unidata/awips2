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
package com.raytheon.uf.common.dataplugin.satellite.dataaccess;

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;

import org.geotools.coverage.grid.GridGeometry2D;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.grid.IGridDataFactory;
import com.raytheon.uf.common.dataaccess.grid.IGridRequest;
import com.raytheon.uf.common.dataaccess.impl.AbstractDataFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridData;
import com.raytheon.uf.common.dataaccess.util.DataWrapperUtil;
import com.raytheon.uf.common.dataaccess.util.PDOUtil;
import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnits;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.comm.RequestRouter;

/**
 * A data factory for getting satellite data from the metadata database. There
 * are currently not any required identifiers.
 * 
 * Note: This factory currently does not support Storage Requests even though
 * they can be included in an IGridRequest.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 02, 2012            bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
public class SatelliteGridFactory extends AbstractDataFactory implements
        IGridDataFactory {
    private static final String FIELD_DATATIME = "dataTime.refTime";

    private static final String FIELD_PYHSICAL_ELEMENT = "physicalElement";

    private static final String FIELD_SECTOR_ID = "sectorID";

    public static final String DBQUERY_PLUGIN_NAME_KEY = "pluginName";

    private Map<String, GridGeometry2D> sectorGeometryMapCache;

    public SatelliteGridFactory() {
        this.sectorGeometryMapCache = new HashMap<String, GridGeometry2D>();
        SatelliteUnits.register();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getAvailableTimes(com.
     * raytheon.uf.common.dataaccess.IDataRequest)
     */
    @Override
    public DataTime[] getAvailableTimes(IGridRequest request)
            throws TimeAgnosticDataException {
        return this.getAvailableTimes(request, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getAvailableTimes(com.
     * raytheon.uf.common.dataaccess.IDataRequest,
     * com.raytheon.uf.common.time.BinOffset)
     */
    @SuppressWarnings("unchecked")
    @Override
    public DataTime[] getAvailableTimes(IGridRequest request,
            BinOffset binOffset) throws TimeAgnosticDataException {
        TimeQueryRequest timeQueryRequest = this.buildTimeQueryRequest(request);
        if ((binOffset == null) == false) {
            timeQueryRequest.setBinOffset(binOffset);
        }

        List<Object> results = null;
        try {
            results = (List<Object>) RequestRouter.route(timeQueryRequest);
        } catch (Exception e) {
            throw new DataRetrievalException(
                    "Failed to retrieve available data times for plugin "
                            + request.getDatatype() + " for request "
                            + request.toString(), e);
        }

        List<DataTime> dataTimes = new ArrayList<DataTime>();
        for (Object result : results) {
            dataTimes.add((DataTime) result);
        }
        Collections.sort(dataTimes);

        return dataTimes.toArray(new DataTime[dataTimes.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getData(com.raytheon.uf
     * .common.dataaccess.IDataRequest, com.raytheon.uf.common.time.DataTime[])
     */
    @Override
    public IGridData[] getData(IGridRequest request, DataTime... times) {

        DbQueryRequest dbQueryRequest = this
                .buildDbQueryRequest(request, times);
        return this.getData(request, dbQueryRequest);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getData(com.raytheon.uf
     * .common.dataaccess.IDataRequest, com.raytheon.uf.common.time.TimeRange)
     */
    @Override
    public IGridData[] getData(IGridRequest request, TimeRange timeRange) {

        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request,
                timeRange);
        return this.getData(request, dbQueryRequest);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.grid.IGridDataFactory#getGeometry(com
     * .raytheon.uf.common.dataaccess.grid.IGridRequest)
     */
    @Override
    public GridGeometry2D getGeometry(IGridRequest request) {
        String satelliteSectorID = this.retrieveSectorID(request);

        if (satelliteSectorID == null) {
            return null;
        }

        /*
         * Has the Geometry for the sector id already been cached?
         */
        synchronized (this.sectorGeometryMapCache) {
            if (this.sectorGeometryMapCache.containsKey(satelliteSectorID)) {
                /*
                 * Return the Geometry from cache.
                 */
                return this.sectorGeometryMapCache.get(satelliteSectorID);
            }
        }

        /*
         * Retrieve the Geometry.
         */
        IGridData[] records = this.getData(request, new DataTime[] {});
        if (records.length <= 0) {
            // No records were found
            return null;
        }
        GridGeometry2D geometry = records[0].getGridGeometry();

        /*
         * Cache the Geometry.
         */
        synchronized (this.sectorGeometryMapCache) {
            this.sectorGeometryMapCache.put(satelliteSectorID, geometry);
        }

        /*
         * Return the Geometry.
         */
        return geometry;
    }

    /**
     * Will either extract the satellite sector id from the request if it has
     * been provided or execute a database query utilizing the information in
     * the request to retrieve the sector id.
     * 
     * @param request
     *            the original grid request
     * @return the satellite sector id
     */
    private String retrieveSectorID(IGridRequest request) {
        /*
         * Determine if the sector id has been included in the request.
         */
        if (request.getIdentifiers().containsKey(FIELD_SECTOR_ID)) {
            return request.getIdentifiers().get(FIELD_SECTOR_ID).toString();
        }

        /*
         * First, retrieve the unique sector id(s) associated with the request.
         * Ideally, there will only be one.
         */
        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request);
        dbQueryRequest.setDistinct(Boolean.TRUE);
        dbQueryRequest.addRequestField(FIELD_SECTOR_ID);

        DbQueryResponse dbQueryResponse = this.executeDbQueryRequest(
                dbQueryRequest, request.toString());

        // Check for no results returned?

        /*
         * Verify that only one sector id has been returned.
         */
        if (dbQueryResponse.getResults().size() > 1) {
            throw new DataRetrievalException(
                    "The provided request parameters refer to more than one geographical location.");
        }

        /*
         * Retrieve the sector id from the results.
         */
        return dbQueryResponse.getResults().get(0).get(FIELD_SECTOR_ID)
                .toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.impl.AbstractDataFactory#
     * getRequiredIdentifiers()
     */
    @Override
    public String[] getRequiredIdentifiers() {
        return null;
    }

    /**
     * Executes the provided DbQueryRequest and returns an array of IGridData
     * 
     * @param request
     *            the original grid request
     * @param dbQueryRequest
     *            the db query request to execute
     * @return an array of IGridData
     */
    /*
     * TODO: add support for StorageRequest (included in the IGridRequest)
     */
    private IGridData[] getData(IGridRequest request,
            DbQueryRequest dbQueryRequest) {
        DbQueryResponse dbQueryResponse = this.executeDbQueryRequest(
                dbQueryRequest, request.toString());

        List<IGridData> gridData = new ArrayList<IGridData>();
        for (Map<String, Object> resultMap : dbQueryResponse.getResults()) {
            if (resultMap.containsKey(null) == false) {
                throw new DataRetrievalException(
                        "The results of the DbQueryRequest do not consist of PluginDataObject objects as expected.");
            }
            if ((resultMap.get(null) instanceof SatelliteRecord) == false) {
                throw new DataRetrievalException(
                        "The PluginDataObject objects returned by the DbQueryRequest are not of type SatelliteRecord as expected.");
            }

            SatelliteRecord satelliteRecord = (SatelliteRecord) resultMap
                    .get(null);

            IDataRecord[] dataRecords = null;

            try {
                dataRecords = PDOUtil.getDataRecords(satelliteRecord);
            } catch (Exception e1) {
                throw new DataRetrievalException(
                        "Failed to retrieve the IDataRecords for PluginDataObject: "
                                + satelliteRecord.toString());
            }

            /*
             * Extract the grid geometry.
             */
            final GridGeometry2D gridGeometry = PDOUtil
                    .retrieveGeometry(satelliteRecord);

            for (IDataRecord dataRecord : dataRecords) {
                DefaultGridData defaultGridData = null;
                try {
                    defaultGridData = this.constructGridDataResponse(request,
                            satelliteRecord, gridGeometry, dataRecord);
                } catch (ParseException e) {
                    throw new DataRetrievalException(
                            "Failed to parse the Unit: "
                                    + satelliteRecord.getUnits(), e);
                }
                gridData.add(defaultGridData);
            }
        }

        return gridData.toArray(new IGridData[gridData.size()]);
    }

    /**
     * Executes the provided DbQueryRequest and returns a DbQueryResponse
     * 
     * @param dbQueryRequest
     *            the DbQueryRequest to execute
     * @param gridRequestString
     *            the original grid request for reporting purposes
     * @return a DbQueryResponse
     */
    private DbQueryResponse executeDbQueryRequest(
            DbQueryRequest dbQueryRequest, String gridRequestString) {
        DbQueryResponse dbQueryResponse = null;

        try {
            dbQueryResponse = (DbQueryResponse) RequestRouter
                    .route(dbQueryRequest);
        } catch (Exception e1) {
            throw new DataRetrievalException(
                    "Unable to complete the DbQueryRequest for request: "
                            + gridRequestString, e1);
        }

        return dbQueryResponse;
    }

    /**
     * Builds a TimeQueryRequest that will be used to retrieve Data Times.
     * 
     * @param request
     *            the original grid request
     * @return a TimeQueryRequest to execute
     */
    /*
     * TODO: maybe this method needs to be a util or a protected method in a
     * superclass so that other data types that utilize a TimeQueryRequest will
     * be able to use this logic?
     */
    private TimeQueryRequest buildTimeQueryRequest(IGridRequest request) {
        TimeQueryRequest timeQueryRequest = new TimeQueryRequest();
        timeQueryRequest.setPluginName(request.getDatatype());
        timeQueryRequest.setQueryTerms(this
                .buildConstraintsFromRequest(request));

        return timeQueryRequest;
    }

    /**
     * Constructs a db query request using the provided data times
     * 
     * @param request
     *            the original grid request
     * @param dataTimes
     *            the data times to include in the request (if any)
     * @return a DbQueryRequest to execute
     */
    private DbQueryRequest buildDbQueryRequest(IGridRequest request,
            DataTime[] dataTimes) {
        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request);
        if (dataTimes.length <= 0) {
            return dbQueryRequest;
        }
        /* Add the DataTime Constraint */
        RequestConstraint requestConstraint = new RequestConstraint();
        requestConstraint.setConstraintType(ConstraintType.IN);
        String[] dataTimeStrings = new String[dataTimes.length];
        int index = 0;
        for (DataTime dataTime : dataTimes) {
            dataTimeStrings[index] = dataTime.toString();
            ++index;
        }
        requestConstraint.setConstraintValueList(dataTimeStrings);
        dbQueryRequest.addConstraint(FIELD_DATATIME, requestConstraint);

        return dbQueryRequest;
    }

    /**
     * Constructs a db request using the provided time range
     * 
     * @param request
     *            the original grid request
     * @param timeRange
     *            the time range to include in the request
     * @return a DbQueryRequest to execute
     */
    private DbQueryRequest buildDbQueryRequest(IGridRequest request,
            TimeRange timeRange) {
        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request);
        /* Add the TimeRange Constraint */
        RequestConstraint requestConstraint = new RequestConstraint();
        requestConstraint.setConstraintType(ConstraintType.BETWEEN);
        String[] dateTimeStrings = new String[] {
                timeRange.getStart().toString(), timeRange.getEnd().toString() };
        requestConstraint.setBetweenValueList(dateTimeStrings);
        dbQueryRequest.addConstraint(FIELD_DATATIME, requestConstraint);

        return dbQueryRequest;
    }

    /**
     * Constructs the base of a db query request using the supplied grid request
     * 
     * @param request
     *            the original grid request
     * @return the base DbQueryRequest
     */
    private DbQueryRequest buildDbQueryRequest(IGridRequest request) {
        DbQueryRequest dbQueryRequest = new DbQueryRequest();
        Map<String, RequestConstraint> constraints = this
                .buildConstraintsFromRequest(request);
        constraints.put(DBQUERY_PLUGIN_NAME_KEY,
                new RequestConstraint(request.getDatatype()));
        dbQueryRequest.setConstraints(constraints);

        return dbQueryRequest;
    }

    /**
     * Builds an IGridData with the information that is supplied
     * 
     * @param request
     *            the original grid request
     * @param satelliteRecord
     *            a record that was retrieved from the database
     * @param gridGeometry
     *            the geometry extracted from the pdo
     * @param dataRecord
     *            the raw data
     * @return the IGridData that was constructed
     * @throws ParseException
     */
    private DefaultGridData constructGridDataResponse(IGridRequest request,
            SatelliteRecord satelliteRecord, GridGeometry2D gridGeometry,
            IDataRecord dataRecord) throws ParseException {
        DefaultGridData defaultGridData = new DefaultGridData(
                DataWrapperUtil.constructArrayWrapper(dataRecord), gridGeometry);
        defaultGridData.setDataTime(satelliteRecord.getDataTime());
        defaultGridData.setParameter(satelliteRecord.getPhysicalElement());
        defaultGridData.setLevel(null);
        // unit
        Unit<?> unit = null;
        if ((satelliteRecord.getUnits() == null) == false) {
            unit = UnitFormat.getUCUMInstance().parseSingleUnit(
                    satelliteRecord.getUnits(), new ParsePosition(0));
        }
        defaultGridData.setUnit(unit);
        defaultGridData.setAttributes(request.getIdentifiers());

        return defaultGridData;
    }

    /**
     * Builds the base constraint map based on the supplied grid request
     * 
     * @param request
     *            the original grid request
     * @return the base constraint map
     */
    /*
     * This method is a candidate for relocation into a utility or superclass if
     * multiple factories will be building a base constraint map using the same
     * technique
     */
    private Map<String, RequestConstraint> buildConstraintsFromRequest(
            IGridRequest request) {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        if ((request.getIdentifiers() == null) == false) {
            Iterator<String> identifiersIterator = request.getIdentifiers()
                    .keySet().iterator();

            while (identifiersIterator.hasNext()) {
                String identifier = identifiersIterator.next();

                constraints.put(identifier, new RequestConstraint(request
                        .getIdentifiers().get(identifier).toString()));
            }
        }
        if ((request.getParameters() == null) == false) {
            RequestConstraint requestConstraint = new RequestConstraint();
            requestConstraint.setConstraintType(ConstraintType.IN);
            requestConstraint.setConstraintValueList(request.getParameters());
            constraints.put(FIELD_PYHSICAL_ELEMENT, requestConstraint);
        }

        return constraints;
    }
}