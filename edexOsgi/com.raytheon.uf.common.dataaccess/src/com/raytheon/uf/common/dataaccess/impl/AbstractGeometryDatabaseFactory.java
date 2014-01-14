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
package com.raytheon.uf.common.dataaccess.impl;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataaccess.IDataFactory;
import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.util.DatabaseQueryUtil;
import com.raytheon.uf.common.dataaccess.util.DatabaseQueryUtil.QUERY_MODE;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Abstracts the retrieval of geometry data by running queries directly against
 * the database. Maybe this class could also be further abstracted and extended
 * to Grid data types?
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2013            bkowal     Initial creation
 * Feb 14, 2013 1614       bsteffen    Refactor data access framework to use
 *                                     single request.
 * Jan 14, 2014 2667       mnash       Remove getGridData methods
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public abstract class AbstractGeometryDatabaseFactory extends
        AbstractDataFactory implements IDataFactory {

    /*
     * for now, we will assume that we will always be executing sql queries. If
     * this assumption ever becomes invalid, the type of query that will be
     * executed could be passed to the constructor or methods could be
     * overridden.
     */
    private static final QUERY_MODE queryMode = QUERY_MODE.MODE_SQLQUERY;

    private String databaseName;

    private String[] requiredIdentifiers;

    /**
     * Constructor
     * 
     * @param databaseName
     *            the name of the database to execute queries against
     * @param requiredIdentifiers
     *            the identifiers that need to be included in the request
     *            (ifdef)
     */
    public AbstractGeometryDatabaseFactory(String databaseName,
            String[] requiredIdentifiers) {
        this.databaseName = databaseName;
        this.requiredIdentifiers = requiredIdentifiers;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getAvailableTimes(com.
     * raytheon.uf.common.dataaccess.IDataRequest)
     */
    @Override
    public DataTime[] getAvailableTimes(IDataRequest request)
            throws TimeAgnosticDataException {
        this.validateRequest(request);
        return this.executeTimeQuery(this.assembleGetTimes(request), request);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getAvailableTimes(com.
     * raytheon.uf.common.dataaccess.IDataRequest,
     * com.raytheon.uf.common.time.BinOffset)
     */
    @Override
    public DataTime[] getAvailableTimes(IDataRequest request,
            BinOffset binOffset) throws TimeAgnosticDataException {
        this.validateRequest(request);
        return this.executeTimeQuery(this.assembleGetTimes(request, binOffset),
                request);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getData(com.raytheon.uf
     * .common.dataaccess.IDataRequest, com.raytheon.uf.common.time.DataTime[])
     */
    @Override
    public IGeometryData[] getGeometryData(IDataRequest request,
            DataTime... times) {
        this.validateRequest(request);
        return this.executeDataQuery(this.assembleGetData(request, times),
                request);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getData(com.raytheon.uf
     * .common.dataaccess.IDataRequest, com.raytheon.uf.common.time.TimeRange)
     */
    @Override
    public IGeometryData[] getGeometryData(IDataRequest request,
            TimeRange timeRange) {
        this.validateRequest(request);
        return this.executeDataQuery(this.assembleGetData(request, timeRange),
                request);
    }

    /**
     * Runs a query to retrieve Data Times from the database.
     * 
     * @param query
     *            the query to execute
     * @param request
     *            the original request that we are processing
     * @return an array of DataTimes
     */
    protected final DataTime[] executeTimeQuery(String query,
            IDataRequest request) {
        List<Object[]> results = this.executeQuery(query, request);
        List<DataTime> dataTimes = new ArrayList<DataTime>();
        for (Object[] objects : results) {
            /*
             * verify that the object is one of the data types we are expecting.
             */
            if (objects[0] instanceof Timestamp) {
                dataTimes.add(new DataTime((Timestamp) objects[0]));
            } else if (objects[0] instanceof DataTime) {
                dataTimes.add((DataTime) objects[0]);
            } else {
                throw new DataRetrievalException(
                        "Unrecognized temporal object: "
                                + objects[0].getClass().getName());
            }
        }

        Collections.sort(dataTimes);
        return dataTimes.toArray(new DataTime[dataTimes.size()]);
    }

    /**
     * Runs a query to retrieve IGeometryData from the database.
     * 
     * @param query
     *            the query to execute
     * @param request
     *            the original request that we are processing
     * @return an array of IGeometryData
     */
    protected final IGeometryData[] executeDataQuery(String query,
            IDataRequest request) {
        List<Object[]> results = this.executeQuery(query, request);
        return this.makeGeometries(results, request.getParameters(),
                request.getIdentifiers());
    }

    /**
     * Runs a query to retrieve raw data from the database.
     * 
     * @param query
     *            the query to execute
     * @param request
     *            the original request that we are processing
     * @return the raw data retrieved from the database
     */
    protected final List<Object[]> executeQuery(String query,
            IDataRequest request) {
        return DatabaseQueryUtil.executeDatabaseQuery(queryMode, query,
                this.databaseName, request.getDatatype());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.geom.IGeometryDataFactory#
     * getAvailableLocationNames
     * (com.raytheon.uf.common.dataaccess.geom.IDataRequest)
     */
    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        this.validateRequest(request);
        List<Object[]> results = this.executeQuery(
                this.assembleGetAvailableLocationNames(request), request);
        List<String> locations = new ArrayList<String>();
        for (Object[] objects : results) {
            locations.add((String) objects[0]);
        }

        Collections.sort(locations, String.CASE_INSENSITIVE_ORDER);
        return locations.toArray(new String[locations.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.impl.AbstractDataFactory#
     * getRequiredIdentifiers()
     */
    @Override
    public String[] getRequiredIdentifiers() {
        return this.requiredIdentifiers;
    }

    /*
     * invoked to build the queries that will be executed.
     */

    /**
     * Builds a query that will be used to retrieve time from the database based
     * on the provided request.
     * 
     * @param request
     *            the original request that we are processing
     * @return the query
     */
    protected abstract String assembleGetTimes(IDataRequest request);

    /**
     * Builds a query that will be used to retrieve time from the database based
     * on the provided request and the provided BinOffset. Necessary?
     * 
     * @param request
     *            the original request that we are processing
     * @param binOffset
     *            the BinOffset to apply to the retrieved DataTimes
     * @return the query
     */
    protected abstract String assembleGetTimes(IDataRequest request,
            BinOffset binOffset);

    /**
     * Builds a query used to retrieve data from the database based on the
     * provided request and a list of DataTimes.
     * 
     * @param request
     *            the original request that we are processing
     * @param times
     *            DataTimes to use when building the query; will most likely
     *            manifest as constraints
     * @return the query
     */
    protected abstract String assembleGetData(IDataRequest request,
            DataTime... times);

    /**
     * Builds a query used to retrieve data from the database based on the
     * provided request and the specified TimeRange.
     * 
     * @param request
     *            the original request that we are processing
     * @param timeRange
     *            a TimeRange to use when building the query; will most likely
     *            manifest as a BETWEEN constraint
     * @return the query
     */
    protected abstract String assembleGetData(IDataRequest request,
            TimeRange timeRange);

    /**
     * Builds a query used to retrieve location information from the database
     * based on the provided request
     * 
     * @param request
     *            the original request that we are processing
     * @return the query
     */
    protected abstract String assembleGetAvailableLocationNames(
            IDataRequest request);

    /**
     * Builds the data objects that will be returned by calls to getData() on
     * the factory
     * 
     * @param serverResult
     *            the results from the query run on the server
     * @param paramNames
     *            the names of the parameters that were requested
     * @param identifiers
     *            the identifiers from the data request
     * @return the IGeometryData based on the results of the query
     */
    protected IGeometryData[] makeGeometries(List<Object[]> serverResult,
            String[] paramNames, Map<String, Object> identifiers) {
        List<IGeometryData> resultList = new ArrayList<IGeometryData>();
        Map<String, Object> attrs = Collections.unmodifiableMap(identifiers);

        // loop over each db row
        for (Object[] row : serverResult) {
            resultList.add(this.makeGeometry(row, paramNames, attrs));
        }

        return resultList.toArray(new DefaultGeometryData[resultList.size()]);
    }

    /**
     * Constructs a single IGeometryData
     * 
     * @param data
     *            the raw data associated with a single row retrieved from the
     *            database
     * @param paramNames
     *            the parameters specified in the original IDataRequest
     * @param attrs
     *            the identifiers specified in the original IDataRequest
     * @return the constructed IGeometryData
     */
    protected abstract IGeometryData makeGeometry(Object[] data,
            String[] paramNames, Map<String, Object> attrs);

    /**
     * Constructs a DefaultGeometryData based on the provided information
     * 
     * @param time
     *            the provided DataTime
     * @param level
     *            the provided Level
     * @param geometry
     *            the provided Geometry
     * @param locationName
     *            the provided Location
     * @param attributes
     *            the identifiers specified in the original IDataRequest
     * @param paramNames
     *            the parameters specified in the original IDataRequest
     * @return the constructed DefaultGeometryData
     */
    protected DefaultGeometryData buildGeometryData(DataTime time, Level level,
            Geometry geometry, String locationName,
            Map<String, Object> attributes, String[] paramNames) {
        return this.buildGeometryData(time, level, geometry, locationName,
                attributes, Integer.MAX_VALUE, null, paramNames);
    }

    /**
     * Constructs a DefaultGeometryData based on the provided information
     * 
     * @param time
     *            the provided DataTime
     * @param level
     *            the provided Level
     * @param geometry
     *            the provided Geometry
     * @param locationName
     *            the provided Location
     * @param attributes
     *            identifiers specified in the original IDataRequest
     * @param dataIndex
     *            a numerical index indicating where user-specified parameters
     *            may start in the provided row of raw data
     * @param data
     *            a row of row data retrieved from the database; all
     *            user-specified parameters are extracted from it and added to
     *            the DefaultGeometryData using the addData method
     * @param paramNames
     *            the parameters specified in the original IDataRequest
     * @return the constructed DefaultGeometryData
     */
    protected DefaultGeometryData buildGeometryData(DataTime time, Level level,
            Geometry geometry, String locationName,
            Map<String, Object> attributes, int dataIndex, Object[] data,
            String[] paramNames) {
        DefaultGeometryData geometryData = new DefaultGeometryData();
        geometryData.setDataTime(time);
        geometryData.setLevel(level);
        geometryData.setGeometry(geometry);
        geometryData.setLocationName(locationName);
        geometryData.setAttributes(attributes);
        if ((data == null) == false && data.length > dataIndex) {
            for (int i = dataIndex; i < data.length; i++) {
                String name = paramNames[i - dataIndex];
                geometryData.addData(name, data[i]);
            }
        }

        return geometryData;
    }
}