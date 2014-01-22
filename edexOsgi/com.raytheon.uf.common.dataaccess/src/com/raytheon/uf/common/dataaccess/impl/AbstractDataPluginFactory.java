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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.dataaccess.exception.UnsupportedOutputTypeException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * An abstract factory for getting data from plugins that use PluginDataObject.
 * *
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 17, 2013           bsteffen    Initial creation
 * Feb 14, 2013  1614     bsteffen    Refactor data access framework to use
 *                                    single request.
 * Nov 26, 2013  2537     bsteffen    Fix NPEs for dataTimes and timeRange requests.
 * Jan 14, 2014  2667     mnash       Change getGridData and getGeometryData methods
 *                                    to throw exception by default
 * Jan 21, 2014  2667     bclement    changed timeRange buildDbQueryRequest method to query against valid times
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractDataPluginFactory extends AbstractDataFactory {

    protected static final String FIELD_DATATIME = "dataTime";

    protected static final String FIELD_VALID_START = FIELD_DATATIME
            + ".validPeriod.start";

    protected static final String FIELD_VALID_END = FIELD_DATATIME
            + ".validPeriod.end";

    protected static final String DBQUERY_PLUGIN_NAME_KEY = "pluginName";

    public DataTime[] getAvailableTimes(IDataRequest request)
            throws TimeAgnosticDataException {
        return this.getAvailableTimes(request, null);
    }

    @SuppressWarnings("unchecked")
    public DataTime[] getAvailableTimes(IDataRequest request,
            BinOffset binOffset) throws TimeAgnosticDataException {
        validateRequest(request);
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

    public IGeometryData[] getGeometryData(IDataRequest request,
            DataTime... times) {
        validateRequest(request);
        DbQueryRequest dbQueryRequest = this
                .buildDbQueryRequest(request, times);
        DbQueryResponse dbQueryResponse = executeDbQueryRequest(dbQueryRequest,
                request.toString());
        return getGeometryData(request, dbQueryResponse);
    }

    public IGeometryData[] getGeometryData(IDataRequest request,
            TimeRange timeRange) {
        validateRequest(request);
        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request,
                timeRange);
        DbQueryResponse dbQueryResponse = executeDbQueryRequest(dbQueryRequest,
                request.toString());
        return getGeometryData(request, dbQueryResponse);
    }

    public IGridData[] getGridData(IDataRequest request, DataTime... times) {
        validateRequest(request);
        DbQueryRequest dbQueryRequest = this
                .buildDbQueryRequest(request, times);
        DbQueryResponse dbQueryResponse = executeDbQueryRequest(dbQueryRequest,
                request.toString());
        return getGridData(request, dbQueryResponse);
    }

    public IGridData[] getGridData(IDataRequest request, TimeRange timeRange) {
        validateRequest(request);
        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request,
                timeRange);
        DbQueryResponse dbQueryResponse = executeDbQueryRequest(dbQueryRequest,
                request.toString());
        return getGridData(request, dbQueryResponse);
    }

    public String[] getAvailableLocationNames(IDataRequest request,
            String locationColumn) {
        validateRequest(request);
        DbQueryRequest dbQueryRequest = buildDbQueryRequest(request);
        dbQueryRequest.setDistinct(Boolean.TRUE);
        dbQueryRequest.addRequestField(locationColumn);

        DbQueryResponse dbQueryResponse = this.executeDbQueryRequest(
                dbQueryRequest, request.toString());
        List<String> locationNames = new ArrayList<String>();
        for (Map<String, Object> result : dbQueryResponse.getResults()) {
            locationNames.add(result.get(locationColumn).toString());
        }
        return locationNames.toArray(new String[0]);
    }

    /**
     * Builds a TimeQueryRequest that will be used to retrieve Data Times.
     * 
     * @param request
     *            the original grid request
     * @return a TimeQueryRequest to execute
     */
    protected TimeQueryRequest buildTimeQueryRequest(IDataRequest request) {
        TimeQueryRequest timeQueryRequest = new TimeQueryRequest();
        timeQueryRequest.setPluginName(request.getDatatype());
        timeQueryRequest.setQueryTerms(this
                .buildConstraintsFromRequest(request));

        return timeQueryRequest;
    }

    /**
     * Executes the provided DbQueryRequest and returns a DbQueryResponse
     * 
     * @param dbQueryRequest
     *            the DbQueryRequest to execute
     * @param requestString
     *            the original request for reporting purposes
     * @return a DbQueryResponse
     */
    protected DbQueryResponse executeDbQueryRequest(
            DbQueryRequest dbQueryRequest, String requestString) {
        DbQueryResponse dbQueryResponse = null;

        try {
            dbQueryResponse = (DbQueryResponse) RequestRouter
                    .route(dbQueryRequest);
        } catch (Exception e1) {
            throw new DataRetrievalException(
                    "Unable to complete the DbQueryRequest for request: "
                            + requestString, e1);
        }

        return dbQueryResponse;
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
    protected DbQueryRequest buildDbQueryRequest(IDataRequest request,
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
            dataTimeStrings[index] = String.valueOf(dataTime);
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
    protected DbQueryRequest buildDbQueryRequest(IDataRequest request,
            TimeRange timeRange) {
        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request);
        /* Add the TimeRange Constraint */
        if (timeRange != null) {
            RequestConstraint afterReqStart = new RequestConstraint(timeRange
                    .getStart().toString(), ConstraintType.GREATER_THAN_EQUALS);
            RequestConstraint beforeReqEnd = new RequestConstraint(timeRange
                    .getEnd().toString(), ConstraintType.LESS_THAN_EQUALS);

            dbQueryRequest.addConstraint(FIELD_VALID_START, afterReqStart);
            dbQueryRequest.addConstraint(FIELD_VALID_END, beforeReqEnd);
        }
        return dbQueryRequest;
    }

    /**
     * Constructs the base of a db query request using the supplied grid request
     * 
     * @param request
     *            the original grid request
     * @return the base DbQueryRequest
     */
    protected DbQueryRequest buildDbQueryRequest(IDataRequest request) {
        DbQueryRequest dbQueryRequest = new DbQueryRequest();
        Map<String, RequestConstraint> constraints = this
                .buildConstraintsFromRequest(request);
        constraints.put(DBQUERY_PLUGIN_NAME_KEY,
                new RequestConstraint(request.getDatatype()));
        dbQueryRequest.setConstraints(constraints);

        return dbQueryRequest;
    }

    protected IGridData[] getGridData(IDataRequest request,
            DbQueryResponse dbQueryResponse) {
        throw new UnsupportedOutputTypeException(request.getDatatype(), "grid");
    }

    protected IGeometryData[] getGeometryData(IDataRequest request,
            DbQueryResponse dbQueryResponse) {
        throw new UnsupportedOutputTypeException(request.getDatatype(),
                "geometry");
    }

    protected abstract Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request);

}
