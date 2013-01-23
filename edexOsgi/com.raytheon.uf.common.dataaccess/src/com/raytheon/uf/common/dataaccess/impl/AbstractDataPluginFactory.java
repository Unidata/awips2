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

import com.raytheon.uf.common.dataaccess.IData;
import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2013            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractDataPluginFactory<R extends IDataRequest<D>, D extends IData>
        extends AbstractDataFactory {

    protected static final String FIELD_DATATIME = "dataTime";

    protected static final String DBQUERY_PLUGIN_NAME_KEY = "pluginName";

    public DataTime[] getAvailableTimes(R request)
            throws TimeAgnosticDataException {
        return this.getAvailableTimes(request, null);
    }

    @SuppressWarnings("unchecked")
    public DataTime[] getAvailableTimes(R request,
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

    /**
     * Builds a TimeQueryRequest that will be used to retrieve Data Times.
     * 
     * @param request
     *            the original grid request
     * @return a TimeQueryRequest to execute
     */
    protected TimeQueryRequest buildTimeQueryRequest(R request) {
        TimeQueryRequest timeQueryRequest = new TimeQueryRequest();
        timeQueryRequest.setPluginName(request.getDatatype());
        timeQueryRequest.setQueryTerms(this
                .buildConstraintsFromRequest(request));
    
        return timeQueryRequest;
    }

    public D[] getData(R request, DataTime... times) {
        DbQueryRequest dbQueryRequest = this
                .buildDbQueryRequest(request, times);
        return this.getData(request, dbQueryRequest);
    }

    public D[] getData(R request, TimeRange timeRange) {
        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request,
                timeRange);
        return this.getData(request, dbQueryRequest);
    }

    protected D[] getData(R request, DbQueryRequest dbQueryRequest) {
        DbQueryResponse dbQueryResponse = executeDbQueryRequest(dbQueryRequest,
                request.toString());
        return getData(request, dbQueryResponse);
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
    protected DbQueryResponse executeDbQueryRequest(
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
     * Constructs a db query request using the provided data times
     * 
     * @param request
     *            the original grid request
     * @param dataTimes
     *            the data times to include in the request (if any)
     * @return a DbQueryRequest to execute
     */
    protected DbQueryRequest buildDbQueryRequest(R request, DataTime[] dataTimes) {
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
    protected DbQueryRequest buildDbQueryRequest(R request, TimeRange timeRange) {
        DbQueryRequest dbQueryRequest = this.buildDbQueryRequest(request);
        /* Add the TimeRange Constraint */
        RequestConstraint requestConstraint = new RequestConstraint();
        requestConstraint.setConstraintType(ConstraintType.BETWEEN);
        String[] dateTimeStrings = new String[] {
                timeRange.getStart().toString(), timeRange.getEnd().toString() };
        requestConstraint.setBetweenValueList(dateTimeStrings);
        // TODO what should this do with forecast products?
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
    protected DbQueryRequest buildDbQueryRequest(R request) {
        DbQueryRequest dbQueryRequest = new DbQueryRequest();
        Map<String, RequestConstraint> constraints = this
                .buildConstraintsFromRequest(request);
        constraints.put(DBQUERY_PLUGIN_NAME_KEY,
                new RequestConstraint(request.getDatatype()));
        dbQueryRequest.setConstraints(constraints);

        return dbQueryRequest;
    }

    protected abstract Map<String, RequestConstraint> buildConstraintsFromRequest(
            R request);

    protected abstract D[] getData(R request, DbQueryResponse dbQueryResponse);

}
