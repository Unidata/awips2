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
package com.raytheon.uf.edex.database.handlers;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.OrderField.ResultOrder;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Handler to query available times that meet certain constraints. If a request
 * contains,
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 05, 2011            njensen     Initial creation
 * Mar 24, 2014    2941    mpduff      Sort data before returning it.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TimeQueryHandler implements IRequestHandler<TimeQueryRequest> {

    private static final String DATA_TIME = "dataTime";

    private static final String REF_TIME = DATA_TIME + ".refTime";

    @Override
    public List<DataTime> handleRequest(TimeQueryRequest request)
            throws Exception {
        // plugin name is sometimes sent over due to viz API
        Map<String, RequestConstraint> map = request.getQueryTerms();
        if (map.containsKey("pluginName")) {
            map.remove("pluginName");
        }

        // Simulated Date is the date set in the CAVE calling this
        if (request.getSimDate() != null) {
            RequestConstraint timeConstraint = new RequestConstraint();
            timeConstraint.setConstraintType(ConstraintType.LESS_THAN);
            timeConstraint
                    .setConstraintValue(new DataTime(request.getSimDate())
                            .toString());
            map.put(REF_TIME, timeConstraint);
        }

        String database = PluginFactory.getInstance().getDatabase(
                request.getPluginName());
        String classname = PluginFactory.getInstance()
                .getPluginRecordClassName(request.getPluginName());
        List<DataTime> times = null;

        CoreDao dao = new CoreDao(DaoConfig.forClass(database, classname));
        BinOffset binOffset = request.getBinOffset();
        if (binOffset != null) {
            // If the resource will potentially have large numbers of times per
            // bin, it can be faster to do a series of max queries rather than
            // getting all times
            DatabaseQuery query = buildQuery(classname, map, true);
            List<DataTime> latestTime = runQuery(dao, query);
            if (!map.containsKey(REF_TIME)) {
                RequestConstraint timeRC = new RequestConstraint(null,
                        ConstraintType.LESS_THAN);
                map.put(REF_TIME, timeRC);
            }
            times = new ArrayList<DataTime>(50);
            while (latestTime != null && latestTime.size() != 0) {
                DataTime normalTime = binOffset.getNormalizedTime(latestTime
                        .get(0));
                times.add(normalTime);
                Date date = binOffset.getTimeRange(normalTime).getStart();
                map.get(REF_TIME).setConstraintValue(
                        new DataTime(date).toString());
                query = buildQuery(classname, map, true);
                latestTime = runQuery(dao, query);
            }
        } else {
            DatabaseQuery query = buildQuery(classname, map,
                    request.isMaxQuery());
            times = runQuery(dao, query);
        }

        return times;
    }

    /**
     * Builds a constrained database query against the table tied to the
     * classname
     * 
     * @param classname
     *            the classname tied to the table
     * @param map
     *            the constraints for the query
     * @param max
     *            whether to return only the max time or all times
     * @return the query object
     */
    private DatabaseQuery buildQuery(String classname,
            Map<String, RequestConstraint> map, boolean max) {
        DatabaseQuery query = new DatabaseQuery(classname);

        if (max) {
            query.addReturnedField(REF_TIME, classname);
            query.setMaxResults(1);
            query.addOrder(REF_TIME, ResultOrder.DESC);
        } else {
            query.addDistinctParameter(DATA_TIME);
        }

        for (String key : map.keySet()) {
            RequestConstraint constraint = map.get(key);
            query.addQueryParam(key, constraint.getConstraintValue(),
                    constraint.getConstraintType().getOperand(), classname);
        }

        // System.out.println("TimeQuery: " + query.createHQLQuery());
        return query;
    }

    /**
     * Runs a database query with a dao
     * 
     * @param dao
     *            the dao
     * @param query
     *            the query
     * @return the times returned by the query
     * @throws Exception
     */
    private List<DataTime> runQuery(CoreDao dao, DatabaseQuery query)
            throws Exception {
        List<?> results = dao.queryByCriteria(query);
        List<DataTime> times = new ArrayList<DataTime>(results.size());
        for (Object o : results) {
            if (o != null) {
                if (o instanceof DataTime) {
                    times.add((DataTime) o);
                } else if (o instanceof Timestamp) {
                    times.add(new DataTime((Timestamp) o));
                } else {
                    throw new Exception(
                            "Time query returned unexpected result: "
                                    + o.getClass().getName());
                }
            }
        }

        Collections.sort(times);
        return times;
    }

}
