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

package com.raytheon.uf.viz.core.catalog;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * 
 * An interface to the catalog capability.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/20/2006   #7         brockwoo    Initial creation
 * 08/19/2009   2586       rjpeter     Added error handling
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class CatalogQuery {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CatalogQuery.class);

    /**
     * Sets a query to return only distinct values from the index.
     */
    public static final int DISTINCTVALUES = 0;

    /**
     * Sets a query to return all values from the index.
     */
    public static final int ALLVALUES = 1;

    private static final int REQUESTTIMEOUT = 60000;

    private static final int LATEST_TIME_QUERY_TIMEOUT = 90000;

    public static enum Mode {
        DISTINCT, MAX
    };

    /**
     * Will search for the fieldName within the catalog that meets the query
     * terms specified by the hash map. Will either return the distinct or all
     * values from the catalog.
     * 
     * @param fieldName
     *            The fieldname to search for
     * @param queryTerms
     *            A hashmap containing the parameter name/value to search for
     * @return A string array containing the found values or null if nothing is
     *         found
     * @throws VizException
     */
    public static String[] performQuery(String fieldName,
            Map<String, RequestConstraint> queryTerms) throws VizException {
        String query = assembleQuery(fieldName, queryTerms, Mode.DISTINCT);
        // System.out.println("Query: " + query);
        Connector value = Connector.getInstance();
        Object[] catalogValues = value.connect(query, null, REQUESTTIMEOUT);
        if (catalogValues == null || catalogValues.length == 0)
            return null;
        return (String[]) catalogValues[0];
    }

    /**
     * Will search for the fieldName within the catalog that meets the query
     * terms specified by the hash map. Will return the max value
     * 
     * @param fieldName
     *            The fieldname to search for
     * @param queryTerms
     *            A hashmap containing the parameter name/value to search for
     * @return A string array containing the found values or null if nothing is
     *         found
     * @throws VizException
     */
    @Deprecated
    public static String[] performMaxQuery(String fieldName,
            Map<String, RequestConstraint> queryTerms) throws VizException {
        String query = assembleQuery(fieldName, queryTerms, Mode.MAX);
        Connector value = Connector.getInstance();
        Object[] catalogValues = value.connect(query, null, REQUESTTIMEOUT);

        if (catalogValues == null || catalogValues.length == 0)
            return null;

        String[] msg = (String[]) catalogValues[0];

        return msg;
    }

    private static String assembleQuery(String fieldName,
            Map<String, RequestConstraint> queryTerms, Mode mode)
            throws VizException {
        HashMap<String, RequestConstraint> map = new HashMap<String, RequestConstraint>(
                queryTerms);
        if (mode == Mode.DISTINCT) {
            map.put("distinctFieldName", new RequestConstraint(fieldName));
        } else if (mode == Mode.MAX) {
            map.put("maxName", new RequestConstraint(fieldName));
        } else {
            throw new IllegalArgumentException("Unsupported mode: " + mode);
        }

        return ScriptCreator.createScript(map, 1, "catalog");
    }

    public static ResponseMessageCatalog performQuery(
            Collection<String> uriCollection) throws VizException {
        StringBuffer uriList = new StringBuffer();
        for (String dataURI : uriCollection) {
            if (uriList.length() > 0) {
                uriList.append(',');
            }
            uriList.append(dataURI);
        }

        Map<String, RequestConstraint> vals = new HashMap<String, RequestConstraint>();
        vals.put("uriList", new RequestConstraint(uriList.toString()));
        vals.put("pluginName", new RequestConstraint("satellite")); // surrogate
        // value
        // that gets
        // the
        // default
        // template
        String script = ScriptCreator.createScript(vals, 99999, "latestTime");

        Connector value = Connector.getInstance();
        System.out.println("Starting LatestTimeQuery...");
        long t0 = System.currentTimeMillis();
        Message response = value.connectMessage(script.toString(), null,
                LATEST_TIME_QUERY_TIMEOUT);
        long t = System.currentTimeMillis() - t0;
        System.out.println("LatestTimeQuery took " + t + " ms");

        AbstractResponseMessage[] respMessages = response.getBody()
                .getResponses();

        ResponseMessageCatalog rmc = null;

        if (respMessages.length > 0) {
            if (respMessages[0] instanceof ResponseMessageCatalog) {
                rmc = (ResponseMessageCatalog) respMessages[0];
            } else if (respMessages[0] instanceof ResponseMessageError) {
                ResponseMessageError error = (ResponseMessageError) respMessages[0];
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Error retrieving latest time data ["
                                + error.getErrorMsg() + "]");
            } else {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Error retrieving latest time data, received unhandled ResponseMessage from server ["
                                        + respMessages[0].getClass() + "]");
            }
        } else {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error retrieving latest time data, received invalid response from server");
        }

        return rmc;
    }

    /**
     * Queries for available times based on the constraints
     * 
     * @param pluginName
     *            the plugin to query
     * @param constraintMap
     *            the constraints to limit the query
     * @param max
     *            whether or not the max time should be requested, or all times
     * @param binOffset
     *            the bin offset to apply to the returned times, or null if it's
     *            not applicable
     * @return the available times that meet the constraints
     * @throws VizException
     */
    public static TimeQueryRequest getTimeQuery(
            Map<String, RequestConstraint> constraintMap, boolean max,
            BinOffset binOffset) throws VizException {
        RequestConstraint plugin = constraintMap.get("pluginName");

        if (plugin == null) {
            throw new VizException("pluginName must be provided to query times");
        }

        String pluginName = plugin.getConstraintValue();
        TimeQueryRequest req = new TimeQueryRequest();
        req.setMaxQuery(max);
        req.setPluginName(pluginName);
        req.setBinOffset(binOffset);
        req.setSimDate(SimulatedTime.getSystemTime().getTime());
        req.setQueryTerms(constraintMap);

        return req;
    }

    /**
     * Queries for available times based on the constraints
     * 
     * @param constraintMap
     *            the constraints to limit the query. This must include a
     *            constraint named pluginName that specifies the plugin name to
     *            query.
     * @param max
     *            whether or not the max time should be requested, or all times
     * @param binOffset
     *            the bin offset to apply to the returned times, or null if it's
     *            not applicable
     * @return the available times that meet the constraints
     * @throws VizException
     */
    @SuppressWarnings("unchecked")
    public static DataTime[] performTimeQuery(
            Map<String, RequestConstraint> constraintMap, boolean max,
            BinOffset binOffset) throws VizException {
        Object result = ThriftClient.sendRequest(getTimeQuery(constraintMap,
                max, binOffset));
        return ((List<DataTime>) result).toArray(new DataTime[0]);
    }
}
