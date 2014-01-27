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

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
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
 * 09/09/2013   2277       mschenke    Got rid of ScriptCreator references
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class CatalogQuery {

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
        DbQueryRequest request = new DbQueryRequest(queryTerms);
        request.addRequestField(fieldName);
        request.setDistinct(true);
        DbQueryResponse response = (DbQueryResponse) ThriftClient
                .sendRequest(request);
        Object[] results = response.getFieldObjects(fieldName, Object.class);
        String[] fieldResults = new String[results.length];
        for (int i = 0; i < fieldResults.length; ++i) {
            fieldResults[i] = String.valueOf(results[i]);
        }
        return fieldResults;
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
    public static DataTime[] performTimeQuery(
            Map<String, RequestConstraint> constraintMap, boolean max,
            BinOffset binOffset) throws VizException {
        RequestConstraint plugin = constraintMap
                .get(PluginDataObject.PLUGIN_NAME_ID);

        if (plugin == null) {
            throw new VizException("pluginName must be provided to query times");
        }

        String pluginName = plugin.getConstraintValue();
        TimeQueryRequest req = new TimeQueryRequest();
        req.setMaxQuery(max);
        req.setPluginName(pluginName);
        req.setBinOffset(binOffset);
        req.setQueryTerms(constraintMap);

        List<?> result = (List<?>) ThriftClient.sendRequest(req);
        return result.toArray(new DataTime[0]);
    }

}
