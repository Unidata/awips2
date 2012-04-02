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
package com.raytheon.uf.viz.derivparam.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequestSet;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequestSet;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponseSet;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter;
import com.raytheon.uf.viz.core.datastructure.VizDataCubeException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode.Dependency;

/**
 * Abstract data cube adapter for standard data type that uses derived
 * parameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractDataCubeAdapter implements IDataCubeAdapter {

    private String[] supportedPlugins;

    protected AbstractDataCubeAdapter(String[] supportedPlugins) {
        this.supportedPlugins = supportedPlugins;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getSupportedPlugins
     * ()
     */
    @Override
    public String[] getSupportedPlugins() {
        return supportedPlugins;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#timeQuery(java
     * .util.List)
     */
    @Override
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws VizException {
        int mapSize = (int) (requests.size() * 1) + 1;
        Map<AbstractRequestableLevelNode, Set<DataTime>> cache = new HashMap<AbstractRequestableLevelNode, Set<DataTime>>(
                mapSize);
        LinkedHashMap<AbstractRequestableLevelNode, TimeQueryRequest> queries = new LinkedHashMap<AbstractRequestableLevelNode, TimeQueryRequest>(
                mapSize);

        for (TimeQueryRequest request : requests) {
            List<AbstractRequestableLevelNode> requestNodes = evaluateRequestConstraints(request
                    .getQueryTerms());
            // pull out time queries and bulk submit
            for (AbstractRequestableLevelNode requestNode : requestNodes) {
                getTimeQuery(request, requestNode, false, queries, cache, null);
            }
        }

        // set the results back into the cache's
        TimeQueryRequestSet reqSet = new TimeQueryRequestSet();
        reqSet.setRequests(queries.values().toArray(
                new TimeQueryRequest[queries.size()]));
        @SuppressWarnings("unchecked")
        List<List<DataTime>> qResponses = (List<List<DataTime>>) ThriftClient
                .sendRequest(reqSet);
        int index = 0;
        for (AbstractRequestableLevelNode node : queries.keySet()) {
            // put results into cache
            node.setTimeQueryResults(false, qResponses.get(index++), cache,
                    null);
        }
        List<List<DataTime>> finalResponse = new ArrayList<List<DataTime>>(
                requests.size());
        for (TimeQueryRequest request : requests) {
            List<AbstractRequestableLevelNode> requestNodes = evaluateRequestConstraints(request
                    .getQueryTerms());
            // pull the actual results from the cache
            Set<DataTime> results = new HashSet<DataTime>(64);
            for (AbstractRequestableLevelNode requestNode : requestNodes) {
                Set<DataTime> times = requestNode.timeQuery(request, false,
                        cache, null);
                if (times == AbstractRequestableLevelNode.TIME_AGNOSTIC) {
                    // TODO: include time agnostic query in main bulk query as
                    // each pressure level will cause a separate query
                    List<DataTime> temp = timeAgnosticQuery(request
                            .getQueryTerms());
                    if (temp != null) {
                        results.addAll(temp);
                    }
                    break;
                } else {
                    results.addAll(times);
                }
            }
            if (!request.isMaxQuery() || results.isEmpty()) {
                finalResponse.add(new ArrayList<DataTime>(results));
            } else {
                ArrayList<DataTime> response = new ArrayList<DataTime>(results);
                Collections.sort(response);
                finalResponse
                        .add(Arrays.asList(response.get(response.size() - 1)));
            }
        }
        return finalResponse;
    }

    protected void getTimeQuery(
            TimeQueryRequest originalRequest,
            AbstractRequestableLevelNode req,
            boolean latestOnly,
            LinkedHashMap<AbstractRequestableLevelNode, TimeQueryRequest> queries,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        List<Dependency> depends = req.getDependencies();
        if (depends.isEmpty()) {
            // is source node
            TimeQueryRequest myQ = req.getTimeQuery(originalRequest,
                    latestOnly, cache, latestOnlyCache);
            if (myQ != null) {
                // no need to merge timeQueries
                queries.put(req, myQ);
            }
        } else {
            for (Dependency dep : depends) {
                // TODO: Optimize dTime/fTime to use bulk query mechanism,
                // small case that is a not easy to get right with a bulk
                // query
                if (dep.timeOffset == 0 || !latestOnly) {
                    getTimeQuery(originalRequest, dep.node, latestOnly,
                            queries, cache, latestOnlyCache);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getPoints(java
     * .lang.String, java.lang.String[], java.util.Map)
     */
    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            Map<String, RequestConstraint> queryParams) throws VizException {
        // TODO Someday we should put objective analysis code
        // into this area
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getPoints(java
     * .lang.String, java.lang.String[], java.lang.String, java.util.Map)
     */
    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            String levelKey, Map<String, RequestConstraint> queryParams)
            throws VizException {
        // TODO Someday we should put objective analysis code
        // into this area
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getData(com.raytheon
     * .uf.viz.core.catalog.LayerProperty, int)
     */
    @Override
    public List<Object> getData(LayerProperty property, int timeOut)
            throws VizException {
        List<AbstractRequestableLevelNode> requests = evaluateRequestConstraints(property
                .getEntryQueryParameters(false));
        int mapSize = (int) (requests.size() * 1.25) + 1;
        Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache = new HashMap<AbstractRequestableLevelNode, List<AbstractRequestableData>>(
                mapSize);
        LinkedHashMap<AbstractRequestableLevelNode, DbQueryRequest> queries = new LinkedHashMap<AbstractRequestableLevelNode, DbQueryRequest>(
                mapSize);
        for (AbstractRequestableLevelNode req : requests) {
            getDataQuery(req, property, timeOut, queries, cache);
        }
        DbQueryRequestSet reqSet = new DbQueryRequestSet();
        reqSet.setQueries(queries.values().toArray(
                new DbQueryRequest[queries.size()]));
        DbQueryResponseSet qSetResponse = (DbQueryResponseSet) ThriftClient
                .sendRequest(reqSet);
        DbQueryResponse[] qResponses = qSetResponse.getResults();
        int index = 0;
        for (AbstractRequestableLevelNode node : queries.keySet()) {
            // put results into cache
            node.setDataQueryResults(qResponses[index++], cache);
        }

        // pull the actual results from the cache
        List<AbstractRequestableData> requesters = new ArrayList<AbstractRequestableData>(
                requests.size());
        for (AbstractRequestableLevelNode request : requests) {
            requesters.addAll(request.getData(property, timeOut, cache));
        }

        return getData(property, requesters);
    }

    protected void getDataQuery(
            AbstractRequestableLevelNode req,
            LayerProperty property,
            int timeOut,
            LinkedHashMap<AbstractRequestableLevelNode, DbQueryRequest> queries,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        List<Dependency> depends = req.getDependencies();
        if (depends.isEmpty()) {
            // is source node
            DbQueryRequest myQ = req.getDataQuery(property, timeOut, cache);
            if (myQ != null) {
                addDataQuery(req, myQ, queries);
            }
        } else {
            for (Dependency dep : depends) {
                // TODO: Optimize dTime/fTime to use bulk query mechanism,
                // small case that is a not easy to get right with a bulk
                // query
                if (dep.timeOffset == 0) {
                    getDataQuery(dep.node, property, timeOut, queries, cache);
                }
            }
        }
    }

    private void addDataQuery(AbstractRequestableLevelNode req,
            DbQueryRequest query,
            LinkedHashMap<AbstractRequestableLevelNode, DbQueryRequest> queries) {
        DbQueryRequest curQuery = queries.get(req);
        if (curQuery == null) {
            queries.put(req, query);
        } else {
            // merge
            // assume same DB, fields, etc, should only be different
            // time constraints since its the same node
            RequestConstraint curDTs = curQuery.getConstraints()
                    .get("dataTime");
            RequestConstraint myDTs = query.getConstraints().get("dataTime");
            if (curDTs != null && myDTs != null) {
                // only merge if both require dataTimes, otherwise one
                // would be constrained when it needs everything, also
                // assuming both to be in lists and needing to be merged
                curDTs.setConstraintType(ConstraintType.IN);
                Pattern split = Pattern.compile(",");

                String[] curVals = split.split(curDTs.getConstraintValue());
                String[] myVals = split.split(myDTs.getConstraintValue());
                HashSet<String> dups = new HashSet<String>(curVals.length
                        + myVals.length);
                dups.addAll(Arrays.asList(curVals));
                dups.addAll(Arrays.asList(myVals));
                curDTs.setConstraintValueList(dups.toArray(new String[dups
                        .size()]));
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#
     * getBaseUpdateConstraints(java.util.Map)
     */
    @Override
    public List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints) {
        List<Map<String, RequestConstraint>> result = new ArrayList<Map<String, RequestConstraint>>(
                1);
        result.add(constraints);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getRecord(com
     * .raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public IDataRecord[] getRecord(PluginDataObject obj)
            throws VizDataCubeException {
        return getRecord(obj, Request.ALL, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getRecord(com
     * .raytheon.uf.common.dataplugin.PluginDataObject,
     * com.raytheon.uf.common.datastorage.Request, java.lang.String)
     */
    @Override
    public IDataRecord[] getRecord(PluginDataObject obj, Request req,
            String dataset) throws VizDataCubeException {
        getRecords(Arrays.asList(obj), req, dataset);
        IDataRecord[] result = (IDataRecord[]) obj.getMessageData();
        obj.setMessageData(null);
        return result;
    }

    /**
     * Scan the inventory for AbstractRequestableLevelNodes that match the
     * request constraints
     * 
     * @param constraints
     * @return
     */
    protected abstract List<AbstractRequestableLevelNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> constraints);

    /**
     * @param queryTerms
     * @return
     */
    protected abstract List<DataTime> timeAgnosticQuery(
            Map<String, RequestConstraint> queryTerms) throws VizException;

    /**
     * @param requesters
     * @return
     */
    protected abstract List<Object> getData(LayerProperty property,
            List<AbstractRequestableData> requesters) throws VizException;
}
