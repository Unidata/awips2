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
package com.raytheon.viz.grid.inv;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequestSet;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponseSet;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.datastructure.DecisionTree;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.MetadataContainer;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;
import com.raytheon.viz.grid.data.GridRequestableData;
import com.raytheon.viz.grid.data.GridRequestableDataFactory;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridMetadataContainer extends MetadataContainer {

    /**
     * @param originalConstraints
     */
    public GridMetadataContainer(
            Map<String, RequestConstraint> originalConstraints,
            AvailabilityContainer availabilityContainer) {
        super(originalConstraints, availabilityContainer);
    }

    @Override
    protected void processRequests() throws VizException {
        DecisionTree<GridRequestableNode> tree = new DecisionTree<GridRequestableNode>();
        List<Map<String, RequestConstraint>> constraintMaps = new ArrayList<Map<String, RequestConstraint>>();
        for (AbstractRequestableNode node : availCache.keySet()) {
            if (dataCache.containsKey(node)) {
                continue;
            }
            if (node instanceof GridRequestableNode) {
                GridRequestableNode dataNode = (GridRequestableNode) node;
                DbQueryRequest request = dataNode.getDataRequest(
                        originalConstraints, availCache.get(node));
                Map<String, RequestConstraint> constraints = request
                        .getConstraints();
                tree.insertCriteria(constraints, dataNode, false);
                constraintMaps.add(constraints);
            }
        }
        if (constraintMaps.isEmpty()) {
            super.processRequests();
            return;
        } else if (constraintMaps.size() == 1) {
            super.processRequests();
            return;
        }
        List<DbQueryRequest> requests = new ArrayList<DbQueryRequest>();
        constraintMaps = mergeConstraints(constraintMaps);
        for (Map<String, RequestConstraint> constraintMap : constraintMaps) {
            DbQueryRequest dbRequest = new DbQueryRequest();
            dbRequest.setEntityClass(GridRecord.class.getName());
            dbRequest.setConstraints(constraintMap);
            requests.add(dbRequest);
        }
        DbQueryRequestSet requestSet = new DbQueryRequestSet();
        requestSet.setQueries(requests.toArray(new DbQueryRequest[0]));
        DbQueryResponseSet responseSet = (DbQueryResponseSet) ThriftClient
                .sendRequest(requestSet);

        Map<AbstractRequestableNode, Set<AbstractRequestableData>> dataCache = new HashMap<AbstractRequestableNode, Set<AbstractRequestableData>>();
        GridRequestableDataFactory grdf = GridRequestableDataFactory
                .getInstance();
        tree.rebuildTree();
        for (DbQueryResponse response : responseSet.getResults()) {
            for (Map<String, Object> result : response.getResults()) {
                GridRecord record = (GridRecord) result.get(null);
                Map<String, Object> recordMap = RecordFactory.getInstance()
                        .loadMapFromUri(record.getDataURI());
                GridRequestableData data = grdf.getGridRequestableData(record);
                for (GridRequestableNode node : tree.searchTree(recordMap)) {
                    Set<AbstractRequestableData> set = dataCache.get(node);
                    if (set == null) {
                        set = new HashSet<AbstractRequestableData>();
                        dataCache.put(node, set);
                    }
                    set.add(data);
                }
            }
        }
        this.dataCache.putAll(dataCache);
        super.processRequests();

    }

    /**
     * Takes the list of constraintMaps and merges the constraint maps that
     * different by only one key into a new list of constraint maps. EX: 5
     * RequestConstraint Maps for GFS40 Temp on 5 MB levels will be consolidated
     * into a single RequestConstraint Map using an IN List for the MB levels.
     * 
     * @param constraintMaps
     * @return
     */
    public static List<Map<String, RequestConstraint>> mergeConstraints(
            List<Map<String, RequestConstraint>> constraintMaps) {
        List<Map<String, RequestConstraint>> rval = new ArrayList<Map<String, RequestConstraint>>();
        if (constraintMaps != null && constraintMaps.size() > 0) {
            List<String> mergeKeyList = new ArrayList<String>();

            for (Map<String, RequestConstraint> constraintMap : constraintMaps) {
                boolean merged = false;
                int index = 0;

                MERGE_MAP_LOOP: for (index = 0; index < rval.size(); index++) {
                    Map<String, RequestConstraint> mergeMap = rval.get(index);

                    // maps must be of same size
                    if (mergeMap.size() == constraintMap.size()) {
                        String mergeKey = mergeKeyList.get(index);

                        if (mergeKey != null) {
                            // determine if it matches map except for uniqueKey
                            for (String key : mergeMap.keySet()) {
                                boolean isUniqueKey = mergeKey.equals(key);
                                boolean constraintEquals = mergeMap.get(key)
                                        .equals(constraintMap.get(key));

                                if (!constraintEquals && !isUniqueKey) {
                                    continue MERGE_MAP_LOOP;
                                }
                            }

                            // map differs except for key
                            boolean constraintMerged = mergeMap.get(mergeKey)
                                    .merge(constraintMap.get(mergeKey));
                            if (!constraintMerged) {
                                continue MERGE_MAP_LOOP;
                            } else {
                                merged = true;
                                break MERGE_MAP_LOOP;
                            }
                        } else {
                            // current mergeMap has never been merged with,
                            // double check that only one key is different
                            // between maps
                            for (String key : mergeMap.keySet()) {
                                if (!mergeMap.get(key).equals(
                                        constraintMap.get(key))) {
                                    if (mergeKey == null) {
                                        // possible merge key, continue checking
                                        // map
                                        mergeKey = key;
                                    } else {
                                        // two merge keys found, go to next
                                        // merge map
                                        continue MERGE_MAP_LOOP;
                                    }
                                }
                            }

                            // found mergeKey, add to mergeKeyList. If mergeKey
                            // still being null is only possible of mergeMap and
                            // constraintMap contain the exact same constraints,
                            // consider them merged
                            if (mergeKey == null) {
                                merged = true;
                                break MERGE_MAP_LOOP;
                            }

                            boolean constraintMerged = mergeMap.get(mergeKey)
                                    .merge(constraintMap.get(mergeKey));
                            if (!constraintMerged) {
                                continue MERGE_MAP_LOOP;
                            } else {
                                merged = true;
                                mergeKeyList.set(index, mergeKey);
                                break MERGE_MAP_LOOP;
                            }

                        }
                    } // maps same size check
                } // MERGE_MAP_LOOP

                if (!merged) {
                    Map<String, RequestConstraint> mergeMap = new HashMap<String, RequestConstraint>();

                    // deep copy the map
                    for (Map.Entry<String, RequestConstraint> entry : constraintMap
                            .entrySet()) {
                        mergeMap.put(entry.getKey(), entry.getValue().clone());
                    }

                    rval.add(mergeMap);
                    mergeKeyList.add(null);
                }
            }
        }

        return rval;
    }

}
