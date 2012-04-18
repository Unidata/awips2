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
package com.raytheon.uf.viz.derivparam.tree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;

/**
 * 
 * The base level node for Cave, each Level Node should be able to handle time
 * queries and metadata requests. This class attempts to handle
 * caching(currently very limited) for all subclasses.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractRequestableLevelNode extends LevelNode {

    @SuppressWarnings("unchecked")
    public static final Set<DataTime> TIME_AGNOSTIC = Collections.EMPTY_SET;

    public static class Dependency {

        public Dependency(AbstractRequestableLevelNode node, int timeOffset) {
            super();
            this.node = node;
            this.timeOffset = timeOffset;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((node == null) ? 0 : node.hashCode());
            result = prime * result + timeOffset;
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Dependency other = (Dependency) obj;
            if (node == null) {
                if (other.node != null)
                    return false;
            } else if (!node.equals(other.node))
                return false;
            if (timeOffset != other.timeOffset)
                return false;
            return true;
        }

        public AbstractRequestableLevelNode node;

        public int timeOffset;
    }

    public AbstractRequestableLevelNode() {
    }

    public AbstractRequestableLevelNode(Level level) {
        super(level);
    }

    public AbstractRequestableLevelNode(LevelNode that) {
        super(that);
    }

    public List<AbstractRequestableData> getData(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        DataTime[] origTimes = property.getSelectedEntryTime();
        List<AbstractRequestableData> rval = new ArrayList<AbstractRequestableData>();
        if (cache != null && cache.containsKey(this) && origTimes != null) {
            List<AbstractRequestableData> cachedRecords = cache.get(this);
            List<DataTime> selectedTimes = Arrays.asList(origTimes);
            List<DataTime> newTimes = new ArrayList<DataTime>(selectedTimes);
            for (AbstractRequestableData record : cachedRecords) {
                if (selectedTimes.contains(record.getDataTime())) {
                    rval.add(record);
                    newTimes.remove(record.getDataTime());
                }
            }
            property.setSelectedEntryTimes(newTimes.toArray(new DataTime[0]));
        }
        if (origTimes == null || property.getSelectedEntryTime().length > 0) {
            List<AbstractRequestableData> newRecords = getDataInternal(
                    property, timeOut, cache);
            rval.addAll(newRecords);
            List<AbstractRequestableData> cachedRecords = cache.get(this);
            if (cachedRecords == null) {
                cache.put(this, new ArrayList<AbstractRequestableData>(
                        newRecords));
            } else {
                cachedRecords.addAll(newRecords);
            }
        }
        property.setSelectedEntryTimes(origTimes);
        return rval;
    }

    public DbQueryRequest getDataQuery(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        DataTime[] origTimes = property.getSelectedEntryTime();
        DbQueryRequest rval = null;
        if (cache != null && cache.containsKey(this) && origTimes != null) {
            List<AbstractRequestableData> cachedRecords = cache.get(this);
            List<DataTime> selectedTimes = Arrays.asList(origTimes);
            List<DataTime> newTimes = new ArrayList<DataTime>(selectedTimes);
            for (AbstractRequestableData record : cachedRecords) {
                if (selectedTimes.contains(record.getDataTime())) {
                    newTimes.remove(record.getDataTime());
                }
            }
            property.setSelectedEntryTimes(newTimes
                    .toArray(new DataTime[newTimes.size()]));
        }
        if (origTimes == null || property.getSelectedEntryTime().length > 0) {
            rval = getDataQueryInternal(property, timeOut, cache);
        }
        property.setSelectedEntryTimes(origTimes);
        return rval;
    }

    public void setDataQueryResults(
            DbQueryResponse queryResponse,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        // need transform method
        List<AbstractRequestableData> records = processDataQueryResults(queryResponse);
        if (records == null || records.size() > 0) {
            List<AbstractRequestableData> cachedRecords = cache.get(this);
            if (cachedRecords == null) {
                cache.put(this, records);
            } else {
                // dups?
                cachedRecords.addAll(records);
            }
        }
    }

    protected abstract List<AbstractRequestableData> processDataQueryResults(
            DbQueryResponse queryResponse) throws VizException;

    public Set<DataTime> timeQuery(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        if (cache != null && cache.containsKey(this)) {
            return cache.get(this);
        } else if (latestOnly && latestOnlyCache.containsKey(this)) {
            return latestOnlyCache.get(this);
        }

        Set<DataTime> results = timeQueryInternal(latestOnly, cache,
                latestOnlyCache);
        if (cache != null && !latestOnly) {
            cache.put(this, results);
        }
        return results;
    }

    public TimeQueryRequest getTimeQuery(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        if (cache != null && cache.containsKey(this)) {
            return null;
        } else if (latestOnly && latestOnlyCache.containsKey(this)) {
            return null;
        }

        return getTimeQueryInternal(latestOnly, cache);
    }

    public void setTimeQueryResults(boolean latestOnly,
            List<DataTime> queryResponse,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        // perform any node specific processing
        processTimeQueryResults(latestOnly, queryResponse);
        if (latestOnly) {
            Set<DataTime> cachedRecords = latestOnlyCache.get(this);
            if (cachedRecords == null) {
                latestOnlyCache.put(this, new HashSet<DataTime>(queryResponse));
            } else {
                cachedRecords.addAll(queryResponse);
            }
        } else {
            Set<DataTime> cachedRecords = cache.get(this);
            if (cachedRecords == null) {
                cache.put(this, new HashSet<DataTime>(queryResponse));
            } else {
                cachedRecords.addAll(queryResponse);
            }
        }
    }

    protected void processTimeQueryResults(boolean latestOnly,
            List<DataTime> queryResponse) throws VizException {
        // default has no internal processing to add
    }

    public abstract List<Dependency> getDependencies();

    public abstract boolean isTimeAgnostic();

    public abstract boolean hasRequestConstraints();

    public abstract Map<String, RequestConstraint> getRequestConstraintMap();

    public boolean isConstant() {
        List<Dependency> dependencies = getDependencies();
        if (dependencies.isEmpty()) {
            return false;
        }
        for (Dependency dep : getDependencies()) {
            if (!dep.node.isConstant()) {
                return false;
            }
        }
        // All dependencies must be constant
        return true;
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
    public List<Map<String, RequestConstraint>> mergeConstraints(
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

                            merged = true;

                            // found mergeKey, add to mergeKeyList. If mergeKey
                            // still being null is only possible of mergeMap and
                            // constraintMap contain the exact same constraints,
                            // consider them merged
                            if (mergeKey != null) {
                                mergeKeyList.set(index, mergeKey);
                            }

                            break MERGE_MAP_LOOP;
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

    protected abstract Set<DataTime> timeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException;

    protected abstract TimeQueryRequest getTimeQueryInternal(
            boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache)
            throws VizException;

    protected abstract List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException;

    protected abstract DbQueryRequest getDataQueryInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException;

    protected Set<DataTime> timeQuery(boolean latestOnly) throws VizException {
        return timeQuery(latestOnly,
                new HashMap<AbstractRequestableLevelNode, Set<DataTime>>(),
                new HashMap<AbstractRequestableLevelNode, Set<DataTime>>());
    }

}
