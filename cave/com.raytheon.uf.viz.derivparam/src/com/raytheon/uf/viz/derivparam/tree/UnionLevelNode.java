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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AggregateRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * 
 * Provided a mechanism for requesting data for an entire 3D cube. If a Level is
 * set it will request records for all Standard levels within that composite
 * level, otherwise it will request all MB records. It will respond to time
 * queries with the Union of all levels it represents, although in the future
 * this may need to be changed to the intersection, or a limited intersection
 * when at least 3 levels are available. It returns all the GribRecords from all
 * the level nodes it represents, these should be sorted by the requesting node.
 * Finally it attempts to merge any requests to avoid the overhead of multiple
 * requests to EDEX.
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
public class UnionLevelNode extends AbstractDerivedLevelNode {

    private static final Comparator<AbstractRequestableData> requestComp = new Comparator<AbstractRequestableData>() {

        @Override
        public int compare(AbstractRequestableData o1,
                AbstractRequestableData o2) {
            return Double.compare(o1.getLevel().getLevelonevalue(), o2
                    .getLevel().getLevelonevalue());
        }

    };

    protected List<AbstractRequestableLevelNode> nodes;

    public UnionLevelNode(UnionLevelNode that) {
        super(that);
        this.nodes = that.nodes;
    }

    public UnionLevelNode(List<AbstractRequestableLevelNode> nodes,
            String modelName) {
        this.nodes = nodes;
        this.modelName = modelName;
    }

    public UnionLevelNode(Level level, String modelName,
            List<AbstractRequestableLevelNode> nodes) {
        this.setLevel(level);
        this.nodes = nodes;
        this.modelName = modelName;
    }

    public UnionLevelNode(Level level, DerivParamDesc desc,
            DerivParamMethod method, String modelName,
            List<AbstractRequestableLevelNode> nodes) {
        super(level, desc, method, modelName);
        this.nodes = nodes;
    }

    @Override
    public List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        List<AbstractRequestableData> rawRecords = new ArrayList<AbstractRequestableData>();

        List<AbstractRequestableLevelNode> requests = new ArrayList<AbstractRequestableLevelNode>(
                nodes);
        for (AbstractRequestableLevelNode request : requests) {
            rawRecords.addAll(request.getData(property, timeOut, cache));
        }
        Map<DataTime, List<AbstractRequestableData>> bins = new HashMap<DataTime, List<AbstractRequestableData>>();
        for (AbstractRequestableData record : rawRecords) {
            List<AbstractRequestableData> bin = bins.get(record.getDataTime());
            if (bin == null) {
                bin = new ArrayList<AbstractRequestableData>();
                bins.put(record.getDataTime(), bin);
            }
            bin.add(record);
        }
        List<AbstractRequestableData> records = new ArrayList<AbstractRequestableData>(
                bins.size());
        for (Entry<DataTime, List<AbstractRequestableData>> entry : bins
                .entrySet()) {
            if (entry.getValue().size() >= 2) {
                Collections.sort(entry.getValue(), requestComp);

                AggregateRequestableData newRecord = new AggregateRequestableData(
                        entry.getValue());
                newRecord.setDataTime(entry.getKey());
                modifyRequest(newRecord);
                records.add(newRecord);
            }
        }
        return records;
    }

    @Override
    public Set<DataTime> timeQueryInternal(TimeQueryRequest originalRequest,
            boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        // Any time in results should have at least 2 nodes with that time
        Set<DataTime> results = TIME_AGNOSTIC;
        // Any time in single has at least one node available at that time
        Set<DataTime> single = new HashSet<DataTime>();
        List<AbstractRequestableLevelNode> requests = new ArrayList<AbstractRequestableLevelNode>(
                nodes);
        for (AbstractRequestableLevelNode request : requests) {
            Set<DataTime> times = request.timeQuery(originalRequest,
                    latestOnly, cache, latestOnlyCache);
            if (times == TIME_AGNOSTIC) {
                continue;
            } else if (results == TIME_AGNOSTIC) {
                results = new HashSet<DataTime>();
                single.addAll(times);
            } else {
                // We must have at least two resources with times. The first
                // node with times will add only to single, after that any
                // time in single will also be added to results
                results.addAll(times);
                results.retainAll(single);
                single.addAll(times);
            }
        }
        return results;
    }

    @Override
    public boolean isTimeAgnostic() {
        boolean timeAgnostic = true;

        for (AbstractRequestableLevelNode node : nodes) {
            if (!node.isTimeAgnostic()) {
                timeAgnostic = false;
                break;
            }
        }

        return timeAgnostic;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDependencies()
     */
    @Override
    public List<Dependency> getDependencies() {
        List<Dependency> dependencies = new ArrayList<Dependency>(nodes.size());
        for (AbstractRequestableLevelNode node : nodes) {
            dependencies.add(new Dependency(node, 0));
        }
        return dependencies;
    }

    @Override
    public UnionLevelNode clone() {
        return new UnionLevelNode(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((nodes == null) ? 0 : nodes.hashCode());
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
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        UnionLevelNode other = (UnionLevelNode) obj;
        if (nodes == null) {
            if (other.nodes != null)
                return false;
        } else if (!nodes.equals(other.nodes))
            return false;
        return true;
    }

}
