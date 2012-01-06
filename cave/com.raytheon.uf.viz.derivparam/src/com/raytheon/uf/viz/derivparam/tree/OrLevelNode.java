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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * 
 * Or LevelNodes will return data for each requested time for the first node
 * which has data. For time queries it simply returns all times available for
 * any node that it has.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class OrLevelNode extends AbstractDerivedLevelNode {

    private List<AbstractRequestableLevelNode> nodes;

    public OrLevelNode(OrLevelNode that) {
        super(that);
        if (that.nodes != null) {
            this.nodes = new ArrayList<AbstractRequestableLevelNode>(that.nodes);
        }
    }

    public OrLevelNode(Level level, DerivParamDesc desc,
            DerivParamMethod method, String modelName,
            List<AbstractRequestableLevelNode> nodes) {
        this(level, desc, method, modelName, nodes, true);
    }

    /**
     * Create an or node
     * 
     * @param level
     *            the level this node is on
     * @param desc
     *            the desc this node is from
     * @param method
     *            the derived parameter method this node implements
     * @param modelName
     *            the source for this node
     * @param nodes
     *            the nodes to or, in order.
     * @param alias
     *            if true all results will be aliased to the name/abbr/units
     *            specified by desc/method.
     */
    public OrLevelNode(Level level, DerivParamDesc desc,
            DerivParamMethod method, String modelName,
            List<AbstractRequestableLevelNode> nodes, boolean alias) {
        super(level, desc, method, modelName);
        if (alias) {
            this.nodes = new ArrayList<AbstractRequestableLevelNode>(
                    nodes.size());
            for (AbstractRequestableLevelNode node : nodes) {
                this.nodes.add(new AliasLevelNode(node, desc, method,
                        modelName, level));
            }
        } else {
            this.nodes = new ArrayList<AbstractRequestableLevelNode>(nodes);
        }
    }

    public void addNodeToOrList(AbstractRequestableLevelNode node) {
        nodes.add(node);
    }

    @Override
    protected List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        Set<DataTime> requestedTimes = null;
        DataTime[] requestedTimesArr = property.getSelectedEntryTime();
        if (requestedTimesArr == null) {
            requestedTimes = this.timeQuery(false);
        } else {
            requestedTimes = new HashSet<DataTime>(
                    Arrays.asList(requestedTimesArr));
        }
        List<AbstractRequestableData> records = new ArrayList<AbstractRequestableData>(
                requestedTimes.size());
        for (AbstractRequestableLevelNode node : nodes) {
            List<AbstractRequestableData> newRecords = node.getData(property,
                    timeOut, cache);
            for (AbstractRequestableData record : newRecords) {
                if (record.getDataTime() == null) {
                    requestedTimes.clear();
                } else {
                    requestedTimes.remove(record.getDataTime());
                }
            }
            records.addAll(newRecords);
            if (requestedTimes.isEmpty()) {
                break;
            }
            property.setSelectedEntryTimes(requestedTimes
                    .toArray(new DataTime[requestedTimes.size()]));
        }
        return records;
    }

    @Override
    protected Set<DataTime> timeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        Set<DataTime> results = new HashSet<DataTime>();
        for (AbstractRequestableLevelNode node : nodes) {
            Set<DataTime> times = node.timeQuery(latestOnly, cache,
                    latestOnlyCache);
            if (times == AbstractRequestableLevelNode.TIME_AGNOSTIC) {
                return times;
            } else {
                for (DataTime time : times) {
                    boolean good = true;
                    for (DataTime result : results) {
                        if (result.getMatchRef() == time.getMatchRef()
                                && result.getMatchFcst() == time.getMatchFcst()) {
                            good = false;
                            break;
                        }
                    }
                    if (good) {
                        results.add(time);
                    }
                }
            }
        }
        return results;
    }

    @Override
    public boolean isTimeAgnostic() {
        for (AbstractRequestableLevelNode node : nodes) {
            if (node.isTimeAgnostic()) {
                return true;
            }
        }

        return false;
    }

    @Override
    public Map<String, RequestConstraint> getRequestConstraintMap() {
        Map<String, RequestConstraint> rval = null;
        List<Map<String, RequestConstraint>> list = new ArrayList<Map<String, RequestConstraint>>(
                nodes.size());

        for (AbstractRequestableLevelNode node : nodes) {
            Map<String, RequestConstraint> rcMap = node
                    .getRequestConstraintMap();

            if (rcMap == null) {
                // sub node has no request constraint short circuit and exit now
                return null;
            }

            list.add(node.getRequestConstraintMap());
        }

        list = mergeConstraints(list);
        if (list.size() == 1) {
            rval = list.get(0);
        } else {
            // not directly combinable until this method is updated to
            // return a list of rcMaps
            rval = null;
        }

        return rval;
    }

    @Override
    public boolean hasRequestConstraints() {
        boolean rval = true;
        for (AbstractRequestableLevelNode node : nodes) {
            if (!node.hasRequestConstraints()) {
                rval = false;
                break;
            }
        }
        if (rval) {
            // handle the maps not being directly combinable
            rval = getRequestConstraintMap() != null;
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDependencies()
     */
    public List<Dependency> getDependencies() {
        List<Dependency> dependencies = new ArrayList<Dependency>(nodes.size());
        for (AbstractRequestableLevelNode node : nodes) {
            dependencies.add(new Dependency(node, 0));
        }
        return dependencies;
    }

    @Override
    public OrLevelNode clone() {
        return new OrLevelNode(this);
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
        OrLevelNode other = (OrLevelNode) obj;
        if (nodes == null) {
            if (other.nodes != null)
                return false;
        } else if (!nodes.equals(other.nodes))
            return false;
        return true;
    }

}
