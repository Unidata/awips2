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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpaceMatcher;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpaceMatcher.MatchResult;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * 
 * Or LevelNodes will return data for each requested time for the first node
 * which has data. For availability queries it simply returns all unique
 * TimeAndSpaces for any node that it has. TimeAndSpaces that only differ by
 * time range are not considered unique, in such cases only the TimeAndSpace
 * from the first node will be returned.
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
public class OrLevelNode extends AbstractDerivedDataNode {

    private List<AbstractRequestableNode> nodes;

    public OrLevelNode(OrLevelNode that) {
        super(that);
        if (that.nodes != null) {
            this.nodes = new ArrayList<AbstractRequestableNode>(that.nodes);
        }
    }

    public OrLevelNode(Level level, DerivParamDesc desc,
            DerivParamMethod method, String modelName,
            List<AbstractRequestableNode> nodes) {
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
            List<AbstractRequestableNode> nodes, boolean alias) {
        super(level, desc, method, modelName);
        if (alias) {
            this.nodes = new ArrayList<AbstractRequestableNode>(nodes.size());
            for (AbstractRequestableNode node : nodes) {
                this.nodes.add(new AliasLevelNode(node, desc, method,
                        modelName, level));
            }
        } else {
            this.nodes = new ArrayList<AbstractRequestableNode>(nodes);
        }
    }

    public void addNodeToOrList(AbstractRequestableNode node) {
        nodes.add(node);
    }

    @Override
    public Map<AbstractRequestableNode, Set<TimeAndSpace>> getDataDependency(
            Set<TimeAndSpace> availability,
            AvailabilityContainer availabilityContainer) throws VizException {
        Map<AbstractRequestableNode, Set<TimeAndSpace>> result = new HashMap<AbstractRequestableNode, Set<TimeAndSpace>>();
        for (AbstractRequestableNode node : nodes) {
            result.put(node, availability);
        }
        return result;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException {
        Map<TimeAndSpace, AbstractRequestableData> dataMap = new HashMap<TimeAndSpace, AbstractRequestableData>();
        for (AbstractRequestableNode node : nodes) {
            Set<AbstractRequestableData> dataSet = dependencyData.get(node);
            for (AbstractRequestableData data : dataSet) {
                TimeAndSpace ast = data.getTimeAndSpace();
                if (!dataMap.containsKey(ast)) {
                    dataMap.put(ast, data);
                }
            }
        }
        return new HashSet<AbstractRequestableData>(dataMap.values());
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDependencies()
     */
    public List<Dependency> getDependencies() {
        List<Dependency> dependencies = new ArrayList<Dependency>(nodes.size());
        for (AbstractRequestableNode node : nodes) {
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

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws VizException {
        // Do not return two identical TimeAndSpaces that are different only by
        // time range. For cases where two nodes have times that only differ by
        // range then only the first time should be returned.
        TimeAndSpaceMatcher matcher = new TimeAndSpaceMatcher();
        matcher.setIgnoreRange(true);
        Set<TimeAndSpace> myAvailability = new HashSet<TimeAndSpace>();
        for (AbstractRequestableNode node : nodes) {
            HashSet<TimeAndSpace> nodeAvail = new HashSet<TimeAndSpace>(
                    availability.get(node));

            // find the times that match ignoring range.
            Collection<MatchResult> matches = matcher.match(myAvailability,
                    nodeAvail).values();
            for (MatchResult match : matches) {
                ISpatialObject space1 = match.get1().getSpace();
                ISpatialObject space2 = match.get2().getSpace();
                // if the spaces are equal then remove the new time so it is not
                // added. This will remove identical times and times that match
                // ignoring range.
                if (space1.equals(space2)) {
                    nodeAvail.remove(match.get2());
                }
            }
            // Add the TimeAndSpace objects that are new.
            myAvailability.addAll(nodeAvail);
        }
        return myAvailability;
    }

}
