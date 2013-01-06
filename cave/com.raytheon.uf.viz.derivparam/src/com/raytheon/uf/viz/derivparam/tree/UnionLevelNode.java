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
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AggregateRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
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
public class UnionLevelNode extends AbstractDerivedDataNode {

    private static final Comparator<AbstractRequestableData> requestComp = new Comparator<AbstractRequestableData>() {

        @Override
        public int compare(AbstractRequestableData o1,
                AbstractRequestableData o2) {
            return Double.compare(o1.getLevel().getLevelonevalue(), o2
                    .getLevel().getLevelonevalue());
        }

    };

    protected List<AbstractRequestableNode> nodes;

    public UnionLevelNode(UnionLevelNode that) {
        super(that);
        this.nodes = that.nodes;
    }

    public UnionLevelNode(List<AbstractRequestableNode> nodes, String modelName) {
        this.nodes = nodes;
        this.modelName = modelName;
    }

    public UnionLevelNode(Level level, String modelName,
            List<AbstractRequestableNode> nodes) {
        this.setLevel(level);
        this.nodes = nodes;
        this.modelName = modelName;
    }

    public UnionLevelNode(Level level, DerivParamDesc desc,
            DerivParamMethod method, String modelName,
            List<AbstractRequestableNode> nodes) {
        super(level, desc, method, modelName);
        this.nodes = nodes;
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
        List<AbstractRequestableData> rawRecords = new ArrayList<AbstractRequestableData>();

        List<AbstractRequestableNode> requests = new ArrayList<AbstractRequestableNode>(
                nodes);
        for (AbstractRequestableNode request : requests) {
            rawRecords.addAll(dependencyData.get(request));
        }
        Map<TimeAndSpace, List<AbstractRequestableData>> bins = new HashMap<TimeAndSpace, List<AbstractRequestableData>>();
        for (AbstractRequestableData record : rawRecords) {
            List<AbstractRequestableData> bin = bins.get(record
                    .getTimeAndSpace());
            if (bin == null) {
                bin = new ArrayList<AbstractRequestableData>();
                bins.put(record.getTimeAndSpace(), bin);
            }
            bin.add(record);
        }
        Set<AbstractRequestableData> records = new HashSet<AbstractRequestableData>(
                bins.size());
        for (Entry<TimeAndSpace, List<AbstractRequestableData>> entry : bins
                .entrySet()) {
            if (entry.getValue().size() >= 2) {
                Collections.sort(entry.getValue(), requestComp);

                AggregateRequestableData newRecord = new AggregateRequestableData(
                        entry.getValue());
                newRecord.setDataTime(entry.getKey().getTime());
                newRecord.setSpace(entry.getKey().getSpace());
                modifyRequest(newRecord);
                records.add(newRecord);
            }
        }
        return records;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws VizException {
        // things in one are available for one level
        Set<TimeAndSpace> one = new HashSet<TimeAndSpace>();
        // things in two are available for two levels.
        Set<TimeAndSpace> two = new HashSet<TimeAndSpace>();

        List<AbstractRequestableNode> requests = new ArrayList<AbstractRequestableNode>(
                nodes);
        for (AbstractRequestableNode request : requests) {
            // Do not request just latest only because if two nodes have
            // different latests than this will return no times
            for (TimeAndSpace time : availability.get(request)) {
                if (one.contains(time)) {
                    two.add(time);
                } else {
                    one.add(time);
                }
            }
        }
        return two;
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
        for (AbstractRequestableNode node : nodes) {
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
