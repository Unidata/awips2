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
package com.raytheon.viz.pointdata.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;

/**
 * A Node representing the Accum derived paramteer method which is used by point
 * data to generate accumulations over time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class PointAccumLevelNode extends AbstractDerivedDataNode {

    private List<AbstractRequestableNode> idNodes;

    private AbstractRequestableNode timeNode;

    private String plugin;

    public PointAccumLevelNode(PointAccumLevelNode that) {
        super(that);
        this.idNodes = that.idNodes;
        this.timeNode = that.timeNode;
        this.plugin = that.plugin;
    }

    public PointAccumLevelNode(DerivParamDesc desc, DerivParamMethod method,
            List<AbstractRequestableNode> idNodes,
            AbstractRequestableNode timeNode, String plugin) {
        super(PointDataInventory.getStationLevel(), desc, method, null);
        this.idNodes = idNodes;
        this.timeNode = timeNode;
        this.plugin = plugin;
    }

    @Override
    public Map<AbstractRequestableNode, Set<TimeAndSpace>> getDataDependency(
            Set<TimeAndSpace> availability,
            AvailabilityContainer availabilityContainer) throws VizException {
        Map<AbstractRequestableNode, Set<TimeAndSpace>> rval = new HashMap<AbstractRequestableNode, Set<TimeAndSpace>>();
        for (AbstractRequestableNode idNode : idNodes) {
            rval.put(idNode, availability);
        }
        rval.put(timeNode, availability);
        return rval;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException {
        List<AbstractRequestableData> idRequesters = new ArrayList<AbstractRequestableData>(
                idNodes.size());
        for (AbstractRequestableNode idNode : idNodes) {
            AbstractRequestableData idRequester = dependencyData.get(idNode)
                    .iterator().next();
            idRequesters.add(idRequester);
        }

        AbstractRequestableData rData = new PointAccumRequestableData(
                idRequesters, dependencyData.get(timeNode).iterator().next(),
                method, plugin);
        rData.setParameter(desc.getAbbreviation());
        rData.setParameterName(desc.getName());
        rData.setUnit(desc.getUnit());
        return new HashSet<AbstractRequestableData>(Arrays.asList(rData));
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDependencies()
     */
    @Override
    public List<Dependency> getDependencies() {
        List<Dependency> dependencies = new ArrayList<Dependency>();
        dependencies.add(new Dependency(timeNode, 0));
        for (AbstractRequestableNode idNode : idNodes) {
            dependencies.add(new Dependency(idNode, 0));
        }
        return dependencies;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws VizException {
        return AvailabilityContainer.AGNOSTIC_SET;
    }

    public PointAccumLevelNode clone() {
        return new PointAccumLevelNode(this);
    }

}
