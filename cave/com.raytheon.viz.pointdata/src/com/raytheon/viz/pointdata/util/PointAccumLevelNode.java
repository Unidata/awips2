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
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;

/**
 * TODO Add Description
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

public class PointAccumLevelNode extends AbstractDerivedLevelNode {

    private List<AbstractRequestableLevelNode> idNodes;

    private AbstractRequestableLevelNode timeNode;

    public PointAccumLevelNode(PointAccumLevelNode that) {
        super(that);
        this.idNodes = that.idNodes;
        this.timeNode = that.timeNode;
    }

    public PointAccumLevelNode(DerivParamDesc desc, DerivParamMethod method,
            List<AbstractRequestableLevelNode> idNodes,
            AbstractRequestableLevelNode timeNode) {
        super(PointDataInventory.getStationLevel(), desc, method, null);
        this.idNodes = idNodes;
        this.timeNode = timeNode;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDataInternal(com.raytheon.uf.viz.core.catalog.LayerProperty, int,
     * java.util.Map)
     */
    @Override
    protected List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        List<AbstractRequestableData> idRequesters = new ArrayList<AbstractRequestableData>(
                idNodes.size());
        for (AbstractRequestableLevelNode idNode : idNodes) {
            idRequesters.add(idNode.getData(property, timeOut, cache).get(0));
        }
        AbstractRequestableData rData = new PointAccumRequestableData(
                idRequesters,
                timeNode.getData(property, timeOut, cache).get(0), method,
                property);
        rData.setParameter(desc.getAbbreviation());
        rData.setParameterName(desc.getName());
        rData.setUnit(desc.getUnit());
        return Arrays.asList(rData);
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
        for (AbstractRequestableLevelNode idNode : idNodes) {
            dependencies.add(new Dependency(idNode, 0));
        }
        return dependencies;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * timeQueryInternal(boolean, java.util.Map)
     */
    @Override
    protected Set<DataTime> timeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        return TIME_AGNOSTIC;
    }

    @Override
    public boolean isTimeAgnostic() {
        return true;
    }

    public PointAccumLevelNode clone() {
        return new PointAccumLevelNode(this);
    }

}
