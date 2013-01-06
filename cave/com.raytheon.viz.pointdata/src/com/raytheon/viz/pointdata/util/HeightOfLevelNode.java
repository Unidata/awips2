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

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;

/**
 * Node for the HeightOf point data derived parameter
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class HeightOfLevelNode extends AbstractDerivedDataNode {

    private AbstractRequestableNode latNode;

    private AbstractRequestableNode lonNode;

    private AbstractRequestableNode timeNode;

    /**
     * @param desc
     * @param method
     * @param latNode
     * @param lonNode
     */
    public HeightOfLevelNode(Level level, DerivParamDesc desc,
            DerivParamMethod method, AbstractRequestableNode latNode,
            AbstractRequestableNode lonNode, AbstractRequestableNode timeNode) {
        super(level, desc, method, null);
        this.latNode = latNode;
        this.lonNode = lonNode;
        this.timeNode = timeNode;
    }

    @Override
    public Map<AbstractRequestableNode, Set<TimeAndSpace>> getDataDependency(
            Set<TimeAndSpace> availability,
            AvailabilityContainer availabilityContainer) throws VizException {
        Map<AbstractRequestableNode, Set<TimeAndSpace>> result = new HashMap<AbstractRequestableNode, Set<TimeAndSpace>>();
        result.put(latNode, availability);
        result.put(lonNode, availability);
        if (timeNode != null) {
            result.put(timeNode, availability);
        }
        return result;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException {
        AbstractRequestableData latRequest = dependencyData.get(latNode)
                .iterator().next();
        AbstractRequestableData lonRequest = dependencyData.get(lonNode)
                .iterator().next();
        AbstractRequestableData timeRequest = null;
        if (timeNode != null) {
            timeRequest = dependencyData.get(timeNode).iterator().next();
        }
        AbstractRequestableData heightOf = new HeightOfRequestableData(
                this.getLevel(), this.desc.getAbbreviation(), latRequest,
                lonRequest, timeRequest);
        return new HashSet<AbstractRequestableData>(Arrays.asList(heightOf));
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDependencies()
     */
    @Override
    public List<Dependency> getDependencies() {
        if (timeNode == null) {
            return Arrays.asList(new Dependency(latNode, 0), new Dependency(
                    lonNode, 0));
        } else {
            return Arrays.asList(new Dependency(latNode, 0), new Dependency(
                    lonNode, 0), new Dependency(timeNode, 0));
        }
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws VizException {
        return AvailabilityContainer.AGNOSTIC_SET;
    }
}
