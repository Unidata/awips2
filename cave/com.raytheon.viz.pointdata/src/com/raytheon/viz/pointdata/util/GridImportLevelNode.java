/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.datacube.DataCubeContainer;

/**
 * A derived data node for point data that can be used to import data from a
 * grid model.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 15, 2018  7019     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class GridImportLevelNode extends AbstractDerivedDataNode {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridImportLevelNode.class);

    private final AbstractRequestableNode latNode;

    private final AbstractRequestableNode lonNode;

    private final AbstractRequestableNode timeNode;

    public GridImportLevelNode(Level level, DerivParamDesc desc,
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
            AvailabilityContainer availabilityContainer)
            throws DataCubeException {
        Map<AbstractRequestableNode, Set<TimeAndSpace>> result = new HashMap<>(
                3);
        result.put(latNode, availability);
        result.put(lonNode, availability);
        result.put(timeNode, availability);
        return result;
    }

    @Override
    public Set<AbstractRequestableData> getData(Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws DataCubeException {
        AbstractRequestableData latRequest = dependencyData.get(latNode)
                .iterator().next();
        AbstractRequestableData lonRequest = dependencyData.get(lonNode)
                .iterator().next();
        AbstractRequestableData timeRequest = dependencyData.get(timeNode)
                .iterator().next();
        AbstractRequestableData importNode = new GridImportRequestableData(
                this.getLevel(), this.desc.getAbbreviation(),
                (DerivParamField) method.getFields().get(0), latRequest,
                lonRequest, timeRequest);
        return Collections.singleton(importNode);
    }

    @Override
    public List<Dependency> getDependencies() {
        return Arrays.asList(new Dependency(latNode, 0),
                new Dependency(lonNode, 0), new Dependency(timeNode, 0));
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws DataCubeException {
        Set<TimeAndSpace> result = new HashSet<>(3);
        result.addAll(availability.get(latNode));
        result.retainAll(availability.get(lonNode));
        result.retainAll(availability.get(timeNode));
        return result;
    }

    public static boolean isAvailable(DerivParamField field, Level level) {
        Map<String, RequestConstraint> constraints = GridImportRequestableData
                .getConstraints(field, level);
        try {
            DataTime[] times = DataCubeContainer.performTimeQuery(constraints,
                    true);
            return times != null && times.length > 0;
        } catch (DataCubeException e) {
            statusHandler.handle(Priority.WARN,
                    "Failed to query availability of grid import for "
                            + field.getValidSource() + " " + field.getParam(),
                    e);
            return false;
        }
    }
}
