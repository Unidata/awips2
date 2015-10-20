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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.common.derivparam.inv.MetadataContainer;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.tree.AbstractAliasLevelNode;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.data.AggregateRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Builds AggregateRecords which contain all perturbations for a specific
 * parameter/level of an ensemble model.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Feb 26, 2010           bsteffen  Initial creation
 * Oct 09, 2015  4931     bsteffen  Do separate requests each distinct
 *                                  ensemble_id
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GatherLevelNode extends AbstractAliasLevelNode {

    public GatherLevelNode(GatherLevelNode that) {
        super(that);
    }

    public GatherLevelNode(AbstractRequestableNode sourceNode,
            DerivParamDesc desc, DerivParamMethod method, String modelName,
            Level level) {
        super(sourceNode, desc, method, modelName, level);
    }

    @Override
    public Map<AbstractRequestableNode, Set<TimeAndSpace>> getDataDependency(
            Set<TimeAndSpace> times, AvailabilityContainer container) {
        /*
         * The MetadataContainer cannot handle the dependencies because we must
         * add an ensemble_id constraint so return that there is no dependency
         * and instead request needed data in getData()
         */
        return Collections.emptyMap();
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws DataCubeException {
        return gatherData(availability, getRequestConstraints());
    }

    /**
     * This method can be used in place of {@link #getData(Set, Map)} to
     * retrieve the data for this node. Since this node must perform separate
     * request for each distinct ensemble_id that is being gathered, the
     * dependency data that is normally passed to getData is unnecessary. This
     * method takes in some original constraints which will be passed along in
     * case the original query had additional constraints not handled normally
     * by derived parameters(For example Secondary Id)
     */
    public Set<AbstractRequestableData> gatherData(
            Set<TimeAndSpace> availability,
            Map<String, RequestConstraint> originalConstraints)
            throws DataCubeException {
        Map<TimeAndSpace, List<AbstractRequestableData>> availMap = new HashMap<TimeAndSpace, List<AbstractRequestableData>>();
        for (String member : getSourceEnsembles()) {
            Map<String, RequestConstraint> constraints = new HashMap<>(
                    originalConstraints);
            constraints.put(GridConstants.ENSEMBLE_ID, new RequestConstraint(
                    member));
            MetadataContainer container = new GridMetadataContainer(
                    constraints, new AvailabilityContainer(constraints));
            for (AbstractRequestableData data : container.getData(sourceNode,
                    availability)) {
                TimeAndSpace ast = data.getTimeAndSpace();
                List<AbstractRequestableData> avail = availMap.get(ast);
                if (avail == null) {
                    avail = new ArrayList<AbstractRequestableData>();
                    availMap.put(ast, avail);
                }
                avail.add(data);
            }
        }
        Set<AbstractRequestableData> result = new HashSet<AbstractRequestableData>();
        for (List<AbstractRequestableData> records : availMap.values()) {
            AggregateRequestableData record = new AggregateRequestableData(
                    records);
            modifyRequest(record);
            result.add(record);
        }
        return result;
    }

    /**
     * @return the {@link List} of ensemble_ids that this node is gathering.
     */
    protected List<String> getSourceEnsembles() throws DataCubeException {
        try {
            return GridInventory.getEnsembles(sourceNode);
        } catch (VizException e) {
            throw new DataCubeException(e);
        }
    }

    /**
     * @return the request constraint map that can be used for requesting all
     *         data for this node.
     */
    protected Map<String, RequestConstraint> getRequestConstraints() {
        Map<String, RequestConstraint> constraints = new HashMap<>();
        constraints.put(GridConstants.PLUGIN_NAME, new RequestConstraint(
                GridConstants.GRID));
        constraints.put(GridConstants.DATASET_ID, new RequestConstraint(
                getModelName()));
        constraints.put(GridConstants.PARAMETER_ABBREVIATION,
                new RequestConstraint(getDesc().getAbbreviation()));
        constraints.put(GridConstants.MASTER_LEVEL_NAME, new RequestConstraint(
                getLevel().getMasterLevel().getName()));
        constraints.put(GridConstants.LEVEL_ONE, new RequestConstraint(
                getLevel().getLevelOneValueAsString()));
        constraints.put(GridConstants.LEVEL_TWO, new RequestConstraint(
                getLevel().getLevelTwoValueAsString()));
        return constraints;
    }

    @Override
    public GatherLevelNode clone() {
        return new GatherLevelNode(this);
    }

}
