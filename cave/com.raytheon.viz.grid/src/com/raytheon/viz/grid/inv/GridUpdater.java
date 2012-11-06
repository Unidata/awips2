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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode.Dependency;
import com.raytheon.uf.viz.derivparam.tree.OrLevelNode;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.grid.util.RadarAdapter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridUpdater implements IAlertObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridUpdater.class);

    private class UpdateValue {
        public int timeOffset;

        public AbstractDerivedDataNode node;

        public UpdateValue(Integer timeOffset, AbstractDerivedDataNode node) {
            this.timeOffset = timeOffset == null ? 0 : timeOffset;
            this.node = node;
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
            result = prime * result + getOuterType().hashCode();
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
            UpdateValue other = (UpdateValue) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (node == null) {
                if (other.node != null)
                    return false;
            } else if (!node.equals(other.node))
                return false;
            if (timeOffset != other.timeOffset)
                return false;
            return true;
        }

        private GridUpdater getOuterType() {
            return GridUpdater.this;
        }

    }

    private Set<String> myUpdates = new HashSet<String>();

    private GridInventory inventory;

    private Map<GridMapKey, Set<UpdateValue>> updateMap = new HashMap<GridMapKey, Set<UpdateValue>>();

    public GridUpdater(GridInventory inventory) {
        this.inventory = inventory;
    }

    public void startObserving() {
        ProductAlertObserver.addObserver(GridInventory.PLUGIN_NAME, this);
    }

    public void stopObserving() {
        ProductAlertObserver.removeObserver(GridInventory.PLUGIN_NAME, this);
    }

    public synchronized void addNode(AbstractDerivedDataNode node)
            throws VizException {
        List<Dependency> dependencies = node.getDependencies();
        if (dependencies == null || dependencies.isEmpty()) {
            return;
        }
        List<Dependency> dep = new ArrayList<Dependency>(dependencies);

        for (int i = 0; i < dep.size(); i++) {
            Dependency dependency = dep.get(i);
            if (dependency.node instanceof GridRequestableNode) {
                GridRequestableNode gNode = (GridRequestableNode) dependency.node;
                GridMapKey updateKey = new GridMapKey(
                        gNode.getRequestConstraintMap());
                Set<UpdateValue> set = updateMap.get(updateKey);
                if (set == null) {
                    set = new HashSet<UpdateValue>();
                    updateMap.put(updateKey, set);
                }
                set.add(new UpdateValue(dependency.timeOffset, node));
            } else if (dependency.node instanceof RadarRequestableLevelNode) {
                RadarRequestableLevelNode rNode = (RadarRequestableLevelNode) dependency.node;
                Level level = rNode.getLevel();
                Map<String, Object> gribMap = new HashMap<String, Object>();
                gribMap.put(GridInventory.MODEL_NAME_QUERY,
                        RadarAdapter.RADAR_SOURCE);
                gribMap.put(GridInventory.PARAMETER_QUERY,
                        rNode.getParamAbbrev());
                gribMap.put(GridInventory.MASTER_LEVEL_QUERY, level
                        .getMasterLevel().getName());
                gribMap.put(GridInventory.LEVEL_ONE_QUERY,
                        level.getLevelonevalue());
                gribMap.put(GridInventory.LEVEL_TWO_QUERY,
                        level.getLeveltwovalue());

                GridMapKey updateKey = new GridMapKey(gribMap);
                Set<UpdateValue> set = updateMap.get(updateKey);
                if (set == null) {
                    set = new HashSet<UpdateValue>();
                    updateMap.put(updateKey, set);
                }
                set.add(new UpdateValue(dependency.timeOffset, node));
            } else if (dependency.node instanceof AbstractDerivedDataNode) {
                AbstractDerivedDataNode dataNode = (AbstractDerivedDataNode) dependency.node;
                for (Dependency d : dataNode.getDependencies()) {
                    d.timeOffset += dependency.timeOffset;
                    if (!dep.contains(d)) {
                        dep.add(d);
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.alerts.IAlertObserver#alertArrived(com.raytheon.uf.viz
     * .core.alerts.AlertMessage[])
     */
    @Override
    public synchronized void alertArrived(Collection<AlertMessage> alertMessages) {
        Set<String> datauris = new HashSet<String>();
        for (AlertMessage alert : alertMessages) {
            if (myUpdates.remove(alert.dataURI)) {
                // This updater triggered this alert, if it processes it now we
                // could do this forever
                continue;
            }
            GridMapKey updateKey = new GridMapKey(alert.decodedAlert);
            GridTimeCache.getInstance().clearTimes(updateKey);
            LevelNode lNode = null;
            try {
                Level level = LevelFactory.getInstance().getLevel(
                        updateKey.masterLevel, updateKey.levelone,
                        updateKey.leveltwo);
                lNode = inventory.getNode(updateKey.modelName,
                        updateKey.parameter, level);
            } catch (CommunicationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

            if (lNode == null) {
                inventory.reinitTree();
                // System.out.println(alert.dataURI);
                // System.out.println("LevelId=" + level.getId());
            } else if (!(lNode instanceof GridRequestableNode)) {
                if (lNode instanceof OrLevelNode) {
                    DerivParamMethod method = ((OrLevelNode) lNode).getMethod();
                    // Null means it is an alias model and supplement means
                    // there exists a true GribNode buried under the or
                    // node
                    if (method == null
                            || !method.getName().equals("Supplement")) {
                        inventory.reinitTree();
                        // System.out.println(((AbstractDerivedLevelNode) lNode)
                        // .getModelName());
                    }
                } else {
                    inventory.reinitTree();
                    // System.out.println(alert.dataURI);
                    // System.out.println(lNode.getClass());
                }
            }
            Set<UpdateValue> set = updateMap.get(updateKey);
            if (set == null) {
                continue;
            }
            for (UpdateValue value : set) {
                GridRecord fakeRec = new GridRecord();
                fakeRec.setPluginName(GridInventory.PLUGIN_NAME);
                Object obj = alert.decodedAlert.get("dataTime");
                if (!(obj instanceof DataTime)) {
                    throw new IllegalArgumentException(
                            "Error processing Alerts: "
                                    + obj.toString()
                                    + " cannot be cast to a DataTime because it is a "
                                    + obj.getClass().getSimpleName());
                }
                DataTime time = (DataTime) obj;
                fakeRec.setDataTime(new DataTime(time.getRefTime(), time
                        .getFcstTime() - value.timeOffset));
                fakeRec.setDatasetId(value.node.getModelName());

                Parameter param = new Parameter(value.node.getDesc()
                        .getAbbreviation(), value.node.getDesc().getName(),
                        value.node.getDesc().getUnit());
                fakeRec.setParameter(param);
                fakeRec.setLevel(value.node.getLevel());
                if (value.node instanceof GatherLevelNode) {
                    fakeRec.setEnsembleId(null);
                } else {
                    fakeRec.setEnsembleId((String) alert.decodedAlert
                            .get(GridInventory.ENSEMBLE_QUERY));
                }
                fakeRec.setSecondaryId((String) alert.decodedAlert
                        .get(GridConstants.SECONDARY_ID));
                fakeRec.setLocation((GridCoverage) alert.decodedAlert
                        .get(GridConstants.LOCATION));
                try {
                    fakeRec.constructDataURI();
                    datauris.add(fakeRec.getDataURI());
                } catch (PluginException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Unable to generate updates for derived product",
                                    e);
                }

            }
        }
        myUpdates.addAll(datauris);
        ProductAlertObserver.processDerivedAlerts(datauris);
    }

    /**
     * 
     */
    public synchronized void refreshNodes() {
        GridTimeCache.getInstance().flush();
        Set<AbstractDerivedDataNode> oldNodes = new HashSet<AbstractDerivedDataNode>();
        for (Set<UpdateValue> values : updateMap.values()) {
            for (UpdateValue value : values) {
                oldNodes.add(value.node);
            }
        }
        updateMap.clear();
        for (AbstractDerivedDataNode node : oldNodes) {
            // Get Node will automatically add this to the updater.
            inventory.getNode(node.getModelName(), node.getDesc()
                    .getAbbreviation(), node.getLevel());
        }
    }

}
