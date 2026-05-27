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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.GridInventoryUpdater;
import com.raytheon.uf.common.dataplugin.grid.derivparam.GridMapKey;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.GridTimeCache;
import com.raytheon.uf.common.dataplugin.grid.derivparam.tree.GridRequestableNode;
import com.raytheon.uf.common.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.uf.common.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode.Dependency;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.grid.GridExtensionManager;

/**
 * Listens for updates to grid data and generates alerts for derived parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------
 * Mar 25, 2010  3547     bsteffen  Initial creation
 * Aug 30, 2013  2298     rjpeter   Make getPluginName abstract
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * Mar 03, 2016  5439     bsteffen  Allow grid derived parameters from edex
 * Aug 15, 2017  6332     bsteffen  Move radar specific logic to extension
 * Aug 23, 2017  6125     bsteffen  Split common updating code to GridInventoryUpdater.
 * Nov 30, 2018  7673     bsteffen  Prevent full queue from blocking.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GridUpdater extends GridInventoryUpdater {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridUpdater.class);

    private static class UpdateValue {
        public int timeOffset;

        public AbstractDerivedDataNode node;

        public UpdateValue(Integer timeOffset, AbstractDerivedDataNode node) {
            this.timeOffset = timeOffset == null ? 0 : timeOffset;
            this.node = node;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = (prime * result) + ((node == null) ? 0 : node.hashCode());
            result = (prime * result) + timeOffset;
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            UpdateValue other = (UpdateValue) obj;
            if (node == null) {
                if (other.node != null) {
                    return false;
                }
            } else if (!node.equals(other.node)) {
                return false;
            }
            if (timeOffset != other.timeOffset) {
                return false;
            }
            return true;
        }
    }

    private final Map<GridMapKey, Set<UpdateValue>> updateMap = new HashMap<>();

    private final BlockingQueue<String> uriUpdateQueue = new LinkedBlockingQueue<>();

    private final Job sendDerivedAlerts = new Job(
            "Sending Derived Grid Alerts") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            Set<String> datauris = new HashSet<>();
            uriUpdateQueue.drainTo(datauris);
            ProductAlertObserver.processDataURIAlerts(datauris);
            return Status.OK_STATUS;
        }

    };

    public GridUpdater(VizGridInventory inventory) {
        super(inventory);
        sendDerivedAlerts.setSystem(true);
    }

    public synchronized void addNode(AbstractDerivedDataNode node) {
        List<Dependency> dependencies = node.getDependencies();
        if ((dependencies == null) || dependencies.isEmpty()) {
            return;
        }
        List<Dependency> dep = new ArrayList<>(dependencies);

        for (int i = 0; i < dep.size(); i++) {
            Dependency dependency = dep.get(i);
            if (dependency.node instanceof GridRequestableNode) {
                GridRequestableNode gNode = (GridRequestableNode) dependency.node;
                GridMapKey updateKey = new GridMapKey(
                        gNode.getRequestConstraintMap());
                Set<UpdateValue> set = updateMap.get(updateKey);
                if (set == null) {
                    set = new HashSet<>();
                    updateMap.put(updateKey, set);
                }
                set.add(new UpdateValue(dependency.timeOffset, node));
            } else if (dependency.node instanceof AbstractBaseDataNode) {
                GridMapKey updateKey = GridExtensionManager
                        .getUpdateKey((AbstractBaseDataNode) (dependency.node));
                if (updateKey != null) {
                    Set<UpdateValue> set = updateMap.get(updateKey);
                    if (set == null) {
                        set = new HashSet<>();
                        updateMap.put(updateKey, set);
                    }
                    set.add(new UpdateValue(dependency.timeOffset, node));
                }
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

    @Override
    public synchronized void update(GridRecord record) {
        super.update(record);
        GridMapKey updateKey = new GridMapKey(record);
        GridTimeCache.getInstance().clearTimes(updateKey);
        Set<UpdateValue> set = updateMap.get(updateKey);
        if (set == null) {
            return;
        }
        for (UpdateValue value : set) {
            /*
             * A record in an ambiguous state. It may be valid and derivable or
             * it may be missing dependencies and completely fake. It's
             * impossible to know which state it is in without looking in the
             * database to determine availability of all dependencies. Since
             * many updates are not used it doesn't make sense to determine the
             * real state of the record here and it is left to the receiver of
             * updates to figure it out.
             */
            GridRecord schrödingersRecord = new GridRecord();
            DataTime time = record.getDataTime();
            schrödingersRecord.setDataTime(new DataTime(time.getRefTime(),
                    time.getFcstTime() - value.timeOffset));
            schrödingersRecord.setDatasetId(value.node.getModelName());

            Parameter param = new Parameter(
                    value.node.getDesc().getAbbreviation(),
                    value.node.getDesc().getName(),
                    value.node.getDesc().getUnit());
            schrödingersRecord.setParameter(param);
            schrödingersRecord.setLevel(value.node.getLevel());
            if (value.node instanceof GatherLevelNode) {
                schrödingersRecord.setEnsembleId(null);
            } else {
                schrödingersRecord.setEnsembleId(record.getEnsembleId());
            }
            schrödingersRecord.setSecondaryId(record.getSecondaryId());
            schrödingersRecord.setLocation(record.getLocation());
            try {
                uriUpdateQueue.put(schrödingersRecord.getDataURI());
            } catch (InterruptedException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to send derived update for "
                                + schrödingersRecord.getDataURI(),
                        e);
            }
        }
        sendDerivedAlerts.schedule();
    }

    public synchronized void refreshNodes() {
        GridTimeCache.getInstance().flush();
        Set<AbstractDerivedDataNode> oldNodes = new HashSet<>();
        for (Set<UpdateValue> values : updateMap.values()) {
            for (UpdateValue value : values) {
                oldNodes.add(value.node);
            }
        }
        updateMap.clear();
        for (AbstractDerivedDataNode node : oldNodes) {
            // Get Node will automatically add this to the updater.
            inventory.getNode(node.getModelName(),
                    node.getDesc().getAbbreviation(), node.getLevel());
        }
    }

}
