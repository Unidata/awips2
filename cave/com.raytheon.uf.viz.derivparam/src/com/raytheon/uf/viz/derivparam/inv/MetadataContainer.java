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
package com.raytheon.uf.viz.derivparam.inv;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequestSet;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponseSet;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode.Dependency;

/**
 * Class is responsible for pulling AbstractRequestableData out of
 * AbstractRequestableNode objects. This class manages caching and bulk
 * requesting to ensure that derived parameter requests are fully optimized.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class MetadataContainer {

    protected final Map<String, RequestConstraint> originalConstraints;

    protected final AvailabilityContainer availabilityContainer;

    protected Map<AbstractRequestableNode, Set<TimeAndSpace>> availCache = new HashMap<AbstractRequestableNode, Set<TimeAndSpace>>();

    protected Map<AbstractRequestableNode, Set<AbstractRequestableData>> dataCache = new HashMap<AbstractRequestableNode, Set<AbstractRequestableData>>();

    public MetadataContainer(
            Map<String, RequestConstraint> originalConstraints,
            AvailabilityContainer availabilityContainer) {
        this.originalConstraints = originalConstraints;
        this.availabilityContainer = availabilityContainer;
    }

    /**
     * This will prepare the container to request data for the given node. If
     * this node is a derived node it will evaluate what data is needed so that
     * when getData is called it can bulk request all data. If data is needed
     * for multiple nodes than calling this multiple times before ever calling
     * getData will maximize the efficiency of bulk requests.
     * 
     * @param node
     * @param availability
     * @throws VizException
     */
    public synchronized void prepareRequests(AbstractRequestableNode node,
            Set<TimeAndSpace> availability) throws VizException {
        generateAvailability(node, availability);
    }

    /**
     * get the requestable data for a node with the given availability. For
     * non-derived nodes this is simple, for derived nodes this container will
     * attempt to fully optimize and cache all data requests.
     * 
     * @param node
     * @param availability
     * @return
     * @throws VizException
     */
    public synchronized Set<AbstractRequestableData> getData(
            AbstractRequestableNode node, Set<TimeAndSpace> availability)
            throws VizException {
        return getData(node, availability, true);
    }

    /**
     * recursively called to get data, but don't reprocess availability at every
     * level, only for the first in the chain of recursive calls.
     * 
     * @param node
     * @param availability
     * @param doRequests
     * @return
     * @throws VizException
     */
    private Set<AbstractRequestableData> getData(AbstractRequestableNode node,
            Set<TimeAndSpace> availability, boolean doRequests)
            throws VizException {
        if (dataCache.containsKey(node)) {
            return dataCache.get(node);
        } else if (availability == null || availability.isEmpty()) {
            Set<AbstractRequestableData> data = Collections.emptySet();
            dataCache.put(node, data);
            return data;
        }
        if (doRequests) {
            generateAvailability(node, availability);
            processRequests();
        }
        if (node instanceof AbstractBaseDataNode) {
            if (dataCache.containsKey(node)) {
                return dataCache.get(node);
            } else {
                throw new IllegalStateException(
                        "DataNode should have been handled in processRequests");
            }
        } else if (node instanceof AbstractDerivedDataNode) {
            AbstractDerivedDataNode dataNode = (AbstractDerivedDataNode) node;
            for (Dependency d : dataNode.getDependencies()) {
                getData(d.node, availCache.get(d.node), false);
            }
            Set<AbstractRequestableData> data = dataNode.getData(availability,
                    dataCache);
            dataCache.put(node, data);
            return data;
        } else {
            throw new IllegalStateException(
                    "Cannot determine availability of unknown Unknown node type : "
                            + node.getClass().getSimpleName());
        }
    }

    /**
     * Walk through all dependencies for a node and calculate the TimeAndSpace
     * where data is needed to get data for the requested TimeAndSpace.
     * 
     * @param node
     * @param availability
     * @throws VizException
     */
    private void generateAvailability(AbstractRequestableNode node,
            Set<TimeAndSpace> availability) throws VizException {
        Set<TimeAndSpace> c = availCache.get(node);
        if (c != null) {
            if (c.containsAll(availability)) {
                return;
            }
            c.addAll(availability);
        } else {
            c = new HashSet<TimeAndSpace>(availability);
            availCache.put(node, c);
        }
        if (node instanceof AbstractDerivedDataNode) {
            AbstractDerivedDataNode derivedNode = (AbstractDerivedDataNode) node;
            Map<AbstractRequestableNode, Set<TimeAndSpace>> avail = derivedNode
                    .getDataDependency(new HashSet<TimeAndSpace>(availability),
                            availabilityContainer);

            for (Entry<AbstractRequestableNode, Set<TimeAndSpace>> entry : avail
                    .entrySet()) {
                AbstractRequestableNode n = entry.getKey();
                Set<TimeAndSpace> a = entry.getValue();
                generateAvailability(n, a);
            }
        }
    }

    /**
     * use the availCache to determine what nodes need data and attempt to bulk
     * all requests.
     * 
     * @throws VizException
     */
    protected void processRequests() throws VizException {
        List<AbstractBaseDataNode> nodes = new ArrayList<AbstractBaseDataNode>();
        List<DbQueryRequest> requests = new ArrayList<DbQueryRequest>();
        for (AbstractRequestableNode node : availCache.keySet()) {
            if (dataCache.containsKey(node)) {
                continue;
            }
            if (node instanceof AbstractBaseDataNode) {
                AbstractBaseDataNode dataNode = (AbstractBaseDataNode) node;
                DbQueryRequest request = dataNode.getDataRequest(
                        originalConstraints, availCache.get(node));
                if (request != null) {
                    nodes.add(dataNode);
                    requests.add(request);
                } else {
                    dataCache.put(
                            node,
                            dataNode.getData(originalConstraints,
                                    availCache.get(node), null));
                }
            }
        }
        if (requests.isEmpty()) {
            return;
        }
        DbQueryRequestSet requestSet = new DbQueryRequestSet();
        requestSet.setQueries(requests.toArray(new DbQueryRequest[0]));
        DbQueryResponseSet responseSet = (DbQueryResponseSet) ThriftClient
                .sendRequest(requestSet);
        for (int i = 0; i < nodes.size(); i++) {
            AbstractBaseDataNode node = nodes.get(i);
            dataCache.put(node, node.getData(originalConstraints,
                    availCache.get(node), responseSet.getResults()[i]));
        }
    }
}
