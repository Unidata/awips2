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
import java.util.Arrays;
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
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponseSet;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode.Dependency;

/**
 * Object for determining the vailability(in time and space) when a
 * AbstractRequestableNode can provide data. Specifically the node recursively
 * determines all dependencies and bulks requests for performance reason,
 * maintaining a cache of time to avoid any redundant requests.
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

public class AvailabilityContainer {

    public static final Set<TimeAndSpace> AGNOSTIC_SET = Collections
            .unmodifiableSet(new HashSet<TimeAndSpace>(Arrays
                    .asList(new TimeAndSpace())));

    protected final Map<String, RequestConstraint> originalConstraints;

    protected Map<AbstractBaseDataNode, DbQueryRequest> requestCache = new HashMap<AbstractBaseDataNode, DbQueryRequest>();

    protected Map<AbstractRequestableNode, Set<TimeAndSpace>> availabilityCache = new HashMap<AbstractRequestableNode, Set<TimeAndSpace>>();

    public AvailabilityContainer(
            Map<String, RequestConstraint> originalConstraints) {
        this.originalConstraints = originalConstraints;
    }

    /**
     * Get the availability in Time and space for a given node.
     * 
     * @param node
     * @return
     * @throws VizException
     */
    public synchronized Set<TimeAndSpace> getAvailability(
            AbstractRequestableNode node) throws VizException {
        return getAvailability(node, true);
    }

    /**
     * recursively determine avaialbilty for a node and its dependencies,
     * doRequests should only be true for the first call, after that all
     * requests will have already been handled.
     * 
     * @param node
     * @param doRequests
     * @return
     * @throws VizException
     */
    private Set<TimeAndSpace> getAvailability(AbstractRequestableNode node,
            boolean doRequests) throws VizException {
        if (availabilityCache.containsKey(node)) {
            return availabilityCache.get(node);
        }
        if (doRequests) {
            prepareRequests(node);
            processRequests();
        }

        if (node instanceof AbstractBaseDataNode) {

            if (availabilityCache.containsKey(node)) {
                return availabilityCache.get(node);
            } else {
                throw new IllegalStateException(
                        "DataNode should have been handled in processRequests");
            }
        } else if (node instanceof AbstractDerivedDataNode) {
            AbstractDerivedDataNode dataNode = (AbstractDerivedDataNode) node;
            for (Dependency d : dataNode.getDependencies()) {
                getAvailability(d.node, false);
            }
            Set<TimeAndSpace> availability = dataNode
                    .getAvailability(availabilityCache);
            availabilityCache.put(node, availability);
            return availability;
        } else {
            throw new IllegalStateException(
                    "Cannot determine availability of unknown Unknown node type : "
                            + node.getClass().getSimpleName());
        }
    }

    /**
     * This will prepare the container to request availability for the given
     * node. If this node is a derived node it will evaluate what data is needed
     * so that when getData is called it can bulk request all data. If
     * availability is needed for multiple nodes than calling this multiple
     * times before ever calling getAvailability will maximize the efficiency of
     * bulk requests.
     * 
     * @param node
     * @param availability
     * @throws VizException
     */
    public synchronized void prepareRequests(AbstractRequestableNode node) {
        if (availabilityCache.containsKey(node)
                || requestCache.containsKey(node)) {
            return;
        }
        if (node instanceof AbstractBaseDataNode) {
            AbstractBaseDataNode dataNode = (AbstractBaseDataNode) node;
            requestCache.put(dataNode,
                    dataNode.getAvailabilityRequest(originalConstraints));
        } else if (node instanceof AbstractDerivedDataNode) {
            AbstractDerivedDataNode dataNode = (AbstractDerivedDataNode) node;
            for (Dependency d : dataNode.getDependencies()) {
                prepareRequests(d.node);
            }
        }

    }

    /**
     * process all requests, in bulk if possible.
     * 
     * @throws VizException
     */
    protected void processRequests() throws VizException {
        List<DbQueryRequest> requests = getAvailabilityRequests();
        DbQueryRequestSet requestSet = new DbQueryRequestSet();
        requestSet.setQueries(requests.toArray(new DbQueryRequest[0]));
        DbQueryResponseSet responseSet = (DbQueryResponseSet) ThriftClient
                .sendRequest(requestSet);
        DbQueryResponse[] responses = responseSet.getResults();
        Map<DbQueryRequest, DbQueryResponse> responseMap = new HashMap<DbQueryRequest, DbQueryResponse>(
                (int) (responses.length / 0.75) + 1, 0.75f);
        for (int i = 0; i < responses.length; i++) {
            responseMap.put(requests.get(i), responses[i]);
        }
        setAvailabilityResponses(responseMap);
    }

    public synchronized List<DbQueryRequest> getAvailabilityRequests() {
        List<DbQueryRequest> requests = new ArrayList<DbQueryRequest>(
                requestCache.size());
        for (Entry<AbstractBaseDataNode, DbQueryRequest> entry : requestCache
                .entrySet()) {
            if (availabilityCache.containsKey(entry.getKey())) {
                continue;
            } else if (entry.getValue() != null) {
                requests.add(entry.getValue());
            }
        }
        return requests;
    }

    public synchronized void setAvailabilityResponses(
            Map<DbQueryRequest, DbQueryResponse> responses) throws VizException {
        for (Entry<AbstractBaseDataNode, DbQueryRequest> entry : requestCache
                .entrySet()) {
            DbQueryResponse response = null;
            if (availabilityCache.containsKey(entry.getKey())) {
                continue;
            } else if (entry.getValue() != null) {
                response = responses.get(entry.getValue());
            }
            availabilityCache.put(entry.getKey(), entry.getKey()
                    .getAvailability(originalConstraints, response));
        }
    }

}
