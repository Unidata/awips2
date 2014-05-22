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
package com.raytheon.uf.edex.esb.camel.context;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.naming.ConfigurationException;

import org.apache.camel.CamelContext;
import org.apache.camel.Endpoint;
import org.apache.camel.Route;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.Pair;

/**
 * Contains context dependency mappings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2014 2726       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ContextDependencyMapping {
    /**
     * Endpoint types that should be tracked for dependency mapping
     */
    protected static final Set<String> DEPENDENCY_ENDPOINT_TYPES;

    static {
        /*
         * Endpoint types that are used for inner context routing. If we add
         * other inner jvm routing types, they should be added here.
         */
        Set<String> types = new HashSet<String>(8);
        types.add("vm");
        types.add("direct-vm");
        types.add("seda");
        types.add("jmx");
        types.add("guava-eventbus");
        DEPENDENCY_ENDPOINT_TYPES = Collections.unmodifiableSet(types);
    }

    /**
     * The dependency mappings.
     */
    protected final Map<CamelContext, DependencyNode> dependencyMapping;

    /**
     * Populates the dependency mappings for all camel contexts.
     * {@code suppressExceptions} can be used to differentiate between
     * startup/shutdown conditions to allow the map to be populated regardless
     * of detected issues.
     * 
     * @param contextData
     * @param suppressExceptions
     * @throws ConfigurationException
     */
    public ContextDependencyMapping(ContextData contextData,
            boolean suppressExceptions) throws ConfigurationException {
        dependencyMapping = Collections
                .unmodifiableMap(populateDependencyMapping(contextData,
                        suppressExceptions));
    }

    /**
     * Returns a {@code IUFStatusHandler}. Not cached as rarely used.
     * 
     * @return
     */
    private static IUFStatusHandler getHandler() {
        return UFStatus.getHandler(ContextDependencyMapping.class);
    }

    /**
     * Dependency mappings per context. The dependency mapping is only for
     * internal vm types that have a direct dependency. Indirect dependency via
     * a JMS queue for example is not returned/enforced.
     * 
     * @param contextData
     * @param suppressExceptions
     *            Done in a shutdown scenario to get the dependencyMapping as
     *            close as possible.
     */
    protected static Map<CamelContext, DependencyNode> populateDependencyMapping(
            ContextData contextData, boolean suppressExceptions)
            throws ConfigurationException {
        List<CamelContext> contexts = contextData.getContexts();
        Map<CamelContext, DependencyNode> dependencyMapping = new LinkedHashMap<CamelContext, DependencyNode>(
                contexts.size());

        // set up dependency nodes for internal types
        Map<String, CamelContext> consumesFrom = new HashMap<String, CamelContext>();
        Map<String, List<CamelContext>> producesTo = new HashMap<String, List<CamelContext>>();
        Set<String> consumers = new HashSet<String>();

        // scan for consuming and producing internal endpoints
        for (CamelContext context : contexts) {
            dependencyMapping.put(context, new DependencyNode(context));
            consumers.clear();
            List<Route> routes = context.getRoutes();
            if ((routes != null) && (routes.size() > 0)) {
                for (Route route : routes) {
                    String uri = route.getEndpoint().getEndpointUri();
                    Pair<String, String> typeAndName = ContextData
                            .getEndpointTypeAndName(uri);
                    if ((typeAndName != null)
                            && DEPENDENCY_ENDPOINT_TYPES.contains(typeAndName
                                    .getFirst())) {
                        String endpointName = typeAndName.getSecond();
                        consumers.add(endpointName);

                        /*
                         * Internal types don't support a fanout type policy
                         * where multiple routes can listen to the same
                         * endpoint.
                         */
                        CamelContext prev = consumesFrom.put(endpointName,
                                context);
                        if (prev != null) {
                            String msg = "Two contexts listen to the same internal endpoint ["
                                    + endpointName
                                    + "].  ContextManager cannot handle this situation.  Double check configuration.  Conflicting contexts ["
                                    + prev.getName()
                                    + "] and ["
                                    + context.getName() + "]";
                            if (suppressExceptions) {
                                getHandler().error(msg);
                            } else {
                                throw new ConfigurationException(msg);
                            }
                        }
                    }
                }
            }

            Collection<Endpoint> endpoints = context.getEndpoints();
            if ((endpoints != null) && (endpoints.size() > 0)) {
                for (Endpoint ep : endpoints) {
                    String uri = ep.getEndpointUri();
                    Pair<String, String> typeAndName = ContextData
                            .getEndpointTypeAndName(uri);
                    if ((typeAndName != null)
                            && DEPENDENCY_ENDPOINT_TYPES.contains(typeAndName
                                    .getFirst())) {
                        String endpointName = typeAndName.getSecond();
                        if (!consumers.contains(endpointName)) {
                            List<CamelContext> producerCtxs = producesTo
                                    .get(endpointName);
                            if (producerCtxs == null) {
                                producerCtxs = new LinkedList<CamelContext>();
                                producesTo.put(endpointName, producerCtxs);
                            }
                            producerCtxs.add(context);
                        }
                    }
                }
            }
        }

        // setup dependencies for internal routes
        for (Map.Entry<String, List<CamelContext>> producersEntry : producesTo
                .entrySet()) {
            String endpoint = producersEntry.getKey();
            CamelContext consumer = consumesFrom.get(endpoint);
            List<CamelContext> producers = producersEntry.getValue();

            if (consumer == null) {
                StringBuilder msg = new StringBuilder(200);
                msg.append("Internal Routing Endpoint [")
                        .append(endpoint)
                        .append("] has no defined consumers.  This is endpoint is used in contexts [");
                Iterator<CamelContext> producerIter = producers.iterator();

                while (producerIter.hasNext()) {
                    CamelContext producer = producerIter.next();
                    msg.append(producer.getName());

                    if (producerIter.hasNext()) {
                        msg.append(", ");
                    }
                }

                msg.append("]");
                if (suppressExceptions) {
                    getHandler().error(msg.toString());
                } else {
                    throw new ConfigurationException(msg.toString());
                }
            } else {
                DependencyNode consumerNode = dependencyMapping.get(consumer);
                for (CamelContext producer : producers) {
                    DependencyNode producerNode = dependencyMapping
                            .get(producer);
                    consumerNode.addDependentNode(producerNode);
                }
            }
        }
        return dependencyMapping;
    }

    /**
     * Get the contexts that depend upon the passed context to work. If the
     * passed context is unknown null will be returned.
     * 
     * @param context
     * @return
     */
    public Set<CamelContext> getDependentContexts(CamelContext context) {
        DependencyNode dNode = dependencyMapping.get(context);
        if (dNode == null) {
            return null;
        }

        return dNode.getDependentContexts();
    }

    /**
     * Get the contexts that the passed context requires to be running to work.
     * If the passed context is unknown null will be returned.
     * 
     * @param context
     * @return
     */
    public Set<CamelContext> getRequiredContexts(CamelContext context) {
        DependencyNode dNode = dependencyMapping.get(context);
        if (dNode == null) {
            return null;
        }

        return dNode.getRequiredContexts();
    }
}
