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

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.naming.ConfigurationException;

import org.apache.camel.CamelContext;
import org.apache.camel.Route;

import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Contains all known contexts and parsed data about the contexts.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2014 2726       rjpeter     Initial creation.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ContextData {
    private final List<CamelContext> contexts;

    private final Map<String, Route> consumerRouteMapping;

    private final Map<String, String> routeIdUriMapping;

    /**
     * Pulls the direct-vm:name, vm:name, queue:name, topic:name section from
     * the endpoint URI.
     */
    private static final Pattern endpointUriParsePattern = Pattern
            .compile("([^:]+)://([^?]+)");

    /**
     * Parses passed contexts for route and endpoint data about all contexts.
     * 
     * @param contexts
     * @throws ConfigurationException
     */
    public ContextData(List<CamelContext> contexts)
            throws ConfigurationException {
        this.contexts = Collections.unmodifiableList(contexts);
        this.consumerRouteMapping = Collections
                .unmodifiableMap(generateRouteMappings(this.contexts));
        Map<String, String> idUriMapping = new HashMap<String, String>(
                consumerRouteMapping.size(), 1);
        for (CamelContext ctx : this.contexts) {
            for (Route route : ctx.getRoutes()) {
                idUriMapping.put(route.getId(), route.getEndpoint()
                        .getEndpointUri());
            }
        }

        this.routeIdUriMapping = Collections.unmodifiableMap(idUriMapping);
    }

    /**
     * Populates an endpointName to {@code Route} mapping for the passed
     * {@code CamelContext}s.
     * 
     * @return
     * @throws ConfigurationException
     */
    protected static Map<String, Route> generateRouteMappings(
            List<CamelContext> contexts) throws ConfigurationException {
        Map<String, Route> routeMapping = new HashMap<String, Route>(
                contexts.size() * 2, 1);

        // populate the consumer definitions
        for (CamelContext context : contexts) {
            List<Route> routes = context.getRoutes();
            if ((routes != null) && (routes.size() > 0)) {
                for (Route route : routes) {
                    String uri = route.getEndpoint().getEndpointUri();
                    Pair<String, String> typeAndName = getEndpointTypeAndName(uri);
                    if (typeAndName != null) {
                        String endpointName = typeAndName.getSecond();

                        Route prev = routeMapping.put(endpointName, route);
                        if ((prev != null)
                                && !endpointName.startsWith("topic:")) {
                            throw new ConfigurationException(
                                    "Two contexts listen to the same endpoint name ["
                                            + endpointName
                                            + "].  ContextManager cannot handle this situation.  Double check configuration.  Conflicting contexts ["
                                            + prev.getRouteContext()
                                                    .getCamelContext()
                                                    .getName() + "] and ["
                                            + context.getName() + "]");
                        }
                    }
                }
            }
        }
        return routeMapping;
    }

    /**
     * Returns the known contexts.
     * 
     * @return
     */
    public List<CamelContext> getContexts() {
        return contexts;
    }

    /**
     * Parses URI for component type and endpoint name.
     * 
     * @param uri
     * @return
     */
    public static Pair<String, String> getEndpointTypeAndName(String uri) {
        Pair<String, String> rval = null;
        Matcher m = endpointUriParsePattern.matcher(uri);

        if (m.find()) {
            String endpointType = m.group(1);
            String endpointName = m.group(2);
            rval = new Pair<String, String>(endpointType, endpointName);
        }

        return rval;
    }

    /**
     * Scans the camel context and associated routes. Groups the routes by
     * consumer type.
     * 
     * @return
     */
    public Map<String, List<Route>> getContextRoutesByEndpointType()
            throws ConfigurationException {
        Map<String, List<Route>> routesByType = new HashMap<String, List<Route>>();
        for (CamelContext context : contexts) {
            List<Route> routes = context.getRoutes();
            if ((routes != null) && (routes.size() > 0)) {
                for (Route route : routes) {
                    String uri = route.getEndpoint().getEndpointUri();
                    Pair<String, String> typeAndName = getEndpointTypeAndName(uri);
                    String type = typeAndName.getFirst();
                    List<Route> routesForType = routesByType.get(type);
                    if (routesForType == null) {
                        routesForType = new LinkedList<Route>();
                        routesByType.put(type, routesForType);
                    }
                    routesForType.add(route);
                }
            }
        }

        return routesByType;
    }

    /**
     * Returns the uri for the consumer endpoint of the route with the specified
     * routeId.
     * 
     * @param routeId
     * @return
     * @throws EdexException
     */
    public String getEndpointUriForRouteId(String routeId) throws EdexException {
        String uri = routeIdUriMapping.get(routeId);
        if (uri == null) {
            throw new EdexException("Route id " + routeId
                    + " not found.  Check loaded spring configurations.");
        }

        return uri;
    }

    /**
     * Returns the route for the endpoint with the passed name as returned from
     * getEndpointTypeAndName().
     * 
     * @param endpointName
     * @return
     * @throws EdexException
     */
    public Route getRouteForEndpointName(String endpointName)
            throws EdexException {
        Route route = consumerRouteMapping.get(endpointName);
        if (route == null) {
            throw new EdexException("Endpoint " + endpointName
                    + " not found.  Check loaded spring configurations.");
        }

        return route;
    }
}
