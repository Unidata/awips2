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

import java.util.HashSet;
import java.util.Set;

import org.apache.camel.CamelContext;
import org.apache.camel.Endpoint;
import org.apache.camel.Route;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.Pair;

/**
 * Class to map a context to its required and dependent contexts.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2014 2726       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class DependencyNode {
    private final CamelContext context;

    /**
     * Contexts required by this context.
     */
    private final Set<CamelContext> requiredContexs = new HashSet<CamelContext>();

    /**
     * Contexts that depend on this context.
     */
    private final Set<CamelContext> dependentContexts = new HashSet<CamelContext>();

    public DependencyNode(CamelContext context) {
        this.context = context;
    }

    /**
     * Add a node who is dependent on this node. Applies linking in both
     * directions.
     * 
     * @param dNode
     */
    public void addDependentNode(DependencyNode dNode) {
        if (!requiredContexs.contains(dNode.context)) {
            dependentContexts.add(dNode.context);
            dNode.requiredContexs.add(context);
        } else {
            StringBuilder msg = new StringBuilder(300);
            msg.append("Circular CamelContext dependency detected between ")
                    .append(context.getName())
                    .append(" and ")
                    .append(dNode.context.getName())
                    .append(". Removing dependency, startup/shutdown may be incorrect, verify configuration. ");
            addRouteData(context, msg);
            addRouteData(dNode.context, msg);
            UFStatus.getHandler(DependencyNode.class).warn(msg.toString());
        }
    }

    public CamelContext getContext() {
        return context;
    }

    /**
     * Get all contexts that this context requires to be running.
     * 
     * @return
     */
    public Set<CamelContext> getRequiredContexts() {
        return requiredContexs;
    }

    /**
     * Get all contexts that depend on this context to be running.
     * 
     * @return
     */
    public Set<CamelContext> getDependentContexts() {
        return dependentContexts;
    }

    /**
     * Utility method for printing information about a context.
     * 
     * @param ctx
     * @param builder
     */
    private static void addRouteData(CamelContext ctx, StringBuilder builder) {
        builder.append("Context [").append(ctx.getName())
                .append("] consumes from [");
        Set<String> consumerEndpoints = new HashSet<String>();
        for (Route route : ctx.getRoutes()) {
            Endpoint endpoint = route.getEndpoint();
            String uri = endpoint.getEndpointUri();
            Pair<String, String> typeAndName = ContextData
                    .getEndpointTypeAndName(uri);
            String name = typeAndName.getFirst() + ":"
                    + typeAndName.getSecond();
            builder.append(name).append(", ");
            consumerEndpoints.add(name);
        }
        builder.delete(builder.length() - 2, builder.length());
        builder.append("] and produces to [");
        for (Endpoint endpoint : ctx.getEndpoints()) {
            String uri = endpoint.getEndpointUri();
            Pair<String, String> typeAndName = ContextData
                    .getEndpointTypeAndName(uri);
            String name = typeAndName.getFirst() + ":"
                    + typeAndName.getSecond();
            if (!consumerEndpoints.contains(name)) {
                builder.append(name).append(", ");
            }
        }
        builder.delete(builder.length() - 2, builder.length());
        builder.append("]. ");
    }
}