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
package com.raytheon.uf.edex.esb.camel.quartz;

import java.util.Collection;
import java.util.concurrent.ThreadPoolExecutor;

import org.apache.camel.CamelContext;
import org.apache.camel.Component;
import org.apache.camel.Endpoint;
import org.apache.camel.ErrorHandlerFactory;
import org.apache.camel.Processor;
import org.apache.camel.Route;
import org.apache.camel.Service;
import org.apache.camel.VetoCamelContextStartException;
import org.apache.camel.impl.DefaultCamelContext;
import org.apache.camel.spi.LifecycleStrategy;
import org.apache.camel.spi.RouteContext;

/**
 * Ensures that the management name field of the camel context is set. If it is
 * found to be null, it will be populated with the name of the context. This is
 * to work around a limitation in the QuartzComponent that it uses the
 * management name as a hash key for contexts.
 * https://issues.apache.org/jira/browse/CAMEL-7132
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2014 2608       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ManagementNameLifecycleStrategy implements LifecycleStrategy {

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onContextStart(org.apache.camel.CamelContext)
     */
    @Override
    public void onContextStart(CamelContext context)
            throws VetoCamelContextStartException {
        if (context.getManagementName() == null
                && context instanceof DefaultCamelContext) {
            ((DefaultCamelContext) context)
                    .setManagementName(context.getName());
        }
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onContextStop(org.apache.camel.CamelContext)
     */
    @Override
    public void onContextStop(CamelContext context) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onComponentAdd(java.lang.String, org.apache.camel.Component)
     */
    @Override
    public void onComponentAdd(String name, Component component) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onComponentRemove(java.lang.String, org.apache.camel.Component)
     */
    @Override
    public void onComponentRemove(String name, Component component) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onEndpointAdd(org.apache.camel.Endpoint)
     */
    @Override
    public void onEndpointAdd(Endpoint endpoint) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onEndpointRemove(org.apache.camel.Endpoint)
     */
    @Override
    public void onEndpointRemove(Endpoint endpoint) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onServiceAdd(org.apache.camel.CamelContext, org.apache.camel.Service, org.apache.camel.Route)
     */
    @Override
    public void onServiceAdd(CamelContext context, Service service, Route route) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onServiceRemove(org.apache.camel.CamelContext, org.apache.camel.Service, org.apache.camel.Route)
     */
    @Override
    public void onServiceRemove(CamelContext context, Service service,
            Route route) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onRoutesAdd(java.util.Collection)
     */
    @Override
    public void onRoutesAdd(Collection<Route> routes) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onRoutesRemove(java.util.Collection)
     */
    @Override
    public void onRoutesRemove(Collection<Route> routes) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onRouteContextCreate(org.apache.camel.spi.RouteContext)
     */
    @Override
    public void onRouteContextCreate(RouteContext routeContext) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onErrorHandlerAdd(org.apache.camel.spi.RouteContext, org.apache.camel.Processor, org.apache.camel.ErrorHandlerFactory)
     */
    @Override
    public void onErrorHandlerAdd(RouteContext routeContext,
            Processor errorHandler, ErrorHandlerFactory errorHandlerBuilder) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onErrorHandlerRemove(org.apache.camel.spi.RouteContext, org.apache.camel.Processor, org.apache.camel.ErrorHandlerFactory)
     */
    @Override
    public void onErrorHandlerRemove(RouteContext routeContext,
            Processor errorHandler, ErrorHandlerFactory errorHandlerBuilder) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onThreadPoolAdd(org.apache.camel.CamelContext, java.util.concurrent.ThreadPoolExecutor, java.lang.String, java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public void onThreadPoolAdd(CamelContext camelContext,
            ThreadPoolExecutor threadPool, String id, String sourceId,
            String routeId, String threadPoolProfileId) {
    }

    /* (non-Javadoc)
     * @see org.apache.camel.spi.LifecycleStrategy#onThreadPoolRemove(org.apache.camel.CamelContext, java.util.concurrent.ThreadPoolExecutor)
     */
    @Override
    public void onThreadPoolRemove(CamelContext camelContext,
            ThreadPoolExecutor threadPool) {
    }

}
