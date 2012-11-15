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
package com.raytheon.uf.common.serialization.comm;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.registry.GenericRegistry;
import com.raytheon.uf.common.util.registry.RegistryException;

/**
 * Routes requests via {@link IRequestRouter}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2010             rjpeter     Initial creation
 * Nov 15, 2012 1322       djohnson    Add ability to route by server key.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public final class RequestRouter {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RequestRouter.class);

    static final String REQUEST_SERVICE = "request.server";

    /**
     * {@link GenericRegistry} implementation that holds {@link IRequestRouter}
     * instances keyed by their server. Intentionally package-private.
     */
    static class RouterRegistry extends GenericRegistry<String, IRequestRouter> {

        /**
         * {@inheritDoc}
         */
        @Override
        public Object register(String t, IRequestRouter s)
                throws RegistryException {
            if (registry.containsKey(t)) {
                throw new RegistryException("Unable to register router",
                        new IllegalStateException(
                        "IRequestRouter of type ["
                        + s.getClass().getName()
                                + "] already registered for key [" + t + "]"));
            }
            return super.register(t, s);
        }
        
        /**
         * Clear the registry.  Intentionally package-private so it can be cleared by test code.
         */
        void clear() {
            registry.clear();
        }
    };
    
    private static final RouterRegistry routerRegistry = new RouterRegistry();

    /**
     * Disabled constructor.
     */
    private RequestRouter() {
    }

    /**
     * Get the router registry.
     * 
     * @return the registry for routers
     */
    public static RouterRegistry getRouterRegistry() {
        return routerRegistry;
    }

    /**
     * Route a request to the request service router.
     * 
     * @param request
     *            the request
     * @return the response
     * @throws Exception
     */
    public static Object route(IServerRequest request) throws Exception {
        return route(request, REQUEST_SERVICE);
    }

    /**
     * Route a request using the router registered for the specific service.
     * 
     * @param request
     *            the request
     * @param service
     *            the service name
     * @return the response
     * @throws Exception
     */
    public static Object route(IServerRequest request, String service)
            throws Exception {
        final IRequestRouter router = routerRegistry.getRegisteredObject(service);
        if (router == null) {
            statusHandler
                    .error("There is no registered router for service ["
                            + service
                            + "].  Routing to the request service, but the request may not be able to be processed!");
            return route(request);
        } else {
            return router.route(request);
        }
    }
}
