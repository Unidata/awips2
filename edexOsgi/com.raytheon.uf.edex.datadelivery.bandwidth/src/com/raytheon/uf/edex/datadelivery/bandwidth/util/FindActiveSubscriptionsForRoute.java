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
package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.edex.datadelivery.bandwidth.hibernate.IFindSubscriptionsForScheduling;

/**
 * Returns active subscriptions in the registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2013 1543       djohnson     Initial creation
 * Jul 09, 2013 2106       djohnson     Dependency inject registry handlers.
 * Jan 29, 2014 2636       mpduff       Scheduling refactor.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class FindActiveSubscriptionsForRoute implements
        IFindSubscriptionsForScheduling {

    private final ISubscriptionHandler subscriptionHandler;

    private final Network[] routes;

    /**
     * Find active subscriptions for a specific route.
     * 
     * @param subscriptionHandler
     *            the subscription handler
     * @param route
     *            the route
     */
    public FindActiveSubscriptionsForRoute(
            ISubscriptionHandler subscriptionHandler, Network route) {
        this(subscriptionHandler, new Network[] { route });
    }

    /**
     * Find active subscriptions for specific routes.
     * 
     * @param subscriptionHandler
     *            the subscription handler
     * @param routes
     *            the routes
     */
    public FindActiveSubscriptionsForRoute(
            ISubscriptionHandler subscriptionHandler, Network... routes) {
        this.subscriptionHandler = subscriptionHandler;
        this.routes = routes;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<Network, List<Subscription>> findSubscriptionsToSchedule()
            throws RegistryHandlerException {
        return subscriptionHandler.getActiveForRoutes(routes);
    }
}
