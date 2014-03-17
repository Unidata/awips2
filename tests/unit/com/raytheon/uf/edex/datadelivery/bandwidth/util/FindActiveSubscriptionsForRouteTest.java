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

import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBuilder;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;

/**
 * Test {@link FindActiveSubscriptionsForRoute}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2013 1543       djohnson     Initial creation
 * Mar 28, 2013 1841       djohnson     Subscription is now UserSubscription.
 * Jul 10, 2013 2106       djohnson     Inject subscriptionHandler.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class FindActiveSubscriptionsForRouteTest {
    private static ISubscriptionHandler subscriptionHandler;

    @BeforeClass
    public static void classSetUp() throws RegistryHandlerException {
        RegistryObjectHandlersUtil.initMemory();
        subscriptionHandler = DataDeliveryHandlers.getSubscriptionHandler();

        // Two OPSNET subscriptions
        final SiteSubscription opsnetSub1 = new SubscriptionBuilder()
                .withName("opsnetSub1").withRoute(Network.OPSNET).build();
        final SiteSubscription opsnetSub2 = new SiteSubscription(opsnetSub1,
                "opsnetSub2");

        // Two SBN subscriptions
        final SiteSubscription sbnSub1 = new SubscriptionBuilder()
                .withName("sbnSub1").withRoute(Network.SBN).build();
        final SiteSubscription sbnSub2 = new SiteSubscription(sbnSub1,
                "sbnSub2");

        // Store all subscriptions
        for (Subscription sub : new Subscription[] { opsnetSub1, opsnetSub2,
                sbnSub1, sbnSub2 }) {
            subscriptionHandler.store(sub);
        }
    }

    @Test
    public void findsSubscriptionForSingleRoute()
            throws RegistryHandlerException {
        // final Set<Subscription> subscriptions = new
        // FindActiveSubscriptionsForRoute(
        // subscriptionHandler, Network.SBN).findSubscriptionsToSchedule();
        // assertThat(subscriptions, hasSize(2));
        // for (Subscription subscription : subscriptions) {
        // assertThat(subscription.getRoute(), is(Network.SBN));
        // }
    }

    @Test
    public void findsSubscriptionsForMultipleRoutes()
            throws RegistryHandlerException {
        // final Set<Subscription> subscriptions = new
        // FindActiveSubscriptionsForRoute(
        // subscriptionHandler, Network.OPSNET, Network.SBN)
        // .findSubscriptionsToSchedule();
        // assertThat(subscriptions, hasSize(4));
    }

}
