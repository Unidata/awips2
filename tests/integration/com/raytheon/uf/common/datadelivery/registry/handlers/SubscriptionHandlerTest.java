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
package com.raytheon.uf.common.datadelivery.registry.handlers;

import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import java.util.List;

import org.junit.Test;

import com.google.common.collect.Lists;
import com.raytheon.uf.common.datadelivery.registry.SharedSubscription;
import com.raytheon.uf.common.datadelivery.registry.SharedSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.edex.registry.ebxml.dao.AbstractRegistryTest;

/**
 * Test {@link SubscriptionHandler}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2013 1650       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionHandlerTest extends AbstractRegistryTest {

    @Test
    public void canDeleteMixedSiteAndSharedSubscriptions()
            throws RegistryHandlerException {
        final SiteSubscription siteSubscription = SiteSubscriptionFixture.INSTANCE
                .get();
        final SharedSubscription sharedSubscription = SharedSubscriptionFixture.INSTANCE
                .get();

        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(siteSubscription);
        subscriptionHandler.store(sharedSubscription);

        List<Subscription> subscriptions = Lists.<Subscription> newArrayList(
                siteSubscription, sharedSubscription);

        subscriptionHandler.delete(subscriptions);

        assertThat(subscriptionHandler.getAll(), is(empty()));
    }

    @Test
    public void canDeleteMixedSiteAndSharedSubscriptionsWithUsername()
            throws RegistryHandlerException {
        final SiteSubscription siteSubscription = SiteSubscriptionFixture.INSTANCE
                .get();
        final SharedSubscription sharedSubscription = SharedSubscriptionFixture.INSTANCE
                .get();

        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(siteSubscription);
        subscriptionHandler.store(sharedSubscription);

        List<Subscription> subscriptions = Lists.<Subscription> newArrayList(
                siteSubscription, sharedSubscription);

        subscriptionHandler.delete("joeSchmo", subscriptions);

        assertThat(subscriptionHandler.getAll(), is(empty()));
    }
}
