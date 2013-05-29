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
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSharedSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSharedSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.PendingSiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSiteSubscriptionFixture;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.edex.registry.ebxml.dao.AbstractRegistryTest;

/**
 * Test {@link PendingSubscriptionHandler}.
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
public class PendingSubscriptionHandlerTest extends AbstractRegistryTest {

    @Test
    public void canDeleteMixedSiteAndPendingSharedSubscriptions()
            throws RegistryHandlerException {

        final PendingSiteSubscription siteSubscription = PendingSiteSubscriptionFixture.INSTANCE
                .get();
        final PendingSharedSubscription sharedSubscription = PendingSharedSubscriptionFixture.INSTANCE
                .get();

        storeSubscriptionsForPendingAssociations(siteSubscription,
                sharedSubscription);

        final IPendingSubscriptionHandler pendingSubHandler = DataDeliveryHandlers
                .getPendingSubscriptionHandler();
        pendingSubHandler.store(siteSubscription);
        pendingSubHandler.store(sharedSubscription);

        List<InitialPendingSubscription> subscriptions = Lists
                .<InitialPendingSubscription> newArrayList(siteSubscription,
                        sharedSubscription);

        pendingSubHandler.delete(subscriptions);

        assertThat(pendingSubHandler.getAll(), is(empty()));
    }

    @Test
    public void canDeleteMixedSiteAndPendingSharedSubscriptionsWithUsername()
            throws RegistryHandlerException {

        final PendingSiteSubscription siteSubscription = PendingSiteSubscriptionFixture.INSTANCE
                .get();
        final PendingSharedSubscription sharedSubscription = PendingSharedSubscriptionFixture.INSTANCE
                .get();

        storeSubscriptionsForPendingAssociations(siteSubscription,
                sharedSubscription);

        final IPendingSubscriptionHandler pendingSubHandler = DataDeliveryHandlers
                .getPendingSubscriptionHandler();
        pendingSubHandler.store(siteSubscription);
        pendingSubHandler.store(sharedSubscription);

        List<InitialPendingSubscription> subscriptions = Lists
                .<InitialPendingSubscription> newArrayList(siteSubscription,
                        sharedSubscription);

        pendingSubHandler.delete("joeSchmo", subscriptions);

        assertThat(pendingSubHandler.getAll(), is(empty()));
    }

    private void storeSubscriptionsForPendingAssociations(
            final PendingSiteSubscription siteSubscription,
            final PendingSharedSubscription sharedSubscription)
            throws RegistryHandlerException {

        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(siteSubscription.subscription());
        subscriptionHandler.store(sharedSubscription.subscription());
    }

}
