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
package com.raytheon.uf.common.datadelivery.registry;

import static org.junit.Assert.assertEquals;

import org.junit.Test;


/**
 * Test {@link PendingSubscription}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2012 0743       djohnson     Initial creation
 * Mar 28, 2013 1841       djohnson     Subscription is now UserSubscription.
 * May 15, 2013 1040       mpduff       Changed to handle the list of office ids.
 * Oct 21, 2013   2292     mpduff       Implement multiple data types
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class PendingSubscriptionTest {

    @Test
    public void testCopyConstructorSetsOriginalSubNameAsName() {
        SiteSubscription subscription = SiteSubscriptionFixture.INSTANCE
                .get(DataType.GRID);

        PendingSiteSubscription pendingSubscription = new PendingSiteSubscription(
                subscription, "djohnson");
        assertEquals(
                "The original subscription name should have been used for the pending subscription!",
                subscription.getName(), pendingSubscription.getName());
    }

    @Test
    public void testCopyConstructorSetsSubscriptionValuesOnPendingSubscription() {
        SiteSubscription subscription = SiteSubscriptionFixture.INSTANCE
                .get(DataType.GRID);

        PendingSiteSubscription copied = new PendingSiteSubscription(
                subscription, "djohnson");

        assertEquals(subscription.getActivePeriodEnd(),
                copied.getActivePeriodEnd());
        assertEquals(subscription.getActivePeriodStart(),
                copied.getActivePeriodStart());
        assertEquals(subscription.getCoverage(), copied.getCoverage());
        assertEquals(subscription.getDataSetName(), copied.getDataSetName());
        assertEquals(subscription.getDataSetSize(), copied.getDataSetSize());
        assertEquals(subscription.getDataSetType(), copied.getDataSetType());
        assertEquals(subscription.getDescription(), copied.getDescription());
        assertEquals(subscription.getGroupName(), copied.getGroupName());
        assertEquals(subscription.getOfficeIDs(), copied.getOfficeIDs());
        assertEquals(subscription.getPriority(), copied.getPriority());
        assertEquals(subscription.getProvider(), copied.getProvider());
        assertEquals(subscription.getStatus(), copied.getStatus());
        assertEquals(subscription.getSubscriptionEnd(),
                copied.getSubscriptionEnd());
        assertEquals(subscription.getSubscriptionStart(),
                copied.getSubscriptionStart());
        assertEquals(subscription.getSubscriptionId(),
                copied.getSubscriptionId());
        assertEquals(subscription.getTime(), copied.getTime());
        assertEquals(subscription.getUrl(), copied.getUrl());
    }
}
