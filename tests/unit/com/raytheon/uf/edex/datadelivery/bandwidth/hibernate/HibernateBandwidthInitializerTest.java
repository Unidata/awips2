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
package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import org.junit.Test;

import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;

/**
 * Test {@link HibernateBandwidthInitializer}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2013 1543       djohnson     Initial creation
 * Apr 18, 2013 1914       djohnson     Fix broken test.
 * Jun 25, 2013 2106       djohnson     init() now takes a {@link RetrievalManager}.
 * Sep 06, 2013 2344       bgonzale     Added property injection of valid test value.
 * Oct 21, 2013 2292       mpduff       Implement multiple data types.
 * Nov 04, 2013 2506       bgonzale     Added site parameter to HibernateBandwidthInitializer
 *                                      constructor.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class HibernateBandwidthInitializerTest {

    @Test
    public void testSchedulesAllSubscriptionReturnedFromIFindSubscriptions()
            throws Exception {
        // final Subscription subscription = SiteSubscriptionFixture.INSTANCE
        // .get(DataType.GRID);
        //
        // subscription.addOfficeID("OAX");
        // IFindSubscriptionsForScheduling strategy =
        // mock(IFindSubscriptionsForScheduling.class);
        // when(strategy.findSubscriptionsToSchedule()).thenReturn(
        // Sets.newHashSet(subscription));
        //
        // IBandwidthManager bandwidthManager = mock(IBandwidthManager.class);
        // IBandwidthDbInit dbInit = mock(IBandwidthDbInit.class);
        //
        // final HibernateBandwidthInitializer initializer = new
        // HibernateBandwidthInitializer(
        // strategy, "OAX");
        // initializer
        // .init(bandwidthManager, dbInit, mock(RetrievalManager.class));
        // initializer.executeAfterRegistryInit();
        //
        // verify(bandwidthManager).schedule(subscription, true);
    }

}
