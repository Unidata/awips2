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
package com.raytheon.uf.edex.datadelivery.bandwidth;

import java.util.Calendar;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.BandwidthInitializer;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.ISubscriptionAggregator;

/**
 * Defines the interface of a BandwidthManager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2012 1286       djohnson     Initial creation
 * Jul 10, 2013 2106       djohnson     Remove EDEX instance specific methods.
 * 10/23/2013   2385       bphillip     Change schedule method to scheduleAdhoc
 * Jan 06, 2014 2636       mpduff       Update javadoc
 * Jan 08, 2014 2615       bgonzale     Added scheduleAdoc method.
 * Jan 29, 2014 2636       mpduff       Scheduling refactor.
 * Feb 06, 2014 2636       bgonzale     added initializeScheduling method.
 * Fev 12, 2014 2636       mpduff       Add updateSchedule method.
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IBandwidthManager<T extends Time, C extends Coverage> {

    /**
     * Schedule retrievals for Subscriptions in the list.
     * 
     * @param subscription
     * @return A map of bandwidth allocations that are not scheduled by
     *         subscription name
     */
    List<BandwidthAllocation> schedule(Subscription<T, C> subscription);

    /**
     * Update the retrieval plan scheduling.
     * 
     * @param Network
     *            the network to update
     * 
     * @return number of subscriptions processed
     */
    int updateSchedule(Network network);

    /**
     * Schedule AdhocSubscription to run as soon as the RetrievalPlan will
     * allow.
     * 
     * @param subscription
     * @param b
     * @return
     */
    List<BandwidthAllocation> scheduleAdhoc(AdhocSubscription<T, C> subscription);

    /**
     * Schedule AdhocSubscription to run at the given time 'now'.
     * 
     * @param subscription
     * @param b
     * @return
     */
    List<BandwidthAllocation> scheduleAdhoc(
            AdhocSubscription<T, C> subscription, Calendar now);

    /**
     * When a Subscription is updated in the Registry, update the retrieval plan
     * accordingly to match the updated Subscription.
     * 
     * @param subscription
     * @return
     * @throws SerializationException
     */
    List<BandwidthAllocation> subscriptionUpdated(
            Subscription<T, C> subscription) throws SerializationException;

    /**
     * 
     * @param adhoc
     * @return
     */
    List<BandwidthAllocation> adhocSubscription(AdhocSubscription<T, C> adhoc);

    void setAggregator(ISubscriptionAggregator aggregator);

    ISubscriptionAggregator getAggregator();

    /**
     * Load the empty bandwidth tables with current active subscription data.
     * 
     * @param aConfig
     */
    void init();

    /**
     * @param initializer
     *            the initializer to set
     */
    void setInitializer(BandwidthInitializer initializer);

    /**
     * @return the initializer
     */
    BandwidthInitializer getInitializer();

    /**
     * Called after a BandwidthManager has been created to initialize scheduling
     * with the given subscriptions in preparation for operation.
     * 
     * @param subMap
     *            map of subscriptions to initialize scheduling with
     * @throws SerializationException
     * 
     * @Returns a list of the names of the subscriptions that were not
     *          scheduled.
     */
    List<String> initializeScheduling(Map<Network, List<Subscription>> subMap)
            throws SerializationException;
}