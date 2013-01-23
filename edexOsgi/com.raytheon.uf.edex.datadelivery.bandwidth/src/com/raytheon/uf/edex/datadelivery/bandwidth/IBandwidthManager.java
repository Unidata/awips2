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

import java.text.ParseException;
import java.util.List;

import com.google.common.eventbus.AllowConcurrentEvents;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.event.InsertRegistryEvent;
import com.raytheon.uf.common.registry.event.RemoveRegistryEvent;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.BandwidthInitializer;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.ISubscriptionAggregator;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.SubscriptionRetrievalFulfilled;

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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IBandwidthManager {

    /**
     * Create a hook into the EDEX Notification sub-system to receive the the
     * necessary InsertRegistryEvents to drive Bandwidth Management.
     * 
     * @param re
     *            The <code>InsertRegistryEvent</code> Object to evaluate.
     */
    @Subscribe
    void registryEventListener(InsertRegistryEvent re);

    /**
     * Persist the DataSetMetaData update and store the necessary statistical
     * data's to generate the predictive update time for a dataset.
     * 
     * @param dataSetMetaData
     * @throws ParseException
     */
    @Subscribe
    void updateDataSetMetaData(DataSetMetaData dataSetMetaData)
            throws ParseException;

    /**
     * When a Subscription is removed from the Registry, a RemoveRegistryEvent
     * is generated and forwarded to this method to remove the necessary
     * BandwidthReservations (and perhaps redefine others).
     * 
     * @param event
     */
    @Subscribe
    @AllowConcurrentEvents
    void subscriptionRemoved(RemoveRegistryEvent event);

    /**
     * Schedule all cycles of a Subscription.
     * 
     * @param subscription
     * @return A list of bandwidth allocations that are not scheduled
     */
    List<BandwidthAllocation> schedule(Subscription subscription);

    /**
     * Schedule AdhocSubscription to run as soon as the RetrievalPlan will
     * allow.
     * 
     * @param subscription
     * @param b
     * @return
     */
    List<BandwidthAllocation> schedule(AdhocSubscription subscription);

    /**
     * When a Subscription is updated in the Registry, update the retrieval plan
     * accordingly to match the updated Subscription.
     * 
     * @param subscription
     * @return
     * @throws SerializationException
     */
    @Subscribe
    List<BandwidthAllocation> subscriptionUpdated(Subscription subscription)
            throws SerializationException;

    /**
     * 
     * @param adhoc
     * @return
     */
    List<BandwidthAllocation> adhocSubscription(AdhocSubscription adhoc);

    /**
     * The callback method for BandwidthEventBus to use to notify
     * BandwidthManager that retrievalManager has completed the retrievals for a
     * Subscription. The updated SubscriptionDao Object is placed on the
     * BandwidthEventBus.
     * 
     * @param subscription
     *            The completed subscription.
     */
    @Subscribe
    void subscriptionFulfilled(
            SubscriptionRetrievalFulfilled subscriptionRetrievalFulfilled);

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
}