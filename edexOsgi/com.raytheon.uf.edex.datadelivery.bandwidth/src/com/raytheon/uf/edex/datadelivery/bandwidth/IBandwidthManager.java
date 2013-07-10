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

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IBandwidthManager {

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
    List<BandwidthAllocation> subscriptionUpdated(Subscription subscription)
            throws SerializationException;

    /**
     * 
     * @param adhoc
     * @return
     */
    List<BandwidthAllocation> adhocSubscription(AdhocSubscription adhoc);

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