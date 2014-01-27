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
package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.SortedSet;

import com.raytheon.uf.common.datadelivery.bandwidth.data.SubscriptionStatusSummary;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.edex.datadelivery.bandwidth.BandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;

/**
 * Extracted from {@link BandwidthContextFactory} so that
 * {@link BandwidthManager} can be run in memory (e.g. for testing proposed
 * bandwidth size limitations and informing the user which subscriptions would
 * be unable to be scheduled).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2012 1286       djohnson     Initial creation
 * Jun 03, 2013 2038       djohnson     Add method to get subscription retrievals by provider, dataset, and status.
 * Jun 13, 2013 2095       djohnson     Implement ability to store a collection of subscriptions.
 * Jun 24, 2013 2106       djohnson     Add more methods.
 * Jul 18, 2013 1653       mpduff       Added getSubscriptionStatusSummary.
 * Dec 17, 2013 2636       bgonzale     Added method to get a BandwidthAllocation.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IBandwidthDao<T extends Time, C extends Coverage> {

    /**
     * Get BandwidthAllocations.
     * 
     * @param subscriptionId
     *            Retrieve BandwidthAllocations with the specified subscription
     *            Id.
     * 
     * @return A List of BandwidthAllocations that have the specified
     *         subscription Id.
     */
    List<BandwidthAllocation> getBandwidthAllocations(Long subscriptionId);

    /**
     * Get BandwidthAllocations.
     * 
     * @param network
     *            Retrieve BandwidthAllocations with the specified network.
     * 
     * @return A List of BandwidthAllocations that have the specified network.
     */
    List<BandwidthAllocation> getBandwidthAllocations(Network network);

    /**
     * Get DataSetMetaDataDaos.
     * 
     * @param providerName
     *            Retrieve DataSetMetaDataDaos with the specified providerName.
     * @param dataSetName
     *            Retrieve DataSetMetaDataDaos with the specified dataSetName.
     * 
     * @return A List of DataSetMetaDataDaos that have the specified
     *         providerName and dataSetName.
     */
    List<BandwidthDataSetUpdate> getBandwidthDataSetUpdate(String providerName,
            String dataSetName);

    /**
     * Get DataSetMetaDataDaos.
     * 
     * @param providerName
     *            Retrieve DataSetMetaDataDaos with the specified providerName.
     * @param dataSetName
     *            Retrieve DataSetMetaDataDaos with the specified dataSetName.
     * @param baseReferenceTime
     *            Retrieve DataSetMetaDataDaos with the specified
     *            baseReferenceTime.
     * 
     * @return A List of DataSetMetaDataDaos that have the specified
     *         providerName, dataSetName and baseReferenceTime.
     */
    List<BandwidthDataSetUpdate> getBandwidthDataSetUpdate(String providerName,
            String dataSetName, Calendar baseReferenceTime);

    /**
     * Get BandwidthAllocations with a status of
     * {@link RetrievalStatus.DEFERRED}.
     * 
     * @return A List of BandwidthAllocations that have a status of
     *         {@link RetrievalStatus.DEFERRED}.
     */
    List<BandwidthAllocation> getDeferred(Network network, Calendar endTime);

    /**
     * Get a BandwidthSubscription.
     * 
     * @param identifier
     *            Retrieve the BandwidthSubscription with the specified
     *            identifier.
     * 
     * @return The BandwidthSubscription that has the specified identifier or
     *         null if no such BandwidthSubscription exists.
     */
    BandwidthSubscription getBandwidthSubscription(long identifier);

    /**
     * Get a BandwidthSubscription.
     * 
     * @param registryId
     *            Retrieve the BandwidthSubscription with the specified
     *            registryId.
     * @param baseReferenceTime
     *            Retrieve the BandwidthSubscription with the specified
     *            baseReferenceTime.
     * 
     * @return The BandwidthSubscription that has the specified identifier and
     *         baseReferenceTime or null if no such BandwidthSubscription
     *         exists.
     */
    BandwidthSubscription getBandwidthSubscription(String registryId,
            Calendar baseReferenceTime);

    /**
     * Get BandwidthSubscriptions.
     * 
     * @param subscription
     *            Retrieve BandwidthSubscriptions that match the specified
     *            subscription's owner, provider, name and dataSetName.
     * 
     * @return A List of BandwidthSubscriptions that have the same owner,
     *         provider, name and dataSetName and the specified subscription.
     */
    List<BandwidthSubscription> getBandwidthSubscription(
            Subscription<T, C> subscription);

    /**
     * Get a BandwidthSubscriptions.
     * 
     * @param registryId
     *            Retrieve the BandwidthSubscriptions with the specified
     *            registryId.
     * 
     * @return A List of BandwidthSubscriptions that has the specified
     *         registryId or null if no such BandwidthSubscription exists.
     */
    List<BandwidthSubscription> getBandwidthSubscriptionByRegistryId(
            String registryId);

    /**
     * Retrieve a SubscriptionRetrieval Object from the database given an
     * identifier.
     * 
     * @param identifier
     *            The identifier for the SubscriptionRetrieval record to return.
     * 
     * @return The SubscriptionRetrieval Object with the specified identifier or
     *         null if no Object has the specified identifier.
     */
    SubscriptionRetrieval getSubscriptionRetrieval(long identifier);

    /**
     * Get all the subscription retrievals for the specified dataset and base
     * reference time.
     * 
     * @param provider
     *            The provider name.
     * 
     * @param dataSetName
     *            The dataset name.
     * 
     * @param baseReferenceTime
     *            The base reference time.
     * 
     * @return All the SubscriptionRetrievals that are scheduled for the
     *         specified time.
     */
    List<SubscriptionRetrieval> getSubscriptionRetrievals(String provider,
            String dataSetName, Calendar baseReferenceTime);

    /**
     * Get all the subscription retrievals for the specified dataset, with the
     * specified status, and ordered by date.
     * 
     * @param provider
     *            The provider name.
     * 
     * @param dataSetName
     *            The dataset name.
     * 
     * @param status
     *            The status
     * 
     * @return the subscription retrievals
     */
    SortedSet<SubscriptionRetrieval> getSubscriptionRetrievals(String provider,
            String dataSetName, RetrievalStatus status);

    /**
     * Get all the subscription retrievals for the specified dataset, with the
     * specified status, ordered by date, with a start date between the two
     * specified dates (inclusive).
     * 
     * @param provider
     *            The provider name.
     * 
     * @param dataSetName
     *            The dataset name.
     * 
     * @param status
     *            The status
     * 
     * @param earliestDate
     *            the earliest date
     * 
     * @param latestDate
     *            the latest date
     * 
     * @return the subscription retrievals
     */
    SortedSet<SubscriptionRetrieval> getSubscriptionRetrievals(String provider,
            String dataSetName, RetrievalStatus status, Date earliestDate,
            Date latestDate);

    /**
     * Get all the subscription retrievals for the specified dataset and base
     * reference time.
     * 
     * @param provider
     *            The provider name.
     * 
     * @param dataSetName
     *            The dataset name.
     * 
     * @param baseReferenceTime
     *            The base reference time.
     * 
     * @return All the SubscriptionRetrievals that are scheduled for the
     *         specified time.
     */
    List<SubscriptionRetrieval> getSubscriptionRetrievals(String provider,
            String dataSetName);

    /**
     * Return all the BandwidthSubscription Objects in the database in ascending
     * order based on the BandwidthSubscription's baseReferenceTime attribute.
     * 
     * @return A List of BandwidthSubscription Objects.
     */
    List<BandwidthSubscription> getBandwidthSubscriptions();

    /**
     * Get all the subscription retrievals for the specified dataset and base
     * reference time.
     * 
     * @param provider
     *            The provider name.
     * 
     * @param dataSetName
     *            The dataset name.
     * 
     * @param baseReferenceTime
     *            The base reference time.
     * 
     * @return All the SubscriptionRetrievals that are scheduled for the
     *         specified time.
     */
    List<BandwidthSubscription> getBandwidthSubscriptions(String provider,
            String dataSetName, Calendar baseReferenceTime);

    /**
     * Create a new BandwidthDataSetUpdate Object based on the dataSetMetaData
     * Object provided.
     * 
     * @param dataSetMetaData
     *            The DataSetMetaData Object to create the
     *            BandwidthDataSetUpdate Object from.
     * 
     * @return A newly created and persisted BandwidthDataSetUpdate Object.
     */
    BandwidthDataSetUpdate newBandwidthDataSetUpdate(
            DataSetMetaData<T> dataSetMetaData);

    /**
     * Create a new BandwidthSubscription Object based on the Subscription and
     * Calendar Objects provided.
     * 
     * @param Subscription
     *            The Subscription Object to create the BandwidthSubscription
     *            Object from.
     * 
     * @param baseReferenceTime
     *            The base reference time to set on the newly created
     *            BandwidthSubscription Object.
     * 
     * @return A newly created and persisted BandwidthSubscription Object.
     */
    BandwidthSubscription newBandwidthSubscription(Subscription<T, C> subscription,
            Calendar baseReferenceTime);

    /**
     * Get a SubscriptionRetrievals.
     * 
     * @param subscriptionId
     *            Retrieve the SubscriptionRetrievals with the specified
     *            subscriptionId.
     * 
     * @return A List of SubscriptionRetrievals that has the specified
     *         subscriptionId.
     */
    List<SubscriptionRetrieval> querySubscriptionRetrievals(long subscriptionId);

    /**
     * Get {@link SubscriptionRetrieval}s for the specific
     * {@link BandwidthSubscription}.
     * 
     * @param subscriptionDao
     *            the dao
     * @return the retrievals
     */
    List<SubscriptionRetrieval> querySubscriptionRetrievals(
            BandwidthSubscription subscriptionDao);

    /**
     * Remove a BandwidthSubscription from the database.
     * 
     * @param subscriptionDao
     *            The subscriptionDao to remove.
     */
    void remove(BandwidthSubscription subscriptionDao);

    /**
     * Persist a BandwidthAllocation to the database.
     * 
     * @param bandwidthAllocation
     *            The BandwidthAllocation to store.
     */
    void store(BandwidthAllocation bandwidthAllocation);

    /**
     * Persist a SubscriptionRetrievalAttributes to the database.
     * 
     * @param attributes
     *            The SubscriptionRetrievalAttributes to store.
     */
    void store(SubscriptionRetrievalAttributes<T, C> attributes);

    /**
     * Persist a List of SubscriptionRetrievals to the database.
     * 
     * @param retrievals
     *            The SubscriptionRetrievals to store.
     */
    void store(List<SubscriptionRetrieval> retrievals);

    /**
     * Persist a list of objects to the database.
     * 
     * @param entities
     *            The entities to store.
     */
    void storeSubscriptionRetrievalAttributes(
            List<SubscriptionRetrievalAttributes<T, C>> list);

    /**
     * Persist a {@link BandwidthSubscription} to the database.
     * 
     * @param subscriptionDao
     *            The {@link BandwidthSubscription} to store.
     */
    void store(BandwidthSubscription subscriptionDao);

    /**
     * Persist a {@link Collection} of {@link BandwidthSubscription}s to the
     * database.
     * 
     * @param newSubscriptions
     *            the subscriptions to persist
     */
    void storeBandwidthSubscriptions(
            Collection<BandwidthSubscription> newSubscriptions);

    /**
     * Update a BandwidthAllocation in the database.
     * 
     * @param allocation
     *            The BandwidthAllocation to store.
     */
    void createOrUpdate(BandwidthAllocation allocation);

    /**
     * Update a BandwidthSubscription in the database.
     * 
     * @param dao
     *            The BandwidthSubscription to store.
     */
    void update(BandwidthSubscription dao);

    /**
     * Update a BandwidthAllocation in the database.
     * 
     * @param bandwidthAllocation
     *            The bandwidthAllocation to update.
     */
    void update(BandwidthAllocation allocation);

    /**
     * Find all bandwidth allocations in the specified state.
     * 
     * @param state
     * @return the allocations in that state
     */
    List<BandwidthAllocation> getBandwidthAllocationsInState(
            RetrievalStatus state);

    /**
     * Get all {@link SubscriptionRetrieval} instances.
     * 
     * @return the retrievals
     */
    List<SubscriptionRetrieval> getSubscriptionRetrievals();

    /**
     * Get {@link BandwidthAllocation}s for the specified network and start
     * time.
     * 
     * @param network
     *            the network
     * @param bucketStartTime
     *            the bucket start time
     * @return the allocations
     */
    List<BandwidthAllocation> getBandwidthAllocationsForNetworkAndBucketStartTime(
            Network network, long bucketStartTime);

    /**
     * @param attributes
     */
    void update(SubscriptionRetrievalAttributes<T,C> attributes);

    /**
     * Get the {@link SubscriptionRetrievalAttributes} for the
     * {@link SubscriptionRetrieval}.
     * 
     * @param retrieval
     * @return the attributes
     */
    SubscriptionRetrievalAttributes<T, C> getSubscriptionRetrievalAttributes(
            SubscriptionRetrieval retrieval);

    /**
     * Get the subscription status summary.
     * 
     * @param sub
     *            The subscription
     * 
     * @return the SubscriptionStatusSummary
     */
    SubscriptionStatusSummary getSubscriptionStatusSummary(Subscription<T, C> sub);

    /**
     * Get the BandwidthAllocation identified by the given id.
     * 
     * @param id
     */
    BandwidthAllocation getBandwidthAllocation(long id);
}