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
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;

/**
 * Extracted from {@link BandwidthContextFactory} so that {@link BandwidthManager}
 * can be run in memory (e.g. for testing proposed bandwidth size limitations
 * and informing the user which subscriptions would be unable to be scheduled).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2012 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IBandwidthDao {

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
    List<DataSetMetaDataDao> getDataSetMetaDataDao(String providerName,
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
    List<DataSetMetaDataDao> getDataSetMetaDataDao(String providerName,
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
     * Get a SubscriptionDao.
     * 
     * @param identifier
     *            Retrieve the SubscriptionDao with the specified identifier.
     * 
     * @return The SubscriptionDao that has the specified identifier or null if
     *         no such SubscriptionDao exists.
     */
    SubscriptionDao getSubscriptionDao(long identifier);

    /**
     * Get a SubscriptionDao.
     * 
     * @param registryId
     *            Retrieve the SubscriptionDao with the specified registryId.
     * @param baseReferenceTime
     *            Retrieve the SubscriptionDao with the specified
     *            baseReferenceTime.
     * 
     * @return The SubscriptionDao that has the specified identifier and
     *         baseReferenceTime or null if no such SubscriptionDao exists.
     */
    SubscriptionDao getSubscriptionDao(String registryId,
            Calendar baseReferenceTime);

    /**
     * Get SubscriptionDaos.
     * 
     * @param subscription
     *            Retrieve SubscriptionDaos that match the specified
     *            subscription's owner, provider, name and dataSetName.
     * 
     * @return A List of SubscriptionDaos that have the same owner, provider,
     *         name and dataSetName and the specified subscription.
     */
    List<SubscriptionDao> getSubscriptionDao(Subscription subscription);

    /**
     * Get a SubscriptionDaos.
     * 
     * @param registryId
     *            Retrieve the SubscriptionDaos with the specified registryId.
     * 
     * @return A List of SubscriptionDaos that has the specified registryId or
     *         null if no such SubscriptionDao exists.
     */
    List<SubscriptionDao> getSubscriptionDaoByRegistryId(String registryId);

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
     * Return all the SubscriptionDao Objects in the database in ascending order
     * based on the SubscriptionDao's baseReferenceTime attribute.
     * 
     * @return A List of SubscriptionDao Objects.
     */
    List<SubscriptionDao> getSubscriptions();

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
    List<SubscriptionDao> getSubscriptions(String provider, String dataSetName,
            Calendar baseReferenceTime);

    /**
     * Create a new DataSetMetaDataDao Object based on the dataSetMetaData
     * Object provided.
     * 
     * @param dataSetMetaData
     *            The DataSetMetaData Object to create the DataSetMetaDataDao
     *            Object from.
     * 
     * @return A newly created and persisted DataSetMetaDataDao Object.
     */
    DataSetMetaDataDao newDataSetMetaDataDao(DataSetMetaData dataSetMetaData);

    /**
     * Create a new SubscriptionDao Object based on the Subscription and
     * Calendar Objects provided.
     * 
     * @param Subscription
     *            The Subscription Object to create the SubscriptionDao Object
     *            from.
     * 
     * @param baseReferenceTime
     *            The base reference time to set on the newly created
     *            SubscriptionDao Object.
     * 
     * @return A newly created and persisted SubscriptionDao Object.
     */
    SubscriptionDao newSubscriptionDao(Subscription subscription,
            Calendar baseReferenceTime) throws SerializationException;

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
     * {@link SubscriptionDao}.
     * 
     * @param subscriptionDao
     *            the dao
     * @return the retrievals
     */
    List<SubscriptionRetrieval> querySubscriptionRetrievals(
            SubscriptionDao subscriptionDao);

    /**
     * Remove a SubscriptionDao from the database.
     * 
     * @param subscriptionDao
     *            The subscriptionDao to remove.
     */
    void remove(SubscriptionDao subscriptionDao);

    /**
     * Persist a BandwidthAllocation to the database.
     * 
     * @param bandwidthAllocation
     *            The BandwidthAllocation to store.
     */
    void store(BandwidthAllocation bandwidthAllocation);

    /**
     * Persist a List of SubscriptionRetrievals to the database.
     * 
     * @param retrievals
     *            The SubscriptionRetrievals to store.
     */
    void store(List<SubscriptionRetrieval> retrievals);

    /**
     * Persist a {@link SubscriptionDao} to the database.
     * 
     * @param subscriptionDao
     *            The {@link SubscriptionDao} to store.
     */
    void store(SubscriptionDao subscriptionDao);

    /**
     * Update a BandwidthAllocation in the database.
     * 
     * @param allocation
     *            The BandwidthAllocation to store.
     */
    void update(BandwidthAllocation allocation);

    /**
     * Update a SubscriptionDao in the database.
     * 
     * @param dao
     *            The SubscriptionDao to store.
     */
    void update(SubscriptionDao dao);

    /**
     * Update a SubscriptionRetrieval in the database.
     * 
     * @param subscriptionRetrieval
     *            The SubscriptionRetrieval to store.
     */
    void update(SubscriptionRetrieval subscriptionRetrieval);

    /**
     * Find all bandwidth allocations in the specified state.
     * 
     * @param state
     * @return the allocations in that state
     */
    List<BandwidthAllocation> getBandwidthAllocationsInState(
            RetrievalStatus state);
}