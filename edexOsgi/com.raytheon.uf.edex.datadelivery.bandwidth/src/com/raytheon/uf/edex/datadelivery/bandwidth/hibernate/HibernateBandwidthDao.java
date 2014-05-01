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

import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.SortedSet;

import org.hibernate.dialect.Dialect;
import org.hibernate.jdbc.Work;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.datadelivery.bandwidth.data.SubscriptionStatusSummary;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthDataSetUpdate;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrievalAttributes;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * {@link IBandwidthDao} implementation that interacts with Hibernate.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2012 1286       djohnson     Extracted from BandwidthContextFactory.
 * Feb 07, 2013 1543       djohnson     Moved session management context to CoreDao.
 * Feb 11, 2013 1543       djohnson     Use Spring transactions.
 * Feb 13, 2013 1543       djohnson     Converted into a service, created new DAOs as required.
 * Jun 03, 2013 2038       djohnson     Add method to get subscription retrievals by provider, dataset, and status.
 * Jun 13, 2013 2095       djohnson     Implement ability to store a collection of subscriptions.
 * Jun 24, 2013 2106       djohnson     Implement new methods.
 * Jul 18, 2013 1653       mpduff       Added getSubscriptionStatusSummary.
 * Aug 28, 2013 2290       mpduff       Check for no subscriptions.
 * Oct 2,  2013 1797       dhladky      Generics
 * Dec 17, 2013 2636       bgonzale     Added method to get a BandwidthAllocation.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Transactional
@Service
public class HibernateBandwidthDao<T extends Time, C extends Coverage> implements IBandwidthDao<T, C> {

    private IBandwidthAllocationDao bandwidthAllocationDao;

    private ISubscriptionRetrievalDao subscriptionRetrievalDao;

    private IBandwidthSubscriptionDao bandwidthSubscriptionDao;

    private IBandwidthDataSetUpdateDao bandwidthDataSetUpdateDao;

    private ISubscriptionRetrievalAttributesDao<T, C> subscriptionRetrievalAttributesDao;

    /**
     * Constructor.
     */
    public HibernateBandwidthDao() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocations(Long subscriptionId) {
        return bandwidthAllocationDao.getBySubscriptionId(subscriptionId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocations(Network network) {
        return bandwidthAllocationDao.getByNetwork(network);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocationsInState(
            RetrievalStatus state) {
        return bandwidthAllocationDao.getByState(state);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthDataSetUpdate> getBandwidthDataSetUpdate(
            String providerName, String dataSetName) {
        return bandwidthDataSetUpdateDao.getByProviderDataSet(providerName,
                dataSetName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthDataSetUpdate> getBandwidthDataSetUpdate(
            String providerName, String dataSetName, Calendar baseReferenceTime) {
        return bandwidthDataSetUpdateDao.getByProviderDataSetReferenceTime(
                providerName, dataSetName, baseReferenceTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getDeferred(Network network,
            Calendar endTime) {
        return bandwidthAllocationDao.getDeferred(network, endTime);
    }

    /**
     * Used by DbInitializer implementation to get the Dialect of the
     * SessionFactory configured for bandwidth management.
     * 
     * @return The Dialect.
     */
    public Dialect getDialect() {
        return subscriptionRetrievalDao.getDialect();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthSubscription getBandwidthSubscription(long identifier) {
        return bandwidthSubscriptionDao.getById(identifier);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthSubscription getBandwidthSubscription(String registryId,
            Calendar baseReferenceTime) {
        return bandwidthSubscriptionDao.getByRegistryIdReferenceTime(
                registryId, baseReferenceTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthSubscription> getBandwidthSubscription(
            Subscription<T, C> subscription) {
        return bandwidthSubscriptionDao.getBySubscription(subscription);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthSubscription> getBandwidthSubscriptionByRegistryId(
            String registryId) {
        return bandwidthSubscriptionDao.getByRegistryId(registryId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionRetrieval getSubscriptionRetrieval(long identifier) {
        return subscriptionRetrievalDao.getById(identifier);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> getSubscriptionRetrievals(
            String provider, String dataSetName, Calendar baseReferenceTime) {
        return subscriptionRetrievalDao.getByProviderDataSetReferenceTime(
                provider, dataSetName, baseReferenceTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> getSubscriptionRetrievals(
            String provider, String dataSetName) {
        return subscriptionRetrievalDao.getByProviderDataSet(provider,
                dataSetName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SortedSet<SubscriptionRetrieval> getSubscriptionRetrievals(
            String provider, String dataSetName, RetrievalStatus status) {
        return subscriptionRetrievalDao.getByProviderDataSetAndStatus(provider,
                dataSetName, status);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SortedSet<SubscriptionRetrieval> getSubscriptionRetrievals(
            String provider, String dataSetName, RetrievalStatus status,
            Date earliestDate, Date latestDate) {
        return this.subscriptionRetrievalDao
                .getByProviderDataSetStatusAndDateRange(provider, dataSetName,
                        status, earliestDate, latestDate);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthSubscription> getBandwidthSubscriptions() {
        return bandwidthSubscriptionDao.getAll();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthSubscription> getBandwidthSubscriptions(
            String provider, String dataSetName, Calendar baseReferenceTime) {
        return bandwidthSubscriptionDao.getByProviderDataSetReferenceTime(
                provider, dataSetName, baseReferenceTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthDataSetUpdate newBandwidthDataSetUpdate(
            DataSetMetaData<T> dataSetMetaData) {

        BandwidthDataSetUpdate entity = BandwidthUtil
                .newDataSetMetaDataDao(dataSetMetaData);
        bandwidthDataSetUpdateDao.create(entity);

        return entity;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthSubscription newBandwidthSubscription(
            Subscription<T, C> subscription, Calendar baseReferenceTime) {
        BandwidthSubscription entity = BandwidthUtil
                .getSubscriptionDaoForSubscription(subscription,
                        baseReferenceTime);

        bandwidthSubscriptionDao.create(entity);

        return entity;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> querySubscriptionRetrievals(
            long subscriptionId) {
        return subscriptionRetrievalDao.getBySubscriptionId(subscriptionId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> querySubscriptionRetrievals(
            BandwidthSubscription subscriptionDao) {
        return querySubscriptionRetrievals(subscriptionDao.getId());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void remove(BandwidthSubscription subscriptionDao) {
        List<SubscriptionRetrieval> bandwidthReservations = subscriptionRetrievalDao
                .getBySubscriptionId(subscriptionDao.getIdentifier());
        for (SubscriptionRetrieval retrieval : bandwidthReservations) {
            subscriptionRetrievalAttributesDao
                    .delete(subscriptionRetrievalAttributesDao
                            .getBySubscriptionRetrieval(retrieval));
        }
        subscriptionRetrievalDao.deleteAll(bandwidthReservations);
        bandwidthSubscriptionDao.delete(subscriptionDao);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(List<SubscriptionRetrieval> retrievals) {
        for (SubscriptionRetrieval retrieval : retrievals) {
            subscriptionRetrievalDao.create(retrieval);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(BandwidthSubscription subscriptionDao) {
        bandwidthSubscriptionDao.create(subscriptionDao);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void storeBandwidthSubscriptions(
            Collection<BandwidthSubscription> newSubscriptions) {
        bandwidthSubscriptionDao.persistAll(newSubscriptions);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(BandwidthAllocation bandwidthAllocation) {
        bandwidthAllocationDao.create(bandwidthAllocation);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createOrUpdate(BandwidthAllocation allocation) {
        bandwidthAllocationDao.createOrUpdate(allocation);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(BandwidthAllocation allocation) {
        bandwidthAllocationDao.update(allocation);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(BandwidthSubscription dao) {
        bandwidthSubscriptionDao.update(dao);
    }

    /**
     * Internal utility method to execute sql transactionally.
     * 
     * @param work
     *            The unit of work to do.
     */
    public void doWork(Work work) {
        subscriptionRetrievalDao.executeWork(work);
    }

    /**
     * @return the subscriptionRetrievalDao
     */
    public ISubscriptionRetrievalDao getSubscriptionRetrievalDao() {
        return subscriptionRetrievalDao;
    }

    /**
     * @param subscriptionRetrievalDao
     *            the subscriptionRetrievalDao to set
     */
    public void setSubscriptionRetrievalDao(
            ISubscriptionRetrievalDao bandwidthAllocationDao) {
        this.subscriptionRetrievalDao = bandwidthAllocationDao;
    }

    /**
     * @return the subscriptionDaoDao
     */
    public IBandwidthSubscriptionDao getBandwidthSubscriptionDao() {
        return bandwidthSubscriptionDao;
    }

    /**
     * @param subscriptionDaoDao
     *            the subscriptionDaoDao to set
     */
    public void setBandwidthSubscriptionDao(
            IBandwidthSubscriptionDao bandwidthSubscriptionDao) {
        this.bandwidthSubscriptionDao = bandwidthSubscriptionDao;
    }

    /**
     * @return the bandwidthAllocationDao
     */
    public IBandwidthAllocationDao getBandwidthAllocationDao() {
        return bandwidthAllocationDao;
    }

    /**
     * @param bandwidthAllocationDao
     *            the bandwidthAllocationDao to set
     */
    public void setBandwidthAllocationDao(
            IBandwidthAllocationDao bandwidthAllocationDao) {
        this.bandwidthAllocationDao = bandwidthAllocationDao;
    }

    /**
     * @return the bandwidthDataSetUpdateDao
     */
    public IBandwidthDataSetUpdateDao getBandwidthDataSetUpdateDao() {
        return bandwidthDataSetUpdateDao;
    }

    /**
     * @param bandwidthDataSetUpdateDao
     *            the bandwidthDataSetUpdateDao to set
     */
    public void setBandwidthDataSetUpdateDao(
            IBandwidthDataSetUpdateDao bandwidthDataSetUpdateDao) {
        this.bandwidthDataSetUpdateDao = bandwidthDataSetUpdateDao;
    }

    /**
     * @return the subscriptionRetrievalAttributesDao
     */
    public ISubscriptionRetrievalAttributesDao<T, C> getSubscriptionRetrievalAttributesDao() {
        return subscriptionRetrievalAttributesDao;
    }

    /**
     * @param subscriptionRetrievalAttributesDao
     *            the subscriptionRetrievalAttributesDao to set
     */
    public void setSubscriptionRetrievalAttributesDao(
            ISubscriptionRetrievalAttributesDao<T, C> subscriptionRetrievalAttributesDao) {
        this.subscriptionRetrievalAttributesDao = subscriptionRetrievalAttributesDao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> getSubscriptionRetrievals() {
        return subscriptionRetrievalDao.getAll();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocationsForNetworkAndBucketStartTime(
            Network network, long bucketStartTime) {
        return bandwidthAllocationDao.getByNetworkAndBucketStartTime(network,
                bucketStartTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(SubscriptionRetrievalAttributes<T, C> attributes) {
        subscriptionRetrievalAttributesDao.create(attributes);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void storeSubscriptionRetrievalAttributes(
            List<SubscriptionRetrievalAttributes<T, C>> retrievalAttributes) {
        subscriptionRetrievalAttributesDao.persistAll(retrievalAttributes);
    }

    @Override
    public void update(SubscriptionRetrievalAttributes<T, C> attributes) {
        subscriptionRetrievalAttributesDao.update(attributes);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionRetrievalAttributes<T, C> getSubscriptionRetrievalAttributes(
            SubscriptionRetrieval retrieval) {
        return subscriptionRetrievalAttributesDao
                .getBySubscriptionRetrieval(retrieval);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionStatusSummary getSubscriptionStatusSummary(
            Subscription<T, C> sub) {
        SubscriptionStatusSummary summary = new SubscriptionStatusSummary();

        List<BandwidthSubscription> bandwidthSubList = this
                .getBandwidthSubscription(sub);

        if (bandwidthSubList != null && !bandwidthSubList.isEmpty()) {
            Collections.sort(bandwidthSubList,
                    new Comparator<BandwidthSubscription>() {
                        @Override
                        public int compare(BandwidthSubscription o1,
                                BandwidthSubscription o2) {
                            Calendar date1 = o1.getBaseReferenceTime();
                            Calendar date2 = o2.getBaseReferenceTime();
                            if (date1.before(date2)) {
                                return -1;
                            } else if (date1.after(date2)) {
                                return 1;
                            }

                            return 0;
                        }
                    });

            List<SubscriptionRetrieval> subRetrievalList = this
                    .querySubscriptionRetrievals(bandwidthSubList.get(0));
            Collections.sort(subRetrievalList,
                    new Comparator<SubscriptionRetrieval>() {
                        @Override
                        public int compare(SubscriptionRetrieval o1,
                                SubscriptionRetrieval o2) {
                            Calendar date1 = o1.getStartTime();
                            Calendar date2 = o2.getStartTime();
                            if (date1.before(date2)) {
                                return -1;
                            } else if (date1.after(date2)) {
                                return 1;
                            }

                            return 0;
                        }

                    });

            summary.setStartTime(subRetrievalList.get(0).getStartTime()
                    .getTimeInMillis());
            summary.setEndTime(subRetrievalList
                    .get(subRetrievalList.size() - 1).getEndTime()
                    .getTimeInMillis());
        }

        summary.setDataSize(sub.getDataSetSize());
        summary.setLatency(sub.getLatencyInMinutes());

        return summary;
    }

    @Override
    public BandwidthAllocation getBandwidthAllocation(long id) {
        return bandwidthAllocationDao.getById(id);
    }
}
