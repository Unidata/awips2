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
import java.util.List;

import org.hibernate.dialect.Dialect;
import org.hibernate.jdbc.Work;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthDataSetUpdate;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Transactional
@Service
public class HibernateBandwidthDao implements IBandwidthDao {

    private IBandwidthAllocationDao bandwidthAllocationDao;

    private ISubscriptionRetrievalDao subscriptionRetrievalDao;

    private IBandwidthSubscriptionDao bandwidthSubscriptionDao;

    private IBandwidthDataSetUpdateDao bandwidthDataSetUpdateDao;

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
            Subscription subscription) {
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
            DataSetMetaData dataSetMetaData) {

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
            Subscription subscription, Calendar baseReferenceTime)
            throws SerializationException {
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
    @Transactional
    public void store(BandwidthSubscription subscriptionDao) {
        bandwidthSubscriptionDao.create(subscriptionDao);
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
        subscriptionRetrievalDao.doWork(work);
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

}
