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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.dialect.Dialect;
import org.hibernate.impl.SessionFactoryImpl;
import org.hibernate.jdbc.Work;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Transactional
@Repository
// TODO: Split service functionality from DAO functionality
public class HibernateBandwidthDao extends SessionManagedDao implements
        IBandwidthDao {

    private BandwidthAllocationDao bandwidthAllocationDao;

    private BandwidthSubscriptionDao bandwidthSubscriptionDao;

    private static final String GET_BANDWIDTH_ALLOCATIONS_BY_NETWORK = "from BandwidthAllocation res where res.network = :network";

    private static final String GET_BANDWIDTH_ALLOCATIONS_BY_STATE = "from BandwidthAllocation res where res.status = :state";

    private static final String GET_BANDWIDTH_ALLOCATIONS_BY_SUBSCRIPTION_ID = "from BandwidthAllocation res where res.bandwidthSubscription.id = :subscriptionId";

    private static final String GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET = "from BandwidthDataSetUpdate d where "
            + "d.providerName = :providerName and "
            + "d.dataSetName = :dataSetName order by dataSetBaseTime desc";

    private static final String GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME = "from BandwidthDataSetUpdate d where "
            + "d.providerName = :providerName and "
            + "d.dataSetName = :dataSetName and "
            + "d.dataSetBaseTime = :dataSetBaseTime "
            + "order by dataSetBaseTime desc";

    private static final String GET_DEFERRED = "from BandwidthAllocation alloc where "
            + "alloc.status = :status and "
            + "alloc.network = :network and "
            + "alloc.endTime <= :endTime";

    private static final String GET_SUBSCRIPTIONDAO = "from BandwidthSubscription sub order by sub.baseReferenceTime asc";

    private static final String GET_SUBSCRIPTIONDAO_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME = "from BandwidthSubscription sub where "
            + "  sub.provider = :provider and "
            + "  sub.dataSetName = :dataSetName and "
            + "  sub.baseReferenceTime = :baseReferenceTime";

    private static final String GET_SUBSCRIPTIONDAO_BY_REGISTRY_ID_AND_BASEREFERENCETIME = "from BandwidthSubscription sub where "
            + "sub.registryId = :registryId and "
            + "sub.baseReferenceTime = :baseReferenceTime";

    private static final String GET_SUBSCRIPTIONDAO_BY_REGISTRYID = "from BandwidthSubscription sub where "
            + "sub.registryId = :registryId";

    private static final String GET_SUBSCRIPTIONDAO_BY_SUBSCRIPTION = "from BandwidthSubscription sub where "
            + "sub.owner = :owner and "
            + "sub.provider = :provider and "
            + "sub.name = :name and " + "sub.dataSetName = :dataSetName";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_IDENTIFIER = "from SubscriptionRetrieval sr where "
            + "sr.id = :identifier";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_BASE = "from SubscriptionRetrieval sr where "
            + " sr.bandwidthSubscription.id in ("
            + "  select sub.id from BandwidthSubscription sub where "
            + "  sub.provider = :provider and "
            + "  sub.dataSetName = :dataSetName";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET = GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_BASE
            + ")";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME = GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_BASE
            + " and sub.baseReferenceTime = :baseReferenceTime)";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_SUBSCRIPTIONID = "from SubscriptionRetrieval sr where "
            + "sr.bandwidthSubscription.id = :subscriptionId";

    private static HibernateBandwidthDao instance;

    /**
     * @return the instance
     */
    public static HibernateBandwidthDao getInstance() {
        return instance;
    }

    /**
     * Constructor.
     */
    public HibernateBandwidthDao() {
        // TODO: Don't use a static instance
        HibernateBandwidthDao.instance = this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocations(Long subscriptionId) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("subscriptionId", subscriptionId);
        return query(GET_BANDWIDTH_ALLOCATIONS_BY_SUBSCRIPTION_ID, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocations(Network network) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("network", network);
        return query(GET_BANDWIDTH_ALLOCATIONS_BY_NETWORK, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocationsInState(
            RetrievalStatus state) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("state", state);
        return query(GET_BANDWIDTH_ALLOCATIONS_BY_STATE, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthDataSetUpdate> getBandwidthDataSetUpdate(
            String providerName, String dataSetName) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("providerName", providerName);
        params.put("dataSetName", dataSetName);
        return query(GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthDataSetUpdate> getBandwidthDataSetUpdate(
            String providerName, String dataSetName, Calendar baseReferenceTime) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("providerName", providerName);
        params.put("dataSetName", dataSetName);
        params.put("dataSetBaseTime", baseReferenceTime);
        return query(
                GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME,
                params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getDeferred(Network network,
            Calendar endTime) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("status", RetrievalStatus.DEFERRED);
        params.put("network", network);
        params.put("endTime", endTime);
        return query(GET_DEFERRED, params);
    }

    /**
     * Used by DbInitializer implementation to get the Dialect of the
     * SessionFactory configured for bandwidth management.
     * 
     * @return The Dialect.
     */
    public Dialect getDialect() {
        return ((SessionFactoryImpl) template.getSessionFactory()).getDialect();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthSubscription getBandwidthSubscription(long identifier) {
        return BandwidthSubscription.class.cast(template.get(
                BandwidthSubscription.class, Long.valueOf(identifier)));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthSubscription getBandwidthSubscription(String registryId,
            Calendar baseReferenceTime) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("registryId", registryId);
        params.put("baseReferenceTime", baseReferenceTime);
        return uniqueResult(
                GET_SUBSCRIPTIONDAO_BY_REGISTRY_ID_AND_BASEREFERENCETIME,
                params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthSubscription> getBandwidthSubscription(
            Subscription subscription) {

        Map<String, Object> params = new HashMap<String, Object>();
        params.put("owner", subscription.getOwner());
        params.put("provider", subscription.getProvider());
        params.put("name", subscription.getName());
        params.put("dataSetName", subscription.getDataSetName());
        return query(GET_SUBSCRIPTIONDAO_BY_SUBSCRIPTION, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthSubscription> getBandwidthSubscriptionByRegistryId(
            String registryId) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("registryId", registryId);
        return query(GET_SUBSCRIPTIONDAO_BY_REGISTRYID, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionRetrieval getSubscriptionRetrieval(long identifier) {

        Map<String, Object> params = new HashMap<String, Object>();
        params.put("identifier", identifier);
        return uniqueResult(GET_SUBSCRIPTIONRETRIEVAL_BY_IDENTIFIER, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> getSubscriptionRetrievals(
            String provider, String dataSetName, Calendar baseReferenceTime) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("provider", provider);
        params.put("dataSetName", dataSetName);
        params.put("baseReferenceTime", baseReferenceTime);
        return query(
                GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME,
                params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> getSubscriptionRetrievals(
            String provider, String dataSetName) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("provider", provider);
        params.put("dataSetName", dataSetName);
        return query(GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET, params);
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    @Override
    public List<BandwidthSubscription> getBandwidthSubscriptions() {
        return template.find(GET_SUBSCRIPTIONDAO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthSubscription> getBandwidthSubscriptions(
            String provider, String dataSetName, Calendar baseReferenceTime) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("provider", provider);
        params.put("dataSetName", dataSetName);
        params.put("baseReferenceTime", baseReferenceTime);
        return query(
                GET_SUBSCRIPTIONDAO_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME,
                params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthDataSetUpdate newBandwidthDataSetUpdate(
            DataSetMetaData dataSetMetaData) {

        BandwidthDataSetUpdate dao = BandwidthUtil
                .newDataSetMetaDataDao(dataSetMetaData);

        create(dao);

        return dao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthSubscription newBandwidthSubscription(
            Subscription subscription, Calendar baseReferenceTime)
            throws SerializationException {
        BandwidthSubscription dao = BandwidthUtil
                .getSubscriptionDaoForSubscription(subscription,
                        baseReferenceTime);

        store(dao);
        return dao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> querySubscriptionRetrievals(
            long subscriptionId) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("subscriptionId", subscriptionId);
        return query(GET_SUBSCRIPTIONRETRIEVAL_BY_SUBSCRIPTIONID, params);
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
        List<BandwidthAllocation> bandwidthReservations = getBandwidthAllocations(subscriptionDao
                .getIdentifier());
        bandwidthAllocationDao.deleteAll(bandwidthReservations);
        bandwidthSubscriptionDao.delete(subscriptionDao);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(BandwidthAllocation bandwidthAllocation) {
        update(bandwidthAllocation);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(List<SubscriptionRetrieval> retrievals) {
        for (SubscriptionRetrieval retrieval : retrievals) {
            store(retrieval);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @Transactional
    public void store(BandwidthSubscription subscriptionDao) {
        template.save(subscriptionDao);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(BandwidthAllocation allocation) {
        createOrUpdate(allocation);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(BandwidthSubscription dao) {
        template.update(dao);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(SubscriptionRetrieval subscriptionRetrieval) {
        createOrUpdate(subscriptionRetrieval);
    }

    /**
     * Internal utility method to execute sql transactionally.
     * 
     * @param work
     *            The unit of work to do.
     */
    public void doWork(Work work) {
        template.getSessionFactory().getCurrentSession().doWork(work);
    }

    /**
     * @return the bandwidthAllocationDao
     */
    public BandwidthAllocationDao getBandwidthAllocationDao() {
        return bandwidthAllocationDao;
    }

    /**
     * @param bandwidthAllocationDao
     *            the bandwidthAllocationDao to set
     */
    public void setBandwidthAllocationDao(
            BandwidthAllocationDao bandwidthAllocationDao) {
        this.bandwidthAllocationDao = bandwidthAllocationDao;
    }

    /**
     * @return the subscriptionDaoDao
     */
    public BandwidthSubscriptionDao getBandwidthSubscriptionDao() {
        return bandwidthSubscriptionDao;
    }

    /**
     * @param subscriptionDaoDao
     *            the subscriptionDaoDao to set
     */
    public void setBandwidthSubscriptionDao(
            BandwidthSubscriptionDao bandwidthSubscriptionDao) {
        this.bandwidthSubscriptionDao = bandwidthSubscriptionDao;
    }
}
