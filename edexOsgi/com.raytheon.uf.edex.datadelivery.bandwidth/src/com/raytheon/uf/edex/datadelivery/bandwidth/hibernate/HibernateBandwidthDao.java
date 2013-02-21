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

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.dialect.Dialect;
import org.hibernate.impl.SessionFactoryImpl;
import org.hibernate.jdbc.Work;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.session.SessionContext;
import com.raytheon.uf.common.util.session.SessionManager;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.DataSetMetaDataDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDao;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class HibernateBandwidthDao extends CoreDao implements IBandwidthDao {
    /**
     * 
     * Implementation of {@link SessionContext} which controls the Hibernate
     * session and transaction. The {@link SessionManager} controls the
     * lifecycle of the context, and you can read the documentation located in
     * the {@link SessionManager} class for more details on its use.
     */
    public static class BandwidthSessionContext implements SessionContext {
        private Session session;

        private Transaction tx;

        /**
         * {@inheritDoc}
         */
        @Override
        public void open() {
            session = instance.getSessionFactory().openSession();
            tx = session.beginTransaction();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void close() {
            if (tx != null && !tx.wasCommitted()) {
                tx.rollback();
            }
            if (session != null) {
                session.close();
            }
        }

        private void commit() {
            tx.commit();
        }

        private Query getQuery(final String queryString) {
            return session.createQuery(queryString).setCacheable(true);
        }

        /**
         * Do some database work.
         * 
         * @param work
         *            the work instance
         */
        private void doWork(Work work) {
            session.doWork(work);
        }
    }

    private static final String GET_BANDWIDTH_ALLOCATIONS_BY_NETWORK = "from BandwidthAllocation res where res.network = :network";

    private static final String GET_BANDWIDTH_ALLOCATIONS_BY_STATE = "from BandwidthAllocation res where res.status = :state";

    private static final String GET_BANDWIDTH_ALLOCATIONS_BY_SUBSCRIPTION_ID = "from BandwidthAllocation res where res.subscriptionDao.id = :subscriptionId";

    private static final String GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET = "from DataSetMetaDataDao d where "
            + "d.providerName = :providerName and "
            + "d.dataSetName = :dataSetName order by dataSetBaseTime desc";

    private static final String GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME = "from DataSetMetaDataDao d where "
            + "d.providerName = :providerName and "
            + "d.dataSetName = :dataSetName and "
            + "d.dataSetBaseTime = :dataSetBaseTime "
            + "order by dataSetBaseTime desc";

    private static final String GET_DEFERRED = "from BandwidthAllocation alloc where "
            + "alloc.status = :status and "
            + "alloc.network = :network and "
            + "alloc.endTime <= :endTime";

    private static final String GET_SUBSCRIPTIONDAO = "from SubscriptionDao sub order by sub.baseReferenceTime asc";

    private static final String GET_SUBSCRIPTIONDAO_BY_IDENTIFIER = "from SubscriptionDao sub where sub.id = :identifier";

    private static final String GET_SUBSCRIPTIONDAO_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME = "from SubscriptionDao sub where "
            + "  sub.provider = :provider and "
            + "  sub.dataSetName = :dataSetName and "
            + "  sub.baseReferenceTime = :baseReferenceTime";

    private static final String GET_SUBSCRIPTIONDAO_BY_REGISTRY_ID_AND_BASEREFERENCETIME = "from SubscriptionDao sub where "
            + "sub.registryId = :registryId and "
            + "sub.baseReferenceTime = :baseReferenceTime";

    private static final String GET_SUBSCRIPTIONDAO_BY_REGISTRYID = "from SubscriptionDao sub where "
            + "sub.registryId = :registryId";

    private static final String GET_SUBSCRIPTIONDAO_BY_SUBSCRIPTION = "from SubscriptionDao sub where "
            + "sub.owner = :owner and "
            + "sub.provider = :provider and "
            + "sub.name = :name and " + "sub.dataSetName = :dataSetName";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_IDENTIFIER = "from SubscriptionRetrieval sr where "
            + "sr.id = :identifier";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_BASE = "from SubscriptionRetrieval sr where "
            + " sr.subscriptionDao.id in ("
            + "  select sub.id from SubscriptionDao sub where "
            + "  sub.provider = :provider and "
            + "  sub.dataSetName = :dataSetName";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET = GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_BASE
            + ")";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME = GET_SUBSCRIPTIONRETRIEVAL_BY_PROVIDER_AND_DATASET_BASE
            + " and sub.baseReferenceTime = :baseReferenceTime)";

    private static final String GET_SUBSCRIPTIONRETRIEVAL_BY_SUBSCRIPTIONID = "from SubscriptionRetrieval sr where "
            + "sr.subscriptionDao.id = :subscriptionId";

    private static final HibernateBandwidthDao instance = new HibernateBandwidthDao();

    /**
     * @return the instance
     */
    public static HibernateBandwidthDao getInstance() {
        return instance;
    }

    private HibernateBandwidthDao() {
        super(DaoConfig.forClass(HibernateBandwidthDao.class));
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
    public List<DataSetMetaDataDao> getDataSetMetaDataDao(String providerName,
            String dataSetName) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("providerName", providerName);
        params.put("dataSetName", dataSetName);
        return query(GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<DataSetMetaDataDao> getDataSetMetaDataDao(String providerName,
            String dataSetName, Calendar baseReferenceTime) {
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
        return ((SessionFactoryImpl) getSessionFactory()).getDialect();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionDao getSubscriptionDao(long identifier) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("identifier", identifier);
        return uniqueResult(GET_SUBSCRIPTIONDAO_BY_IDENTIFIER, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionDao getSubscriptionDao(String registryId,
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
    public List<SubscriptionDao> getSubscriptionDao(Subscription subscription) {

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
    public List<SubscriptionDao> getSubscriptionDaoByRegistryId(
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
    @Override
    public List<SubscriptionDao> getSubscriptions() {
        Map<String, Object> params = new HashMap<String, Object>();
        return query(GET_SUBSCRIPTIONDAO, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionDao> getSubscriptions(String provider,
            String dataSetName, Calendar baseReferenceTime) {
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
    public DataSetMetaDataDao newDataSetMetaDataDao(
            DataSetMetaData dataSetMetaData) {

        DataSetMetaDataDao dao = BandwidthUtil
                .newDataSetMetaDataDao(dataSetMetaData);

        persist(dao);

        return dao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionDao newSubscriptionDao(Subscription subscription,
            Calendar baseReferenceTime) throws SerializationException {
        SubscriptionDao dao = BandwidthUtil.getSubscriptionDaoForSubscription(
                subscription, baseReferenceTime);

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
            SubscriptionDao subscriptionDao) {
        return querySubscriptionRetrievals(subscriptionDao.getId());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void remove(SubscriptionDao subscriptionDao) {
        List<BandwidthAllocation> bandwidthReservations = getBandwidthAllocations(subscriptionDao
                .getIdentifier());
        deleteAll(bandwidthReservations);
        delete(subscriptionDao);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(BandwidthAllocation bandwidthAllocation) {
        persist(bandwidthAllocation);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(List<SubscriptionRetrieval> retrievals) {

        for (SubscriptionRetrieval retrieval : retrievals) {
            persist(retrieval);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(SubscriptionDao subscriptionDao) {
        persist(subscriptionDao);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(BandwidthAllocation allocation) {
        persist(allocation);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(SubscriptionDao dao) {
        persist(dao);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(SubscriptionRetrieval subscriptionRetrieval) {
        persist(subscriptionRetrieval);
    }

    /**
     * Internal convenience method for querying.
     * 
     * @param <T>
     * @param queryString
     * @param params
     * @return
     */
    @SuppressWarnings("unchecked")
    private <T extends Object> List<T> query(String queryString,
            Map<String, Object> params) {
        List<T> results = null;
        BandwidthSessionContext ctx = SessionManager
                .openSession(BandwidthSessionContext.class);
        try {
            Query query = ctx.getQuery(queryString);

            if (params != null) {
                for (String name : params.keySet()) {
                    Object val = params.get(name);
                    query.setParameter(name, val);
                }
            }
            results = query.list();
            ctx.commit();
        } finally {
            SessionManager.closeSession(BandwidthSessionContext.class);
        }
        return results;
    }

    /**
     * Internal convenience method for returning a single result.
     * 
     * @param <T>
     * @param queryString
     * @param params
     * @return
     */
    @SuppressWarnings("unchecked")
    private <T extends Object> T uniqueResult(String queryString,
            Map<String, Object> params) {
        T results = null;
        BandwidthSessionContext ctx = SessionManager
                .openSession(BandwidthSessionContext.class);
        try {
            Query query = ctx.getQuery(queryString);

            if (params != null) {
                for (String name : params.keySet()) {
                    Object val = params.get(name);
                    query.setParameter(name, val);
                }
            }
            results = (T) query.uniqueResult();
            ctx.commit();
        } finally {
            SessionManager.closeSession(BandwidthSessionContext.class);
        }
        return results;
    }

    /**
     * Internal utility method to execute sql transactionally.
     * 
     * @param work
     *            The unit of work to do.
     */
    public void doWork(Work work) {
        BandwidthSessionContext ctx = SessionManager
                .openSession(BandwidthSessionContext.class);
        try {
            ctx.doWork(work);
            ctx.commit();
        } finally {
            SessionManager.closeSession(BandwidthSessionContext.class);
        }
    }
}
