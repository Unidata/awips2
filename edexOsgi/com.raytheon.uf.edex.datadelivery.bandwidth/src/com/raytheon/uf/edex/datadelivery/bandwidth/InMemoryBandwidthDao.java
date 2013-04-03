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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicLong;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.ReflectionUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.DataSetMetaDataDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * Provides a {@link IBandwidthDao} implementation in memory. Intentionally
 * package-private.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2012 1286       djohnson     Initial creation
 * Dec 12, 2012 1286       djohnson     Use concurrent lists to avoid concurrent modification exceptions.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class InMemoryBandwidthDao implements IBandwidthDao {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(InMemoryBandwidthDao.class);

    private static final AtomicLong idSequence = new AtomicLong(1);

    // Explicitly ConcurrentLinkedQueue so we can use methods that require that
    // type to be concurrently safe
    private final ConcurrentLinkedQueue<BandwidthAllocation> bandwidthAllocations = new ConcurrentLinkedQueue<BandwidthAllocation>();

    private final ConcurrentLinkedQueue<SubscriptionDao> subscriptionDaos = new ConcurrentLinkedQueue<SubscriptionDao>();

    private final ConcurrentLinkedQueue<DataSetMetaDataDao> dataSetMetaDataDaos = new ConcurrentLinkedQueue<DataSetMetaDataDao>();

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocations(Long subscriptionId) {
        List<BandwidthAllocation> allocations = clone(bandwidthAllocations);

        for (Iterator<BandwidthAllocation> iter = allocations.iterator(); iter
                .hasNext();) {
            BandwidthAllocation current = iter.next();
            if ((current instanceof SubscriptionRetrieval)
                    && ((SubscriptionRetrieval) current).getSubscriptionDao()
                            .getId() == subscriptionId) {
                continue;
            }

            iter.remove();
        }
        return allocations;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocations(Network network) {
        List<BandwidthAllocation> results = clone(bandwidthAllocations);

        for (Iterator<BandwidthAllocation> iter = results.iterator(); iter
                .hasNext();) {
            BandwidthAllocation current = iter.next();
            if (network.equals(current.getNetwork())) {
                continue;
            }

            iter.remove();
        }
        return results;
    }

    /**
     * @param sourceList
     * @return
     */
    @SuppressWarnings("unchecked")
    private static <T> ArrayList<T> clone(ConcurrentLinkedQueue<T> sourceList) {
        ArrayList<T> results;
        try {
            results = BandwidthUtil.cheapClone(ArrayList.class,
                    new ArrayList<T>(sourceList));
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to clone list, returning empty list.", e);
            results = new ArrayList<T>();
        }

        return results;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getBandwidthAllocationsInState(
            RetrievalStatus state) {
        List<BandwidthAllocation> results = clone(bandwidthAllocations);
        for (Iterator<BandwidthAllocation> iter = results.iterator(); iter
                .hasNext();) {
            BandwidthAllocation current = iter.next();
            if (state.equals(current.getStatus())) {
                continue;
            }

            iter.remove();
        }
        return results;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<DataSetMetaDataDao> getDataSetMetaDataDao(String providerName,
            String dataSetName) {
        ArrayList<DataSetMetaDataDao> results = clone(dataSetMetaDataDaos);

        for (Iterator<DataSetMetaDataDao> iter = results.iterator(); iter
                .hasNext();) {
            DataSetMetaDataDao current = iter.next();
            if (providerName.equals(current.getProviderName())
                    && dataSetName.equals(current.getDataSetName())) {
                continue;
            }

            iter.remove();
        }

        return results;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<DataSetMetaDataDao> getDataSetMetaDataDao(String providerName,
            String dataSetName, Calendar baseReferenceTime) {
        List<DataSetMetaDataDao> results = getDataSetMetaDataDao(providerName,
                dataSetName);

        for (Iterator<DataSetMetaDataDao> iter = results.iterator(); iter
                .hasNext();) {
            DataSetMetaDataDao current = iter.next();
            if (current.getDataSetBaseTime().getTimeInMillis() == baseReferenceTime
                    .getTimeInMillis()) {
                continue;
            }

            iter.remove();
        }
        return results;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> getDeferred(Network network,
            Calendar endTime) {
        List<BandwidthAllocation> results = getBandwidthAllocations(network);
        for (Iterator<BandwidthAllocation> iter = results.iterator(); iter.hasNext();) {
            BandwidthAllocation current = iter.next();
            if (RetrievalStatus.DEFERRED.equals(current.getStatus())
                    && !current.getEndTime().after(endTime)) {
                continue;
            }

            iter.remove();
        }

        return results;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionDao getSubscriptionDao(long identifier) {
        ArrayList<SubscriptionDao> subscriptionDaos = clone(this.subscriptionDaos);
        for (SubscriptionDao dao : subscriptionDaos) {
            if (dao.getIdentifier() == identifier) {
                return dao;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionDao getSubscriptionDao(String registryId,
            Calendar baseReferenceTime) {
        List<SubscriptionDao> daos = getSubscriptionDaoByRegistryId(registryId);
        for (SubscriptionDao dao : daos) {
            if (dao.getBaseReferenceTime().getTimeInMillis() == baseReferenceTime
                    .getTimeInMillis()) {
                return dao;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionDao> getSubscriptionDao(Subscription subscription) {
        return getSubscriptionDaoByRegistryId(subscription.getId());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionDao> getSubscriptionDaoByRegistryId(
            String registryId) {
        final ArrayList<SubscriptionDao> results = clone(subscriptionDaos);
        for (Iterator<SubscriptionDao> iter = results
                .iterator(); iter.hasNext();) {
            final SubscriptionDao current = iter.next();
            if (registryId.equals(current.getRegistryId())) {
                continue;
            }

            iter.remove();
        }
        return results;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionRetrieval getSubscriptionRetrieval(long identifier) {
        ArrayList<BandwidthAllocation> clone = clone(bandwidthAllocations);
        for (BandwidthAllocation current : clone) {
            if (current.getId() == identifier
                    && current instanceof SubscriptionRetrieval) {
                return (SubscriptionRetrieval) current;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> getSubscriptionRetrievals(
            String provider, String dataSetName, Calendar baseReferenceTime) {
        List<SubscriptionRetrieval> results = new ArrayList<SubscriptionRetrieval>(
                getSubscriptionRetrievals(provider, dataSetName));
        List<SubscriptionDao> subscriptionsMatching = getSubscriptions(
                provider,
                dataSetName, baseReferenceTime);

        OUTER: for (Iterator<SubscriptionRetrieval> iter = results.iterator(); iter
                .hasNext();) {
            SubscriptionRetrieval current = iter.next();
            for (SubscriptionDao subscription : subscriptionsMatching) {
                if (current.getSubscriptionDao().getId() == subscription
                        .getIdentifier()) {
                    continue OUTER;
                }
            }
            iter.remove();
        }

        return results;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> getSubscriptionRetrievals(
            String provider, String dataSetName) {
        ArrayList<BandwidthAllocation> clone = clone(bandwidthAllocations);
        List<SubscriptionRetrieval> results = new ArrayList<SubscriptionRetrieval>(
                bandwidthAllocations.size());

        for (Iterator<BandwidthAllocation> iter = clone.iterator(); iter
                .hasNext();) {
            BandwidthAllocation current = iter.next();
            if (current instanceof SubscriptionRetrieval) {
                Subscription subscription;
                try {
                    subscription = ((SubscriptionRetrieval) current)
                            .getSubscription();
                    if (provider.equals(subscription.getProvider())
                            && dataSetName
                                    .equals(subscription.getDataSetName())) {
                        results.add((SubscriptionRetrieval) current);
                    }
                } catch (SerializationException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Unable to deserialize the retrieval's subscription, removing it...",
                                    e);
                    iter.remove();
                    continue;
                }

            }
        }

        return results;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionDao> getSubscriptions() {
        return clone(subscriptionDaos);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionDao> getSubscriptions(String provider,
            String dataSetName, Calendar baseReferenceTime) {
        List<SubscriptionDao> subscriptionDaos = getSubscriptions();

        for (Iterator<SubscriptionDao> iter = subscriptionDaos.iterator(); iter
                .hasNext();) {
            SubscriptionDao current = iter.next();
            if (provider.equals(current.getProvider())
                    && dataSetName.equals(current.getDataSetName())
                    && baseReferenceTime.getTimeInMillis() == current
                            .getBaseReferenceTime().getTimeInMillis()) {
                continue;
            }
            iter.remove();
        }

        return subscriptionDaos;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DataSetMetaDataDao newDataSetMetaDataDao(
            DataSetMetaData dataSetMetaData) {
        DataSetMetaDataDao dao = BandwidthUtil
                .newDataSetMetaDataDao(dataSetMetaData);
        dao.setIdentifier(getNextId());

        dataSetMetaDataDaos.add(dao);

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

        update(dao);

        return dao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> querySubscriptionRetrievals(
            long subscriptionId) {
        ArrayList<BandwidthAllocation> clone = clone(bandwidthAllocations);
        List<SubscriptionRetrieval> results = new ArrayList<SubscriptionRetrieval>(
                bandwidthAllocations.size());

        for (Iterator<BandwidthAllocation> iter = clone.iterator(); iter
                .hasNext();) {
            BandwidthAllocation current = iter.next();
            if (current instanceof SubscriptionRetrieval) {
                if (((SubscriptionRetrieval) current).getSubscriptionDao()
                        .getId() == subscriptionId) {
                    results.add((SubscriptionRetrieval) current);
                }
            }
        }

        return results;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SubscriptionRetrieval> querySubscriptionRetrievals(
            SubscriptionDao dao) {
        return querySubscriptionRetrievals(dao.getId());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void remove(SubscriptionDao subscriptionDao) {
        removeFromCollection(subscriptionDaos, subscriptionDao);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(BandwidthAllocation bandwidthAllocation) {
        replaceOldOrAddToCollection(bandwidthAllocations, bandwidthAllocation);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(SubscriptionDao subscriptionDao) {
        replaceOldOrAddToCollection(subscriptionDaos, subscriptionDao);
    }

    /**
     * @return
     */
    private long getNextId() {
        return idSequence.getAndIncrement();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(List<SubscriptionRetrieval> retrievals) {
        for (SubscriptionRetrieval retrieval : retrievals) {
            update(retrieval);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(BandwidthAllocation allocation) {
        replaceOldOrAddToCollection(bandwidthAllocations, allocation);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(SubscriptionDao dao) {
        replaceOldOrAddToCollection(subscriptionDaos, dao);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(SubscriptionRetrieval subscriptionRetrieval) {
        replaceOldOrAddToCollection(bandwidthAllocations,
                subscriptionRetrieval);
    }

    private <T extends IPersistableDataObject<Long>> void replaceOldOrAddToCollection(
            ConcurrentLinkedQueue<T> collection, T obj) {
        if (obj.getIdentifier() == BandwidthUtil.DEFAULT_IDENTIFIER) {
            // Have to reflectively set the identifier since it's not part of
            // the interface
            ReflectionUtil.setter(obj, "identifier", getNextId());
        } else {
            // Always use a greater id than any of the objects in the collection
            long idValue = idSequence.get();
            while (obj.getIdentifier() + 1 > idValue) {
                idValue = idSequence.incrementAndGet();
            }
            removeFromCollection(collection, obj);
        }

        collection.add(obj);
    }

    private <T extends IPersistableDataObject<Long>> void removeFromCollection(
            ConcurrentLinkedQueue<T> collection, T obj) {
        for (Iterator<T> iter = collection.iterator(); iter.hasNext();) {
            T old = iter.next();
            if (old.getIdentifier().equals(obj.getIdentifier())) {
                iter.remove();
                break;
            }
        }
    }
}
