
/**
 * 
 */
package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.registry.handlers.IProviderHandler;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalGenerator;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalManagerNotifyEvent;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceTypeFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecordPK;
import com.raytheon.uf.edex.datadelivery.retrieval.util.RetrievalGeneratorUtilities;

/**
 * Class used to process SubscriptionRetrieval BandwidthAllocations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2012 726        jspinks      Initial release.
 * Oct 10, 2012 0726       djohnson     Add generics, constants, defaultPriority.
 * Nov 26, 2012            dhladky      Override default ingest routes based on plugin
 * Jan 30, 2013 1543       djohnson     Should not implement IRetrievalHandler.
 * Feb 05, 2013 1580       mpduff       EventBus refactor.
 * Jun 24, 2013 2106       djohnson     Set actual start time when sending to retrieval rather than overwrite scheduled start.
 * Jul 09, 2013 2106       djohnson     Dependency inject registry handlers.
 * Jul 11, 2013 2106       djohnson     Use SubscriptionPriority enum.
 * Jan 15, 2014 2678       bgonzale     Use Queue for passing RetrievalRequestRecords to the 
 *                                      RetrievalTasks (PerformRetrievalsThenReturnFinder).
 *                                      Added constructor that sets the retrievalQueue to null.
 * Jan 30, 2014   2686     dhladky      refactor of retrieval.
 * Feb 10, 2014  2678      dhladky      Prevent duplicate allocations.
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class SubscriptionRetrievalAgent extends
        RetrievalAgent<SubscriptionRetrieval> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionRetrievalAgent.class);

    private final int defaultPriority;

    private final IBandwidthDao<?, ?> bandwidthDao;

    private final IRetrievalDao retrievalDao;

    private final IProviderHandler providerHandler;

    public SubscriptionRetrievalAgent(Network network, String destinationUri,
            final Object notifier, int defaultPriority,
            RetrievalManager retrievalManager, IBandwidthDao<?, ?> bandwidthDao,
            IRetrievalDao retrievalDao, IProviderHandler providerHandler) {
        super(network, destinationUri, notifier, retrievalManager);
        this.defaultPriority = defaultPriority;
        this.bandwidthDao = bandwidthDao;
        this.retrievalDao = retrievalDao;
        this.providerHandler = providerHandler;
    }

    @Override
    void processAllocations(List<SubscriptionRetrieval> subRetrievals)
            throws EdexException {

        SubscriptionBundle bundle = new SubscriptionBundle();
        ConcurrentHashMap<Subscription<?, ?>, SubscriptionRetrieval> retrievalsMap = new ConcurrentHashMap<Subscription<?, ?>, SubscriptionRetrieval>();

        // Get subs from allocations and search for duplicates
        for (SubscriptionRetrieval subRetrieval : subRetrievals) {

            Subscription<?, ?> sub = null;

            try {
                sub = bandwidthDao.getSubscriptionRetrievalAttributes(
                        subRetrieval).getSubscription();
            } catch (SerializationException e) {
                throw new EdexException(
                        "Unable to deserialize the subscription.", e);
            }

            // We only allow one subscription retrieval per DSM update.
            // Remove any duplicate subscription allocations/retrievals, cancel them.
            if (!retrievalsMap.containsKey(sub)) {
                retrievalsMap.put(sub, subRetrieval);
            } else {
                // Check for most recent startTime, that's the one we want for
                // retrieval.
                SubscriptionRetrieval currentRetrieval = retrievalsMap.get(sub);
                if (subRetrieval.getStartTime().getTime()
                        .after(currentRetrieval.getStartTime().getTime())) {
                    // Replace it in the map, set previous to canceled.
                    currentRetrieval.setStatus(RetrievalStatus.CANCELLED);
                    bandwidthDao.update(currentRetrieval);
                    retrievalsMap.replace(sub, subRetrieval);
                    statusHandler
                            .info("More recent, setting previous allocation to Cancelled ["
                                    + currentRetrieval.getIdentifier()
                                    + "] "
                                    + sub.getName());
                } else {
                    // Not more recent, cancel
                    subRetrieval.setStatus(RetrievalStatus.CANCELLED);
                    bandwidthDao.update(subRetrieval);
                    statusHandler
                            .info("Older, setting to Cancelled ["
                                    + currentRetrieval.getIdentifier() + "] "
                                    + sub.getName());
                }
            }
        }

        for (Entry<Subscription<?, ?>, SubscriptionRetrieval> entry: retrievalsMap.entrySet()) {

            SubscriptionRetrieval retrieval = entry.getValue();
            Subscription<?, ?> sub = entry.getKey();
            final String originalSubName = sub.getName();

            Provider provider = getProvider(sub.getProvider());
            if (provider == null) {
                statusHandler
                        .error("provider was null, skipping subscription ["
                                + originalSubName + "]");
                return;
            }
            bundle.setBundleId(sub.getSubscriptionId());
            bundle.setPriority(retrieval.getPriority());
            bundle.setProvider(provider);
            bundle.setConnection(provider.getConnection());
            bundle.setSubscription(sub);

            retrieval.setActualStart(TimeUtil.newGmtCalendar());
            retrieval.setStatus(RetrievalStatus.RETRIEVAL);

            // update database
            bandwidthDao.update(retrieval);

            // generateRetrieval will pipeline the RetrievalRecord Objects
            // created to the DB.
            // The PK objects returned are sent to the RetrievalQueue for
            // processing.
            List<RetrievalRequestRecordPK> retrievals = generateRetrieval(
                    bundle, retrieval.getIdentifier());

            if (!CollectionUtil.isNullOrEmpty(retrievals)) {
                try {
                    Object[] payload = retrievals.toArray();
                    RetrievalGeneratorUtilities.sendToRetrieval(destinationUri,
                            network, payload);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Couldn't send RetrievalRecords to Queue!", e);
                }
                statusHandler.info("Sent " + retrievals.size()
                        + " retrievals to queue. " + network.toString());
            } else {
                // Normally this is the job of the SubscriptionNotifyTask, but
                // if no
                // retrievals were generated we have to send it manually
                RetrievalManagerNotifyEvent retrievalManagerNotifyEvent = new RetrievalManagerNotifyEvent();
                retrievalManagerNotifyEvent.setId(Long.toString(retrieval
                        .getId()));
                EventBus.publish(retrievalManagerNotifyEvent);
            }
        }
    }
    
    @Override
    protected String getAgentType() {
        return SUBSCRIPTION_AGENT;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Class<SubscriptionRetrieval> getAllocationTypeClass() {
        return SubscriptionRetrieval.class;
    }

    /**
     * Generate the retrievals for a subscription bundle.
     * 
     * @param bundle
     *            the bundle
     * @param subRetrievalKey
     *            the subscription retrieval key
     * @return true if retrievals were generated (and waiting to be processed)
     */
    private List<RetrievalRequestRecordPK> generateRetrieval(SubscriptionBundle bundle,
            Long subRetrievalKey) {

        // process the bundle into a retrieval
        RetrievalGenerator rg = ServiceTypeFactory.retrieveServiceFactory(
                bundle.getProvider()).getRetrievalGenerator();

        final String subscriptionName = bundle.getSubscription().getName();
        statusHandler.info("Subscription: " + subscriptionName
                + " Being Processed for Retrieval...");

        List<Retrieval> retrievals = rg.buildRetrieval(bundle);
        List<RetrievalRequestRecord> requestRecords = null;
        List<RetrievalRequestRecordPK> requestRecordPKs = null;
        boolean retrievalsGenerated = !CollectionUtil.isNullOrEmpty(retrievals);
        
        if (retrievalsGenerated) {

            String owner = bundle.getSubscription().getOwner();
            String provider = bundle.getSubscription().getProvider();

            int priority = (bundle.getPriority() != null) ? bundle
                    .getPriority().getPriorityValue() : defaultPriority;
            Date insertTime = TimeUtil.newDate();
            requestRecords = new ArrayList<RetrievalRequestRecord>(
                    retrievals.size());
            requestRecordPKs = new ArrayList<RetrievalRequestRecordPK>(
                    retrievals.size());

            ITimer timer = TimeUtil.getTimer();
            timer.start();

            final int numberOfRetrievals = retrievals.size();
            final ProviderType providerType = bundle.getProvider()
                    .getProviderType(bundle.getDataType());
            final String plugin = providerType.getPlugin();
            for (int i = 0; i < numberOfRetrievals; i++) {
                Retrieval retrieval = retrievals.get(i);
                RetrievalRequestRecord rec = new RetrievalRequestRecord(
                        subscriptionName, i, subRetrievalKey);

                rec.setOwner(owner);
                rec.setPriority(priority);
                rec.setInsertTime(insertTime);
                rec.setNetwork(retrieval.getNetwork());
                rec.setProvider(provider);
                rec.setPlugin(plugin);
                rec.setSubscriptionType(retrieval.getSubscriptionType());

                try {
                    rec.setRetrieval(SerializationUtil
                            .transformToThrift(retrieval));
                    rec.setState(RetrievalRequestRecord.State.PENDING);
                    requestRecords.add(rec);
                    requestRecordPKs.add(rec.getId());
                } catch (Exception e) {
                    statusHandler.error("Subscription: " + subscriptionName
                            + " Failed to serialize request [" + retrieval
                            + "]", e);
                    rec.setRetrieval(new byte[0]);
                    rec.setState(RetrievalRequestRecord.State.FAILED);
                }
            }

            timer.stop();
            statusHandler.info("Cumulative time to create ["
                    + numberOfRetrievals + "] request records ["
                    + timer.getElapsedTime() + "] ms");

            try {
                timer.reset();
                timer.start();

                retrievalDao.persistAll(requestRecords);

                timer.stop();
                statusHandler.info("Time to persist requests to db ["
                        + timer.getElapsedTime() + "] ms");
            } catch (Exception e) {
                statusHandler.handle(Priority.WARN, "Subscription: "
                        + subscriptionName + " Failed to store to retrievals.",
                        e);
                requestRecordPKs.clear();
            }
        } else {
            statusHandler.warn("Subscription: " + subscriptionName
                    + " Did not generate any retrieval messages");
        }

        return requestRecordPKs;
    }

    private Provider getProvider(String providerName) {
        try {
            return providerHandler.getByName(providerName);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve provider by name.", e);
            return null;
        }
    }

}