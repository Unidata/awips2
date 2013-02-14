/**
 * 
 */
package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalGenerator;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalManagerNotifyEvent;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceTypeFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter.TranslationException;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalHandler;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.util.RetrievalPersistUtil;
import com.raytheon.uf.edex.event.EventBus;

/**
 * Class used to process SubscriptionRetrieval BandwidthAllocations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2012 726        jspinks     Initial release.
 * Oct 10, 2012 0726       djohnson    Add generics, constants, defaultPriority.
 * Nov 26, 2012            dhladky     Override default ingest routes based on plugin
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class SubscriptionRetrievalAgent extends
        RetrievalAgent<SubscriptionRetrieval> implements
        IRetrievalHandler {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionRetrievalAgent.class);

    public static final String SUBSCRIPTION_AGENT = "SubscriptionAgent";

    private final int defaultPriority;

    private final IBandwidthDao bandwidthDao;



    public SubscriptionRetrievalAgent(Network network, String destinationUri,
            final Object notifier, int defaultPriority,
            RetrievalManager retrievalManager, IBandwidthDao bandwidthDao) {
        super(network, destinationUri, notifier, retrievalManager);
        this.defaultPriority = defaultPriority;
        this.bandwidthDao = bandwidthDao;
    }

    @Override
    void processAllocation(SubscriptionRetrieval retrieval)
            throws EdexException {
        Subscription sub;
        try {
            sub = retrieval.getSubscription();
        } catch (SerializationException e) {
            throw new EdexException("Unable to deserialize the subscription.",
                    e);
        }
        final String originalSubName = sub.getName();

        SubscriptionBundle bundle = new SubscriptionBundle();
        Provider provider = getProvider(sub.getProvider());
        if (provider == null) {
            statusHandler.error("provider was null, skipping subscription ["
                    + originalSubName + "]");
            return;
        }
        bundle.setBundleId(sub.getSubscriptionId());
        bundle.setPriority((int) retrieval.getPriority());
        bundle.setProvider(provider);
        bundle.setConnection(provider.getConnection());
        bundle.setSubscription(sub);

        retrieval.setStartTime(TimeUtil.newCalendar());
        retrieval.setStatus(RetrievalStatus.RETRIEVAL);

        // update database
        bandwidthDao.update(retrieval);

        // Handler will pipeline the Retrieval Objects created to the
        // Database where the pool of RetrievalTasks will process them
        boolean retrievalsGenerated = generateRetrieval(bundle,
                retrieval.getIdentifier());
        if (retrievalsGenerated) {
            // Wake the RetrievalTasks to fetch the data..
            EDEXUtil.getMessageProducer().sendAsync(destinationUri, null);
            EDEXUtil.getMessageProducer().sendAsync(destinationUri, null);
            EDEXUtil.getMessageProducer().sendAsync(destinationUri, null);
            EDEXUtil.getMessageProducer().sendAsync(destinationUri, null);
        } else {
            // Normally this is the job of the SubscriptionNotifyTask, but if no
            // retrievals were generated we have to send it manually
            RetrievalManagerNotifyEvent retrievalManagerNotifyEvent = new RetrievalManagerNotifyEvent();
            retrievalManagerNotifyEvent.setId(Long.toString(retrieval.getId()));
            EventBus.getInstance().publish(retrievalManagerNotifyEvent);
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
    private boolean generateRetrieval(SubscriptionBundle bundle,
            Long subRetrievalKey) {

        RetrievalDao dao = new RetrievalDao();
        // process the bundle into a retrieval
        RetrievalGenerator rg = ServiceTypeFactory.retrieveServiceFactory(
                bundle.getProvider()).getRetrievalGenerator();

        final String subscriptionName = bundle.getSubscription().getName();
        statusHandler.info("Subscription: " + subscriptionName
                + " Being Processed for Retrieval...");

        List<Retrieval> retrievals = rg.buildRetrieval(bundle);

        boolean retrievalsGenerated = !CollectionUtil.isNullOrEmpty(retrievals);
        if (retrievalsGenerated) {

            String owner = bundle.getSubscription().getOwner();
            String provider = bundle.getSubscription().getProvider();

            int priority = defaultPriority;
            Integer bundlePriority = bundle.getPriority();
            if (bundlePriority != null) {
                priority = bundlePriority.intValue();
            }
            Date insertTime = TimeUtil.newCalendar().getTime();

            List<RetrievalRequestRecord> requestRecords = new ArrayList<RetrievalRequestRecord>(
                    retrievals.size());

            ITimer timer = TimeUtil.getTimer();
            timer.start();

            final int numberOfRetrievals = retrievals.size();
            for (int i = 0; i < numberOfRetrievals; i++) {
                Retrieval retrieval = retrievals.get(i);
                RetrievalRequestRecord rec = new RetrievalRequestRecord(
                        subscriptionName, i, subRetrievalKey);

                rec.setOwner(owner);
                rec.setPriority(priority);
                rec.setInsertTime(insertTime);
                rec.setNetwork(retrieval.getNetwork());
                rec.setProvider(provider);
                rec.setPlugin(retrieval.getProviderType().getPlugin());
                rec.setSubscriptionType(retrieval.getSubscriptionType());

                try {
                    rec.setRetrieval(SerializationUtil
                            .transformToThrift(retrieval));
                    rec.setState(RetrievalRequestRecord.State.PENDING);
                } catch (Exception e) {
                    statusHandler.error("Subscription: " + subscriptionName
                            + " Failed to serialize request [" + retrieval
                            + "]", e);
                    rec.setRetrieval(new byte[0]);
                    rec.setState(RetrievalRequestRecord.State.FAILED);
                }

                requestRecords.add(rec);
            }

            timer.stop();
            statusHandler.info("Cumulative time to create ["
                    + numberOfRetrievals + "] request records ["
                    + timer.getElapsedTime() + "] ms");

            try {
                timer.reset();
                timer.start();

                dao.persistAll(requestRecords);

                timer.stop();
                statusHandler.info("Time to persist requests to db ["
                        + timer.getElapsedTime() + "] ms");
            } catch (Exception e) {
                statusHandler.warn("Subscription: " + subscriptionName
                        + " Failed to store to retrievals.");
            }
        } else {
            statusHandler.warn("Subscription: " + subscriptionName
                    + " Did not generate any retrieval messages");
        }

        return retrievalsGenerated;
    }

    /**
     * The actual work gets done here.
     */
    @Override
    public boolean process(Retrieval retrieval) {
        boolean success = true;

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.debug("Starting Retrieval: Subscription: "
                + retrieval.getSubscriptionName());
        }

        RetrievalAdapter pra = ServiceTypeFactory
                .retrieveServiceRetrievalAdapter(retrieval.getServiceType());
        String adapterClassName = pra.getClass().getName();

        try {

            pra.setProviderRetrievalXML(retrieval);

            // could have multiple retrievals
            for (RetrievalAttribute attXML : retrieval.getAttribute()) {
                IRetrievalRequestBuilder request = pra
                        .createRequestMessage(attXML);
                statusHandler
                        .info("Translated provider attribute Request XML: "
                                + request.getRequest());
                IRetrievalResponse response = null;

                if (request != null) {

                    response = pra.performRequest(request);
                    HashMap<String, PluginDataObject[]> pdoHash = null;

                    if (response != null) {

                        pdoHash = pra.processResponse(response);

                        if (pdoHash != null && pdoHash.size() > 0) {
                            // store all types
                            for (Entry<String, PluginDataObject[]> entry : pdoHash
                                    .entrySet()) {
                                PluginDataObject[] value = entry.getValue();
                                if (store(attXML, value)) {
                                    statusHandler.info("Successfully stored: "
                                            + value.length + " : "
                                            + adapterClassName + " Plugin : "
                                            + entry.getKey());
                                } else {
                                    throw new IllegalStateException(
                                            "Unable to store " + value.length
                                                    + " PDOs to the database!");
                                }
                            }
                        } else {
                            throw new IllegalStateException(
                                    "No PDO's to store: " + adapterClassName
                                            + " original: " + attXML.toString());
                        }
                    } else {
                        // null response
                        throw new IllegalStateException(
                                "Null response from provider: "
                                        + adapterClassName + " original: "
                                        + attXML.toString());
                    }
                }
            }
        } catch (IllegalStateException e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            success = false;
        } catch (TranslationException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
            success = false;
        }

        return success;
    }

    /**
     * Store PDO's from Provider to EDEX
     */
    @Override
    public boolean store(RetrievalAttribute attXML, PluginDataObject[] pdos) {

        boolean success = false;
        String pluginName = pdos[0].getPluginName();

        if (pluginName != null) {

            success = RetrievalPersistUtil.routePlugin(destinationUri,
                    pluginName, pdos);
        }

        return success;
    }

    private static Provider getProvider(String providerName) {
        try {
            return DataDeliveryHandlers.getProviderHandler().getByName(
                    providerName);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve provider by name.", e);
            return null;
        }
    }
}
