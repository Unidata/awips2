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
package com.raytheon.uf.edex.datadelivery.registry.replication;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import javax.xml.bind.JAXBException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.Duration;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.ws.wsaddressing.W3CEndpointReference;
import javax.xml.ws.wsaddressing.W3CEndpointReferenceBuilder;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DeliveryInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

import org.springframework.transaction.support.TransactionTemplate;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.registry.RegistryException;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.DeliveryMethodTypes;
import com.raytheon.uf.common.registry.constants.Namespaces;
import com.raytheon.uf.common.registry.constants.NotificationOptionTypes;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.RegistryRESTServices;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.datadelivery.registry.availability.FederatedRegistryMonitor;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * Manages subscriptions associated with Data Delivery Registry replication
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/24/2013    1675        bphillip    Initial implementation
 * 6/4/2013     1707        bphillip    Changed to use new NotificationServer objects
 * 7/29/2013    2191        bphillip    Implemented registry sync for registries that have been down for an extended period of time
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryReplicationManager {

    /** The logger instance */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryReplicationManager.class);

    private FederatedRegistryMonitor federatedRegistryMonitor;

    private static final long MAX_DOWN_TIME_DURATION = TimeUtil.MILLIS_PER_DAY
            * 2 - TimeUtil.MILLIS_PER_HOUR;

    /** Scheduler service for monitoring subscription submission tasks */
    private ScheduledExecutorService scheduler;

    /** The servers that we are subscribing to */
    private NotificationServers servers;

    private RegistryObjectDao dao;

    private TransactionTemplate txTemplate;

    /**
     * The JAXBManager instance for marshalling/unmarshalling objects
     */
    private JAXBManager jaxbManager;

    /**
     * The name of the configuration files defining which servers we are
     * sending/receiving replicated objects to/from
     */
    private String replicationConfigFileName;

    /** Environment variable denoting whether subscription processing is enabled */
    private boolean subscriptionProcessingEnabled;

    /** Object types to automatically create subscriptions for */
    private static List<String> objectTypes = new ArrayList<String>();

    /**
     * Creates a new RegistryReplicationManager
     * 
     * @param subscriptionProcessingEnabled
     *            The state of subscription processing
     * @param notificationServerConfigFileName
     *            The replication configuration files
     * @throws JAXBException
     *             If errors occur while creating the JAXBManager
     * @throws SerializationException
     */
    public RegistryReplicationManager(boolean subscriptionProcessingEnabled,
            String notificationServerConfigFileName, RegistryObjectDao dao,
            FederatedRegistryMonitor availabilityMonitor,
            TransactionTemplate txTemplate) throws JAXBException,
            SerializationException {
        this.subscriptionProcessingEnabled = subscriptionProcessingEnabled;
        this.replicationConfigFileName = notificationServerConfigFileName;
        this.dao = dao;
        this.txTemplate = txTemplate;
        this.federatedRegistryMonitor = availabilityMonitor;
        jaxbManager = new JAXBManager(NotificationServers.class,
                SubscriptionType.class);
        File notificationServerConfigFile = PathManagerFactory.getPathManager()
                .getStaticFile(replicationConfigFileName);

        if (notificationServerConfigFile == null) {
            statusHandler
                    .error("Notification server config file not found! Registry replication will be disabled");
            this.subscriptionProcessingEnabled = false;
            return;
        }
        servers = (NotificationServers) jaxbManager
                .jaxbUnmarshalFromXmlFile(notificationServerConfigFile);
        scheduler = Executors.newSingleThreadScheduledExecutor();
    }

    /**
     * Checks how long a registry has been down. If the registry has been down
     * for over 2 days, the registry is synchronized with one of the federation
     * members
     * 
     * @throws MsgRegistryException
     *             If errors occur during registry synchronization
     * @throws EbxmlRegistryException
     *             If errors occur during registry synchronization
     */
    public void checkDownTime() throws MsgRegistryException,
            EbxmlRegistryException {
        long currentTime = TimeUtil.currentTimeMillis();
        long lastKnownUp = federatedRegistryMonitor.getLasKnownUptime();
        long downTime = currentTime - lastKnownUp;
        statusHandler
                .info("Registry has been down since: "
                        + new Date(currentTime - downTime)
                        + ". Checking if synchronization with the federation is necessary...");

        // The registry has been down for ~2 days, this requires a
        // synchronization of the
        // data from the federation
        if (currentTime - lastKnownUp > MAX_DOWN_TIME_DURATION) {
            statusHandler
                    .warn("Registry has been down for ~2 days. Initiating federated data synchronization....");
            List<NotificationHostConfiguration> notificationServers = servers
                    .getRegistryReplicationServers();
            if (servers == null
                    || CollectionUtil.isNullOrEmpty(servers
                            .getRegistryReplicationServers())) {
                statusHandler
                        .warn("No servers configured for replication. Unable to synchronize data with federation!");
            } else {
                NotificationHostConfiguration registryToSyncFrom = null;
                for (NotificationHostConfiguration config : notificationServers) {
                    statusHandler.info("Checking availability of registry at: "
                            + config.getRegistryBaseURL());
                    if (RegistryRESTServices.isRegistryAvailable(config
                            .getRegistryBaseURL())) {
                        registryToSyncFrom = config;
                        break;
                    }

                    statusHandler.info("Registry at "
                            + config.getRegistryBaseURL()
                            + " is not available...");
                }

                // No available registry was found!
                if (registryToSyncFrom == null) {
                    statusHandler
                            .warn("No available registries found! Registry data will not be synchronized with the federation!");
                } else {
                    synchronizeRegistryWithFederation(registryToSyncFrom
                            .getRegistryBaseURL());
                }
            }

        }

        statusHandler.info("Starting federated uptime monitor...");
        scheduler.scheduleAtFixedRate(federatedRegistryMonitor, 0, 1,
                TimeUnit.MINUTES);
    }

    private void synchronizeRegistryWithFederation(String remoteRegistryUrl)
            throws MsgRegistryException, EbxmlRegistryException {
        ExecutorService executor = Executors.newFixedThreadPool(25);
        for (String objectType : objectTypes) {
            Set<String> localIds = new HashSet<String>();
            Set<String> remoteIds = new HashSet<String>();
            statusHandler
                    .info("Getting registry object Ids from local registry...");
            Collection<String> response = RegistryRESTServices
                    .getRegistryDataAccessService(
                            RegistryUtil.LOCAL_REGISTRY_ADDRESS)
                    .getRegistryObjectIdsOfType(objectType).getPayload();
            if (response != null) {
                localIds.addAll(response);
            }
            statusHandler.info(localIds.size() + " objects of type "
                    + objectType + " present in local registry.");
            statusHandler.info("Getting registry object Ids from "
                    + remoteRegistryUrl + "...");
            response = RegistryRESTServices
                    .getRegistryDataAccessService(remoteRegistryUrl)
                    .getRegistryObjectIdsOfType(objectType).getPayload();
            if (response != null) {
                remoteIds.addAll(response);
            }
            statusHandler.info(remoteIds.size() + " objects of type "
                    + objectType + " present on registry at "
                    + remoteRegistryUrl);
            statusHandler.info("Synchronizing objects of type " + objectType
                    + "...");

            /*
             * Iterate through local objects and compare them with the remote
             * object inventory to determine if they need to be updated or
             * deleted locally
             */
            for (String localId : localIds) {
                if (remoteIds.contains(localId)) {
                    RegistryObjectType objectToSubmit;
                    try {
                        objectToSubmit = getRemoteObject(remoteRegistryUrl,
                                localId);
                    } catch (Exception e) {
                        statusHandler.error("Error getting remote object: "
                                + localId, e);
                        continue;
                    }
                    objectToSubmit.addSlot(EbxmlObjectUtil.HOME_SLOT_NAME,
                            remoteRegistryUrl);
                    RegistrySubmitTask submitTask = new RegistrySubmitTask(
                            txTemplate, dao, objectToSubmit, remoteRegistryUrl);
                    executor.submit(submitTask);
                } else {
                    RegistryRemoveTask removeTask = new RegistryRemoveTask(
                            txTemplate, dao, localId);
                    executor.submit(removeTask);
                }
            }

            /*
             * Iterate through the remote objects to see if there are any
             * objects on the remote registry that do not exist local. If found,
             * retrieve them and add them to the local registry
             */
            for (String remoteId : remoteIds) {
                if (!localIds.contains(remoteId)) {
                    RegistryObjectType objectToSubmit;
                    try {
                        objectToSubmit = getRemoteObject(remoteRegistryUrl,
                                remoteId);
                    } catch (Exception e) {
                        statusHandler.error("Error getting remote object: "
                                + remoteId, e);
                        continue;
                    }
                    objectToSubmit.addSlot(EbxmlObjectUtil.HOME_SLOT_NAME,
                            remoteRegistryUrl);
                    RegistrySubmitTask submitTask = new RegistrySubmitTask(
                            txTemplate, dao, objectToSubmit, remoteRegistryUrl);
                    executor.submit(submitTask);
                }
            }
        }

        // Wait for all threads to complete
        executor.shutdown();
        try {
            executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
        } catch (InterruptedException e) {
            statusHandler
                    .error("Registry synchronization using ["
                            + remoteRegistryUrl
                            + "] did not complete successfully!", e);
            throw new EbxmlRegistryException(
                    "Task executor did not shutdown properly!", e);
        }

        statusHandler.info("Registry synchronization using ["
                + remoteRegistryUrl + "] completed successfully!");
    }

    private RegistryObjectType getRemoteObject(String remoteRegistryUrl,
            String objectId) throws Exception {
        final QueryType queryType = new QueryType();
        queryType.setQueryDefinition(CanonicalQueryTypes.GET_OBJECT_BY_ID);
        Set<SlotType> slots = new HashSet<SlotType>();
        final SlotType slot = new SlotType();
        slot.setName(QueryConstants.ID);
        final StringValueType slotValue = new StringValueType();
        slotValue.setStringValue(objectId);
        slot.setSlotValue(slotValue);
        slots.add(slot);
        queryType.setSlot(slots);

        QueryRequest queryRequest = new QueryRequest();
        queryRequest.setResponseOption(new ResponseOptionType(
                QueryReturnTypes.REGISTRY_OBJECT, true));
        queryRequest.setFederated(false);
        queryRequest.setQuery(queryType);
        QueryResponse response = RegistrySOAPServices.getQueryServiceForHost(
                remoteRegistryUrl).executeQuery(queryRequest);
        if (!CollectionUtil.isNullOrEmpty(response.getRegistryObjects())) {
            return response.getRegistryObjects().get(0);
        } else {
            throw new EbxmlRegistryException("Object " + objectId
                    + " not found on remote server!");
        }
    }

    /**
     * Static method only used during the Spring container to inject the object
     * types to subscribe to for registry replication
     * 
     * @param types
     *            The object types to subscribe to
     */
    public static void addObjectTypesToSubscribeTo(String... types) {
        for (String type : types) {
            objectTypes.add(type);
        }
    }

    /**
     * Submits subscriptions to the registry at the provided URL
     * 
     * @param baseURL
     *            The url of the registry to send the subscriptions to
     */
    public void submitRemoteSubscriptions(RegistryType registry) {

        if (subscriptionProcessingEnabled) {
            statusHandler.info("Registry Replication is enabled.");
        } else {
            statusHandler.warn("Registry Replication is disabled");
            // TODO: Add code in here to remove replication
            // subscriptions from remote servers if replication is disabled
            return;

        }

        List<NotificationHostConfiguration> replicationRegistries = servers
                .getRegistryReplicationServers();

        if (CollectionUtil.isNullOrEmpty(replicationRegistries)) {
            statusHandler.info("No registry replication servers configured.");
        } else {
            statusHandler
                    .info("Submitting subscriptions to registry replication servers...");
            for (NotificationHostConfiguration config : replicationRegistries) {
                scheduleSubscriptionSubmission(config, registry);
            }
        }
    }

    /**
     * Schedules a task to submit subscriptions
     * 
     * @param config
     *            The server configuration
     * @param baseURL
     *            The url of the registry to submit the subscriptions to
     */
    private void scheduleSubscriptionSubmission(
            NotificationHostConfiguration config, RegistryType registry) {
        final SubmitSubscriptionTask submitSubscriptionTask = new SubmitSubscriptionTask(
                config, registry);
        final ScheduledFuture<?> future = scheduler.scheduleAtFixedRate(
                submitSubscriptionTask, 0, 10, TimeUnit.SECONDS);
        scheduler.schedule(new Runnable() {
            @Override
            public void run() {
                if (submitSubscriptionTask.success) {
                    statusHandler
                            .info("Subscription submission successful. Cancelling future subscription submission retries");
                    future.cancel(false);
                }

            }
        }, 5, TimeUnit.SECONDS);
    }

    /**
     * Creates and submits objects to the server described in the config object
     * 
     * @param config
     *            The object describing the destination server to make registry
     *            replication subscriptions to
     */
    private void submitSubscriptionsToHost(
            NotificationHostConfiguration config, RegistryType localRegistry) {
        statusHandler
                .info("Generating registry replication subscriptions for registry at ["
                        + config.getRegistrySiteName()
                        + "] at URL ["
                        + config.getRegistryBaseURL() + "]");

        List<RegistryObjectType> subscriptions = new ArrayList<RegistryObjectType>();
        for (String objectType : objectTypes) {
            SubscriptionType subscription;
            try {
                subscription = createSubscription(config.getRegistryBaseURL(),
                        objectType, localRegistry);
            } catch (Exception e) {
                throw new RegistryException("Error creating subscription", e);
            }
            subscriptions.add(subscription);
        }

        SubmitObjectsRequest request = new SubmitObjectsRequest(
                "Subscription Submission", "Subscription Submission", null,
                new RegistryObjectListType(subscriptions), false,
                Mode.CREATE_OR_REPLACE);
        RegistrySOAPServices.sendSubmitObjectsRequest(request,
                config.getRegistryBaseURL());

    }

    /**
     * Creates a new subscription object
     * 
     * @param host
     *            The destination host
     * @param objectType
     *            The object type to create the subscription for
     * @return The subscription object
     * @throws Exception
     *             If errors occur while creating the subscription object
     */
    private SubscriptionType createSubscription(String host, String objectType,
            RegistryType registry) throws Exception {
        // Set normal registry object fields
        String subscriptionDetail = "Replication Subscription for ["
                + objectType + "] objects for server [" + registry.getBaseURL()
                + "]";
        SubscriptionType sub = new SubscriptionType();
        sub.setId(subscriptionDetail);
        sub.setLid(subscriptionDetail);
        sub.setObjectType(RegistryObjectTypes.SUBSCRIPTION);
        sub.setName(RegistryUtil.getInternationalString(subscriptionDetail));
        sub.setDescription(RegistryUtil
                .getInternationalString(subscriptionDetail));
        VersionInfoType version = new VersionInfoType();
        version.setVersionName("1");
        version.setUserVersionName("1");
        sub.setVersionInfo(version);
        sub.setOwner(registry.getOwner());
        sub.setStatus(StatusTypes.APPROVED);

        sub.setStartTime(EbxmlObjectUtil.getTimeAsXMLGregorianCalendar(0));
        QueryType selectorQuery = new QueryType();
        selectorQuery.setQueryDefinition(CanonicalQueryTypes.BASIC_QUERY);
        SlotType slot = new SlotType();
        StringValueType valType = new StringValueType();
        valType.setValue(objectType);
        slot.setName("objectType");
        slot.setSlotValue(valType);
        selectorQuery.getSlot().add(slot);
        sub.setSelector(selectorQuery);

        Duration notificationInterval = DatatypeFactory.newInstance()
                .newDuration(0);
        sub.setNotificationInterval(notificationInterval);

        String endpointType = DeliveryMethodTypes.SOAP;
        W3CEndpointReferenceBuilder builder = new W3CEndpointReferenceBuilder();
        builder.address(RegistrySOAPServices.getNotificationListenerServiceUrl(
                registry.getBaseURL()).toString());
        W3CEndpointReference ref = builder.build();
        DOMResult dom = new DOMResult();
        ref.writeTo(dom);
        Document doc = (Document) dom.getNode();
        NodeList nodes = doc.getElementsByTagNameNS(
                Namespaces.ADDRESSING_NAMESPACE, "Address");
        for (int i = 0; i < nodes.getLength(); i++) {
            Node addressNode = nodes.item(i);
            Attr endpointTypeAttr = doc.createAttributeNS(
                    Namespaces.EBXML_RIM_NAMESPACE_URI, "endpointType");
            endpointTypeAttr.setValue(endpointType);
            addressNode.getAttributes().setNamedItemNS(endpointTypeAttr);
        }
        ref = new W3CEndpointReference(new DOMSource(dom.getNode()));

        // Set subscription specific fields
        DeliveryInfoType deliveryInfo = new DeliveryInfoType();
        deliveryInfo.setNotificationOption(NotificationOptionTypes.OBJECT_REFS);
        deliveryInfo.setNotifyTo(ref);
        sub.getDeliveryInfo().add(deliveryInfo);
        return sub;
    }

    /**
     * 
     * Task for submitting subscriptions to a remote registry
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#     Engineer    Description
     * ------------ ----------  ----------- --------------------------
     * 5/21/2013    1707        bphillip    Initial implementation
     * </pre>
     * 
     * @author bphillip
     * @version 1
     */
    private class SubmitSubscriptionTask implements Runnable {

        /**
         * Denotes if the subscription has successfully been submitted to the
         * remote server
         */
        private boolean success = false;

        /** The server configuration */
        private NotificationHostConfiguration config;

        /** The remote registry */
        private RegistryType registry;

        /**
         * Creates a new SubmitSubscriptionTask
         * 
         * @param config
         *            The server configuration
         * @param baseURL
         *            The base URL of the remote registry
         */
        public SubmitSubscriptionTask(NotificationHostConfiguration config,
                RegistryType registry) {
            this.config = config;
            this.registry = registry;
        }

        @Override
        public void run() {
            if (!success) {
                final String remoteRegistryBaseURL = config
                        .getRegistryBaseURL();
                statusHandler.info("Checking if remote registry at ["
                        + remoteRegistryBaseURL + "] is available...");

                if (RegistryRESTServices
                        .isRegistryAvailable(remoteRegistryBaseURL)) {
                    statusHandler.info("Registry at [" + remoteRegistryBaseURL
                            + "] is available!");
                } else {
                    statusHandler.error("Registry at [" + remoteRegistryBaseURL
                            + "] is not available! Retrying in 10 seconds...");
                    success = false;
                    return;
                }
                try {
                    statusHandler
                            .info("Removing remote subscriptions prior to submission of new subscriptions");
                    RegistryRESTServices.getRegistryDataAccessService(
                            config.getRegistryBaseURL())
                            .removeSubscriptionsForSite(registry.getOwner());
                    submitSubscriptionsToHost(config, registry);

                    /*
                     * Adds a hook to remove the subscriptions from the remote
                     * server when this server shuts down
                     */
                    Runtime.getRuntime().addShutdownHook(new Thread() {
                        public void run() {
                            RegistryRESTServices.getRegistryDataAccessService(
                                    remoteRegistryBaseURL)
                                    .removeSubscriptionsForSite(
                                            registry.getOwner());
                        }
                    });
                    success = true;
                } catch (Exception e) {
                    statusHandler
                            .error("Error submitting subscriptions! Retrying in 10 seconds...",
                                    e);
                    success = false;
                }
            }
        }

    }

    public void setSubscriptionProcessingEnabled(
            boolean subscriptionProcessingEnabled) {
        this.subscriptionProcessingEnabled = subscriptionProcessingEnabled;
    }

    public NotificationServers getServers() {
        return servers;
    }

}
