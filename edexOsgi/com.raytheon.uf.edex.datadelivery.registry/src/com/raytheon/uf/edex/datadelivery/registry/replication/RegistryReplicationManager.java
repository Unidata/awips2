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
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
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

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
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

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.RegistryException;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.DeliveryMethodTypes;
import com.raytheon.uf.common.registry.constants.NotificationOptionTypes;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.datadelivery.registry.availability.FederatedRegistryMonitor;
import com.raytheon.uf.edex.datadelivery.registry.web.DataDeliveryRESTServices;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.exception.NoReplicationServersAvailableException;
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
 * 8/1/2013     1693        bphillip    Switch to use rest service instead of query manager for federation synchronization
 * 9/5/2013     1538        bphillip    Changed when the registry availability monitor is started
 * 10/20/2013   1682        bphillip    Fixed query invocation
 * 10/30/2013   1538        bphillip    Changed method visibility, added add/remove/save notification servers and updated to use non-static rest/soap clients
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
     * When a federation sync is necessary, this is the number of threads that
     * will be used for synchronization. Configurable in the
     * com.raytheon.uf.edex.registry.ebxml.properties file. Default is 5
     */
    private int registrySyncThreads = 5;

    /** Maximum times this registry will try to sync data before failure */
    private int maxSyncRetries = 3;

    /** Data Delivery REST services client */
    private DataDeliveryRESTServices dataDeliveryRestClient;

    /** REgistry Soap client */
    private RegistrySOAPServices registrySoapClient;

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
     * @throws MalformedURLException
     */
    public RegistryReplicationManager(boolean subscriptionProcessingEnabled,
            RegistryObjectDao dao,
            FederatedRegistryMonitor availabilityMonitor,
            TransactionTemplate txTemplate) throws JAXBException,
            SerializationException, MalformedURLException {
        this.subscriptionProcessingEnabled = subscriptionProcessingEnabled;
        if (System.getProperty("edex.run.mode").equals("centralRegistry")) {
            this.replicationConfigFileName = "ebxml/notification/notificationServers_NCF.xml";
        } else {
            this.replicationConfigFileName = "ebxml/notification/notificationServers_WFO.xml";
        }
        this.dao = dao;
        this.txTemplate = txTemplate;
        this.federatedRegistryMonitor = availabilityMonitor;
        if (System.getProperty("ebxml-federation-sync-threads") != null) {
            registrySyncThreads = Integer.valueOf(System
                    .getProperty("ebxml-federation-sync-threads"));
        }
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
        servers = jaxbManager.unmarshalFromXmlFile(NotificationServers.class,
                notificationServerConfigFile);
        scheduler = Executors.newSingleThreadScheduledExecutor();
    }

    /**
     * Removes a notificationServer based on the URL.
     * 
     * @param baseURL
     *            The URL of the server to be removed
     */
    public void removeNotificationServer(String baseURL) {
        NotificationHostConfiguration toRemove = null;
        for (NotificationHostConfiguration hostConfig : this.servers
                .getRegistryReplicationServers()) {
            if (hostConfig.getRegistryBaseURL().equals(baseURL)) {
                toRemove = hostConfig;
            }
        }
        if (toRemove != null) {
            this.servers.getRegistryReplicationServers().remove(toRemove);
        }
    }

    /**
     * Adds a notification server to the list.
     * 
     * @param host
     *            The host to be added
     */
    public void addNotificationServer(NotificationHostConfiguration host) {
        for (NotificationHostConfiguration hostConfig : this.servers
                .getRegistryReplicationServers()) {
            if (hostConfig.getRegistryBaseURL().equals(
                    host.getRegistryBaseURL())) {
                return;
            }
        }
        this.servers.getRegistryReplicationServers().add(host);
    }

    /**
     * Persists the list of notification servers to the localized file
     */
    public void saveNotificationServers() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.EDEX_STATIC,
                LocalizationLevel.CONFIGURED);
        LocalizationFile lf = pm.getLocalizationFile(lc,
                this.replicationConfigFileName);
        File file = lf.getFile();

        try {
            jaxbManager.marshalToXmlFile(this.servers, file.getAbsolutePath());
            lf.save();
        } catch (SerializationException e) {
            statusHandler.error("Unable to update replication server file!", e);
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Checks how long a registry has been down. If the registry has been down
     * for over 2 days, the registry is synchronized with one of the federation
     * members
     * 
     * @throws Exception
     */
    public void checkDownTime() throws Exception {
        long currentTime = TimeUtil.currentTimeMillis();
        long lastKnownUp = federatedRegistryMonitor.getLastKnownUptime();
        long downTime = currentTime - lastKnownUp;
        statusHandler
                .info("Registry has been down since: "
                        + new Date(currentTime - downTime)
                        + ". Checking if synchronization with the federation is necessary...");

        // The registry has been down for ~2 days, this requires a
        // synchronization of the
        // data from the federation
        if (currentTime - lastKnownUp > MAX_DOWN_TIME_DURATION) {
            int syncAttempt = 1;
            for (; syncAttempt <= maxSyncRetries; syncAttempt++) {
                try {
                    statusHandler
                            .warn("Registry has been down for ~2 days. Initiating federated registry data synchronization attempt #"
                                    + syncAttempt
                                    + "/"
                                    + maxSyncRetries
                                    + "...");
                    List<NotificationHostConfiguration> notificationServers = servers
                            .getRegistryReplicationServers();
                    if (servers == null
                            || CollectionUtil.isNullOrEmpty(servers
                                    .getRegistryReplicationServers())) {
                        statusHandler
                                .error("No servers configured for replication. Unable to synchronize registry data with federation!");
                    } else {
                        NotificationHostConfiguration registryToSyncFrom = null;
                        for (NotificationHostConfiguration config : notificationServers) {
                            statusHandler
                                    .info("Checking availability of registry at: "
                                            + config.getRegistryBaseURL());
                            if (dataDeliveryRestClient
                                    .isRegistryAvailable(config
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
                            throw new NoReplicationServersAvailableException(
                                    "No available registries found! Registry data will not be synchronized with the federation!");
                        } else {
                            synchronizeRegistryWithFederation(registryToSyncFrom
                                    .getRegistryBaseURL());

                            break;
                        }
                    }
                } catch (Exception e) {
                    // If no servers are found, don't retry, just throw the
                    // exception
                    if (e instanceof NoReplicationServersAvailableException) {
                        throw e;
                    }
                    if (syncAttempt < maxSyncRetries) {
                        statusHandler.error(
                                "Federation registry data synchronization attempt #"
                                        + syncAttempt + "/" + maxSyncRetries
                                        + " failed! Retrying...", e);
                    } else {
                        statusHandler
                                .fatal("Federation registry data synchronization has failed",
                                        e);
                        throw e;
                    }
                }
            }
        }
        statusHandler.info("Starting federated uptime monitor...");
        scheduler.scheduleAtFixedRate(federatedRegistryMonitor, 0, 1,
                TimeUnit.MINUTES);
    }

    /**
     * Synchronizes this registry's data with the registry at the specified URL
     * 
     * @param remoteRegistryUrl
     *            The URL of the registry to sync with
     * @throws EbxmlRegistryException
     *             If the thread executor fails to shut down properly
     */
    public void synchronizeRegistryWithFederation(String remoteRegistryUrl)
            throws EbxmlRegistryException {
        ExecutorService executor = Executors
                .newFixedThreadPool(this.registrySyncThreads);
        for (String objectType : objectTypes) {
            Set<String> localIds = new HashSet<String>();
            Set<String> remoteIds = new HashSet<String>();
            statusHandler
                    .info("Getting registry object Ids from local registry...");
            Collection<String> response = dataDeliveryRestClient
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
            response = dataDeliveryRestClient
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
                    executor.submit(new RegistrySubmitTask(txTemplate, dao,
                            localId, remoteRegistryUrl, dataDeliveryRestClient));
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
                    executor.submit(new RegistrySubmitTask(txTemplate, dao,
                            remoteId, remoteRegistryUrl, dataDeliveryRestClient));
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
    public void submitSubscriptionsToHost(NotificationHostConfiguration config,
            RegistryType localRegistry) {
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
        registrySoapClient.sendSubmitObjectsRequest(request,
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
        selectorQuery.setQueryDefinition(CanonicalQueryTypes.ADHOC_QUERY);

        SlotType expressionSlot = new SlotType();
        StringValueType expressionValue = new StringValueType();
        expressionValue
                .setValue("FROM RegistryObjectType obj where obj.objectType='"
                        + objectType + "'");
        expressionSlot.setName(QueryConstants.QUERY_EXPRESSION);
        expressionSlot.setSlotValue(expressionValue);
        selectorQuery.getSlot().add(expressionSlot);

        SlotType languageSlot = new SlotType();
        StringValueType languageValue = new StringValueType();
        languageValue.setValue(QueryLanguages.HQL);
        languageSlot.setName(QueryConstants.QUERY_LANGUAGE);
        languageSlot.setSlotValue(languageValue);
        selectorQuery.getSlot().add(languageSlot);

        sub.setSelector(selectorQuery);

        Duration notificationInterval = DatatypeFactory.newInstance()
                .newDuration(0);
        sub.setNotificationInterval(notificationInterval);

        String endpointType = DeliveryMethodTypes.SOAP;
        W3CEndpointReferenceBuilder builder = new W3CEndpointReferenceBuilder();
        builder.address(registrySoapClient.getNotificationListenerServiceUrl(
                registry.getBaseURL()).toString());
        W3CEndpointReference ref = builder.build();
        DOMResult dom = new DOMResult();
        ref.writeTo(dom);
        Document doc = (Document) dom.getNode();
        NodeList nodes = doc.getElementsByTagNameNS(
                EbxmlNamespaces.ADDRESSING_URI, "Address");
        for (int i = 0; i < nodes.getLength(); i++) {
            Node addressNode = nodes.item(i);
            Attr endpointTypeAttr = doc.createAttributeNS(
                    EbxmlNamespaces.RIM_URI, "endpointType");
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

                if (dataDeliveryRestClient
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
                    dataDeliveryRestClient.getRegistryDataAccessService(
                            config.getRegistryBaseURL())
                            .removeSubscriptionsForSite(registry.getOwner());
                    submitSubscriptionsToHost(config, registry);

                    /*
                     * Adds a hook to remove the subscriptions from the remote
                     * server when this server shuts down
                     */
                    Runtime.getRuntime().addShutdownHook(new Thread() {
                        public void run() {
                            statusHandler
                                    .info("Registry shutting down. Removing subscriptions from: ["
                                            + remoteRegistryBaseURL + "]");
                            dataDeliveryRestClient
                                    .getRegistryDataAccessService(
                                            remoteRegistryBaseURL)
                                    .removeSubscriptionsForSite(
                                            registry.getOwner());
                            statusHandler.info("Subscriptions removed from: ["
                                    + remoteRegistryBaseURL + "]");
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

    public static List<String> getObjectTypes() {
        return Collections.unmodifiableList(objectTypes);
    }

    public void setSubscriptionProcessingEnabled(
            boolean subscriptionProcessingEnabled) {
        this.subscriptionProcessingEnabled = subscriptionProcessingEnabled;
    }

    public NotificationServers getServers() {
        return servers;
    }

    public void setDataDeliveryRestClient(
            DataDeliveryRESTServices dataDeliveryRestClient) {
        this.dataDeliveryRestClient = dataDeliveryRestClient;
    }

    public void setRegistrySoapClient(RegistrySOAPServices registrySoapClient) {
        this.registrySoapClient = registrySoapClient;
    }

}
