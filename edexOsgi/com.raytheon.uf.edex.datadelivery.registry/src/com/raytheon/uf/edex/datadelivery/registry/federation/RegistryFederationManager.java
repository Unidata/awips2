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
package com.raytheon.uf.edex.datadelivery.registry.federation;

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
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.xml.bind.JAXBException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.Duration;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.ws.wsaddressing.W3CEndpointReference;
import javax.xml.ws.wsaddressing.W3CEndpointReferenceBuilder;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DeliveryInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
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
import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.DeliveryMethodTypes;
import com.raytheon.uf.common.registry.constants.NotificationOptionTypes;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.RunnableWithTransaction;
import com.raytheon.uf.edex.datadelivery.registry.availability.FederatedRegistryMonitor;
import com.raytheon.uf.edex.datadelivery.registry.replication.NotificationHostConfiguration;
import com.raytheon.uf.edex.datadelivery.registry.replication.NotificationServers;
import com.raytheon.uf.edex.datadelivery.registry.replication.RegistryRemoveTask;
import com.raytheon.uf.edex.datadelivery.registry.replication.RegistrySubmitTask;
import com.raytheon.uf.edex.datadelivery.registry.web.DataDeliveryRESTServices;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.exception.NoReplicationServersAvailableException;
import com.raytheon.uf.edex.registry.ebxml.init.RegistryInitializedListener;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * This class is responsible for managing this registry's connection and
 * communication with the rest of the registry federation. <br>
 * On startup, 2 configuration files are read:<br>
 * 1. notificationServers.xml - This file contains the registries that this
 * registry will be subscribing to<br>
 * 2. federationConfig.xml - This file contains details about this registry
 * <p>
 * When this class is initialized, if federation capabilities are enabled, it
 * will submit several objects up to the NCF registry in order to join the
 * federation. Then, it will attempt to submit subscriptions to the registries
 * specified in the notificationServers.xml file. Upon failure, attempts will be
 * made every 10 seconds until the subscriptions are succesfully submitted to
 * the remote registries.
 * <p>
 * If this registry has been down for 48 hours, a full registry sync will take
 * place with an adjacent registry. A monitor thread periodically logs that the
 * registry is up.
 * <p>
 * A monitor thread also checks every 10 minutes that the subscriptions
 * submitted to the remote registries still exist on those servers. If it is
 * found that the subscriptions have been deleted on the remote registry for
 * some reason, they will be resubmitted to that remote registry. This prevents
 * a registry from unknowingly being detached from the registry federation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/22/2013    1707        bphillip    Initial implementation
 * 7/29/2013    2191        bphillip    Implemented registry sync for registries that have been down for an extended period of time
 * 10/30/2013   1538        bphillip    Changed submitObjects method to submit objects to NCF by default
 * 11/20/2013   2534        bphillip    Consolidated RegistryReplicationManager into this class and added reciprocal subscriptions.  Added remote subscription monitor.
 * 12/2/2013    1829        bphillip    Modified to use correct getters for slot values
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Service
@Transactional
public class RegistryFederationManager implements RegistryInitializedListener {

    /** The logger instance */
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryFederationManager.class);

    /** The federation identifier */
    public static final String FEDERATION_ID = "Registry Federation";

    /**
     * The name of the configuration files defining which servers we are
     * sending/receiving replicated objects to/from
     */
    private static final String NOTIFICATION_SERVERS_FILE = "datadelivery/registry/notificationServers.xml";

    /**
     * The name of the configuration file defining the characteristics of the
     * federation
     */
    private static final String FEDERATION_CONFIG_FILE = "datadelivery/registry/federationConfig.xml";

    /**
     * The maximum time a registry can be down before a full synchronization is
     * performed
     */
    private static final long MAX_DOWN_TIME_DURATION = TimeUtil.MILLIS_PER_DAY
            * 2 - TimeUtil.MILLIS_PER_HOUR;

    /** The central registry mode string */
    private static final String CENTRAL_REGISTRY_MODE = "centralRegistry";

    /** String format for ids of replication subscriptions */
    private static final String SUBSCRIPTION_DETAIL_FORMAT = "Replication Subscription for [%s] objects for server ["
            + RegistryUtil.LOCAL_REGISTRY_ADDRESS + "]";

    /** The address of the NCF */
    protected String ncfAddress = System.getenv("NCF_ADDRESS");

    /** The mode that EDEX was started in */
    private String registryMode = System.getProperty("edex.run.mode");

    /**
     * When a federation sync is necessary, this is the number of threads that
     * will be used for synchronization. Configurable in the
     * com.raytheon.uf.edex.registry.ebxml.properties file. Default is 5
     */
    private int registrySyncThreads = 5;

    /** Maximum times this registry will try to sync data before failure */
    private int maxSyncRetries = 3;

    /** The servers that we are subscribing to */
    private static NotificationServers servers;

    /** Object types to automatically create subscriptions for */
    private static Set<String> objectTypes = new HashSet<String>();

    /**
     * Hibernate transaction template used for transaction isolation when
     * synchronizing with other registries
     */
    private TransactionTemplate txTemplate;

    /** Monitors how long this registry has been connected to the federation */
    private FederatedRegistryMonitor federatedRegistryMonitor;

    /**
     * The scheduler service used for registering this registry with the
     * federation
     */
    protected ScheduledExecutorService scheduler;

    /** Denotes if this registry is participating in the federation */
    protected boolean federationEnabled;

    /** The lifecycle manager */
    protected LifecycleManager lcm;

    /** The JAXB Manager for serializing registry objects */
    protected JAXBManager jaxbManager;

    /** The properties describing this registry in the federation */
    protected static FederationProperties federationProperties;

    /** Data access object for registry objects */
    protected RegistryObjectDao registryObjectDao;

    /** Data Access object for RegistryType objects */
    protected RegistryDao registryDao;

    /** Registry SOAP Service Client */
    protected RegistrySOAPServices registrySoapServices;

    /** Data Delivery REST services client */
    private DataDeliveryRESTServices dataDeliveryRestClient;

    /**
     * Denotes if initialization has already occurred for this class. It is a
     * static variable because at this time, multiple Spring containers load
     * this class, yet it only needs to be initialized once
     */
    private static AtomicBoolean initialized = new AtomicBoolean(false);

    /**
     * Creates a new RegistryFederationManager
     */
    protected RegistryFederationManager() {

    }

    /**
     * Creates a new RegistryFederationManager
     * 
     * @param federationEnabled
     *            Boolean denoting if the federation is enabled
     * @param lcm
     *            The lifecycle manager to be used
     * @param federationPropertiesFileName
     *            The name of the file containing the properties for this
     *            registry
     * @throws EbxmlRegistryException
     *             If errors occur during initialization
     */
    protected RegistryFederationManager(LifecycleManager lcm,
            FederatedRegistryMonitor federatedRegistryMonitor,
            TransactionTemplate txTemplate) throws EbxmlRegistryException {
        this.federationEnabled = Boolean.parseBoolean(System
                .getenv("EBXML_REGISTRY_FEDERATION_ENABLED"));
        this.federatedRegistryMonitor = federatedRegistryMonitor;
        this.lcm = lcm;
        this.txTemplate = txTemplate;

        try {
            jaxbManager = new JAXBManager(SubmitObjectsRequest.class,
                    FederationProperties.class, NotificationServers.class,
                    SubscriptionType.class);
        } catch (JAXBException e) {
            throw new EbxmlRegistryException(
                    "Error initializing JAXB Manager!", e);
        }

        if (System.getProperty("ebxml-federation-sync-threads") != null) {
            registrySyncThreads = Integer.valueOf(System
                    .getProperty("ebxml-federation-sync-threads"));
        }

        /*
         * Check if federation capability is enabled. If so, load the
         * properties.
         */
        if (federationEnabled) {
            if (federationProperties == null) {
                statusHandler.info("Loading Federation Configuration...");
                File federationPropertiesFile = PathManagerFactory
                        .getPathManager().getStaticFile(FEDERATION_CONFIG_FILE);
                if (federationPropertiesFile == null) {
                    throw new EbxmlRegistryException(
                            "Unable to locate federation configuration file: "
                                    + FEDERATION_CONFIG_FILE);
                } else {
                    try {
                        federationProperties = (FederationProperties) jaxbManager
                                .unmarshalFromXmlFile(
                                        FederationProperties.class,
                                        federationPropertiesFile);

                    } catch (SerializationException e) {
                        throw new EbxmlRegistryException(
                                "Error unmarshalling federation properties file",
                                e);
                    }
                }
            }

            if (servers == null) {
                File notificationServerConfigFile = PathManagerFactory
                        .getPathManager().getStaticFile(
                                NOTIFICATION_SERVERS_FILE);

                // If this registry is participating in the federation and
                // the
                // config
                // file is not found, throw an error
                if (federationEnabled && notificationServerConfigFile == null) {
                    throw new EbxmlRegistryException(
                            "Notification server config file not found!");
                }

                try {
                    statusHandler.info("Loading replication servers from ["
                            + notificationServerConfigFile.getPath() + "]...");
                    servers = jaxbManager.unmarshalFromXmlFile(
                            NotificationServers.class,
                            notificationServerConfigFile);
                } catch (SerializationException e) {
                    throw new EbxmlRegistryException(
                            "Error unmarshalling notification servers file!", e);
                }
                statusHandler.info("Found "
                        + servers.getRegistryReplicationServers().size()
                        + " servers for replication");
            }

            scheduler = Executors.newScheduledThreadPool(1);
        } else {
            statusHandler.info("Federation capabilities disabled");
        }

    }

    @Override
    public void executeAfterRegistryInit() throws EbxmlRegistryException {

        if (federationEnabled && !initialized.getAndSet(true)) {
            statusHandler
                    .info("Federation/Replication enabled for this registry. Scheduling Federation registration task...");
            final RegisterWithFederationTask federationRegistrationTask = new RegisterWithFederationTask();
            scheduler.schedule(federationRegistrationTask, 0, TimeUnit.SECONDS);
        }
    }

    /**
     * Gets the federation object for this federation
     * 
     * @return The federation object
     * @throws EbxmlRegistryException
     *             If errors occur getting the federation
     * @throws JAXBException
     * @throws RegistryServiceException
     */
    private FederationType getFederation() throws EbxmlRegistryException,
            RegistryServiceException, JAXBException {

        FederationType federation = null;
        if (isCentralRegistry()) {
            statusHandler.info("Creating Federation object...");
            federation = new FederationType();
            federation.setId(FEDERATION_ID);
            federation.setLid(FEDERATION_ID);
            federation.setName(RegistryUtil
                    .getInternationalString(FEDERATION_ID));
            federation.setDescription(RegistryUtil
                    .getInternationalString(FEDERATION_ID));
            federation.setOwner(RegistryUtil.DEFAULT_OWNER);
            federation.setStatus(StatusTypes.APPROVED);
            federation.setObjectType(RegistryObjectTypes.FEDERATION);
            federation
                    .setReplicationSyncLatency(RegistryFederationManager.federationProperties
                            .getFederationReplicationSyncLatency());
        } else {
            statusHandler
                    .info("Retrieving Federation object from NCF registry at ["
                            + ncfAddress + "]...");
            federation = dataDeliveryRestClient.getRegistryObject(ncfAddress,
                    FEDERATION_ID);
            statusHandler
                    .info("Federation object successfully retrieved from NCF!");

        }

        return federation;

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
            if (!objectTypes.contains(type)) {
                objectTypes.add(type);
                statusHandler.info("Add object type for replication [" + type
                        + "]");
            }
        }
    }

    /**
     * Creates the association object between this registry and the federation
     * 
     * @param registry
     *            The registry joining the federation
     * @param federation
     *            The federation the registry is joining
     * @return The association object
     */
    protected AssociationType getFederationAssociation(RegistryType registry,
            FederationType federation) {
        AssociationType association = new AssociationType();
        association.setId(registry.getId()
                + " Federation Membership Association");
        association.setLid(association.getId());
        association.setObjectType(RegistryObjectTypes.ASSOCIATION);
        association.setOwner(federationProperties.getSiteIdentifier());
        association.setType(AssociationTypes.HAS_FEDERATION_MEMBER);
        association.setStatus(StatusTypes.APPROVED);
        association.setName(RegistryUtil.getInternationalString(registry
                .getId() + " Federation Membership"));
        association.setDescription(association.getName());
        association.setTargetObject(registry.getId());
        association.setSourceObject(federation.getId());
        return association;
    }

    /**
     * Submits objects necessary for the registry/federation to operate properly
     * to the registry. This method first submits it locally, then submits the
     * objects to the NCF
     * 
     * @param objects
     *            The objects to submit
     * @throws EbxmlRegistryException
     *             If object submission fails
     */
    protected void submitObjects(List<RegistryObjectType> objects)
            throws EbxmlRegistryException {
        SubmitObjectsRequest submitObjectsRequest = new SubmitObjectsRequest(
                "Federation Objects submission",
                "Submitting federation related objects", null,
                new RegistryObjectListType(objects), false,
                Mode.CREATE_OR_REPLACE);
        try {
            statusHandler
                    .info("Submitting federation registration objects to local registry...");
            lcm.submitObjects(submitObjectsRequest);
            statusHandler
                    .info("Successfully submitted federation registration objects to local registry!");
        } catch (MsgRegistryException e) {
            throw new EbxmlRegistryException(
                    "Error submitting federation objects to registry", e);
        }

        if (!isCentralRegistry()) {
            statusHandler
                    .info("Submitting federation registration objects to NCF...");
            try {
                registrySoapServices.getLifecycleManagerServiceForHost(
                        ncfAddress).submitObjects(submitObjectsRequest);
                statusHandler
                        .info("Successfully submitted federation registration objects to NCF!");
            } catch (MsgRegistryException e) {
                throw new EbxmlRegistryException(
                        "Error submitting federation objects to registry", e);
            }
        }
    }

    /**
     * Removes a notificationServer based on the URL.
     * 
     * @param baseURL
     *            The URL of the server to be removed
     */
    public void removeNotificationServer(String baseURL) {
        statusHandler.info("Removing replication registry for URL [" + baseURL
                + "]");
        NotificationHostConfiguration toRemove = null;
        for (NotificationHostConfiguration hostConfig : servers
                .getRegistryReplicationServers()) {
            if (hostConfig.getRegistryBaseURL().equals(baseURL)) {
                toRemove = hostConfig;
            }
        }
        if (toRemove == null) {
            statusHandler.warn("Replication registry at URL [" + baseURL
                    + "] not present in configration");
        } else {
            servers.getRegistryReplicationServers().remove(toRemove);
        }

    }

    /**
     * Adds a notification server to the list.
     * 
     * @param host
     *            The host to be added
     */
    public void addNotificationServer(RegistryType registry) {
        statusHandler.info("Adding registry [" + registry.getId()
                + "] to list of registries for replication");
        if (!notificationServersContainRegistry(registry)) {
            servers.getRegistryReplicationServers().add(
                    new NotificationHostConfiguration(registry.getId(),
                            registry.getId(), registry.getBaseURL()));
            statusHandler
                    .info("Registry ["
                            + registry.getId()
                            + "] successfully added to list of registries for replication");
        }

    }

    public boolean notificationServersContainRegistry(RegistryType registry) {
        for (NotificationHostConfiguration hostConfig : servers
                .getRegistryReplicationServers()) {
            if (hostConfig.getRegistryBaseURL().equals(registry.getBaseURL())) {
                return true;
            }
        }

        return false;
    }

    /**
     * Persists the list of notification servers to the localized file
     */
    public void saveNotificationServers() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.EDEX_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile lf = pm.getLocalizationFile(lc,
                NOTIFICATION_SERVERS_FILE);
        File file = lf.getFile();

        try {
            jaxbManager.marshalToXmlFile(servers, file.getAbsolutePath());

            lf.save();

        } catch (SerializationException e) {
            statusHandler.error("Unable to update replication server file!", e);
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Submits subscriptions to the remote registries
     * 
     * @param baseURL
     *            The url of the registry to send the subscriptions to
     */
    public void submitRemoteSubscriptions() {

        statusHandler
                .info("Submitting subscriptions to registry replication servers...");
        List<NotificationHostConfiguration> replicationRegistries = servers
                .getRegistryReplicationServers();

        if (CollectionUtil.isNullOrEmpty(replicationRegistries)) {
            statusHandler.info("No registry replication servers configured.");
        } else {
            for (NotificationHostConfiguration config : replicationRegistries) {
                statusHandler
                        .info("Scheduling subscription submission to registry at ["
                                + config.getRegistryBaseURL() + "]...");
                final SubmitSubscriptionTask submitSubscriptionTask = new SubmitSubscriptionTask(
                        config);
                scheduler.schedule(submitSubscriptionTask, 0, TimeUnit.SECONDS);
                statusHandler
                        .info("Successfully scheduled subscription submission to registry at ["
                                + config.getRegistryBaseURL() + "]");
            }
        }
    }

    public void submitSubscriptionsToRegistry(RegistryType registry) {
        statusHandler.info("Submitting subscriptions to registry ["
                + registry.getId() + "]...");

        NotificationHostConfiguration config = new NotificationHostConfiguration(
                registry.getId(), registry.getId(), registry.getBaseURL());
        SubmitSubscriptionTask task = new SubmitSubscriptionTask(config);
        task.run();
        if (task.scheduleSubscriptions()) {
            statusHandler
                    .info("Successfully submitted subscriptions to registry ["
                            + registry.getId() + "]");
        } else {
            statusHandler.warn("Unable to submit subscriptions to registry ["
                    + registry.getId() + "]");
        }
    }

    public boolean isSubscribedTo(RegistryType registry) {
        for (NotificationHostConfiguration config : servers
                .getRegistryReplicationServers()) {
            if (config.getRegistryBaseURL().equals(registry.getBaseURL())) {
                return true;
            }
        }
        return false;
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
            boolean reciprocateSubscription) throws Exception {
        // Set normal registry object fields
        String subscriptionDetail = String.format(SUBSCRIPTION_DETAIL_FORMAT,
                objectType);
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
        sub.setOwner(federationProperties.getSiteIdentifier());
        sub.setStatus(StatusTypes.APPROVED);

        sub.setStartTime(EbxmlObjectUtil.getTimeAsXMLGregorianCalendar(0));
        QueryType selectorQuery = new QueryType();
        selectorQuery.setQueryDefinition(CanonicalQueryTypes.ADHOC_QUERY);

        SlotType expressionSlot = new SlotType();
        StringValueType expressionValue = new StringValueType();
        expressionValue
                .setStringValue("FROM RegistryObjectType obj where obj.objectType='"
                        + objectType + "'");
        expressionSlot.setName(QueryConstants.QUERY_EXPRESSION);
        expressionSlot.setSlotValue(expressionValue);
        selectorQuery.getSlot().add(expressionSlot);

        SlotType languageSlot = new SlotType();
        StringValueType languageValue = new StringValueType();
        languageValue.setStringValue(QueryLanguages.HQL);
        languageSlot.setName(QueryConstants.QUERY_LANGUAGE);
        languageSlot.setSlotValue(languageValue);
        selectorQuery.getSlot().add(languageSlot);

        sub.setSelector(selectorQuery);

        Duration notificationInterval = DatatypeFactory.newInstance()
                .newDuration(0);
        sub.setNotificationInterval(notificationInterval);

        String endpointType = DeliveryMethodTypes.SOAP;
        W3CEndpointReferenceBuilder builder = new W3CEndpointReferenceBuilder();
        builder.address(registrySoapServices
                .getNotificationListenerServiceUrl(RegistryUtil.LOCAL_REGISTRY_ADDRESS));
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
     * Checks how long a registry has been down. If the registry has been down
     * for over 2 days, the registry is synchronized with one of the federation
     * members
     * 
     * @throws Exception
     */
    private void checkDownTime() throws Exception {
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
                        break;
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
            Set<String> localIds = null;
            Set<String> remoteIds = null;
            statusHandler
                    .info("Getting registry object Ids from local registry...");
            Collection<String> response = dataDeliveryRestClient
                    .getRegistryDataAccessService(
                            RegistryUtil.LOCAL_REGISTRY_ADDRESS)
                    .getRegistryObjectIdsOfType(objectType).getPayload();
            if (response != null) {
                localIds = new HashSet<String>(response.size(), 1);
                localIds.addAll(response);
            } else {
                localIds = new HashSet<String>();
            }

            statusHandler.info(localIds.size() + " objects of type "
                    + objectType + " present in local registry.");
            statusHandler.info("Getting registry object Ids from "
                    + remoteRegistryUrl + "...");
            response = dataDeliveryRestClient
                    .getRegistryDataAccessService(remoteRegistryUrl)
                    .getRegistryObjectIdsOfType(objectType).getPayload();
            if (response != null) {
                remoteIds = new HashSet<String>(response.size(), 1);
                remoteIds.addAll(response);
            } else {
                remoteIds = new HashSet<String>();
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
                    executor.submit(new RegistrySubmitTask(txTemplate,
                            registryObjectDao, localId, remoteRegistryUrl,
                            dataDeliveryRestClient));
                } else {
                    RegistryRemoveTask removeTask = new RegistryRemoveTask(
                            txTemplate, registryObjectDao, localId);
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
                    executor.submit(new RegistrySubmitTask(txTemplate,
                            registryObjectDao, remoteId, remoteRegistryUrl,
                            dataDeliveryRestClient));
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

        /** The server configuration */
        private NotificationHostConfiguration config;

        /**
         * Creates a new SubmitSubscriptionTask
         * 
         * @param config
         *            The server configuration
         */
        public SubmitSubscriptionTask(NotificationHostConfiguration config) {
            this.config = config;
        }

        @Override
        public void run() {

            if (scheduleSubscriptions()) {
                statusHandler.info("Successfully submitted subscriptions to "
                        + config.getRegistryBaseURL());
            } else {
                statusHandler
                        .warn("Subscriptions not submitted to registry at "
                                + config.getRegistryBaseURL()
                                + ". Retrying in 10 seconds...");
                scheduler.schedule(this, 10, TimeUnit.SECONDS);
            }

        }

        public boolean scheduleSubscriptions() {
            try {
                final String remoteRegistryBaseURL = config
                        .getRegistryBaseURL();
                RegistryType remoteRegistry = getRemoteRegistryByURL(remoteRegistryBaseURL);

                if (remoteRegistry == null) {
                    statusHandler.error("Registry at URL ["
                            + remoteRegistryBaseURL
                            + "] not found in federation.");
                    return false;
                } else {

                    statusHandler.info("Checking if remote registry at ["
                            + remoteRegistryBaseURL + "] is available...");

                    if (dataDeliveryRestClient
                            .isRegistryAvailable(remoteRegistryBaseURL)) {
                        statusHandler.info("Registry at ["
                                + remoteRegistryBaseURL + "] is available!");
                    } else {
                        statusHandler
                                .error("Registry at [" + remoteRegistryBaseURL
                                        + "] is not available.");
                        return false;
                    }

                    statusHandler
                            .info("Removing remote subscriptions prior to submission of new subscriptions");
                    dataDeliveryRestClient.getRegistryDataAccessService(
                            remoteRegistryBaseURL).removeSubscriptionsForSite(
                            federationProperties.getSiteIdentifier());
                    statusHandler
                            .info("Generating registry replication subscriptions for registry at ["
                                    + config.getRegistrySiteName()
                                    + "] at URL ["
                                    + remoteRegistryBaseURL
                                    + "]");

                    List<RegistryObjectType> subscriptions = new ArrayList<RegistryObjectType>(
                            objectTypes.size());
                    for (String objectType : objectTypes) {
                        SubscriptionType subscription;
                        try {
                            subscription = createSubscription(
                                    remoteRegistryBaseURL, objectType,
                                    config.isReciprocate());
                        } catch (Exception e) {
                            throw new RegistryException(
                                    "Error creating subscription", e);
                        }
                        subscriptions.add(subscription);
                    }

                    SubmitObjectsRequest request = new SubmitObjectsRequest(
                            "Subscription Submission",
                            "Subscription Submission", null,
                            new RegistryObjectListType(subscriptions), false,
                            Mode.CREATE_OR_REPLACE);
                    registrySoapServices.sendSubmitObjectsRequest(request,
                            remoteRegistryBaseURL);
                    statusHandler.info("Requesting subscriptions from ["
                            + remoteRegistryBaseURL + "]...");

                    if (dataDeliveryRestClient
                            .getFederationService(remoteRegistryBaseURL)
                            .isFederated().equalsIgnoreCase("true")) {
                        if (config.isReciprocate()) {
                            dataDeliveryRestClient.getFederationService(
                                    remoteRegistryBaseURL).subscribeToRegistry(
                                    federationProperties.createRegistryObject()
                                            .getId());
                        }
                    } else {
                        statusHandler
                                .warn("Registry at ["
                                        + remoteRegistryBaseURL
                                        + "] is not participating in the federation. Unable to request subscriptions");
                    }

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
                                            federationProperties
                                                    .getSiteIdentifier());
                            statusHandler.info("Subscriptions removed from: ["
                                    + remoteRegistryBaseURL + "]");
                        }
                    });
                    return true;
                }
            } catch (Exception e) {
                statusHandler.error("Error submitting subscriptions!.", e);
                return false;

            }
        }
    }

    private RegistryType getRemoteRegistryByURL(String remoteRegistryBaseURL)
            throws MsgRegistryException {
        QueryManager ncfQueryManager = registrySoapServices
                .getQueryServiceForHost(ncfAddress);
        QueryType query = new QueryType(CanonicalQueryTypes.ADHOC_QUERY);
        query.getSlot().add(
                new SlotType(QueryConstants.QUERY_LANGUAGE,
                        new StringValueType(QueryLanguages.HQL)));
        query.getSlot()
                .add(new SlotType(QueryConstants.QUERY_EXPRESSION,
                        new StringValueType(
                                "FROM RegistryType reg WHERE reg.baseURL=:url")));
        query.getSlot()
                .add(new SlotType("url", new StringValueType(
                        remoteRegistryBaseURL)));

        QueryRequest queryRequest = new QueryRequest("Registry Retrieval",
                query, new ResponseOptionType(QueryReturnTypes.REGISTRY_OBJECT,
                        true));
        List<RegistryObjectType> registries = ncfQueryManager.executeQuery(
                queryRequest).getRegistryObjects();
        for (RegistryObjectType registryObj : registries) {
            RegistryType reg = (RegistryType) registryObj;
            if (reg.getBaseURL().equals(remoteRegistryBaseURL)) {
                return reg;
            }
        }
        return null;
    }

    /**
     * 
     * Runnable task that continuously attempts to register this registry with
     * the federation until it succeeds
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#     Engineer    Description
     * ------------ ----------  ----------- --------------------------
     * 5/22/2013    1707        bphillip    Initial implementation
     * </pre>
     * 
     * @author bphillip
     * @version 1
     */
    private class RegisterWithFederationTask extends RunnableWithTransaction {

        /**
         * Creates a new RegisterwithFederationTask
         */
        public RegisterWithFederationTask() {
            super(txTemplate);

        }

        @Override
        public void runWithTransaction() {

            boolean joinFederationSuccess = joinFederation();

            if (joinFederationSuccess) {
                statusHandler.info("Federation registration successful.");
                submitRemoteSubscriptions();

                statusHandler.info("Starting registry subscription monitor...");

                Runnable subscriptionMonitor = new Runnable() {
                    @Override
                    public void run() {
                        List<NotificationHostConfiguration> registries = null;
                        registries = servers.getRegistryReplicationServers();
                        if (registries.isEmpty()) {
                            statusHandler
                                    .info("Skipping replication subscription verification.  No remote registries currently configured for replication.");
                            return;
                        } else {
                            statusHandler
                                    .info("Verifying replication subscriptions on "
                                            + registries.size()
                                            + " remote registries");
                        }
                        for (NotificationHostConfiguration config : registries) {
                            String remoteRegistryUrl = config
                                    .getRegistryBaseURL();
                            statusHandler
                                    .info("Verifying replication subscriptions at ["
                                            + remoteRegistryUrl + "]...");
                            if (!dataDeliveryRestClient
                                    .isRegistryAvailable(config
                                            .getRegistryBaseURL())) {
                                statusHandler
                                        .warn("Registry at ["
                                                + remoteRegistryUrl
                                                + "] is unavailable. Unable to verify subscriptions");
                                return;
                            }
                            int resubmissions = 0;
                            QueryManager remoteQueryManager = registrySoapServices
                                    .getQueryServiceForHost(remoteRegistryUrl);
                            for (String objectType : objectTypes) {
                                String subId = String.format(
                                        SUBSCRIPTION_DETAIL_FORMAT, objectType);
                                SubscriptionType subscription = null;
                                try {
                                    QueryType query = new QueryType(
                                            CanonicalQueryTypes.GET_OBJECT_BY_ID);
                                    query.getSlot()
                                            .add(new SlotType(
                                                    QueryConstants.ID,
                                                    new StringValueType(subId)));
                                    QueryRequest queryRequest = new QueryRequest(
                                            "Subscription verification ["
                                                    + subId + "]",
                                            query,
                                            new ResponseOptionType(
                                                    QueryReturnTypes.OBJECT_REF,
                                                    true));
                                    List<ObjectRefType> queryResult = remoteQueryManager
                                            .executeQuery(queryRequest)
                                            .getObjectRefs();
                                    boolean found = false;
                                    for (ObjectRefType ref : queryResult) {
                                        if (ref.getId().equals(subId)) {
                                            found = true;
                                            break;
                                        }
                                    }
                                    if (!found) {
                                        statusHandler
                                                .warn("Subscription ["
                                                        + subId
                                                        + "] not found on remote server ["
                                                        + remoteRegistryUrl
                                                        + "]. Resubmitting subscription...");
                                        try {
                                            subscription = createSubscription(
                                                    remoteRegistryUrl,
                                                    objectType,
                                                    config.isReciprocate());
                                            SubmitObjectsRequest request = new SubmitObjectsRequest(
                                                    "Resubmission of subscription ["
                                                            + subId + "]",
                                                    "Resubmission of subscription ["
                                                            + subId + "]",
                                                    null,
                                                    new RegistryObjectListType(
                                                            subscription),
                                                    false,
                                                    Mode.CREATE_OR_REPLACE);
                                            registrySoapServices
                                                    .sendSubmitObjectsRequest(
                                                            request,
                                                            remoteRegistryUrl);
                                            resubmissions++;

                                        } catch (Exception e1) {
                                            statusHandler
                                                    .error("Error creating subscription for resubmission!",
                                                            e1);
                                        }
                                    }
                                } catch (MsgRegistryException e) {
                                    statusHandler.error(
                                            "Error verifying subscription!", e);
                                }
                            }
                            if (resubmissions == 0) {
                                statusHandler
                                        .info("Successfully verified replication subscriptions for registry at ["
                                                + remoteRegistryUrl + "]");
                            } else {
                                statusHandler.warn("Resubmitted "
                                        + resubmissions
                                        + " subscriptions to registry at ["
                                        + remoteRegistryUrl + "].");
                            }

                        }
                    }
                };
                scheduler.scheduleAtFixedRate(subscriptionMonitor, 5, 10,
                        TimeUnit.MINUTES);
                statusHandler.info("Registry subscription monitor started.");
            } else {
                statusHandler
                        .warn("Unable to join federation. Retrying in 10 seconds...");
                scheduler.schedule(this, 10, TimeUnit.SECONDS);
            }
        }

        private boolean joinFederation() {
            try {
                try {
                    if (!isCentralRegistry()) {
                        if (dataDeliveryRestClient
                                .isRegistryAvailable(ncfAddress)) {
                            statusHandler
                                    .info("NCF Registry is available. Attempting to join federation...");
                        } else {
                            statusHandler
                                    .error("Unable to join federation. NCF is currently unreachable.");
                            return false;
                        }
                    }
                    List<RegistryObjectType> objects = new ArrayList<RegistryObjectType>(
                            5);
                    RegistryType registry = federationProperties
                            .createRegistryObject();
                    OrganizationType org = federationProperties
                            .createOrganization();
                    PersonType primaryContact = federationProperties
                            .createPrimaryContactPerson();
                    FederationType federation = getFederation();
                    AssociationType federationAssociation = null;
                    if (federation == null) {
                        statusHandler
                                .error("Unable to join federation.  Federation Object not found.");
                    } else {
                        federationAssociation = getFederationAssociation(
                                registry, federation);
                    }
                    if (isCentralRegistry()) {
                        objects.add(federation);
                    }
                    objects.add(registry);
                    objects.add(org);
                    objects.add(primaryContact);
                    objects.add(federationAssociation);
                    submitObjects(objects);
                    try {
                        checkDownTime();
                    } catch (NoReplicationServersAvailableException e) {
                        statusHandler
                                .warn("No replication servers have been specified!");
                    }
                    return true;
                } catch (EbxmlRegistryException e) {
                    statusHandler.error("Error registering with federation", e);
                    return false;
                }
            } catch (Throwable e) {
                statusHandler.error("Error initializing EBXML database!", e);
                return false;
            }
        }
    }

    private boolean isCentralRegistry() {
        return registryMode.equals(CENTRAL_REGISTRY_MODE);
    }

    public static Set<String> getObjectTypes() {
        return objectTypes;
    }

    public NotificationServers getServers() {
        return servers;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setRegistryDao(RegistryDao registryDao) {
        this.registryDao = registryDao;
    }

    public void setRegistrySoapServices(
            RegistrySOAPServices registrySoapServices) {
        this.registrySoapServices = registrySoapServices;
    }

    public void setDataDeliveryRestClient(
            DataDeliveryRESTServices dataDeliveryRestClient) {
        this.dataDeliveryRestClient = dataDeliveryRestClient;
    }
}
