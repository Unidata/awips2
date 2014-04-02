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
import java.io.FileNotFoundException;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
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

import org.springframework.stereotype.Service;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;
import org.springframework.transaction.support.TransactionTemplate;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.datadelivery.registry.web.IRegistryFederationManager;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.DeletionScope;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.common.registry.services.rest.response.RestCollectionResponse;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.datadelivery.registry.availability.FederatedRegistryMonitor;
import com.raytheon.uf.edex.datadelivery.registry.dao.ReplicationEventDao;
import com.raytheon.uf.edex.datadelivery.registry.replication.NotificationServers;
import com.raytheon.uf.edex.datadelivery.registry.web.DataDeliveryRESTServices;
import com.raytheon.uf.edex.datadelivery.util.DataDeliveryIdUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.DbInit;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.exception.NoReplicationServersAvailableException;
import com.raytheon.uf.edex.registry.ebxml.init.RegistryInitializedListener;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.RegistryQueryUtil;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;
import com.raytheon.uf.edex.registry.events.CreateAuditTrailEvent;

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
 * 12/9/2013    2613        bphillip    Optimized registry sync function
 * 1/15/2014    2613        bphillip    Added leaveFederation method to prevent inactive registries from participating in the federation unintentionally.
 * 1/21/2014    2613        bphillip    Changed max down time which requires a sync
 * Feb 11, 2014 2771        bgonzale    Use Data Delivery ID instead of Site.
 * 2/13/2014    2769        bphillip    Refactored registry sync. Created quartz tasks to monitor registry uptime as well as subscription integrity
 * Mar 31, 2014 2889        dhladky     Added username for notification center tracking.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Path(IRegistryFederationManager.REGISTRY_FEDERATION_MANAGER_PATH)
@Service
public class RegistryFederationManager implements IRegistryFederationManager,
        RegistryInitializedListener {

    /** The logger instance */
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryFederationManager.class);

    /** Query used for synchronizing registries */
    private static final String SYNC_QUERY = "FROM RegistryObjectType obj where obj.id in (%s) order by obj.id asc";

    /** Batch size for registry synchronization queries */
    private static final int SYNC_BATCH_SIZE = Integer.parseInt(System
            .getProperty("ebxml-notification-batch-size"));

    private static Set<String> replicatedObjectTypes = new HashSet<String>();

    public static final String FEDERATION_ID = "Registry Federation";

    /** The mode that EDEX was started in */
    public static final boolean centralRegistry = System.getProperty(
            "edex.run.mode").equals("centralRegistry");

    private static final String FEDERATION_CONFIG_FILE = "datadelivery/registry/federationConfig.xml";

    /**
     * The name of the configuration files defining which servers we are
     * sending/receiving replicated objects to/from
     */
    private static final String NOTIFICATION_SERVERS_FILE = "datadelivery/registry/notificationServers.xml";

    private static FederationProperties federationProperties;

    /**
     * The maximum time a registry can be down before a full synchronization is
     * performed
     */
    private static final long MAX_DOWN_TIME_DURATION = TimeUtil.MILLIS_PER_HOUR * 6;

    /** Cutoff parameter for the query to get the expired events */
    private static final String GET_EXPIRED_EVENTS_QUERY_CUTOFF_PARAMETER = "cutoff";

    /** Query to get Expired AuditableEvents */
    private static final String GET_EXPIRED_EVENTS_QUERY = "FROM ReplicationEvent event where event.eventTime < :"
            + GET_EXPIRED_EVENTS_QUERY_CUTOFF_PARAMETER;

    /** Maximum times this registry will try to sync data before failure */
    private int maxSyncRetries = 3;

    /**
     * Denotes if initialization has already occurred for this class. It is a
     * static variable because at this time, multiple Spring containers load
     * this class, yet it only needs to be initialized once
     */
    public static AtomicBoolean initialized = new AtomicBoolean(false);

    private boolean federationEnabled = Boolean.parseBoolean(System
            .getenv("EBXML_REGISTRY_FEDERATION_ENABLED"));

    private static AtomicBoolean running = new AtomicBoolean(false);

    /** The servers that we are subscribing to */
    private static NotificationServers servers;

    private String ncfAddress = System.getenv("NCF_ADDRESS");

    /** Monitors how long this registry has been connected to the federation */
    private FederatedRegistryMonitor federatedRegistryMonitor;

    /** The JAXB Manager for serializing registry objects */
    protected JAXBManager jaxbManager;

    private ReplicationEventDao replicationEventDao;

    private RegistryDao registryDao;

    private TransactionTemplate txTemplate;

    private RegistrySOAPServices soapService;

    private LifecycleManager localLifecycleManager;

    private DataDeliveryRESTServices dataDeliveryRestClient;

    private RegistryObjectDao registryObjectDao;

    private FederationDbInit federationDbInit;

    public RegistryFederationManager() throws JAXBException {
        jaxbManager = new JAXBManager(SubmitObjectsRequest.class,
                FederationProperties.class, NotificationServers.class,
                SubscriptionType.class);
    }

    @Override
    @Transactional
    public void executeAfterRegistryInit() throws EbxmlRegistryException {
        try {
            this.federationDbInit.initDb();
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error initializing database for federation!", e);
        }
        if (federationEnabled && !initialized.get()) {
            try {
                if (federationProperties == null) {
                    statusHandler.info("Loading Federation Configuration...");
                    File federationPropertiesFile = PathManagerFactory
                            .getPathManager().getStaticFile(
                                    FEDERATION_CONFIG_FILE);
                    if (federationPropertiesFile == null) {
                        throw new FileNotFoundException(
                                "Unable to locate federation configuration file: "
                                        + FEDERATION_CONFIG_FILE);
                    } else {
                        federationProperties = (FederationProperties) jaxbManager
                                .unmarshalFromXmlFile(
                                        FederationProperties.class,
                                        federationPropertiesFile);
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
                    if (federationEnabled
                            && notificationServerConfigFile == null) {
                        throw new EbxmlRegistryException(
                                "Notification server config file not found!");
                    }

                    try {
                        statusHandler.info("Loading replication servers from ["
                                + notificationServerConfigFile.getPath()
                                + "]...");
                        servers = jaxbManager.unmarshalFromXmlFile(
                                NotificationServers.class,
                                notificationServerConfigFile);
                    } catch (SerializationException e) {
                        throw new EbxmlRegistryException(
                                "Error unmarshalling notification servers file!",
                                e);
                    }
                    statusHandler.info("Found "
                            + servers.getRegistryReplicationServers().size()
                            + " servers for replication");
                }

                if (!joinFederation()) {
                    throw new EbxmlRegistryException(
                            "Error joining federation!!");
                }
                if (!centralRegistry) {
                    checkDownTime();
                }
                federatedRegistryMonitor.updateTime();

            } catch (Exception e1) {
                throw new EbxmlRegistryException(
                        "Error initializing RegistryReplicationManager", e1);
            }

            Runtime.getRuntime().addShutdownHook(new Thread() {
                public void run() {
                    txTemplate.execute(new TransactionCallbackWithoutResult() {

                        @Override
                        protected void doInTransactionWithoutResult(
                                TransactionStatus status) {
                            leaveFederation();
                        }
                    });
                }
            });
        }
        initialized.set(true);
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
                            .warn("Registry has been down for more than "
                                    + (MAX_DOWN_TIME_DURATION / TimeUtil.MILLIS_PER_HOUR)
                                    + " hours. Initiating federated registry data synchronization attempt #"
                                    + syncAttempt + "/" + maxSyncRetries
                                    + "...");
                    if (CollectionUtil.isNullOrEmpty(servers
                            .getRegistryReplicationServers())) {
                        statusHandler
                                .error("No servers configured for replication. Unable to synchronize registry data with federation!");
                        break;
                    } else {
                        RegistryType registryToSyncFrom = null;
                        for (String remoteRegistryId : servers
                                .getRegistryReplicationServers()) {
                            statusHandler.info("Checking availability of ["
                                    + remoteRegistryId + "]...");
                            RegistryType remoteRegistry = dataDeliveryRestClient
                                    .getRegistryObject(
                                            ncfAddress,
                                            remoteRegistryId
                                                    + FederationProperties.REGISTRY_SUFFIX);
                            if (remoteRegistry == null) {
                                statusHandler
                                        .warn("Registry at ["
                                                + remoteRegistryId
                                                + "] not found in federation. Unable to use as synchronization source.");
                            } else if (dataDeliveryRestClient
                                    .isRegistryAvailable(remoteRegistry
                                            .getBaseURL())) {
                                registryToSyncFrom = remoteRegistry;
                                break;
                            } else {
                                statusHandler
                                        .info("Registry at ["
                                                + remoteRegistryId
                                                + "] is not available.  Unable to use as synchronization source.");
                            }
                        }

                        // No available registry was found!
                        if (registryToSyncFrom == null) {
                            throw new NoReplicationServersAvailableException(
                                    "No available registries found! Registry data will not be synchronized with the federation!");
                        } else {
                            synchronizeWithRegistry(registryToSyncFrom.getId());

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
    }

    public boolean joinFederation() {
        try {
            final List<RegistryObjectType> objects = new ArrayList<RegistryObjectType>(
                    5);
            final RegistryType registry = federationProperties
                    .createRegistryObject();
            final OrganizationType org = federationProperties
                    .createOrganization();
            final PersonType primaryContact = federationProperties
                    .createPrimaryContactPerson();
            FederationType federation = null;
            try {
                federation = getFederation();
            } catch (Exception e) {
                throw new EbxmlRegistryException("Error getting federation", e);
            }

            if (federation != null) {
                final AssociationType federationAssociation = federationProperties
                        .getFederationAssociation(registry, federation);

                if (centralRegistry) {
                    objects.add(federation);
                }
                objects.add(registry);
                objects.add(org);
                objects.add(primaryContact);
                objects.add(federationAssociation);

                SubmitObjectsRequest submitObjectsRequest = new SubmitObjectsRequest(
                        "Federation Objects submission",
                        "Submitting federation related objects", null,
                        new RegistryObjectListType(objects), false,
                        Mode.CREATE_OR_REPLACE);
                submitObjectsRequest
                        .getSlot()
                        .add(new SlotType(EbxmlObjectUtil.EVENT_SOURCE_SLOT,
                                new StringValueType(DataDeliveryIdUtil.getId())));
                try {
                    statusHandler
                            .info("Submitting federation registration objects to local registry...");
                    localLifecycleManager.submitObjects(submitObjectsRequest);
                    statusHandler
                            .info("Successfully submitted federation registration objects to local registry!");
                } catch (MsgRegistryException e) {
                    throw new EbxmlRegistryException(
                            "Error submitting federation objects to registry",
                            e);
                }
                if (!centralRegistry) {
                    statusHandler
                            .info("Submitting federation registration objects to NCF...");
                    try {
                        soapService.getLifecycleManagerServiceForHost(
                                ncfAddress).submitObjects(submitObjectsRequest);
                        statusHandler
                                .info("Successfully submitted federation registration objects to NCF!");
                    } catch (MsgRegistryException e) {
                        throw new EbxmlRegistryException(
                                "Error submitting federation objects to registry",
                                e);
                    }
                }

                for (String registryId : servers
                        .getRegistryReplicationServers()) {
                    try {
                        subscribeToRegistry(registryId);
                    } catch (Exception e) {
                        statusHandler
                                .error("Error establishing replication connection with ["
                                        + registryId + "]");
                    }
                }
            }
        } catch (Throwable e) {
            statusHandler.error("Error joining federation!", e);
            return false;
        }
        return true;

    }

    public boolean leaveFederation() {
        try {
            final RegistryType registry = federationProperties
                    .createRegistryObject();
            final OrganizationType org = federationProperties
                    .createOrganization();
            final PersonType primaryContact = federationProperties
                    .createPrimaryContactPerson();
            FederationType federation = getFederation();

            if (federation != null) {
                final AssociationType federationAssociation = federationProperties
                        .getFederationAssociation(registry, federation);
                ObjectRefListType refList = new ObjectRefListType();
                refList.getObjectRef().add(new ObjectRefType(registry.getId()));
                refList.getObjectRef().add(new ObjectRefType(org.getId()));
                refList.getObjectRef().add(
                        new ObjectRefType(primaryContact.getId()));
                refList.getObjectRef().add(
                        new ObjectRefType(federationAssociation.getId()));
                RemoveObjectsRequest req = new RemoveObjectsRequest();
                req.setId("Removing [" + registry.getId()
                        + "] from the federation...");
                req.setComment("Remove request to remove federation related objects");
                req.setDeleteChildren(true);
                req.setObjectRefList(refList);
                statusHandler.info("Disconnecting from federation...");
                for (String registryId : servers
                        .getRegistryReplicationServers()) {
                    unsubscribeFromRegistry(registryId);
                }
                try {
                    if (RegistryFederationManager.centralRegistry) {
                        localLifecycleManager.removeObjects(req);
                    } else {
                        soapService.getLifecycleManagerServiceForHost(
                                ncfAddress).removeObjects(req);
                    }
                    statusHandler
                            .info("Registry disconnected from federation.");
                } catch (MsgRegistryException e) {
                    statusHandler.error(
                            "Error while disconnecting from federation!", e);
                }

            }
        } catch (Throwable e) {
            statusHandler.error("Error leaving federation", e);
            return false;
        }
        return true;
    }

    /**
     * Synchronizes this registry's data with the registry at the specified URL
     * 
     * @param remoteRegistryUrl
     *            The URL of the registry to sync with
     * @throws EbxmlRegistryException
     *             If the thread executor fails to shut down properly
     * @throws MsgRegistryException
     */
    @Transactional
    @GET
    @Path("synchronizeWithRegistry/{registryId}")
    public void synchronizeWithRegistry(
            @PathParam("registryId") String registryId) throws Exception {
        long start = TimeUtil.currentTimeMillis();
        RegistryType remoteRegistry = null;
        try {
            if (!registryId.endsWith(FederationProperties.REGISTRY_SUFFIX)) {
                registryId += FederationProperties.REGISTRY_SUFFIX;
            }
            remoteRegistry = dataDeliveryRestClient.getRegistryObject(
                    ncfAddress, registryId);
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error retrieving info for remote registry [" + registryId
                            + "] ", e);
        }
        if (remoteRegistry == null) {
            throw new EbxmlRegistryException("Unable to synchronize with ["
                    + registryId + "]. Registry not found in federation");
        }
        String remoteRegistryUrl = remoteRegistry.getBaseURL();

        for (final String objectType : replicatedObjectTypes) {
            syncObjectType(objectType, remoteRegistryUrl);
        }
        statusHandler.info("Registry synchronization using ["
                + remoteRegistryUrl + "] completed successfully in "
                + (TimeUtil.currentTimeMillis() - start) + " ms");
    }

    /**
     * Synchronizes objects of the specified type with the specified remote
     * registry
     * 
     * @param objectType
     *            The object type to synchronize
     * @param remoteRegistryUrl
     *            The url of the remote registry
     * @throws EbxmlRegistryException
     *             If there are errors deleting existing objects
     * @throws MsgRegistryException
     *             If there are errors executing the remote soap query
     */
    public void syncObjectType(String objectType, String remoteRegistryUrl)
            throws EbxmlRegistryException, MsgRegistryException {

        statusHandler.info("Deleting objects of type: " + objectType);
        // Executing a select before delete since Hibernate doesn't cascade
        // deletes properly when directly deleting with HQL
        registryObjectDao.deleteAll(registryObjectDao.query(
                "FROM RegistryObjectType obj where obj.objectType=:objectType",
                "objectType", objectType));
        registryObjectDao.flushAndClearSession();

        // Get the list of remote object ids so we can check later to ensure all
        // objects were retrieved
        RestCollectionResponse<String> response = dataDeliveryRestClient
                .getRegistryDataAccessService(remoteRegistryUrl)
                .getRegistryObjectIdsOfType(objectType);
        if (response.getPayload() == null) {
            statusHandler.info("0 objects of type [" + objectType
                    + "] present on remote registry. Skipping.");

        } else {
            List<String> remoteIds = new ArrayList<String>(
                    response.getPayload());

            statusHandler.info("Synchronizing " + remoteIds.size()
                    + " objects of type [" + objectType + "]");
            int batches = remoteIds.size() / SYNC_BATCH_SIZE;
            int remainder = remoteIds.size() % SYNC_BATCH_SIZE;

            for (int currentBatch = 0; currentBatch < batches; currentBatch++) {
                persistBatch(objectType, remoteRegistryUrl, remoteIds.subList(
                        currentBatch * SYNC_BATCH_SIZE, (currentBatch + 1)
                                * SYNC_BATCH_SIZE));
            }
            // Grab any remaining
            if (remainder > 0) {
                persistBatch(
                        objectType,
                        remoteRegistryUrl,
                        remoteIds.subList(batches * SYNC_BATCH_SIZE,
                                remoteIds.size()));
            }
        }
    }

    private void persistBatch(String objectType, String remoteRegistryUrl,
            List<String> batch) throws MsgRegistryException {

        // Average length of ids of registry object is 52. Add 3 for quotes and
        // comma
        StringBuilder builder = new StringBuilder(55 * batch.size());
        for (int i = 0; i < batch.size(); i++) {
            builder.append("'").append(batch.get(i)).append("'");
            if (i != batch.size() - 1) {
                builder.append(",");
            }
        }

        SlotType queryLanguageSlot = new SlotType(
                QueryConstants.QUERY_LANGUAGE, new StringValueType(
                        QueryLanguages.HQL));
        SlotType queryExpressionSlot = new SlotType(
                QueryConstants.QUERY_EXPRESSION, new StringValueType(""));
        QueryRequest queryRequest = new QueryRequest();
        QueryType query = new QueryType();
        query.setQueryDefinition(CanonicalQueryTypes.ADHOC_QUERY);
        query.getSlot().add(queryLanguageSlot);
        query.getSlot().add(queryExpressionSlot);
        queryRequest.setQuery(query);
        queryRequest.setResponseOption(new ResponseOptionType(
                QueryReturnTypes.REGISTRY_OBJECT, true));
        queryRequest.setId("Synchronizing object type: " + objectType);
        StringValueType queryValue = new StringValueType(String.format(
                SYNC_QUERY, builder.toString()));
        queryExpressionSlot.setSlotValue(queryValue);

        QueryResponse queryResponse = soapService.getQueryServiceForHost(
                remoteRegistryUrl).executeQuery(queryRequest);
        List<RegistryObjectType> queryResult = queryResponse
                .getRegistryObjects();
        if (!CollectionUtil.isNullOrEmpty(queryResult)) {
            registryObjectDao.persistAll(queryResult);
            registryObjectDao.flushAndClearSession();
        }
    }

    @GET
    @Path("isFederated")
    @Transactional
    public String isFederated() {
        return System.getenv("EBXML_REGISTRY_FEDERATION_ENABLED");
    }

    @GET
    @Path("dataDeliveryId")
    public String dataDeliveryId() {
        return DataDeliveryIdUtil.getId();
    }

    @GET
    @Path("siteId")
    public String siteId() {
        return System.getenv("AW_SITE_IDENTIFIER");
    }

    @GET
    @Path("getObjectTypesReplicated")
    public String getObjectTypesReplicated() {
        return RegistryQueryUtil.formatArrayString(replicatedObjectTypes
                .toArray());
    }

    @GET
    @Path("getFederationMembers")
    @Transactional
    public String getFederationMembers() throws Exception {
        Comparator<RegistryObjectType> sorter = new Comparator<RegistryObjectType>() {
            @Override
            public int compare(RegistryObjectType o1, RegistryObjectType o2) {
                return o1.getId().compareTo(o2.getId());
            }
        };
        StringBuilder builder = new StringBuilder();
        List<RegistryType> registries = getFederatedRegistries();
        Collections.sort(registries, sorter);
        for (RegistryType registry : registries) {
            appendRegistryInfo(registry, builder);
        }
        return builder.toString();
    }

    @GET
    @Path("getReplicatingTo")
    @Transactional
    public String getReplicatingTo() {
        return RegistryQueryUtil.formatArrayString(servers
                .getRegistryReplicationServers().toArray());
    }

    @GET
    @Path("getReplicatingFrom")
    @Transactional
    public String getReplicatingFrom() throws Exception {
        Set<String> registrySet = new HashSet<String>();
        List<RegistryType> registries = getFederatedRegistries();
        for (RegistryType registry : registries) {
            String remoteReplicatingTo = null;
            try {
                remoteReplicatingTo = dataDeliveryRestClient
                        .getRegistryFederationManager(registry.getBaseURL())
                        .getReplicatingTo();
            } catch (Exception e) {
                statusHandler.error("Error getting replication list from ["
                        + registry.getId() + "]", e);
                continue;
            }
            if (remoteReplicatingTo.contains(DataDeliveryIdUtil.getId())) {
                registrySet.add(registry.getId().replace(
                        FederationProperties.REGISTRY_SUFFIX, ""));
            }
        }
        return RegistryQueryUtil.formatArrayString(registrySet.toArray());
    }

    @GET
    @Path("subscribeToRegistry/{registryId}")
    @Transactional
    public void subscribeToRegistry(@PathParam("registryId") String registryId)
            throws Exception {
        statusHandler.info("Establishing replication with [" + registryId
                + "]...");
        RegistryType remoteRegistry = getRegistry(registryId);
        dataDeliveryRestClient.getRegistryFederationManager(
                remoteRegistry.getBaseURL()).addReplicationServer(
                DataDeliveryIdUtil.getId());
        statusHandler.info("Established replication with [" + registryId + "]");
    }

    @GET
    @Path("unsubscribeFromRegistry/{registryId}")
    @Transactional
    public void unsubscribeFromRegistry(
            @PathParam("registryId") String registryId) throws Exception {
        statusHandler.info("Disconnecting replication with [" + registryId
                + "]...");
        RegistryType remoteRegistry = getRegistry(registryId);
        dataDeliveryRestClient.getRegistryFederationManager(
                remoteRegistry.getBaseURL()).removeReplicationServer(
                DataDeliveryIdUtil.getId());
        statusHandler
                .info("Disconnected replication with [" + registryId + "]");
    }

    @GET
    @Path("addReplicationServer/{registryId}")
    @Transactional
    public void addReplicationServer(@PathParam("registryId") String registryId)
            throws Exception {
        getRegistry(registryId);
        servers.addReplicationServer(registryId);
        saveNotificationServers();
    }

    @GET
    @Path("removeReplicationServer/{registryId}")
    @Transactional
    public void removeReplicationServer(
            @PathParam("registryId") String registryId) throws Exception {
        getRegistry(registryId);
        servers.removeReplicationServer(registryId);
        saveNotificationServers();
    }

    /**
     * Appends registry information to the stringbuilder
     * 
     * @param registry
     *            The registry to get information for
     * @param builder
     *            The string builder to append to
     */
    private void appendRegistryInfo(RegistryType registry, StringBuilder builder) {
        builder.append(
                registry.getId().replace(FederationProperties.REGISTRY_SUFFIX,
                        "")).append(",");
        builder.append(registry.getBaseURL()).append(",");
        builder.append(registry.getConformanceProfile()).append(",");
        builder.append(registry.getSpecificationVersion());
        builder.append(StringUtil.NEWLINE);
    }

    /**
     * Persists the list of notification servers to the localized file
     */
    private void saveNotificationServers() {
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

    private List<RegistryType> getFederatedRegistries()
            throws EbxmlRegistryException {
        final Collection<String> remoteIds = dataDeliveryRestClient
                .getRegistryDataAccessService(ncfAddress)
                .getRegistryObjectIdsOfType(RegistryObjectTypes.REGISTRY)
                .getPayload();
        if (remoteIds.isEmpty()) {
            return Collections.emptyList();
        }
        List<RegistryType> registries = new ArrayList<RegistryType>(
                remoteIds.size());
        for (String remoteId : remoteIds) {
            registries.add(getRegistry(remoteId));
        }
        return registries;
    }

    private RegistryType getRegistry(String registryId)
            throws EbxmlRegistryException {
        RegistryType registry = null;
        try {
            if (!registryId.endsWith(FederationProperties.REGISTRY_SUFFIX)) {
                registryId += FederationProperties.REGISTRY_SUFFIX;
            }
            registry = dataDeliveryRestClient.getRegistryObject(ncfAddress,
                    registryId);
        } catch (Exception e) {
            throw new EbxmlRegistryException("Error retrieving registry ["
                    + registryId + "] from NCF", e);
        }
        if (registry == null) {
            throw new RegistryNotFoundException("Registry [" + registryId
                    + "] not found in federation");
        }
        return registry;
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
        if (centralRegistry) {
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

    @Subscribe
    @Transactional(propagation = Propagation.REQUIRED)
    public void processEvent(CreateAuditTrailEvent event) {
        String sourceRegistry = event.getRequest().getSlotValue(
                EbxmlObjectUtil.EVENT_SOURCE_SLOT);
        String actionType = event.getActionType();
        List<RegistryObjectType> objsAffected = event.getObjectsAffected();
        for (RegistryObjectType affectedObj : objsAffected) {
            if (replicatedObjectTypes.contains(affectedObj.getObjectType())) {
                ReplicationEvent replicationEvent = new ReplicationEvent(
                        actionType, event.getEventTime(), affectedObj.getId(),
                        affectedObj.getObjectType(), sourceRegistry);
                replicationEventDao.create(replicationEvent);
            }
        }
    }

    public void processReplicationEvents() {
        if (federationEnabled && DbInit.isDbInitialized() && initialized.get()) {
            if (!running.getAndSet(true)) {
                try {
                    for (final String remoteRegistryId : servers
                            .getRegistryReplicationServers()) {
                        txTemplate
                                .execute(new TransactionCallbackWithoutResult() {
                                    @Override
                                    protected void doInTransactionWithoutResult(
                                            TransactionStatus status) {
                                        try {
                                            long eventProcessingStart = TimeUtil
                                                    .currentTimeMillis();
                                            int eventsProcessed = processEventsFor(remoteRegistryId);
                                            if (eventsProcessed > 0) {
                                                statusHandler.info("Processed ["
                                                        + eventsProcessed
                                                        + "] replication events for ["
                                                        + remoteRegistryId
                                                        + "] in "
                                                        + (TimeUtil
                                                                .currentTimeMillis() - eventProcessingStart)
                                                        + " ms");
                                            }
                                        } catch (EbxmlRegistryException e) {
                                            throw new RuntimeException(
                                                    "Error processing events",
                                                    e);
                                        }
                                    }
                                });
                    }
                } finally {
                    running.set(false);
                }
            }
        }
    }

    private int processEventsFor(String remoteRegistryId)
            throws EbxmlRegistryException {
        int eventsProcessed = 0;
        RegistryType remoteRegistry = registryDao.getById(remoteRegistryId
                + FederationProperties.REGISTRY_SUFFIX);
        if (remoteRegistry == null) {
            statusHandler.warn("Registry [" + remoteRegistryId
                    + "] not present in federation. Skipping.");
        } else if (dataDeliveryRestClient.isRegistryAvailable(remoteRegistry
                .getBaseURL())) {

            List<ReplicationEvent> events = replicationEventDao
                    .getReplicationEvents(remoteRegistryId);

            List<SimpleEntry<String, List<ReplicationEvent>>> orderedBatchedEvents = new ArrayList<SimpleEntry<String, List<ReplicationEvent>>>();
            SimpleEntry<String, List<ReplicationEvent>> lastEntry = null;
            String currentEventType = null;
            for (ReplicationEvent event : events) {
                event.addReplicatedTo(remoteRegistryId);
                replicationEventDao.update(event);
                currentEventType = event.getEventType();
                /*
                 * If this is the first event processed or the event type
                 * differs from the last, create a new entry
                 */
                if (lastEntry == null
                        || !currentEventType.equals(lastEntry.getKey())) {
                    List<ReplicationEvent> newList = new ArrayList<ReplicationEvent>();
                    newList.add(event);
                    SimpleEntry<String, List<ReplicationEvent>> newEntry = new SimpleEntry<String, List<ReplicationEvent>>(
                            currentEventType, newList);
                    orderedBatchedEvents.add(newEntry);
                    lastEntry = newEntry;
                }
                /*
                 * If this event is of the same type as the last, append the
                 * event to the list
                 */
                else if (currentEventType.equals(lastEntry.getKey())) {
                    lastEntry.getValue().add(event);
                }
            }

            for (SimpleEntry<String, List<ReplicationEvent>> entry : orderedBatchedEvents) {
                currentEventType = entry.getKey();
                if (currentEventType.equals(ActionTypes.create)
                        || currentEventType.equals(ActionTypes.update)
                        || currentEventType.equals(ActionTypes.version)) {
                    SubmitObjectsRequest request = null;
                    request = new SubmitObjectsRequest();
                    request.setId("Replicate - Insert/Update events");
                    List<String> objIds = new ArrayList<String>(entry
                            .getValue().size());
                    for (ReplicationEvent event : entry.getValue()) {
                        objIds.add(event.getObjectId());
                    }
                    request.getRegistryObjects().addAll(
                            registryObjectDao.getById(objIds));
                    request.setCheckReferences(false);
                    request.setMode(Mode.CREATE_OR_REPLACE);
                    request.setUsername(RegistryUtil.registryUser);
                    request.getSlot().add(
                            new SlotType(EbxmlObjectUtil.EVENT_SOURCE_SLOT,
                                    new StringValueType(DataDeliveryIdUtil
                                            .getId())));
                    try {
                        if (!request.getRegistryObjects().isEmpty()) {
                            soapService.getLifecycleManagerServiceForHost(
                                    remoteRegistry.getBaseURL()).submitObjects(
                                    request);
                        }
                    } catch (MsgRegistryException e) {
                        throw new EbxmlRegistryException(
                                "Error processing events for ["
                                        + remoteRegistryId + "]", e);
                    }
                } else if (currentEventType.equals(ActionTypes.delete)) {
                    ObjectRefListType refList = new ObjectRefListType();
                    for (ReplicationEvent event : entry.getValue()) {
                        refList.getObjectRef().add(
                                new ObjectRefType(event.getObjectId()));
                    }
                    RemoveObjectsRequest request = new RemoveObjectsRequest(
                            "Replicate - Remove events", "", null, null,
                            refList, false, true, DeletionScope.DELETE_ALL);
                    request.getSlot().add(
                            new SlotType(EbxmlObjectUtil.EVENT_SOURCE_SLOT,
                                    new StringValueType(DataDeliveryIdUtil
                                            .getId())));
                    try {
                        if (!refList.getObjectRef().isEmpty()) {
                            soapService.getLifecycleManagerServiceForHost(
                                    remoteRegistry.getBaseURL()).removeObjects(
                                    request);
                        }
                    } catch (MsgRegistryException e) {
                        throw new EbxmlRegistryException(
                                "Error processing events for ["
                                        + remoteRegistryId + "]", e);
                    }

                }
            }
            eventsProcessed = events.size();
        } else {
            statusHandler.warn("Unable to replicate to [" + remoteRegistryId
                    + "]. Registry is currently unavailable.");
        }
        return eventsProcessed;
    }

    /**
     * Updates the record in the registry that keeps track of if this registry
     * has been up. This method is called every minute via a quartz cron
     * configured in Camel
     */
    @Transactional
    public void updateUpTime() {
        if (initialized.get()) {
            federatedRegistryMonitor.updateTime();
        }
    }

    @Transactional
    public void deleteExpiredEvents() {
        statusHandler.info("Purging expired replication events...");
        Calendar cutoffTime = TimeUtil.newGmtCalendar();
        cutoffTime.add(Calendar.HOUR_OF_DAY, -48);
        List<ReplicationEvent> events = replicationEventDao.executeHQLQuery(
                GET_EXPIRED_EVENTS_QUERY,
                GET_EXPIRED_EVENTS_QUERY_CUTOFF_PARAMETER,
                cutoffTime.getTimeInMillis());
        replicationEventDao.deleteAll(events);
        statusHandler.info("Purged " + events.size() + " expired events");
    }

    @Transactional
    public void verifyReplication() throws Exception {
        if (federationEnabled && servers != null
                && !servers.getRegistryReplicationServers().isEmpty()) {
            statusHandler
                    .info("Verifying replication with the following registries: "
                            + servers.getRegistryReplicationServers() + "...");
            for (String registryId : servers.getRegistryReplicationServers()) {
                verifyReplicationFor(registryId
                        + FederationProperties.REGISTRY_SUFFIX);
            }
            statusHandler.info("Replication verification complete");
        }
    }

    private void verifyReplicationFor(String registryId) throws Exception {
        statusHandler.info("Verifying replication with [" + registryId + "]");
        RegistryType remoteRegistry = registryDao.getById(registryId);
        String replicatingTo = dataDeliveryRestClient
                .getRegistryFederationManager(remoteRegistry.getBaseURL())
                .getReplicatingTo();
        if (replicatingTo.contains(DataDeliveryIdUtil.getId())) {
            statusHandler.info("Successfully verified replication with ["
                    + registryId + "]");
        } else {
            statusHandler.info("Establishing replication with [" + registryId
                    + "]...");
            subscribeToRegistry(registryId);
            statusHandler.info("Replication with [" + registryId
                    + "] established");
        }
    }

    public static void addObjectTypesToSubscribeTo(String... types) {
        for (String type : types) {
            if (!replicatedObjectTypes.contains(type)) {
                replicatedObjectTypes.add(type);
                statusHandler.info("Add object type for replication [" + type
                        + "]");
            }
        }
    }

    public void setReplicationEventDao(ReplicationEventDao replicationEventDao) {
        this.replicationEventDao = replicationEventDao;
    }

    public void setRegistryDao(RegistryDao registryDao) {
        this.registryDao = registryDao;
    }

    public void setTxTemplate(TransactionTemplate txTemplate) {
        this.txTemplate = txTemplate;
    }

    public void setSoapService(RegistrySOAPServices soapService) {
        this.soapService = soapService;
    }

    public void setLocalLifecycleManager(LifecycleManager localLifecycleManager) {
        this.localLifecycleManager = localLifecycleManager;
    }

    public void setDataDeliveryRestClient(
            DataDeliveryRESTServices dataDeliveryRestClient) {
        this.dataDeliveryRestClient = dataDeliveryRestClient;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setFederatedRegistryMonitor(
            FederatedRegistryMonitor federatedRegistryMonitor) {
        this.federatedRegistryMonitor = federatedRegistryMonitor;
    }

    public void setFederationDbInit(FederationDbInit federationDbInit) {
        this.federationDbInit = federationDbInit;
    }

    public static Set<String> getReplicatedObjectTypes() {
        return replicatedObjectTypes;
    }

    public NotificationServers getServers() {
        return servers;
    }

}
