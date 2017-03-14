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
package com.raytheon.uf.edex.registry.ebxml.services.rest;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;
import org.springframework.transaction.support.TransactionTemplate;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.registry.constants.DeletionScope;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.common.registry.services.rest.IRegistryFederationManager;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.ClusterIdUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.registry.ebxml.RegistryUsers;
import com.raytheon.uf.edex.registry.ebxml.dao.DbInit;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.dao.ReplicationEventDao;
import com.raytheon.uf.edex.registry.ebxml.dao.ReplicationSiteEventDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.exception.NoReplicationServersAvailableException;
import com.raytheon.uf.edex.registry.ebxml.init.RegistryInitializedListener;
import com.raytheon.uf.edex.registry.ebxml.services.RegistryRESTServices;
import com.raytheon.uf.edex.registry.ebxml.services.query.RegistryQueryUtil;
import com.raytheon.uf.edex.registry.ebxml.services.soap.RegistrySOAPServices;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;
import com.raytheon.uf.edex.registry.ebxml.util.RegistryIdUtil;
import com.raytheon.uf.edex.registry.events.CreateAuditTrailEvent;
import com.raytheon.uf.edex.registry.federation.FederatedRegistryMonitor;
import com.raytheon.uf.edex.registry.federation.FederationDbInit;
import com.raytheon.uf.edex.registry.federation.FederationProperties;
import com.raytheon.uf.edex.registry.federation.NotificationServers;
import com.raytheon.uf.edex.registry.federation.RegistryNotFoundException;
import com.raytheon.uf.edex.registry.federation.ReplicationEvent;
import com.raytheon.uf.edex.registry.synchronization.SynchronizationTask;
import com.raytheon.uf.edex.security.SecurityConfiguration;

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
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------------------------------
 * May 22, 2013  1707     bphillip  Initial implementation
 * Jul 29, 2013  2191     bphillip  Implemented registry sync for registries that have been down for
 *                                  an extended period of time
 * Oct 30, 2013  1538     bphillip  Changed submitObjects method to submit objects to NCF by default
 * Nov 20, 2013  2534     bphillip  Consolidated RegistryReplicationManager into this class and
 *                                  added reciprocal subscriptions.  Added remote subscription
 *                                  monitor.
 * Dec 02, 2013  1829     bphillip  Modified to use correct getters for slot values
 * Dec 09, 2013  2613     bphillip  Optimized registry sync function
 * Jan 15, 2014  2613     bphillip  Added leaveFederation method to prevent inactive registries from
 *                                  participating in the federation unintentionally.
 * Jan 21, 2014  2613     bphillip  Changed max down time which requires a sync
 * Feb 11, 2014  2771     bgonzale  Use Data Delivery ID instead of Site.
 * Feb 13, 2014  2769     bphillip  Refactored registry sync. Created quartz tasks to monitor
 *                                  registry uptime as well as subscription integrity
 * Mar 31, 2014  2889     dhladky   Added username for notification center tracking.
 * Apr 11, 2014  3011     bphillip  Removed automatic registry sync check on startup
 * Apr 15, 2014  3012     dhladky   Merge fixes.
 * Jun 05, 2014  1712     bhillip   Fixed typo. Registry now only updates uptime when federation is
 *                                  enabled
 * Jun 25, 2014  3320     dhladky   Remove all references to DD environment variables from setup.env
 * Jul 10, 2014  1717     bphillip  Central registry now inserts system user
 * Jul 28, 2014  2752     dhladky   Fixed bad registry user name.
 * Aug 27, 2014  3560     bphillip  Added updateRegistryEvents method
 * May 11, 2015  4448     bphillip  Separated EBXML Registry from Data Delivery
 * May 29, 2015  4448     bphillip  Added default user to registry on startup
 * Oct 20, 2015  4992     dhladky   Improve error handling.
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to LocalizationException
 * Feb 02, 2016  5388     dhladky   Improve performance, multi-threaded replication.
 * Feb 08, 2016  5198     dhladky   Class cast for String expecting Long fixed
 * Apr 20, 2016  5386     tjensen   Improve performance, multi-threaded synchronization.
 * Apr 27, 2016  5611     dhladky   Improve performance, multi-threaded gather.
 * May 11, 2016  5638     tjensen   Improve registry event replication
 * May 31, 2016  5659     dhladky   Don't send empty events, Don't kill thread with Runtime
 *                                  Exception. Continue attempts to connect if gather fails.
 * Aug 05, 2016  5810     tjensen   Refactor replication
 * Aug 16, 2016  5810     tjensen   Clean up logging from processEvent
 * Aug 25, 2016  5846     rjpeter   Remove InternationalString from DB. Update replication logging.
 * Sep 01, 2016  5810     tjensen   Added improved logging messages
 * Sep 22, 2016  5762     tjensen   Harden replication code
 * Oct 04, 2016  5762     tjensen   Fix connection check
 * 
 * </pre>
 * 
 * @author bphillip
 */
@Path(IRegistryFederationManager.REGISTRY_FEDERATION_MANAGER_PATH)
@Service
public class RegistryFederationManager implements IRegistryFederationManager,
        RegistryInitializedListener {

    /** The logger instance */
    protected static final Logger statusHandler = LoggerFactory
            .getLogger(RegistryFederationManager.class);

    private static final transient IUFStatusHandler monitorHandler = UFStatus
            .getMonitorHandler(RegistryFederationManager.class);

    /** Batch size for registry synchronization queries */
    private static final int SYNC_BATCH_SIZE = Integer.parseInt(System
            .getProperty("ebxml-notification-batch-size"));

    /** Pool size for registry synchronization thread pool */
    private static final int SYNC_POOL_SIZE = Integer.parseInt(System
            .getProperty("ebxml-federation-sync-threads"));

    /** Max number of seconds to wait for a sync thread to complete */
    private static final int SYNC_POOL_WAIT_TIME = 60000;

    private static final int REPLICATION_RETRY_ATTEMPTS = 3;

    private static Set<String> replicatedObjectTypes = new HashSet<>();

    public static final String FEDERATION_ID = "Registry Federation";

    /** The mode that EDEX was started in */
    public static final boolean centralRegistry;

    /** registry user property **/
    public static final String EDEX_SECURITY_AUTH_USER = "edex.security.auth.user";

    /** registry user password **/
    public static final String EDEX_SECURITY_AUTH_PASSWORD = "edex.security.auth.password";

    /** Local (client) level adminstrator user **/
    public static final String REGISTRY_LOCAL_ADMIN = "RegistryLocalAdministrator";

    /** Registry adminstrator (central) **/
    public static final String REGISTRY_ADMIN = "RegistryAdministrator";

    /** primary replication directory **/
    public static final String REPLICATION_PATH = "/awips2/edex/data/replication/";

    /** Count down latch object for gather **/
    private final LinkedList<String> gatherServers = new LinkedList<>();

    private static Map<String, Integer> gatherFails = new HashMap<>();

    /** Queue backed gather thread pool executor **/
    private Executor gatherThreadPool;
    static {
        try {
            /*
             * Create replication directory tree
             */
            File repDir = new File(REPLICATION_PATH);
            if (!repDir.exists()) {
                repDir.mkdirs();
            }

            /*
             * Compares the ncf.host and ebxml.registry.host properties. If they
             * are the same, then this registry is assumed to be the central
             * registry
             */
            centralRegistry = InetAddress.getByName(
                    System.getProperty("ncf.host")).equals(
                    InetAddress.getByName(System
                            .getProperty("ebxml.registry.host")));
            if (centralRegistry) {
                statusHandler
                        .info("This registry instance is configured as a central registry");
            } else {
                statusHandler
                        .info("This registry instance is not configured as a central registry instance. The central registry instance is located at "
                                + System.getProperty("ncf.host"));
            }
        } catch (UnknownHostException e) {
            throw new RuntimeException(
                    "RegistryFederationManager is unable to determine if this registry is a central registry instance",
                    e);
        }
    }

    private static final String FEDERATION_CONFIG_FILE = "ebxml/registry/federationConfig.xml";

    /**
     * The name of the configuration files defining which servers we are
     * sending/receiving replicated objects to/from
     */
    private static final String NOTIFICATION_SERVERS_FILE = "ebxml/registry/notificationServers.xml";

    private static FederationProperties federationProperties;

    /**
     * The maximum time a registry can be down before a full synchronization is
     * performed
     */
    private static final long MAX_DOWN_TIME_DURATION = TimeUtil.MILLIS_PER_HOUR * 48;

    public static AtomicBoolean SYNC_IN_PROGRESS = new AtomicBoolean(false);

    /**
     * Denotes if initialization has already occurred for this class. It is a
     * static variable because at this time, multiple Spring containers load
     * this class, yet it only needs to be initialized once
     */
    public static AtomicBoolean initialized = new AtomicBoolean(false);

    private final boolean federationEnabled = Boolean.parseBoolean(System
            .getProperty("ebxml.registry.federation.enabled"));

    /**
     * The servers that we are subscribing to are tracked in three places:
     * 
     * 1) An XML file used to persist what registries to track events for
     * between processes and to initialize the other two. Updated whenever the
     * ReplicationRegistry DB is updated (kept in sync).
     * 
     * 2) The ReplicationRegistry DB tracks events in the ReplicationEvent table
     * that need to be replicated for each registry. Updated when a new registry
     * joins the federation or events for a registry begin being purged due to
     * timeout. 'registryServers' is used to cache the names of the registries
     * in the DB.
     * 
     * 3) 'replicateToServers' tracks what registries to attempt replication to.
     * Updated when a new registry joins or leaves the federation.
     */
    private static NotificationServers replicateToServers;

    private final String ncfAddress = "https://"
            + (System.getProperty("ncf.host")) + ":"
            + (System.getProperty("ebxml.registry.webserver.port"));

    /** Monitors how long this registry has been connected to the federation */
    private FederatedRegistryMonitor federatedRegistryMonitor;

    /** The JAXB Manager for serializing registry objects */
    protected JAXBManager jaxbManager;

    private ReplicationEventDao replicationEventDao;

    private ReplicationSiteEventDao replicationSiteEventDao;

    private RegistryDao registryDao;

    private TransactionTemplate txTemplate;

    private RegistrySOAPServices soapService;

    private LifecycleManager localLifecycleManager;

    private RegistryRESTServices restClient;

    private RegistryObjectDao registryObjectDao;

    private FederationDbInit federationDbInit;

    private RegistryUsers registryUsers;

    private SecurityConfiguration securityConfig;

    public RegistryFederationManager() throws JAXBException {
        jaxbManager = new JAXBManager(RegistryRequestType.class,
                SubmitObjectsRequest.class, RemoveObjectsRequest.class,
                FederationProperties.class, NotificationServers.class,
                RegistryObjectType.class, SubscriptionType.class);
    }

    @Override
    @Transactional
    public void executeAfterRegistryInit() throws EbxmlRegistryException {
        if (federationEnabled && !initialized.get()) {
            try {
                this.federationDbInit.initDb();
            } catch (Exception e) {
                throw new EbxmlRegistryException(
                        "Error initializing database for federation!", e);
            }
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
                        federationProperties = jaxbManager
                                .unmarshalFromXmlFile(
                                        FederationProperties.class,
                                        federationPropertiesFile);
                    }
                }

                if (replicateToServers == null) {
                    File notificationServerConfigFile = PathManagerFactory
                            .getPathManager().getStaticFile(
                                    NOTIFICATION_SERVERS_FILE);

                    /*
                     * If this registry is participating in the federation and
                     * the config file is not found, throw an error
                     */
                    if (federationEnabled
                            && notificationServerConfigFile == null) {
                        throw new EbxmlRegistryException(
                                "Notification server config file not found!");
                    }

                    try {
                        statusHandler.info("Loading replication servers from ["
                                + notificationServerConfigFile.getPath()
                                + "]...");
                        replicateToServers = jaxbManager.unmarshalFromXmlFile(
                                NotificationServers.class,
                                notificationServerConfigFile);
                    } catch (SerializationException e) {
                        throw new EbxmlRegistryException(
                                "Error unmarshalling notification servers file!",
                                e);
                    }
                    statusHandler.info("Found "
                            + replicateToServers
                                    .getRegistryReplicationServers().size()
                            + " servers for replication");
                }

                if (!joinFederation()) {
                    throw new EbxmlRegistryException(
                            "Error joining federation!!");
                }

            } catch (Exception e1) {
                throw new EbxmlRegistryException(
                        "Error initializing RegistryFederationManager", e1);
            }

            Runtime.getRuntime().addShutdownHook(new Thread() {
                @Override
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

        /*
         * If this is the central registry then we ensure that the superuser is
         * in the registry
         */
        try {
            if (centralRegistry) {
                if (!registryUsers.userExists(RegistryUtil.registryUser)) {
                    /*
                     * The registry super user initially gets the default
                     * password which *must* be changed immediately
                     */
                    statusHandler.info(REGISTRY_ADMIN
                            + " Not present in Central Registry! Adding user: "
                            + REGISTRY_ADMIN);

                    registryUsers.addUser(RegistryUtil.registryUser,
                            "password", REGISTRY_ADMIN);
                } else {
                    statusHandler
                            .info(REGISTRY_ADMIN
                                    + " present in Central Registry: "
                                    + REGISTRY_ADMIN);
                }

            } else if (!centralRegistry) {

                if (!registryUsers.userExists(securityConfig
                        .getSecurityProperties().getProperty(
                                EDEX_SECURITY_AUTH_USER))) {

                    statusHandler.info(EDEX_SECURITY_AUTH_USER
                            + " Not present in Registry! Adding user: "
                            + securityConfig.getSecurityProperties()
                                    .getProperty(EDEX_SECURITY_AUTH_USER));

                    registryUsers.addUser(
                            securityConfig.getSecurityProperties().getProperty(
                                    EDEX_SECURITY_AUTH_USER),
                            securityConfig.getSecurityProperties().getProperty(
                                    EDEX_SECURITY_AUTH_PASSWORD),
                            REGISTRY_LOCAL_ADMIN);
                } else {
                    statusHandler.info(EDEX_SECURITY_AUTH_USER
                            + " Present in Registry: "
                            + securityConfig.getSecurityProperties()
                                    .getProperty(EDEX_SECURITY_AUTH_USER));
                }
            }
        } catch (MsgRegistryException e) {
            throw new EbxmlRegistryException("Error Checking registry user! ",
                    e);
        }

        initialized.set(true);
    }

    public boolean joinFederation() {
        try {
            final List<RegistryObjectType> objects = new ArrayList<>(5);
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
                submitObjectsRequest.getSlot().add(
                        new SlotType(EbxmlObjectUtil.EVENT_SOURCE_SLOT,
                                new StringValueType(RegistryIdUtil.getId())));
                try {
                    statusHandler
                            .info("Submitting federation registration objects to local registry...");
                    localLifecycleManager.submitObjects(submitObjectsRequest);
                    statusHandler
                            .info("Successfully submitted federation registration objects to local registry!");
                } catch (MsgRegistryException e) {
                    throw new EbxmlRegistryException(
                            "Error submitting federation objects to local registry",
                            e);
                }
                if (!centralRegistry) {
                    statusHandler
                            .info("Submitting federation registration objects to NCF (Central Registry)...");
                    try {
                        soapService.getLifecycleManagerServiceForHost(
                                ncfAddress).submitObjects(submitObjectsRequest);
                        statusHandler
                                .info("Successfully submitted federation registration objects to NCF (Central Registry)!");
                    } catch (MsgRegistryException e) {
                        throw new EbxmlRegistryException(
                                "Error submitting federation objects to NCF (Central Registry)!",
                                e);
                    }
                }

                for (String registryId : replicateToServers
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
                for (String registryId : replicateToServers
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
     * Checks how long a registry has been down. If the registry has been down
     * for over 2 days, the registry is synchronized with one of the federation
     * members
     * 
     * @throws Exception
     */
    private void synchronize() throws EbxmlRegistryException {

        monitorHandler.warn("Synchronizing registry with federation...");
        RegistryType registryToSyncFrom = null;
        for (String remoteRegistryId : replicateToServers
                .getRegistryReplicationServers()) {
            statusHandler.info("Checking availability of [" + remoteRegistryId
                    + "]...");
            RegistryType remoteRegistry = null;
            try {
                remoteRegistry = restClient
                        .getRegistryObject(ncfAddress, remoteRegistryId
                                + FederationProperties.REGISTRY_SUFFIX);
            } catch (Exception e) {
                throw new EbxmlRegistryException(
                        "Error getting remote registry object!", e);
            }
            if (remoteRegistry == null) {
                statusHandler
                        .warn("Registry at ["
                                + remoteRegistryId
                                + "] not found in federation. Unable to use as synchronization source.");
            } else if (restClient.isRegistryAvailable(remoteRegistry
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
            try {
                synchronizeWithRegistry(registryToSyncFrom.getId());
            } catch (Exception e) {
                throw new EbxmlRegistryException(
                        "Error synchronizing with registry ["
                                + registryToSyncFrom.getId() + "]", e);
            }
        }

    }

    /**
     * Removes the specified registry from all events prior to a given time.
     * This is used to prevent replication of events to registries that have
     * caught up through synchronization.
     */
    @Override
    @Transactional
    @GET
    @Path("updateRegistryEvents/{registryId}/{time}")
    public void updateRegistryEvents(@PathParam("registryId")
    String registryId, @PathParam("time")
    String time) {
        
        Long lTime= Long.parseLong(time);
        
        try {
            replicationSiteEventDao.removeEventsBeforeTime(lTime, registryId);
        } catch (DataAccessLayerException e) {
            statusHandler.error("Error occurred updating registry uptime for "
                    + registryId, e);
        }
    }

    /**
     * Removes the event from the selected registry. If the event is no longer
     * needed by any registries, remove the event as well. The update to the
     * registry's event list is persisted when the transaction completes.
     * 
     * @param registryId
     *            Registry to remove the event from
     * @param eventId
     *            Event to be removed
     */
    private void removeRegistryFromEvent(String registryId, List<Long> eventIds) {
        try {
            replicationSiteEventDao.deleteMapping(eventIds, registryId);
        } catch (DataAccessLayerException e) {
            statusHandler.error(
                    "Error occurred removing events from replication for site "
                            + registryId, e);
        }
    }

    /**
     * Synchronizes this registry's data with the registry at the specified URL
     * 
     * @param registryId
     * @throws EbxmlRegistryException
     *             If the thread executor fails to shut down properly
     * @throws MsgRegistryException
     */
    @Override
    @Transactional
    @GET
    @Path("synchronizeWithRegistry/{registryId}")
    public void synchronizeWithRegistry(@PathParam("registryId")
    String registryId) throws Exception {

        if (SYNC_IN_PROGRESS.compareAndSet(false, true)) {
            try {
                long start = TimeUtil.currentTimeMillis();

                monitorHandler.handle(Priority.WARN,
                        "Synchronizing registry with [" + registryId + "]...");
                RegistryType remoteRegistry = null;
                try {
                    if (!registryId
                            .endsWith(FederationProperties.REGISTRY_SUFFIX)) {
                        registryId += FederationProperties.REGISTRY_SUFFIX;
                    }
                    remoteRegistry = restClient.getRegistryObject(ncfAddress,
                            registryId);
                } catch (Exception e) {
                    throw new EbxmlRegistryException(
                            "Error retrieving info for remote registry ["
                                    + registryId + "] ", e);
                }
                if (remoteRegistry == null) {
                    throw new EbxmlRegistryException(
                            "Unable to synchronize with [" + registryId
                                    + "]. Registry not found in federation");
                }
                final String remoteRegistryUrl = remoteRegistry.getBaseURL();

                ThreadPoolTaskExecutor taskExecutor = new ThreadPoolTaskExecutor();
                taskExecutor.setMaxPoolSize(SYNC_POOL_SIZE);
                taskExecutor.setKeepAliveSeconds(SYNC_POOL_WAIT_TIME);
                taskExecutor.initialize();

                for (final String objectType : replicatedObjectTypes) {
                    taskExecutor.execute(new SynchronizationTask(objectType,
                            remoteRegistryUrl, txTemplate, restClient,
                            soapService, registryObjectDao));

                }
                // Wait for all threads to finished.
                taskExecutor.setWaitForTasksToCompleteOnShutdown(true);
                taskExecutor.shutdown();
                try {
                    taskExecutor.getThreadPoolExecutor().awaitTermination(
                            SYNC_POOL_WAIT_TIME, TimeUnit.SECONDS);
                } catch (IllegalStateException | InterruptedException e) {
                    throw new EbxmlRegistryException(
                            "Error synchronizing with ["
                                    + registryId
                                    + "]. All threads did not complete successfully",
                            e);
                }

                federatedRegistryMonitor.updateTime();
                StringBuilder syncMsg = new StringBuilder();

                /*
                 * Update the registry events table on the remote registry so
                 * duplicate data is not sent again
                 */
                restClient.getRegistryFederationManager(remoteRegistryUrl)
                        .updateRegistryEvents(ClusterIdUtil.getId(),
                                String.valueOf(start));

                syncMsg.append("Registry synchronization using [")
                        .append(remoteRegistryUrl)
                        .append("] completed successfully in ")
                        .append((TimeUtil.currentTimeMillis() - start))
                        .append(" ms");
                // Reset the fail count since the site has resynced.
                gatherFails.put(remoteRegistryUrl, 0);
                statusHandler.info(syncMsg.toString());
            } catch (Exception e) {
                statusHandler
                        .error("Could not create registry synchronization objects!",
                                e);
            } finally {
                SYNC_IN_PROGRESS.set(false);
            }

        } else {
            statusHandler.info("Registry sync already in progress.");
        }
    }

    @Override
    @GET
    @Path("isFederated")
    @Transactional
    public String isFederated() {
        return System.getProperty("ebxml.registry.federation.enabled");
    }

    @Override
    @GET
    @Path("clusterId")
    public String clusterId() {
        return RegistryIdUtil.getId();
    }

    @Override
    @GET
    @Path("siteId")
    public String siteId() {
        return EDEXUtil.getEdexSite();
    }

    @Override
    @GET
    @Path("getObjectTypesReplicated")
    public String getObjectTypesReplicated() {
        return RegistryQueryUtil.formatArrayString(replicatedObjectTypes
                .toArray());
    }

    @Override
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

    @Override
    @GET
    @Path("getReplicatingTo")
    @Transactional
    public String getReplicatingTo() {
        return RegistryQueryUtil.formatArrayString(replicateToServers
                .getRegistryReplicationServers().toArray());
    }

    @Override
    @GET
    @Path("getReplicatingFrom")
    @Transactional
    public String getReplicatingFrom() throws Exception {
        Set<String> registrySet = new HashSet<>();
        List<RegistryType> registries = getFederatedRegistries();
        for (RegistryType registry : registries) {
            String remoteReplicatingTo = null;
            try {
                remoteReplicatingTo = restClient.getRegistryFederationManager(
                        registry.getBaseURL()).getReplicatingTo();
            } catch (Exception e) {
                statusHandler.error("Error getting replication list from ["
                        + registry.getId() + "]", e);
                continue;
            }
            if (remoteReplicatingTo.contains(RegistryIdUtil.getId())) {
                registrySet.add(registry.getId().replace(
                        FederationProperties.REGISTRY_SUFFIX, ""));
            }
        }
        return RegistryQueryUtil.formatArrayString(registrySet.toArray());
    }

    @Override
    @GET
    @Path("subscribeToRegistry/{registryId}")
    @Transactional
    public void subscribeToRegistry(@PathParam("registryId")
    String registryId) throws Exception {
        statusHandler.info("Establishing replication with [" + registryId
                + "]...");
        RegistryType remoteRegistry = getRegistry(registryId);
        restClient.getRegistryFederationManager(remoteRegistry.getBaseURL())
                .addReplicationServer(RegistryIdUtil.getId());
        statusHandler.info("Established replication with [" + registryId + "]");
    }

    @Override
    @GET
    @Path("unsubscribeFromRegistry/{registryId}")
    @Transactional
    public void unsubscribeFromRegistry(@PathParam("registryId")
    String registryId) throws Exception {
        statusHandler.info("Disconnecting replication with [" + registryId
                + "]...");
        RegistryType remoteRegistry = getRegistry(registryId);
        restClient.getRegistryFederationManager(remoteRegistry.getBaseURL())
                .removeReplicationServer(RegistryIdUtil.getId());
        statusHandler
                .info("Disconnected replication with [" + registryId + "]");
    }

    @Override
    @GET
    @Path("addReplicationServer/{registryId}")
    @Transactional
    public void addReplicationServer(@PathParam("registryId")
    String registryId) throws Exception {
        /*
         * Whenever we add a new replication server, make sure we add it to the
         * registry list and the XML file as well so we properly track events
         * for that registry.
         */
        replicateToServers.addReplicationServer(registryId);
        saveNotificationServers();
    }

    @Override
    @GET
    @Path("removeReplicationServer/{registryId}")
    @Transactional
    public void removeReplicationServer(@PathParam("registryId")
    String registryId) {
        /*
         * Removing a replication server only means we stop replicating events
         * to it, but the replication registry DB need to keep tracking events
         * to replicate at next connection. DB and XML file will be updated if
         * the registry exceeds the max downtime and the events can be purged.
         */
        replicateToServers.removeReplicationServer(registryId);
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
        try (SaveableOutputStream os = lf.openOutputStream()) {
            jaxbManager.marshalToStream(replicateToServers, os);
        } catch (SerializationException | IOException e) {
            statusHandler.error("Unable to update replication server file!", e);
        } catch (LocalizationException e) {
            statusHandler
                    .error("Error occurred saving notification servers", e);
        }
    }

    private List<RegistryType> getFederatedRegistries()
            throws EbxmlRegistryException {
        final Collection<String> remoteIds = restClient
                .getRegistryDataAccessService(ncfAddress)
                .getRegistryObjectIdsOfType(RegistryObjectTypes.REGISTRY)
                .getPayload();
        if (remoteIds.isEmpty()) {
            return Collections.emptyList();
        }
        List<RegistryType> registries = new ArrayList<>(remoteIds.size());
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
            registry = restClient.getRegistryObject(ncfAddress, registryId);
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
            federation.setName(FEDERATION_ID);
            federation.setDescription(FEDERATION_ID);
            federation.setOwner(RegistryUtil.registryUser);
            federation.setStatus(StatusTypes.APPROVED);
            federation.setObjectType(RegistryObjectTypes.FEDERATION);
            federation
                    .setReplicationSyncLatency(RegistryFederationManager.federationProperties
                            .getFederationReplicationSyncLatency());
        } else {
            statusHandler
                    .info("Retrieving Federation object from NCF registry at ["
                            + ncfAddress + "]...");
            federation = restClient
                    .getRegistryObject(ncfAddress, FEDERATION_ID);
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
        boolean eventsToReplicate = false;

        for (RegistryObjectType affectedObj : objsAffected) {
            if (replicatedObjectTypes.contains(affectedObj.getObjectType())) {
                ReplicationEvent replicationEvent = new ReplicationEvent(
                        actionType, event.getEventTime(), affectedObj.getId(),
                        affectedObj.getObjectType(), sourceRegistry);

                // Add all registries we are tracking events for to the event
                for (String registryId : replicateToServers
                        .getRegistryReplicationServers()) {
                    if (!registryId.equals(replicationEvent.getSource())) {
                        // Persisted when event is created.
                        replicationEvent.addReplicateTo(registryId);
                    }
                }
                if (!replicationEvent.getReplicateTo().isEmpty()) {
                    replicationEventDao.create(replicationEvent);
                    eventsToReplicate = true;
                } else {
                    statusHandler
                            .debug("Event from source '"
                                    + replicationEvent.getSource()
                                    + "' dropped due to no valid targets to replicate to.");
                }
            }
        }

        if (eventsToReplicate) {
            gatherReplicationEvents(replicateToServers
                    .getRegistryReplicationServers());
        }
    }

    /**
     * Gathers replication events by site and sends them to be processed on
     * different threads
     * 
     * @param replicateToSet
     *            Set of registryId's to gather for if not already being
     *            processed.
     */
    public void gatherReplicationEvents(Set<String> replicateToSet) {
        if (federationEnabled && DbInit.isDbInitialized() && initialized.get()
                && !SYNC_IN_PROGRESS.get()) {

            for (String remoteRegistryId : replicateToSet) {
                if (!gatherServers.contains(remoteRegistryId)) {
                    gatherServers.add(remoteRegistryId);
                    try {
                        gatherThreadPool.execute(new GatherTask(
                                remoteRegistryId));

                    } catch (Exception e) {
                        statusHandler.error(
                                "Unable to create Gather Task. ID: "
                                        + remoteRegistryId, e);
                        gatherServers.remove(remoteRegistryId);
                    }
                }
            }
        }
    }

    /**
     * Inner thread class to gather events for each site. The events are then
     * handed off to replication.
     */
    private class GatherTask implements Runnable {

        /** remote registry ID events are gathered for */
        private final String remoteRegistryId;

        private int eventsGathered = -1;

        /** constructor **/
        public GatherTask(String remoteRegistryId) {
            this.remoteRegistryId = remoteRegistryId;
        }

        @Override
        public void run() {

            txTemplate.execute(new TransactionCallbackWithoutResult() {
                @Override
                protected void doInTransactionWithoutResult(
                        TransactionStatus status) {
                    try {
                        long eventGatheringStart = TimeUtil.currentTimeMillis();
                        eventsGathered = gatherEventsFor(remoteRegistryId);
                        if (eventsGathered > 0) {
                            statusHandler.info("Sent ["
                                    + eventsGathered
                                    + "] replication event(s) to ["
                                    + remoteRegistryId
                                    + "] in "
                                    + (TimeUtil.currentTimeMillis() - eventGatheringStart)
                                    + " ms");
                        }
                    } catch (ConnectException e) {
                        statusHandler.error("Sending events to ["
                                + remoteRegistryId
                                + "] has failed due to connection loss.", e);
                    } catch (Exception e) {
                        statusHandler.error("Sending events to ["
                                + remoteRegistryId + "] has failed.", e);
                    } finally {
                        synchronized (gatherServers) {
                            gatherServers.remove(remoteRegistryId);
                        }
                    }
                }
            });

            /*
             * If events were found, attempt to gather events for this
             * registryId again. If no events were found, wait for a new event
             * to process.
             */
            if (eventsGathered != 0) {
                Set<String> registrySet = new HashSet<>(2);
                registrySet.add(remoteRegistryId);
                gatherReplicationEvents(registrySet);
            }
        }
    }

    /**
     * Gather replication events for a registry and processing them
     * 
     * @param remoteRegistryId
     * @return
     * @throws EbxmlRegistryException
     * @throws ConnectException
     */
    private int gatherEventsFor(String remoteRegistryId)
            throws EbxmlRegistryException, ConnectException {
        int eventsGathered = 0;
        RegistryType remoteRegistry = registryDao.getById(remoteRegistryId
                + FederationProperties.REGISTRY_SUFFIX);
        if (remoteRegistry == null) {
            statusHandler.warn("Registry [" + remoteRegistryId
                    + "] not present in federation. Skipping...");
            return eventsGathered;
        }

        if (!restClient.isRegistryAvailable(remoteRegistry.getBaseURL())) {
            statusHandler.warn("Unable to gather events for ["
                    + remoteRegistryId
                    + "]. Registry is currently unavailable.");
            return eventsGathered;
        }

        long t0 = TimeUtil.currentTimeMillis();
        List<Long> eventIds = replicationSiteEventDao.getEventsBatch(
                remoteRegistryId, SYNC_BATCH_SIZE);

        if (eventIds != null && !eventIds.isEmpty()) {
            List<ReplicationEvent> events = replicationEventDao
                    .getEvents(eventIds);
            long t1 = TimeUtil.currentTimeMillis();
            eventsGathered = events.size();

            statusHandler.info("Gathered {} events for {} in {}",
                    eventsGathered, remoteRegistryId,
                    TimeUtil.prettyDuration(t1 - t0));

            List<List<ReplicationEvent>> orderedBatchedEvents = new ArrayList<>();
            List<ReplicationEvent> currentList = null;
            boolean previousIsDelete = false;

            for (ReplicationEvent event : events) {
                boolean isDelete = ActionTypes.delete.equals(event
                        .getEventType());

                /*
                 * If the event type differs from the last or if this is the
                 * first event processed, create a new list.
                 */
                if ((isDelete != previousIsDelete) || currentList == null) {
                    previousIsDelete = isDelete;
                    currentList = new ArrayList<>();
                    orderedBatchedEvents.add(currentList);
                }

                currentList.add(event);
            }

            for (List<ReplicationEvent> batchedEvents : orderedBatchedEvents) {
                String currentEventType = batchedEvents.get(0).getEventType();
                int batchSize = batchedEvents.size();

                if (!currentEventType.equals(ActionTypes.delete)) {

                    List<String> objIds = new ArrayList<>(batchSize);
                    for (ReplicationEvent event : batchedEvents) {
                        objIds.add(event.getObjectId());
                    }

                    t0 = TimeUtil.currentTimeMillis();
                    List<RegistryObjectType> dbResults = registryObjectDao
                            .getById(objIds);
                    t1 = TimeUtil.currentTimeMillis();
                    int dbSize = dbResults.size();

                    if (batchSize == dbSize) {
                        statusHandler
                                .info("Retrieved {} registry objects from local registry in {}",
                                        batchSize,
                                        TimeUtil.prettyDuration(t1 - t0));
                    } else {
                        statusHandler
                                .info("Retrieved {} registry objects from local registry in {}. Expected {} objects, some objects have already been deleted from registry before they could be synced.",
                                        dbSize,
                                        TimeUtil.prettyDuration(t1 - t0),
                                        batchSize);
                    }

                    if (dbSize > 0) {
                        SubmitObjectsRequest request = null;
                        request = new SubmitObjectsRequest();
                        request.setId("Replicate - Insert/Update events");
                        request.setCheckReferences(false);
                        request.setMode(Mode.CREATE_OR_REPLACE);
                        request.setUsername(RegistryUtil.registryUser);
                        RegistryObjectListType objList = new RegistryObjectListType();
                        objList.setRegistryObject(dbResults);
                        request.setRegistryObjectList(objList);
                        request.getSlot().add(
                                new SlotType(EbxmlObjectUtil.EVENT_SOURCE_SLOT,
                                        new StringValueType(RegistryIdUtil
                                                .getId())));

                        int submitAttempts = 0;
                        while (restClient.isRegistryAvailable(remoteRegistry
                                .getBaseURL())
                                && submitAttempts < REPLICATION_RETRY_ATTEMPTS) {
                            submitAttempts++;
                            t0 = TimeUtil.currentTimeMillis();
                            try {
                                soapService.getLifecycleManagerServiceForHost(
                                        remoteRegistry.getBaseURL())
                                        .submitObjects(request);
                                t1 = TimeUtil.currentTimeMillis();
                                statusHandler.info(
                                        "Sent {} registry objects to {} in {}",
                                        batchSize, remoteRegistryId,
                                        TimeUtil.prettyDuration(t1 - t0));
                                // Successfully sent request, so stop looping.
                                break;
                            } catch (Exception e) {
                                t1 = TimeUtil.currentTimeMillis();
                                statusHandler.error(
                                        "Failed after "
                                                + TimeUtil.prettyDuration(t1
                                                        - t0) + " submitting "
                                                + batchSize + " objects to "
                                                + remoteRegistryId, e);
                                handleGatherSoapFailure(remoteRegistryId,
                                        remoteRegistry, batchSize,
                                        submitAttempts);
                            }
                        }
                    }
                } else {
                    ObjectRefListType refList = new ObjectRefListType();
                    List<ObjectRefType> refs = new ArrayList<>(batchSize);
                    for (ReplicationEvent event : batchedEvents) {
                        refs.add(new ObjectRefType(event.getObjectId()));
                    }
                    refList.setObjectRef(refs);
                    RemoveObjectsRequest request = new RemoveObjectsRequest(
                            "Replicate - Remove events", "", null, null,
                            refList, false, true, DeletionScope.DELETE_ALL);
                    request.getSlot()
                            .add(new SlotType(
                                    EbxmlObjectUtil.EVENT_SOURCE_SLOT,
                                    new StringValueType(RegistryIdUtil.getId())));

                    int removeAttempts = 0;
                    while (restClient.isRegistryAvailable(remoteRegistry
                            .getBaseURL())
                            && removeAttempts < REPLICATION_RETRY_ATTEMPTS) {
                        removeAttempts++;
                        t0 = TimeUtil.currentTimeMillis();
                        try {
                            soapService.getLifecycleManagerServiceForHost(
                                    remoteRegistry.getBaseURL()).removeObjects(
                                    request);
                            t1 = TimeUtil.currentTimeMillis();
                            statusHandler
                                    .info("Removed {} registry objects from {} in {}",
                                            batchSize, remoteRegistryId,
                                            TimeUtil.prettyDuration(t1 - t0));
                            // Successfully sent request, so stop looping.
                            break;
                        } catch (Exception e) {
                            t1 = TimeUtil.currentTimeMillis();
                            statusHandler.error(
                                    "Failed after "
                                            + TimeUtil.prettyDuration(t1 - t0)
                                            + " removing " + batchSize
                                            + " objects from "
                                            + remoteRegistryId, e);
                            handleGatherSoapFailure(remoteRegistryId,
                                    remoteRegistry, batchSize, removeAttempts);
                        }
                    }
                }

                /*
                 * Remove registry from event's list. Events with no registries
                 * left to replicate to will be cleaned up by
                 * deleteExpiredEvents.
                 */
                List<Long> ids = new ArrayList<>(batchSize);
                for (ReplicationEvent event : batchedEvents) {
                    ids.add(event.getId());
                }
                t0 = TimeUtil.currentTimeMillis();
                removeRegistryFromEvent(remoteRegistryId, ids);
                t1 = TimeUtil.currentTimeMillis();
                statusHandler.info(
                        "Removed {} replication events for {} in {}",
                        batchSize, remoteRegistryId,
                        TimeUtil.prettyDuration(t1 - t0));
            }
        } else {
            statusHandler.info("No events found for {}", remoteRegistryId);
        }

        return eventsGathered;
    }

    private void handleGatherSoapFailure(String remoteRegistryId,
            RegistryType remoteRegistry, int batchSize, int attempts)
            throws ConnectException {
        if (restClient.isRegistryAvailable(remoteRegistry.getBaseURL())
                && attempts < REPLICATION_RETRY_ATTEMPTS) {
            statusHandler.info("Attempt " + attempts
                    + " unsuccessful. Attempting to resend request.");
        } else if (attempts >= REPLICATION_RETRY_ATTEMPTS) {
            int failCount = gatherFails.containsKey(remoteRegistryId) ? gatherFails
                    .get(remoteRegistryId) : 0;
            failCount = failCount + batchSize;
            gatherFails.put(remoteRegistryId, failCount);
            statusHandler
                    .error("Unable to successfully send request to "
                            + remoteRegistryId
                            + ". "
                            + batchSize
                            + " objects will be skipped in replication."
                            + " Total objects skipped since last sync or restart of central: "
                            + failCount + ". Registry '" + remoteRegistryId
                            + "' will need resynced.");
        } else {
            throw new ConnectException("Lost connection to " + remoteRegistryId);
        }
    }

    /**
     * Updates the record in the registry that keeps track of if this registry
     * has been up. This method is called every minute via a quartz cron
     * configured in Camel
     * 
     * @throws EbxmlRegistryException
     */
    @Transactional
    public void updateUpTime() throws EbxmlRegistryException {
        if (federationEnabled && initialized.get() && EDEXUtil.isRunning()) {
            long currentTime = TimeUtil.currentTimeMillis();
            long lastKnownUp = federatedRegistryMonitor.getLastKnownUptime();
            long downTime = currentTime - lastKnownUp;
            if (downTime > MAX_DOWN_TIME_DURATION && !centralRegistry) {
                if (!SYNC_IN_PROGRESS.get()) {
                    statusHandler.info("Registry has been down since: "
                            + new Date(currentTime - downTime));
                    statusHandler
                            .warn("Registry is out of sync with federation. Attempting to automatically synchronize");
                    try {
                        synchronize();
                        monitorHandler
                                .info("Registry successfully synchronized!");
                    } catch (EbxmlRegistryException e) {
                        monitorHandler
                                .error("Error synchronizing registry!", e);
                        throw e;
                    }
                }
            } else {
                federatedRegistryMonitor.updateTime();
            }
        }
    }

    public void deleteExpiredEvents() {
        statusHandler
                .info("Purging expired or processed replication events...");
        List<String> registries = getExpiredSites();

        /*
         * Stop tracking events for all registries that have events that are
         * expiring. If events are expiring, then that registry has exceeded
         * it's max downtime and will need to synchronize.
         */
        for (String registryId : registries) {
            stopTrackingEventsForSite(registryId);
        }

        /*
         * Clean up any events that no longer need to replicate to anywhere.
         * Delete the actual events here as to avoid issues with threading. This
         * could get quite large, so break the delete up into batches.
         */
        int deleteBatchSize = 1000;
        int idsDeleted = 0;
        do {
            try {
                idsDeleted = deleteEventsBatch(deleteBatchSize);
            } catch (DataAccessLayerException e) {
                statusHandler
                        .error("Error occurred deleting expired events", e);
            }
            statusHandler.info("Deleted " + idsDeleted + " events");
        } while (idsDeleted == deleteBatchSize);
        statusHandler.info("Finished purging replication events");
    }

    public List<String> getExpiredSites() {
        Calendar gmtTime = TimeUtil.newGmtCalendar();
        /* Find all registries associated with the events to be purged. */
        List<String> registries = replicationSiteEventDao
                .getExpiredEventSites((gmtTime.getTimeInMillis() - MAX_DOWN_TIME_DURATION));
        return registries;
    }

    public void stopTrackingEventsForSite(String registryId) {
        statusHandler.info("No longer tracking events for registry ["
                + registryId + "] due to exceeding maximum downtime. "
                + "Registry [" + registryId + "] must synchronize.");
        replicateToServers.removeReplicationServer(registryId);
        saveNotificationServers();
        try {
            replicationSiteEventDao.clearSiteMappings(registryId);
        } catch (DataAccessLayerException e) {
            statusHandler.error(
                    "Error ocurred removing replication events for "
                            + registryId, e);
        }
    }

    public int deleteEventsBatch(int deleteBatchSize)
            throws DataAccessLayerException {
        List<Long> deleteIds = replicationEventDao
                .getReplicatedEvents(deleteBatchSize);
        int idsDeleted = deleteIds.size();
        if (idsDeleted > 0) {
            replicationEventDao.deleteEvents(deleteIds);
        }
        return idsDeleted;
    }

    @Transactional
    public void verifyReplication() throws Exception {
        if (federationEnabled
                && replicateToServers != null
                && !replicateToServers.getRegistryReplicationServers()
                        .isEmpty()) {
            statusHandler
                    .info("Verifying replication with the following registries: "
                            + replicateToServers
                                    .getRegistryReplicationServers() + "...");
            for (String registryId : replicateToServers
                    .getRegistryReplicationServers()) {
                verifyReplicationFor(registryId
                        + FederationProperties.REGISTRY_SUFFIX);
            }
            statusHandler.info("Replication verification complete");
        }
    }

    private void verifyReplicationFor(String registryId) throws Exception {
        statusHandler.info("Verifying replication with [" + registryId + "]");
        RegistryType remoteRegistry = registryDao.getById(registryId);
        if (remoteRegistry != null) {
            String replicatingTo = restClient.getRegistryFederationManager(
                    remoteRegistry.getBaseURL()).getReplicatingTo();
            if (replicatingTo.contains(RegistryIdUtil.getId())) {
                statusHandler.info("Successfully verified replication with ["
                        + registryId + "]");
            } else {
                statusHandler.info("Establishing replication with ["
                        + registryId + "]...");
                subscribeToRegistry(registryId);
                statusHandler.info("Replication with [" + registryId
                        + "] established");
            }
        } else {
            statusHandler.warn("Registry [" + registryId
                    + "] has not established a connection yet. "
                    + "Replication cannot be established");
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

    public void setReplicationSiteEventDao(
            ReplicationSiteEventDao replicationSiteEventDao) {
        this.replicationSiteEventDao = replicationSiteEventDao;
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

    public void setGatherThreadPool(Executor gatherThreadPool) {
        this.gatherThreadPool = gatherThreadPool;
    }

    public NotificationServers getServers() {
        return replicateToServers;
    }

    /**
     * @param registryUsers
     *            the registryUsers to set
     */
    public void setRegistryUsers(RegistryUsers registryUsers) {
        this.registryUsers = registryUsers;
    }

    /**
     * @param restClient
     *            the restClient to set
     */
    public void setRestClient(RegistryRESTServices restClient) {
        this.restClient = restClient;
    }

    /**
     * @param securityConfig
     *            the securityConfig to set
     */
    public void setSecurityConfig(SecurityConfiguration securityConfig) {
        this.securityConfig = securityConfig;
    }
}
