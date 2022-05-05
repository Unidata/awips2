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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.common.registry.services.rest.IRegistryFederationManager;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.ClusterIdUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
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
import com.raytheon.uf.edex.registry.ebxml.util.PurgeOrphanedRegObjectSlots;
import com.raytheon.uf.edex.registry.ebxml.util.RegistryIdUtil;
import com.raytheon.uf.edex.registry.events.CreateAuditTrailEvent;
import com.raytheon.uf.edex.registry.federation.FederatedRegistryMonitor;
import com.raytheon.uf.edex.registry.federation.FederationDbInit;
import com.raytheon.uf.edex.registry.federation.FederationProperties;
import com.raytheon.uf.edex.registry.federation.RegistryNotFoundException;
import com.raytheon.uf.edex.registry.federation.ReplicationCache;
import com.raytheon.uf.edex.registry.federation.ReplicationEvent;
import com.raytheon.uf.edex.registry.federation.ReplicationJob;
import com.raytheon.uf.edex.registry.federation.ReplicationSiteEvent;
import com.raytheon.uf.edex.registry.synchronization.SynchronizationTask;
import com.raytheon.uf.edex.security.SecurityConfiguration;

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

/**
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
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to
 *                                  LocalizationException
 * Feb 02, 2016  5388     dhladky   Improve performance, multi-threaded replication.
 * Feb 08, 2016  5198     dhladky   Class cast for String expecting Long fixed
 * Apr 20, 2016  5386     tjensen   Improve performance, multi-threaded synchronization.
 * Apr 27, 2016  5611     dhladky   Improve performance, multi-threaded gather.
 * May 11, 2016  5638     tjensen   Improve registry event replication
 * May 31, 2016  5659     dhladky   Don't send empty events, Don't kill thread
 *                                  with Runtime Exception. Continue attempts to
 *                                  connect if gather fails.
 * Jul 15, 2016  5744     mapeters  Config files moved from edex_static to
 *                                  common_static
 * Aug 05, 2016  5810     tjensen   Refactor replication
 * Aug 16, 2016  5810     tjensen   Clean up logging from processEvent
 * Aug 25, 2016  5846     rjpeter   Remove InternationalString from DB. Update replication logging.
 * Sep 01, 2016  5810     tjensen   Added improved logging messages
 * Sep 22, 2016  5762     tjensen   Harden replication code
 * Oct 04, 2016  5762     tjensen   Fix connection check
 * Jul 18, 2018  7239     ksunil    Added queued versions of add/remove objects call.
 * Aug 29, 2018  7238     skabasele Added Automatic registry synchronization
 *
 * Nov 20, 2018  7634     rjpeter   Fix threading and add replication threshold logs
 * Jul 25, 2019  7890     ksunil    Enhanced logging.
 * Jul 25, 2019  7834     skabasele Removed references to replicateToServers and added replicationList
 * Jul 26, 2019  7839     skabasele Added ability to delete orphaned Registry Object slots on startup.
 * Aug 29, 2019  7836     bsteffen  Extract logic for sending replication events.
 * Oct 22, 2019  7952     bsteffen  Fixes so that replication tables aren't used when federation is disabled.
 * May 06, 2020  8066     skabasele Added ability to remove duplicate registry entries on startup
 * Jun 18, 2020  8066     skabasele update removeReplicationserver method to also delete the replication site event
 * Sep 14, 2021  8656     rjpeter   No longer pass txTemplate to ReplicationJob.
 * 
 * </pre>
 *
 * @author bphillip
 */
@Path(IRegistryFederationManager.REGISTRY_FEDERATION_MANAGER_PATH)
@Service
public class RegistryFederationManager
        implements IRegistryFederationManager, RegistryInitializedListener {

    /** The logger instance */
    protected static final Logger statusHandler = LoggerFactory
            .getLogger(RegistryFederationManager.class);

    /** Pool size for registry synchronization thread pool */
    private static final int SYNC_POOL_SIZE = Integer
            .parseInt(System.getProperty("ebxml-federation-sync-threads"));

    /** Max number of seconds to wait for a sync thread to complete */
    private static final int SYNC_POOL_WAIT_TIME = 60_000;

    /**
     * During registry synch , this delay buffer time is used against the
     * current time to calculate the registryObject updatetime that qualifies to
     * be synchronized.
     */

    private static final long SYNC_DELAY = Long.getLong(
            "ebxml.registry.synchDelay", 5) * TimeUtil.MILLIS_PER_MINUTE;

    /**
     * Interval for auto synchronization in ms
     */
    public static final long autoSynchIntervalInMs = Long
            .getLong("ebxml.registry.automaticSynchronizationInternval", 24)
            * TimeUtil.MILLIS_PER_HOUR;

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

    static {
        try {
            /*
             * Compares the ncf.host and ebxml.registry.host properties. If they
             * are the same, then this registry is assumed to be the central
             * registry
             */
            centralRegistry = InetAddress
                    .getByName(System.getProperty("ncf.host"))
                    .equals(InetAddress.getByName(
                            System.getProperty("ebxml.registry.host")));
            if (centralRegistry) {
                statusHandler.info(
                        "This registry instance is configured as a central registry");
            } else {
                statusHandler.info(
                        "This registry instance is not configured as a central registry instance. The central registry instance is located at "
                                + System.getProperty("ncf.host"));
            }
        } catch (UnknownHostException e) {
            String msg = "RegistryFederationManager is unable to determine if this registry is a central registry instance";
            /* Sometimes spring is hiding the caused by so log it here. */
            statusHandler.error(msg, e);
            /* This will stop the class loading */
            throw new RuntimeException(msg, e);
        }
    }

    private static final String FEDERATION_CONFIG_FILE = "ebxml"
            + IPathManager.SEPARATOR + "registry" + IPathManager.SEPARATOR
            + "federationConfig.xml";

    private static FederationProperties federationProperties;

    public static final AtomicBoolean SYNC_IN_PROGRESS = new AtomicBoolean(
            false);

    /**
     * Denotes which instance of this class was initialized. At this time,
     * multiple Spring containers load this class, yet it only needs to be
     * initialized once and accessing this instance allows it to behave more
     * like a singleton.
     */
    private static final AtomicReference<RegistryFederationManager> initializedInstance = new AtomicReference<>();

    private final boolean federationEnabled = Boolean.parseBoolean(
            System.getProperty("ebxml.registry.federation.enabled"));

    private static ReplicationCache replicationCache;

    /**
     * Access must be synchronized on replicationJobs and all jobs should be
     * created from the {@link #initializedInstance}
     */
    private static final Map<String, ReplicationJob> replicationJobs = new HashMap<>();

    /** Queue backed gather thread pool executor **/
    private Executor gatherThreadPool;

    private final String ncfAddress = "https://"
            + (System.getProperty("ncf.host")) + ":"
            + (System.getProperty("ebxml.registry.webserver.port"));

    /** Monitors how long this registry has been connected to the federation */
    private FederatedRegistryMonitor federatedRegistryMonitor;

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

    @Override
    @Transactional
    public void executeAfterRegistryInit() throws EbxmlRegistryException {
        synchronized (initializedInstance) {
            if (federationEnabled && !isInititialized()) {
                try {
                    this.federationDbInit.initDb();
                } catch (Exception e) {
                    throw new EbxmlRegistryException(
                            "Error initializing database for federation!", e);
                }
                try {
                    if (federationProperties == null) {
                        JAXBManager jaxbManager = new JAXBManager(
                                FederationProperties.class);
                        statusHandler
                                .info("Loading Federation Configuration...");
                        ILocalizationFile federationPropertiesFile = PathManagerFactory
                                .getPathManager().getStaticLocalizationFile(
                                        FEDERATION_CONFIG_FILE);
                        if (federationPropertiesFile == null
                                || !federationPropertiesFile.exists()) {
                            throw new FileNotFoundException(
                                    "Unable to locate federation configuration file: "
                                            + FEDERATION_CONFIG_FILE);
                        } else {
                            try (InputStream is = federationPropertiesFile
                                    .openInputStream()) {
                                federationProperties = jaxbManager
                                        .unmarshalFromInputStream(
                                                FederationProperties.class, is);
                            } catch (IOException e) {
                                statusHandler.error(
                                        "Error reading federation configuration ["
                                                + federationPropertiesFile
                                                        .getPath()
                                                + "]",
                                        e);
                            }
                        }
                    }

                    if (!joinFederation()) {
                        throw new EbxmlRegistryException(
                                "Error joining federation!!");
                    }

                } catch (Exception e1) {
                    throw new EbxmlRegistryException(
                            "Error initializing RegistryFederationManager", e1);
                }

                PurgeOrphanedRegObjectSlots purgeOrphanedRegObjectSlots = new PurgeOrphanedRegObjectSlots(
                        registryObjectDao);
                Thread thread = new Thread(purgeOrphanedRegObjectSlots);
                thread.start();

            }

            /*
             * If this is the central registry then we ensure that the superuser
             * is in the registry
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
                        statusHandler.info(REGISTRY_ADMIN
                                + " present in Central Registry: "
                                + REGISTRY_ADMIN);
                    }

                } else if (!centralRegistry) {

                    if (!registryUsers
                            .userExists(securityConfig.getSecurityProperties()
                                    .getProperty(EDEX_SECURITY_AUTH_USER))) {

                        statusHandler.info(EDEX_SECURITY_AUTH_USER
                                + " Not present in Registry! Adding user: "
                                + securityConfig.getSecurityProperties()
                                        .getProperty(EDEX_SECURITY_AUTH_USER));

                        registryUsers.addUser(
                                securityConfig.getSecurityProperties()
                                        .getProperty(EDEX_SECURITY_AUTH_USER),
                                securityConfig.getSecurityProperties()
                                        .getProperty(
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
                throw new EbxmlRegistryException(
                        "Error Checking registry user! ", e);
            }

            initializedInstance.set(this);
            computeRegistryReplicationList();
        }

    }

    public static boolean isInititialized() {
        return initializedInstance.get() != null;
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
                    DbInit.addToInitialDbOjectIdsSet(federation.getId());
                }
                objects.add(registry);
                objects.add(org);
                objects.add(primaryContact);
                objects.add(federationAssociation);

                DbInit.addToInitialDbOjectIdsSet(registry.getId());
                DbInit.addToInitialDbOjectIdsSet(org.getId());
                DbInit.addToInitialDbOjectIdsSet(primaryContact.getId());
                DbInit.addToInitialDbOjectIdsSet(federationAssociation.getId());

                SubmitObjectsRequest submitObjectsRequest = new SubmitObjectsRequest(
                        "Federation Objects submission",
                        "Submitting federation related objects", null,
                        new RegistryObjectListType(objects), false,
                        Mode.CREATE_OR_REPLACE);
                submitObjectsRequest.getSlot()
                        .add(new SlotType(EbxmlObjectUtil.EVENT_SOURCE_SLOT,
                                new StringValueType(RegistryIdUtil.getId())));
                try {

                    statusHandler.info(
                            "Submitting federation registration objects to local registry...");
                    localLifecycleManager.submitObjects(submitObjectsRequest);
                    statusHandler.info(
                            "Successfully submitted federation registration objects to local registry!");

                } catch (MsgRegistryException e) {
                    throw new EbxmlRegistryException(
                            "Error submitting federation objects to local registry",
                            e);
                }
                if (!centralRegistry) {
                    statusHandler.info(
                            "Submitting federation registration objects to NCF (Central Registry)...");
                    try {
                        soapService
                                .getLifecycleManagerServiceForHost(ncfAddress)
                                .submitObjects(submitObjectsRequest);
                        statusHandler.info(
                                "Successfully submitted federation registration objects to NCF (Central Registry)!");
                        subscribeToRegistry("NCF");
                    } catch (MsgRegistryException e) {
                        throw new EbxmlRegistryException(
                                "Error submitting federation objects to NCF (Central Registry)!",
                                e);
                    }
                }

            }
            removeDuplicateRegistry();
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
                refList.getObjectRef()
                        .add(new ObjectRefType(primaryContact.getId()));
                refList.getObjectRef()
                        .add(new ObjectRefType(federationAssociation.getId()));
                RemoveObjectsRequest req = new RemoveObjectsRequest();
                req.setId("Removing [" + registry.getId()
                        + "] from the federation...");
                req.setComment(
                        "Remove request to remove federation related objects");
                req.setDeleteChildren(true);
                req.setObjectRefList(refList);
                statusHandler.info("Disconnecting from federation...");

                try {
                    if (RegistryFederationManager.centralRegistry) {
                        localLifecycleManager.removeObjects(req);

                    } else {
                        soapService
                                .getLifecycleManagerServiceForHost(ncfAddress)
                                .removeObjects(req);
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
     * Method used to remove any occurrence of registry duplication while only
     * keeping the latest registry object present. Duplication is detected when
     * different registry entries have the same base URL.
     */
    private synchronized void removeDuplicateRegistry() {

        statusHandler
                .info("Scanning database for registry info duplication:  ");
        Map<String, List<RegistryType>> registryMap = new HashMap<>();
        List<RegistryType> allRegistries = registryDao.getAll();
        List<RegistryType> allDuplicatedRegistries = new ArrayList<>();
        List<RegistryType> localDuplicatedRegistries = new ArrayList<>();
        String localhostIP = System.getProperty("ebxml.registry.host");

        // build the map with the base url as the key
        for (RegistryType registry : allRegistries) {
            String baseUrl = registry.getBaseURL();
            List<RegistryType> registryValues = registryMap.get(baseUrl);
            if (registryValues != null) {
                registryValues.add(registry);
            } else {
                registryValues = new ArrayList<>();
                registryValues.add(registry);
                registryMap.put(baseUrl, registryValues);
            }

        }

        // using the map to compute the duplicates
        for (Map.Entry<String, List<RegistryType>> entry : registryMap
                .entrySet()) {
            String baseUrl = entry.getKey();
            List<RegistryType> valueRegistries = entry.getValue();
            // find the registry with the latest update .
            if (valueRegistries.size() > 1) {
                // Initially default to the first element
                Date latestDate = valueRegistries.get(0).getUpdateTime();
                for (RegistryType valueRegistry : valueRegistries) {
                    Date currentRegistryDate = valueRegistry.getUpdateTime();
                    // If currentRegistryDate occurs after latestDate
                    if (currentRegistryDate.compareTo(latestDate) > 0) {
                        latestDate = currentRegistryDate;
                    }
                }

                // Now find the registry with old date
                for (RegistryType valueRegistry : valueRegistries) {
                    Date currentRegistryDate = valueRegistry.getUpdateTime();
                    // If currentRegistryDate occurs before latestDate
                    if (currentRegistryDate.compareTo(latestDate) < 0) {
                        allDuplicatedRegistries.add(valueRegistry);
                        // keeping track of only local duplicates here
                        if (baseUrl.contains(localhostIP)) {
                            localDuplicatedRegistries.add(valueRegistry);
                        }

                    }
                }

            }
        }

        // Now delete theses registry from the database.
        if (!allDuplicatedRegistries.isEmpty()) {
            statusHandler.info("Found " + allDuplicatedRegistries.size()
                    + " duplicated registry entries in the local database");
            statusHandler.info(
                    "Deleting the following duplicated registry entries in the local database: ");
            for (RegistryType duplicatedRegistry : allDuplicatedRegistries) {
                statusHandler.info("Deleting registry  "
                        + duplicatedRegistry.getId() + " with url "
                        + duplicatedRegistry.getBaseURL());
                replicationSiteEventDao
                        .deleteByRegistryId(duplicatedRegistry.getOwner());

            }
            registryDao.deleteAll(allDuplicatedRegistries);
            statusHandler.info("Deletion complete in local database ");

        } else {
            statusHandler.info("No registry entry duplication found ");
        }

        if (!centralRegistry && !localDuplicatedRegistries.isEmpty()) {
            RemoveObjectsRequest req = createRemoveObjectsRequest(
                    localDuplicatedRegistries);
            try {
                statusHandler.info(
                        "Deleting the following local duplicated registry entries in the central registry database: ");
                for (RegistryType localDuplicatedRegistry : localDuplicatedRegistries) {
                    statusHandler.info("Deleting from centralRegistry "
                            + localDuplicatedRegistry.getId() + " with url "
                            + localDuplicatedRegistry.getBaseURL());
                    restClient.getRegistryFederationManager(ncfAddress)
                            .removeReplicationServer(
                                    localDuplicatedRegistry.getOwner());
                }

                soapService.getLifecycleManagerServiceForHost(ncfAddress)
                        .removeObjects(req);

                statusHandler.info(
                        "Deletion complete in central registry database ");
            } catch (Exception e) {
                statusHandler.info(
                        "Error Deleting registry entry in central registry database ",
                        e);
            }

        }

    }

    /**
     * Creates a RemoveObjectsRequest object based on the passed List of
     * RegistryObjectType
     * 
     * @param objs
     * @return
     */
    private RemoveObjectsRequest createRemoveObjectsRequest(
            List<RegistryType> objs) {
        ObjectRefListType refList = new ObjectRefListType();
        RemoveObjectsRequest req = new RemoveObjectsRequest();
        for (RegistryObjectType obj : objs) {
            refList.getObjectRef().add(new ObjectRefType(obj.getId()));
        }

        req.setId("Removing registry objects");
        req.setComment("Remove request to remove registry objects");
        req.setDeleteChildren(true);
        req.setObjectRefList(refList);

        return req;
    }

    /**
     * Checks last time the registry has been synchronized with federation.If
     * the registry hasn't synchronized with federation for more than the value
     * expressed in ebxml.registry.automaticSynchronizationInternval (currently
     * set to 24 hours) , the registry is synchronized with one of the
     * federation members.
     *
     * Added Logic(7238): code checks 1) which objects are missing from the
     * site's registry that are on the federation's 2) which objects on the
     * site's registry need to be deleted as they were removed from the
     * federation's 3) which objects on the site's registry need to be
     * replicated up to the federation's 4) if an object is in both the
     * federation and the site , use the latest version of the object.
     *
     * @throws Exception
     */
    private void synchronize() throws EbxmlRegistryException {

        statusHandler.warn("Synchronizing registry with federation...");
        Set<String> replicationSet = getReplicationSet();

        RegistryType registryToSyncFrom = null;
        for (String remoteRegistryId : replicationSet) {
            statusHandler.info(
                    "Checking availability of [" + remoteRegistryId + "]...");
            RegistryType remoteRegistry = null;
            try {
                remoteRegistry = restClient.getRegistryObject(ncfAddress,
                        remoteRegistryId
                                + FederationProperties.REGISTRY_SUFFIX);
            } catch (Exception e) {
                throw new EbxmlRegistryException(
                        "Error getting remote registry object!", e);
            }
            if (remoteRegistry == null) {
                statusHandler.warn("Registry at [" + remoteRegistryId
                        + "] not found in federation. Unable to use as synchronization source.");
            } else if (restClient
                    .isRegistryAvailable(remoteRegistry.getBaseURL())) {
                registryToSyncFrom = remoteRegistry;
                break;
            } else {
                statusHandler.info("Registry at [" + remoteRegistryId
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
                                + registryToSyncFrom.getId() + "]",
                        e);
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
    public void updateRegistryEvents(@PathParam("registryId") String registryId,
            @PathParam("time") String time) {
        long lTime = Long.parseLong(time);
        updateRegistryEvents(registryId, new Date(lTime));
    }

    protected void updateRegistryEvents(String registryId, Date time) {
        if (registryId.endsWith(FederationProperties.REGISTRY_SUFFIX)) {
            registryId = registryId
                    .replace(FederationProperties.REGISTRY_SUFFIX, "");
        }
        try {
            if (replicationSiteEventDao.updateSite(registryId, null, time)) {
                synchronized (replicationJobs) {
                    ReplicationJob task = replicationJobs.get(registryId);
                    if (task != null) {
                        task.reset();
                    }
                }
            }
        } catch (DataAccessLayerException e) {
            statusHandler.warn(
                    "Failed to update registry replication events for {}",
                    registryId, e);
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
    public void synchronizeWithRegistry(
            @PathParam("registryId") String registryId) throws Exception {

        if (SYNC_IN_PROGRESS.compareAndSet(false, true)) {
            try {
                long start = TimeUtil.currentTimeMillis();

                statusHandler.warn(
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
                                    + registryId + "] ",
                            e);
                }
                if (remoteRegistry == null) {
                    throw new EbxmlRegistryException(
                            "Unable to synchronize with [" + registryId
                                    + "]. Registry not found in federation");
                }
                final String remoteRegistryUrl = remoteRegistry.getBaseURL();

                /*
                 * coredao is passed to each thread. It is used to create a
                 * unique hibernate session for each thread since hibernate does
                 * not allow one session to be shared among multiple threads.
                 */
                CoreDao coredao = new CoreDao(
                        DaoConfig.forDatabase(DaoConfig.DEFAULT_DB_NAME));

                ThreadPoolTaskExecutor taskExecutor = new ThreadPoolTaskExecutor();
                taskExecutor.setMaxPoolSize(SYNC_POOL_SIZE);
                taskExecutor.setKeepAliveSeconds(SYNC_POOL_WAIT_TIME);
                taskExecutor.initialize();

                /*
                 * ensuring the we only look into object that have been updated
                 * 5 minutes (SYNC_DELAY) prior the current start of execution
                 */
                Date syncDelayTime = new Date(start - SYNC_DELAY);

                for (final String objectType : replicatedObjectTypes) {

                    taskExecutor.execute(new SynchronizationTask(objectType,
                            remoteRegistryUrl, txTemplate, soapService,
                            restClient, registryObjectDao, syncDelayTime,
                            coredao));
                }
                // Wait for all threads to finished.
                taskExecutor.setWaitForTasksToCompleteOnShutdown(true);
                taskExecutor.shutdown();
                try {
                    taskExecutor.getThreadPoolExecutor().awaitTermination(
                            SYNC_POOL_WAIT_TIME, TimeUnit.SECONDS);
                } catch (IllegalStateException | InterruptedException e) {
                    throw new EbxmlRegistryException(
                            "Error synchronizing with [" + registryId
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
                this.updateRegistryEvents(remoteRegistry.getId(),
                        new Date(start));
                syncMsg.append("Registry synchronization using [")
                        .append(remoteRegistryUrl)
                        .append("] completed successfully in ")
                        .append((TimeUtil.currentTimeMillis() - start))
                        .append(" ms");
                statusHandler.info(syncMsg.toString());
            } catch (Exception e) {
                statusHandler.error(
                        "Could not create registry synchronization objects!",
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
        return RegistryQueryUtil
                .formatArrayString(replicatedObjectTypes.toArray());
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

    /**
     * Determines which registries should receive replication events. A new job
     * is created and scheduled for any registry that does not already have one.
     * If a registry has left the federation this will cancel its replication
     * job.
     */
    private synchronized void computeRegistryReplicationList() {
        if (!isInititialized() || !federationEnabled) {
            return;
        }
        if (replicationCache == null) {
            replicationCache = new ReplicationCache(replicationEventDao,
                    registryObjectDao);
        }

        Set<String> replicate = new HashSet<>();

        if (centralRegistry) {
            List<RegistryType> registries = registryDao.getAll();
            for (RegistryType registry : registries) {
                String owner = registry.getOwner();
                if (!"NCF".equals(owner)) {
                    replicate.add(owner);

                }
            }
        } else {
            replicate.add("NCF");

        }

        synchronized (replicationJobs) {
            Set<String> cancel = new TreeSet<>(replicationJobs.keySet());
            cancel.removeAll(replicate);
            if (!cancel.isEmpty()) {
                statusHandler.info(
                        "Canceling replication to the following registries: {}",
                        cancel.stream().collect(Collectors.joining(", ")));
            }
            for (String registryId : cancel) {
                replicationJobs.remove(registryId);
            }

            Date now = new Date();
            Set<String> create = new TreeSet<>(replicate);
            create.removeAll(replicationJobs.keySet());
            if (!create.isEmpty()) {
                statusHandler.info(
                        "Initiating replication to the following registries: {}",
                        create.stream().collect(Collectors.joining(", ")));
            }
            for (String registryId : create) {
                ReplicationSiteEvent siteEvent = replicationSiteEventDao
                        .getById(registryId);
                if (siteEvent == null) {
                    replicationSiteEventDao
                            .create(new ReplicationSiteEvent(registryId, now));
                }
                ReplicationJob task = initializedInstance.get()
                        .createReplicationJob(registryId);
                replicationJobs.put(registryId, task);
                task.schedule();
            }
        }
    }

    /**
     * Create a new replication job. This method should only be called on the
     * {@link #initializedInstance} of this class to ensure that the txTemplate
     * and DAO objects used by the job are the same as those used in the
     * ReplicationCache.
     * 
     * @param remoteRegistryId
     */
    protected ReplicationJob createReplicationJob(String remoteRegistryId) {
        return new ReplicationJob(remoteRegistryId, replicationSiteEventDao,
                registryDao, soapService, restClient, gatherThreadPool,
                replicationCache);
    }

    @Override
    @GET
    @Path("getReplicatingTo")
    @Transactional
    public String getReplicatingTo() {
        return RegistryQueryUtil
                .formatArrayString(getReplicationSet().toArray());
    }

    protected Set<String> getReplicationSet() {
        synchronized (replicationJobs) {
            /* Copy to a new set to allow thread safe access. */
            return new HashSet<>(replicationJobs.keySet());
        }
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
                remoteReplicatingTo = restClient
                        .getRegistryFederationManager(registry.getBaseURL())
                        .getReplicatingTo();
            } catch (Exception e) {
                statusHandler.error("Error getting replication list from ["
                        + registry.getId() + "]", e);
                continue;
            }
            if (remoteReplicatingTo.contains(RegistryIdUtil.getId())) {
                registrySet.add(registry.getId()
                        .replace(FederationProperties.REGISTRY_SUFFIX, ""));
            }
        }
        return RegistryQueryUtil.formatArrayString(registrySet.toArray());
    }

    @Override
    @GET
    @Path("subscribeToRegistry/{registryId}")
    @Transactional
    public void subscribeToRegistry(@PathParam("registryId") String registryId)
            throws Exception {
        statusHandler
                .info("Establishing replication with [" + registryId + "]...");
        RegistryType remoteRegistry = getRegistry(registryId);
        restClient.getRegistryFederationManager(remoteRegistry.getBaseURL())
                .addReplicationServer(RegistryIdUtil.getId());
        statusHandler.info("Established replication with [" + registryId + "]");

    }

    @Override
    @GET
    @Path("unsubscribeFromRegistry/{registryId}")
    @Transactional
    public void unsubscribeFromRegistry(
            @PathParam("registryId") String registryId) throws Exception {
        statusHandler
                .info("Disconnecting replication with [" + registryId + "]...");
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
    public synchronized void addReplicationServer(
            @PathParam("registryId") String registryId) throws Exception {
        computeRegistryReplicationList();
        synchronized (replicationJobs) {
            /*
             * If the remote registry was down before this call it needs to be
             * scheduled to catch up with replication.
             * computeRegistryReplicationList will not schedule if a job already
             * existed.
             */
            ReplicationJob job = replicationJobs.get(registryId);
            if (job != null) {
                job.schedule();
            } else {
                statusHandler.warn("No replication job found for {}",
                        registryId);
            }

        }
    }

    @Override
    @GET
    @Path("removeReplicationServer/{registryId}")
    @Transactional
    public synchronized void removeReplicationServer(
            @PathParam("registryId") String registryId) {
        computeRegistryReplicationList();
        replicationSiteEventDao.deleteByRegistryId(registryId);
    }

    /**
     * Appends registry information to the stringbuilder
     *
     * @param registry
     *            The registry to get information for
     * @param builder
     *            The string builder to append to
     */
    private void appendRegistryInfo(RegistryType registry,
            StringBuilder builder) {
        builder.append(registry.getId()
                .replace(FederationProperties.REGISTRY_SUFFIX, "")).append(",");
        builder.append(registry.getBaseURL()).append(",");
        builder.append(registry.getConformanceProfile()).append(",");
        builder.append(registry.getSpecificationVersion());
        builder.append(StringUtil.NEWLINE);
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
            throw new EbxmlRegistryException(
                    "Error retrieving registry [" + registryId + "] from NCF",
                    e);
        }
        if (registry == null) {
            throw new RegistryNotFoundException(
                    "Registry [" + registryId + "] not found in federation");
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
            federation.setReplicationSyncLatency(
                    RegistryFederationManager.federationProperties
                            .getFederationReplicationSyncLatency());
        } else {
            statusHandler
                    .info("Retrieving Federation object from NCF registry at ["
                            + ncfAddress + "]...");
            federation = restClient.getRegistryObject(ncfAddress,
                    FEDERATION_ID);
            statusHandler
                    .info("Federation object successfully retrieved from NCF!");
        }
        return federation;

    }

    @Subscribe
    @Transactional(propagation = Propagation.REQUIRED)
    public void processEvent(CreateAuditTrailEvent event) {
        statusHandler.info("Processing Event id:[" + event.getId() + "]");
        String sourceRegistry = event.getRequest()
                .getSlotValue(EbxmlObjectUtil.EVENT_SOURCE_SLOT);
        String actionType = event.getActionType();
        List<RegistryObjectType> objsAffected = event.getObjectsAffected();
        boolean needsReplication = false;
        for (String registryId : getReplicationSet()) {
            if (!registryId.equals(sourceRegistry)) {
                needsReplication = true;
                break;
            }
        }
        if (!needsReplication) {
            if (centralRegistry) {
                statusHandler.info(
                        "Event from {} dropped due to no valid targets to replicate to.",
                        sourceRegistry);
            }
            return;
        }
        statusHandler.info("{} Affected objects need to be replicated.",
                objsAffected.size());
        for (RegistryObjectType affectedObj : objsAffected) {
            if (replicatedObjectTypes.contains(affectedObj.getObjectType())) {
                ReplicationEvent replicationEvent = new ReplicationEvent(
                        actionType, new Date(event.getEventTime()),
                        affectedObj.getId(), affectedObj.getObjectType(),
                        sourceRegistry);
                replicationEventDao.create(replicationEvent);
                replicationCache.addLiveEvent(replicationEvent, affectedObj);

            }
        }
        if (federationEnabled && DbInit.isDbInitialized() && isInititialized()
                && !SYNC_IN_PROGRESS.get()) {
            synchronized (replicationJobs) {
                for (ReplicationJob task : replicationJobs.values()) {
                    task.schedule();
                }
            }
        }
    }

    /**
     * 
     * 
     * statusHandler.info("Event Ids size:[" + eventIds.size() + "]");
     * 
     * 
     * Updates the record in the registry that keeps track of if this registry
     * has been synchronized. This method is called every hour via a quartz cron
     * configured in Camel
     *
     * @throws EbxmlRegistryException
     */
    @Transactional
    public void updateUpTime() throws EbxmlRegistryException {

        if (!centralRegistry) {
            if (federationEnabled && isInititialized()
                    && EDEXUtil.isRunning()) {
                long currentTime = TimeUtil.currentTimeMillis();
                long lastKnownUp = federatedRegistryMonitor
                        .getLastKnownSynchronizedtime();

                statusHandler.debug("autoSynchIntervalInMs is "
                        + autoSynchIntervalInMs + " ms");

                long downTime = currentTime - lastKnownUp;

                if (downTime > autoSynchIntervalInMs) {

                    if (!SYNC_IN_PROGRESS.get()) {
                        statusHandler.info(
                                "Registry has not been synchronized since: "
                                        + new Date(currentTime - downTime));
                        statusHandler.warn(
                                "Registry may be out of sync with federation. Attempting to automatically synchronize");
                        try {
                            synchronize();
                            statusHandler.info(
                                    "Registry successfully synchronized!");
                        } catch (EbxmlRegistryException e) {
                            statusHandler.error("Error synchronizing registry!",
                                    e);
                            throw e;
                        }
                    }
                } else {

                    Date date = new Date(lastKnownUp);
                    SimpleDateFormat dt = new SimpleDateFormat(
                            "yyyy-MM-dd HH:mm:ss");
                    String dateText = dt.format(date);
                    statusHandler
                            .info("** Registry last synchronized at " + dateText
                                    + "  , no synchronization necessary. ***");
                }

            }
        }
    }

    @Transactional
    public void deleteExpiredEvents() {
        if (!federationEnabled) {
            return;
        }
        statusHandler
                .info("Purging expired or processed replication events...");
        Date oldestRegistry = new Date();

        List<ReplicationSiteEvent> registries = replicationSiteEventDao
                .getAll();
        for (ReplicationSiteEvent registry : registries) {
            if (registry.needsSync()) {
                String registryId = registry.getRegistryId();
                synchronized (replicationJobs) {
                    if (replicationJobs.containsKey(registryId)) {
                        statusHandler.error(
                                "{} has exceeded the maximum downtime and must synchronize.",
                                registryId);
                    } else {
                        replicationSiteEventDao.delete(registry);
                        statusHandler.info(
                                "Clearing replication site event for {}",
                                registryId);
                    }
                }
            } else {
                Date registryTime = registry.getEventTime();
                if (registryTime.before(oldestRegistry)) {
                    oldestRegistry = registryTime;
                }
            }
        }

        try {
            int count = replicationEventDao.deleteExpiredEvents(oldestRegistry);
            statusHandler.info("Deleted {} replication events before {}", count,
                    oldestRegistry);
        } catch (DataAccessLayerException e) {
            statusHandler.error("Error occurred deleting expired events", e);
        }

        statusHandler.info("Finished purging replication events");
    }

    public static void addObjectTypesToSubscribeTo(String... types) {
        for (String type : types) {
            if (!replicatedObjectTypes.contains(type)) {
                replicatedObjectTypes.add(type);
                statusHandler
                        .info("Add object type for replication [" + type + "]");
            }
        }
    }

    public void setReplicationEventDao(
            ReplicationEventDao replicationEventDao) {
        this.replicationEventDao = replicationEventDao;
    }

    public void setReplicationSiteEventDao(
            ReplicationSiteEventDao replicationSiteEventDao) {
        this.replicationSiteEventDao = replicationSiteEventDao;
    }

    public void setRegistryDao(RegistryDao registryDao) {
        this.registryDao = registryDao;
    }

    public void setSoapService(RegistrySOAPServices soapService) {
        this.soapService = soapService;
    }

    public void setTxTemplate(TransactionTemplate txTemplate) {
        this.txTemplate = txTemplate;
    }

    public void setLocalLifecycleManager(
            LifecycleManager localLifecycleManager) {
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
