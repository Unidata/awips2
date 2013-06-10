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
import java.util.List;
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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

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
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.RegistryRESTServices;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryReplicationManager {

    /** The logger instance */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryReplicationManager.class);

    /** Scheduler service for monitoring subscription submission tasks */
    private ScheduledExecutorService scheduler;

    /** The servers that we are subscribing to */
    private NotificationServers servers;

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
            String notificationServerConfigFileName) throws JAXBException,
            SerializationException {
        this.subscriptionProcessingEnabled = subscriptionProcessingEnabled;
        this.replicationConfigFileName = notificationServerConfigFileName;
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
    public void submitRemoteSubscriptions(String baseURL) {

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
                scheduleSubscriptionSubmission(config, baseURL);
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
            NotificationHostConfiguration config, String baseURL) {
        final SubmitSubscriptionTask submitSubscriptionTask = new SubmitSubscriptionTask(
                config, baseURL);
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
            NotificationHostConfiguration config, String localRegistryBaseURL) {
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
                        objectType, localRegistryBaseURL);
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
            String localRegistryBaseURL) throws Exception {
        // Set normal registry object fields
        String subscriptionDetail = "Replication Subscription for ["
                + objectType + "] objects for server [" + localRegistryBaseURL
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
        sub.setOwner("Subscription Owner");
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
                localRegistryBaseURL).toString());
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

        /** The base URL of the remote registry */
        private String baseURL;

        /**
         * Creates a new SubmitSubscriptionTask
         * 
         * @param config
         *            The server configuration
         * @param baseURL
         *            The base URL of the remote registry
         */
        public SubmitSubscriptionTask(NotificationHostConfiguration config,
                String baseURL) {
            this.config = config;
            this.baseURL = baseURL;
        }

        @Override
        public void run() {
            if (!success) {
                String remoteRegistryBaseURL = config.getRegistryBaseURL();
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
                    submitSubscriptionsToHost(config, baseURL);
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
