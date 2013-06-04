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
package com.raytheon.uf.edex.registry.ebxml.services.notification;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.xml.bind.JAXBException;
import javax.xml.datatype.Duration;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.dom.DOMResult;
import javax.xml.ws.wsaddressing.W3CEndpointReference;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DateTimeValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DeliveryInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.google.common.collect.Lists;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.registry.constants.DeliveryMethodTypes;
import com.raytheon.uf.common.registry.constants.Namespaces;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.event.InsertRegistryEvent;
import com.raytheon.uf.common.registry.event.RemoveRegistryEvent;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.SubscriptionDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.IRegistrySubscriptionManager;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * Class to manage registry replication subscriptions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/9/2013     1802        bphillip    Initial implementation
 * Apr 17, 2013 1672        djohnson    Keeps track of the notification listeners.
 * 5/21/2013    2022        bphillip    Made logging less verbose. added running boolean so subscriptions are not process on every single
 *                                      event.
 * 6/4/2013     2022        bphillip    Changed slot type of subscription last run time. Longs were being truncated when casting to ints
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Transactional
@Component
public class RegistrySubscriptionManager implements
        IRegistrySubscriptionManager {

    /** The logger instance */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistrySubscriptionManager.class);

    /** Boolean used to denote if subscriptions should be processed */
    private AtomicBoolean running = new AtomicBoolean(false);

    /**
     * Associates a {@link SubscriptionType} with its
     * {@link NotificationListenerWrapper} instances.
     */
    static final class SubscriptionNotificationListeners {
        public final SubscriptionType subscription;

        public final List<NotificationListenerWrapper> listeners;

        private SubscriptionNotificationListeners(
                SubscriptionType subscription,
                List<NotificationListenerWrapper> listeners) {
            this.subscription = subscription;
            this.listeners = listeners;
        }
    }

    /**
     * Associates a {@link NotificationListener} with its address.
     */
    static final class NotificationListenerWrapper {
        public final String address;

        public final NotificationListener notificationListener;

        private NotificationListenerWrapper(String address,
                NotificationListener notificationListener) {
            this.address = address;
            this.notificationListener = notificationListener;
        }
    }

    /**
     * If a subscription does not specify a notification interval, wait this
     * amount of milliseconds before processing the subscription again
     */
    public static final long DEFAULT_WAIT_TIME = 30000;

    /** The XML address tag */
    public static final String ADDRESS_TAG = "Address";

    /** The XML endpointType tag */
    public static final String ENDPOINT_TAG = "endpointType";

    /** Status of whether subscriptions are being processed */
    private boolean subscriptionProcessingEnabled;

    /** The notification manager */
    private RegistryNotificationManager notificationManager;

    /** The local query manager */
    private QueryManagerImpl queryManager;

    /** Data access object for subscription objects */
    private SubscriptionDao subscriptionDao;

    private INotificationListenerFactory notificationListenerFactory;

    private final ConcurrentMap<String, SubscriptionNotificationListeners> listeners = new ConcurrentHashMap<String, SubscriptionNotificationListeners>();

    public RegistrySubscriptionManager() {

    }

    public RegistrySubscriptionManager(boolean subscriptionProcessingEnabled)
            throws JAXBException {
        this.subscriptionProcessingEnabled = subscriptionProcessingEnabled;
    }

    /**
     * Adds subscription notification listeners for any subscriptions.
     */
    @Subscribe
    public void addSubscriptionNotificationListeners(InsertRegistryEvent re) {
        final String objectType = re.getObjectType();

        if (RegistryObjectTypes.SUBSCRIPTION.equals(objectType)) {
            final String id = re.getId();
            try {
                final SubscriptionType subscription = subscriptionDao
                        .eagerGetById(id);
                final List<NotificationListenerWrapper> subscriptionListeners = getNotificationListenersForSubscription(subscription);
                listeners.put(id, new SubscriptionNotificationListeners(
                        subscription, subscriptionListeners));
            } catch (EbxmlRegistryException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * Removes subscription notification listeners for any subscriptions.
     */
    @Subscribe
    public void removeSubscriptionNotificationListeners(RemoveRegistryEvent re) {
        final String objectType = re.getObjectType();

        if (RegistryObjectTypes.SUBSCRIPTION.equals(objectType)) {
            listeners.remove(re.getId());
        }
    }

    /**
     * Get the notification listeners for a subscription.
     * 
     * @param subscription
     *            the subscription
     * @return the list of listeners
     * @throws EbxmlRegistryException
     *             on error
     */
    protected List<NotificationListenerWrapper> getNotificationListenersForSubscription(
            final SubscriptionType subscription) throws EbxmlRegistryException {
        try {
            List<NotificationListenerWrapper> listeners = Lists.newArrayList();
            // Get the list of destinations for the notifications
            final List<NotificationDestination> destinations = getNotificationDestinations(subscription);
            if (destinations.isEmpty()) {
                statusHandler.warn("No destinations found for notification!");
            } else {
                for (NotificationDestination destination : destinations) {
                    listeners
                            .add(new NotificationListenerWrapper(
                                    destination.getDestination(),
                                    notificationListenerFactory
                                            .getNotificationListenerForDestination(destination)));
                }
            }
            return listeners;
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error extracting service addresses from subscription delivery info!",
                    e);
        }
    }

    /**
     * Extracts where the notifications are to be sent from the subscription
     * object
     * 
     * @param subscription
     *            The subscriptions to get the delivery information from
     * @return The list of destinations for the notifications
     * @throws Exception
     *             If errors occur while extracting the destinations
     */
    public List<NotificationDestination> getNotificationDestinations(
            final SubscriptionType subscription) throws EbxmlRegistryException {
        List<NotificationDestination> addresses = new ArrayList<NotificationDestination>();

        List<DeliveryInfoType> deliveryInfos = subscription.getDeliveryInfo();
        try {
            for (DeliveryInfoType deliveryInfo : deliveryInfos) {
                W3CEndpointReference endpointReference = deliveryInfo
                        .getNotifyTo();
                DOMResult dom = new DOMResult();
                endpointReference.writeTo(dom);
                Document doc = (Document) dom.getNode();
                NodeList nodes = doc.getElementsByTagNameNS(
                        Namespaces.ADDRESSING_NAMESPACE,
                        RegistrySubscriptionManager.ADDRESS_TAG);
                Node addressNode = nodes.item(0);
                String serviceAddress = addressNode.getTextContent().trim();
                String endpointType = addressNode
                        .getAttributes()
                        .getNamedItemNS(Namespaces.EBXML_RIM_NAMESPACE_URI,
                                RegistrySubscriptionManager.ENDPOINT_TAG)
                        .getNodeValue();
                final NotificationDestination destination = new NotificationDestination(
                        endpointType, serviceAddress);
                if (endpointType.equals(DeliveryMethodTypes.EMAIL)) {
                    destination
                            .setEmailNotificationFormatter((String) deliveryInfo
                                    .getSlotValue(EbxmlObjectUtil.EMAIL_NOTIFICATION_FORMATTER_SLOT));
                }
                addresses.add(destination);
            }
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error getting destinations from subscription!", e);
        }
        return addresses;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processSubscriptions() {
        if (!subscriptionProcessingEnabled) {
            return;
        }
        if (!running.compareAndSet(false, true)) {
            return;
        }
        try {
            long start = TimeUtil.currentTimeMillis();

            Collection<SubscriptionNotificationListeners> subs = listeners
                    .values();

            for (SubscriptionNotificationListeners subNotificationListener : subs) {
                SubscriptionType sub = subNotificationListener.subscription;
                try {
                    if (subscriptionShouldRun(sub)) {
                        try {
                            processSubscription(subNotificationListener);
                        } catch (EbxmlRegistryException e) {
                            statusHandler.error(
                                    "Errors occurred while processing subscription ["
                                            + sub.getId() + "]", e);
                        } catch (MsgRegistryException e) {
                            statusHandler.error(
                                    "Errors occurred while processing subscription ["
                                            + sub.getId() + "]", e);
                        }
                    } else {
                        statusHandler
                                .info("Skipping subscription ["
                                        + sub.getId()
                                        + "]. Required notification frequency interval has not elapsed.");
                    }
                } catch (EbxmlRegistryException e) {
                    statusHandler.error(
                            "Error processing subscription [" + sub.getId()
                                    + "]", e);
                }
            }
            if (!subs.isEmpty()) {
                statusHandler.info("Registry subscriptions processed in "
                        + (TimeUtil.currentTimeMillis() - start) + " ms.");
            }
        } finally {
            running.set(false);
        }
    }

    /**
     * Checks if this subscription should run based on the notification interval
     * specified in the subscription. If no notification interval is specified,
     * the default wait time is used
     * 
     * @param subscription
     *            The subscription to check
     * @return True if this subscription should be run
     * @throws EbxmlRegistryException
     *             If errors occur getting the last run time
     */
    private boolean subscriptionShouldRun(SubscriptionType subscription)
            throws EbxmlRegistryException {
        Calendar subRunTime = getLastRunTime(subscription);
        long timeSinceLastRun = TimeUtil.currentTimeMillis()
                - subRunTime.getTimeInMillis();
        long mandatoryWaitTime = DEFAULT_WAIT_TIME;
        Duration duration = subscription.getNotificationInterval();
        if (duration != null) {
            mandatoryWaitTime = duration.getTimeInMillis(subRunTime);
        }
        return timeSinceLastRun >= mandatoryWaitTime;
    }

    /**
     * Updates the last run time of the subscription on a slot on the
     * subscription object
     * 
     * @param subscription
     *            The subscription to update
     * @param time
     *            The time at which the subscription was run
     * @throws EbxmlRegistryException
     *             if errors occur accessing the slot on the subscription
     */
    private void updateLastRunTime(SubscriptionType subscription, long time)
            throws EbxmlRegistryException {
        try {
            SlotType lastRunTimeSlot = subscription
                    .getSlotByName(EbxmlObjectUtil.SUBSCRIPTION_LAST_RUN_TIME_SLOT_NAME);
            if (lastRunTimeSlot == null) {
                lastRunTimeSlot = new SlotType(
                        EbxmlObjectUtil.SUBSCRIPTION_LAST_RUN_TIME_SLOT_NAME,
                        new DateTimeValueType(time));
                subscription.getSlot().add(lastRunTimeSlot);
            } else {
                lastRunTimeSlot.setSlotValue(new DateTimeValueType(time));
            }
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error getting subscription run time", e);
        }
        subscriptionDao.createOrUpdate(subscription);
    }

    /**
     * Gets the time at which the subscription was last run
     * 
     * @param subscription
     *            The subscription to check
     * @return The time at which the subscription was last run
     * @throws EbxmlRegistryException
     *             If errors occur accessing the slot on the subscription
     */
    private Calendar getLastRunTime(SubscriptionType subscription)
            throws EbxmlRegistryException {
        XMLGregorianCalendar lastRunTime = subscription
                .getSlotValue(EbxmlObjectUtil.SUBSCRIPTION_LAST_RUN_TIME_SLOT_NAME);
        if (lastRunTime == null) {
            updateLastRunTime(subscription, 0);
            lastRunTime = EbxmlObjectUtil.getTimeAsXMLGregorianCalendar(0);
        }
        return lastRunTime.toGregorianCalendar();
    }

    /**
     * Gets the objects of interest based on the subscription and delegates to
     * the notification manager to create and send notifications based on the
     * list of objects
     * 
     * @param subscription
     *            The subscription to process
     * @throws MsgRegistryException
     * @throws EbxmlRegistryException
     */
    private void processSubscription(
            final SubscriptionNotificationListeners subscriptionNotificationsListeners)
            throws MsgRegistryException, EbxmlRegistryException {
        updateLastRunTime(subscriptionNotificationsListeners.subscription,
                TimeUtil.currentTimeMillis());
        SubscriptionType subscription = subscriptionNotificationsListeners.subscription;
        statusHandler.info("Processing subscription [" + subscription.getId()
                + "]...");

        List<ObjectRefType> objectsOfInterest = getObjectsOfInterest(subscriptionNotificationsListeners.subscription);
        if (!objectsOfInterest.isEmpty()) {
            notificationManager.sendNotifications(
                    subscriptionNotificationsListeners, objectsOfInterest);
        }

    }

    public NotificationType getOnDemandNotification(String address,
            String subscriptionId, XMLGregorianCalendar startTime)
            throws MsgRegistryException, EbxmlRegistryException {
        SubscriptionNotificationListeners subscriptionListener = listeners
                .get(subscriptionId);
        SubscriptionType subscription = subscriptionListener.subscription;
        List<ObjectRefType> objectsOfInterest = getObjectsOfInterest(subscription);
        List<AuditableEventType> eventsOfInterest = notificationManager
                .getEventsOfInterest(startTime, null, objectsOfInterest);
        NotificationType notification = notificationManager.getNotification(
                subscription, address, objectsOfInterest, eventsOfInterest);
        notificationManager.saveNotification(notification);
        return notification;
    }

    private List<ObjectRefType> getObjectsOfInterest(
            SubscriptionType subscription) throws MsgRegistryException {
        // Get objects that match selector query
        QueryType selectorQuery = subscription.getSelector();
        ResponseOptionType responseOption = EbxmlObjectUtil.queryObjectFactory
                .createResponseOptionType();
        responseOption.setReturnType(QueryReturnTypes.OBJECT_REF);
        QueryResponse queryResponse = queryManager.executeQuery(responseOption,
                selectorQuery);
        return queryResponse.getObjectRefList().getObjectRef();
    }

    public void setQueryManager(QueryManagerImpl queryManager) {
        this.queryManager = queryManager;
    }

    public void setSubscriptionDao(SubscriptionDao subscriptionDao) {
        this.subscriptionDao = subscriptionDao;
    }

    public void setNotificationManager(
            RegistryNotificationManager notificationManager) {
        this.notificationManager = notificationManager;
    }

    /**
     * @return the notificationListenerFactory
     */
    public INotificationListenerFactory getNotificationListenerFactory() {
        return notificationListenerFactory;
    }

    /**
     * @param notificationListenerFactory
     *            the notificationListenerFactory to set
     */
    public void setNotificationListenerFactory(
            INotificationListenerFactory notificationListenerFactory) {
        this.notificationListenerFactory = notificationListenerFactory;
    }
}
