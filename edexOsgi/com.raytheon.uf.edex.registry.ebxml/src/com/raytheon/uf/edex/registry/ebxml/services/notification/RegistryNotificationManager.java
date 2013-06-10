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

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.soap.SOAPException;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ActionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.NotificationTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.notification.RegistrySubscriptionManager.NotificationListenerWrapper;
import com.raytheon.uf.edex.registry.ebxml.services.notification.RegistrySubscriptionManager.SubscriptionNotificationListeners;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * Class used to manage and send registry replication notifications
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/9/2013     1802        bphillip    Initial Coding
 * 4/15/2013    1905        bphillip    Implemented notification to email endpoints
 * Apr 17, 2013 1672        djohnson    No longer cares about notification protocol.
 * 5/21/2013    2022        bphillip    Cleaned up unused method parameters and batching of notifications
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Transactional
public class RegistryNotificationManager {

    /** The log handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryNotificationManager.class);

    /** Data access object for auditable events */
    private AuditableEventTypeDao auditableEventDao;

    /** Data access object for notifications */
    private NotificationTypeDao notificationDao;

    /**
     * Creates a new RegistryNotificationManager
     * 
     * @throws JAXBException
     * @throws SOAPException
     * @throws ParserConfigurationException
     */
    public RegistryNotificationManager() {
    }

    protected List<AuditableEventType> getEventsOfInterest(
            XMLGregorianCalendar startTime, XMLGregorianCalendar endTime,
            List<ObjectRefType> objectsOfInterest) {
        return this.auditableEventDao.getEventsOfInterest(startTime, endTime,
                objectsOfInterest);
    }

    protected List<AuditableEventType> getEventsOfInterest(
            SubscriptionType subscription, List<ObjectRefType> objectsOfInterest) {
        return this.auditableEventDao.getEventsOfInterest(
                subscription.getStartTime(), subscription.getEndTime(),
                objectsOfInterest);
    }

    protected NotificationType getNotification(SubscriptionType subscription,
            String address, List<ObjectRefType> objectsOfInterest,
            List<AuditableEventType> eventsOfInterest)
            throws EbxmlRegistryException {
        // Create the notification object
        NotificationType notification = createNotification(
                subscription.getId(), eventsOfInterest);
        checkNotification(notification, objectsOfInterest, address);
        notification.addSlot(EbxmlObjectUtil.NOTIFICATION_SOURCE_URL_SLOT_NAME,
                EbxmlObjectUtil.REGISTRY_BASE_URL);
        return notification;
    }

    protected void saveNotification(NotificationType notification) {
        notificationDao.createOrUpdate(notification);
    }

    protected void sendNotification(NotificationListenerWrapper listener,
            NotificationType notification, String address) {

        statusHandler.info("Sending notification [" + notification.getId()
                + "] to address [" + address + "]");
        listener.notificationListener.onNotification(notification);
        statusHandler.info("Notification [" + notification.getId()
                + " successfully sent to address [" + address + "]");

        // Keep a record of when the auditable event was sent to
        // the target
        auditableEventDao.persistSendDate(notification.getEvent(),
                notification.getSubscription(), address);
        // Persist the notification
        notificationDao.createOrUpdate(notification);
    }

    /**
     * Sends notifications for a subscription based on the objectsOfInterest
     * list
     * 
     * @param subscription
     *            The subscription to send notifications for
     * @param objectsOfInterest
     *            The objects of interest to send notifications for
     * @throws EbxmlRegistryException
     *             If errors occur while sending the notifications
     */
    protected void sendNotifications(
            SubscriptionNotificationListeners notificationListeners,
            final List<ObjectRefType> objectsOfInterest)
            throws EbxmlRegistryException {
        int SIZE_LIMIT = 100;

        final List<NotificationListenerWrapper> listeners = notificationListeners.listeners;
        final SubscriptionType subscription = notificationListeners.subscription;

        List<AuditableEventType> eventsOfInterest = getEventsOfInterest(
                subscription, objectsOfInterest);

        if (!eventsOfInterest.isEmpty()) {
            try {
                for (NotificationListenerWrapper listener : listeners) {
                    int subListCount = eventsOfInterest.size() / SIZE_LIMIT;
                    int lastListSize = eventsOfInterest.size() % SIZE_LIMIT;
                    for (int i = 0; i < subListCount; i++) {
                        NotificationType notification = getNotification(
                                subscription, listener.address,
                                objectsOfInterest, eventsOfInterest.subList(
                                        SIZE_LIMIT * i, SIZE_LIMIT * i
                                                + SIZE_LIMIT));
                        if (!notification.getEvent().isEmpty()) {
                            sendNotification(listener, notification,
                                    listener.address);
                        }
                    }
                    if (lastListSize > 0) {
                        NotificationType notification = getNotification(
                                subscription,
                                listener.address,
                                objectsOfInterest,
                                eventsOfInterest.subList(SIZE_LIMIT
                                        * subListCount, SIZE_LIMIT
                                        * subListCount + lastListSize));
                        if (!notification.getEvent().isEmpty()) {
                            sendNotification(listener, notification,
                                    listener.address);
                        }
                    }

                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to determine notification destinations.", e);
            }
        }
    }

    protected void checkNotification(NotificationType notification,
            List<ObjectRefType> objectsOfInterest, String serviceAddress)
            throws EbxmlRegistryException {

        List<AuditableEventType> eventsToSend = new ArrayList<AuditableEventType>();
        List<RegistryObjectType> objectsToRemove = new ArrayList<RegistryObjectType>();
        List<ObjectRefType> refsToRemove = new ArrayList<ObjectRefType>();
        for (AuditableEventType event : notification.getEvent()) {
            // Check to see if this is a reciprocal notification. We don't want
            // to send a notification back to the server who sent us the object
            if (isReciprocalNotification(serviceAddress, event)) {
                continue;
            }

            // Checks to see if this event was already sent to this destination
            BigInteger sentDate = auditableEventDao.getSendTime(event,
                    notification.getSubscription(), serviceAddress);
            if (sentDate == null) {
                /*
                 * The sent date was not found. This event has not yet been sent
                 * to this destination. Iterate through the actions and make
                 * sure that the objects have not already been sent to the
                 * destination. If the objects have been sent, remove them from
                 * the auditable event.
                 */

                List<ActionType> actionList = event.getAction();
                for (ActionType action : actionList) {
                    objectsToRemove.clear();

                    if (action.getAffectedObjectRefs() != null) {
                        List<ObjectRefType> objRefs = action
                                .getAffectedObjectRefs().getObjectRef();
                        for (ObjectRefType obj : objRefs) {
                            boolean found = objectInList(objectsOfInterest, obj);
                            if (!found) {
                                refsToRemove.add(obj);
                            }
                        }
                        objRefs.removeAll(objectsToRemove);
                    } else if (action.getAffectedObjects() != null) {
                        List<RegistryObjectType> regObjs = action
                                .getAffectedObjects().getRegistryObject();
                        for (RegistryObjectType obj : regObjs) {
                            boolean found = objectInList(objectsOfInterest, obj);
                            if (!found) {
                                objectsToRemove.add(obj);
                            }
                        }
                        regObjs.removeAll(objectsToRemove);
                    } else {
                        statusHandler
                                .info("No affected objects present for Auditable event.");
                    }

                }
                eventsToSend.add(event);
            } else {
                statusHandler.debug("Auditable Event [" + event.getId()
                        + "] for subscription ["
                        + notification.getSubscription()
                        + "] already sent to service address ["
                        + serviceAddress + "] at " + sentDate);

            }
        }
        notification.setEvent(eventsToSend);
        if (!eventsToSend.isEmpty()) {
            statusHandler.info(eventsToSend.size()
                    + " applicable events found for subscription ["
                    + notification.getSubscription() + "]");
        }
    }

    /**
     * Creates a notification based on the list of objects and associated
     * auditable events
     * 
     * @param subscription
     *            The subscription to create the notifications for
     * @param serviceAddress
     *            The address to send the notifications to
     * @param objectsOfInterest
     *            The objects of interest
     * @param eventsOfInterest
     *            The events of interest
     * @return The notification to send
     * @throws EbxmlRegistryException
     *             If errors occur while creating the notification
     */
    protected NotificationType createNotification(String subscriptionId,
            List<AuditableEventType> eventsOfInterest)
            throws EbxmlRegistryException {

        /*
         * Create the notification object
         */
        NotificationType notification = new NotificationType();
        notification.setId(EbxmlObjectUtil.getUUID());
        notification.setLid(notification.getId());
        notification.setName(RegistryUtil
                .getInternationalString("Subscription Notification"));
        notification.setDescription(RegistryUtil
                .getInternationalString("Notification for subscription ["
                        + subscriptionId + "]"));
        notification.setObjectType(RegistryObjectTypes.NOTIFICATION);
        notification.setStatus(StatusTypes.APPROVED);
        notification.setOwner(RegistryUtil.DEFAULT_OWNER);
        notification.setSubscription(subscriptionId);
        notification.setEvent(eventsOfInterest);
        return notification;

    }

    /**
     * Checks if the object is in the objectsOfInterest list by comparing the
     * identifiers
     * 
     * @param objectsOfInterest
     *            The list to check
     * @param object
     *            The object being checked to see if it's in the list
     * @return True if the object is in the list
     */
    private boolean objectInList(List<ObjectRefType> objectsOfInterest,
            IPersistableDataObject<String> object) {
        for (ObjectRefType objOfInterest : objectsOfInterest) {
            if (objOfInterest.getId().equals(object.getIdentifier())) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks to make sure we are not send a notification based on objects
     * inserted from a notification that was sent to use
     * 
     * @param serviceAddress
     *            The address we are currently sending the notifications to
     * @param event
     *            The event to check if it was generated as a result of a
     *            notification
     * @return True if this is an event generated from a previous notification,
     *         else false
     * @throws EbxmlRegistryException
     *             In the case of an invalid service address
     */
    private boolean isReciprocalNotification(String serviceAddress,
            AuditableEventType event) throws EbxmlRegistryException {
        String notificationHost = event
                .getSlotValue(EbxmlObjectUtil.HOME_SLOT_NAME);
        return notificationHost != null
                && serviceAddress.contains(notificationHost);

    }

    public void setAuditableEventDao(AuditableEventTypeDao auditableEventDao) {
        this.auditableEventDao = auditableEventDao;
    }

    public void setNotificationDao(NotificationTypeDao notificationDao) {
        this.notificationDao = notificationDao;
    }

}
