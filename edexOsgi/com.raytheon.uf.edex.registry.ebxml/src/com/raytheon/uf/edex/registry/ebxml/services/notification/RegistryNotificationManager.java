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
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ActionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.notification.RegistrySubscriptionManager.NotificationListenerWrapper;
import com.raytheon.uf.edex.registry.ebxml.services.notification.RegistrySubscriptionManager.SubscriptionNotificationListeners;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl;
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
 * 9/11/2013    2354        bphillip    Added logic to ensure delete events get included in notifications
 * 9/30/2013    2191        bphillip    Fixing federated replication
 * 10/8/2013    1682        bphillip    Moved get objects of interest from RegistrySubscriptionManager and javadoc
 * 10/20/2013   1682        bphillip    Added synchronous notification delivery
 * 10/23/2013   1538        bphillip    Adding log messages and changed methods to handle DateTime value on 
 *                                      AuditableEvents instead of integer
 * 12/9/2013    2613        bphillip    Changed start time boundary of get auditable events to be the last run time of the subscription
 * 01/21/2014   2613        bphillip    Changed start time boundary again and also a few minor cleanup items
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

    /** The query manager implementation */
    private QueryManagerImpl queryManager;

    /** The maximum number of auditable events sent in a notification message */
    private int notificationBatchSize = Integer.parseInt(System
            .getProperty("ebxml-notification-batch-size"));

    /**
     * Creates a new RegistryNotificationManager
     * 
     */
    public RegistryNotificationManager() {
    }

    /**
     * Gets the objects of interest for a given subscription
     * 
     * @param subscription
     *            The subscription to get the objects of interest for
     * @return The objects of interest for the given subscription
     * @throws MsgRegistryException
     *             If errors occur while executing the subscription's selector
     *             query
     */
    public List<ObjectRefType> getObjectsOfInterest(
            SubscriptionType subscription) throws MsgRegistryException {
        // Get objects that match selector query
        return queryManager
                .executeQuery(
                        new QueryRequest("Objects of Interest Query for ["
                                + subscription.getId() + "]", subscription
                                .getSelector(), new ResponseOptionType(
                                QueryReturnTypes.OBJECT_REF, false)))
                .getObjectRefList().getObjectRef();
    }

    /**
     * Gets events of interest based on a start time and end time for the given
     * list of objects
     * 
     * @param startTime
     *            The lower time boundary for the query
     * @param endTime
     *            The optional upper time boundary for the query
     * @param objectsOfInterest
     *            The objects to get events for
     * @return The events of interest for the given set of objects
     * @throws MsgRegistryException
     * @throws EbxmlRegistryException
     */
    public List<AuditableEventType> getEventsOfInterest(
            SubscriptionType subscription, String serviceAddress,
            XMLGregorianCalendar startTime, XMLGregorianCalendar endTime,
            List<ObjectRefType> objectsOfInterest)
            throws EbxmlRegistryException, MsgRegistryException {
        return this.auditableEventDao.getEventsOfInterest(subscription,
                serviceAddress, startTime, endTime, objectsOfInterest);
    }

    /**
     * Constructs the NotificationType object for the given parameters
     * 
     * @param subscription
     *            The subscription that this notification is for
     * @param address
     *            The address to send the notification to
     * @param objectsOfInterest
     *            The objects affected by the change
     * @param eventsOfInterest
     *            The events related to the objects of interest
     * @return The NotificationType object for the given parameters
     * @throws EbxmlRegistryException
     *             If errors occur while creating or checking the notification
     */
    public NotificationType getNotification(SubscriptionType subscription,
            String address, List<ObjectRefType> objectsOfInterest,
            List<AuditableEventType> eventsOfInterest)
            throws EbxmlRegistryException {
        // Create the notification object
        NotificationType notification = createNotification(
                subscription.getId(), eventsOfInterest);
        checkNotification(notification, objectsOfInterest, address);
        return notification;
    }

    /**
     * Sends a notification
     * 
     * @param listener
     *            The notification listener to use to send the notification
     * @param notification
     *            The notification to send
     * @param address
     *            The address to send the notification to
     * @throws MsgRegistryException
     */
    protected void sendNotification(NotificationListenerWrapper listener,
            NotificationType notification, String address)
            throws MsgRegistryException {

        statusHandler.info("Sending notification [" + notification.getId()
                + "] to address [" + address + "]");

        long sentTime = TimeUtil.currentTimeMillis();
        try {
            listener.notificationListener.synchronousNotification(notification);
        } catch (MsgRegistryException e) {
            statusHandler.error("Notification [" + notification.getId()
                    + " failed to address [" + address + "]", e);
            throw e;
        } finally {
            statusHandler.info("Notification [" + notification.getId()
                    + "] transmission took ["
                    + (TimeUtil.currentTimeMillis() - sentTime) + "] ms");
        }
        statusHandler.info("Notification [" + notification.getId()
                + " successfully sent to address [" + address + "]");

        // Keep a record of when the auditable event was sent to
        // the target
        auditableEventDao.persistSendDate(notification.getEvent(),
                notification.getSubscription(), address, sentTime);
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
     * @throws MsgRegistryException
     */
    protected XMLGregorianCalendar sendNotifications(
            SubscriptionNotificationListeners notificationListeners,
            XMLGregorianCalendar startTime) throws EbxmlRegistryException,
            MsgRegistryException {

        // Object to hold the last timestampe of the latest event in order to
        // update the subscription last run time correctly
        XMLGregorianCalendar lastTime = null;
        final List<NotificationListenerWrapper> listeners = notificationListeners.listeners;
        final SubscriptionType subscription = notificationListeners.subscription;

        List<ObjectRefType> objectsOfInterest = getObjectsOfInterest(subscription);

        for (NotificationListenerWrapper listener : listeners) {
            List<AuditableEventType> eventsOfInterest = getEventsOfInterest(
                    subscription, listener.address, startTime,
                    subscription.getEndTime(), objectsOfInterest);
            if (!eventsOfInterest.isEmpty()) {
                lastTime = eventsOfInterest.get(eventsOfInterest.size() - 1)
                        .getTimestamp();
                int subListCount = eventsOfInterest.size()
                        / notificationBatchSize;
                int lastListSize = eventsOfInterest.size()
                        % notificationBatchSize;
                try {
                    for (int i = 0; i < subListCount; i++) {

                        NotificationType notification = getNotification(
                                subscription,
                                listener.address,
                                objectsOfInterest,
                                eventsOfInterest.subList(notificationBatchSize
                                        * i, notificationBatchSize * i
                                        + notificationBatchSize));
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
                                eventsOfInterest.subList(notificationBatchSize
                                        * subListCount, notificationBatchSize
                                        * subListCount + lastListSize));
                        if (!notification.getEvent().isEmpty()) {
                            sendNotification(listener, notification,
                                    listener.address);
                        }
                    }
                } catch (MsgRegistryException e) {
                    statusHandler.error("Notification delivery failed!", e);
                }

            }
        }
        return lastTime;
    }

    /**
     * Checks a notification to ensure that previously transmitted events are
     * not being sent again
     * 
     * @param notification
     *            The notification to check
     * @param objectsOfInterest
     *            The objects affected by the change that generated the
     *            notification
     * @param serviceAddress
     *            The destination address for the notification
     * @throws EbxmlRegistryException
     *             If errors occur while checking the notification
     */
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
            Long sentDate = auditableEventDao.getSendTime(event,
                    notification.getSubscription(), serviceAddress);
            if (sentDate == 0) {
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
                    refsToRemove.clear();

                    if (action.getAffectedObjectRefs() != null) {
                        List<ObjectRefType> objRefs = action
                                .getAffectedObjectRefs().getObjectRef();
                        for (ObjectRefType obj : objRefs) {
                            boolean found = objectInList(objectsOfInterest, obj);
                            if (!found
                                    && !action.getEventType().equals(
                                            ActionTypes.delete)) {
                                refsToRemove.add(obj);
                            }
                        }
                        objRefs.removeAll(refsToRemove);
                    } else if (action.getAffectedObjects() != null) {
                        List<RegistryObjectType> regObjs = action
                                .getAffectedObjects().getRegistryObject();
                        for (RegistryObjectType obj : regObjs) {
                            boolean found = objectInList(objectsOfInterest, obj);
                            if (!found
                                    && !action.getEventType().equals(
                                            ActionTypes.delete)) {
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

    public void setQueryManager(QueryManagerImpl queryManager) {
        this.queryManager = queryManager;
    }

}
