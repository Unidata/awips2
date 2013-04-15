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

package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.datatype.DatatypeConfigurationException;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ActionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/18/2013    1802       bphillip    Initial creation
 * 4/9/2013     1802       bphillip    Removed exception catching
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class AuditableEventTypeDao extends
        RegistryObjectTypeDao<AuditableEventType> {

    private static final String IDS = ":ids";

    /**
     * Query to find events of interest when sending registry replication
     * notifications
     */
    private static final String EVENTS_OF_INTEREST_QUERY = "select event from AuditableEventType as event "
            + "inner join event.action as action "
            + "inner join action.affectedObjects as AffectedObjects "
            + "inner join AffectedObjects.registryObject as RegistryObjects "
            + "where (RegistryObjects.id in (:ids) OR action.eventType = ?) and event.timestamp >= ?";

    /** Optional end time clause */
    private static final String END_TIME_CLAUSE = " and event.timestamp <= ?";

    /** Order by clause */
    private static final String ORDER_CLAUSE = " order by event.timestamp asc";

    public void create(AuditableEventType event) {
        template.save(event);
        // Notify the subscription monitor that a new event has occurred
        try {
            EDEXUtil.getMessageProducer().sendAsyncUri(
                    "vm:processSubscriptions", null);
        } catch (Throwable t) {
            statusHandler
                    .error("Unexpected error ecountered while processing subscriptions!",
                            t);
        }
    }

    /**
     * Get events of interest for registry subscriptions
     * 
     * @param objectsOfInterest
     * @return @
     */
    public Iterator<AuditableEventType> getEventsOfInterest(
            SubscriptionType subscription, List<ObjectRefType> objectsOfInterest) {
        if (objectsOfInterest.isEmpty()) {
            return null;
        }
        StringBuilder buf = new StringBuilder();
        for (ObjectRefType objOfInterest : objectsOfInterest) {
            buf.append(", '").append(objOfInterest.getId()).append("'");
        }
        String inString = buf.toString().replaceFirst(",", "");
        Iterator<AuditableEventType> eventIterator = null;
        if (subscription.getEndTime() == null) {
            eventIterator = this.getQueryIterator(
                    (EVENTS_OF_INTEREST_QUERY + ORDER_CLAUSE).replace(IDS,
                            inString), ActionTypes.delete, subscription
                            .getStartTime());
        } else {
            eventIterator = this.getQueryIterator((EVENTS_OF_INTEREST_QUERY
                    + END_TIME_CLAUSE + ORDER_CLAUSE).replace(IDS, inString),
                    ActionTypes.delete, subscription.getStartTime(),
                    subscription.getEndTime());
        }
        return eventIterator;
    }

    /**
     * Adds the date that the auditable event was sent to a particular host
     * 
     * @param auditableEvents
     *            The events to add the last sent date
     * @param subscription
     *            The subscription that this auditable event is being used
     * @param deliveryAddress
     *            The delivery address @ * If errors occur while adding the slot
     *            to the auditable event
     */
    public void addSendDate(List<AuditableEventType> auditableEvents,
            SubscriptionType subscription, String deliveryAddress) {
        for (AuditableEventType auditableEvent : auditableEvents) {
            EbxmlObjectUtil.addStringSlot(auditableEvent, subscription.getId()
                    + deliveryAddress,
                    String.valueOf(System.currentTimeMillis()), false);
            this.createOrUpdate(auditableEvent);
        }
    }

    /**
     * Gets the date that the auditable event was sent to a delivery address, if
     * any
     * 
     * @param auditableEvent
     *            The auditable event to check
     * @param subscription
     *            The subscription that this auditable event pertains to
     * @param deliveryAddress
     *            The delivery address to check
     * @return The last sent date in millis
     */
    public Long getSendDate(AuditableEventType auditableEvent,
            SubscriptionType subscription, String deliveryAddress) {
        String sendDate = EbxmlObjectUtil.getStringSlotValue(auditableEvent,
                subscription.getId() + deliveryAddress);
        if (sendDate == null) {
            return null;
        } else {
            return Long.parseLong(sendDate);
        }
    }

    /**
     * Creates an auditable event from a registry request and object references
     * 
     * @param request
     *            The request that generated the events
     * @param actionMap
     *            The actions that occurred
     * @param currentTime
     *            The time the event occurred @ * If errors occur while creating
     *            the event
     * @throws EbxmlRegistryException
     */
    public void createAuditableEventsFromRefs(RegistryRequestType request,
            Map<String, List<ObjectRefType>> actionMap, long currentTime)
            throws EbxmlRegistryException {
        AuditableEventType event = createEvent(request, currentTime);
        for (String actionType : actionMap.keySet()) {
            event.getAction().add(
                    createAuditableEventAction(actionMap.get(actionType), null,
                            actionType));
        }
        create(event);
    }

    /**
     * Creates an auditable event from a registry request and registry objects
     * 
     * @param request
     *            The request that generated the events
     * @param actionMap
     *            The actions that occurred
     * @param currentTime
     *            The time the event occurred @ * If errors occur while creating
     *            the event
     * @throws EbxmlRegistryException
     */
    public void createAuditableEventsFromObjects(RegistryRequestType request,
            Map<String, List<RegistryObjectType>> actionMap, long currentTime)
            throws EbxmlRegistryException {
        AuditableEventType event = createEvent(request, currentTime);
        for (String actionType : actionMap.keySet()) {
            event.getAction().add(
                    createAuditableEventAction(null, actionMap.get(actionType),
                            actionType));
        }
        create(event);
    }

    /**
     * Helper class to create and action for the AuditableEventType
     * 
     * @param affectedObjectRefs
     *            The references to any affected objects
     * @param affectedObjects
     *            The affected objects
     * @param actionType
     *            The action that occurred
     * @return The ActionType object
     */
    private ActionType createAuditableEventAction(
            List<ObjectRefType> affectedObjectRefs,
            List<RegistryObjectType> affectedObjects, String actionType) {
        ActionType action = new ActionType();
        action.setEventType(actionType);
        if (affectedObjectRefs != null) {
            ObjectRefListType refList = new ObjectRefListType();
            refList.getObjectRef().addAll(affectedObjectRefs);
            action.setAffectedObjectRefs(refList);
        }
        if (affectedObjects != null) {
            RegistryObjectListType regObjList = new RegistryObjectListType();
            regObjList.getRegistryObject().addAll(affectedObjects);
            action.setAffectedObjects(regObjList);
        }
        return action;
    }

    /**
     * Creates and Auditable event from a request
     * 
     * @param request
     *            The request that generated the event
     * @param currentTime
     *            The time of the event
     * @return The AuditableEventType object
     * @throws EbxmlRegistryException
     *             @ * If errors occur while creating the event
     */
    private AuditableEventType createEvent(RegistryRequestType request,
            long currentTime) throws EbxmlRegistryException {
        AuditableEventType event = EbxmlObjectUtil.rimObjectFactory
                .createAuditableEventType();
        event.setId(EbxmlObjectUtil.getUUID());
        event.setLid(EbxmlObjectUtil.getUUID());
        event.setOwner(RegistryUtil.DEFAULT_OWNER);
        event.setObjectType(RegistryObjectTypes.AUDITABLE_EVENT);
        event.setRequestId(request.getId());
        try {
            event.setTimestamp(EbxmlObjectUtil
                    .getTimeAsXMLGregorianCalendar(currentTime));
        } catch (DatatypeConfigurationException e) {
            throw new EbxmlRegistryException(
                    "Error creating timestamp for auditable event", e);
        }
        event.setUser("Client");
        event.setStatus(StatusTypes.APPROVED);
        event.setVersionInfo(new VersionInfoType());
        String notificationFrom = EbxmlObjectUtil.getStringSlotValue(request,
                EbxmlObjectUtil.HOME_SLOT_NAME);
        if (notificationFrom != null) {
            EbxmlObjectUtil.addStringSlot(event,
                    EbxmlObjectUtil.HOME_SLOT_NAME, notificationFrom, false);
        }
        return event;

    }

    @Override
    protected Class<AuditableEventType> getEntityClass() {
        return AuditableEventType.class;
    }

}
