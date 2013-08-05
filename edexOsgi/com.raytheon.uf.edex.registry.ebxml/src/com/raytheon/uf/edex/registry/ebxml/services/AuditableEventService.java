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
package com.raytheon.uf.edex.registry.ebxml.services;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ActionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * Service to interact with {@link AuditableEventType} objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 02, 2013 1910       djohnson     Extracted subscription notification from the dao.
 * 8/1/2013     1692       bphillip    Refactored auditable event creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class AuditableEventService {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AuditableEventService.class);

    /** Data access object for accessing auditable events */
    private final AuditableEventTypeDao auditDao;

    /** The subscription manager */
    private final IRegistrySubscriptionManager subscriptionManager;

    /**
     * Creates a new AuditableEventService
     * 
     * @param auditDao
     *            Auditable event dao
     * @param subscriptionManager
     *            The subscription manager
     */
    public AuditableEventService(AuditableEventTypeDao auditDao,
            IRegistrySubscriptionManager subscriptionManager) {
        this.auditDao = auditDao;
        this.subscriptionManager = subscriptionManager;
    }

    /**
     * Creates auditable events from the given objects
     * 
     * @param request
     *            The request object which was process that generated the events
     * @param objectsCreated
     *            The objects created during the processing of the request
     * @param objectsUpdated
     *            The objects updated during the processing of the request
     * @param objectsVersioned
     *            The objects versioned during the processing of the request
     * @param objectsRemoved
     *            The objects removed during the processing of the request
     * @throws EbxmlRegistryException
     *             If errors occur creating the event
     */
    public void createAuditableEventFromObjects(RegistryRequestType request,
            List<RegistryObjectType> objectsCreated,
            List<RegistryObjectType> objectsUpdated,
            List<RegistryObjectType> objectsVersioned,
            List<RegistryObjectType> objectsRemoved)
            throws EbxmlRegistryException {
        AuditableEventType event = createEvent(request,
                TimeUtil.currentTimeMillis());
        addRegistryObjectActionToEvent(event, ActionTypes.create,
                objectsCreated);
        addRegistryObjectActionToEvent(event, ActionTypes.update,
                objectsUpdated);
        addRegistryObjectActionToEvent(event, ActionTypes.version,
                objectsVersioned);
        addRegistryObjectActionToEvent(event, ActionTypes.delete,
                objectsRemoved);
        auditDao.create(event);
        notifySubscriptionManager();
    }

    /**
     * Adds an action to the event object
     * 
     * @param event
     *            The event to add the action to
     * @param eventType
     *            The type of event
     * @param objs
     *            The objects affected by the event
     */
    private void addRegistryObjectActionToEvent(AuditableEventType event,
            String eventType, List<RegistryObjectType> objs) {
        if (!CollectionUtil.isNullOrEmpty(objs)) {
            ActionType action = new ActionType();
            action.setEventType(eventType);
            RegistryObjectListType objList = new RegistryObjectListType();
            objList.getRegistryObject().addAll(objs);
            action.setAffectedObjects(objList);
            event.getAction().add(action);
        }
    }

    /**
     * Creates an auditable event from the given object references
     * 
     * @param request
     *            The request that is generating the event
     * @param objectsCreated
     *            References to the objects created during the processing of the
     *            request
     * @param objectsUpdated
     *            References to the objects updated during the processing of the
     *            request
     * @param objectsVersioned
     *            References to the object versioned during the processing of
     *            the request
     * @param objectsRemoved
     *            References to the objects removed during the processing of the
     *            request
     * @throws EbxmlRegistryException
     *             If error occur while creating the event
     */
    public void createAuditableEventFromRefs(RegistryRequestType request,
            List<ObjectRefType> objectsCreated,
            List<ObjectRefType> objectsUpdated,
            List<ObjectRefType> objectsVersioned,
            List<ObjectRefType> objectsRemoved) throws EbxmlRegistryException {
        AuditableEventType event = createEvent(request,
                TimeUtil.currentTimeMillis());
        addObjectRefActionToEvent(event, ActionTypes.create, objectsCreated);
        addObjectRefActionToEvent(event, ActionTypes.update, objectsUpdated);
        addObjectRefActionToEvent(event, ActionTypes.version, objectsVersioned);
        addObjectRefActionToEvent(event, ActionTypes.delete, objectsRemoved);
        auditDao.create(event);
        notifySubscriptionManager();
    }

    /**
     * Adds an action to the event
     * 
     * @param event
     *            The event to add the action to
     * @param eventType
     *            The type of event
     * @param objs
     *            The object references to add to the event
     */
    private void addObjectRefActionToEvent(AuditableEventType event,
            String eventType, List<ObjectRefType> objs) {
        if (!CollectionUtil.isNullOrEmpty(objs)) {
            ActionType action = new ActionType();
            action.setEventType(eventType);
            ObjectRefListType objList = new ObjectRefListType();
            objList.getObjectRef().addAll(objs);
            action.setAffectedObjectRefs(objList);
            event.getAction().add(action);
        }
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
        String objectId = RegistryUtil.generateRegistryObjectId();
        event.setId(objectId);
        event.setLid(objectId);
        event.setOwner(RegistryUtil.DEFAULT_OWNER);
        event.setObjectType(RegistryObjectTypes.AUDITABLE_EVENT);
        event.setRequestId(request.getId());
        event.setTimestamp(EbxmlObjectUtil
                .getTimeAsXMLGregorianCalendar(currentTime));
        event.setUser("Client");
        event.setStatus(StatusTypes.APPROVED);
        event.setVersionInfo(new VersionInfoType());
        String notificationFrom = request
                .getSlotValue(EbxmlObjectUtil.HOME_SLOT_NAME);
        if (notificationFrom != null) {
            event.addSlot(EbxmlObjectUtil.HOME_SLOT_NAME, notificationFrom);
        }
        return event;

    }

    /**
     * Notifies the subscription manager that events have occurred and they must
     * be propagated to the federation
     */
    private void notifySubscriptionManager() {
        // Notify the subscription monitor that a new event has occurred
        try {
            subscriptionManager.processSubscriptions();
        } catch (Throwable t) {
            statusHandler
                    .error("Unexpected error ecountered while processing subscriptions!",
                            t);
        }
    }
}
