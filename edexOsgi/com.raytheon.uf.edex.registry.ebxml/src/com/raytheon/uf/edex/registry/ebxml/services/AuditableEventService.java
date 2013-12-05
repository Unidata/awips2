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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

import org.springframework.transaction.annotation.Transactional;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;
import com.raytheon.uf.edex.registry.events.CreateAuditTrailEvent;

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
 * 9/11/2013    2254       bphillip    Cleaned up creation of auditable events
 * 10/23/2013   1538       bphillip    Removed call to subscription manager. Subscriptions will now
 *                                     only be run on a quartz timer
 * 12/2/2013    1829       bphillip    Now uses event bus for triggering auditable event generation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Transactional
public class AuditableEventService {

    /** Data access object for accessing auditable events */
    private AuditableEventTypeDao auditDao;

    public AuditableEventService() {

    }

    /**
     * Creates a new AuditableEventService
     * 
     * @param auditDao
     *            Auditable event dao
     * @param subscriptionManager
     *            The subscription manager
     */
    public AuditableEventService(AuditableEventTypeDao auditDao) {
        this.auditDao = auditDao;
    }

    /**
     * Creates auditable events from the given objects
     * 
     * @param request
     *            The request that generated the changes
     * @param actionType
     *            The action that was taken on the object
     * @param objectsAffected
     *            The objects that were affected
     * @throws EbxmlRegistryException
     *             If errors occur while creating the event
     */
    @Subscribe
    public void createAuditableEventFromObjects(
            CreateAuditTrailEvent registryEvent) throws EbxmlRegistryException {
        if (!CollectionUtil.isNullOrEmpty(registryEvent.getObjectsAffected())) {
            AuditableEventType event = createEvent(registryEvent.getRequest(),
                    TimeUtil.currentTimeMillis());
            addRegistryObjectActionToEvent(event,
                    registryEvent.getActionType(),
                    registryEvent.getObjectsAffected());
            auditDao.createOrUpdate(event);
        }
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
        ActionType action = new ActionType();
        action.setEventType(eventType);
        ObjectRefListType objList = new ObjectRefListType();
        for (RegistryObjectType obj : objs) {
            objList.getObjectRef().add(new ObjectRefType(obj.getId()));
        }
        action.setAffectedObjectRefs(objList);
        event.getAction().add(action);
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
            event.getSlot().add(
                    new SlotType(EbxmlObjectUtil.HOME_SLOT_NAME,
                            new StringValueType(notificationFrom)));
        }
        return event;
    }
}
