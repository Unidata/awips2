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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.soap.RegistrySOAPServices;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ActionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DateTimeValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

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
 * Apr 17, 2013 1914       djohnson    Use strategy for subscription processing.
 * May 02, 2013 1910       djohnson    Broke out registry subscription notification to a service class.
 * 7/29/2013    2191       bphillip    Changed method to get expired events
 * 8/1/2013     1693       bphillip    Moved creation of auditable events to the auditable event service class
 * 9/11/2013    2354       bphillip    Modified queries to find deleted objects
 * 10/23/2013   1538       bphillip    Changed send time slot to be DateTimeValue instead of integer
 * 12/2/2013    1829       bphillip    Changed to use non generic getter of value type
 * 01/21/2014   2613       bphillip    Modified queries to better handle deletes
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4
 * 10/25/2019   7948       bsteffen    Add method to purge a batch of expired events.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class AuditableEventTypeDao
        extends RegistryObjectTypeDao<AuditableEventType> {

    private static final String IDS = ":ids";

    /**
     * Query to find events of interest when sending registry replication
     * notifications
     */
    private static final String FIND_EVENTS_OF_INTEREST_QUERY = "select event from AuditableEventType as event "
            + "left outer join event.action as action "
            + "left outer join action.affectedObjects as AffectedObjects "
            + "left outer join action.affectedObjectRefs as AffectedObjectRefs "
            + "left outer join AffectedObjects.registryObject as RegistryObjects "
            + "left outer join AffectedObjectRefs.objectRef as ObjRefs "
            + "where (ObjRefs.id in (:ids) OR RegistryObjects.id in (:ids)) and event.timestamp >= :startTime";

    /**
     * Query to find deleted events
     */
    private static final String FIND_DELETED_EVENTS_OF_INTEREST_QUERY = "select event from AuditableEventType as event "
            + "left outer join event.action as action "
            + "where action.eventType = 'urn:oasis:names:tc:ebxml-regrep:ActionType:delete' and event.timestamp > :startTime";

    /** Optional end time clause */
    private static final String END_TIME_CLAUSE = " and event.timestamp <= :endTime";

    /** Order by clause */
    private static final String ORDER_CLAUSE = " order by event.timestamp asc";

    /** The number of hours to retain auditable events */
    public static final int AUDITABLE_EVENT_RETENTION_TIME = 48;

    /** Cutoff parameter for the query to get the expired events */
    private static final String GET_EXPIRED_EVENTS_QUERY_CUTOFF_PARAMETER = "cutoff";

    /** Query to get Expired AuditableEvents */
    private static final String GET_EXPIRED_EVENTS_QUERY = "FROM AuditableEventType event where event.timestamp < :"
            + GET_EXPIRED_EVENTS_QUERY_CUTOFF_PARAMETER;

    /** The registry soap services */
    private RegistrySOAPServices soapService;

    /** Sorter for sorting events */
    private static final Comparator<AuditableEventType> EVENT_TIME_COMPARATOR = new Comparator<AuditableEventType>() {
        @Override
        public int compare(AuditableEventType o1, AuditableEventType o2) {
            return o2.getTimestamp().compare(o1.getTimestamp());
        }
    };

    /**
     * Constructor.
     * 
     * @param subscriptionProcessor
     */
    public AuditableEventTypeDao() {
    }

    @Override
    public void create(AuditableEventType event) {
        getCurrentSession().save(event);
    }

    /**
     * Gets auditable events older than 48 hrs old
     * 
     * @throws EbxmlRegistryException
     *             If errors occur purging auditable events
     */
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public List<AuditableEventType> getExpiredEvents(int limit)
            throws EbxmlRegistryException {
        Calendar cutoffTime = TimeUtil.newGmtCalendar();
        cutoffTime.add(Calendar.HOUR_OF_DAY, -AUDITABLE_EVENT_RETENTION_TIME);
        return this.executeHQLQuery(GET_EXPIRED_EVENTS_QUERY, limit,
                GET_EXPIRED_EVENTS_QUERY_CUTOFF_PARAMETER,
                EbxmlObjectUtil.getTimeAsXMLGregorianCalendar(
                        cutoffTime.getTimeInMillis()));
    }

    /**
     * Gets all auditable events which reference the objects of interest.
     * 
     * @param subscription
     *            The subscription to get the events for
     * @param serviceAddress
     *            The address to the registry to use to verify deleted objects
     * @param startTime
     *            The start time boundary of the query
     * @param endTime
     *            The end time boundary of the query
     * @param objectsOfInterest
     *            The objects of interest to get events for
     * @return The list of auditable events referencing the objects of interest
     * @throws EbxmlRegistryException
     * @throws MsgRegistryException
     */
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public List<AuditableEventType> getEventsOfInterest(
            SubscriptionType subscription, String serviceAddress,
            XMLGregorianCalendar startTime, XMLGregorianCalendar endTime,
            List<ObjectRefType> objectsOfInterest)
            throws EbxmlRegistryException, MsgRegistryException {
        List<AuditableEventType> events = new ArrayList<>(0);
        if (!objectsOfInterest.isEmpty()) {
            events = getEventsOfInterest(FIND_EVENTS_OF_INTEREST_QUERY,
                    startTime, endTime, objectsOfInterest);
        }
        List<AuditableEventType> deleteEvents = getDeleteEventsOfInterest(
                subscription, serviceAddress, startTime, endTime);
        if (!deleteEvents.isEmpty()) {
            events.addAll(deleteEvents);
        }
        Collections.sort(events, EVENT_TIME_COMPARATOR);
        return events;
    }

    /**
     * Gets applicable delete events
     * 
     * @param subscription
     *            The subscription to get the events for
     * @param serviceAddress
     *            The address to the registry to use to verify deleted objects
     * @param startTime
     *            The start time boundary of the query
     * @param endTime
     *            The end time boundary of the query
     * @return The list of auditable events referencing deleted objects
     * @throws EbxmlRegistryException
     * @throws MsgRegistryException
     */
    private List<AuditableEventType> getDeleteEventsOfInterest(
            SubscriptionType subscription, String serviceAddress,
            XMLGregorianCalendar startTime, XMLGregorianCalendar endTime)
            throws EbxmlRegistryException, MsgRegistryException {

        List<AuditableEventType> retVal = new LinkedList<>();
        List<AuditableEventType> deletedEvents = getEventsOfInterest(
                FIND_DELETED_EVENTS_OF_INTEREST_QUERY, startTime, endTime,
                null);
        try {
            URL url = new URL(serviceAddress);
            String baseURL = url.toString().replace(url.getPath(), "");
            List<ObjectRefType> remoteRefs = soapService
                    .getQueryServiceForHost(baseURL)
                    .executeQuery(new QueryRequest(
                            "Deleted Objects of Interest Query for ["
                                    + subscription.getId() + "]",
                            subscription.getSelector(), new ResponseOptionType(
                                    QueryReturnTypes.OBJECT_REF, false)))
                    .getObjectRefList().getObjectRef();

            for (AuditableEventType event : deletedEvents) {
                for (ActionType action : event.getAction()) {
                    if (action.getAffectedObjectRefs() != null && !action
                            .getAffectedObjectRefs().getObjectRef().isEmpty()) {
                        if (remoteRefs.contains(action.getAffectedObjectRefs()
                                .getObjectRef().get(0))) {
                            retVal.add(event);
                        }
                    }
                }
            }
        } catch (MalformedURLException e) {
            throw new EbxmlRegistryException(
                    "Error parsing notification address", e);
        }

        return retVal;

    }

    /**
     * Gets the events of interest based on the start time, end time, and the
     * list of objects of interest
     * 
     * @param startTime
     *            The start time boundary
     * @param endTime
     *            The end time boundary
     * @param objectsOfInterest
     *            The objects of interest
     * @return The list of auditable events of interest within the constrains of
     *         the start time, end time and including the objects of interest
     */
    private List<AuditableEventType> getEventsOfInterest(String query,
            XMLGregorianCalendar startTime, XMLGregorianCalendar endTime,
            List<ObjectRefType> objectsOfInterest) {
        List<Object> queryParams = new ArrayList<>(2);
        queryParams.add("startTime");
        queryParams.add(startTime);
        if (!CollectionUtil.isNullOrEmpty(objectsOfInterest)) {
            StringBuilder buf = new StringBuilder();
            for (int i = 0; i < objectsOfInterest.size(); i++) {
                if (i > 0) {
                    buf.append(", ");
                }
                buf.append("'").append(objectsOfInterest.get(i).getId())
                        .append("'");
            }
            query = FIND_EVENTS_OF_INTEREST_QUERY.replaceAll(IDS,
                    buf.toString());

            if (endTime == null) {
                query += ORDER_CLAUSE;
            } else {
                query += END_TIME_CLAUSE + ORDER_CLAUSE;
                queryParams.add("endTime");
                queryParams.add(endTime);
            }
        }
        return this.query(query,
                queryParams.toArray(new Object[queryParams.size()]));
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
    public void persistSendDate(List<AuditableEventType> auditableEvents,
            String subscriptionId, String deliveryAddress, long sentTime) {
        String slotName = subscriptionId + deliveryAddress;
        for (AuditableEventType auditableEvent : auditableEvents) {
            SlotType slot = auditableEvent
                    .getSlotByName(subscriptionId + deliveryAddress);
            if (slot == null) {
                SlotType sentTimeSlot = new SlotType(slotName,
                        new DateTimeValueType(sentTime));
                auditableEvent.getSlot().add(sentTimeSlot);
            } else {
                DateTimeValueType dateValue = (DateTimeValueType) slot
                        .getSlotValue();
                dateValue.setTime(sentTime);
            }
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
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public Long getSendTime(AuditableEventType auditableEvent,
            String subscriptionId, String deliveryAddress) {
        SlotType slot = auditableEvent
                .getSlotByName(subscriptionId + deliveryAddress);
        if (slot == null) {
            return 0l;
        } else {
            DateTimeValueType dateValue = (DateTimeValueType) slot
                    .getSlotValue();
            return dateValue.getDateTimeValue().toGregorianCalendar()
                    .getTimeInMillis();
        }
    }

    @Override
    protected Class<AuditableEventType> getEntityClass() {
        return AuditableEventType.class;
    }

    public void setSoapService(RegistrySOAPServices soapService) {
        this.soapService = soapService;
    }

    /**
     * Purges one batch of expired events older than 2 days
     * 
     * @param batchSize
     *            the maximum number of events to purge.
     * @return the number of items purged
     * @throws EbxmlRegistryException
     *             If errors occur while enqueuing events to be deleted
     */
    @Transactional
    public int purgeBatchOfExpiredEvents(int batchSize)
            throws EbxmlRegistryException {
        List<AuditableEventType> expiredEvents = getExpiredEvents(batchSize);
        if (!expiredEvents.isEmpty()) {
            deleteAll(expiredEvents);
        }
        return expiredEvents.size();
    }

}
