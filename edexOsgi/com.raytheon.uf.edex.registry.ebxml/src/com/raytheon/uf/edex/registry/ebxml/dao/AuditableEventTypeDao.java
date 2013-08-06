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

import java.math.BigInteger;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.time.util.TimeUtil;
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
 * Apr 17, 2013 1914       djohnson    Use strategy for subscription processing.
 * May 02, 2013 1910       djohnson    Broke out registry subscription notification to a service class.
 * 7/29/2013    2191       bphillip    Changed method to get expired events
 * 8/1/2013     1693       bphillip    Moved creation of auditable events to the auditable event service class
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
            + "left outer join event.action as action "
            + "left outer join action.affectedObjects as AffectedObjects "
            + "left outer join AffectedObjects.registryObject as RegistryObjects "
            + "where (RegistryObjects.id in (:ids) OR action.eventType = :eventType) and event.timestamp >= :startTime";

    /** Optional end time clause */
    private static final String END_TIME_CLAUSE = " and event.timestamp <= :endTime";

    /** Order by clause */
    private static final String ORDER_CLAUSE = " order by event.timestamp asc";

    /** The number of hours to retain auditable events */
    private static final int AUDITABLE_EVENT_RETENTION_TIME = 48;

    /** Cutoff parameter for the query to get the expired events */
    private static final String GET_EXPIRED_EVENTS_QUERY_CUTOFF_PARAMETER = "cutoff";

    /** Query to get Expired AuditableEvents */
    private static final String GET_EXPIRED_EVENTS_QUERY = "FROM AuditableEventType event where event.timestamp < :"
            + GET_EXPIRED_EVENTS_QUERY_CUTOFF_PARAMETER;

    /**
     * Constructor.
     * 
     * @param subscriptionProcessor
     */
    public AuditableEventTypeDao() {
    }

    @Override
    public void create(AuditableEventType event) {
        template.save(event);
    }

    /**
     * Gets auditable events older than 48 hrs old
     * 
     * @throws EbxmlRegistryException
     *             If errors occur purging auditable events
     */
    @Transactional(propagation = Propagation.REQUIRED)
    public List<AuditableEventType> getExpiredEvents(int limit)
            throws EbxmlRegistryException {
        Calendar cutoffTime = TimeUtil.newGmtCalendar();
        cutoffTime.add(Calendar.HOUR_OF_DAY, -AUDITABLE_EVENT_RETENTION_TIME);
        return this.executeHQLQuery(GET_EXPIRED_EVENTS_QUERY, limit,
                GET_EXPIRED_EVENTS_QUERY_CUTOFF_PARAMETER, EbxmlObjectUtil
                        .getTimeAsXMLGregorianCalendar(cutoffTime
                                .getTimeInMillis()));
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
    public List<AuditableEventType> getEventsOfInterest(
            XMLGregorianCalendar startTime, XMLGregorianCalendar endTime,
            List<ObjectRefType> objectsOfInterest) {
        if (objectsOfInterest.isEmpty()) {
            return Collections.emptyList();
        }
        StringBuilder buf = new StringBuilder();
        for (ObjectRefType objOfInterest : objectsOfInterest) {
            buf.append(", '").append(objOfInterest.getId()).append("'");
        }
        String inString = buf.toString().replaceFirst(",", "");
        if (endTime == null) {
            return this.query((EVENTS_OF_INTEREST_QUERY + ORDER_CLAUSE)
                    .replace(IDS, inString), "startTime", startTime,
                    "eventType", ActionTypes.delete);
        } else {
            return this.query(
                    (EVENTS_OF_INTEREST_QUERY + END_TIME_CLAUSE + ORDER_CLAUSE)
                            .replace(IDS, inString), "startTime", startTime,
                    "eventType", ActionTypes.delete, "endTime", endTime);
        }
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
            String subscriptionId, String deliveryAddress) {
        for (AuditableEventType auditableEvent : auditableEvents) {
            auditableEvent.updateSlot(subscriptionId + deliveryAddress,
                    (int) TimeUtil.currentTimeMillis());
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
    public BigInteger getSendTime(AuditableEventType auditableEvent,
            String subscriptionId, String deliveryAddress) {
        return auditableEvent.getSlotValue(subscriptionId + deliveryAddress);
    }

    @Override
    protected Class<AuditableEventType> getEntityClass() {
        return AuditableEventType.class;
    }

}
