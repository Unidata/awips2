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

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.registry.federation.ReplicationEvent;

/**
 * <pre>
 * 
 * Data Access object for interactions with ReplicationEvent objects
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 19, 2014  2769     bphillip  Initial Creation
 * Aug 27, 2014  3560     bphillip  Added query by event time method
 * May 11, 2015  4448     bphillip  Separated EBXML Registry from Data Delivery
 * Feb 08, 2016  5198     dhladky   Class cast for String expecting Long fixed
 * May 13, 2016  5638     tjensen   Removed getReplicationEvents
 * Aug 05, 2016  5810     tjensen   Refactor replication
 * Sep 01, 2016  5810     tjensen   Improve replication memory usage
 * Nov 20, 2018  7634     rjpeter   Added order to GET_EVENTS
 * Aug 29, 2019  7836     bsteffen  Simplify interaction with replication site events
 *
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class ReplicationEventDao
        extends SessionManagedDao<Long, ReplicationEvent> {

    private static final String DELETE_EXPIRED_EVENTS = "DELETE from ReplicationEvent where eventTime < :eventTime";

    private static final String GET_EVENTS_BATCH_AFTER_TIME = "from ReplicationEvent where eventTime > (:startTime) order by id";

    private static final String GET_EVENTS_BATCH_AFTER_ID = "from ReplicationEvent where id > (:lastId) order by id";

    private static final String GET_MIN_TIME = "select min(eventTime) from ReplicationEvent";

    @Override
    protected Class<ReplicationEvent> getEntityClass() {
        return ReplicationEvent.class;
    }

    /**
     * Delete all events with a time less than the specified time.
     * 
     * @param eventTime
     * @return the number of deleted events.
     * @throws DataAccessLayerException
     */
    @Transactional(propagation = Propagation.REQUIRED)
    public int deleteExpiredEvents(Date eventTime)
            throws DataAccessLayerException {
        List<Date> dates = this.executeHQLQuery(GET_MIN_TIME);
        if (dates == null || dates.isEmpty() || dates.get(0) == null) {
            return 0;
        }
        Instant startTime = dates.get(0).toInstant();
        Instant endTime = eventTime.toInstant();
        if (startTime.until(endTime, ChronoUnit.HOURS) < 2) {
            return this.executeHQLStatement(DELETE_EXPIRED_EVENTS, "eventTime",
                    eventTime);
        } else {
            int count = 0;
            while (startTime.isBefore(endTime)) {
                startTime = startTime.plus(30, ChronoUnit.MINUTES);
                if (startTime.isAfter(endTime)) {
                    startTime = endTime;
                }
                count += this.executeHQLStatement(DELETE_EXPIRED_EVENTS,
                        "eventTime", Date.from(startTime));
            }
            return count;
        }
    }

    /**
     * Get a batch of events with a time after the specified start time ordered
     * by id.
     * 
     * @param startTime
     * @param maxResults
     *            the maximum number of events to return
     * @return
     */
    @Transactional(propagation = Propagation.REQUIRED)
    public List<ReplicationEvent> getEventsAfterTime(Date startTime,
            int maxResults) {
        return this.executeHQLQuery(GET_EVENTS_BATCH_AFTER_TIME, maxResults,
                "startTime", startTime);
    }

    /**
     * Get a batch of events with an id after the specified id ordered by id.
     * 
     * @param lastId
     * @param maxResults
     *            the maximum number of events to return
     * @return
     */
    @Transactional(propagation = Propagation.REQUIRED)
    public List<ReplicationEvent> getEventsAfterId(long lastId,
            int maxResults) {
        return this.executeHQLQuery(GET_EVENTS_BATCH_AFTER_ID, maxResults,
                "lastId", lastId);
    }
}
