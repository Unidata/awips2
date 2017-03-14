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

import java.util.ArrayList;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class ReplicationEventDao extends
        SessionManagedDao<Long, ReplicationEvent> {

    /** Query to get Expired AuditableEvents */

    private static final String GET_REPLICATED_EVENTS = "SELECT event.id FROM ReplicationEvent event where event.replicateTo IS EMPTY";

    private static final String DELETE_EVENTS = "DELETE from ReplicationEvent event where event.id in :eventIds";

    private static final String GET_EVENTS = "SELECT event from ReplicationEvent event where event.id in (:eventIds)";

    @Override
    protected Class<ReplicationEvent> getEntityClass() {
        return ReplicationEvent.class;
    }

    @Transactional(propagation = Propagation.REQUIRED)
    public List<Long> getReplicatedEvents(int batchSize) {
        return this.executeHQLQuery(GET_REPLICATED_EVENTS, batchSize);
    }

    @Transactional(propagation = Propagation.REQUIRED)
    public void deleteEvents(List<Long> eventId)
            throws DataAccessLayerException {
        this.executeHQLStatement(DELETE_EVENTS, "eventIds", eventId);
    }

    @Transactional(propagation = Propagation.REQUIRED)
    public List<ReplicationEvent> getEvents(List<Long> eventId) {
        if (eventId != null && !eventId.isEmpty()) {
            return this.executeHQLQuery(GET_EVENTS, "eventIds", eventId);
        } else {
            return new ArrayList<>(0);
        }
    }
}
