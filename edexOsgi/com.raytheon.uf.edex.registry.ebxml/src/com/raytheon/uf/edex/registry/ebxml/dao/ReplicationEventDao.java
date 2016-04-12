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

import java.util.List;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.registry.federation.ReplicationEvent;

/**
 * <pre>
 * 
 * Data Access object for interactions with ReplicationEvent objects
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/19/2014    2769       bphillip    Initial Creation
 * 8/27/2014    3560       bphillip    Added query by event time method
 * 5/11/2015    4448        bphillip    Separated EBXML Registry from Data Delivery
 * 8 Feb, 2016  5198        dhladky     Class cast for String expecting Long fixed
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class ReplicationEventDao extends
        SessionManagedDao<Long, ReplicationEvent> {

    private static final String GET_REPLICATION_EVENT_QUERY = "from ReplicationEvent event "
            + "where (event.source is null or event.source != :source) "
            + "and (event.replicatedTo is null or event.replicatedTo not like :registry) "
            + "order by event.eventTime asc";

    private static final String GET_EVENTS_BY_TIME = "from ReplicationEvent event where event.eventTime < :eventTime";

    @Override
    protected Class<ReplicationEvent> getEntityClass() {
        return ReplicationEvent.class;
    }

    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public List<ReplicationEvent> getReplicationEvents(String remoteRegistry,
            int batchSize) {
        return this.executeHQLQuery(GET_REPLICATION_EVENT_QUERY, batchSize,
                "source", remoteRegistry, "registry", "%" + remoteRegistry
                        + "%");
    }

    public List<ReplicationEvent> getEventsBeforeTime(Long time) {
        return this.executeHQLQuery(GET_EVENTS_BY_TIME, "eventTime", time);
    }
}
