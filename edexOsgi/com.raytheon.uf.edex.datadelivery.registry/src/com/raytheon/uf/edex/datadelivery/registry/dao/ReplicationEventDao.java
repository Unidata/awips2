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
package com.raytheon.uf.edex.datadelivery.registry.dao;

import java.util.List;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.datadelivery.registry.federation.ReplicationEvent;

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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class ReplicationEventDao extends
        SessionManagedDao<Long, ReplicationEvent> {

    private static final String GET_REPLICATION_EVENT_QUERY = "from ReplicationEvent event where (event.source is null or event.source != '%s') and (event.replicatedTo is null or event.replicatedTo not like '%%%s%%') order by event.eventTime asc";

    @Override
    protected Class<ReplicationEvent> getEntityClass() {
        return ReplicationEvent.class;
    }

    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public List<ReplicationEvent> getReplicationEvents(String remoteRegistry) {
        return this.executeHQLQuery(String.format(GET_REPLICATION_EVENT_QUERY,
                remoteRegistry, remoteRegistry));
    }
}
