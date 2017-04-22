package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.List;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.registry.federation.ReplicationSiteEvent;
import com.raytheon.uf.edex.registry.federation.ReplicationSiteEvent.ReplicationSiteEventId;

/**
 * 
 * Data Access object for interactions with ReplicationSiteEvent objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------
 * Aug 08, 2016  5810     tjensen   Initial creation
 * Aug 16, 2016  5810     tjensen   Fix issue with delete by time
 * Sep 01, 2016  5810     tjensen   Improve replication memory usage
 * 
 * </pre>
 * 
 * @author tjensen
 * @version 1.0
 */
public class ReplicationSiteEventDao extends
        SessionManagedDao<ReplicationSiteEventId, ReplicationSiteEvent> {

    private static final String DELETE_MAPPINGS = "DELETE from ReplicationSiteEvent site where site.id.eventId in :eventIds and site.id.registryId = :registryId";

    private static final String DELETE_ALL_MAPPINGS_FOR_SITE = "DELETE from ReplicationSiteEvent site where site.id.registryId = :registryId";

    private static final String DELETE_EVENTS_BY_TIME = "DELETE from ReplicationSiteEvent site inner join site.event event where event.eventTime < :eventTime and site.id.registryId = :registryId";

    private static final String GET_EVENTS_BATCH = "select site.id.eventId from ReplicationSiteEvent site where site.id.registryId = :registryId order by site.id.eventId";

    private static final String GET_EXPIRED_EVENT_SITES_QUERY = "select distinct site.id.registryId from ReplicationSiteEvent site where site.event.eventTime < :cutoff";

    @Override
    protected Class<ReplicationSiteEvent> getEntityClass() {
        return ReplicationSiteEvent.class;
    }

    public void deleteMapping(List<Long> eventId, String registryId)
            throws DataAccessLayerException {
        this.executeHQLStatement(DELETE_MAPPINGS, "eventIds", eventId,
                "registryId", registryId);
    }

    @Transactional(propagation = Propagation.REQUIRED)
    public void clearSiteMappings(String registryId)
            throws DataAccessLayerException {
        this.executeHQLStatement(DELETE_ALL_MAPPINGS_FOR_SITE, "registryId",
                registryId);
    }

    public List<Long> getEventsBatch(String registryId, int syncBatchSize) {
        return this.executeHQLQuery(GET_EVENTS_BATCH, syncBatchSize,
                "registryId", registryId);
    }

    public void removeEventsBeforeTime(Long time, String registryId)
            throws DataAccessLayerException {
        this.executeHQLStatement(DELETE_EVENTS_BY_TIME, "eventTime", time,
                "registryId", registryId);
    }

    @Transactional(propagation = Propagation.REQUIRED)
    public List<String> getExpiredEventSites(long cutoff) {
        return this.executeHQLQuery(GET_EXPIRED_EVENT_SITES_QUERY, "cutoff",
                cutoff);
    }

}
