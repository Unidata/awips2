package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.Date;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.registry.federation.ReplicationSiteEvent;

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
 * 07/25/2019    7890     ksunil    added missing transactional directive to getEventsBatch.
 * Aug 29, 2019  7836     bsteffen  Keep track of only latest event for each site.
 * Jun 18, 2020  8066     skabasele Added the ability to delete by registry id
 * </pre>
 *
 * @author tjensen
 */
public class ReplicationSiteEventDao
        extends SessionManagedDao<String, ReplicationSiteEvent> {

    private static final String UPDATE_REGISTRY = "UPDATE ReplicationSiteEvent SET eventId = :eventId, eventTime = :eventTime WHERE registryId = :registryId AND (eventTime <= :eventTime or eventId < :eventId)";

    private static final String DELETE_BY_REGISTRY_ID = "Delete from ReplicationSiteEvent WHERE registryId = :registryId";

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReplicationSiteEventDao.class);

    @Override
    protected Class<ReplicationSiteEvent> getEntityClass() {
        return ReplicationSiteEvent.class;
    }

    @Transactional(propagation = Propagation.REQUIRED)
    public boolean updateSite(String registryId, Long eventId, Date eventTime)
            throws DataAccessLayerException {
        int count = this.executeHQLStatement(UPDATE_REGISTRY, "registryId",
                registryId, "eventId", eventId, "eventTime", eventTime);
        return count > 0;
    }

    @Transactional(propagation = Propagation.REQUIRED)
    public boolean deleteByRegistryId(String registryId) {
        int count = 0;
        try {
            count = this.executeHQLStatement(DELETE_BY_REGISTRY_ID,
                    "registryId", registryId);
        } catch (DataAccessLayerException e) {
            statusHandler
                    .error("Error deleteing registry by Id : " + registryId, e);
        }
        return count > 0;
    }
}
