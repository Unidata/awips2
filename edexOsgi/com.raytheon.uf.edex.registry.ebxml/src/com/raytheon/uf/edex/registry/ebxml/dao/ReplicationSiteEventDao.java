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

import java.util.Date;

import org.hibernate.TransactionException;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.annotation.Propagation;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.IDaoConfigFactory;
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
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- ---------------------------------------------------------------
 * Aug 08, 2016  5810     tjensen    Initial creation
 * Aug 16, 2016  5810     tjensen    Fix issue with delete by time
 * Sep 01, 2016  5810     tjensen    Improve replication memory usage
 * Jul 25, 2019  7890     ksunil     added missing transactional directive to getEventsBatch.
 * Aug 29, 2019  7836     bsteffen   Keep track of only latest event for each site.
 * Jun 18, 2020  8066     skabasele  Added the ability to delete by registry id
 * Apr 14, 2021  7849     mapeters   Refactor to use TransactionTemplate.execute instead of
 *                                   Transactional annotations
 *
 * </pre>
 *
 * @author tjensen
 */
public class ReplicationSiteEventDao
        extends SessionManagedDao<String, ReplicationSiteEvent> {

    private static final String UPDATE_REGISTRY = "UPDATE ReplicationSiteEvent SET eventId = :eventId, eventTime = :eventTime WHERE registryId = :registryId AND (eventTime <= :eventTime or eventId < :eventId)";

    private static final String DELETE_BY_REGISTRY_ID = "Delete from ReplicationSiteEvent WHERE registryId = :registryId";

    public ReplicationSiteEventDao(IDaoConfigFactory daoConfigFactory) {
        super(daoConfigFactory);
    }

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReplicationSiteEventDao.class);

    @Override
    protected Class<ReplicationSiteEvent> getEntityClass() {
        return ReplicationSiteEvent.class;
    }

    public boolean updateSite(String registryId, Long eventId, Date eventTime)
            throws DataAccessLayerException {
        TransactionDefinition transactionDef = getTransactionDef(
                Propagation.REQUIRED);
        try {
            return supplyInTransaction(transactionDef, () -> {
                try {
                    int count = this.executeHQLStatement(UPDATE_REGISTRY,
                            "registryId", registryId, "eventId", eventId,
                            "eventTime", eventTime);
                    return count > 0;
                } catch (DataAccessLayerException e) {
                    throw new TransactionException("Error updating site", e);
                }
            });
        } catch (TransactionException e) {
            String msg = String.format(
                    "Error updating site for registryId=%s, eventId=%s, eventTime=%s",
                    registryId, eventId, eventTime);
            throw new DataAccessLayerException(msg, e);
        }
    }

    public boolean deleteByRegistryId(String registryId) {
        TransactionDefinition transactionDef = getTransactionDef(
                Propagation.REQUIRED);
        return supplyInTransaction(transactionDef, () -> {
            int count = 0;
            try {
                count = this.executeHQLStatement(DELETE_BY_REGISTRY_ID,
                        "registryId", registryId);
            } catch (DataAccessLayerException e) {
                statusHandler.error(
                        "Error deleting registry by Id : " + registryId, e);
            }
            return count > 0;
        });
    }
}
