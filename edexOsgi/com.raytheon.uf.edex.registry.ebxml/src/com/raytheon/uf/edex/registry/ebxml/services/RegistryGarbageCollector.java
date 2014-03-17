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
import java.util.concurrent.atomic.AtomicBoolean;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * 
 * Garbage collector object to clean up unused and orphaned objects from the
 * registry. The gc method is run on a cron to continuously clean up the
 * registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/11/2013    1707        bphillip    Initial implementation
 * 7/29/2013    2191        bphillip    Added executors to remove orphaned slots and expired events
 * 1/15/2014    2613        bphillip    Added Hibernate flush() call
 * 2/4/2014     2769        bphillip    Removed flush and clear call
 * 2/13/2014    2769        bphillip    Refactored to no longer use executor threads
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Service
@Transactional
public class RegistryGarbageCollector {

    /** The logger instance */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryGarbageCollector.class);

    /** Sentinel to denote if the garbage collection is currently running */
    private AtomicBoolean running = new AtomicBoolean(false);

    /** Data access object for AuditableEventType */
    private AuditableEventTypeDao eventDao;

    /** The number of events to delete per batch */
    private static final int DELETE_BATCH_SIZE = 100;

    /**
     * Creates a new GarbageCollector object
     */
    public RegistryGarbageCollector() {
    }

    /**
     * Creates a new GarbageCollector object
     * 
     * @param slotDao
     *            The slot dao to use
     * @param eventDao
     *            The auditable event dao to use
     */
    public RegistryGarbageCollector(AuditableEventTypeDao eventDao) {
        this();
        this.eventDao = eventDao;

    }

    /**
     * Cleans up the registry by removing unused and/or orphaned objects
     */
    public void gc() {

        if (!running.compareAndSet(false, true)) {
            return;
        }
        try {
            purgeExpiredEvents();
        } catch (EbxmlRegistryException e) {
            statusHandler.error("Error purging auditable events!", e);
        } finally {
            running.set(false);
        }
    }

    /**
     * Purges expired events older than 2 days
     * 
     * @throws EbxmlRegistryException
     *             If errors occur while enqueuing events to be deleted
     */
    private void purgeExpiredEvents() throws EbxmlRegistryException {

        List<AuditableEventType> expiredEvents = null;
        do {
            expiredEvents = eventDao.getExpiredEvents(DELETE_BATCH_SIZE);
            if (!expiredEvents.isEmpty()) {
                statusHandler.info("Deleting " + expiredEvents.size()
                        + " expired Auditable Events");
                eventDao.deleteAll(expiredEvents);
            }
        } while (!expiredEvents.isEmpty());
    }
}
