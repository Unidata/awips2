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

import java.util.concurrent.atomic.AtomicBoolean;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.eventbus.AllowConcurrentEvents;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.SlotTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.events.DeleteSlotEvent;

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
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 11, 2013  1707     bphillip  Initial implementation
 * Jul 29, 2013  2191     bphillip  Added executors to remove orphaned slots and
 *                                  expired events
 * Jan 15, 2014  2613     bphillip  Added Hibernate flush() call
 * Feb 04, 2014  2769     bphillip  Removed flush and clear call
 * Feb 13, 2014  2769     bphillip  Refactored to no longer use executor threads
 * Apr 11, 2014  3011     bphillip  Added slot purging via event bus
 *                                  notifications
 * Apr 17, 2014  3011     bphillip  Delete slot events now contain strings
 * Apr 19, 2016  5424     dhladky   Debug level slot deletion.
 * Aug 09, 2016  5771     rjpeter   Allow concurrent event processing
 * Oct 25, 2019  7948     bsteffen  Purge each batch of auditable events in a
 *                                  separate transaction.
 * 
 * </pre>
 * 
 * @author bphillip
 */
@Service
public class RegistryGarbageCollector {

    /** The logger instance */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryGarbageCollector.class);

    /** Sentinel to denote if the garbage collection is currently running */
    private final AtomicBoolean running = new AtomicBoolean(false);

    /** Data access object for AuditableEventType */
    private AuditableEventTypeDao eventDao;

    private SlotTypeDao slotDao;

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
    public RegistryGarbageCollector(AuditableEventTypeDao eventDao,
            SlotTypeDao slotDao) {
        this();
        this.eventDao = eventDao;
        this.slotDao = slotDao;

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
        long t0 = System.currentTimeMillis();
        int total = 0;

        int count = 1;
        while (count > 0) {
            count = eventDao.purgeBatchOfExpiredEvents(DELETE_BATCH_SIZE);
            statusHandler
                    .debug("Deleted " + count + " expired Auditable Events");
            total += count;
        }
        long t1 = System.currentTimeMillis();
        statusHandler.info("Purged " + total + " Auditable events in "
                + TimeUtil.prettyDuration(t1 - t0));
    }

    @Subscribe
    @AllowConcurrentEvents
    @Transactional
    public void deleteOrphanedSlot(DeleteSlotEvent slotEvent) {

        if (!CollectionUtil.isNullOrEmpty(slotEvent.getSlotsToDelete())) {
            long start = TimeUtil.currentTimeMillis();
            statusHandler.debug("Deleting "
                    + slotEvent.getSlotsToDelete().size() + " slots...");
            slotDao.deleteBySlotId(slotEvent.getSlotsToDelete());
            statusHandler.debug("Deleted " + slotEvent.getSlotsToDelete().size()
                    + " slots in " + (TimeUtil.currentTimeMillis() - start)
                    + " ms");
        }

    }

}
