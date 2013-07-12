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

import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.SlotTypeDao;
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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryGarbageCollector {

    /** Sentinel to denote if the garbage collection is currently running */
    private AtomicBoolean running = new AtomicBoolean(false);

    /** Data access object for SlotType */
    private SlotTypeDao slotDao;

    /** Data access object for AuditableEventType */
    private AuditableEventTypeDao eventDao;

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
    public RegistryGarbageCollector(SlotTypeDao slotDao,
            AuditableEventTypeDao eventDao) {
        this.slotDao = slotDao;
        this.eventDao = eventDao;
    }

    /**
     * Cleans up the registry by removing unused and/or orphaned objects
     */
    public void gc() throws EbxmlRegistryException {
        if (!running.compareAndSet(false, true)) {
            return;
        }
        try {
            eventDao.deleteExpiredEvents();
            slotDao.deleteOrphanedSlots();
        } finally {
            running.set(false);
        }
    }

}
