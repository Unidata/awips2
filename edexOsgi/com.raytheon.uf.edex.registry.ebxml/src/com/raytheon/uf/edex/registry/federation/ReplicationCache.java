/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
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
package com.raytheon.uf.edex.registry.federation;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.LinkedBlockingDeque;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.dao.ReplicationEventDao;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

/**
 * Cache of {@link ReplicationEvent}s and their associated
 * {@link RegistryObjectType}s that is used to reduce redundant loading of
 * objects form the database.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 26, 2019  7836     bsteffen  Initial creation
 * Sep 09, 2021  8656     rjpeter   Force objects to be fully initialized.
 *
 * </pre>
 *
 * @author bsteffen
 */
public class ReplicationCache {

    protected static final Logger logger = LoggerFactory
            .getLogger(ReplicationCache.class);

    private static final int MAX_LIVE = 1024;

    private final ReplicationEventDao replicationEventDao;

    private final RegistryObjectDao registryObjectDao;

    private final Deque<ReplicationEvent> liveEvents = new LinkedBlockingDeque<>(
            MAX_LIVE);

    /**
     * Access must be synchronized on registryObjectCache
     */
    private final Map<String, Reference<RegistryObjectType>> registryObjectCache = new SelfCleaningReferenceMap<>();

    /**
     * Access must be synchronized on registryObjectCache
     */
    private final Map<String, Object> registryObjectRequestLocks = new HashMap<>();

    public ReplicationCache(ReplicationEventDao replicationEventDao,
            RegistryObjectDao registryObjectDao) {
        this.replicationEventDao = replicationEventDao;
        this.registryObjectDao = registryObjectDao;
    }

    /**
     * Add an event to the cache. Events must be added in order
     * 
     * @param event
     *            the event to add
     * @param object
     *            the object that the event is modifying.
     */
    public void addLiveEvent(ReplicationEvent event,
            RegistryObjectType object) {
        synchronized (registryObjectCache) {
            if (ActionTypes.delete.equals(event.getEventType())) {
                registryObjectCache.remove(event.getObjectId());
            } else {
                registryObjectCache.put(event.getObjectId(),
                        new SoftReference<>(object));
            }
        }
        while (!liveEvents.offerLast(event)) {
            liveEvents.pollFirst();
        }
    }

    /**
     * Get a batch of events after the specified id.
     * 
     * @param lastId
     *            the id of the last sent event.
     * @return any events newer than the arguments, or an empty list if there
     *         are none. Never null.
     */
    public List<ReplicationEvent> getEventsAfterId(long lastId) {
        List<ReplicationEvent> result = null;
        for (ReplicationEvent queuedEvent : liveEvents) {
            long queuedId = queuedEvent.getId();
            if (lastId == queuedId) {
                result = new ArrayList<>();
            } else if (result != null) {
                result.add(queuedEvent);
            }
        }
        if (result == null) {
            logger.info("Replication Cache miss for id {}.", lastId);
            result = replicationEventDao.getEventsAfterId(lastId, MAX_LIVE);
        }
        return result;
    }

    /**
     * Get a batch of events with an eventTime after the specified time.
     * 
     * @param lastTime
     *            the time of the last synchronize.
     * @return any events newer than the specified time, or an empty list if
     *         there are none. Never null.
     */
    public List<ReplicationEvent> getEventsAfterTime(Date lastTime) {
        /*
         * Sometimes lastTime is a java.sql.Timestamp, when a java.util.Date is
         * compared to a Timestamp the millis are not handled correctly so need
         * to normalize all times to the exact same type, Instant is easy
         * enough.
         */
        Instant lastInstant = lastTime.toInstant();
        List<ReplicationEvent> result = null;
        for (ReplicationEvent queuedEvent : liveEvents) {
            Instant queuedTime = queuedEvent.getEventTime().toInstant();
            if (queuedTime.isBefore(lastInstant)) {
                if (result == null) {
                    result = new ArrayList<>();
                }
            } else if (result != null) {
                result.add(queuedEvent);
            }
        }
        if (result == null) {
            logger.info("Replication Cache miss for time {}.", lastTime);
            result = replicationEventDao.getEventsAfterTime(lastTime, MAX_LIVE);
        }
        return result;
    }

    /**
     * Get the Registry Object for the specified replication event. This will
     * used a cached version if possible, if another thread is requesting the
     * same object it will wait, otherwise the object will be loaded from the
     * database.
     * 
     * @param event
     *            the replication event that changed a RegistryObject.
     * @return the RegistryObject, or null if it is no longer in the database.
     */
    public RegistryObjectType getRegistryObject(ReplicationEvent event) {
        String id = event.getObjectId();

        Object sync = new Object();
        RegistryObjectType result = null;
        boolean syncIsInMap = false;
        /*
         * This loop just restarts the checking/loading until the object is in
         * the cache or the loading can be synchronized on the shared lock. If
         * there is no other threads requesting then this loop only executes
         * once. If there is another thread already doing the request then it
         * will come around a second time to synchronize on the correct lock. It
         * should not normally come around more than that although it is
         * possible for deleted objects or very aggressive garbage collection.
         */
        while (result == null && !syncIsInMap) {
            synchronized (sync) {
                synchronized (registryObjectCache) {
                    Reference<RegistryObjectType> ref = registryObjectCache
                            .get(id);
                    if (ref != null) {
                        result = ref.get();
                        if (result == null) {
                            registryObjectCache.remove(id);
                        }
                    }
                    if (result == null) {
                        Object lock = registryObjectRequestLocks.putIfAbsent(id,
                                sync);
                        if (lock == null || lock == sync) {
                            syncIsInMap = true;
                        } else {
                            /*
                             * This causes the next iteration through the loop
                             * to wait until the thread that inserted the lock
                             * is done.
                             */
                            sync = lock;
                        }
                    }
                }
                /*
                 * Very important for the actual load to happen while not
                 * synchronized on registryObjectCache so multiple threads can
                 * be loading for different objects.
                 */
                if (syncIsInMap) {
                    long t0 = TimeUtil.currentTimeMillis();
                    try {
                        result = registryObjectDao.getByIdFullyInitialized(id);
                    } finally {
                        long t1 = TimeUtil.currentTimeMillis();
                        synchronized (registryObjectCache) {
                            if (result != null) {
                                logger.info(
                                        "Retrieved registry object [{}] from local registry in {}",
                                        id, TimeUtil.prettyDuration(t1 - t0));
                                registryObjectCache.putIfAbsent(id,
                                        new SoftReference<>(result));
                            }
                            registryObjectRequestLocks.remove(id);
                        }
                    }
                }
            }
        }
        return result;
    }

    /**
     * Map which manages the removal of cleared references internally. When
     * using this map there is no need to periodically check for and remove
     * cleared references because that is handled internally. Retrieval
     * operations may still return entries that have been cleared so it is still
     * necessary to check for cleared references when getting values from the
     * map.
     */
    private static class SelfCleaningReferenceMap<K, V>
            extends LinkedHashMap<K, Reference<V>> {

        private static final long serialVersionUID = 1L;

        private int maxSize = MAX_LIVE * 4;

        @Override
        protected boolean removeEldestEntry(Entry<K, Reference<V>> eldest) {
            int beforeSize = size();
            if (eldest.getValue().get() == null) {
                /*
                 * This is quick and very incremental and should handle most
                 * normal cleanup.
                 */
                return true;
            } else if (beforeSize > maxSize) {
                /*
                 * This is the fallback and should only happen if the usage and
                 * garbage collection patterns are sporadic.
                 */
                Iterator<Entry<K, Reference<V>>> it = entrySet().iterator();
                while (it.hasNext()) {
                    if (it.next().getValue().get() == null) {
                        it.remove();
                    }
                }
                int afterSize = size();
                int change = beforeSize - afterSize;
                if (change < MAX_LIVE) {
                    /*
                     * Iterating the entire map hurts concurrency so if it isn't
                     * finding enough references to remove increase the maxSize.
                     * 
                     * If the map is getting too large it will increase memory
                     * pressure and cause the garbage collector to clear
                     * references faster which will leave many references to
                     * clear and stop the growth naturally.
                     * 
                     * For smaller values of maxSize this math will almost
                     * double maxSize but larger values will grow more slowly
                     * and it never exceeds Integer.MAX_VALUE.
                     */
                    maxSize = (int) (2l * maxSize * Integer.MAX_VALUE
                            / ((long) maxSize + Integer.MAX_VALUE));
                    logger.info(
                            "Cache cleanup removed {} entries({} -> {}), maxSize has increased to {}.",
                            change, beforeSize, afterSize, maxSize);
                } else {
                    logger.info("Cache cleanup removed {} entries({} -> {}).",
                            change, beforeSize, afterSize);
                }
                return false;
            } else {
                return false;
            }
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = super.hashCode();
            result = prime * result + maxSize;
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (!super.equals(obj)) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            if (maxSize != ((SelfCleaningReferenceMap<?, ?>) obj).maxSize) {
                return false;
            }
            return true;
        }

    }

}
