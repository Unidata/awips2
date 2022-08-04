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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executor;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.registry.constants.DeletionScope;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.format.BytesFormat;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryDao;
import com.raytheon.uf.edex.registry.ebxml.dao.ReplicationSiteEventDao;
import com.raytheon.uf.edex.registry.ebxml.services.RegistryRESTServices;
import com.raytheon.uf.edex.registry.ebxml.services.soap.RegistrySOAPServices;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;
import com.raytheon.uf.edex.registry.ebxml.util.RegistryIdUtil;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;

/**
 * Task to perform asynchronous transmission of {@link ReplicationEvent}s for a
 * single remote registry. The job should be run by calling {@link #schedule()}
 * which will automatically execute it using the Executor passed into the
 * constructor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 26, 2019  7836     bsteffen  Initial creation
 * Oct 31, 2019  7963     bsteffen  Ensure events aren't sent back to where they came from.
 * Nov 21, 2019  7977     bsteffen  Prevent job from being scheduled more than once.
 * Jul 13, 2020  8169     ksunil    code tweak to DELETE multiple objects in a single request
 * Aug 21, 2021  8641     rjpeter   Allow submits to be batched.
 * Sep 10, 2021  8656     rjpeter   Rerun job on runtime exception. Remove outer transaction 
 *                                  block and force smaller database transactions.
 *
 * </pre>
 *
 * @author bsteffen
 */
public class ReplicationJob implements Runnable {

    protected static final Logger logger = LoggerFactory
            .getLogger(ReplicationJob.class);

    private static final int REPLICATION_RETRY_ATTEMPTS = Integer
            .getInteger("ebxml-federation-retry-attemts", 3);

    private static final long REPLICATION_RETRY_DELAY = Long
            .getLong("ebxml-federation-retry-delay-seconds", 10)
            * TimeUtil.MILLIS_PER_SECOND;

    private static final long MEMORY_BATCH_SIZE = BytesFormat
            .parseSystemProperty("ebxml-federation-repl-memory-batch-size",
                    "5MiB");

    private static final long PERMIT_SIZE = BytesFormat.parseSystemProperty(
            "ebxml-federation-repl-memory-permit-size", "10MiB");

    private static final int MAX_PERMITS = Integer
            .getInteger("ebxml-federation-repl-memory-permits", 100);

    /**
     * When replication for a registry is older than this threshold log a
     * warning.
     */
    private static final long REPLICATION_TIME_WARN_THRESHOLD = Long
            .getLong("ebxml-federation-warning-threshold", 30)
            * TimeUtil.MILLIS_PER_MINUTE;

    /**
     * When replication for a registry is older than this threshold log an
     * error.
     */
    private static final long REPLICATION_TIME_ERROR_THRESHOLD = Long
            .getLong("ebxml-federation-error-threshold", 60)
            * TimeUtil.MILLIS_PER_MINUTE;

    private final String registryId;

    private final ReplicationSiteEventDao replicationSiteEventDao;

    private final RegistryDao registryDao;

    private final RegistrySOAPServices soapService;

    private final RegistryRESTServices restClient;

    private final Executor gatherThreadPool;

    private final ReplicationCache cache;

    private final AtomicInteger failures = new AtomicInteger(0);

    private volatile RegistryType registry;

    private volatile LifecycleManager lcm;

    private volatile ReplicationEvent lastEvent;

    /*
     * Indicates synchronize occured, stop anything currently running and check
     * the replication time in the database before sending anything else.
     */
    private volatile boolean reset = false;

    private volatile boolean running = false;

    private volatile boolean runAgain = false;

    public ReplicationJob(String registryId,
            ReplicationSiteEventDao replicationSiteEventDao,
            RegistryDao registryDao, RegistrySOAPServices soapService,
            RegistryRESTServices restClient, Executor gatherThreadPool,
            ReplicationCache cache) {
        this.registryId = registryId;
        this.replicationSiteEventDao = replicationSiteEventDao;
        this.registryDao = registryDao;
        this.soapService = soapService;
        this.restClient = restClient;
        this.gatherThreadPool = gatherThreadPool;
        this.cache = cache;
    }

    /**
     * Schedule this job to be run as soon as a thread is available.
     */
    public void schedule() {
        try {
            synchronized (this) {
                if (!running && !runAgain) {
                    gatherThreadPool.execute(this);
                }

                runAgain = true;
            }
        } catch (RejectedExecutionException e) {
            logger.error("Failed to schedule replication for {}", registryId,
                    e);
        }
    }

    /**
     * Stop sending the current batch and check the database for an updated
     * {@link ReplicationSiteEvent} before sending any more data. Use this after
     * a synchronize to bring this job up to date.
     */
    public void reset() {
        reset = true;
    }

    @Override
    public void run() {
        synchronized (this) {
            if (running) {
                /* Just in case. */
                return;
            }

            running = true;
            runAgain = false;

            if (reset) {
                failures.set(0);
                lastEvent = null;
                reset = false;
            }
        }
        boolean sentAny = false;
        try {
            sentAny = replicateBatch();
        } catch (Exception e) {
            logger.error("Sending events to {} has failed.", registryId, e);
            runAgain = true;
        } finally {
            synchronized (this) {
                running = false;
                if (runAgain || sentAny) {
                    /*
                     * If events were found, attempt to gather events for this
                     * registryId again. If no events were found, wait for a new
                     * event to process.
                     */
                    try {
                        gatherThreadPool.execute(this);
                    } catch (RejectedExecutionException e) {
                        /* Ensure schedule can try again */
                        runAgain = false;
                        throw e;
                    }
                    runAgain = true;
                }
            }
        }
    }

    protected Boolean replicateBatch() {
        int eventsFromSelf = 0;
        int eventsSent = 0;
        long t0 = System.currentTimeMillis();
        try {
            if (lcm == null && !loadLifecycleManager()) {
                /*
                 * lcm is set to null when sending an event fails and will
                 * remain null if the remote registry is down.
                 */
                return Boolean.FALSE;
            }

            List<ReplicationEvent> batch = loadBatch();
            if (batch == null || batch.isEmpty()) {
                logger.info("No new replication event(s) for {}", registryId);
                return Boolean.FALSE;
            }

            /*
             * we can send delete requests in bulk. But other requests one at a
             * time. Batch up all consecutive delete events. As soon as you find
             * a non-delete, first send a delete for all the deletes you have
             * queued up, then process the current event. That way the event
             * order of delete-create is still maintained.
             */

            List<ReplicationEvent> deleteEvents = new ArrayList<>(batch.size());
            List<ReplicationEvent> submitEvents = new ArrayList<>(batch.size());
            for (ReplicationEvent event : batch) {
                if (registryId.equals(event.getSource())) {
                    lastEvent = event;
                    eventsFromSelf += 1;
                } else if (!ActionTypes.delete.equals(event.getEventType())) {
                    if (!deleteEvents.isEmpty()) {
                        eventsSent += sendRemoveRequest(deleteEvents);
                        deleteEvents.clear();
                    }

                    submitEvents.add(event);
                } else {
                    if (!submitEvents.isEmpty()) {
                        eventsSent += sendSubmitRequest(submitEvents);
                        submitEvents.clear();
                    }

                    deleteEvents.add(event);
                }
                if (reset || lcm == null) {
                    break;
                }
            }

            if (!reset && lcm != null) {
                if (!deleteEvents.isEmpty()) {
                    eventsSent += sendRemoveRequest(deleteEvents);
                    deleteEvents.clear();
                }
                if (!submitEvents.isEmpty()) {
                    eventsSent += sendSubmitRequest(submitEvents);
                    submitEvents.clear();
                }
            }
        } finally {
            long t1 = System.currentTimeMillis();
            if (eventsFromSelf > 0) {
                logger.info(
                        "Skipped sending {} replication event(s) back to source for {}.",
                        eventsFromSelf, registryId);
            }
            if (eventsSent > 0) {
                logger.info("Sent {} replication event(s) to {} in {}",
                        eventsSent, registryId,
                        TimeUtil.prettyDuration(t1 - t0));
            }

            updateReplicationSiteEvent();
        }
        return eventsFromSelf > 0 || eventsSent > 0;
    }

    protected boolean loadLifecycleManager() {
        registry = registryDao
                .getById(registryId + FederationProperties.REGISTRY_SUFFIX);
        if (registry == null) {
            logger.warn("Registry [" + registryId
                    + "] not present in federation. Skipping...");
            return false;
        }

        if (!restClient.isRegistryAvailable(registry.getBaseURL())) {
            logger.warn("Unable to gather events for [" + registryId
                    + "]. Registry is currently unavailable.");
            return false;
        }

        lcm = soapService
                .getLifecycleManagerServiceForHost(registry.getBaseURL());
        return true;
    }

    protected List<ReplicationEvent> loadBatch() {
        List<ReplicationEvent> batch;
        long t0 = System.currentTimeMillis();
        if (lastEvent == null) {
            ReplicationSiteEvent event = replicationSiteEventDao
                    .getById(registryId);
            if (event == null) {
                logger.warn(
                        "No ReplicationSiteEvent for {} so replicating events starting now.",
                        registryId);
                batch = cache.getEventsAfterTime(new Date());
            } else if (event.needsSync()) {
                logger.error(
                        "{} has exceeded the maximum downtime and must synchronize.",
                        registryId);
                batch = Collections.emptyList();
            } else if (event.getEventId() == null) {
                batch = cache.getEventsAfterTime(event.getEventTime());
            } else {
                batch = cache.getEventsAfterId(event.getEventId());
            }
        } else {
            batch = cache.getEventsAfterId(lastEvent.getId());
        }
        if (!batch.isEmpty()) {
            ReplicationEvent event = batch.get(0);
            long t1 = System.currentTimeMillis();
            long latency = t1 - event.getEventTime().getTime();
            logger.info(
                    "Gathered {} events for {} site version {} in {}. Site Latency: {}",
                    batch.size(), registryId, registry.getVersionInfo(),
                    TimeUtil.prettyDuration(t1 - t0),
                    TimeUtil.prettyDuration(latency));
            if (latency > REPLICATION_TIME_ERROR_THRESHOLD) {
                logger.error(
                        "Registry {} is over {} behind in replication events. Investigation necessary if site has not been down for an extended period",
                        registryId, TimeUtil.prettyDuration(
                                REPLICATION_TIME_ERROR_THRESHOLD));
            } else if (latency > REPLICATION_TIME_WARN_THRESHOLD) {
                logger.warn(
                        "Registry {} is over {} behind in replication events. Investigation may be necessary if site has not been down for an extended period",
                        registryId, TimeUtil.prettyDuration(
                                REPLICATION_TIME_WARN_THRESHOLD));
            }
        }

        return batch;
    }

    protected int sendRemoveRequest(List<ReplicationEvent> events) {
        List<ObjectRefType> refs = new ArrayList<>(events.size());
        for (ReplicationEvent event : events) {
            refs.add(new ObjectRefType(event.getObjectId()));
        }
        ObjectRefListType refList = new ObjectRefListType();
        refList.setObjectRef(refs);
        RemoveObjectsRequest removeRequest = new RemoveObjectsRequest(
                "Replicate - Remove events", "", null, null, refList, false,
                true, DeletionScope.DELETE_ALL);
        removeRequest.getSlot()
                .add(new SlotType(EbxmlObjectUtil.EVENT_SOURCE_SLOT,
                        new StringValueType(RegistryIdUtil.getId())));
        int attempts = 0;
        while (attempts < REPLICATION_RETRY_ATTEMPTS && !reset) {
            attempts++;
            long t0 = TimeUtil.currentTimeMillis();
            try {
                logger.info("Sending removal of {} objects to {}:",
                        events.size(), registryId);
                lcm.removeObjectsQueued(removeRequest);
                long t1 = TimeUtil.currentTimeMillis();
                logger.info("Removed {} objects from {} in {}", events.size(),
                        registryId, TimeUtil.prettyDuration(t1 - t0));
                lastEvent = events.get(events.size() - 1);
                return events.size();
            } catch (Exception e) {
                long t1 = TimeUtil.currentTimeMillis();
                if (attempts >= REPLICATION_RETRY_ATTEMPTS) {
                    int failCount = failures.incrementAndGet();
                    logger.error(
                            "Attempt {} failed to remove {} objects from {} after {}. "
                                    + "Final attempt failed it will be skipped in replication. "
                                    + "{} objects skipped since last sync or restart of central. "
                                    + "{} will need resynced.",
                            attempts, events.size(), registryId,
                            TimeUtil.prettyDuration(t1 - t0), failCount,
                            registryId);
                    logger.error("Failed object IDs:"
                            + events.stream().map(ReplicationEvent::getObjectId)
                                    .collect(Collectors.joining(",")));
                    lastEvent = events.get(events.size() - 1);
                } else if (!restClient
                        .isRegistryAvailable(registry.getBaseURL())) {
                    logger.error(
                            "Attempt {} failed to remove {} objects from {} after {}. Connection was lost.",
                            attempts, events.size(), registryId,
                            TimeUtil.prettyDuration(t1 - t0), e);
                    lcm = null;
                    break;
                } else {
                    logger.error(
                            "Attempt {} failed to remove {} objects from {} after {}.  Retrying in {}...",
                            attempts, events.size(), registryId,
                            TimeUtil.prettyDuration(t1 - t0),
                            REPLICATION_RETRY_DELAY, e);

                    try {
                        Thread.sleep(REPLICATION_RETRY_DELAY);
                    } catch (InterruptedException e1) {
                        // skip
                    }
                }
            }
        }

        return 0;
    }

    protected int sendSubmitRequest(List<ReplicationEvent> events) {
        MemoryLimiter limiter = new MemoryLimiter(events);
        int rval = 0;

        while (limiter.hasRemaining() && !reset && lcm != null) {
            // log loading next batch
            int batchSize = limiter.loadNext();
            // log finish loading

            int objsSubmitted = sendSubmitRequest(limiter);
            rval += objsSubmitted;

            // double check we don't need to stop processing
            if (lcm == null) {
                break;
            }

            // if we didn't submit anything in bulk, we should try and submit
            // individually
            if (objsSubmitted == 0 && batchSize > 1) {
                // send batch individually
                limiter.processPreviousIndividually();

                for (int i = 0; i < batchSize && !reset; i++) {
                    rval += sendSubmitRequest(limiter);

                    if (lcm == null) {
                        break;
                    }

                    limiter.completePrevious();
                }
            } else {
                limiter.completePrevious();
            }
        }

        return rval;
    }

    protected int sendSubmitRequest(MemoryLimiter limiter) {
        int attempts = 0;
        List<RegistryObjectType> objects = limiter.getObjects();
        SlotSize slotSize = limiter.getSlotSize();
        int batchSize = objects.size();

        if (batchSize < 1) {
            /*
             * handle edge case where last item to replicate in batch was
             * already deleted
             */
            return 0;
        }

        String sizeStr = new BytesFormat().format(slotSize.getTotalSlotSize());
        SubmitObjectsRequest submitRequest = new SubmitObjectsRequest();
        submitRequest.setId("Replicate - Insert/Update events");
        submitRequest.setCheckReferences(false);
        submitRequest.setMode(Mode.CREATE_OR_REPLACE);
        submitRequest.setUsername(RegistryUtil.registryUser);
        RegistryObjectListType objList = new RegistryObjectListType();
        objList.setRegistryObject(objects);
        submitRequest.setRegistryObjectList(objList);
        submitRequest.getSlot()
                .add(new SlotType(EbxmlObjectUtil.EVENT_SOURCE_SLOT,
                        new StringValueType(RegistryIdUtil.getId())));

        while (attempts < REPLICATION_RETRY_ATTEMPTS && !reset) {
            attempts++;
            long t0 = TimeUtil.currentTimeMillis();

            try (MemorySemaphore mem = limiter.lock()) {
                logger.info(
                        "Sending {} insert/update objects, approx total size {}, to {}",
                        batchSize, sizeStr, registryId);
                long t1 = TimeUtil.currentTimeMillis();
                lcm.submitObjectsQueued(submitRequest);
                long t2 = TimeUtil.currentTimeMillis();
                logger.info("Sent {} insert/update to {} in {}", batchSize,
                        registryId, TimeUtil.prettyDuration(t2 - t1));
                return batchSize;
            } catch (Exception e) {
                long t1 = TimeUtil.currentTimeMillis();
                if (attempts >= REPLICATION_RETRY_ATTEMPTS) {
                    if (batchSize == 1) {
                        int failCount = failures.incrementAndGet();
                        logger.error(
                                "Attempt {} failed to submit {} to {} after {}. "
                                        + "Final attempt failed it will be skipped in replication. "
                                        + "{} objects skipped since last sync or restart of central. "
                                        + "{} will need resynced.",
                                attempts, objects.get(0).getLid(), registryId,
                                TimeUtil.prettyDuration(t1 - t0), failCount,
                                registryId, e);
                    } else {
                        /**
                         * Assumes logic in
                         * sendSubmitRequest(List<ReplicationEvent>) has not
                         * changed.
                         */
                        logger.error(
                                "Attempt {} failed to submit {} objects to {} after {}. "
                                        + "Final attempt to send batch failed. Will attempt sending objects individually",
                                attempts, batchSize, registryId,
                                TimeUtil.prettyDuration(t1 - t0), e);
                    }
                } else if (!restClient
                        .isRegistryAvailable(registry.getBaseURL())) {
                    if (batchSize == 1) {
                        logger.error(
                                "Attempt {} failed to submit {} to {} after {}. Connection was lost.",
                                attempts, objects.get(0).getLid(), registryId,
                                TimeUtil.prettyDuration(t1 - t0), e);
                    } else {
                        logger.error(
                                "Attempt {} failed to submit batch of {} to {} after {}. Connection was lost.",
                                attempts, batchSize, registryId,
                                TimeUtil.prettyDuration(t1 - t0), e);
                    }

                    lcm = null;
                    break;
                } else {
                    if (batchSize == 1) {
                        logger.error(
                                "Attempt {} failed to submit {} to {} after {}. Retrying in {}...",
                                attempts, objects.get(0).getLid(), registryId,
                                TimeUtil.prettyDuration(t1 - t0),
                                TimeUtil.prettyDuration(
                                        REPLICATION_RETRY_DELAY),
                                e);
                    } else {
                        logger.error(
                                "Attempt {} failed to submit batch of {} to {} after {}. Retrying in {}...",
                                attempts, batchSize, registryId,
                                TimeUtil.prettyDuration(t1 - t0),
                                TimeUtil.prettyDuration(
                                        REPLICATION_RETRY_DELAY),
                                e);
                    }

                    try {
                        Thread.sleep(REPLICATION_RETRY_DELAY);
                    } catch (InterruptedException e1) {
                        // skip
                    }
                }
            }
        }

        return 0;
    }

    protected void updateReplicationSiteEvent() {
        if (lastEvent != null) {
            try {
                if (!replicationSiteEventDao.updateSite(registryId,
                        lastEvent.getId(), lastEvent.getEventTime())) {
                    /*
                     * Something else updated the table before we got there.
                     */
                    lastEvent = null;
                }
            } catch (DataAccessLayerException e) {
                logger.error("Error occurred updating replication for site {}",
                        registryId, e);
            }
        }
    }

    /**
     * This class is used to prevent running out of memory while serializing
     * gigantic strings to xml. For example there could be a RegistryObject with
     * a content slot with 67,000,000 characters in a String. This String will
     * use 128MiB of memory internally. During Serialization this String will be
     * passed to com.sun.xml.bind.v2.runtime.output.Encoded.setEscape(String,
     * boolean) which allocates a buffer with 6 bytes for every character,
     * allocating 284MiB. If there are dozens of replication jobs that all hit
     * that code at the same time an {@link OutOfMemoryError} occurs. This class
     * detects gigantic strings and uses a semaphore to limit the number of
     * concurrent jobs that can be sending them. Most registry objects are very
     * small and this class should have minimal impact for those cases.
     */
    protected final class MemoryLimiter {

        private final List<ReplicationEvent> events;

        // indexes that have already been deleted
        private Set<Integer> skippedEventIndexes = new HashSet<>();

        private int startEventIndex = 0;

        private int endEventIndex = 0;

        private List<RegistryObjectType> objects;

        private boolean processIndividually = false;

        private int individualObjectIndex = 0;

        private SlotSize totalBatchSize = new SlotSize();

        public MemoryLimiter(List<ReplicationEvent> events) {
            this.events = events;
            objects = new ArrayList<>(events.size());
        }

        public boolean hasRemaining() {
            return startEventIndex < events.size();
        }

        /**
         * Loads registry objects into memory and will continue loading until
         * batch is full.
         *
         * @return
         */
        public int loadNext() {
            objects.clear();
            skippedEventIndexes.clear();
            boolean loadMore = true;
            processIndividually = false;
            totalBatchSize = new SlotSize();

            while (loadMore && endEventIndex < events.size()) {
                ReplicationEvent event = events.get(endEventIndex);

                RegistryObjectType dbObject = cache.getRegistryObject(event);
                if (dbObject == null) {
                    logger.info(
                            "Registry object {} has already been deleted from registry before it could be synced to {}.",
                            event.getObjectId(), registryId);
                    skippedEventIndexes.add(endEventIndex);
                    endEventIndex += 1;
                    continue;
                }

                loadMore = false;
                SlotSize objSize = new SlotSize(dbObject);

                if (objects.isEmpty()) {
                    // nothing else in batch, add regardless
                    loadMore = true;
                } else if (totalBatchSize.withinBatchSize(objSize)) {
                    // object fits in batch size
                    loadMore = true;
                }

                if (loadMore) {
                    totalBatchSize.merge(objSize);
                    objects.add(dbObject);
                    endEventIndex += 1;

                    if (objSize.getPermitSize() > PERMIT_SIZE) {
                        BytesFormat bf = new BytesFormat();
                        logger.info(
                                "Replicating a large registry object to {}: lid={}, total slot size={}, encoder memory usage={}",
                                registryId, dbObject.getLid(),
                                bf.format(objSize.getTotalSlotSize()),
                                bf.format(objSize.getEncodedSize()));
                    }

                    /*
                     * double check if single object exceeds batch size this can
                     * happen when a large HS object comes in
                     */
                    if (totalBatchSize.getPermitSize() > MEMORY_BATCH_SIZE) {
                        loadMore = false;
                    }
                }
            }

            return objects.size();
        }

        /**
         * Process the previously loaded batch individually.
         */
        public void processPreviousIndividually() {
            /*
             * Only need to set flag as start isn't moved until completePrevious
             * is called.
             */
            processIndividually = true;
            individualObjectIndex = 0;

            while (skippedEventIndexes.contains(startEventIndex)) {
                // handle events that were already deleted.
                lastEvent = events.get(startEventIndex);
                startEventIndex += 1;
            }
        }

        /**
         * Called to mark that the processing of the loaded batch is complete.
         * This updates the associated indexes and lastEvent sent.
         */
        public void completePrevious() {
            if (processIndividually) {
                if (startEventIndex < endEventIndex) {
                    lastEvent = events.get(startEventIndex);
                    startEventIndex += 1;
                    individualObjectIndex += 1;

                    // handle events that were already deleted.
                    while (skippedEventIndexes.contains(startEventIndex)
                            && startEventIndex < endEventIndex) {
                        lastEvent = events.get(startEventIndex);
                        startEventIndex += 1;
                    }
                }
            } else {
                lastEvent = events.get(endEventIndex - 1);
                startEventIndex = endEventIndex;
            }
        }

        public List<RegistryObjectType> getObjects() {
            if (processIndividually) {
                if (individualObjectIndex < objects.size()) {
                    return Collections
                            .singletonList(objects.get(individualObjectIndex));
                } else {
                    // shouldn't happen, but...
                    return Collections.emptyList();
                }
            }

            return objects;
        }

        public SlotSize getSlotSize() {
            if (processIndividually) {
                if (individualObjectIndex < objects.size()) {
                    return new SlotSize(objects.get(individualObjectIndex));
                } else {
                    // shouldn't happen, but...
                    return new SlotSize();
                }
            }

            return totalBatchSize;
        }

        public MemorySemaphore lock() {
            return new MemorySemaphore(registryId, getObjects(), getSlotSize());
        }
    }

    protected class SlotSize {
        private long totalSlotSize = 0;

        private long largestSlotSize = 0;

        private long encodedSize = 0;

        public SlotSize() {
        }

        public SlotSize(RegistryObjectType object) {
            for (SlotType slot : object.getSlot()) {
                ValueType value = slot.getSlotValue();

                // instanceof guarantees a null check
                if (value instanceof StringValueType) {
                    String str = ((StringValueType) value).getStringValue();

                    if (str != null) {
                        int size = str.length();

                        if (size > largestSlotSize) {
                            largestSlotSize = size;
                        }

                        totalSlotSize += size;
                    }
                }
            }

            encodedSize = calcEncodedSize(largestSlotSize);
        }

        protected long calcEncodedSize(long val) {
            /*
             * Match the memory consumed in
             * com.sun.xml.bind.v2.runtime.output.Encoded.setEscape( String,
             * boolean)
             */
            return (6 * val + 1);
        }

        public long getEncodedSize() {
            return encodedSize;
        }

        public void merge(SlotSize that) {
            this.totalSlotSize += that.totalSlotSize;
            this.largestSlotSize = Math.max(that.largestSlotSize,
                    this.largestSlotSize);
            this.encodedSize = calcEncodedSize(this.largestSlotSize);
        }

        public boolean withinBatchSize(SlotSize that) {
            long val = this.totalSlotSize + that.totalSlotSize
                    + Math.max(this.encodedSize, that.encodedSize);
            return val < MEMORY_BATCH_SIZE;
        }

        public long getTotalSlotSize() {
            return totalSlotSize;
        }

        public long getPermitSize() {
            return totalSlotSize + encodedSize;
        }
    }

    protected static final class MemorySemaphore implements AutoCloseable {

        private static final Semaphore sem = new Semaphore(MAX_PERMITS, true);

        private final int permits;

        private final int batchSize;

        public MemorySemaphore(String registryId,
                List<RegistryObjectType> objects, SlotSize totalObjSize) {
            this.batchSize = objects.size();

            if (totalObjSize.getPermitSize() > PERMIT_SIZE) {
                String sizeStr = new BytesFormat()
                        .format(totalObjSize.getPermitSize());
                int permits = (int) (totalObjSize.getPermitSize()
                        / PERMIT_SIZE);
                if (permits > MAX_PERMITS) {
                    permits = MAX_PERMITS;

                    // shouldn't ever happen size a batch size should be
                    // less than a permit
                    logger.warn(
                            "Replicating a very large registry batch to {} will require significant memory: batchSize={}, memory={}",
                            registryId, batchSize, sizeStr);
                }

                this.permits = permits;

                /*
                 * Used for error handling, set to true immediately after
                 * acquire and only set to false if everything completes
                 * successfully.
                 */
                boolean needsToRelease = false;
                try {
                    if (sem.tryAcquire(permits)) {
                        needsToRelease = true;
                        logger.info(
                                "Replicating a large registry batch to {}: items in batch={}, memory={}, permits={} avail={}/{}",
                                registryId, batchSize, sizeStr, permits,
                                sem.availablePermits(), MAX_PERMITS);
                    } else {
                        logger.info(
                                "Replicating a large registry batch to {} may delay replication: items in batch={}, memory={}, permits={} avail={}/{}",
                                registryId, batchSize, sizeStr, permits,
                                sem.availablePermits(), MAX_PERMITS);
                        long t0 = System.currentTimeMillis();
                        sem.acquireUninterruptibly(permits);
                        needsToRelease = true;
                        long t1 = System.currentTimeMillis();
                        String timeStr = TimeUtil.prettyDuration(t1 - t0);
                        logger.info(
                                "Replicating a large registry {} to {} was delayed by {}: permits={} avail={}/{}",
                                (batchSize == 1 ? "object" : "batch"),
                                registryId, timeStr, permits,
                                sem.availablePermits(), MAX_PERMITS);
                    }
                    needsToRelease = false;
                } finally {
                    if (needsToRelease) {
                        sem.release(permits);
                    }
                }
            } else {
                this.permits = 0;
            }
        }

        @Override
        public void close() {
            if (permits > 0) {
                sem.release(permits);
                logger.info(
                        "Finished replicating a large registry batch: permits={} avail={}/{}",
                        permits, sem.availablePermits(), MAX_PERMITS);
            }
        }
    }
}
