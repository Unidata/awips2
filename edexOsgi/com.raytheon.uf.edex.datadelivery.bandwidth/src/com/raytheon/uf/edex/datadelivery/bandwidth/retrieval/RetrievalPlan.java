package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Preconditions;
import com.google.common.collect.Sets;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * Class used to hold the current scheduled and available bandwidth for a
 * particular route. {@link RetrievalManager} uses this Class to hold the
 * scheduled BandwidthAllocations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2012 726        jspinks      Initial release.
 * Oct 16, 2012 0726       djohnson     Fix bug never updating allocations in memory.
 * Oct 23, 2012 1286       djohnson     Add ability to get/set the default bandwidth.
 * Nov 20, 2012 1286       djohnson     Handle null bucketIds being returned.
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class RetrievalPlan {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalPlan.class);

    /**
     * Representation of a bucket of bandwidth. Any methods that directly access
     * fields or mutate objects should be private, and all access should go
     * through {@link RetrievalPlan}.
     */
    public class BandwidthBucket implements Comparable<BandwidthBucket> {

        // Number of allocated bytes
        private long currentSize;

        private final List<BandwidthReservation> reservations = new ArrayList<BandwidthReservation>();

        private final List<BandwidthAllocation> allocations = new ArrayList<BandwidthAllocation>();

        // Number of bytes of bandwidth;
        private final long bucketSize;

        private final long bucketStartTime;

        public BandwidthBucket(long bucketStartTime, long sizeInBytes) {
            this.bucketStartTime = bucketStartTime;
            this.bucketSize = sizeInBytes;
        }

        /**
         * Add the allocation. Private because all interaction should go through
         * RetrievalPlan.
         * 
         * @param allocation
         *            the allocation to add
         */
        private void add(BandwidthAllocation allocation) {
            allocations.add(allocation);
            currentSize += allocation.getEstimatedSizeInBytes();
        }

        /**
         * Add the reservation. Private because all interaction should go
         * through RetrievalPlan.
         * 
         * @param reservation
         *            the reservation to add
         */
        private void add(BandwidthReservation reservation) {
            reservations.add(reservation);
            currentSize += reservation.getSizeInBytes();
        }

        public long getAvailableBandwidth() {
            return Math.max(0, bucketSize - currentSize);
        }

        public long getBucketSize() {
            return bucketSize;
        }

        public long getBucketStartTime() {
            return bucketStartTime;
        }

        public long getCurrentSize() {
            return currentSize;
        }

        /**
         * Get the next allocation to be processed. Private because all access
         * should go through RetrievalPlan.
         * 
         * @param agentType
         *            the agent type
         * @return the allocation, or null if there is none to process
         */
        private BandwidthAllocation getNextReservation(String agentType) {
            BandwidthAllocation allocation = null;
            for (BandwidthAllocation o : allocations) {
                if (RetrievalStatus.READY.equals(o.getStatus())
                        && o.getAgentType().equals(agentType)) {
                    allocation = o;
                    allocation.setStatus(RetrievalStatus.PROCESSING);
                    // Persist this change to the database
                    bandwidthDao.update(allocation);
                    break;
                }
            }
            return allocation;
        }

        /**
         * A read-only look at what requests are available in this bucket.
         * 
         * @return the unmodifiable list of requests
         */
        public List<BandwidthAllocation> getRequests() {
            return Collections.unmodifiableList(allocations);
        }

        /**
         * A read-only look at what reservations are available in this bucket.
         * 
         * @return the unmodifiable list of reservations
         */
        public List<BandwidthReservation> getReservations() {
            return Collections.unmodifiableList(reservations);
        }

        public String showReservations() {
            StringBuilder sb = new StringBuilder();
            sb.append(toString()).append("\n");

            for (BandwidthAllocation allocation : allocations) {
                sb.append("  ").append(allocation.toString()).append("\n");
            }
            for (BandwidthReservation reservation : reservations) {
                sb.append("  ").append(reservation.toString()).append("\n");
            }

            return sb.toString();
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            Calendar b = BandwidthUtil.now();
            b.setTimeInMillis(bucketStartTime);
            sb.append("Bucket [").append(BandwidthUtil.format(b));
            sb.append("] bandwidth [").append(bucketSize);
            sb.append("] available [").append(getAvailableBandwidth())
                    .append("]);");

            return sb.toString();
        }

        /**
         * Remove the bandwidth allocation or reservation by its id. Private
         * because all access should go through RetrievalPlan.
         * 
         * @param id
         *            the id of the allocation/reservation
         */
        private void remove(long id) {

            
            for (Iterator<BandwidthAllocation> itr = allocations.iterator();itr.hasNext();) {
                BandwidthAllocation reservation = itr.next();
                if (reservation.getId() == id) {
                    itr.remove();
                }
            }
            
            for (Iterator<BandwidthReservation> itr = reservations.iterator();itr.hasNext();) {
                BandwidthReservation reservation = itr.next();
                if (reservation.getId() == id) {
                    itr.remove();
                }
            }

            long totalSize = 0;
            // Recalculate the current size since
            for (BandwidthAllocation allocation : allocations) {
                totalSize += allocation.getEstimatedSizeInBytes();
            }

            for (BandwidthReservation reservation : reservations) {
                totalSize += reservation.getSizeInBytes();
            }
            currentSize = totalSize;
        }

        /**
         * Return whether this bucket is empty.
         * 
         * @return true if empty
         */
        public boolean isEmpty() {
            return currentSize == 0;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + (int) (bucketStartTime ^ (bucketStartTime >>> 32));
            return result;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            BandwidthBucket other = (BandwidthBucket) obj;
            if (bucketStartTime != other.bucketStartTime)
                return false;
            return true;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int compareTo(BandwidthBucket o) {
            if (this.bucketStartTime > o.bucketStartTime) {
                return 1;
            } else if (this.bucketStartTime < o.bucketStartTime) {
                return -1;
            } else {
                return 0;
            }
        }
    }

    private final IBandwidthDao bandwidthDao;

    // which retrieval plan
    private final Network network;

    private final BandwidthMap map;

    // The scheduler used to insert retrievals into the plan
    private IRetrievalScheduler scheduler;

    // How many days of retrievals should the plan hold
    private int planDays;

    // NOTE: If you must read or make changes to the buckets and requestMap, you
    // must acquire the locks in the following order: buckets first, then
    // requestMap

    // access to buckets should always be synchronized on buckets itself..
    private final NavigableMap<Long, BandwidthBucket> buckets = new TreeMap<Long, BandwidthBucket>();

    // access to requestMap should always be synchronized on requestMap itself..
    private final Map<Long, Set<Long>> requestMap = new HashMap<Long, Set<Long>>();

    // Number of minutes of bandwidth per bucket.
    private int bucketMinutes;

    private Calendar planEnd;

    private Calendar planStart;

    private long bytesPerBucket;

    /**
     * 
     * @param network
     *            The Network to create the RetrievalPlan for.
     * 
     * @param map
     *            The BandwidthMap Object to use to initialize the plan.
     */
    public RetrievalPlan(Network network, BandwidthMap map,
            IBandwidthDao bandwidthDao) {
        this.network = network;
        this.bandwidthDao = bandwidthDao;
        this.map = map;

        boolean found = false;
        BandwidthRoute route = map.getRoute(network);
        if (route != null) {
            found = true;
            this.planDays = route.getPlanDays();
            this.bucketMinutes = route.getBucketSizeMinutes();
        }

        if (found) {
            Calendar currentBucket = BandwidthUtil.now();
            planStart = BandwidthUtil.now();
            planEnd = BandwidthUtil.copy(planStart);
            planEnd.add(Calendar.DAY_OF_YEAR, planDays);

            // Make the buckets...
            long bucket = 0;
            while (!currentBucket.after(planEnd)) {
                int bw = map.getBandwidth(network, currentBucket);
                bucket = currentBucket.getTimeInMillis();
                // Get the bucket size..
                // buckets are (bandwidth [kilobits/second] * milliseconds per
                // minute *
                // bucket minutes)/bits per byte) ...
                bytesPerBucket = BandwidthUtil
                        .convertKilobytesPerSecondToBytesPerSpecifiedMinutes(
                                bw,
                        bucketMinutes);

                buckets.put(bucket, new BandwidthBucket(bucket, bytesPerBucket));
                currentBucket.add(Calendar.MINUTE, bucketMinutes);
            }
        } else {
            // Can't proceed, throw an Exception
            throw new IllegalArgumentException(
                    "The BandwidthMap specified does not contain the route["
                            + network + "]");
        }
    }

    public Network getNetwork() {
        return network;
    }

    public int getDefaultBandwidth() {
        return map.getRoute(getNetwork()).getDefaultBandwidth();
    }

    public void setDefaultBandwidth(int defaultBandwidth)
            throws SerializationException {
        map.getRoute(getNetwork()).setDefaultBandwidth(defaultBandwidth);
        map.save();
    }

    /**
     * Schedule the {@link BandwidthAllocation}.
     * 
     * @param bandwidthAllocation
     *            the allocation
     * @return the list of unscheduled bandwidth allocations
     */
    public List<BandwidthAllocation> schedule(
            BandwidthAllocation bandwidthAllocation) {
        // First make sure we have the same path
        if (!(network == bandwidthAllocation.getNetwork())) {
            throw new IllegalArgumentException("BandwidthAllocation ["
                    + bandwidthAllocation.getId()
                    + "] does not have the same path ["
                    + bandwidthAllocation.getNetwork()
                    + "] as the RetrievalPlan [" + network + "]");
        }

        if (planEnd.before(bandwidthAllocation.getStartTime())) {
            resize();
        }

        synchronized (buckets) {
            return scheduler.schedule(this, bandwidthAllocation);
        }
    }

    public void resize() {
        // The end of the plan should always be planDays from
        // now...
        Calendar currentBucket = BandwidthUtil.now();
        Calendar newEndOfPlan = BandwidthUtil.copy(currentBucket);
        newEndOfPlan.add(Calendar.DAY_OF_YEAR, planDays);

        resize(currentBucket, newEndOfPlan);
    }

    /**
     * Resize the plan for the parameters in the date. This method should only
     * be called internally to this class, and in tests which need access to
     * make specific changes.
     * 
     * @param newStartOfPlan
     *            the new start of the plan
     * @param newEndOfPlan
     *            the new end of the plan
     */
    @VisibleForTesting
    void resize(Calendar newStartOfPlan, Calendar newEndOfPlan) {

        // If the calculated end of plan is after the current end of plan,
        // we have to add buckets to the plan to account for the difference
        // to make sure that the plan maintains "planDays" of schedule..
        if (newEndOfPlan.after(planEnd)) {

            synchronized (buckets) {
                // Get the last bucket and add buckets to make up the
                // difference..
                // Make the buckets...
                long bucket = buckets.lastKey();
                Calendar currentBucket = BandwidthUtil.now();
                currentBucket.setTimeInMillis(bucket);

                // Add the buckets minutes to the last bucket and add
                // buckets until we have the new plan size.
                currentBucket.add(Calendar.MINUTE, bucketMinutes);

                while (!currentBucket.after(newEndOfPlan)) {
                    int bw = map.getBandwidth(network, currentBucket);
                    bucket = currentBucket.getTimeInMillis();
                    // Get the bucket size..
                    // buckets are (bandwidth * kilobits/second * 60 seconds *
                    // bucket minutes)/bits per byte) ...
                    bytesPerBucket = BandwidthUtil
                            .convertKilobytesPerSecondToBytesPerSpecifiedMinutes(
                            bw, bucketMinutes);
                    buckets.put(bucket, new BandwidthBucket(bucket,
                            bytesPerBucket));
                    currentBucket.add(Calendar.MINUTE, bucketMinutes);
                    statusHandler.info("resize() - Adding bucket [" + bucket
                            + "] bandwidth = [" + bw + "]");
                }
            }

        }
        // Now remove buckets from the front of the map whos time slot
        // is past and are empty
        long newStart = newStartOfPlan.getTimeInMillis();
        NavigableMap<Long, BandwidthBucket> x = buckets.headMap(newStart, true);
        Iterator<Long> itr = x.keySet().iterator();
        while (itr.hasNext()) {
            Long key = itr.next();
            BandwidthBucket b = x.get(key);
            // If the bucket is empty, remove it from the Map,
            // which should result in removal from the parent Map,
            if (b.isEmpty()) {
                statusHandler.info("resize() - Removing bucket ["
                        + b.getBucketStartTime() + "]");
                itr.remove();
            }
        }

        this.planStart = newStartOfPlan;
        this.planEnd = newEndOfPlan;
    }

    public void setScheduler(IRetrievalScheduler scheduler) {
        this.scheduler = scheduler;
    }

    public String showPlan() {
        StringBuilder sb = new StringBuilder();

        synchronized (buckets) {
            Iterator<Long> itr = buckets.keySet().iterator();
            while (itr.hasNext()) {
                BandwidthBucket bucket = getBucket(itr.next());
                sb.append(bucket.showReservations());
            }
        }

        return sb.toString();
    }

    public void setPlanDays(int planDays) {
        this.planDays = planDays;
    }

    public int getPlanDays() {
        return planDays;
    }

    /**
     * Get the next scheduled RetrievalRequest..
     * 
     * @param agentType
     * 
     * @return
     */
    public BandwidthAllocation nextAllocation(String agentType) {
        BandwidthAllocation reservation = null;

        synchronized (buckets) {

            // Get the portion of the Map that is before the
            // current time (DO NOT want to return future reservations)
            SortedMap<Long, BandwidthBucket> available = buckets
                    .headMap(buckets.ceilingKey(TimeUtil.currentTimeMillis() + 1));

            // Iterate over the buckets and find the first
            // BandwidthAllocation that is in the READY state
            for (BandwidthBucket bucket : available.values()) {
                reservation = bucket.getNextReservation(agentType);
                if (reservation != null) {
                    break;
                }
            }
        }

        return reservation;
    }

    private NavigableMap<Long, BandwidthBucket> getBuckets() {
        synchronized (buckets) {
            return buckets;
        }
    }

    public void updateRequestMapping(long requestId,
            Set<BandwidthBucket> buckets) {
        Set<Long> bucketIds = new TreeSet<Long>();
        for (BandwidthBucket bucket : buckets) {
            bucketIds.add(bucket.getBucketStartTime());
        }

        synchronized (requestMap) {
            requestMap.put(requestId, bucketIds);
        }
    }

    public void remove(BandwidthAllocation reservation) {

        synchronized (buckets) {
            // Must have both monitors
            synchronized (requestMap) {
                Set<Long> bucketIds = requestMap.get(reservation.getId());
                if (bucketIds == null) {
                    // This can happen when an allocation/reservation is in a
                    // DEFERRED state, at a minimum
                    return;
                }
                for (Long bucketId : bucketIds) {
                    BandwidthBucket bucket = getBucket(bucketId);
                    bucket.remove(reservation.getId());
                }
            }
        }
    }

    public Calendar getPlanEnd() {
        // Don't want an inadvertent change to plan end, so make a copy of the
        // Calendar Object and return that.
        return BandwidthUtil.copy(planEnd);
    }

    public Calendar getPlanStart() {
        // Don't want an inadvertent change to plan start, so make a copy of the
        // Calendar Object and return that.
        return BandwidthUtil.copy(planStart);
    }

    /**
     * Update the BandwidthAllocation(s) in the BandwidthBuckets for a
     * particular allocation.
     * 
     * @param allocation
     *            the allocation to update.
     */
    public void updateBandwidthReservation(BandwidthAllocation allocation) {
        final long id = allocation.getId();
        synchronized (buckets) {
            // Must have both monitors
            synchronized (requestMap) {
                if (!requestMap.containsKey(id)) {
                    statusHandler
                            .warn("The request map should always contain a mapping for a bandwidth allocation prior to reaching this point.  "
                                    + "Adding to the map manually, but seeing this message without expecting it signifies a logic error, "
                                    + "and bandwidth is not being properly managed!");
                    BandwidthBucket bucket = buckets.firstEntry().getValue();
                    bucket.add(allocation);
                    allocation.setBandwidthBucket(bucket.getBucketStartTime());
                    bandwidthDao.update(allocation);

                    bucket.add(allocation);

                    TreeSet<BandwidthBucket> set = Sets.newTreeSet();
                    set.add(bucket);
                    updateRequestMapping(id, set);
                }

                Set<Long> bucketIds = requestMap.get(id);
                for (Long bucketId : bucketIds) {
                    BandwidthBucket bucket = getBucket(bucketId);
                    // Remove the existing version with the same ID and replace
                    // it
                    int indexToReplace = -1;
                    // Direct access to allocations here, because the getter
                    // returns an unmodifiable version for public access
                    List<BandwidthAllocation> requests = bucket.allocations;
                    for (int i = 0; i < requests.size(); i++) {
                        BandwidthAllocation current = requests.get(i);
                        if (current.getId() == id) {
                            indexToReplace = i;
                            break;
                        }
                    }

                    if (indexToReplace > -1) {
                        requests.set(indexToReplace, allocation);
                    } else {
                        statusHandler.warn("Unable to find allocation [" + id
                                + "] for replacement in the bucket!");
                    }
                }
            }
        }
    }

    /**
     * Return the bucket for the specified id.
     * 
     * @param bucketId
     *            the bucketId
     * @return the bucket
     * @throws NullPointerException
     *             if no bucket exists with the id
     */
    public BandwidthBucket getBucket(long bucketId) {
        BandwidthBucket bucket = getBuckets().get(bucketId);
        Preconditions.checkNotNull(bucket,
                "Unable to find bucket for start time [" + bucketId + "]");
        return bucket;
    }

    /**
     * Returns the greatest key less than or equal to the given key, or null if
     * there is no such key.
     * 
     * @param key
     * @return the floored key, or null
     */
    private Long floorBucket(long key) {
        return getBuckets().floorKey(key);
    }

    /**
     * Returns the least key greater than or equal to the given key, or null if
     * there is no such key.
     * 
     * @param key
     * @return the ceiling-ed key, or null
     */
    private Long ceilingKey(long key) {
        return getBuckets().ceilingKey(key);
    }

    /**
     * Return the buckets in the specified window, both boundaries are
     * inclusive. Buckets will be in order of their start time.
     * 
     * @param startMillis
     *            the start time for buckets to include
     * @param endMillis
     *            the end time for buckets to include
     * @return the buckets in the window, sorted in ascending order of start
     *         time
     */
    public SortedSet<BandwidthBucket> getBucketsInWindow(Long startMillis,
            Long endMillis) {
        // Get the bucket for the startTime and endTime.
        Long startKey = floorBucket(startMillis);
        if (startKey == null) {
            // TODO: If this happens a lot (monitor log message frequency)
            // then change the default behavior to just use a ceilingKey
            startKey = ceilingKey(startMillis);
            statusHandler
                    .info(String
                            .format("Unable to find bucket before or up to [%s] using nearest bucket of [%s]",
                                    startMillis, startKey));
        }
        Long endKey = floorBucket(endMillis);
        if (endKey == null) {
            endKey = ceilingKey(startMillis);
        }

        // Handle the case where an invalid range was somehow specified
        // (shouldn't happen, so just throw an exception with as much
        // information as we have)
        if (startKey == null || endKey == null) {
            throw new IllegalArgumentException(
                    String.format(
                            "Invalid start and end times requested for getBucketsInWindow(): start time [%s], end time [%s], bucket start key [%s], bucket end key [%s].  "
                                    + "Plan boundaries: start [%s] end [%s]",
                            startMillis, endMillis, startKey, endKey,
                            BandwidthUtil.format(getPlanStart()),
                            BandwidthUtil.format(getPlanEnd())));
        }

        NavigableMap<Long, BandwidthBucket> window = getBuckets().subMap(
                startKey, true, endKey, true);
        return new TreeSet<BandwidthBucket>(window.values());
    }

    /**
     * Adds the {@link BandwidthAllocation} to the specified bucket.
     * 
     * @param bucket
     *            the bucket
     * @param allocation
     *            the allocation
     * @throws NullPointerException
     *             if the bucket start time is invalid
     */
    public void addToBucket(BandwidthBucket bucket, BandwidthAllocation allocation) {
        long bucketStartTime = bucket.getBucketStartTime();

        synchronized (buckets) {
            BandwidthBucket actualBucket = getBucket(bucketStartTime);
            actualBucket.add(allocation);
        }
    }

    /**
     * Adds the {@link BandwidthReservation} to the specified bucket.
     * 
     * @param bucket
     *            the bucket
     * @param reservation
     *            the reservation
     * @throws NullPointerException
     *             if the bucket start time is invalid
     */
    public void addToBucket(BandwidthBucket bucket,
            BandwidthReservation reservation) {
        long bucketStartTime = bucket.getBucketStartTime();

        synchronized (buckets) {
            BandwidthBucket actualBucket = getBucket(bucketStartTime);
            actualBucket.add(reservation);
        }
    }

    /**
     * Get the number of minutes each bucket, by default, contains.
     * 
     * @return the bucketMinutes
     */
    public int getBucketMinutes() {
        return bucketMinutes;
    }
}
