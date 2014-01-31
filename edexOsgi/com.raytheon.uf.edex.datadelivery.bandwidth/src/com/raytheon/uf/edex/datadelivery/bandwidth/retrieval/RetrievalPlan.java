package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Preconditions;
import com.google.common.collect.Sets;
import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthMap;
import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthRoute;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthBucketDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.registry.RegistryBandwidthService;
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
 * Jun 25, 2013 2106       djohnson     Separate state into other classes, promote BandwidthBucket to a class proper.
 * Oct 30, 2013  2448      dhladky      Moved methods to TimeUtil.
 * Nov 16, 2013 1736       dhladky      Alter size of available bandwidth by subtracting that used by registry.
 * Dec 05, 2013 2545       mpduff       BandwidthReservation now stored in bytes.
 * Dec 13, 2013 2545       mpduff       Prevent negative values in bandwidth bucket sizes.
 * Dec 17, 2013 2636       bgonzale     Check for removed buckets when removing BandwidthAllocations or 
 *                                      BandwidthReservations. Add constrained bucket addition method.
 *                                      Added debug logging.
 * Jan 08, 2014 2615       bgonzale     Log registry bandwidth calculation errors.
 * 
 * </pre>
 * 
 * @version 1.0
 */
// TODO: Need to enable transactions from BandwidthManager forward
// @Service
// @Transactional(propagation = MANDATORY)
public class RetrievalPlan {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalPlan.class);

    private IBandwidthDao<?, ?> bandwidthDao;

    // which retrieval plan
    private Network network;

    private BandwidthMap map;

    // The scheduler used to insert retrievals into the plan
    private IRetrievalScheduler scheduler;

    // How many days of retrievals should the plan hold
    private int planDays;

    // NOTE: If you must read or make changes to the buckets and requestMap, you
    // must acquire the locks in the following order: buckets first, then
    // requestMap

    private IBandwidthBucketDao bucketsDao;

    // access to buckets should always be synchronized on bucketsLock..
    private final Object bucketsLock = new Object();

    // access to requestMap should always be synchronized on requestMap itself..
    private final Map<Long, Set<Long>> requestMap = new HashMap<Long, Set<Long>>();

    // Number of minutes of bandwidth per bucket.
    private int bucketMinutes;

    private Calendar planEnd;

    private Calendar planStart;

    private long bytesPerBucket;

    private IBandwidthBucketAllocationAssociator associator;

    /**
     * Constructor.
     */
    public RetrievalPlan() {
    }

    /**
     * Initialize the retrieval plan. Intentionally package-private as it should
     * only be called by {@link RetrievalManager}.
     */
    void init() {
        boolean found = false;
        BandwidthRoute route = map.getRoute(network);

        if (route != null) {
            found = true;
            this.planDays = route.getPlanDays();
            this.bucketMinutes = route.getBucketSizeMinutes();
        }

        if (found) {
            // create registry bandwidth service
            RegistryBandwidthService rbs = new RegistryBandwidthService(
                    bucketsDao, network, bucketMinutes);
            long bucketMillis = bucketMinutes * TimeUtil.MILLIS_PER_MINUTE;
            Calendar currentBucket = BandwidthUtil.now();
            planStart = BandwidthUtil.now();
            planEnd = TimeUtil.newCalendar(planStart);
            planEnd.add(Calendar.DAY_OF_YEAR, planDays);
            long currentMillis = currentBucket.getTimeInMillis();
            long planEndMillis = planEnd.getTimeInMillis();
            // Make the buckets...

            while (!(currentMillis > planEndMillis)) {

                int bw = map.getBandwidth(network, currentBucket);
                // Get the bucket size..
                // buckets are (bandwidth [kilobytes/second] * milliseconds per
                // minute *
                // bucket minutes)/bits per byte) ...
                bytesPerBucket = BandwidthUtil
                        .convertKilobytesPerSecondToBytesPerSpecifiedMinutes(
                                bw, bucketMinutes);

                bucketsDao.create(new BandwidthBucket(currentMillis,
                        bytesPerBucket, network));

                currentMillis += bucketMillis;
            }

            // subtract registry traffic from total available bytes/per second
            for (BandwidthBucket bucket : bucketsDao.getAll(network)) {
                long startMillis = bucket.getBucketStartTime();
                int registryBytesPerSecond = 0;
                try {
                    registryBytesPerSecond = rbs
                            .getRegistryBandwidth(startMillis);
                } catch (IllegalArgumentException e) {
                    statusHandler
                            .error("Failed to init registry bandwidth calculation.  Registry bandwidth will be ignored.",
                                    e);
                }
                bucket.setBucketSize(bucket.getBucketSize()
                        - (registryBytesPerSecond * TimeUtil.SECONDS_PER_MINUTE * bucketMinutes));
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

        synchronized (bucketsLock) {
            return scheduler.schedule(this, bandwidthAllocation);
        }
    }

    public void resize() {
        // The end of the plan should always be planDays from
        // now...
        Calendar currentBucket = BandwidthUtil.now();
        Calendar newEndOfPlan = TimeUtil.newCalendar(currentBucket);
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

            synchronized (bucketsLock) {
                // Get the last bucket and add buckets to make up the
                // difference..
                // Make the buckets...
                long bucketMillis = bucketMinutes * TimeUtil.MILLIS_PER_MINUTE;
                long newPlanEndMillis = newEndOfPlan.getTimeInMillis();
                BandwidthBucket bucket = bucketsDao.getLastBucket(network);
                Calendar currentBucket = BandwidthUtil.now();
                currentBucket.setTimeInMillis(bucket.getBucketStartTime());
                long currentBucketMillis = bucket.getBucketStartTime();

                // Add the buckets minutes to the last bucket and add
                // buckets until we have the new plan size.
                currentBucketMillis += bucketMillis;
                // create Registry Bandwidth Service
                RegistryBandwidthService rbs = new RegistryBandwidthService(
                        bucketsDao, network, bucketMinutes);

                while (!(currentBucketMillis > newPlanEndMillis)) {

                    int bw = map.getBandwidth(network, currentBucket);
                    // subtract registry traffic from total available bytes/per
                    // second
                    int registryBytesPerSecond = rbs
                            .getRegistryBandwidth(currentBucketMillis);
                    bw = bw - registryBytesPerSecond;
                    // Get the bucket size..
                    // buckets are (bandwidth * kilobytes/second * 60 seconds *
                    // bucket minutes)/bits per byte) ...
                    bytesPerBucket = BandwidthUtil
                            .convertKilobytesPerSecondToBytesPerSpecifiedMinutes(
                                    bw, bucketMinutes);
                    bucketsDao.create(new BandwidthBucket(currentBucketMillis,
                            bytesPerBucket, network));

                    currentBucketMillis += bucketMillis;
                    statusHandler.info("resize() - Adding bucket [" + bucket
                            + "] bandwidth = [" + bw + "]");
                }
            }
        }
        // Now remove buckets from the front of the map who's time slot
        // is past and are empty
        long newStart = newStartOfPlan.getTimeInMillis();

        try {
            bucketsDao.deleteEmptyBucketsUpToTime(newStart, network);
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to remove empty buckets!", e);
        }

        this.planStart = newStartOfPlan;
        this.planEnd = newEndOfPlan;
    }

    public void setScheduler(IRetrievalScheduler scheduler) {
        this.scheduler = scheduler;
    }

    /**
     * Show the contents of the {@link RetrievalPlan}.
     * 
     * @return
     */
    public String showPlan() {
        StringBuilder sb = new StringBuilder();

        List<BandwidthBucket> buckets = bucketsDao.getAll(network);
        for (BandwidthBucket bucket : buckets) {
            sb.append(showBucket(bucket));
        }

        return sb.toString();
    }

    /**
     * Show the contents of the {@link BandwidthBucket}.
     * 
     * @param bucket
     *            the bucket
     * @return the String to display
     */
    public String showBucket(BandwidthBucket bucket) {
        return associator.showBucket(bucket);
    }

    public void setPlanDays(int planDays) {
        this.planDays = planDays;
    }

    public int getPlanDays() {
        return planDays;
    }

    public void setNetwork(Network network) {
        this.network = network;
    }

    public void setMap(BandwidthMap map) {
        this.map = map;
    }

    public void setBandwidthDao(IBandwidthDao<?, ?> bandwidthDao) {
        this.bandwidthDao = bandwidthDao;
    }

    public void setBucketsDao(IBandwidthBucketDao bucketsDao) {
        this.bucketsDao = bucketsDao;
    }

    public void setAssociator(IBandwidthBucketAllocationAssociator associator) {
        this.associator = associator;
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

        synchronized (bucketsLock) {

            // Get the portion of the Map that is before the
            // current time (DO NOT want to return future reservations)
            final List<BandwidthBucket> buckets = bucketsDao
                    .getWhereStartTimeIsLessThanOrEqualTo(
                            TimeUtil.currentTimeMillis(), network);

            // Iterate over the buckets and find the first
            // BandwidthAllocation that is in the READY state
            for (BandwidthBucket bucket : buckets) {
                reservation = associator.getNextReservation(bucket, agentType);
                if (reservation != null) {
                    break;
                }
            }
        }

        return reservation;
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

    /**
     * Remove the {@link BandwidthAllocation} from the {@link RetrievalPlan}.
     * 
     * @param allocation
     *            the allocation
     */
    public void remove(BandwidthAllocation allocation) {

        synchronized (bucketsLock) {
            // Must have both monitors
            synchronized (requestMap) {
                Set<Long> bucketIds = requestMap.get(allocation.getId());
                if (bucketIds == null) {
                    // This can happen when an allocation/reservation is in a
                    // DEFERRED state, at a minimum
                    return;
                }
                for (Long bucketId : bucketIds) {
                    // get bucket without checks. sometimes the
                    // first bucket may have been removed.
                    BandwidthBucket bucket = getBucketNoChecks(bucketId);
                    if (bucket != null) {
                        bucket.setCurrentSize(Math.max(
                                0,
                                bucket.getCurrentSize()
                                        - allocation.getEstimatedSizeInBytes()));
                        associator.removeFromBucket(bucket, allocation);
                    }
                }
            }
        }
    }

    /**
     * Remove the {@link BandwidthReservation} from the {@link RetrievalPlan}.
     * 
     * @param reservation
     *            the reservation
     */
    public void remove(BandwidthReservation reservation) {

        synchronized (bucketsLock) {
            // Must have both monitors
            synchronized (requestMap) {
                Set<Long> bucketIds = requestMap.get(reservation.getId());
                if (bucketIds == null) {
                    // This can happen when an allocation/reservation is in a
                    // DEFERRED state, at a minimum
                    return;
                }
                for (Long bucketId : bucketIds) {
                    // get bucket without checks. sometimes the
                    // first bucket may have been removed.
                    BandwidthBucket bucket = getBucketNoChecks(bucketId);
                    if (bucket != null) {
                        bucket.setCurrentSize(Math.max(0,
                                bucket.getCurrentSize() - reservation.getSize()));
                        associator.removeFromBucket(bucket, reservation);
                    }
                }
            }
        }
    }

    public Calendar getPlanEnd() {
        // Don't want an inadvertent change to plan end, so make a copy of the
        // Calendar Object and return that.
        return TimeUtil.newCalendar(planEnd);
    }

    public Calendar getPlanStart() {
        // Don't want an inadvertent change to plan start, so make a copy of the
        // Calendar Object and return that.
        return TimeUtil.newCalendar(planStart);
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
        synchronized (bucketsLock) {
            // Must have both monitors
            synchronized (requestMap) {
                if (!requestMap.containsKey(id)) {
                    statusHandler
                            .warn("The request map should always contain a mapping for a bandwidth allocation prior to reaching this point.  "
                                    + "Adding to the map manually, but seeing this message without expecting it signifies a logic error, "
                                    + "and bandwidth is not being properly managed!");
                    BandwidthBucket bucket = bucketsDao.getFirstBucket(network);
                    addToBucket(bucket, allocation);
                    allocation.setBandwidthBucket(bucket.getBucketStartTime());
                    bandwidthDao.createOrUpdate(allocation);

                    TreeSet<BandwidthBucket> set = Sets.newTreeSet();
                    set.add(bucket);
                    updateRequestMapping(id, set);
                }

                Set<Long> bucketIds = requestMap.get(id);
                for (Long bucketId : bucketIds) {
                    BandwidthBucket bucket = getBucket(bucketId);
                    associator.removeFromBucket(bucket, allocation);
                    associator.addToBucket(bucket, allocation);
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
        BandwidthBucket bucket = bucketsDao.getByStartTime(bucketId, network);
        Preconditions.checkNotNull(bucket,
                "Unable to find bucket for start time [" + bucketId + "]");
        return bucket;
    }

    /**
     * Return the bucket for the specified id.
     * 
     * @param bucketId
     *            the bucketId
     * @return the bucket; null if not found
     */
    private BandwidthBucket getBucketNoChecks(long bucketId) {
        return bucketsDao.getByStartTime(bucketId, network);
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
    public SortedSet<BandwidthBucket> getBucketsInWindow(long earliestTime,
            long latestTime) {
        return bucketsDao.getBucketsInWindow(earliestTime, latestTime, network);
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
    public void addToBucketWithSizeConstraint(BandwidthBucket bucket,
            BandwidthAllocation allocation) {
        long bucketStartTime = bucket.getBucketStartTime();

        synchronized (bucketsLock) {
            BandwidthBucket actualBucket = getBucket(bucketStartTime);
            long bucketSize = actualBucket.getBucketSize();
            long totalSize = actualBucket.getCurrentSize()
                    + allocation.getEstimatedSizeInBytes();
            // constrain size by size of bucket. Reservations will have filled
            // out the rest of the allocation in subsequent buckets.
            totalSize = totalSize > bucketSize ? bucketSize
                    : totalSize;
            actualBucket.setCurrentSize(totalSize);
            associator.addToBucket(actualBucket, allocation);
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Adding (constrained) to bucket "
                        + actualBucket.getBucketStartTime() + " with size "
                        + actualBucket.getBucketSize() / 1000
                        + "k an Allocation "
                        + allocation.getEstimatedSizeInBytes() / 1000
                        + "k.  Remaining in bucket "
                        + actualBucket.getAvailableBandwidth() / 1000 + "k");
            }
        }
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
    public void addToBucket(BandwidthBucket bucket,
            BandwidthAllocation allocation) {
        long bucketStartTime = bucket.getBucketStartTime();

        synchronized (bucketsLock) {
            BandwidthBucket actualBucket = getBucket(bucketStartTime);
            actualBucket.setCurrentSize(actualBucket.getCurrentSize()
                    + allocation.getEstimatedSizeInBytes());
            associator.addToBucket(actualBucket, allocation);
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Adding to bucket "
                        + actualBucket.getBucketStartTime() + " with size "
                        + actualBucket.getBucketSize() / 1000
                        + "k an Allocation "
                        + allocation.getEstimatedSizeInBytes() / 1000
                        + "k.  Remaining in bucket "
                        + actualBucket.getAvailableBandwidth() / 1000 + "k");
            }
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

        synchronized (bucketsLock) {
            BandwidthBucket actualBucket = getBucket(bucketStartTime);
            actualBucket.setCurrentSize(actualBucket.getCurrentSize()
                    + reservation.getSize());
            associator.addToBucket(actualBucket, reservation);
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Adding to bucket "
                        + actualBucket.getBucketStartTime() + " with size "
                        + actualBucket.getBucketSize() / 1000
                        + "k a Reservation " + reservation.getSize() / 1000
                        + "k.  Remaining in bucket "
                        + actualBucket.getAvailableBandwidth() / 1000 + "k");
            }
        }
    }

    /**
     * Retrieve the {@link BandwidthAllocation}s for a {@link BandwidthBucket}.
     * 
     * @param bucket
     *            the bucket
     * @return the bandwidth allocations
     */
    public List<BandwidthAllocation> getBandwidthAllocationsForBucket(
            BandwidthBucket bucket) {
        return associator.getBandwidthAllocationsForBucket(bucket);
    }

    /**
     * Retrieve the {@link BandwidthReservation}s for a {@link BandwidthBucket}.
     * 
     * @param bucket
     *            the bucket
     * @return the bandwidth reservations
     */
    public List<BandwidthReservation> getBandwidthReservationsForBucket(
            BandwidthBucket bucket) {
        return associator.getBandwidthReservationsForBucket(bucket);
    }

    /**
     * Get the number of minutes each bucket, by default, contains.
     * 
     * @return the bucketMinutes
     */
    public int getBucketMinutes() {
        return bucketMinutes;
    }

    /**
     * Copy state from the specied {@link RetrievalPlan}.
     * 
     * @param fromPlan
     *            the other plan
     */
    public void copyState(RetrievalPlan fromPlan) {
        this.bucketsDao.copyState(fromPlan.bucketsDao);
        this.bucketMinutes = fromPlan.bucketMinutes;
        this.bytesPerBucket = fromPlan.bytesPerBucket;
        this.planDays = fromPlan.planDays;
        this.planEnd = TimeUtil.newCalendar(fromPlan.planEnd);
        this.planStart = TimeUtil.newCalendar(fromPlan.planStart);
        this.requestMap.clear();
        this.requestMap.putAll(fromPlan.requestMap);
        this.associator.copyState(fromPlan.associator);
    }

}
