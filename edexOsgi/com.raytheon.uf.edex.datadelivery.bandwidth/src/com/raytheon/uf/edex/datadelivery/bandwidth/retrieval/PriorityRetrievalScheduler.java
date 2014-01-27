package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * Implementation of IRetrievalScheduler that evaluates Subscription priority
 * values and fills the RetrievalPlan accordingly.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2012 726        jspinks     Initial release.
 * Oct 17, 2012 0726       djohnson    If unable to find a bucket with floorKey, use ceilingKey.
 * Oct 26, 2012 1286       djohnson    Return list of unscheduled allocations.
 * Jan 25, 2013 1528       djohnson    Lower priority requests should not be able to unschedule higher priority requests.
 * Jun 25, 2013 2106       djohnson    Access bandwidth bucket contents through RetrievalPlan.
 * Dec 17, 2013 2636       bgonzale    When adding to buckets, call the constrained method.
 * </pre>
 * 
 * @version 1.0
 */
public class PriorityRetrievalScheduler implements IRetrievalScheduler {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PriorityRetrievalScheduler.class);

    @Override
    public List<BandwidthAllocation> schedule(RetrievalPlan plan,
            BandwidthAllocation allocation) {

        Set<BandwidthAllocation> unscheduled = new HashSet<BandwidthAllocation>();

        // First get the retrieval start time. Compare the buckets
        // in order, to see if there is room for the BandwidthAllocation
        // If there is room, simply add the allocation to the
        // bucket and return. If there is not room, then we need
        // to compare the allocations that are not active to the
        // proposed allocation and see if we need to move already
        // scheduled allocations.
        Calendar startTime = allocation.getStartTime();
        Calendar endTime = allocation.getEndTime();

        long startTimeMillis = startTime.getTimeInMillis();
        long endTimeMillis = endTime.getTimeInMillis();
        
        if (startTimeMillis > endTimeMillis) {
            throw new IllegalArgumentException(String.format(
                    "Invalid start and end times passed for allocation [%s]: "
                            + "start time [%s] is greater than end time [%s].",
                    allocation.getId(), BandwidthUtil.format(startTime),
                    BandwidthUtil.format(endTime)));
        }

        boolean notScheduled = true;

        // Get the buckets that are in the 'window'
        // for the BandwidthAllocation.
        SortedSet<BandwidthBucket> window = plan.getBucketsInWindow(
                startTimeMillis, endTimeMillis);

        // Look through the buckets in the window for bandwidth.
        Iterator<BandwidthBucket> itr = window.iterator();

        long bandwidthRequired = allocation.getEstimatedSizeInBytes();
        boolean split = false;

        SortedMap<BandwidthBucket, Object> reservations = new TreeMap<BandwidthBucket, Object>();

        while (notScheduled && itr.hasNext()) {

            BandwidthBucket bucket = itr.next();
            // How much is available?
            long available = bucket.getAvailableBandwidth();

            // The whole allocation can fit..
            if (available >= bandwidthRequired) {

                if (split) {
                    BandwidthReservation reserve = new BandwidthReservation(
                            allocation, bandwidthRequired);
                    // Since we have had to split the allocation between
                    // buckets, assign the reservation to the current bucket.
                    reserve.setBandwidthBucket(bucket.getBucketStartTime());
                    reservations.put(bucket, reserve);
                } else {
                    // If we haven't split the allocation over buckets,
                    // assign the bucket number to the allocation.
                    allocation.setBandwidthBucket(bucket.getBucketStartTime());
                    reservations.put(bucket, allocation);
                }
                // All of the required bandwidth was found. Consider the
                // BandwidthAllocation scheduled.
                notScheduled = false;

            } else if (available > 0) {
                // There is some bandwidth to be used, so add the request
                // and let the rest overflow into the next bucket...
                if (split) {
                    BandwidthReservation reserve = new BandwidthReservation(
                            allocation, available);
                    // Since we have had to split the allocation between
                    // buckets, assign the reservation to the current bucket.
                    reserve.setBandwidthBucket(bucket.getBucketStartTime());
                    reservations.put(bucket, reserve);
                } else {
                    allocation.setBandwidthBucket(bucket.getBucketStartTime());
                    reservations.put(bucket, allocation);
                }
                split = true;

                // Reduced the required amount by what is available in this
                // bucket.
                bandwidthRequired -= available;
            }
        }

        // If still not scheduled,
        if (notScheduled) {
            // Time to look at re-prioritizing some retrievals...
            unscheduled.addAll(reprioritize(plan, allocation, startTimeMillis,
                    endTimeMillis));
            notScheduled = unscheduled.contains(allocation);
        } else {
            // Commit the reservations
            for (BandwidthBucket key : reservations.keySet()) {
                Object o = reservations.get(key);

                if (o instanceof BandwidthAllocation) {
                    BandwidthAllocation obj = (BandwidthAllocation) o;
                    obj.setStatus(RetrievalStatus.SCHEDULED);
                    plan.addToBucketWithSizeConstraint(key, obj);
                } else {
                    plan.addToBucket(key, (BandwidthReservation) o);
                }
            }

            // Update the requestMap in the RetrievalPlan with all the
            // bucket(s) that the BandwidthAllocation and/or BandwidthAllocation
            // has been allocated to.
            plan.updateRequestMapping(allocation.getId(), reservations.keySet());
        }

        if (notScheduled) {
            unscheduled.add(allocation);
        }

        return new ArrayList<BandwidthAllocation>(unscheduled);
    }

    private List<BandwidthAllocation> reprioritize(RetrievalPlan plan,
            BandwidthAllocation request,
            Long startKey, Long endKey) {

        statusHandler.info("Re-prioritizing necessary for BandwidthAllocation["
                + request + "]");

        // Look in the window between start and end times to see if there are
        // lower priority
        // retrievals that can be moved..
        SortedSet<BandwidthBucket> window = plan
                .getBucketsInWindow(startKey, endKey);

        boolean enoughBandwidth = false;
        long total = 0;
        List<BandwidthAllocation> lowerPriorityRequests = new ArrayList<BandwidthAllocation>();
        for (BandwidthBucket bucket : window) {
            for (BandwidthAllocation o : plan
                    .getBandwidthAllocationsForBucket(bucket)) {
                long estimatedSizeInBytes = o.getEstimatedSizeInBytes();
                // This was bad... we just about released giving lower
                // priority requests the ability to unschedule higher priority
                // requests....
                if (request.isHigherPriorityThan(o)) {
                    total += estimatedSizeInBytes;
                    lowerPriorityRequests.add(o);
                }

                // See if we have found enough room
                if (total >= estimatedSizeInBytes) {
                    enoughBandwidth = true;
                    break;
                }
            }
        }

        if (enoughBandwidth) {
            // Since we have found enough bandwidth, go back and remove
            // the identified BandwidthAllocations from the plan. Then
            // attempt to reinsert them at a later point...
            for (BandwidthAllocation reservation : lowerPriorityRequests) {
                statusHandler.info("Removing request " + reservation
                        + " to make room for request " + request);
                plan.remove(reservation);
            }

            // This should insert the request in the window we just created...
            List<BandwidthAllocation> unscheduled = schedule(plan, request);

            // Now attempt to reschedule the removed requests (but not the
            // reservations),
            // which may result in further rescheduling...
            for (BandwidthAllocation reservation : lowerPriorityRequests) {
                if (reservation instanceof BandwidthAllocation) {
                    unscheduled.addAll(schedule(plan, reservation));
                }
            }
            return unscheduled;
        } else {
            List<BandwidthAllocation> unscheduled = new ArrayList<BandwidthAllocation>(
                    1);
            unscheduled.add(request);
            return unscheduled;
        }
    }
}
