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
package com.raytheon.uf.edex.datadelivery.bandwidth;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedSet;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.datadelivery.bandwidth.data.TimeWindowData;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.BandwidthReservation;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan.BandwidthBucket;

/**
 * Adapts the {@link BandwidthManager} formatted data into a GUI usable graphing
 * object format.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 6, 2012  1397      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

class BandwidthGraphDataAdapter {

    private final RetrievalManager retrievalManager;

    /**
     * Constructor.
     * 
     * @param retrievalManager
     *            the bucket time in minutes
     * @param bandwidthDao
     *            the bandwidth dao
     */
    public BandwidthGraphDataAdapter(RetrievalManager retrievalManager) {
        this.retrievalManager = retrievalManager;
    }

    /**
     * Return the adapted graph data.
     * 
     * @return the data
     */
    public BandwidthGraphData get() {
        // Technically this is wrong, because different Networks can have
        // different bucket minutes. The BandwidthGraphData object should be
        // changed later to reflect this
        final BandwidthGraphData bandwidthGraphData = new BandwidthGraphData(
                retrievalManager.getRetrievalPlans().values().iterator().next()
                        .getBucketMinutes());

        Map<String, List<TimeWindowData>> dataMap = new HashMap<String, List<TimeWindowData>>();
        Map<String, Integer> priorityMap = new HashMap<String, Integer>();

        Map<Long, SubscriptionRetrieval> retrievals = new HashMap<Long, SubscriptionRetrieval>();
        Multimap<Long, BandwidthReservation> reservations = ArrayListMultimap
                .create();
        Multimap<String, SubscriptionRetrieval> subNameToRetrievals = ArrayListMultimap
                .create();

        Collection<RetrievalPlan> retrievalPlans = retrievalManager
                .getRetrievalPlans().values();
        for (RetrievalPlan retrievalPlan : retrievalPlans) {
            // Get all buckets that are in the retrieval plan from the current
            // time forward
            final SortedSet<BandwidthBucket> bandwidthBuckets = retrievalPlan
                    .getBucketsInWindow(TimeUtil.currentTimeMillis(),
                            Long.MAX_VALUE);

            // Add all subscription retrievals to a collection keyed by sub
            // name, and associate all of the bandwidth reservations with their
            // associated retrievals
            for (BandwidthBucket bucket : bandwidthBuckets) {
                final List<BandwidthAllocation> requests = bucket.getRequests();
                for (BandwidthAllocation allocation : requests) {
                    if (allocation instanceof SubscriptionRetrieval) {
                        final SubscriptionRetrieval subRetrieval = (SubscriptionRetrieval) allocation;
                        retrievals.put(allocation.getId(), subRetrieval);
                        subNameToRetrievals.put(subRetrieval
                                .getSubscriptionDao().getName(), subRetrieval);
                    }
                }

                final List<BandwidthReservation> bandwidthReservations = bucket
                        .getReservations();
                for (BandwidthReservation reservation : bandwidthReservations) {
                    reservations.put(reservation.getId(), reservation);
                }
            }
        }

        // Create time windows for each subscription retrieval by aggregating
        // them with an reservations they have
        for (Entry<Long, SubscriptionRetrieval> entry : retrievals.entrySet()) {
            final SubscriptionRetrieval value = entry.getValue();
            SubscriptionDao dao = value.getSubscriptionDao();
            final String subName = dao.getName();
            priorityMap.put(subName, Integer.valueOf((int) dao.getPriority()));

            List<TimeWindowData> timeWindows = dataMap.get(subName);

            if (timeWindows == null) {
                timeWindows = new ArrayList<TimeWindowData>();
                dataMap.put(subName, timeWindows);
            }

            final long startMillis = value.getStartTime().getTimeInMillis();
            final long endMillis = startMillis
                    + (value.getSubscriptionLatency() * TimeUtil.MILLIS_PER_MINUTE);
            TimeWindowData window = new TimeWindowData(startMillis, endMillis);

            List<Long> binStartTimes = new ArrayList<Long>();
            binStartTimes.add(value.getStartTime().getTimeInMillis());
            for (BandwidthReservation reservation : reservations.get(value
                    .getIdentifier())) {
                binStartTimes.add(reservation.getBandwidthBucket());
            }
            window.setBinStartTimes(binStartTimes);
            timeWindows.add(window);
        }

        bandwidthGraphData.setDataMap(dataMap);
        bandwidthGraphData.setPriorityMap(priorityMap);

        return bandwidthGraphData;
    }

}
