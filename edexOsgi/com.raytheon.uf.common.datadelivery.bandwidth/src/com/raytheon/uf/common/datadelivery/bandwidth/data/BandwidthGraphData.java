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
package com.raytheon.uf.common.datadelivery.bandwidth.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response object for the GraphDataRequest.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 25, 2012    1269    lvenable    Initial creation.
 * Dec 06, 2012    1397    djohnson    Add dynamic serialize class annotation.
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
@DynamicSerialize
public class BandwidthGraphData {
    /** Subscription Name -> TimeWindowData list */
    @DynamicSerializeElement
    private Map<String, List<TimeWindowData>> dataMap;

    /** Subscription Name -> Subscription Priority */
    @DynamicSerializeElement
    private Map<String, Integer> priorityMap;

    /** Bin duration in minutes */
    @DynamicSerializeElement
    private int binTimeInMins;

    /**
     * Constructor.
     *
     * @deprecated Required by dynamic serialization, use
     *             {@link #BandwidthGraphData(int)} instead.
     */
    @Deprecated
    public BandwidthGraphData() {
        this(3);
    }

    /**
     * Constructor.
     *
     * @param binTimeMins
     *            bin duration in minutes
     */
    public BandwidthGraphData(int binTimeMins) {
        this.binTimeInMins = binTimeMins;
        dataMap = new HashMap<String, List<TimeWindowData>>();
        priorityMap = new HashMap<String, Integer>();
    }

    /**
     * @return the dataMap
     */
    public Map<String, List<TimeWindowData>> getDataMap() {
        return dataMap;
    }

    /**
     * @param dataMap
     *            the dataMap to set
     */
    public void setDataMap(Map<String, List<TimeWindowData>> dataMap) {
        this.dataMap = dataMap;
    }

    /**
     * @return the priorityMap
     */
    public Map<String, Integer> getPriorityMap() {
        return priorityMap;
    }

    /**
     * @param priorityMap
     *            the priorityMap to set
     */
    public void setPriorityMap(Map<String, Integer> priorityMap) {
        this.priorityMap = priorityMap;
    }

    /**
     * @return the binTimeInMins
     */
    public int getBinTimeInMins() {
        return binTimeInMins;
    }

    /**
     * @param binTimeInMins
     *            the binTimeInMins to set
     */
    public void setBinTimeInMins(int binTimeInMins) {
        this.binTimeInMins = binTimeInMins;
    }

    /**
     * Get the bin time in minutes
     *
     * @return bin time
     */
    public int getBinTimeInMinutes() {
        return binTimeInMins;
    }

    /**
     * Add a Graph Data Array.
     *
     * @param subscriptionName
     * @param priority
     * @param dataArray
     */
    public void addGraphDataArray(String subscriptionName, int priority,
            List<TimeWindowData> dataArray) {
        dataMap.put(subscriptionName, dataArray);
        priorityMap.put(subscriptionName, priority);
    }

    /**
     * Get the sorted names.
     *
     * @param ascending
     *            sort in ascending or descending, true for ascending
     * @return List of names
     */
    public List<String> getSortedNames(boolean ascending) {
        ArrayList<String> keyArray = new ArrayList<String>();

        for (String s : dataMap.keySet()) {
            keyArray.add(s);
        }

        Collections.sort(keyArray);

        /*
         * Since the data will be drawn from the bottom up, ascending is
         * actually reverse order of the sort.
         */
        if (ascending) {
            Collections.reverse(keyArray);
        }

        return keyArray;
    }

    /**
     * Sort the graph data
     */
    public void sortGraphData() {
        for (Entry<String, List<TimeWindowData>> entry : dataMap.entrySet()) {
            Collections.sort(entry.getValue());
        }
    }

    /**
     * Get the time window array for the subscription
     *
     * @param subName
     *            Subscription name
     *
     * @return List of TimeWindowData objects
     */
    public List<TimeWindowData> getTimeWindowArray(String subName) {
        return dataMap.get(subName);
    }

    /**
     * Get the priority for the subscription name.
     *
     * @param subscriptionName
     *            The subscription name.
     * @return The priority number.
     */
    public int getPriority(String subscriptionName) {
        if (priorityMap.containsKey(subscriptionName)) {
            return priorityMap.get(subscriptionName);
        }

        // This should never occur. A low priority number is being return rather
        // than a null.
        return 99;
    }

    /**
     * Get the number of subscriptions
     *
     * @return Number of subscriptions
     */
    public int getNumberOfSubscriptions() {
        return dataMap.keySet().size();
    }

    /**
     * Get a list of subscription names that are sorted by the earliest time
     * window time. If two subscription share the same time window time then the
     * subscription names will be in alphabetical order.
     *
     * @return List of subscription names.
     */
    public List<String> getSubscriptionsSortedByTime(long currentTime,
            boolean intersect) {
        /*
         * Sort each of the subscriptions array time windows so they are in
         * order by time window start time.
         */
        sortGraphData();

        /*
         * Create a sorted map of time window start times and subscription
         * names.
         */
        Map<Long, List<String>> sortedTimeMap = new TreeMap<Long, List<String>>();

        long startTime = 0L;
        for (Entry<String, List<TimeWindowData>> entry : dataMap.entrySet()) {
            if (!entry.getValue().isEmpty()) {
                for (TimeWindowData data : entry.getValue()) {
                    if (intersect) {
                        if (data.getTimeWindowEndTime() > currentTime) {
                            startTime = data.getTimeWindowStartTime();
                            break;
                        }
                    } else {
                        if (data.getTimeWindowStartTime() >= currentTime) {
                            startTime = data.getTimeWindowStartTime();
                            break;
                        }
                    }
                }
                if (sortedTimeMap.containsKey(startTime)) {
                    sortedTimeMap.get(startTime).add(entry.getKey());
                } else {
                    List<String> tmpArray = new ArrayList<String>();
                    tmpArray.add(entry.getKey());
                    sortedTimeMap.put(startTime, tmpArray);
                }
            }
        }

        /*
         * Take the sorted map of time window start times and subscription names
         * and build a list subscription names that will already be sorted with
         * the most current time windows first.
         */
        List<String> sortedSubscriptionNames = new ArrayList<String>();

        for (Entry<Long, List<String>> entry : sortedTimeMap.entrySet()) {
            Collections.sort(entry.getValue());
            sortedSubscriptionNames.addAll(entry.getValue());
        }

        return sortedSubscriptionNames;
    }
}
