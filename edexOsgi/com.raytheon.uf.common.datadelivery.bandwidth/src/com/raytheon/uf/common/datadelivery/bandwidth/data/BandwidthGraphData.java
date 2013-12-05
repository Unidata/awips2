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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
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
 * Jan 25, 2013   1528     djohnson    Subscription priority is now an enum.
 * Sep 20, 2013   2397     bgonzale    Added Map of Bucket Descriptions.
 * Nov 19, 2013   2545     bgonzale    Added 'add' method stub.  Still work to do.
 * Nov 25, 2013   2545     mpduff      Finished implementing 2545.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
@DynamicSerialize
public class BandwidthGraphData {
    /** Network -> List of SubscriptionWindowData */
    @DynamicSerializeElement
    private Map<Network, List<SubscriptionWindowData>> networkDataMap;

    /** Network -> Bandwidth Bucket Descriptions */
    @DynamicSerializeElement
    private Map<Network, SortedSet<BandwidthBucketDescription>> networkBucketMap;

    /**
     * Constructor.
     */
    public BandwidthGraphData() {
        networkDataMap = new HashMap<Network, List<SubscriptionWindowData>>(2);
        networkBucketMap = new HashMap<Network, SortedSet<BandwidthBucketDescription>>(
                2);
    }

    /**
     * Save the bucket descriptions.
     * 
     * @param network
     *            The network the buckets belong to
     * @param buckets
     *            The buckets to save
     */
    public void addBucketDescriptions(Network network,
            SortedSet<BandwidthBucketDescription> buckets) {
        networkBucketMap.put(network, buckets);
    }

    /**
     * @return the networkBucketMap
     */
    public Map<Network, SortedSet<BandwidthBucketDescription>> getNetworkBucketMap() {
        return networkBucketMap;
    }

    /**
     * @param networkBucketMap
     *            the networkBucketMap to set
     */
    public void setNetworkBucketMap(
            Map<Network, SortedSet<BandwidthBucketDescription>> networkBucketMap) {
        /*
         * Ensure bucket description set sorting. This is done like this because
         * the thrift version we are using is recreating the SortedSet as a
         * HashSet on deserialization and causing
         * getNetworkBucketMap().get(Network) to throw
         * "java.lang.ClassCastException: java.util.HashSet cannot be cast to java.util.SortedSet"
         */
        this.networkBucketMap = new HashMap<Network, SortedSet<BandwidthBucketDescription>>();
        for (Entry<Network, SortedSet<BandwidthBucketDescription>> descEntry : networkBucketMap
                .entrySet()) {
            this.networkBucketMap.put(
                    descEntry.getKey(),
                    new TreeSet<BandwidthBucketDescription>(
                            (Collection<BandwidthBucketDescription>) descEntry
                                    .getValue()));
        }
    }

    /**
     * Get the sorted names.
     * 
     * @param ascending
     *            sort in ascending or descending, true for ascending
     * @param network
     *            The network type requested
     * @return List of names
     */
    public List<String> getSortedNames(boolean ascending, Network network) {
        List<SubscriptionWindowData> subList = this.networkDataMap.get(network);
        List<String> subNameList = new ArrayList<String>(subList.size());
        for (SubscriptionWindowData s : subList) {
            subNameList.add(s.getSubscriptionName());
        }

        /*
         * Since the data will be drawn from the bottom up, ascending is
         * actually reverse order of the sort.
         */
        if (ascending) {
            Collections.reverse(subNameList);
        } else {
            Collections.sort(subNameList);
        }

        return subNameList;
    }

    /**
     * Sort the graph data
     */
    public void sortGraphData() {
        for (Entry<Network, List<SubscriptionWindowData>> entry : networkDataMap
                .entrySet()) {
            for (SubscriptionWindowData swd : entry.getValue()) {
                Collections.sort(swd.getWindowDataList());
            }
        }
    }

    /**
     * Get the time window array for the subscription
     * 
     * @param network
     *            The Network
     * 
     * @param subName
     *            Subscription name
     * 
     * @return List of TimeWindowData objects
     */
    public List<TimeWindowData> getTimeWindowArray(Network network,
            String subName) {
        for (Entry<Network, List<SubscriptionWindowData>> entry : networkDataMap
                .entrySet()) {
            for (SubscriptionWindowData swd : entry.getValue()) {
                if (swd.getSubscriptionName().equals(subName)) {
                    return swd.getWindowDataList();
                }
            }
        }

        return new ArrayList<TimeWindowData>(0);
    }

    /**
     * Get the priority for the subscription name.
     * 
     * @param network
     *            The network to check for the subscription name
     * 
     * @param subName
     *            The subscription name.
     * @return The priority number.
     */
    public SubscriptionPriority getPriority(Network network, String subName) {
        for (Entry<Network, List<SubscriptionWindowData>> entry : networkDataMap
                .entrySet()) {
            for (SubscriptionWindowData swd : entry.getValue()) {
                if (swd.getSubscriptionName().equals(subName)) {
                    return swd.getPriority();
                }
            }
        }

        // This should never occur.
        throw new IllegalArgumentException(
                "Unable to find a priority for subscription [" + subName + "]");
    }

    /**
     * Get the number of subscriptions
     * 
     * @param network
     *            The network to check
     * 
     * @return Number of subscriptions
     */
    public int getNumberOfSubscriptions(Network network) {
        return networkDataMap.get(network).size();
    }

    /**
     * Get a list of subscription names that are sorted by the earliest time
     * window time. If two subscription share the same time window time then the
     * subscription names will be in alphabetical order.
     * 
     * @param network
     *            The network
     * @param currentTime
     *            The time
     * @param intersect
     *            True if current time intesects the download window
     * 
     * @return List of subscription names.
     */
    public List<String> getSubscriptionsSortedByTime(Network network,
            long currentTime, boolean intersect) {
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
        for (SubscriptionWindowData swd : networkDataMap.get(network)) {
            for (TimeWindowData data : swd.getWindowDataList()) {
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
                sortedTimeMap.get(startTime).add(swd.getSubscriptionName());
            } else {
                List<String> tmpArray = new ArrayList<String>();
                tmpArray.add(swd.getSubscriptionName());
                sortedTimeMap.put(startTime, tmpArray);
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

    /**
     * Get a list of available networks
     * 
     * @return List of available Networks
     */
    public List<String> getAvailableNetworks() {
        List<String> networkList = new ArrayList<String>(3);
        for (Network network : networkDataMap.keySet()) {
            networkList.add(network.name());
        }

        return networkList;
    }

    /**
     * @return the networkDataMap
     */
    public Map<Network, List<SubscriptionWindowData>> getNetworkDataMap() {
        return networkDataMap;
    }

    /**
     * @param networkDataMap
     *            the networkDataMap to set
     */
    public void setNetworkDataMap(
            Map<Network, List<SubscriptionWindowData>> networkDataMap) {
        this.networkDataMap = networkDataMap;
    }

    /**
     * Get the bin time in minutes.
     * 
     * @param network
     *            The network to check
     * @return The bin time in minutes
     */
    public int getBinTimeInMinutes(Network network) {
        Set<BandwidthBucketDescription> bucketSet = this.networkBucketMap
                .get(network);
        return bucketSet.iterator().next().getBucketTimeMinutes();
    }

    /**
     * Merge another Bandwidth graph data into this object.
     * 
     * @param data2
     *            The other data set to merge
     */
    public void merge(BandwidthGraphData data2) {
        Map<Network, SortedSet<BandwidthBucketDescription>> nbm = data2
                .getNetworkBucketMap();

        for (Network network : nbm.keySet()) {
            if (!networkBucketMap.containsKey(network)) {
                networkBucketMap.put(network, nbm.get(network));
            }
        }

        Map<Network, List<SubscriptionWindowData>> ndm = data2
                .getNetworkDataMap();

        for (Network network : ndm.keySet()) {
            if (!networkDataMap.containsKey(network)) {
                networkDataMap.put(network, ndm.get(network));
            }
        }
    }
}
