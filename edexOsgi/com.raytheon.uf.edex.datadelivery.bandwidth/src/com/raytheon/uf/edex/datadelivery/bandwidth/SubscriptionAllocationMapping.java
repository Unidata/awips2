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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;

/**
 * Data class to hold Subscription to download allocations map
 * BandwidthAllocation id to SubscriptionRetrieval map
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 23, 2014   2636     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionAllocationMapping {

    /**
     * Subscription name to list of BandwidthAllocations for the subscription.
     */
    private final Map<String, List<BandwidthAllocation>> subAllocationMap = new HashMap<String, List<BandwidthAllocation>>();

    /**
     * BandwidthAllocation id to SubscriptionRetrieval map.
     */
    private final Map<Long, SubscriptionRetrieval> subRetrievalMap = new HashMap<Long, SubscriptionRetrieval>();

    /**
     * Constructor.
     */
    public SubscriptionAllocationMapping() {

    }

    /**
     * Add the allocation to the subscription list.
     * 
     * @param subName
     *            The subscription name for the allocation
     * @param allocation
     *            The allocation to add to the subscription list
     */
    public void addAllocationForSubscription(String subName,
            BandwidthAllocation allocation) {
        if (subAllocationMap.get(subName) == null) {
            subAllocationMap
                    .put(subName, new ArrayList<BandwidthAllocation>(8));
        }

        subAllocationMap.get(subName).add(allocation);
        subRetrievalMap.put(allocation.getId(),
                (SubscriptionRetrieval) allocation);
    }

    /**
     * @return the subAllocationMap
     */
    public Map<String, List<BandwidthAllocation>> getSubAllocationMap() {
        return subAllocationMap;
    }

    /**
     * @return the subRetrievalMap
     */
    public Map<Long, SubscriptionRetrieval> getSubRetrievalMap() {
        return subRetrievalMap;
    }
}
