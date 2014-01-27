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

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Describes a bucket, especially the total bytes available and bytes used. Used
 * by the UI to display information about buckets. Comparable by startime and
 * bucketSize
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2013 2397       bgonzale    Initial creation.
 * Nov 25, 2013 2545       mpduff      Added bucketTimeInMinutes.
 * Dec 17, 2013 2636       bgonzale    Refactored bucket fill in edex.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

@DynamicSerialize
public class BandwidthBucketDescription implements
        Comparable<BandwidthBucketDescription> {

    @DynamicSerializeElement
    private Network network;

    // Number of bytes of bandwidth;
    @DynamicSerializeElement
    private long bucketSize;

    // Number of allocated bytes
    @DynamicSerializeElement
    private long usedBytes;

    @DynamicSerializeElement
    private long bucketStartTime;

    @DynamicSerializeElement
    private int bucketTimeMinutes;

    /**
     * Default Constructor.
     */
    public BandwidthBucketDescription() {
    }

    /**
     * Initialize all fields Constructor.
     * 
     * @param network
     * @param bucketSize
     * @param usedBytes
     * @param bucketStartTime
     */
    public BandwidthBucketDescription(Network network, long bucketSize,
            long usedBytes, long bucketStartTime) {
        this.network = network;
        this.bucketSize = bucketSize;
        this.usedBytes = usedBytes;
        this.bucketStartTime = bucketStartTime;
    }

    /**
     * Compare by bucket start time. If bucket start times are equal, then
     * compare by bucket size.
     */
    @Override
    public int compareTo(BandwidthBucketDescription o) {
        long compStart = this.bucketStartTime - o.getBucketStartTime();
        int retval = (int) (compStart == 0 ? this.bucketSize - o.bucketSize
                : compStart);
        return retval;
    }

    /**
     * @return the network
     */
    public Network getNetwork() {
        return network;
    }

    /**
     * @param network
     *            the network to set
     */
    public void setNetwork(Network network) {
        this.network = network;
    }

    /**
     * @return the bucketSize
     */
    public long getBucketSize() {
        return bucketSize;
    }

    /**
     * @param bucketSize
     *            the bucketSize to set
     */
    public void setBucketSize(long bucketSize) {
        this.bucketSize = bucketSize;
    }

    /**
     * @return the usedBytes
     */
    public long getUsedBytes() {
        return usedBytes;
    }

    /**
     * @param usedBytes
     *            the usedBytes to set
     */
    public void setUsedBytes(long usedBytes) {
        this.usedBytes = usedBytes;
    }

    /**
     * @return the bucketStartTime
     */
    public long getBucketStartTime() {
        return bucketStartTime;
    }

    /**
     * @param bucketStartTime
     *            the bucketStartTime to set
     */
    public void setBucketStartTime(long bucketStartTime) {
        this.bucketStartTime = bucketStartTime;
    }

    /**
     * @return the bucketTimeMinutes
     */
    public int getBucketTimeMinutes() {
        return bucketTimeMinutes;
    }

    /**
     * @param bucketTimeMinutes
     *            the bucketTimeMinutes to set
     */
    public void setBucketTimeMinutes(int bucketTimeMinutes) {
        this.bucketTimeMinutes = bucketTimeMinutes;
    }

}
