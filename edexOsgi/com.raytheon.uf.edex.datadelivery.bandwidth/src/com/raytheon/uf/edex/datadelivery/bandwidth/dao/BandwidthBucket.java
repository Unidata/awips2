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
package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.util.IDeepCopyable;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;


/**
 * Associates a portion of bandwidth to a given interval of time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2013 2106       djohnson     Extracted from {@link RetrievalPlan}.
 * Dec 17, 2013 2636       bgonzale     Throw exception if attempt to overfill the bucket.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Entity
@Table(name = "bandwidth_bucket")
@SequenceGenerator(name = "BANDWIDTH_SEQ", sequenceName = "bandwidth_seq", allocationSize = 1, initialValue = 1)
public class BandwidthBucket implements Comparable<BandwidthBucket>,
        IPersistableDataObject<Long>, IDeepCopyable<BandwidthBucket> {

    @Id
    @Column(name = "identifier")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BANDWIDTH_SEQ")
    @DynamicSerializeElement
    private long id = BandwidthUtil.DEFAULT_IDENTIFIER;

    // Number of allocated bytes
    @Column
    private long currentSize;

    @Column
    private Network network;

    // Number of bytes of bandwidth;
    @Column
    private long bucketSize;

    @Column
    private long bucketStartTime;

    public BandwidthBucket() {
    }

    public BandwidthBucket(long bucketStartTime, long sizeInBytes,
            Network network) {
        this.bucketStartTime = bucketStartTime;
        this.bucketSize = sizeInBytes;
        this.network = network;
    }

    /**
     * Copy constructor.
     * 
     * @param from
     *            the instance to copy from
     */
    public BandwidthBucket(BandwidthBucket from) {
        this.bucketStartTime = from.bucketStartTime;
        this.bucketSize = from.bucketSize;
        this.currentSize = from.currentSize;
        this.network = from.network;
        this.id = from.id;
    }

    public long getAvailableBandwidth() {
        return bucketSize - currentSize;
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

    public void setCurrentSize(long size) {
        if (size > this.bucketSize) {
            throw new IllegalArgumentException("New data size, " + size
                    + ", is greater than available bucket size "
                    + this.bucketSize);
        }
        this.currentSize = size;
    }

    /**
     * @param bucketSize
     *            the bucketSize to set
     */
    public void setBucketSize(long bucketSize) {
        this.bucketSize = bucketSize;
    }

    /**
     * @param bucketStartTime
     *            the bucketStartTime to set
     */
    public void setBucketStartTime(long bucketStartTime) {
        this.bucketStartTime = bucketStartTime;
    }

    /**
     * @param network
     *            the network to set
     */
    public void setNetwork(Network network) {
        this.network = network;
    }

    public long getId() {
        return id;
    }

    @Override
    public Long getIdentifier() {
        return Long.valueOf(id);
    }

    public void setId(long id) {
        this.id = id;
    }

    public void setIdentifier(Long identifier) {
        setId(identifier.longValue());
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        Calendar b = BandwidthUtil.now();
        b.setTimeInMillis(bucketStartTime);
        sb.append("Bucket [").append(getBucketStartTime()).append("] [")
                .append(BandwidthUtil.format(b));
        sb.append("] bandwidth [").append(bucketSize);
        sb.append("] available [").append(getAvailableBandwidth())
                .append("]);");

        return sb.toString();
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
        result = prime * result + ((network == null) ? 0 : network.hashCode());
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
        if (network != other.network)
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

    /**
     * @return
     */
    public Network getNetwork() {
        return network;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthBucket copy() {
        return new BandwidthBucket(this);
    }
}
