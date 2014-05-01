package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.io.Serializable;
import java.util.Calendar;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.IDeepCopyable;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * Class to use as a place holder in a retrieval plan for bandwidth required to
 * fulfill a bandwidth allocation that does not fit in one bandwidth bucket.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 02, 2012 726        jspinks     Initial release.
 * Nov 09, 2012 1286       djohnson    Add getters for bytes.
 * Jun 24, 2013 2106       djohnson    Add copy constructor.
 * Jul 11, 2013 2106       djohnson    Use SubscriptionPriority enum.
 * Oct 30, 2013  2448      dhladky     Moved methods to TimeUtil.
 * Dec 02, 2013  2545      mpduff      Set size in bytes.
 * 
 * </pre>
 * 
 * @version 1.0
 */
// TODO: Add Hibernate annotations
public class BandwidthReservation implements Serializable,
        IDeepCopyable<BandwidthReservation> {

    private static final long serialVersionUID = 4556094983346648973L;

    private long id;

    private Network network;

    private SubscriptionPriority priority;

    /** Size of the reservation in bytes */
    private long size;

    private final RetrievalStatus status = RetrievalStatus.RESERVED;

    private Calendar startTime;

    private Calendar endTime;

    private long bandwidthBucket;

    /**
     * Bean constructor.
     */
    public BandwidthReservation() {
    }

    /**
     * Create a BandwidthReservation from a BandwidthAllocation.
     * 
     * @param allocation
     *            The BandwidthAllocation to use to initialize this
     *            BandwidthReservation.
     * 
     * @param bandwidthRequired
     *            The amount of bandwidth in bytes this BandwidthReservation
     *            should reserve in a {@link RetrievalPlan}.
     */
    public BandwidthReservation(BandwidthAllocation allocation,
            long bandwidthRequired) {
        this.setId(allocation.getId());
        this.startTime = allocation.getStartTime();
        this.endTime = allocation.getEndTime();
        this.priority = allocation.getPriority();
        this.network = allocation.getNetwork();
        this.size = bandwidthRequired;
        this.setBandwidthBucket(allocation.getBandwidthBucket());
    }

    /**
     * Copy constructor.
     * 
     * @param from
     *            the instance to copy from
     */
    public BandwidthReservation(BandwidthReservation from) {
        this.bandwidthBucket = from.bandwidthBucket;
        this.endTime = TimeUtil.newCalendar(from.endTime);
        this.id = from.id;
        this.network = from.network;
        this.priority = from.priority;
        this.size = from.size;
        this.startTime = TimeUtil.newCalendar(from.startTime);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("reserve id: [").append(getId()).append("] ");
        sb.append("path [").append(getNetwork()).append("] ");
        sb.append("priority [").append(getPriority()).append("] ");
        sb.append("size (bytes) [").append(getSize()).append("] ");
        sb.append("status [").append(getStatus()).append("] ");
        sb.append("startTime [").append(BandwidthUtil.format(getStartTime()))
                .append("] ");
        sb.append("endTime [").append(BandwidthUtil.format(getEndTime()))
                .append("]");
        return sb.toString();
    }

    /**
     * @param network
     *            the network to set
     */
    public void setNetwork(Network network) {
        this.network = network;
    }

    /**
     * @return the network
     */
    public Network getNetwork() {
        return network;
    }

    /**
     * @param priority
     *            the priority to set
     */
    public void setPriority(SubscriptionPriority priority) {
        this.priority = priority;
    }

    /**
     * @return the priority
     */
    public SubscriptionPriority getPriority() {
        return priority;
    }

    /**
     * @param size
     *            the estimatedSize to set, in bytes
     */
    public void setSize(long size) {
        this.size = size;
    }

    /**
     * @return the estimatedSize, in bytes
     */
    public long getSize() {
        return size;
    }

    /**
     * @return the status
     */
    public RetrievalStatus getStatus() {
        return status;
    }

    /**
     * @param startTime
     *            the startTime to set
     */
    public void setStartTime(Calendar startTime) {
        this.startTime = startTime;
    }

    /**
     * @return the startTime
     */
    public Calendar getStartTime() {
        return startTime;
    }

    /**
     * @param endTime
     *            the endTime to set
     */
    public void setEndTime(Calendar endTime) {
        this.endTime = endTime;
    }

    /**
     * @return the endTime
     */
    public Calendar getEndTime() {
        return endTime;
    }

    /**
     * @param bandwidthBucket
     *            the bandwidthBucket to set
     */
    public void setBandwidthBucket(long bandwidthBucket) {
        this.bandwidthBucket = bandwidthBucket;
    }

    /**
     * @return the bandwidthBucket
     */
    public long getBandwidthBucket() {
        return bandwidthBucket;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(long id) {
        this.id = id;
    }

    /**
     * @return the id
     */
    public long getId() {
        return id;
    }

    /**
     * @return
     */
    @Override
    public BandwidthReservation copy() {
        return new BandwidthReservation(this);
    }
}
