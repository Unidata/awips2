package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;

import org.hibernate.annotations.IndexColumn;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class representing a Subscription that may have been aggregated with other
 * subscriptions to maximize bandwidth usage.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 02, 2012 726        jspinks     Initial creation
 * Oct 16, 2012 0726       djohnson    Added explicit length to subSubscription, 
 *                                     made it nullable for single table strategy.
 * Nov 09, 2012 1286       djohnson    Add reference back to owning BandwidthSubscription.
 * Jun 24, 2013 2106       djohnson    Add copy constructor.
 * Jul 11, 2013 2106       djohnson    Use SubscriptionPriority enum, lazy load the Subscription object.
 * 
 * </pre>
 * 
 * @version 1.0
 */
@Entity
@DiscriminatorValue("SubscriptionRetrieval")
@DynamicSerialize
public class SubscriptionRetrieval extends BandwidthAllocation {

    private static final long serialVersionUID = 4563049024191145668L;

    @Column
    @DynamicSerializeElement
    private int dataSetAvailablityDelay;

    /**
     * A link to the owning BandwidthSubscription entity.
     */
    @DynamicSerializeElement
    @ManyToOne(fetch = FetchType.EAGER, optional = true, cascade = CascadeType.PERSIST)
    // Must be nullable because we use a single table strategy
    @IndexColumn(name = "subscriptionid_fk", nullable = true)
    private BandwidthSubscription bandwidthSubscription;

    @Column
    @DynamicSerializeElement
    private int subscriptionLatency;

    @Column
    @DynamicSerializeElement
    private String subsumedBy;

    /**
     * Constructor.
     */
    public SubscriptionRetrieval() {
    }

    /**
     * Copy constructor.
     * 
     * @param from
     *            the instance to copy from
     */
    public SubscriptionRetrieval(SubscriptionRetrieval from) {
        super(from);
        this.setBandwidthSubscription(from.getBandwidthSubscription().copy());
        this.setDataSetAvailablityDelay(from.dataSetAvailablityDelay);
        this.setSubscriptionLatency(from.getSubscriptionLatency());
        this.setSubsumedBy(from.getSubsumedBy());
    }

    /**
     * @return the dataSetAvailablityDelay
     */
    public int getDataSetAvailablityDelay() {
        return dataSetAvailablityDelay;
    }

    /**
     * @return the subscriptionLatency
     */
    public int getSubscriptionLatency() {
        return subscriptionLatency;
    }

    public String getSubsumedBy() {
        return subsumedBy;
    }

    /**
     * @return the subsumed
     */
    public boolean isSubsumed() {
        return (subsumedBy != null);
    }

    public void setDataSetAvailablityDelay(int dataSetAvailablityDelay) {
        this.dataSetAvailablityDelay = dataSetAvailablityDelay;

    }

    /**
     * @param subscriptionLatency
     *            the subscriptionLatency to set
     */
    public void setSubscriptionLatency(int subscriptionLatency) {
        this.subscriptionLatency = subscriptionLatency;
    }

    public void setSubsumedBy(String subsumedBy) {
        this.subsumedBy = subsumedBy;
    }

    /**
     * @return the bandwidthSubscription
     */
    public BandwidthSubscription getBandwidthSubscription() {
        return bandwidthSubscription;
    }

    /**
     * @param bandwidthSubscription
     *            the bandwidthSubscription to set
     */
    public void setBandwidthSubscription(
            BandwidthSubscription bandwidthSubscription) {
        this.bandwidthSubscription = bandwidthSubscription;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionRetrieval copy() {
        return new SubscriptionRetrieval(this);
    }
}
