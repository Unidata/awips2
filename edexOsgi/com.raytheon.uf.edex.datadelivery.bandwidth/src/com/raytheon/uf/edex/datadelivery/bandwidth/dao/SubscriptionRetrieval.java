package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;

import org.hibernate.annotations.IndexColumn;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

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
 * Nov 09, 2012 1286       djohnson    Add reference back to owning SubscriptionDao.
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

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionRetrieval.class);

    @Column
    @DynamicSerializeElement
    private int dataSetAvailablityDelay;

    @DynamicSerializeElement
    // Must be nullable because we use a single table strategy
    @Column(nullable = true, length = 100000)
    private byte[] subSubscription;

    /**
     * A link to the owning SubscriptionDao entity.
     */
    @DynamicSerializeElement
    @ManyToOne(fetch = FetchType.EAGER, optional = true)
    // Must be nullable because we use a single table strategy
    @IndexColumn(name = "subscriptionid_fk", nullable = true)
    private SubscriptionDao subscriptionDao;

    @Column
    @DynamicSerializeElement
    private int subscriptionLatency;

    @Column
    @DynamicSerializeElement
    private String subsumedBy;

    @Transient
    private transient Subscription subscription;

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
     * @return the subscriptionDao
     */
    public SubscriptionDao getSubscriptionDao() {
        return subscriptionDao;
    }

    /**
     * @param subscriptionDao
     *            the subscriptionDao to set
     */
    public void setSubscriptionDao(SubscriptionDao subscriptionDao) {
        this.subscriptionDao = subscriptionDao;
    }

    /**
     * @return the subSubscription
     * @throws SerializationException
     */
    public Subscription getSubscription() throws SerializationException {
        if (subscription == null) {
            if (subSubscription != null) {
                subscription = SerializationUtil.transformFromThrift(
                        Subscription.class, subSubscription);
            } else {
                statusHandler.handle(Priority.WARN,
                        "Null subSubscription as field, not deserializing.");
            }

        }
        return subscription;
    }

    /**
     * @param sub
     * @throws SerializationException
     */
    public void setSubscription(Subscription sub) throws SerializationException {
        // Set the transient field subscription so that we don't
        // have to deserialize the subscription if it was set
        // already.
        this.subscription = sub;
        if (sub != null) {
            this.subSubscription = SerializationUtil.transformToThrift(sub);
        } else {
            statusHandler.handle(Priority.WARN,
                    "Null subscription passed as parameter, not serializing.");
        }
    }

    /**
     * Added only to comply with DynamicSerialize, use
     * {@link #getSubscription()} instead.
     * 
     * @deprecated
     * @return the subSubscription the raw bytes of the serialized subscription
     */
    @Deprecated
    public byte[] getSubSubscription() {
        return subSubscription;
    }

    /**
     * Added only to comply with DynamicSerialize, use
     * {@link #setSubscription()} instead.
     * 
     * @deprecated
     * @param subSubscription
     *            the subSubscription to set
     */
    @Deprecated
    public void setSubSubscription(byte[] subSubscription) {
        this.subSubscription = subSubscription;
    }
}
