package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.IDeepCopyable;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * Holds attributes for {@link SubscriptionRetrieval} instances that should be
 * only loaded on-demand.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013 2106       djohnson    Initial creation
 * Oct 2,  2013 1797       dhladky     Applied some generics
 * 
 * </pre>
 * 
 * @version 1.0
 */
@Entity
@Table(name = "bandwidth_subscription_retrieval_attributes")
@SequenceGenerator(name = "BANDWIDTH_SEQ", sequenceName = "bandwidth_seq", allocationSize = 1, initialValue = 1)
@DynamicSerialize
public class SubscriptionRetrievalAttributes<T extends Time, C extends Coverage> implements
        IPersistableDataObject<Long>, Serializable, ISerializableObject,
        IDeepCopyable<SubscriptionRetrievalAttributes<T, C>> {

    private static final long serialVersionUID = 4563049024191145668L;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionRetrievalAttributes.class);

    @Id
    @Column(name = "identifier")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BANDWIDTH_SEQ")
    @DynamicSerializeElement
    private long id = BandwidthUtil.DEFAULT_IDENTIFIER;

    @DynamicSerializeElement
    // Must be nullable because we use a single table strategy
    @Column(nullable = true)
    private byte[] subSubscription;

    @Transient
    private transient Subscription<T, C> subscription;

    @DynamicSerializeElement
    @OneToOne(fetch = FetchType.EAGER, optional = false, cascade = CascadeType.PERSIST)
    private SubscriptionRetrieval subscriptionRetrieval;

    /**
     * Constructor.
     */
    public SubscriptionRetrievalAttributes() {
    }

    /**
     * Copy constructor.
     * 
     * @param from
     *            the instance to copy from
     */
    public SubscriptionRetrievalAttributes(SubscriptionRetrievalAttributes<T, C> from) {
        if (from.subSubscription != null) {
            final int srcLength = from.subSubscription.length;
            this.subSubscription = new byte[srcLength];
            System.arraycopy(from.subSubscription, 0, this.subSubscription, 0,
                    srcLength);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionRetrievalAttributes<T, C> copy() {
        return new SubscriptionRetrievalAttributes<T, C>(this);
    }

    /**
     * @return the subSubscription
     * @throws SerializationException
     */
    public Subscription<T, C> getSubscription() throws SerializationException {
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
    public void setSubscription(Subscription<T, C> sub) throws SerializationException {
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

    /**
     * {@inheritDoc}
     */
    @Override
    public Long getIdentifier() {
        return id;
    }

    /**
     * @return the id
     */
    public long getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(long id) {
        this.id = id;
    }

    /**
     * 
     * @param subscriptionRetrieval
     */
    public void setSubscriptionRetrieval(
            SubscriptionRetrieval subscriptionRetrieval) {
        this.subscriptionRetrieval = subscriptionRetrieval;
    }

    /**
     * 
     * @return
     */
    public SubscriptionRetrieval getSubscriptionRetrieval() {
        return this.subscriptionRetrieval;
    }
}