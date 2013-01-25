package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import java.io.Serializable;
import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * 
 * Data access object for subscriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2012 0726       djohnson     Added SW history, added length to subscription.
 * Nov 09, 2012 1286       djohnson     Add convenience methods for retrieving the subscription.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Entity
@Table(name = "bandwidth_subscription")
@DynamicSerialize
@SequenceGenerator(name = "BANDWIDTH_SEQ", sequenceName = "bandwidth_seq", allocationSize = 1)
public class SubscriptionDao extends PersistableDataObject<Long> implements
        Serializable, ISerializableObject {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionDao.class);

    private static final long serialVersionUID = 20120723L;

    @DynamicSerializeElement
    @Column(nullable = false)
    private String dataSetName;

    @DynamicSerializeElement
    @Column(nullable = false)
    private long estimatedSize;

    @Id
    @Column(name = "identifier")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BANDWIDTH_SEQ")
    @DynamicSerializeElement
    private long id = BandwidthUtil.DEFAULT_IDENTIFIER;

    @DynamicSerializeElement
    @Column(nullable = false)
    private String owner;

    @DynamicSerializeElement
    @Column(nullable = false)
    private String name;

    @DynamicSerializeElement
    @Column(nullable = false)
    @Enumerated(EnumType.STRING)
    private Network route;

    @DynamicSerializeElement
    @Column(nullable = false)
    private String provider;

    @DynamicSerializeElement
    @Column(nullable = false)
    private Calendar baseReferenceTime;

    @DynamicSerializeElement
    @Column(nullable = false)
    private int cycle;

    @DynamicSerializeElement
    @Column(nullable = false)
    private double priority;

    @DynamicSerializeElement
    @Column(nullable = false)
    private String registryId;

    @DynamicSerializeElement
    @Column(nullable = false, length = 100000)
    private byte[] subSubscription;

    @Transient
    private transient Subscription subscription;

    public void setRegistryId(String registryId) {
        this.registryId = registryId;
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

    public SubscriptionDao() {
        // Bean constructor
    }

    /**
     * @return the datasetName
     */
    public String getDataSetName() {
        return dataSetName;
    }

    /**
     * @return the estimatedSize in kB (kilobytes)
     */
    public long getEstimatedSize() {
        return estimatedSize;
    }

    @Override
    public Long getIdentifier() {
        return Long.valueOf(this.id);
    }

    /**
     * @param id
     *            the id to set
     */
    @Override
    public void setIdentifier(Long identifier) {
        this.id = identifier;
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
     * @return the owner
     */
    public String getOwner() {
        return owner;
    }

    /**
     * @return the route
     */
    public Network getRoute() {
        return route;
    }

    /**
     * @return the provider
     */
    public String getProvider() {
        return provider;
    }

    /**
     * @param dataSetName
     *            the datasetName to set
     */
    public void setDataSetName(String dataSetName) {
        this.dataSetName = dataSetName;
    }

    /**
     * @param estimatedSize
     *            the estimatedSize to set in kB (kilobytes)
     */
    public void setEstimatedSize(long estimatedSize) {
        this.estimatedSize = estimatedSize;
    }

    /**
     * @param owner
     *            the owner to set
     */
    public void setOwner(String owner) {
        this.owner = owner;
    }

    /**
     * @param route
     *            the route to set
     */
    public void setRoute(Network route) {
        this.route = route;
    }

    /**
     * @param provider
     *            the provider to set
     */
    public void setProvider(String provider) {
        this.provider = provider;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setBaseReferenceTime(Calendar baseReferenceTime) {
        this.baseReferenceTime = baseReferenceTime;
    }

    public Calendar getBaseReferenceTime() {
        return baseReferenceTime;
    }

    public void setPriority(double priority) {
        this.priority = priority;
    }

    public double getPriority() {
        return priority;
    }

    /**
     * @param cycle
     *            the cycle to set
     */
    public void setCycle(int cycle) {
        this.cycle = cycle;
    }

    /**
     * @return the cycle
     */
    public int getCycle() {
        return cycle;
    }

    public String getRegistryId() {
        return registryId;
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
