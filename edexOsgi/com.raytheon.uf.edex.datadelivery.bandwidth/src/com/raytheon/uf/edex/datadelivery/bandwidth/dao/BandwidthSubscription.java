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

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.IDeepCopyable;
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
 * Jun 13, 2013 2095       djohnson     Add flag for whether or not data set update should be looked for on aggregating.
 * Jun 24, 2013 2106       djohnson     Add copy constructor.
 * Jul 11, 2013 2106       djohnson     Use SubscriptionPriority enum, remove the Subscription.
 * Oct 30, 2013  2448      dhladky      Moved methods to TimeUtil.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Entity
@Table(name = "bandwidth_subscription")
@DynamicSerialize
@SequenceGenerator(name = "BANDWIDTH_SEQ", sequenceName = "bandwidth_seq", allocationSize = 1, initialValue = 1)
public class BandwidthSubscription extends PersistableDataObject<Long>
        implements Serializable, IDeepCopyable<BandwidthSubscription> {

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
    @Enumerated(EnumType.STRING)
    private SubscriptionPriority priority;

    @DynamicSerializeElement
    @Column(nullable = false)
    private String registryId;

    @DynamicSerializeElement
    @Column(nullable = false)
    private boolean checkForDataSetUpdate;

    public void setRegistryId(String registryId) {
        this.registryId = registryId;
    }

    public BandwidthSubscription() {
        // Bean constructor
    }

    /**
     * @param bandwidthSubscription
     */
    public BandwidthSubscription(BandwidthSubscription bandwidthSubscription) {
        this.baseReferenceTime = TimeUtil.newCalendar(bandwidthSubscription
                .getBaseReferenceTime());
        this.checkForDataSetUpdate = bandwidthSubscription.checkForDataSetUpdate;
        this.cycle = bandwidthSubscription.cycle;
        this.dataSetName = bandwidthSubscription.dataSetName;
        this.estimatedSize = bandwidthSubscription.estimatedSize;
        this.id = bandwidthSubscription.id;
        this.identifier = bandwidthSubscription.identifier;
        this.name = bandwidthSubscription.name;
        this.owner = bandwidthSubscription.owner;
        this.priority = bandwidthSubscription.priority;
        this.provider = bandwidthSubscription.provider;
        this.registryId = bandwidthSubscription.registryId;
        this.route = bandwidthSubscription.route;
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

    public void setPriority(SubscriptionPriority priority) {
        this.priority = priority;
    }

    public SubscriptionPriority getPriority() {
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
     * @return the checkForDataSetUpdate
     */
    public boolean isCheckForDataSetUpdate() {
        return checkForDataSetUpdate;
    }

    /**
     * @param checkForDataSetUpdate
     *            the checkForDataSetUpdate to set
     */
    public void setCheckForDataSetUpdate(boolean checkForDataSetUpdate) {
        this.checkForDataSetUpdate = checkForDataSetUpdate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthSubscription copy() {
        return new BandwidthSubscription(this);
    }
}
