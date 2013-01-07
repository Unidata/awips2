package com.raytheon.uf.common.datadelivery.registry;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.datadelivery.registry.Provider.ProviderType;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A bundle of {@link Subscription}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SubscriptionBundle implements ISerializableObject {

    public SubscriptionBundle() {

    }

    @XmlElement
    @DynamicSerializeElement
    private Subscription subscription;

    /**
     * If this is an aggregation, these are the subscriptions being full filled
     * that may need to be subsetted/stored separately from what is returned.
     */
    @XmlElements({ @XmlElement(name = "subscriptionsMet", type = Subscription.class) })
    @DynamicSerializeElement
    private ArrayList<Subscription> subscriptionsMet;

    @XmlAttribute
    @DynamicSerializeElement
    private String bundleId;

    @XmlAttribute
    @DynamicSerializeElement
    private Integer priority;

    @XmlElement
    @DynamicSerializeElement
    private Connection connection;

    @XmlElement
    @DynamicSerializeElement
    private Provider provider;

    public Subscription getSubscription() {
        return subscription;
    }

    public void setSubscription(Subscription subscription) {
        this.subscription = subscription;
    }

    public ArrayList<Subscription> getSubscriptionsMet() {
        return subscriptionsMet;
    }

    public void setSubscriptionsMet(ArrayList<Subscription> subscriptionsMet) {
        this.subscriptionsMet = subscriptionsMet;
    }

    public String getBundleId() {
        return bundleId;
    }

    public void setBundleId(String bundleId) {
        this.bundleId = bundleId;
    }

    public Integer getPriority() {
        return priority;
    }

    public void setPriority(Integer priority) {
        this.priority = priority;
    }

    public void setConnection(Connection connection) {
        this.connection = connection;
    }

    public Connection getConnection() {
        return connection;
    }

    public void setProvider(Provider provider) {
        this.provider = provider;
    }

    public Provider getProvider() {
        return provider;
    }

    /**
     * Get the data type from the bundle.
     * 
     * @return the type
     */
    public ProviderType getDataType() {
        ProviderType pt = null;
        if (subscription != null) {
            if (subscription.getCoverage() instanceof GriddedCoverage) {
                pt = ProviderType.GRID;
            } else {
                pt = ProviderType.POINT;
            }
        }

        return pt;
    }

}
