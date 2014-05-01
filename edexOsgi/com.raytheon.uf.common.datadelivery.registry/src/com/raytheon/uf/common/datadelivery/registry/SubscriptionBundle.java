package com.raytheon.uf.common.datadelivery.registry;

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
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
 * Mar 29, 2013 1841       djohnson    Remove JAXB annotations.
 * Jul 11, 2013 2106       djohnson    Use SubscriptionPriority.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@DynamicSerialize
public class SubscriptionBundle {

    public SubscriptionBundle() {

    }

    @DynamicSerializeElement
    private Subscription subscription;

    /**
     * If this is an aggregation, these are the subscriptions being full filled
     * that may need to be subsetted/stored separately from what is returned.
     */
    @DynamicSerializeElement
    private List<Subscription> subscriptionsMet;

    @DynamicSerializeElement
    private String bundleId;

    @DynamicSerializeElement
    private SubscriptionPriority priority;

    @DynamicSerializeElement
    private Connection connection;

    @DynamicSerializeElement
    private Provider provider;

    public Subscription getSubscription() {
        return subscription;
    }

    public void setSubscription(Subscription subscription) {
        this.subscription = subscription;
    }

    public List<Subscription> getSubscriptionsMet() {
        return subscriptionsMet;
    }

    public void setSubscriptionsMet(List<Subscription> subscriptionsMet) {
        this.subscriptionsMet = subscriptionsMet;
    }

    public String getBundleId() {
        return bundleId;
    }

    public void setBundleId(String bundleId) {
        this.bundleId = bundleId;
    }

    public SubscriptionPriority getPriority() {
        return priority;
    }

    public void setPriority(SubscriptionPriority priority) {
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
    public DataType getDataType() {
        if (subscription != null) {
            if (subscription.getCoverage() instanceof GriddedCoverage) {
                return DataType.GRID;
            }
            // TODO: Add more data types, currently defaulting to POINT only if
            // not a GriddedCoverage, when there could be other data types than
            // just Grid/Point
            else {
                return DataType.POINT;
            }
        }
        return null;
    }

}
