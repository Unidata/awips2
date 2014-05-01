package com.raytheon.uf.common.datadelivery.service.subscription;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;


/**
 * Configuration for the {@link ISubscriptionOverlapService}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2013    2386    dhladky     Point Subscription Overlap
 * Oct 21, 2013    2292    mpduff      Changes for new implementation of point rules
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class PointSubscriptionOverlapConfig extends SubscriptionOverlapConfig {

    @XmlElement(required = true)
    private int maxAllowedTimeDuplication;

    /**
     * Constructor.
     */
    public PointSubscriptionOverlapConfig() {

    }

    /**
     * Constructor.
     * 
     * @param maxAllowedParameterDuplication
     * @param maxAllowedTimeDuplication
     * @param maxAllowedSpatialDuplication
     * @param matchStrategy
     */
    public PointSubscriptionOverlapConfig(int maxAllowedParameterDuplication,
            int maxAllowedTimeDuplication, int maxAllowedSpatialDuplication,
            SubscriptionOverlapMatchStrategy matchStrategy) {

        this.maxAllowedParameterDuplication = maxAllowedParameterDuplication;
        this.maxAllowedTimeDuplication = maxAllowedTimeDuplication;
        this.maxAllowedSpatialDuplication = maxAllowedSpatialDuplication;
        this.matchStrategy = matchStrategy;
    }

    /**
     * @return the maxAllowedTimeDuplication
     */
    public int getMaxAllowedTimeDuplication() {
        return maxAllowedTimeDuplication;
    }

    /**
     * @param maxAllowedTimeDuplication
     *            the maxAllowedTimeDuplication to set
     */
    public void setMaxAllowedTimeDuplication(int maxAllowedTimeDuplication) {
        this.maxAllowedTimeDuplication = maxAllowedTimeDuplication;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionOverlapConfig getNeverOverlaps() {
        return new PointSubscriptionOverlapConfig(
                SubscriptionOverlapConfig.ONE_HUNDRED_PERCENT,
                SubscriptionOverlapConfig.ONE_HUNDRED_PERCENT,
                SubscriptionOverlapConfig.ONE_HUNDRED_PERCENT,
                SubscriptionOverlapMatchStrategy.MATCH_ALL);
    }
}
