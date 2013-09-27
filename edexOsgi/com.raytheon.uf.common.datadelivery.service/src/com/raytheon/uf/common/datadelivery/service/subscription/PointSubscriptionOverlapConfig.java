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
 * Sept 24, 2013 2386       dhladky     Point Subscription Overlap
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
     * @param notUsed
     * @param maxAllowedSpatialDuplication
     * @param matchStrategy
     */
    public PointSubscriptionOverlapConfig(int maxAllowedParameterDuplication,
            int maxAllowedTimeDuplication, int notUsed,
            int maxAllowedSpatialDuplication,
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
     * @param maxAllowedForecastHourDuplication
     *            the maxAllowedForecastHourDuplication to set
     */
    public void setMaxAllowedTimeDuplication(int maxAllowedTimeDuplication) {
        this.maxAllowedTimeDuplication = maxAllowedTimeDuplication;
    }

    /**
     * Check whether the given duplication percents indicate an overlapping
     * subscription.
     * 
     * @param parameterDuplicationPercent
     * @param timeDuplicationPercent
     * @param notUsed
     * @param spatialDuplicationPercent
     * @return true if the subscription should be considered overlapping
     */
    public boolean isOverlapping(int parameterDuplicationPercent,
            int timeDuplicationPercent, int notUsed,
            int spatialDuplicationPercent) {

        // Pass through to the match strategy
        return this.matchStrategy.isOverlapping(this,
                parameterDuplicationPercent, timeDuplicationPercent, notUsed,
                spatialDuplicationPercent);
    }

    @Override
    public SubscriptionOverlapConfig getNeverOverlaps() {
        return new PointSubscriptionOverlapConfig(
                ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
                ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
                ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
                ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
                SubscriptionOverlapMatchStrategy.MATCH_ALL);

    }

}
