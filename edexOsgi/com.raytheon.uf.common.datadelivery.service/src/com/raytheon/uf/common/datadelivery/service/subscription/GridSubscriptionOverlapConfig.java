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
 * Sept 24, 2013 2386       dhladky     Grid Subscription Overlap
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class GridSubscriptionOverlapConfig extends SubscriptionOverlapConfig {
     
    @XmlElement(required = true)
    private int maxAllowedForecastHourDuplication;

    @XmlElement(required = true)
    private int maxAllowedCycleDuplication;

    /**
     * Constructor.
     */
    public GridSubscriptionOverlapConfig() {
      
    }

    /**
     * Constructor.
     * 
     * @param maxAllowedParameterDuplication
     * @param maxAllowedForecastHourDuplication
     * @param maxAllowedCycleDuplication
     * @param maxAllowedSpatialDuplication
     * @param matchStrategy
     */
    public GridSubscriptionOverlapConfig(int maxAllowedParameterDuplication,
            int maxAllowedForecastHourDuplication,
            int maxAllowedCycleDuplication, int maxAllowedSpatialDuplication,
            SubscriptionOverlapMatchStrategy matchStrategy) {
        
        this.maxAllowedParameterDuplication = maxAllowedParameterDuplication;
        this.maxAllowedForecastHourDuplication = maxAllowedForecastHourDuplication;
        this.maxAllowedCycleDuplication = maxAllowedCycleDuplication;
        this.maxAllowedSpatialDuplication = maxAllowedSpatialDuplication;
        this.matchStrategy = matchStrategy;
    }

    /**
     * @return the maxAllowedForecastHourDuplication
     */
    public int getMaxAllowedForecastHourDuplication() {
        return maxAllowedForecastHourDuplication;
    }

    /**
     * @param maxAllowedForecastHourDuplication
     *            the maxAllowedForecastHourDuplication to set
     */
    public void setMaxAllowedForecastHourDuplication(
            int maxAllowedForecastHourDuplication) {
        this.maxAllowedForecastHourDuplication = maxAllowedForecastHourDuplication;
    }

    /**
     * @return the maxAllowedCycleDuplication
     */
    public int getMaxAllowedCycleDuplication() {
        return maxAllowedCycleDuplication;
    }

    /**
     * @param maxAllowedCycleDuplication
     *            the maxAllowedCycleDuplication to set
     */
    public void setMaxAllowedCycleDuplication(int maxAllowedCycleDuplication) {
        this.maxAllowedCycleDuplication = maxAllowedCycleDuplication;
    }

    /**
     * Check whether the given duplication percents indicate an overlapping
     * subscription.
     * 
     * @param parameterDuplicationPercent
     * @param forecastHourDuplicationPercent
     * @param cycleDuplicationPercent
     * @param spatialDuplicationPercent
     * @return true if the subscription should be considered overlapping
     */
    public boolean isOverlapping(int parameterDuplicationPercent,
            int forecastHourDuplicationPercent, int cycleDuplicationPercent,
            int spatialDuplicationPercent) {

        // Pass through to the match strategy
        return this.matchStrategy.isOverlapping(this,
                parameterDuplicationPercent, forecastHourDuplicationPercent,
                cycleDuplicationPercent, spatialDuplicationPercent);
    }
    
    public SubscriptionOverlapConfig getNeverOverlaps() {
        return new GridSubscriptionOverlapConfig(
                ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
                ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
                ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
                ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
                SubscriptionOverlapMatchStrategy.MATCH_ALL);
    }
    
}
