/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
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
 * May 08, 2013 2000       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class SubscriptionOverlapConfig {

    public static final SubscriptionOverlapConfig NEVER_OVERLAPS = new SubscriptionOverlapConfig(
            ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
            ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
            ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
            ISubscriptionOverlapService.ONE_HUNDRED_PERCENT,
            SubscriptionOverlapMatchStrategy.MATCH_ALL);

    @XmlElement(required = true)
    private int maxAllowedParameterDuplication;

    @XmlElement(required = true)
    private int maxAllowedForecastHourDuplication;

    @XmlElement(required = true)
    private int maxAllowedCycleDuplication;

    @XmlElement(required = true)
    private int maxAllowedSpatialDuplication;

    @XmlElement(required = true)
    private SubscriptionOverlapMatchStrategy matchStrategy;

    /**
     * Constructor.
     */
    public SubscriptionOverlapConfig() {
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
    public SubscriptionOverlapConfig(int maxAllowedParameterDuplication,
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
     * @return the maxAllowedParameterDuplication
     */
    public int getMaxAllowedParameterDuplication() {
        return maxAllowedParameterDuplication;
    }

    /**
     * @param maxAllowedParameterDuplication
     *            the maxAllowedParameterDuplication to set
     */
    public void setMaxAllowedParameterDuplication(
            int maxAllowedParameterDuplication) {
        this.maxAllowedParameterDuplication = maxAllowedParameterDuplication;
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
     * @return the maxAllowedSpatialDuplication
     */
    public int getMaxAllowedSpatialDuplication() {
        return maxAllowedSpatialDuplication;
    }

    /**
     * @param maxAllowedSpatialDuplication
     *            the maxAllowedSpatialDuplication to set
     */
    public void setMaxAllowedSpatialDuplication(int maxAllowedSpatialDuplication) {
        this.maxAllowedSpatialDuplication = maxAllowedSpatialDuplication;
    }

    /**
     * @return the matchStrategy
     */
    public SubscriptionOverlapMatchStrategy getMatchStrategy() {
        return matchStrategy;
    }

    /**
     * @param matchStrategy
     *            the matchStrategy to set
     */
    public void setMatchStrategy(SubscriptionOverlapMatchStrategy matchStrategy) {
        this.matchStrategy = matchStrategy;
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
}
