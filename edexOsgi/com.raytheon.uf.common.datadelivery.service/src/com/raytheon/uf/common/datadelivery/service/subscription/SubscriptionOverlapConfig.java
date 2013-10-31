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
 * May 08, 2013   2000     djohnson    Initial creation
 * Sep 24, 2013   2386     dhladky     Abstracted for more types
 * Oct 21, 2013   2292     mpduff      Changes for point type implementation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public abstract class SubscriptionOverlapConfig {
    public final static transient int ONE_HUNDRED_PERCENT = 100;

    @XmlElement(required = true)
    protected int maxAllowedParameterDuplication;

    @XmlElement(required = true)
    protected int maxAllowedSpatialDuplication;

    @XmlElement(required = true)
    protected SubscriptionOverlapMatchStrategy matchStrategy;

    /**
     * Constructor.
     */
    public SubscriptionOverlapConfig() {

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
     * setup a default never overlapping config
     */
    public abstract SubscriptionOverlapConfig getNeverOverlaps();

}
