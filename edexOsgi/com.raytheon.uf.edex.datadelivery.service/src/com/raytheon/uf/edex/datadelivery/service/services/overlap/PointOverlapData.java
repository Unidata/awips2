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
package com.raytheon.uf.edex.datadelivery.service.services.overlap;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.service.subscription.PointSubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy;

/**
 * Point overlap data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2013   2292     mpduff      Initial creation
 * Feb 13, 2014   2386     bgonzale    Change pass comparisons to >= instead of only >.
 *                                     Change halfNumAttrs comp to a double for comparisons
 *                                     against half of uneven numbers of attributes.
 *                                     Renamed sub1 and sub2 to otherSub and sub to make
 *                                     it easier to see what is compared against.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointOverlapData<T extends PointTime, C extends Coverage> extends
        OverlapData<PointTime, Coverage> {
    /** Number of point attributes */
    private final int numberOfPointAttributes = 1;

    /** Time duplication percent */
    private int timeDuplication = -999;

    /** Time duplication pass flag */
    protected boolean timeDuplicationPass = false;

    /**
     * Constructor.
     * 
     * @param sub
     * @param otherSub
     * @param config
     */
    public PointOverlapData(Subscription sub, Subscription otherSub,
            SubscriptionOverlapConfig config) {
        super(sub, otherSub, config);
    }

    /**
     * Calculates the percent, 0-100, of how similar the time is from sub2 to
     * sub1.
     * 
     * @param otherSub
     * @param sub
     */
    private void calculateTimeDuplicationPercent(
            Subscription<PointTime, Coverage> sub,
            Subscription<PointTime, Coverage> otherSub) {
        PointTime ptimeOther = otherSub.getTime();
        PointTime ptime = sub.getTime();

        List<Integer> intervalListOther = new ArrayList<Integer>();
        intervalListOther.add(ptimeOther.getInterval());
        List<Integer> intervalList = new ArrayList<Integer>();
        intervalList.add(ptime.getInterval());

        timeDuplication = getDuplicationPercent(intervalListOther, intervalList);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void determineOverlapping() {
        super.determineOverlapping();
        PointSubscriptionOverlapConfig config = (PointSubscriptionOverlapConfig) this.config;
        calculateTimeDuplicationPercent(sub, otherSub);
        this.timeDuplicationPass = this.timeDuplication >= config
                .getMaxAllowedTimeDuplication();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isOverlapping() {
        determineOverlapping();
        boolean response = false;

        if (this.matchStrategy == SubscriptionOverlapMatchStrategy.MATCH_ALL) {
            response = this.parameterPass && this.spatialPass
                    && this.timeDuplicationPass;
        } else if (matchStrategy == SubscriptionOverlapMatchStrategy.MATCH_ANY) {
            response = this.parameterPass || this.spatialPass
                    || this.timeDuplicationPass;
        } else if (matchStrategy == SubscriptionOverlapMatchStrategy.AT_LEAST_HALF) {
            double halfNumAttrs = (numberOfPointAttributes + numberOfCommonAttributes) / 2.0;
            List<Boolean> toCheck = new ArrayList<Boolean>(3);
            toCheck.add(timeDuplicationPass);
            toCheck.add(spatialPass);
            toCheck.add(parameterPass);

            int exceeded = 0;
            for (boolean check : toCheck) {
                if (check) {
                    exceeded++;
                    if (exceeded >= halfNumAttrs) {
                        response = true;
                    }
                }
            }
        }

        return response;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isDuplicate() {
        determineOverlapping();
        return timeDuplication == ONE_HUNDRED_PERCENT
                && parameterDuplication == ONE_HUNDRED_PERCENT
                && spatialDuplication == ONE_HUNDRED_PERCENT;
    }
}
