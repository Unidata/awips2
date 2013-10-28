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

import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.service.subscription.GridSubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapMatchStrategy;

/**
 * Gridded overlap data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2013   2292     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 * @param <T>
 */

public class GridOverlapData<T extends GriddedTime, C extends GriddedCoverage>
        extends OverlapData<GriddedTime, GriddedCoverage> {
    /** Number of grid attributes */
    private final int numberOfGridAttributes = 2;

    /** Forecast hour duplication percent */
    private int fcstHrDuplication = -999;

    /** Cycle time duplication percetn */
    private int cycleDuplication = -999;

    /** Forecast hour pass flag */
    private boolean fcstHrPass = false;

    /** Cycle time pass flag */
    private boolean cyclePass = false;

    /**
     * Constructor.
     * 
     * @param sub1
     * @param sub2
     * @param config
     */
    public GridOverlapData(Subscription sub1, Subscription sub2,
            SubscriptionOverlapConfig config) {
        super(sub1, sub2, config);
    }

    /**
     * Calculates the percent, 0-100, of how many cycle hours from sub2 are
     * satisfied by sub1.
     * 
     * @param sub1
     * @param sub2
     */
    public void calculateCycleDuplicationPercent(
            Subscription<GriddedTime, GriddedCoverage> sub1,
            Subscription<GriddedTime, GriddedCoverage> sub2) {

        GriddedTime gtime1 = sub1.getTime();
        GriddedTime gtime2 = sub2.getTime();

        cycleDuplication = this.getDuplicationPercent(gtime1.getCycleTimes(),
                gtime2.getCycleTimes());
    }

    /**
     * Calculates the percent, 0-100, of how many forecast hours from sub2 are
     * satisfied by sub1.
     * 
     * @param sub1
     * @param sub2
     */
    public void calculateForecastHourDuplicationPercent(
            Subscription<GriddedTime, GriddedCoverage> sub1,
            Subscription<GriddedTime, GriddedCoverage> sub2) {
        GriddedTime gtime1 = sub1.getTime();
        GriddedTime gtime2 = sub2.getTime();

        fcstHrDuplication = getDuplicationPercent(
                gtime1.getSelectedTimeIndices(),
                gtime2.getSelectedTimeIndices());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void determineOverlapping() {
        super.determineOverlapping();
        calculateCycleDuplicationPercent(sub1, sub2);
        calculateForecastHourDuplicationPercent(sub1, sub2);
        GridSubscriptionOverlapConfig config = (GridSubscriptionOverlapConfig) this.config;

        fcstHrPass = fcstHrDuplication > config
                .getMaxAllowedForecastHourDuplication();

        cyclePass = cycleDuplication > config.getMaxAllowedCycleDuplication();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isOverlapping() {
        determineOverlapping();
        boolean response = false;

        if (matchStrategy == SubscriptionOverlapMatchStrategy.MATCH_ALL) {
            response = cyclePass && fcstHrPass && parameterPass && spatialPass;
        } else if (matchStrategy == SubscriptionOverlapMatchStrategy.MATCH_ANY) {
            response = cyclePass || fcstHrPass || parameterPass || spatialPass;
        } else if (matchStrategy == SubscriptionOverlapMatchStrategy.AT_LEAST_HALF) {
            int halfNumAttrs = (numberOfGridAttributes + numberOfCommonAttributes) / 2;
            List<Boolean> toCheck = new ArrayList<Boolean>();
            toCheck.add(cyclePass);
            toCheck.add(fcstHrPass);
            toCheck.add(parameterPass);
            toCheck.add(spatialPass);

            int exceeded = 0;
            for (boolean check : toCheck) {
                if (check) {
                    exceeded++;
                    if (exceeded >= halfNumAttrs) {
                        response = true;
                        break;
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
        return cycleDuplication == ONE_HUNDRED_PERCENT
                && fcstHrDuplication == ONE_HUNDRED_PERCENT
                && parameterDuplication == ONE_HUNDRED_PERCENT
                && spatialDuplication == ONE_HUNDRED_PERCENT;
    }
}
