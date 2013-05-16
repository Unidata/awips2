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

import javax.xml.bind.annotation.XmlEnum;

/**
 * Strategy for how to apply the rules in the subscription overlap
 * configuration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2013 2000       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlEnum
public enum SubscriptionOverlapMatchStrategy {
    AT_LEAST_HALF {
        @Override
        public boolean isOverlapping(SubscriptionOverlapConfig config,
                int parameterDuplicationPercent,
                int forecastHourDuplicationPercent,
                int cycleDuplicationPercent, int spatialDuplicationPercent) {
            final boolean exceedsAllowedParameterDuplication = parameterDuplicationPercent > config
                    .getMaxAllowedParameterDuplication();
            final boolean exceedsAllowedForecastHourDuplication = forecastHourDuplicationPercent > config
                    .getMaxAllowedForecastHourDuplication();
            final boolean exceedsAllowedCycleDuplication = cycleDuplicationPercent > config
                    .getMaxAllowedCycleDuplication();
            final boolean exceedsAllowedSpatialDuplication = spatialDuplicationPercent > config
                    .getMaxAllowedSpatialDuplication();

            boolean[] toCheck = new boolean[] {
                    exceedsAllowedParameterDuplication,
                    exceedsAllowedForecastHourDuplication,
                    exceedsAllowedCycleDuplication,
                    exceedsAllowedSpatialDuplication };
            final int numBooleans = toCheck.length;
            final int halfNumBooleans = numBooleans / 2;

            int exceeded = 0;
            for (boolean check : toCheck) {
                if (check) {
                    exceeded++;
                    if (exceeded >= halfNumBooleans) {
                        return true;
                    }
                }
            }
            return false;
        }

        @Override
        public String getDisplayString() {
            return "At Least Half";
        }
    },
    MATCH_ALL {
        @Override
        public boolean isOverlapping(SubscriptionOverlapConfig config,
                int parameterDuplicationPercent,
                int forecastHourDuplicationPercent,
                int cycleDuplicationPercent, int spatialDuplicationPercent) {

            final boolean exceedsAllowedParameterDuplication = parameterDuplicationPercent > config
                    .getMaxAllowedParameterDuplication();
            final boolean exceedsAllowedForecastHourDuplication = forecastHourDuplicationPercent > config
                    .getMaxAllowedForecastHourDuplication();
            final boolean exceedsAllowedCycleDuplication = cycleDuplicationPercent > config
                    .getMaxAllowedCycleDuplication();
            final boolean exceedsAllowedSpatialDuplication = spatialDuplicationPercent > config
                    .getMaxAllowedSpatialDuplication();

            return exceedsAllowedParameterDuplication
                    && exceedsAllowedForecastHourDuplication
                    && exceedsAllowedCycleDuplication
                    && exceedsAllowedSpatialDuplication;
        }

        @Override
        public String getDisplayString() {
            return "Match All";
        }
    },
    MATCH_ANY {
        @Override
        public boolean isOverlapping(SubscriptionOverlapConfig config,
                int parameterDuplicationPercent,
                int forecastHourDuplicationPercent,
                int cycleDuplicationPercent, int spatialDuplicationPercent) {

            final boolean exceedsAllowedParameterDuplication = parameterDuplicationPercent > config
                    .getMaxAllowedParameterDuplication();
            final boolean exceedsAllowedForecastHourDuplication = forecastHourDuplicationPercent > config
                    .getMaxAllowedForecastHourDuplication();
            final boolean exceedsAllowedCycleDuplication = cycleDuplicationPercent > config
                    .getMaxAllowedCycleDuplication();
            final boolean exceedsAllowedSpatialDuplication = spatialDuplicationPercent > config
                    .getMaxAllowedSpatialDuplication();

            return exceedsAllowedParameterDuplication
                    || exceedsAllowedForecastHourDuplication
                    || exceedsAllowedCycleDuplication
                    || exceedsAllowedSpatialDuplication;
        }

        @Override
        public String getDisplayString() {
            return "Match Any";
        }
    };

    /**
     * Check whether the given duplication percents indicate an overlapping
     * subscription.
     * 
     * @param config
     * @param parameterDuplicationPercent
     * @param forecastHourDuplicationPercent
     * @param cycleDuplicationPercent
     * @param spatialDuplicationPercent
     * @return true if the subscription should be considered overlapping
     */
    public abstract boolean isOverlapping(SubscriptionOverlapConfig config,
            int parameterDuplicationPercent,
            int forecastHourDuplicationPercent, int cycleDuplicationPercent,
            int spatialDuplicationPercent);

    /**
     * Get the display string.
     * 
     * @return the display string
     */
    public abstract String getDisplayString();
}
