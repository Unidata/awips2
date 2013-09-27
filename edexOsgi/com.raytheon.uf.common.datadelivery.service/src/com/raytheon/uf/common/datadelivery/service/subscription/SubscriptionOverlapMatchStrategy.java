package com.raytheon.uf.common.datadelivery.service.subscription;

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
 * Sept 24, 2013 2386      dhladky      Added impl for other types besides grid
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
            
            boolean[] toCheck = null;

            final boolean exceedsAllowedParameterDuplication = parameterDuplicationPercent > config
                    .getMaxAllowedParameterDuplication();
            final boolean exceedsAllowedSpatialDuplication = spatialDuplicationPercent > config
                    .getMaxAllowedSpatialDuplication();

            // gridded products
            if (config instanceof GridSubscriptionOverlapConfig) {

                GridSubscriptionOverlapConfig gconfig = (GridSubscriptionOverlapConfig) config;

                final boolean exceedsAllowedForecastHourDuplication = forecastHourDuplicationPercent > gconfig
                        .getMaxAllowedForecastHourDuplication();
                final boolean exceedsAllowedCycleDuplication = cycleDuplicationPercent > gconfig
                        .getMaxAllowedCycleDuplication();

                toCheck = new boolean[] { exceedsAllowedParameterDuplication,
                        exceedsAllowedForecastHourDuplication,
                        exceedsAllowedCycleDuplication,
                        exceedsAllowedSpatialDuplication };
            } 
             // point products
            else if (config instanceof PointSubscriptionOverlapConfig) {
                PointSubscriptionOverlapConfig pconfig = (PointSubscriptionOverlapConfig) config;

                final boolean exceedsAllowedTimeDuplication = forecastHourDuplicationPercent > pconfig
                        .getMaxAllowedTimeDuplication();

                toCheck = new boolean[] { exceedsAllowedParameterDuplication,
                        exceedsAllowedTimeDuplication,
                        exceedsAllowedSpatialDuplication };
            } else {
                throw new IllegalArgumentException("Data type "+config.getClass()+" has no implementation");
            }

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

            boolean response = false;
            
            final boolean exceedsAllowedParameterDuplication = parameterDuplicationPercent > config
                    .getMaxAllowedParameterDuplication();

            final boolean exceedsAllowedSpatialDuplication = spatialDuplicationPercent > config
                    .getMaxAllowedSpatialDuplication();

            // gridded products
            if (config instanceof GridSubscriptionOverlapConfig) {

                GridSubscriptionOverlapConfig gconfig = (GridSubscriptionOverlapConfig) config;

                final boolean exceedsAllowedForecastHourDuplication = forecastHourDuplicationPercent > gconfig
                        .getMaxAllowedForecastHourDuplication();
                final boolean exceedsAllowedCycleDuplication = cycleDuplicationPercent > gconfig
                        .getMaxAllowedCycleDuplication();

                response = exceedsAllowedParameterDuplication
                        && exceedsAllowedForecastHourDuplication
                        && exceedsAllowedCycleDuplication
                        && exceedsAllowedSpatialDuplication;

            // point products
            } else if (config instanceof PointSubscriptionOverlapConfig) {
                PointSubscriptionOverlapConfig pconfig = (PointSubscriptionOverlapConfig) config;

                final boolean exceedsAllowedTimeDuplication = forecastHourDuplicationPercent > pconfig
                        .getMaxAllowedTimeDuplication();
               
                response = exceedsAllowedParameterDuplication
                        && exceedsAllowedTimeDuplication
                        && exceedsAllowedSpatialDuplication;
            } else {
                throw new IllegalArgumentException("Data type "+config.getClass()+" has no implementation");
            }

            
            return response;
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
            
            boolean response = false;

            final boolean exceedsAllowedParameterDuplication = parameterDuplicationPercent > config
                    .getMaxAllowedParameterDuplication();

            final boolean exceedsAllowedSpatialDuplication = spatialDuplicationPercent > config
                    .getMaxAllowedSpatialDuplication();

            // gridded products
            if (config instanceof GridSubscriptionOverlapConfig) {
                GridSubscriptionOverlapConfig gconfig = (GridSubscriptionOverlapConfig) config;

                final boolean exceedsAllowedForecastHourDuplication = forecastHourDuplicationPercent > gconfig
                        .getMaxAllowedForecastHourDuplication();
                final boolean exceedsAllowedCycleDuplication = cycleDuplicationPercent > gconfig
                        .getMaxAllowedCycleDuplication();

                response = exceedsAllowedParameterDuplication
                        || exceedsAllowedForecastHourDuplication
                        || exceedsAllowedCycleDuplication
                        || exceedsAllowedSpatialDuplication;
            }
            // point products
            else if (config instanceof PointSubscriptionOverlapConfig) {
                PointSubscriptionOverlapConfig pconfig = (PointSubscriptionOverlapConfig) config;

                final boolean exceedsAllowedTimeDuplication = forecastHourDuplicationPercent > pconfig
                        .getMaxAllowedTimeDuplication();

                response = exceedsAllowedParameterDuplication
                        || exceedsAllowedTimeDuplication
                        || exceedsAllowedSpatialDuplication;
            } else {
                throw new IllegalArgumentException("Data type "+config.getClass()+" has no implementation");
            }

            return response;
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
     * @param forecastHourDuplicationPercent or timeDuplicatePercent
     * @param cycleDuplicationPercent or notUsed
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

