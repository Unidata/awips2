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
package com.raytheon.uf.viz.datadelivery.utils;

import java.util.List;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Data Set Frequency Enumeration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 08, 2013 1420       mpduff       Initial creation.
 * Jan 22, 2013 1519       djohnson     Correct the non-hourly default latency to match requirements.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public enum DataSetFrequency {
    HOURLY(DataDeliveryUtils.HOURLY_DATASET_LATENCY_IN_MINUTES), SIX_HOURLY(
            DataDeliveryUtils.NON_HOURLY_DATASET_LATENCY_IN_MINUTES), TWELVE_HOURLY(
            DataDeliveryUtils.NON_HOURLY_DATASET_LATENCY_IN_MINUTES), DAILY(
            DataDeliveryUtils.NON_HOURLY_DATASET_LATENCY_IN_MINUTES);
    
    private int defaultLatency;

    private DataSetFrequency(int defaultLatency) {
        this.defaultLatency = defaultLatency;
    }

    public static DataSetFrequency fromCycleTimes(List<Integer> cycleTimes) {
        if (cycleTimes.size() > 1) {
            final int hoursBetweenCycles = cycleTimes.get(1) - cycleTimes.get(0);
            if (hoursBetweenCycles == 1) {
                return DataSetFrequency.HOURLY;
            } else if (hoursBetweenCycles == TimeUtil.HOURS_PER_QUARTER_DAY) {
                return DataSetFrequency.SIX_HOURLY;
            } else if (hoursBetweenCycles == TimeUtil.HOURS_PER_HALF_DAY) {
                return DataSetFrequency.TWELVE_HOURLY;
            } else if (hoursBetweenCycles == TimeUtil.HOURS_PER_DAY) {
                return DataSetFrequency.DAILY;
            }
        }
        return DataSetFrequency.DAILY;
    }

    public int getDefaultLatency() {
        return this.defaultLatency;
    }
}
