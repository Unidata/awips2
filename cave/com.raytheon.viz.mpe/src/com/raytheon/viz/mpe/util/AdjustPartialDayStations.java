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
package com.raytheon.viz.mpe.util;

import java.util.Calendar;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Used to make necessary adjustments to the MPE data when there is not / should
 * not be a full day of data available based on the system time. The necessary
 * adjustments include: 1) any 6 hour period that has not yet occurred must have
 * a value of missing. 2) the 12z - 12z total must be equal to the sum of the
 * totals (when not missing) of every 6 hour period that has occurred. Ideally,
 * this would have been handled when the data was read. However, the data
 * reading and storage need significant updates so that they will utilize modern
 * Java.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2017 6148       bkowal      Initial creation
 * Jan 23, 2018 6547       bkowal      Allow the 12z-12z value to be set to "missing".
 *
 * </pre>
 *
 * @author bkowal
 */

public class AdjustPartialDayStations {

    private static final int INDEX_18_TO_00 = 1;

    private static final int INDEX_00_TO_06 = 2;

    private static final int INDEX_06_TO_12 = 3;

    private static final int INDEX_12_TO_12 = 4;

    private static final float MISSING_VALUE = -99f;

    public static void adjust(final int day, final int numStations) {
        if (DailyQcUtils.pdata[day].data_time == null) {
            return;
        }

        final int hourOfDay = TimeUtil.newCalendar(TimeUtil.GMT_TIME_ZONE)
                .get(Calendar.HOUR_OF_DAY);

        for (int s = 0; s < numStations; s++) {
            if (hourOfDay > 18) {
                DailyQcUtils.pdata[day].stn[s].frain[INDEX_18_TO_00].data = MISSING_VALUE;
                DailyQcUtils.pdata[day].stn[s].frain[INDEX_00_TO_06].data = MISSING_VALUE;
                DailyQcUtils.pdata[day].stn[s].frain[INDEX_06_TO_12].data = MISSING_VALUE;
            } else if (hourOfDay > 6) {
                DailyQcUtils.pdata[day].stn[s].frain[INDEX_06_TO_12].data = MISSING_VALUE;
            } else if (hourOfDay > 0) {
                DailyQcUtils.pdata[day].stn[s].frain[INDEX_00_TO_06].data = MISSING_VALUE;
                DailyQcUtils.pdata[day].stn[s].frain[INDEX_06_TO_12].data = MISSING_VALUE;
            }

            if (DailyQcUtils.pdata[day].stn[s].frain[INDEX_12_TO_12].data < 0) {
                continue;
            }
            /*
             * Adjust the 12z to 12z total as needed.
             */
            float total12to12 = 0.0f;
            for (int i = 0; i <= INDEX_06_TO_12; i++) {
                if (DailyQcUtils.pdata[day].stn[s].frain[i].data < 0) {
                    continue;
                }
                total12to12 += DailyQcUtils.pdata[day].stn[s].frain[i].data;
            }
            DailyQcUtils.pdata[day].stn[s].frain[INDEX_12_TO_12].data = total12to12;
        }
    }
}