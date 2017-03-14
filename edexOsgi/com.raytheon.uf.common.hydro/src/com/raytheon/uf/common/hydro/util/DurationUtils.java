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
package com.raytheon.uf.common.hydro.util;

import java.util.Calendar;

import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Duration Utilities. Duration codes specify units and number as follows: 7XXX
 * seconds, OXXX minutes, 1XXX hours, 2XXX days, 3XXX months, 4XXX years, 5000
 * specified as default 5001 seasonal, 5002 entire period of record, 5003
 * variable period - duration specified separately, 5004 time period beginning
 * at 7AM local time prior to the observation and ending at the observation
 * time, 5005 unknown, 6XXX months-end of month. Where XXX is the number of
 * units. For example, eight days would be coded as 2008,instantaneous as 0. A
 * special case exists when the duration is specified as 2015. . . a) If
 * translated from SHEF code V with DVD15, then any 15 day duration is implied
 * b) If translated from SHEF code N, then duration is first day of the month to
 * and ending on the 15th day of the same month.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2009            mpduff      Initial creation
 * May 26, 2016 5571       skorolev    Relocated to a common plugin 
 *                                     for use in both EDEX/CAVE. Cleanup.
 * </pre>
 * 
 * @author mpduff
 */

public class DurationUtils {

    /**
     * Convert the encoded duration into a count of milliseconds.
     * 
     * @param dur
     *            The duration value
     * @param obstime
     *            The obstime in milliseconds
     * @return The number of milliseconds represented by the encoded duration
     */
    public static long durationToMilliseconds(int dur, long obstime) {
        long retVal = 0;
        long local7Time;
        int local7amWindow;
        long diff;

        /* 7XXX values are in seconds */
        if ((dur >= 7000) && (dur < 8000)) {
            retVal = (dur - 7000) * TimeUtil.MILLIS_PER_SECOND;
        }
        /* OXXX values are in minutes */
        else if (dur < 1000) {
            retVal = dur * TimeUtil.MILLIS_PER_MINUTE;
        }
        /* 1XXX values are in hours */
        else if ((dur >= 1000) && (dur < 2000)) {
            retVal = (dur - 1000) * TimeUtil.MILLIS_PER_HOUR;
        }
        /* 2XXX days values are in days */
        else if ((dur >= 2000) && (dur < 3000)) {
            retVal = (dur - 2000) * TimeUtil.MILLIS_PER_DAY;
        }
        /* 3XXX values are in months */
        else if ((dur >= 3000) && (dur < 4000)) {
            int m = dur - 3000;
            if (m > 12) {
                retVal = 0;
            }
            Calendar c = TimeUtil.newGmtCalendar();
            Calendar newc = TimeUtil.newGmtCalendar();
            // add m month to obstime
            newc.add(Calendar.MONTH, m);
            retVal = newc.getTimeInMillis() - c.getTimeInMillis();
        }
        /* 4XXX values are in years */
        else if ((dur >= 4000) && (dur < 5000)) {
            retVal = (dur - 4000) * TimeUtil.MILLIS_PER_YEAR;
        }
        /*
         * 500x values are specially defined. only consider 5004; its duration
         * is defined as the period ending at the observation time and beginning
         * at the most recent 7AM local find the time_t for 7AM local. use the
         * data time. subtract a day from it so the modulo approach can be used
         * to determine the duration. this is needed to easily handle the cases
         * where the ending time is between midnight and 7 AM.
         */
        else if (dur == 5004) {

            /* Get the search window around local 7 AM. */
            local7amWindow = AppsDefaults.getInstance().getInt(
                    CommonHydroConstants.AppsDefaults.PPP_PPD_LOCAL_7AM_WINDOW,
                    0);
            Calendar obstm = TimeUtil.newGmtCalendar();
            obstm.setTimeInMillis(obstime);
            obstm.set(Calendar.HOUR_OF_DAY, 07);
            TimeUtil.minCalendarFields(obstm, Calendar.MINUTE, Calendar.SECOND,
                    Calendar.MILLISECOND);
            local7Time = obstm.getTimeInMillis();

            diff = Math.abs(obstime - local7Time);

            if (diff < (TimeUtil.MILLIS_PER_HOUR * local7amWindow)) {
                /*
                 * The difference between the 5004 report obstime and local 7am
                 * is within the window within which the report is considered to
                 * have a duration of 24 hours ending at local 7am.
                 */
                retVal = TimeUtil.MILLIS_PER_DAY;
            } else if (obstime < local7Time) {
                /*
                 * The 5004 report obstime is smaller than local 7am and outside
                 * of the 7am window. The duration of this report is the
                 * difference between its time and 7am local of the previous
                 * day.
                 */
                retVal = TimeUtil.SECONDS_PER_DAY - diff;
            } else {
                /*
                 * The 5004 report obstime is greater than local 7am and outside
                 * of the 7am window. The duration of this report is the
                 * difference between its time and 7am local.
                 */
                retVal = diff;
            }
        } else {
            /* then there are some undefined values */
            retVal = 0;
        }
        return retVal;
    }

    /**
     * @param endTime
     * @param obstime
     * @return
     */
    public static boolean isNear7am(long endTime, long obstime) {
        int local7amWindow = AppsDefaults.getInstance().getInt(
                CommonHydroConstants.AppsDefaults.PPP_PPD_LOCAL_7AM_WINDOW, 0);
        Calendar obstm = TimeUtil.newGmtCalendar();
        obstm.setTimeInMillis(obstime);
        obstm.set(Calendar.HOUR_OF_DAY, 07);
        TimeUtil.minCalendarFields(obstm, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);

        long diff = Math.abs(endTime - obstm.getTimeInMillis());

        if (diff <= (local7amWindow * TimeUtil.MILLIS_PER_DAY)) {
            return true;
        } else {
            return false;
        }
    }
}
