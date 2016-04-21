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
package com.raytheon.viz.hydrocommon.util;

import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DurationUtils {
    private static int first = 1;

    private static int local_7am_window = 3;

    /**
     * Convert the encoded duration into a count of milliseconds.
     * 
     * @param dur 
     *      The duration value
     * @param obstime
     *      The obstime in milliseconds
     * @return
     *      The number of milliseconds represented by the 
     *      encoded duration
     */
    public static long durationToSeconds(int dur, long obstime) {
        long retVal = 0;
        long local7Time;
        int local7amWindow;
        long diff;

        /* 000x values are in minutes */
        if (dur < 1000) {
            retVal = dur * HydroConstants.MILLIS_PER_MINUTE;
        } else if ((dur >= 1000) && (dur < 2000)) {
            /* 100x values are in hours */
            retVal = (dur % 1000) * HydroConstants.MILLIS_PER_HOUR;
        } else if ((dur >= 2000) && (dur < 3000)) {
            /* 200x values are in days */
            retVal = (dur % 2000) * HydroConstants.MILLIS_PER_DAY;
        } else if ((dur >= 3000) && (dur < 4000)) {
            /* 300x values are in months */
            long dailyval = (dur % 3000) * HydroConstants.MILLIS_PER_DAY;
            Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            c.setTimeInMillis(obstime);
            int days = c.getActualMaximum(Calendar.DAY_OF_MONTH);
            retVal = dailyval * days;
        } else if ((dur >= 4000) && (dur < 5000)) {
            /*
             * 400x values are in years. doesn't handle this, need to know the
             * number of days in the year to do this, and therefore need to know
             * the year
             */
            retVal = 0;
        } else if (dur == 5004) {
            /*
             * 500x values are specially defined. only consider 5004; its
             * duration is defined as the period ending at the observation time
             * and beginning at the most recent 7AM local
             */
            /*
             * find the time_t for 7AM local. use the data time. subtract a day
             * from it so the modulo approach can be used to determine the
             * duration. this is needed to easily handle the cases where the
             * ending time is between midnight and 7 AM.
             */

            /* Get the search window around local 7 AM. */
            local7amWindow = DurationUtils.get_local_7am_search_window();
            Calendar obstm = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            obstm.setTimeInMillis(obstime);
            obstm.set(Calendar.HOUR_OF_DAY, 07);
            obstm.set(Calendar.MINUTE, 0);
            obstm.set(Calendar.SECOND, 0);
            local7Time = obstm.getTimeInMillis();

            diff = Math.abs(obstime - local7Time);

            if (diff < (HydroConstants.MILLIS_PER_HOUR * local7amWindow)) {
                /*
                 * The difference between the 5004 report obstime and local 7am
                 * is within the window within which the report is considered to
                 * have a duration of 24 hours ending at local 7am.
                 */
                retVal = HydroConstants.MILLIS_PER_DAY;
            } else if (obstime < local7Time) {
                /*
                 * The 5004 report obstime is smaller than local 7am and outside
                 * of the 7am window. The duration of this report is the
                 * difference between its time and 7am local of the previous
                 * day.
                 */
                retVal = HydroConstants.SECONDS_PER_DAY - diff;
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

    public static int get_local_7am_search_window() {
        String reply = "";
        final String token_name = "ppp_ppd_local_7am_window";

        if (first == 1) {
            first = 0;
            reply = AppsDefaults.getInstance().getToken(token_name);
            if (reply.length() > 0) {
                local_7am_window = Integer.parseInt(reply);
            }
        }
        return local_7am_window;
    }
    
    public static boolean isNear7am(long endTime, long obstime) {
        int local7amWindow = DurationUtils.get_local_7am_search_window();
        Calendar obstm = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        obstm.setTimeInMillis(obstime);
        obstm.set(Calendar.HOUR_OF_DAY, 07);
        obstm.set(Calendar.MINUTE, 0);
        obstm.set(Calendar.SECOND, 0);
        
        long diff = Math.abs(endTime - obstm.getTimeInMillis());

        if (diff <= (local7amWindow * HydroConstants.MILLIS_PER_DAY)) {
            return true;
        } else {
            return false;
        }
    }

}
