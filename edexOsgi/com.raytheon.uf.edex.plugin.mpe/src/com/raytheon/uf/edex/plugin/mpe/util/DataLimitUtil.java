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
package com.raytheon.uf.edex.plugin.mpe.util;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.rocchecker.RocCheckerFailedException;

/**
 * Utilities for interacting with the MPE/Hydro data limits.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 9, 2018  7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class DataLimitUtil {

    private static final String DATA_LIMIT_DATE_FORMAT = "MM-dd";

    private static final ThreadLocal<SimpleDateFormat> dataLimitDF = TimeUtil
            .buildThreadLocalSimpleDateFormat(DATA_LIMIT_DATE_FORMAT,
                    TimeUtil.GMT_TIME_ZONE);

    private DataLimitUtil() {
    }

    /**
     * Determines if the specified observation date/time is within the data
     * limits formatted (MM-dd) date range.
     * 
     * @param obstime
     *            the specified observation date/time
     * @param monthdaystart
     *            the data limits formatted start of the date range
     * @param monthdayend
     *            the data limits formatted end of the date range
     * @return {@code true} when the specified observation date/time is within
     *         the range; {code false}, otherwise.
     * @throws Exception
     */
    public static boolean withinDataLimitTimeRange(final Date obstime,
            final String monthdaystart, final String monthdayend)
                    throws Exception {
        Date parsedLimitStart = null;
        Date parsedLimitEnd = null;
        try {
            parsedLimitStart = dataLimitDF.get().parse(monthdaystart);
        } catch (ParseException e) {
            throw new RocCheckerFailedException(
                    "Failed to parse data limit date: " + monthdaystart + ".");
        }

        try {
            parsedLimitEnd = dataLimitDF.get().parse(monthdayend);
        } catch (ParseException e) {
            throw new RocCheckerFailedException(
                    "Failed to parse data limit date: " + monthdayend + ".");
        }

        /*
         * Most of {@link Date} is deprecated; so, it is easier to work with
         * {@link Calendar}.
         */
        Calendar limitStart = TimeUtil.newGmtCalendar(parsedLimitStart);
        Calendar limitEnd = TimeUtil.newGmtCalendar(parsedLimitEnd);
        Calendar obsCompareTime = TimeUtil.newGmtCalendar(obstime);

        /*
         * Only the month/day are important for this comparison. So, set all
         * other fields to the minimum value.
         */
        TimeUtil.minCalendarFields(limitStart, Calendar.YEAR,
                Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);
        TimeUtil.minCalendarFields(limitEnd, Calendar.YEAR,
                Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);
        TimeUtil.minCalendarFields(obsCompareTime, Calendar.YEAR,
                Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);

        /*
         * The specified obs time must be within (not before, not after) the
         * limit start date and end date.
         */
        if (obsCompareTime.before(limitStart)
                || obsCompareTime.after(limitEnd)) {
            return false;
        }

        return true;
    }
}