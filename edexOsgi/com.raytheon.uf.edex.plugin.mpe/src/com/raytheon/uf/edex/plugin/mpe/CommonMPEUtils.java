package com.raytheon.uf.edex.plugin.mpe;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Common utilities for MPE
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2016 4623       skorolev    Initial creation
 * 
 * 
 * </pre>
 * 
 * @author skorolev
 */
public class CommonMPEUtils {

    private static final String DATA_LIMIT_DATE_FORMAT = "MM-dd";

    private static final ThreadLocal<SimpleDateFormat> dataLimitDF = TimeUtil
            .buildThreadLocalSimpleDateFormat(DATA_LIMIT_DATE_FORMAT,
                    TimeUtil.GMT_TIME_ZONE);

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
            throw new MpeException("Failed to parse start date: "
                    + monthdaystart + ".");
        }

        try {
            parsedLimitEnd = dataLimitDF.get().parse(monthdayend);
        } catch (ParseException e) {
            throw new MpeException("Failed to parse end date: "
                    + monthdayend + ".");
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
        if (obsCompareTime.before(limitStart) || obsCompareTime.after(limitEnd)) {
            return false;
        }

        return true;
    }

}
