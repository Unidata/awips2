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
package com.raytheon.uf.common.wmo;

import java.text.ParseException;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.DataFormatException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Static utilities for parsing dates/times from WMO products
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2014 2536       bclement    Initial creation, moved from TimeTools
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WMOTimeParser {
    
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(WMOTimeParser.class);

    /** Environment variable to indicate archived files. */
    private static final String ALLOW_ARCHIVE_ENV = "ALLOW_ARCHIVE_DATA";

    /**
     * Time stamp that includes the receipt time format at the end of the file
     * name: .YYYYMMDD or .YYYYMMDDHH
     */
    private static final Pattern FILE_TIMESTAMP = Pattern
            .compile("(.*\\.)(\\d{8}|\\d{10})$");

    public static final Pattern WMO_TIMESTAMP = Pattern
            .compile("([0-3][0-9])(\\d{2})(\\d{2})[Zz]?");

    /**
     * Get a calendar that expresses the current system time based on specified
     * date information or the current service time if not allowing archive.
     * 
     * Note that the month argument should be 1 based (e.g. January = 1) and the
     * returned calendar will have the month be 0 based (e.g. January = 0).
     * 
     * @param year
     *            Year to set.
     * @param month
     * @param day
     * @param hour
     * @param minute
     * @return The current time as a GMT Calendar.
     */
    private static final Calendar getSystemCalendar(int year, int month,
            int day, int hour, int minute) {
        Calendar retCal = TimeUtil.newGmtCalendar();
        if (allowArchive()) {
            if (isValidDate(year, month, day)) {
                if (hour != -1) {
                    if (minute != -1) {
                        retCal.set(Calendar.YEAR, year);
                        retCal.set(Calendar.MONTH, month - 1);
                        retCal.set(Calendar.DAY_OF_MONTH, day);
                        retCal.set(Calendar.HOUR_OF_DAY, hour);
                        retCal.set(Calendar.MINUTE, minute);
                        retCal.set(Calendar.SECOND, 0);
                        retCal.set(Calendar.MILLISECOND, 0);
                    }
                }
            }
        }
        return retCal;
    }

    /**
     * Get Calendar with the time based on the timestamp at the end of the
     * fileName.
     * 
     * @param fileName
     * @return calendar
     */
    public static final Calendar getSystemCalendar(String fileName) {
        int year = -1;
        int month = -1;
        int day = -1;
        int hour = -1;

        if (fileName != null) {
            Matcher matcher = FILE_TIMESTAMP.matcher(fileName);
            if (matcher.find()) {
                String yyyymmdd = matcher.group(2);
                try {
                    year = Integer.parseInt(yyyymmdd.substring(0, 4));
                    month = Integer.parseInt(yyyymmdd.substring(4, 6));
                    day = Integer.parseInt(yyyymmdd.substring(6, 8));
                    if (yyyymmdd.length() < 10) {
                        hour = 0;
                    } else {
                        hour = Integer.parseInt(yyyymmdd.substring(8, 10));
                    }
                } catch (NumberFormatException nfe) {
                    year = -1;
                    month = -1;
                    day = -1;
                }
            }
        }
        return getSystemCalendar(year, month, day, hour, 0);
    }

    /**
     * Get the timestamp in the file name.
     * 
     * @param fileName
     * @return timestamp if it matches FILE_TIMESTAMP otherwise null
     */
    public static final String getTimestamp(String fileName) {
        String timestamp = null;
        Matcher matcher = FILE_TIMESTAMP.matcher(fileName);
        if (matcher.find()) {
            timestamp = matcher.group(2);
        }
        return timestamp;
    }

    /**
     * Converts a ddhhmm time group to a Calendar. Adjusts the calendar as
     * follows: Any time group with a day (dd) in the future is set back one
     * month.
     * 
     * @param wmoDateStamp
     *            the time to convert
     * 
     * @return the converted time
     * 
     * @throws DataFormatException
     *             if an error occurs
     */
    public static final Calendar findCurrentTime(String wmoDateStamp,
            String fileName) throws DataFormatException {
        Calendar refCal = getSystemCalendar(fileName);
        try {
            Matcher matcher = WMO_TIMESTAMP.matcher(wmoDateStamp);
            if (matcher.matches()) {
                int iDay = Integer.parseInt(matcher.group(1));
                int iHour = Integer.parseInt(matcher.group(2));
                int iMinute = Integer.parseInt(matcher.group(3));

                refCal = adjustDayHourMinute(refCal, iDay, iHour, iMinute);
            } else {
                throw new ParseException(
                        "Invalid format - time does not match "
                                + WMO_TIMESTAMP.pattern(), 0);
            }
        } catch (Exception e) {
            throw new DataFormatException("Unable to find current time for "
                    + wmoDateStamp + ", exception was " + e.toString());
        }
        return refCal;
    }

    /**
     * Convert a string in ddhhmm format to a standard {@link Calendar} format
     * where ddhhmm is the GMT format while the standard time is in Calendar
     * format with Year and Month information. Usage: ddhhmm is the issue time
     * whereas utcTime can be the MDN time. The former comes "after" the latter.
     * 
     * @parm ddhhmm day-hour-minute in GMT
     * @parm local Time UTC time in Calendar
     */
    public static final Calendar findDataTime(String ddhhmm, String fileName) {
        Calendar issueTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        try {
            issueTime = findCurrentTime(ddhhmm, fileName);
        } catch (DataFormatException e) {
            logger.info(" Error in processing MND time; return current time ");
            issueTime = null;
        }
        return issueTime;
    }

    /**
     * Adjusts the calendar from the current date to the specified date. If the
     * specified date is later than the current date, the calendar is "backed
     * up" one month. In addition, the second and millisecond fields are set to
     * zero.
     * 
     * @param cal
     *            the calendar to adjust
     * @param day
     *            the new day of month
     * @param hour
     *            the new hour of day
     * @param minute
     *            the new minute of the hour
     */
    private static Calendar adjustDayHourMinute(Calendar cal, int wmoDay,
            int wmoHour, int wmoMinute) {
        if (cal != null) {

            int cDay = cal.get(Calendar.DAY_OF_MONTH);

            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);

            // Range check hour/minute first. Have to wait for
            // checking the day
            if (isValidHour(wmoHour) && (isValidMinSec(wmoMinute))) {
                Calendar lastMonth = (Calendar) cal.clone();
                lastMonth.set(Calendar.DAY_OF_MONTH, 1);
                lastMonth.add(Calendar.MONTH, -1);

                // Get the maximum day of the current month from the reference
                // calendar
                int maxDayThisMonth = cal
                        .getActualMaximum(Calendar.DAY_OF_MONTH);
                // Set the day to one so all add/subtracts work correctly
                cal.set(Calendar.DAY_OF_MONTH, 1);
                cal.set(Calendar.HOUR_OF_DAY, wmoHour);
                cal.set(Calendar.MINUTE, wmoMinute);
                if (wmoDay == 1) {
                    // the wmoDay is 1
                    // and the reference calendar is the last
                    // day of the month
                    if (cDay == maxDayThisMonth) {
                        // This is potentially next month's data received early
                        // Allow three hours into the next day
                        if (wmoHour < 3) {
                            // Advance to the next month
                            cal.add(Calendar.MONTH, 1);
                            // and set the hour, minute
                        }
                    }
                } else if (wmoDay > cDay) {
                    // Is the wmoDay valid for this month?
                    if (wmoDay <= maxDayThisMonth) {
                        // First allow up to 3 hours into the next day
                        if ((cDay + 1) == wmoDay) {
                            // This is potentially next month's data received
                            // early. Allow three hours into the next day
                            if (wmoHour > 2) {
                                // Back up a month
                                cal.add(Calendar.MONTH, -1);
                            }
                        } else {
                            // Back up a month
                            cal.add(Calendar.MONTH, -1);
                            int mDay = cal
                                    .getActualMaximum(Calendar.DAY_OF_MONTH);
                            if (mDay < wmoDay) {
                                cal.add(Calendar.MONTH, -1);
                            }
                        }
                    } else {
                        // The wmoDay is greater than the maximum number
                        // of days for the reference month. We can't back
                        // up one month, but can always back up two months.
                        cal.add(Calendar.MONTH, -2);
                    }
                }
                cal.set(Calendar.DAY_OF_MONTH, wmoDay);
            } else {
                // bad
                cal = null;
            }
        }
        return cal;
    }

    /**
     * Is a specified date valid? This method checks an entire year, month, day
     * timestamp.
     * 
     * @param year
     *            The year to check.
     * @param month
     *            Numeric value of the month.
     * @param day
     *            Is the month valid?
     * @return Is year, month, day timestamp valid.
     */
    private static final boolean isValidDate(int year, int month, int day) {
        boolean validDay = false;
        if (day > -1) {
            if (isValidYear(year)) {
                if (isValidMonth(month)) {
                    Calendar c = TimeUtil.newGmtCalendar(year, month, 1);
                    int lastDay = c.getActualMaximum(Calendar.DAY_OF_MONTH) + 1;

                    validDay = (day < lastDay);
                }
            }
        }
        return validDay;
    }

    /**
     * Is the year valid. This method supposes any positive year value as valid.
     * 
     * @param year
     *            The year to check.
     * @return Is the year valid?
     */
    private static final boolean isValidYear(int year) {
        return (year >= 0);
    }

    /**
     * The the specified month of the year valid.
     * 
     * @param month
     *            Numeric value of the month.
     * @return Is the month valid?
     */
    private static final boolean isValidMonth(int month) {
        return ((month > 0) && (month <= 12));
    }

    /**
     * Is the specified hour of the day valid? Range 0..23 inclusive.
     * 
     * @param hour
     *            The hour to check.
     * @return Is the hour valid?
     */
    private static final boolean isValidHour(int hour) {
        return ((hour > -1) && (hour < TimeUtil.HOURS_PER_DAY));
    }

    /**
     * Is the specified minute/second valid? Range 0..59 inclusive.
     * 
     * @param hour
     *            The minute/second to check.
     * @return Is the minute/second valid?
     */
    private static final boolean isValidMinSec(int value) {
        return ((value > -1) && (value < TimeUtil.MINUTES_PER_HOUR));
    }

    /**
     * Check to see if archive files are allowed.
     * 
     * @return true when archive files are allowed
     */
    public static boolean allowArchive() {
        // This doesn't pick up the environment variable.
        // return Boolean.getBoolean(ALLOW_ARCHIVE_ENV);
        return "true".equalsIgnoreCase(System.getenv().get(ALLOW_ARCHIVE_ENV));
    }
}
