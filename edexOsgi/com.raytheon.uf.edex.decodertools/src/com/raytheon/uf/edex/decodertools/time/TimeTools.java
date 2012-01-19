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
package com.raytheon.uf.edex.decodertools.time;

import java.text.ParseException;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.DataFormatException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;

/**
 * TimeTools provides a set of mid level Calendar manipulation methods. The
 * rollBy"X" methods allow changing the date of a calendar by specifying an
 * increment/decrement hours, days, etc. This is useful for some decoded data
 * types that specify the number of "X" units from a given base date/time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070727            411 jkorman     Initial Development
 * 20070903            379 jkorman     Added newCalendar from milliseconds.
 * 20070925            391 jkorman     Added copyToNearestHour method.
 * 20071019            391 jkorman     Added getSystemCalendar and TimeService.
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public class TimeTools {

    public static final int SECONDS_HOUR = 3600;

    public static final int SECONDS_DAY = 24 * SECONDS_HOUR;

    public static final long MILLIS_HOUR = 1000L * SECONDS_HOUR;

    public static final long MILLIS_DAY = MILLIS_HOUR * 24L;

    public static final String ZULU_TIMEZONE = "Zulu";

    private static ITimeService timeService = null;

    private static final Log logger = LogFactory.getLog(TimeTools.class);

    /**
     * Get a calendar that expresses the current system time. If an ITimeService
     * provider is registered, the time is retrieved from the service.
     * 
     * @return The current time as a GMT Calendar.
     */
    public static Calendar getSystemCalendar() {
        Calendar retCal = null;
        if (timeService != null) {
            retCal = timeService.getCalendar();
        } else {
            retCal = Calendar.getInstance(TimeZone.getTimeZone(ZULU_TIMEZONE));
        }
        return retCal;
    }

    /**
     * Get a calendar that expresses the current system time. With the month day
     * and year of the file extension.
     * 
     * @return The current time as a GMT Calendar.
     */
    public static Calendar getSystemCalendar(int year, int month, int day) {
        Calendar retCal = getSystemCalendar();
        String allow = System.getenv("ALLOW_ARCHIVE_DATA");
        if ("true".equalsIgnoreCase(allow)) {
            if (year != -1) {
                retCal.set(Calendar.YEAR, year);
            }
            if (month != -1) {
                retCal.set(Calendar.MONTH, month - 1);
            }
            if (day != -1) {
                retCal.set(Calendar.DATE, day);
            }
        }
        return retCal;
    }

    public static Calendar getSystemCalendar(String fileName) {
        int year = -1;
        int month = -1;
        int day = -1;
        if (fileName != null && fileName.matches(".*\\.\\d{8}$")) {
            Pattern pattern = Pattern.compile("(.*\\.)(\\d{8}$)");
            Matcher matcher = pattern.matcher(fileName);
            matcher.find();
            String yyyymmdd = matcher.group(2);
            year = Integer.parseInt(yyyymmdd.substring(0, 4));
            month = Integer.parseInt(yyyymmdd.substring(4, 6));
            day = Integer.parseInt(yyyymmdd.substring(6, 8));

        }
        return getSystemCalendar(year, month, day);
    }

    /**
     * Converts a ddhhmm time group to a Calendar. Adjusts the calendar as
     * follows: Any time group with a day (dd) in the future is set back one
     * month.
     * 
     * @param baseTime
     *            the time to convert
     * 
     * @return the converted time
     * 
     * @throws DataFormatException
     *             if an error occurs
     */
    public static Calendar findCurrentTime(String baseTime, String fileName)
            throws DataFormatException {
        Calendar retVal = getSystemCalendar(fileName);
        try {
            String regexe = "(\\d{2})(\\d{2})(\\d{2})[Zz]?";
            Pattern pattern = Pattern.compile(regexe);
            Matcher matcher = pattern.matcher(baseTime);
            if (matcher.matches()) {
                adjustDayHourMinute(retVal, matcher.group(1), matcher.group(2),
                        matcher.group(3));
            } else {
                throw new ParseException("Invalid format - does not match "
                        + regexe, 0);
            }
        } catch (Exception e) {
            throw new DataFormatException("Unable to find current time for "
                    + baseTime + ", exception was " + e.toString());
        }
        return retVal;
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
    private static void adjustDayHourMinute(Calendar cal, String day,
            String hour, String minute) {
        int iDay = Integer.parseInt(day);
        int iHour = Integer.parseInt(hour);
        int iMinute = Integer.parseInt(minute);
        int iMonth = cal.get(Calendar.MONTH);
        int iYear = cal.get(Calendar.YEAR);
        // adjust the month and year for roll-over situations
        if (iDay > cal.get(Calendar.DAY_OF_MONTH)) {
            iMonth--;
            if (iMonth < 0) {
                iMonth = Calendar.DECEMBER;
                iYear--;
            }
        }
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        cal.set(Calendar.YEAR, iYear);
        cal.set(Calendar.MONTH, iMonth);
        cal.set(Calendar.DAY_OF_MONTH, iDay);
        cal.set(Calendar.HOUR_OF_DAY, iHour);
        cal.set(Calendar.MINUTE, iMinute);

    }

    /**
     * Set the time service. To clear an existing service, set a null reference.
     * 
     * @param service
     *            A time service instance. Setting to null clears the current
     *            service.
     * @return The TimeService that had been previously defined.
     */
    public static ITimeService setTimeService(ITimeService service) {
        ITimeService retService = null;
        // get the current service if any.
        retService = timeService;
        timeService = service;
        return retService;
    }

    /**
     * Create a Greenwich Mean Time calendar for a given base date at 0 hours,
     * minutes and seconds on a specified year, month, and day.
     * 
     * @param year
     *            Calendar year.
     * @param month
     *            Month of the year [1..12].
     * @param day
     *            Day of the month [1..31] varies by month rules.
     * @return
     */
    public static Calendar getBaseCalendar(int year, int month, int day) {
        Calendar calendar = null;

        calendar = getSystemCalendar();

        calendar.set(Calendar.YEAR, year);
        calendar.set(Calendar.MONTH, month - 1);
        calendar.set(Calendar.DAY_OF_MONTH, day);

        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        return calendar;
    }

    /**
     * Get a new GMT time-zone calendar set to a specified time in milliseconds.
     * 
     * @param timeInMillis
     *            The time to set in milliseconds.
     * @return The new calendar instance.
     */
    public static Calendar newCalendar(long timeInMillis) {
        Calendar calendar = getSystemCalendar();

        calendar.setTimeInMillis(timeInMillis);

        return calendar;
    }

    /**
     * Make a copy of a calendar instance.
     * 
     * @param calendar
     *            The calendar to copy.
     * @return The copied calendar.
     */
    public static Calendar copy(Calendar calendar) {
        Calendar retValue = null;
        if (calendar != null) {
            retValue = (Calendar) calendar.clone();
        }
        return retValue;
    }

    /**
     * Make a copy of a calendar instance to the nearest hour.
     * 
     * @param calendar
     *            The calendar to copy.
     * @return The copied calendar.
     */
    public static Calendar copyToNearestHour(Calendar calendar) {
        Calendar retValue = null;
        if (calendar != null) {
            retValue = (Calendar) calendar.clone();
            retValue.set(Calendar.MINUTE, 0);
            retValue.set(Calendar.SECOND, 0);
            retValue.set(Calendar.MILLISECOND, 0);
        }
        return retValue;
    }

    /**
     * Make a copy of a calendar instance and round to the nearest hour.
     * 
     * @param calendar
     *            The calendar to copy.
     * @return The copied calendar.
     */
    public static Calendar roundToNearestHour(Calendar calendar) {
        Calendar retValue = null;
        if (calendar != null) {
            retValue = (Calendar) calendar.clone();
            if (calendar.get(Calendar.MINUTE) >= 30) {
                retValue.add(Calendar.HOUR_OF_DAY, 1);
            }
            retValue.set(Calendar.MINUTE, 0);
            retValue.set(Calendar.SECOND, 0);
            retValue.set(Calendar.MILLISECOND, 0);
        }
        return retValue;
    }

    /**
     * Roll the date of a calendar +/- a given number of days.
     * 
     * @param calendar
     *            Calendar instance to modify.
     * @param byDays
     *            Number of days to add or subtract.
     * @return The modified calendar.
     */
    public static Calendar rollByDays(Calendar calendar, int byDays) {
        if (calendar != null) {
            long millis = calendar.getTimeInMillis();

            long days = byDays * MILLIS_DAY;

            calendar.setTimeInMillis(millis + days);
        }
        return calendar;
    }

    /**
     * Roll the date of a calendar +/- a given number of hours.
     * 
     * @param calendar
     *            Calendar instance to modify.
     * @param byHours
     *            Number of hours to add or subtract.
     * @return The modified calendar.
     */
    public static Calendar rollByHours(Calendar calendar, int byHours) {
        if (calendar != null) {
            long millis = calendar.getTimeInMillis();

            long hours = byHours * MILLIS_HOUR;

            calendar.setTimeInMillis(millis + hours);
        }
        return calendar;
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
    public static Calendar findDataTime(String ddhhmm, Headers headers) {
        Calendar issueTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        String fileName = null;
        if (headers != null) {
            fileName = (String) headers.get(DecoderTools.INGEST_FILE_NAME);
        }
        try {
            return issueTime = findCurrentTime(ddhhmm, fileName);
        } catch (DataFormatException e) {
            if (logger.isInfoEnabled()) {
                logger.info(" Error in processing MND time; return current time ");
            }
            return issueTime;
        }
    }

}
