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

import java.io.File;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * TimeTools provides a set of mid level Calendar manipulation methods. The
 * rollBy"X" methods allow changing the date of a calendar by specifying an
 * increment/decrement hours, days, etc. This is useful for some decoded data
 * types that specify the number of "X" units from a given base date/time.
 * 
 * When looking for generic Time utilities, please consider
 * {@link com.raytheon.uf.common.time.util.TimeUtil} first.
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
 * 20130219           1636 rferrel     File timestamp can now be YYMMDD or YYMMDDHH.
 * 20130912           2249 rferrel     Added getWarningTimestamp method.
 * 20140409           2907 njensen     Deprecated duplicated functionality
 * 20140514           2536 bclement    moved WMO Header time parsing to WMOTimeParser
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public class TimeTools {

    /**
     * Time stamp for a file name created by the Text Editor Dialog. This
     * assumes the 10 digit following .wan is the warning's issue time in epoch
     * seconds.
     */
    private static final String TEXT_EDITOR_WARNING = ".*\\.wan(\\d{10})$";

    /**
     * Environment variable with the root directory.
     */
    private static final String DATA_ARCHIVE_ROOT = "data.archive.root";

    /**
     * Pattern for getting time stamp from Text Editor Dialog created files.
     */
    private static Pattern FILE_WARNING_TIMESTAMP = null;


    /**
     * @deprecated use com.raytheon.uf.common.time.util.TimeUtil instead
     */
    @Deprecated
    public static final int HOURS_DAY = TimeUtil.HOURS_PER_DAY;

    /**
     * @deprecated use com.raytheon.uf.common.time.util.TimeUtil instead
     */
    @Deprecated
    public static final int MINUTES_HOUR = TimeUtil.MINUTES_PER_HOUR;

    /**
     * @deprecated use com.raytheon.uf.common.time.util.TimeUtil instead
     */
    @Deprecated
    public static final int SECONDS_HOUR = TimeUtil.SECONDS_PER_HOUR;

    /**
     * @deprecated use com.raytheon.uf.common.time.util.TimeUtil instead
     */
    @Deprecated
    public static final int SECONDS_DAY = TimeUtil.SECONDS_PER_DAY;

    /**
     * @deprecated use com.raytheon.uf.common.time.util.TimeUtil instead
     */
    @Deprecated
    public static final long MILLIS_HOUR = TimeUtil.MILLIS_PER_HOUR;

    /**
     * @deprecated use com.raytheon.uf.common.time.util.TimeUtil instead
     */
    @Deprecated
    public static final long MILLIS_DAY = TimeUtil.MILLIS_PER_DAY;

    /**
     * @deprecated use com.raytheon.uf.common.time.util.TimeUtil instead
     */
    @Deprecated
    public static final String ZULU_TIMEZONE = "Zulu";

    private static ITimeService timeService = null;

    /**
     * Get a calendar that expresses the current system time. If an ITimeService
     * provider is registered, the time is retrieved from the service.
     * 
     * @return The current time as a GMT Calendar.
     * @deprecated use com.raytheon.uf.common.time.util.TimeUtil instead
     */
    @Deprecated
    public static final Calendar getSystemCalendar() {
        Calendar retCal = null;
        if (timeService != null) {
            retCal = timeService.getCalendar();
        } else {
            retCal = Calendar.getInstance(TimeZone.getTimeZone(ZULU_TIMEZONE));
        }
        if (retCal != null) {
            TimeZone tz = retCal.getTimeZone();
            if (tz != null) {
                if (0 != tz.getRawOffset()) {
                    retCal.setTimeZone(TimeZone.getTimeZone(ZULU_TIMEZONE));
                }
            } else {
                retCal.setTimeZone(TimeZone.getTimeZone(ZULU_TIMEZONE));
            }
        }
        return retCal;
    }

    /**
     * Get the time stamp of a warning file name based on the name generated by
     * the TextEditorDialog.
     * 
     * @param fileName
     * @return timestamp warning's issue time in epoch seconds when fileName is
     *         a Text Editor Dialog file otherwise null
     */
    public static final String getWarningTimestamp(String fileName) {
        if (FILE_WARNING_TIMESTAMP == null) {
            // Create pattern to test if fileName is in a director relative to
            // DATA_ARCHIVE_ROOT and ends with the expected extension.
            StringBuilder pattern = new StringBuilder("^");
            pattern.append(System.getProperty(DATA_ARCHIVE_ROOT));
            if (!pattern.substring(pattern.length() - 1).equals(File.separator)) {
                pattern.append(File.separator);
            }
            pattern.append(TEXT_EDITOR_WARNING);
            FILE_WARNING_TIMESTAMP = Pattern.compile(pattern.toString());
        }

        String timestamp = null;
        Matcher matcher = FILE_WARNING_TIMESTAMP.matcher(fileName);
        if (matcher.find()) {
            timestamp = matcher.group(1);
        }
        return timestamp;
    }

    /**
     * Set the time service. To clear an existing service, set a null reference.
     * 
     * @param service
     *            A time service instance. Setting to null clears the current
     *            service.
     * @return The TimeService that had been previously defined.
     */
    public static final ITimeService setTimeService(ITimeService service) {
        ITimeService retService = null;
        // get the current service if any.
        retService = timeService;
        timeService = service;
        return retService;
    }



    /**
     * Get a new GMT time-zone calendar set to a specified time in milliseconds.
     * 
     * @param timeInMillis
     *            The time to set in milliseconds.
     * @return The new calendar instance.
     * @deprecated use com.raytheon.uf.common.time.util.TimeUtil instead
     */
    @Deprecated
    public static final Calendar newCalendar(long timeInMillis) {
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
     * @deprecated use com.raytheon.uf.common.time.util.TimeUtil instead
     */
    @Deprecated
    public static final Calendar copy(Calendar calendar) {
        Calendar retValue = null;
        if (calendar != null) {
            retValue = newCalendar(calendar.getTimeInMillis());
            retValue.setTimeZone(calendar.getTimeZone());
        }
        return retValue;
    }

    /**
     * Make a copy of a calendar instance truncated to the hour.
     * 
     * @param calendar
     *            The calendar to copy.
     * @return The copied calendar.
     */
    public static final Calendar copyToNearestHour(Calendar calendar) {
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
    public static final Calendar roundToNearestHour(Calendar calendar) {
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
     * @deprecated use java.util.Calendar.add(int, int) instead
     */
    @Deprecated
    public static final Calendar rollByDays(Calendar calendar, int byDays) {
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
     * @deprecated use java.util.Calendar.add(int, int) instead
     */
    @Deprecated
    public static final Calendar rollByHours(Calendar calendar, int byHours) {
        if (calendar != null) {
            long millis = calendar.getTimeInMillis();

            long hours = byHours * MILLIS_HOUR;

            calendar.setTimeInMillis(millis + hours);
        }
        return calendar;
    }

    /**
     * Disable constructor.
     */
    private TimeTools() {
    }
}
