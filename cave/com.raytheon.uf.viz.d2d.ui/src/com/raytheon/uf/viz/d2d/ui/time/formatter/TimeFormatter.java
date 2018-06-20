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
package com.raytheon.uf.viz.d2d.ui.time.formatter;

import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.time.DataTime;

/**
 * Formats time for resolutions, offsets, and resources. This class is not made
 * up of static methods because the internal format objects that it creates
 * (DateFormat, NumberFormat) are not thread safe.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 4, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class TimeFormatter {
    private static final long secondsInDay = 86400;

    private static final long secondsInTenthOfDay = 8640;

    private static final long secondsInHour = 3600;

    private static final long secondsInMinute = 60;

    private DateFormat df_HHmm = new SimpleDateFormat("HH:mm");

    private DateFormat df_dayTime = new SimpleDateFormat("dd.HHmm");

    private DateFormat df_dayHour = new SimpleDateFormat("dd.HH");

    private DateFormat df_timeZMonthDate = new SimpleDateFormat(
            "HH:mm'Z' EEE dd-MMM-yy");

    private DateFormat df_Z = new SimpleDateFormat("Z");

    private NumberFormat nf_00 = new DecimalFormat("00");

    private NumberFormat nf_trimZeros = new DecimalFormat("####");

    /**
     * Default Constructor.
     */
    public TimeFormatter() {
        super();
        df_HHmm.setTimeZone(TimeZone.getTimeZone("GMT"));
        df_dayTime.setTimeZone(TimeZone.getTimeZone("GMT"));
        df_dayHour.setTimeZone(TimeZone.getTimeZone("GMT"));
        df_timeZMonthDate.setTimeZone(TimeZone.getTimeZone("GMT"));
        df_Z.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Format the given time in seconds into a readable uptime.
     */
    public String getFormattedOffsetString(long offset) {
        // Get the days, hours, minutes, and seconds.
        long seconds = offset;
        long days = seconds / secondsInDay;
        seconds = seconds % secondsInDay;
        long hours = seconds / secondsInHour;
        seconds = seconds % secondsInHour;
        long minutes = seconds / secondsInMinute;
        seconds = seconds % secondsInMinute;

        StringBuilder formatString = new StringBuilder();

        // Prepend the number of days to the rest of the string.
        if (days != 0) {
            if (days == 1) {
                formatString.append("1 day");
            } else {
                formatString.append(days).append(" days");
            }
            if (hours != 0 || minutes != 0 || seconds != 0) {
                formatString.append(", ");
            }
        } else {
            formatString.append("");
        }
        if (hours > 12 || (hours != 0 && minutes == 0)) {
            formatString.append(hours).append(" hours");
        } else if (hours != 0 && minutes != 0) {
            formatString.append(hours);
            formatString.append(":");
            formatString.append(nf_00.format(Math.abs(minutes)));
        } else if (minutes > 1 || minutes < -1) {
            formatString.append(minutes).append(" minutes");
        } else if (minutes == 1 || minutes == -1) {
            formatString.append(minutes).append(" minute");
        } else if (seconds == 1 || seconds == -1) {
            formatString.append(seconds).append(" second");
        } else if (seconds != 0) {
            formatString.append(seconds).append(" seconds");
        }
        return formatString.toString();
    }

    public String getDayTimeZHRString(DataTime dataTime) {
        StringBuilder sb = new StringBuilder();
        Date date = dataTime.getRefTimeAsCalendar().getTime();

        sb.append(df_dayTime.format(date));
        sb.append(" ");

        String zStr = df_Z.format(date);
        int z = Integer.parseInt(zStr) / 100;

        sb.append(nf_trimZeros.format(z));
        sb.append("HR");
        return sb.toString();
    }

    public String getDayHourForecastHRString(DataTime dataTime) {
        StringBuilder sb = new StringBuilder();
        Date date = dataTime.getRefTime();

        sb.append(df_dayHour.format(date));
        sb.append(" ");
        sb.append(dataTime.getFcstTime() / secondsInHour);
        sb.append("HR");
        return sb.toString();
    }

    public String getDayTimeForecastHRString(DataTime dataTime) {
        StringBuilder sb = new StringBuilder();
        Date date = dataTime.getRefTime();

        sb.append(df_dayTime.format(date));
        sb.append(" ");
        sb.append(dataTime.getFcstTime() / secondsInHour);
        sb.append("HR");
        return sb.toString();
    }

    public String getTimeMonthDateString(DataTime dataTime) {
        StringBuilder sb = new StringBuilder();
        Date date = dataTime.getRefTime();
        sb.append(df_timeZMonthDate.format(date));
        return sb.toString();
    }

    /*
     * Translated from load-mgr.tcl. Formats a seconds time value into a human
     * readable time value with a time frame calculation based on the number of
     * frames.
     */
    public String getFormattedTimePeriodString(long seconds, int frameCount) {
        StringBuilder sb = new StringBuilder();
        NumberFormat nf = new DecimalFormat("00");
        // Get the days, hours, minutes, and seconds.
        long timper = seconds * frameCount - seconds;
        long days = seconds / secondsInDay;
        seconds = seconds % secondsInDay;
        long hours = seconds / secondsInHour;
        seconds = seconds % secondsInHour;
        long minutes = seconds / secondsInMinute;
        seconds = seconds % secondsInMinute;

        // Prepend the number of days to the rest of the string.
        if (days > 0) {
            sb.append(days);
            sb.append(" dy");
            if (hours > 0 || minutes > 0 || seconds > 0) {
                sb.append(", ");
            }
        } else {
            sb.append("");
        }
        if (hours > 12 || (hours != 0 && minutes == 0)) {
            sb.append(hours);
            sb.append(" hrs");
        } else if (hours > 0 && minutes > 0) {
            sb.append(hours);
            sb.append(":");
            sb.append(nf.format(minutes));
            sb.append(" hrs");
        } else if (minutes > 1) {
            sb.append(minutes);
            sb.append(" min");
        } else if (minutes == 1) {
            sb.append("1 min");
        } else if (seconds == 1) {
            sb.append("1 sec");
        } else if (seconds != 0) {
            sb.append(seconds);
            sb.append(" sec");
        }

        if (timper <= 0) {
            return sb.toString();
        }

        seconds = timper;
        days = seconds / secondsInDay;
        seconds = seconds % secondsInDay;
        hours = seconds / secondsInHour;
        long tenths = seconds / secondsInTenthOfDay;
        seconds = seconds % secondsInHour;
        minutes = seconds / secondsInMinute;

        // Prepend the number of days to the rest of the string.
        if (days != 0) {
            if (tenths == 0) {
                sb.append(" (");
                sb.append(days);
                sb.append(" dy");
            } else {
                sb.append(" (");
                sb.append(days);
                sb.append(".");
                sb.append(tenths);
                sb.append(" dy)");
            }
        } else if (hours == 0) {
            sb.append(" (");
            sb.append(minutes);
            sb.append(" min)");
        } else if (minutes > 0) {
            sb.append(" (");
            sb.append(hours);
            sb.append(":");
            sb.append(nf.format(minutes));
            sb.append(")");
        } else {
            sb.append(" (");
            sb.append(hours);
            sb.append(" hr)");
        }
        return sb.toString();
    }
}
