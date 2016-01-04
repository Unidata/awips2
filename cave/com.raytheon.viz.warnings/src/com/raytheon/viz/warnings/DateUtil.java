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
package com.raytheon.viz.warnings;

import java.text.DateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.viz.texteditor.TextWarningConstants;

/**
 * Warning Date Util
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    06182008                  bwoodle     additional format method overloads, javadocs.
 *    Sep 12, 2014 ASM RM#15551 Qinglu Lin  Added formatUseNoonMidnight().
 *    Oct 21, 2015   5022       randerso    Changes for mixed case WarnGen products
 * 
 * </pre>
 * 
 * @author ?
 * 
 */
public class DateUtil {

    Pattern timePtrn = Pattern.compile("(1200\\s(AM|PM))");

    /**
     * Format a date for the severe weather warnings templates
     * 
     * @param date
     * @param format
     * @return
     */
    public String format(Date date, DateFormat format) {
        return format(date, format, 0);
    }

    /**
     * Format a date for the severe weather warnings templates
     * 
     * @param date
     * @param format
     * @param interval
     * @return
     */
    public String format(Date date, DateFormat format, int interval) {
        return format(date, format, interval, TimeZone.getTimeZone("GMT"));
    }

    /**
     * Format a date for the severe weather warnings templates
     * 
     * This method rounds to the closest interval - up or down.
     * 
     * @param date
     * @param format
     * @param interval
     * @param tz
     * @return
     */
    public String format(Date date, DateFormat format, int interval, String tz) {
        return format(date, format, interval, getTimeZoneFromString(tz));
    }

    /**
     * Format a date for the severe weather warnings templates
     * 
     * This method rounds to the closest interval - up or down.
     * 
     * @param date
     * @param format
     * @param interval
     * @param tz
     * @return
     */
    public String format(Date date, DateFormat format, int interval, TimeZone tz) {
        String str;
        Date workingDate = date;
        if (interval > 0) {
            workingDate = roundDate(date, interval);
        }
        synchronized (format) {
            format.setTimeZone(tz);
            str = format.format(workingDate);
        }
        return str;
    }

    public String formatUseNoonMidnight(Date date, DateFormat format,
            int interval, String tz) {
        return formatUseNoonMidnight(date, format, interval,
                getTimeZoneFromString(tz));
    }

    public String formatUseNoonMidnight(Date date, DateFormat format,
            int interval, TimeZone tz) {
        String str;
        Date workingDate = date;
        if (interval > 0) {
            workingDate = roundDate(date, interval);
        }
        synchronized (format) {
            format.setTimeZone(tz);
            str = format.format(workingDate);
        }
        Matcher m = timePtrn.matcher(str);
        if (m.find()) {
            str = str.replace("1200 AM", "midnight");
            str = str.replace("1200 PM", "noon");
        }
        return str;
    }

    /**
     * Format a date for the severe weather warnings templates
     * 
     * @param date
     * @param format
     * @param tz
     * @return
     */
    public String format(Date date, DateFormat format, TimeZone tz) {
        return format(date, format, 0, tz);
    }

    /**
     * Format a date for the severe weather warnings templates
     * 
     * @param date
     * @param format
     * @param tz
     * @return
     */
    public String format(Date date, DateFormat format, String tz) {
        return format(date, format, 0, getTimeZoneFromString(tz));
    }

    /**
     * Format a date for the severe weather warnings templates
     * 
     * @param date
     * @param format
     * @return
     */
    public String formatLocal(Date date, DateFormat format) {
        return formatLocal(date, format, 0);
    }

    /**
     * Format a date for the severe weather warnings templates
     * 
     * @param date
     * @param format
     * @param interval
     * @return
     */
    public String formatLocal(Date date, DateFormat format, int interval) {
        return format(date, format, interval, TimeZone.getDefault());
    }

    private TimeZone getTimeZoneFromString(String tz) {
        TimeZone rval = null;

        rval = TextWarningConstants.timeZoneAbbreviationMap.get(tz);
        if (rval == null) {
            rval = TimeZone.getDefault();
        }
        return rval;
    }

    public String period(Date date, DateFormat format, int interval, String tz) {
        Pattern periodPtrn = Pattern.compile("(\\d{3})\\s(\\w{2})");
        Matcher m = periodPtrn.matcher(format(date, format, interval, tz));
        if (m.find()) {
            if (m.group(2).equalsIgnoreCase("AM")) {
                return " morning";
            } else if (Integer.parseInt(m.group(1)) < 600) {
                return " afternoon";
            } else {
                return " evening";
            }
        } else {
            return "";
        }
    }

    public static Date roundDateTo15(Date input) {
        int intervalInMs = 60 * 1000 * 15;
        long tNormal = input.getTime();
        long tEarlier = input.getTime() - (input.getTime() % intervalInMs);
        long tLater = tEarlier + intervalInMs;
        long earlierDelta = tNormal - tEarlier;
        long laterDelta = tLater - tNormal;
        if (earlierDelta > laterDelta) {
            return new Date(tLater);
        } else {
            return new Date(tEarlier);
        }
    }

    public static Date roundDate(Date input, int intervalInMin) {
        int intervalInMs = 60 * 1000 * intervalInMin;
        long tNormal = input.getTime();
        long tEarlier = input.getTime() - (input.getTime() % intervalInMs);
        long tLater = tEarlier + intervalInMs;
        long earlierDelta = tNormal - tEarlier;
        long laterDelta = tLater - tNormal;
        if (earlierDelta > laterDelta) {
            return new Date(tLater);
        } else {
            return new Date(tEarlier);
        }
    }
}