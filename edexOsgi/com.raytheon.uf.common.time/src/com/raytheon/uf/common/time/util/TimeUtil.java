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
package com.raytheon.uf.common.time.util;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

/**
 * Utilities for time, some extracted from Util.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TimeUtil {

    public static final String DATE_STRING = "(\\d{4})-(\\d{2})-(\\d{2})[ _](\\d{2}):(\\d{2}):(\\d{2})\\.(\\d{1,3})";

    // create instance of simple date format on class load, as instantiating it
    // is expensive the SimpleDateFormat class is not thread-safe,
    // so calling methods use synchronized
    private static SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyy-MM-dd_HH:mm:ss.SSS");

    private static SimpleDateFormat sqlSdf = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss.SSS");

    static {
        sqlSdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Retrieve date as a string in the index standard format: yyyy-MM-dd
     * kk:mm:ss.SSS
     * 
     * @param aCalendar
     *            A Calendar instance
     * @return The formatted date string from the Calendar instance
     */
    public static String formatDate(Calendar aCalendar) {
        return formatDate(aCalendar.getTime());
    }

    /**
     * Retrieve date as a string in the index standard format: yyyy-MM-dd
     * kk:mm:ss.SSS
     * 
     * @param aDate
     *            A Date instance
     * @return The formatted date string from the Date instance
     */
    public static String formatDate(Date aDate) {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis(aDate.getTime());
        return formatCalendar(cal);
    }

    /**
     * Formats a calendar object into the following format yyyy-MM-dd_HH:mm:ss.S
     * 
     * @param cal
     *            The calendar to format
     * @return The formatted result
     */
    public static String formatCalendar(Calendar cal) {
        String format = null;

        synchronized (sdf) {
            sdf.setTimeZone(cal.getTimeZone());
            format = sdf.format(cal.getTime());
        }
        return format;
    }

    /**
     * Converts a Calendar in the local time zone to a GMT date
     * 
     * @param cal
     *            A Calendar object in the local time zone
     * @return The GMT date
     */
    public static Date calendarToGMT(Calendar cal) {
        Date dt = null;
        synchronized (sdf) {
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            String str = formatCalendar(cal);
            sdf.setTimeZone(TimeZone.getDefault());
            try {
                dt = sdf.parse(str);
            } catch (ParseException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        return dt;
    }

    public static long formattedDateToLong(String formattedDate) {
        long retVal = 0;
        try {
            retVal = sdf.parse(formattedDate).getTime();
        } catch (ParseException e) {
            e.printStackTrace();
        }
        return retVal;
    }

    public static String formatToSqlTimestamp(Date aDate) {
        synchronized (sqlSdf) {
            return sqlSdf.format(aDate);
        }
    }

}
