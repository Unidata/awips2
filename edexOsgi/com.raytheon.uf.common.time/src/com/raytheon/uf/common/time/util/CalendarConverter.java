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

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.DatatypeConverter;

import org.apache.commons.beanutils.Converter;

/**
 * Custom converter implementation for converting Calendar objects to and from
 * Strings
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         bphillip    Initial Creation
 * Mar 13, 2013 1789       bsteffen    Move Calendar and Date parsing out of
 *                                     ConvertUtil and also fix date parsing.
 * jan 22, 2014 2731       dhladky     Calendar converter now returns a calendar.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class CalendarConverter implements Converter {

    private static Pattern DATE_PATTERN = Pattern.compile(TimeUtil.DATE_STRING);

    public CalendarConverter() {

    }

    @SuppressWarnings("rawtypes")
    @Override
    public Object convert(Class type, Object value) {

        if (value instanceof String) {
            String date = (String) value;
            try {
                // see if string is in ISO 8601
                return DatatypeConverter.parseDateTime(date);
            } catch (Exception e) {
                // try to match the pattern.
            }

            Matcher m = DATE_PATTERN.matcher(date);

            // get a calender based on GMT timezone
            // all time stamp strings from the catalog query are in GMT
            GregorianCalendar calendar = new GregorianCalendar(TimeZone
                    .getTimeZone("GMT"));

            if (m.matches()) {
                int year = Integer.parseInt(m.group(1));
                int month = Integer.parseInt(m.group(2)) - 1;
                int day = Integer.parseInt(m.group(3));
                int hour = Integer.parseInt(m.group(4));
                int min = Integer.parseInt(m.group(5));
                int sec = Integer.parseInt(m.group(6));
                calendar.set(year, month, day, hour, min, sec);
                calendar
                        .set(Calendar.MILLISECOND, Integer.parseInt(m.group(7)));

            }
            return calendar;
        }
        if (value instanceof Calendar) {
            Calendar cal = (Calendar) value;
            return TimeUtil.formatCalendar(cal);
        }
        return null;
    }

}
