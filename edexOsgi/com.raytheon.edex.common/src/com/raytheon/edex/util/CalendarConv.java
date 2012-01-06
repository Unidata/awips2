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
package com.raytheon.edex.util;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;

/**
 * JiBX support for java.util.Calendar
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 28, 2007      #276   garmendariz Initial creation
 * Aug 4, 2008				randerso	Changed to use long millis as the serialized form
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class CalendarConv {

    /**
     * Convert serialized string form to java.util.Calendar
     * 
     * @param time
     *            serialized string form
     * @return calendar
     */
    public static Calendar deserializer(String time) {
        if (time != null) {
            long millis = Long.parseLong(time);

            GregorianCalendar calendar = new GregorianCalendar(TimeZone
                    .getTimeZone("GMT"));

            calendar.setTimeInMillis(millis);

            return calendar;
        } else {
            return null;
        }
    }

    /**
     * Convert java.util.Calendar to serialized string form
     * 
     * @param calendar
     * @return serialized string form
     */
    public static String serializer(Calendar calendar) {
        return Long.toString(calendar.getTimeInMillis());
    }

}