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
package com.raytheon.viz.mpe;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

/**
 * 
 * MPE Date formatter
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 3, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MPEDateFormatter {

    public static final TimeZone GMT_TIMEZONE = TimeZone.getTimeZone("GMT");

    public static final String yyyyMMddHH = "yyyyMMddHH";

    public static final String MMM_dd_yyyy_HH = "MMM dd yyyy HH";

    public static final String MMddyyyyHH = "MMddyyyyHH";

    public static final String MMM = "MMM";

    public static final String yyyyMMddHHmmss = "yyyy-MM-dd HH:mm:ss";

    public static final String OBS_DATE_FORMAT = yyyyMMddHHmmss;

    public static String format_yyyyMMddHH(Date date) {
        return format(date, yyyyMMddHH);
    }

    public static String format_MMddyyyyHH(Date date) {
        return format(date, MMddyyyyHH);
    }

    public static String format_MMM_dd_yyyy_HH(Date date) {
        return format(date, MMM_dd_yyyy_HH);
    }

    public static String format_obs(Date date) {
        return format(date, OBS_DATE_FORMAT);
    }

    public static String format_yyyyMMddHHmmss(Date date) {
        return format(date, yyyyMMddHHmmss);
    }

    public static SimpleDateFormat createSimpleDateFormat(String format) {
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        sdf.setTimeZone(GMT_TIMEZONE);
        return sdf;
    }

    public static String format(Date date, String format) {
        return createSimpleDateFormat(format).format(date);
    }
}
