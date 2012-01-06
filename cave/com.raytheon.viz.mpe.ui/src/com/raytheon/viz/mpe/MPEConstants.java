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
import java.util.TimeZone;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public class MPEConstants {
    private static final String GMT = "GMT";

    /**
     * yyyyMMddHH format
     */
    public static SimpleDateFormat DATE_FORMAT = new SimpleDateFormat(
            "yyyyMMddHH");

    /**
     * MMddyyyyHH format
     */
    public static SimpleDateFormat XMRG_DATE_FORMAT = new SimpleDateFormat(
            "MMddyyyyHH");

    /**
     * MMM dd yyyy HH format
     */
    public static SimpleDateFormat DATE_FORMAT_MMDDYYYYHH = new SimpleDateFormat(
            "MMM dd yyyy HH");

    /**
     * MMddyyyyHHmm
     */
    public static SimpleDateFormat DATE_FORMAT_MMDDYYYYHHMM = new SimpleDateFormat(
            "MMddyyyyHHmm");

    public static final SimpleDateFormat DATE_FORMAT_YYYYMMDDHHMMSS = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    public static final int NUM_DPA_COLS = 131;

    public static final int NUM_DPA_ROWS = 131;

    static {
        DATE_FORMAT.setTimeZone(TimeZone.getTimeZone(GMT));
        DATE_FORMAT_MMDDYYYYHH.setTimeZone(TimeZone.getTimeZone(GMT));
        DATE_FORMAT_YYYYMMDDHHMMSS.setTimeZone(TimeZone.getTimeZone(GMT));
        DATE_FORMAT_MMDDYYYYHHMM.setTimeZone(TimeZone.getTimeZone(GMT));
        XMRG_DATE_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));

    }
}
