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
package com.raytheon.viz.texteditor.util;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.GetNextEtnRequest;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * Utility class for assigning the next ETN to a VTEC string in a transmitted
 * VTEC product.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 09, 2009            bwoodle     Initial creation
 * May 08, 2013  #1842     dgilling    Code cleanup.
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */

public class VtecUtil {

    private static final String dateFormat = "%1$ty%1$tm%1$tdT%1$tH%1$tMZ";

    public static final Pattern VTEC_REGEX = Pattern
            .compile("\\/([OTEX])\\.([A-Z]{3})\\.([A-Za-z0-9]{4})\\.([A-Z]{2})\\.([WAYSFON])\\.(\\d{4})\\.(\\d{6}T\\d{4}Z)-(\\d{6}T\\d{4}Z)\\/");

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     * 
     */
    private VtecUtil() {
        throw new AssertionError();
    }

    /**
     * Gets the next available ETN for a specific product and office
     * 
     * @param message
     *            the message containing the VTEC string to update
     * 
     * @return the update message
     * 
     * @throws VizException
     *             if an error occurs
     */
    public static String getVtec(String message) throws VizException {
        return getVtec(message, false);
    }

    /**
     * Gets the next available ETN for a specific product and office
     * 
     * @param message
     *            the message containing the VTEC string to update
     * 
     * @return the update message
     * 
     * @throws VizException
     *             if an error occurs
     */
    public static String getVtec(String message, boolean lockEtn)
            throws VizException {
        /* quick short circuit - exit if message is null */
        if (message == null) {
            return message;
        }
        VtecObject vtec = parseMessage(message);
        if (vtec == null || vtec.getAction() == null) {
            return message;
        }

        if (vtec.getAction().equals("NEW")) {
            vtec.setSequence(getNextEtn(vtec.getOffice(), vtec.getPhenomena()
                    + "." + vtec.getSignificance(), lockEtn));
        }
        return replaceFirstVtecString(message, vtec);
    }

    public static int getNextEtn(String office, String phensig, boolean lockEtn)
            throws VizException {
        int rval = 1;
        GetNextEtnRequest req = new GetNextEtnRequest();
        req.setSiteID(office);
        req.setPhensig(phensig);
        req.setLockEtn(lockEtn);
        Calendar currentTime = Calendar.getInstance();
        currentTime.setTime(SimulatedTime.getSystemTime().getTime());
        req.setCurrentTime(currentTime);
        CAVEMode mode = CAVEMode.getMode();
        if (mode.equals(CAVEMode.PRACTICE)) {
            req.setMode(ActiveTableMode.PRACTICE);
        } else {
            req.setMode(ActiveTableMode.OPERATIONAL);
        }

        Integer resp = (Integer) ThriftClient.sendRequest(req);
        if (resp != null) {
            rval = resp;
        }
        return rval;
    }

    public static VtecObject parseMessage(String message) {
        VtecObject rval = null;
        Matcher m = VTEC_REGEX.matcher(message);
        if (m.find()) {
            rval = new VtecObject(m.group());
        }
        return rval;
    }

    /**
     * Performs a replacement on a P-VTEC string embedded in a message. This
     * assumes that 1) the P-VTEC string is identifiable by the WFO, phenomenon,
     * and sequence, and 2) at most one matching VTEC string will occur in a
     * single message. If the message does not have a matching VTEC string, the
     * original message is returned.
     * 
     * @param message
     *            the message to modify
     * @param obj
     *            new VTEC for the message
     * 
     * @return the modified message
     */
    public static String replaceFirstVtecString(String message, VtecObject vtec) {
        String vStr = vtec.getVtecString();
        String pattern = vtec.getMatchString();
        // Replace the representation of 0 time with the text of 0 time.
        // This is needed for follow ups (i.e. EXT, CON, CAN)
        return message.replaceFirst(pattern, vStr).replace("991130T0000Z",
                "000000T0000Z");
    }

    /**
     * Formats the Calendar into the standard VTEC date/time string format. If
     * the Calendar is {@code null}, an underscore is used as the formatted
     * string. The standard VTEC format is "yymmddThhmmZ"
     * 
     * @param cal
     *            the Calendar to format
     * @return the formatted Calendar
     */
    public static final String formatVtecTime(Calendar cal) {
        return (cal != null) ? String.format(dateFormat, cal) : "_";
    }

    /**
     * Creates a calendar for the specified dateTime. The time zone is set to
     * GMT. No checking is performed to validate entries.
     * 
     * @param year
     *            four digit year
     * @param month
     *            month of the year (January = 0)
     * @param day
     *            day of the month
     * @param hour
     *            hour of the day (0 - 23)
     * @param min
     *            minute of the hour (0 - 59)
     * 
     * @return a Calendar representing the specified date.
     */
    public static final Calendar getVtecCalendar(int year, int month, int day,
            int hour, int min) {
        GregorianCalendar cal = new GregorianCalendar(
                TimeZone.getTimeZone("GMT"));
        cal.clear();
        cal.set(year, month, day, hour, min);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        return cal;
    }

    /**
     * Creates a Calendar object by parsing the VTEC date/time string.
     * 
     * @param date
     *            the date/time string to parse.
     * 
     * @return the Calendar representing the date.
     */
    public static final Calendar calendarFromVtec(String date) {
        Pattern p = Pattern
                .compile("(\\d{2})(\\d{2})(\\d{2})T(\\d{2})(\\d{2})Z");
        Matcher m = p.matcher(date);

        // get a calendar based on GMT time zone
        // all time stamp strings from the catalog query are in GMT
        GregorianCalendar cal = new GregorianCalendar(
                TimeZone.getTimeZone("GMT"));

        if (m.matches()) {
            int year = Integer.parseInt(m.group(1)) + 2000;
            int month = Integer.parseInt(m.group(2)) - 1;
            int day = Integer.parseInt(m.group(3));
            int hour = Integer.parseInt(m.group(4));
            int min = Integer.parseInt(m.group(5));
            cal.clear();
            cal.set(year, month, day, hour, min);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);
        }
        return cal;
    }

}
