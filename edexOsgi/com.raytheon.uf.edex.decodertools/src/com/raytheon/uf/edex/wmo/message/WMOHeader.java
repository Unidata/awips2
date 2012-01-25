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
package com.raytheon.uf.edex.wmo.message;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * The WMOHeader extracts and parses the relevant wmo header information (listed
 * below) from a wmo weather message. TTAAii CCCC YYGGgg (BBB) TT = Data type
 * and subtype AA = Regional origin ii = Numeric identifier 00-99 CCCC = Message
 * originator. (ICAO) YYGGgg = message time (GMT) (BBB) = (Optional) AA[Z-Z]
 * Amendment part A through Z CC[A-Z] Correction part A through Z RR[A-Z]
 * Routine delayed part A through Z P[A-Z][A-Z] Message Part AA through ZZ The
 * pattern used to match a WMO header is not strict as it allows zero to two
 * digits in the Regional origin, ii, field. The standard requires 2 digits,
 * however some bulletins are not compliant.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071001            391 jkorman     Initial Development
 * 20071130            410 jkorman     Changed bad copyright symbol.
 * 20071203            410 jkorman     JavaDoc complaints.
 * Jul 10, 2009 2191       rjpeter     Added getTtaaii.
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public class WMOHeader {

    public static final String INGEST_FILE_NAME = "ingestfilename";

    public static final String WMO_HEADER = "[A-Z]{3}[A-Z0-9](?:\\d{0,2}|[A-Z]{0,2}) [A-Z0-9]{4} \\d{6}(?: [A-Z]{3})?[^\\r\\n]*[\\r\\n]+";

    private static final Pattern WMO_HEADER_PATTERN = Pattern
            .compile(WMO_HEADER);

    private static final int CCCCGROUP_SIZE = 4;

    private static final int DTGROUP_SIZE = 6;

    private String wmoHeader;

    private char t1;

    private char t2;

    private char a1;

    private char a2;

    private int ii;

    private String cccc = null;

    private int headerYear = -1;

    private int headerMonth = -1;

    private int headerDay = -1;

    private int headerHour = -1;

    private int headerMinute = -1;

    private String YYGGgg = null;

    private String BBBIndicator = null;

    private boolean isValid = false;

    private int messageDataStart = -1;

    private int wmoHeaderStart = -1;

    private Calendar headerDate = null;

    private String ttaaii = null;

    private String originalMessage = null;

    public WMOHeader(byte[] bytes) {
        this(bytes, null);
    }

    /**
     * Construct the header from a wmo message.
     * 
     * @param messageData
     */
    public WMOHeader(byte[] messageData, Headers headers) {
        // Assume not valid until proven otherwise!
        isValid = false;
        if (messageData != null) {
            originalMessage = new String(messageData);
            Matcher m = WMO_HEADER_PATTERN.matcher(originalMessage); // handles
            // the skip
            // ccb
            if (m.find()) {
                wmoHeaderStart = m.start();
                messageDataStart = m.end();
                wmoHeader = originalMessage.substring(m.start(), m.end())
                        .trim();

                int hdrIndex = 0;
                t1 = wmoHeader.charAt(hdrIndex++);
                t2 = wmoHeader.charAt(hdrIndex++);
                a1 = wmoHeader.charAt(hdrIndex++);
                a2 = wmoHeader.charAt(hdrIndex++);
                char t = wmoHeader.charAt(hdrIndex++);
                int tt = 0;
                int iiChars = 0;
                // Get the ii part. The standard says there must be two digits,
                // however some data contain 0 or 1 digits.
                while (t != ' ') {
                    tt *= 10;
                    tt += (t - '0');
                    iiChars++;
                    t = wmoHeader.charAt(hdrIndex++);
                }

                if (iiChars > 0) {
                    ii = tt;
                } else {
                    ii = -1;
                }
                t = wmoHeader.charAt(hdrIndex);
                while (t == ' ') {
                    t = wmoHeader.charAt(hdrIndex++);
                    if (t != ' ') {
                        hdrIndex--;
                    }
                }

                cccc = wmoHeader.substring(hdrIndex, hdrIndex + CCCCGROUP_SIZE);
                hdrIndex += CCCCGROUP_SIZE;

                t = wmoHeader.charAt(hdrIndex);
                while (t == ' ') {
                    t = wmoHeader.charAt(hdrIndex++);
                    if (t != ' ') {
                        hdrIndex--;
                    }
                }
                YYGGgg = wmoHeader.substring(hdrIndex, hdrIndex + DTGROUP_SIZE);
                parseDateTime(YYGGgg);
                headerDate = TimeTools.findDataTime(YYGGgg, headers);
                // At this point headerDate will either be the current time (non-archive) or
                // a time generated from the WMOHeader and filename dateStamp
                
                headerYear = headerDate.get(Calendar.YEAR);
                headerMonth = headerDate.get(Calendar.MONTH) + 1;
                headerDay = headerDate.get(Calendar.DAY_OF_MONTH);
                headerHour = headerDate.get(Calendar.HOUR_OF_DAY);
                headerMinute = headerDate.get(Calendar.MINUTE);
                
                hdrIndex += DTGROUP_SIZE;

                // Everything else goes here for now. Leave it to the client to
                // perform any BBB validation.
                BBBIndicator = wmoHeader.substring(hdrIndex).trim();

                isValid = true;
            }
        }
    }

    public String getOriginalMessage() {
        return originalMessage;
    }

    /**
     * Get the extracted wmo header text. Does not return the trailing carriage
     * control on the header line.
     * 
     * @return The extracted wmo header.
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * Get the T1 character.
     * 
     * @return The T1 character.
     */
    public char getT1() {
        return t1;
    }

    /**
     * Get the T2 character.
     * 
     * @return The T2 character.
     */
    public char getT2() {
        return t2;
    }

    /**
     * Get the A1 character.
     * 
     * @return The A1 character.
     */
    public char getA1() {
        return a1;
    }

    /**
     * Get the A2 character.
     * 
     * @return The A2 character.
     */
    public char getA2() {
        return a2;
    }

    /**
     * Get the ii part. If ii is not defined in the header, a value of -1 is
     * returned.
     * 
     * @return The ii part.
     */
    public int getIi() {
        return ii;
    }

    /**
     * Get the sender CCCC identifier.
     * 
     * @return The sender CCCC identifier.
     */
    public String getCccc() {
        return cccc;
    }

    /**
     * @return the year YYYY
     */
    public int getYear() {
        return headerYear;
    }

    /**
     * @return the month [1-12]
     */
    public int getMonth() {
        return headerMonth;
    }

    /**
     * Get the day part of the header datetime.
     * 
     * @return The day part of the header datetime.
     */
    public int getDay() {
        return headerDay;
    }

    /**
     * Get the hour part of the header datetime.
     * 
     * @return The hour part of the header datetime.
     */
    public int getHour() {
        return headerHour;
    }

    /**
     * Get the minute part of the header datetime.
     * 
     * @return The minute part of the header datetime.
     */
    public int getMinute() {
        return headerMinute;
    }

    /**
     * Get the header datetime as a string.
     * 
     * @return The header datetime information.
     */
    public String getYYGGgg() {
        return YYGGgg;
    }

    /**
     * Get the BBB information. If the header does not contain BBB information
     * then a null reference is returned.
     * 
     * @return The BBB information.
     */
    public String getBBBIndicator() {
        return BBBIndicator;
    }

    /**
     * Is the WMO header information valid.
     * 
     * @return Is the WMO header information valid.
     */
    public boolean isValid() {
        return isValid;
    }

    /**
     * Get the starting position of the header within the message. This position
     * is the position of the first character of the WMO header.
     * 
     * @return The message start position.
     */
    public int getWmoHeaderStart() {
        return wmoHeaderStart;
    }

    /**
     * Get the starting position of the data within the message. This position
     * is the position of the next character after the end of the WMO header.
     * 
     * @return The message start position.
     */
    public int getMessageDataStart() {
        return messageDataStart;
    }

    /**
     * @return the headerDate
     */
    public Calendar getHeaderDate() {
        return headerDate;
    }

    /**
     * 
     * @return
     */
    public String getTtaaii() {
        if (ttaaii == null && isValid()) {
            StringBuilder sb = new StringBuilder();
            sb.append(wmoHeader.substring(0, 6));
            int spcIndex = sb.indexOf(" ");
            if (spcIndex >= 0 && spcIndex + 1 < sb.length()) {
                sb.delete(spcIndex + 1, sb.length());
            }
            while (sb.length() < 6) {
                sb.append(" ");
            }
            ttaaii = sb.toString();
        }
        return ttaaii;
    }

    /**
     * Get the string representation of this WMO header.
     * 
     * @return The string representation of this WMO header.
     */
    public String toString() {
        return wmoHeader;
    }

    /**
     * Parse the header date time information. If any part of the YYGGgg group
     * is in error, then all date elements are set to -1;
     * 
     * @param ddhhmm
     *            The YYGGgg group extracted from the header.
     */
    private void parseDateTime(String ddhhmm) {
        try {
            headerDay = Integer.parseInt(ddhhmm.substring(0, 2));
            headerHour = Integer.parseInt(ddhhmm.substring(2, 4));
            headerMinute = Integer.parseInt(ddhhmm.substring(4));
        } catch (Exception nfe) {
            // any failure in the parse above invalidates the entire date info.
            headerDay = -1;
            headerHour = -1;
            headerMinute = -1;
        }
    }

//    /**
//     * Use the parsed date/time elements to create the Calendar date time info
//     * for this WMO header. The logic in this method allows the WMO header time
//     * to be set to the current day, the next day, or up to 25 days in the past.
//     * 
//     * @return A Calendar instance based on the current system date time.
//     */
//    private Calendar createCalendarDate() {
//        Calendar msgDate = null;
//        // check the internal data first
//        if ((headerDay > -1) && (headerHour > -1) && (headerMinute > -1)) {
//            Calendar currentClock = TimeTools.getSystemCalendar(headerYear,
//                    headerMonth, headerDay);
//
//            Calendar obsDate = null;
//            Calendar tTime = TimeTools.copyToNearestHour(currentClock);
//            // Set to the next day.
//            TimeTools.rollByDays(tTime, 1);
//
//            if (headerDay == currentClock.get(Calendar.DAY_OF_MONTH)) {
//                obsDate = TimeTools.copyToNearestHour(currentClock);
//                obsDate.set(Calendar.HOUR_OF_DAY, headerHour);
//                obsDate.set(Calendar.MINUTE, headerMinute);
//            } else if (headerDay == tTime.get(Calendar.DAY_OF_MONTH)) {
//                // Observation time is in the next day
//                obsDate = TimeTools.copyToNearestHour(tTime);
//                obsDate.set(Calendar.HOUR_OF_DAY, headerHour);
//                obsDate.set(Calendar.MINUTE, headerMinute);
//            } else {
//                tTime = TimeTools.copyToNearestHour(currentClock);
//                int i = 0;
//                while (i++ < 25) {
//                    // Go back a day
//                    TimeTools.rollByDays(tTime, -1);
//                    if (headerDay == tTime.get(Calendar.DAY_OF_MONTH)) {
//                        // Day values are equal, so this is it.
//                        obsDate = TimeTools.copyToNearestHour(tTime);
//                        obsDate.set(Calendar.HOUR_OF_DAY, headerHour);
//                        obsDate.set(Calendar.MINUTE, headerMinute);
//                        break;
//                    }
//                }
//            }
//            if (obsDate != null) {
//                msgDate = obsDate;
//            }
//        }
//        return msgDate;
//    }
}
