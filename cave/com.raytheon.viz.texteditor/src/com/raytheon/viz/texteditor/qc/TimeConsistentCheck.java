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
package com.raytheon.viz.texteditor.qc;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.TextWarningConstants;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.texteditor.util.VtecUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Aug 25, 2011 10719      rferrel     ugcPtrn now local to file.
 * Mar 14, 2014  DR 17175  D. Friedman Get correct time zone from times.
 * </pre>
 * 
 * @version 1.0
 */
public class TimeConsistentCheck implements IQCCheck {
    private static final Pattern ugcPtrn = Pattern
            .compile("(((\\w{2}[CZ](\\d{3}-){1,}){1,})|(\\d{3}-){1,})(\\d{2})(\\d{2})(\\d{2})-");

    @Override
    public String runQC(String header, String body, String nnn) {
        String errorMsg = "";
        Matcher m = null;
        VtecObject vtec = VtecUtil.parseMessage(body);
        Calendar currenttime = Calendar
                .getInstance(TimeZone.getTimeZone("GMT"));

        if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
            if (body.contains(TEST_MESSAGE_LABEL)) {
                body = body.replaceAll(TEST_MESSAGE_LABEL, "");
            }
        }

        if (vtec != null) {
            currenttime.add(Calendar.MINUTE, 5);
            if (!vtec.getAction().equals("EXP")
                    && vtec.getEndTime().before(currenttime)) {
                errorMsg += "Product has expired or will expire in\n less than 5 minutes. (UGC line)\n";
                return errorMsg;
            }

            // Event ending time vs UGC
            m = ugcPtrn.matcher(body);
            if (m.find()) {
                int hour = Integer.parseInt(m.group(7));
                int minute = Integer.parseInt(m.group(8));
                currenttime.set(Calendar.HOUR_OF_DAY, hour);
                currenttime.set(Calendar.MINUTE, minute);
                currenttime.set(Calendar.SECOND, 0);
                currenttime.setTimeZone(TimeZone.getTimeZone("GMT"));
                if (currenttime.getTimeInMillis()
                        - vtec.getEndTime().getTimeInMillis() > 16 * 60 * 1000) {
                    errorMsg = "VTEC end time is 15 minutes older\n than UGC expiration times differ";
                    return errorMsg;
                }
            }

            // Event ending time (second bullet) vs Expiration
            m = secondBulletPtrn.matcher(body);
            if (m.find()) {
                TimeZone timeZone = TextWarningConstants.timeZoneShortNameMap
                        .get(m.group(4));
                if (timeZone == null) {
                    errorMsg += "Could not determine time zone in second bullet";
                    return errorMsg;
                }
                int am_pm = m.group(3).equals("AM") ? Calendar.AM : Calendar.PM;
                int minute = Integer.parseInt(m.group(2));
                int hour = Integer.parseInt(m.group(1)) == 12 ? 0 : Integer
                        .parseInt(m.group(1));

                Calendar secondBulletTime = new GregorianCalendar(timeZone);
                if (secondBulletTime.get(Calendar.AM_PM) == Calendar.PM
                        && am_pm == Calendar.AM) {
                    int month = secondBulletTime.get(Calendar.DAY_OF_MONTH);
                    secondBulletTime.set(Calendar.DAY_OF_MONTH, month + 1);
                }
                secondBulletTime.set(Calendar.HOUR, hour);
                secondBulletTime.set(Calendar.MINUTE, minute);
                secondBulletTime.set(Calendar.SECOND, 0);
                secondBulletTime.set(Calendar.AM_PM, am_pm);

                currenttime.setTimeInMillis(secondBulletTime.getTimeInMillis());

                if (currenttime.get(Calendar.HOUR_OF_DAY) != vtec.getEndTime()
                        .get(Calendar.HOUR_OF_DAY)) {
                    errorMsg += "VTEC and bullet expiration times differ,\n or no * UNTIL line found.\n";
                    return errorMsg;
                }

            } else if (!nnn.equalsIgnoreCase("SVS")
                    && !nnn.equalsIgnoreCase("FFS")
                    && !nnn.equalsIgnoreCase("FLW")
                    && !nnn.equalsIgnoreCase("FLS")
                    && !nnn.equalsIgnoreCase("MWS")) {
                errorMsg += "VTEC and bullet expiration times differ,\n or no * UNTIL line found.\n";
                return errorMsg;
            }

            // Event beginning time vs ending time
            if (vtec.getEndTime().before(vtec.getStartTime())) {
                errorMsg += "VTEC ending time is earlier than\n VTEC beginning time.\n";
                return errorMsg;
            }
        }

        m = thirdBulletPtrn.matcher(body);
        if (m.find()) {
            TimeZone timeZone = TextWarningConstants.timeZoneShortNameMap
                    .get(m.group(4));
            if (timeZone == null) {
                errorMsg += "Could not determine time zone in third bullet";
                return errorMsg;
            }
            int am_pm = m.group(3).equals("AM") ? Calendar.AM : Calendar.PM;
            int minute = Integer.parseInt(m.group(2));
            int hour = Integer.parseInt(m.group(1));
            if (hour == 12) {
                hour = 0;
            }

            Calendar thirdBullettime = new GregorianCalendar(timeZone);
            thirdBullettime.set(Calendar.HOUR, hour);
            thirdBullettime.set(Calendar.MINUTE, minute);
            thirdBullettime.set(Calendar.SECOND, 0);
            thirdBullettime.set(Calendar.AM_PM, am_pm);

            long issuetime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                    .getTimeInMillis();

            if (thirdBullettime.getTimeInMillis() - 60 * 1000 > issuetime) {
                errorMsg += "Event time is later than the MND\n issue time.\n";
            } else if (issuetime - thirdBullettime.getTimeInMillis() > 15 * 60 * 1000) {
                errorMsg += "The event time is more than 15 minutes\n earlier than the issue time.\n";
            }
        }
        return errorMsg;
    }

}
