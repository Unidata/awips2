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

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.viz.vtec.VtecObject;
import com.raytheon.uf.viz.vtec.VtecUtil;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.TextWarningConstants;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 *                                     Initial creation
 * Aug 25, 2011  10719    rferrel      ugcPtrn now local to file.
 * Mar 14, 2014  17175    D. Friedman  Get correct time zone from times.
 * May 13, 2014  17177    Qinglu Lin   Updated runQC().
 * Mar 10, 2016  5411     randerso     Moved upper case conversion for QC checks
 *                                     into the specific checks that need it.
 * Nov 03, 2016  5934     randerso     Moved VtecObject and VtecUtil to a
 *                                     separate plugin.
 * Apr 25, 2017  6251     dgilling     Code cleanup.
 * May 19, 2017  6252     dgilling     Add exception case for DSW/SQW
 *                                     followups.
 *
 * </pre>
 */
public class TimeConsistentCheck implements IQCCheck {
    private static final Pattern UGC_PATTERN = Pattern.compile(
            "(((\\w{2}[CZ](\\d{3}-){1,}){1,})|(\\d{3}-){1,})(\\d{2})(\\d{2})(\\d{2})-");

    @Override
    public String runQC(String header, String body, String nnn) {
        body = body.toUpperCase();

        if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
            if (body.contains(QCCheckConstants.TEST_MESSAGE_LABEL)) {
                body = body.replaceAll(QCCheckConstants.TEST_MESSAGE_PATTERN,
                        StringUtils.EMPTY);
            }
        }

        VtecObject vtec = VtecUtil.parseMessage(body);
        Calendar currenttime = Calendar
                .getInstance(TimeZone.getTimeZone("GMT"));

        if (vtec != null) {
            currenttime.add(Calendar.MINUTE, 5);
            if (!vtec.getAction().equals("EXP")
                    && vtec.getEndTime().before(currenttime)) {
                return "Product has expired or will expire in\n less than 5 minutes. (UGC line)\n";
            }

            // Event ending time vs UGC
            Matcher m = UGC_PATTERN.matcher(body);
            if (m.find()) {
                int hour = Integer.parseInt(m.group(7));
                int minute = Integer.parseInt(m.group(8));
                currenttime.set(Calendar.HOUR_OF_DAY, hour);
                currenttime.set(Calendar.MINUTE, minute);
                currenttime.set(Calendar.SECOND, 0);
                currenttime.setTimeZone(TimeZone.getTimeZone("GMT"));
                if ((currenttime.getTimeInMillis()
                        - vtec.getEndTime().getTimeInMillis()) > (16 * 60
                                * 1000)) {
                    return "VTEC end time is 15 minutes older\n than UGC expiration times differ";
                }
            }

            // Event ending time (second bullet) vs Expiration
            String newBody = body.replaceAll("UNTIL NOON", "UNTIL 1200 PM");
            newBody = newBody.replaceAll("UNTIL MIDNIGHT", "UNTIL 1200 AM");
            m = QCCheckConstants.SECOND_BULLET_PATTERN.matcher(newBody);
            if (m.find()) {
                TimeZone timeZone = TextWarningConstants.timeZoneShortNameMap
                        .get(m.group(4));
                if (timeZone == null) {
                    return "Could not determine time zone in second bullet";
                }
                int am_pm = m.group(3).equals("AM") ? Calendar.AM : Calendar.PM;
                int minute = Integer.parseInt(m.group(2));
                int hour = Integer.parseInt(m.group(1)) == 12 ? 0
                        : Integer.parseInt(m.group(1));

                Calendar secondBulletTime = new GregorianCalendar(timeZone);
                if ((secondBulletTime.get(Calendar.AM_PM) == Calendar.PM)
                        && (am_pm == Calendar.AM)) {
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
                    return "VTEC and bullet expiration times differ,\n or no * UNTIL line found.\n";
                }

            } else if (!nnn.equalsIgnoreCase("SVS")
                    && !nnn.equalsIgnoreCase("FFS")
                    && !nnn.equalsIgnoreCase("FLW")
                    && !nnn.equalsIgnoreCase("FLS")
                    && !nnn.equalsIgnoreCase("MWS")
                    && ((nnn.equalsIgnoreCase("DSW")
                            || nnn.equalsIgnoreCase("SQW"))
                            && "NEW".equals(vtec.getAction()))) {
                return "VTEC and bullet expiration times differ,\n or no * UNTIL line found.\n";

            }

            // Event beginning time vs ending time
            if (vtec.getEndTime().before(vtec.getStartTime())) {
                return "VTEC ending time is earlier than\n VTEC beginning time.\n";
            }
        }

        Matcher m = QCCheckConstants.THIRD_BULLET_PATTERN.matcher(body);
        if (m.find()) {
            TimeZone timeZone = TextWarningConstants.timeZoneShortNameMap
                    .get(m.group(4));
            if (timeZone == null) {
                return "Could not determine time zone in third bullet";
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

            if ((thirdBullettime.getTimeInMillis() - (60 * 1000)) > issuetime) {
                return "Event time is later than the MND\n issue time.\n";
            } else if ((issuetime - thirdBullettime.getTimeInMillis()) > (15
                    * 60 * 1000)) {
                return "The event time is more than 15 minutes\n earlier than the issue time.\n";
            }
        }

        return StringUtils.EMPTY;
    }
}
