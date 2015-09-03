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
package com.raytheon.uf.edex.plugin.taf.decoder;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.taf.ChangeGroup;
import com.raytheon.uf.common.dataplugin.taf.TAFParts;
import com.raytheon.uf.common.dataplugin.taf.TafConstants;
import com.raytheon.uf.common.dataplugin.taf.TafPeriod;
import com.raytheon.uf.common.dataplugin.taf.TafRecord;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * The TAF parser accepts a potential TAF report and attempts to parse and
 * decode various information from that report.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 20080424     1001        jkorman     Initial implementation.
 * 9/4/2008     1444        grichard    Import constants from TafConstants class.
 * Oct 21, 2008       1515  jkorman     Added 30 Hour capability changes.
 * Feb 27, 2013 1638        mschenke    Moved ObStationDao to edex pointdata plugin
 * May 14, 2014 2536        bclement    moved WMO Header to common, removed TimeTools usage
 * May 15, 2014 3002        bgonzale    Moved common taf code to com.raytheon.uf.common.dataplugin.taf.
 *                                      Refactored Strings to Patterns in TafConstants.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TAFParser {

    private final Log logger = LogFactory.getLog(getClass());

    // COR hhmm
    // AMD hhmm
    // 01234567
    private static final String AMD_COR_TIME = "(" + TafConstants.AMD_IND + "|"
            + TafConstants.COR_IND
            + ") \\d{4}";

    private static final int HOUR_START = 4;

    private static final int MINUTE_START = 6;

    private TAFParts tafParts = null;

    private Calendar issueDate = null;

    TafPeriod validPeriod = null;

    private boolean isCOR = false;

    private boolean isAMD = false;

    private TafRecord record = null;

    private ArrayList<String> tafGroups = null;

    /**
     * 
     * @param tafParts
     * @param header
     * @throws DecoderException
     */
    public TAFParser(TAFParts tafParts, WMOHeader header)
            throws DecoderException {
        internalParse(tafParts, header);
    }

    /**
     * 
     * @return
     */
    public TafRecord getDecodedRecord() {
        return record;
    }

    /**
     * 
     * @param tafParts
     * @param header
     * @throws DecoderException
     */
    private void internalParse(TAFParts tafParts, WMOHeader header)
            throws DecoderException {

        this.tafParts = tafParts;

        TAFChangeGroupFactory fact = new TAFChangeGroupFactory();
        record = fact.getTafRecord(header, tafParts);
        if (record == null) {
            parseHeader(header);
            if (record != null) {
                StringBuilder sb = new StringBuilder(header.getWmoHeader());
                sb.append("\n");
                sb.append(separate_groups(tafParts));
                record.setMessageData(sb.toString());

                if ((tafGroups != null) && (tafGroups.size() > 0)) {

                    ArrayList<ChangeGroup> groups = new ArrayList<ChangeGroup>();
                    ArrayList<ChangeGroup> cGroups = new ArrayList<ChangeGroup>();
                    Set<ChangeGroup> groupSet = new HashSet<ChangeGroup>();
                    int currGroup = 0;
                    for (String grp : tafGroups) {

                        Calendar cStart = TimeUtil.newCalendar(validPeriod
                                .getStartDate());
                        Calendar cStop = TimeUtil.newCalendar(validPeriod
                                .getEndDate());

                        TafPeriod tPeriod = new TafPeriod(cStart, cStop);
                        ChangeGroup group = new ChangeGroup(grp, tPeriod);
                        if (group != null) {
                            group.setSequenceId(currGroup++);
                            group.setParentID(record);
                            groups.add(group);
                            groupSet.add(group);
                        }
                    }

                    if (groups.get(0).getChange_indicator() == null) {
                        groups.get(0).setChange_indicator("INITIAL");
                    } else {
                        record = null;
                        logger.error("Invalid TAF-Initial group not found");
                        return;
                    }

                    for (ChangeGroup group : groups) {
                        if (isChangeGroup(group.getChange_indicator())) {
                            cGroups.add(group);
                        }
                    }

                    if (cGroups.size() > 1) {
                        ChangeGroup group1 = null;
                        ChangeGroup group2 = null;

                        TafPeriod period1 = null;
                        TafPeriod period2 = null;
                        for (int i = 0; i < cGroups.size() - 1; i++) {
                            group1 = cGroups.get(i);
                            group2 = cGroups.get(i + 1);

                            period1 = group1.getTafChangePeriod();
                            period2 = group2.getTafChangePeriod();

                            period1.setEndDate(TimeUtil.newCalendar(period2
                                    .getStartDate()));

                        }
                        period2.setEndDate((Calendar) validPeriod.getEndDate()
                                .clone());
                    }
                    record.setChangeGroups(groupSet);
                }
            }
        }

        if (record != null) {
            String s = header.getYYGGgg();
            Calendar bulletinTime = transformDate(s, header);
            if (bulletinTime != null) {
                record.setBulletin_time(bulletinTime.getTime());
            }

            record.setTafText(tafParts.getTafHeader()
                    + formatTAF(tafParts.getTafBody()));
            record.setWmoHeader(header.getWmoHeader());

            if (isAMD) {
                record.setAmdIndicator(TafConstants.AMD_IND);
            }
            if (isCOR) {
                record.setCorIndicator(TafConstants.COR_IND);
            }

            ObStation location = null;
            try {
                location = new ObStationDao()
                        .queryByIcao(record.getStationId());
            } catch (DataAccessLayerException e) {
                logger.info("Error querying for ICAO [" + record.getStationId()
                        + "]");
            }

            if (location == null) {
                logger.info("Station id not found [" + record.getStationId()
                        + "]");
                record = null;
            } else {
                record.setLocation(location);
            }
        }
    }

    /**
     * Extracts when the TAF was issued. If the TAF was issued without an issue
     * time, an attempt is made to create an issue time from the valid time.
     */
    private void parseHeader(WMOHeader header) {

        record = new TafRecord();


        Matcher matcher = TafConstants.REPORT_HEADER.matcher(tafParts
                .getTafHeader());
        if (matcher.find()) {

            String s = matcher.group(TafSeparator.STATION_ID);
            if (s != null) {
                record.setStationId(s);
            } else {
                record = null;
                return;
            }

            s = matcher.group(TafSeparator.ISSUE_TIME);
            if (s != null) {
                issueDate = transformDate(s, header);
                issueDate.set(Calendar.MILLISECOND, 0);
            } else {
                // This is probably a military taf and doesn't have an issue
                // date-time. So we need to create one.
                // Create an issue time from the valid period data.
                StringBuilder sb = new StringBuilder();
                sb.append(matcher.group(TafSeparator.VALID_TIME)
                        .substring(0, 2));
                sb.append(matcher.group(TafSeparator.VALID_TIME)
                        .substring(2, 4));
                sb.append("00Z");
                issueDate = transformDate(sb.toString(), header);
                issueDate.set(Calendar.MILLISECOND, 0);
            }
            // Now check for an amendment or correction time!
            // This has been a backup for military tafs.

            checkAMDCOR(issueDate, tafParts.getTafBody());
            s = tafParts.getTafHeader();
            if (!isCOR) {
                isCOR = s.indexOf(TafConstants.COR_IND) >= 0;
            }
            if (!isAMD) {
                isAMD = s.indexOf(TafConstants.AMD_IND) >= 0;
            }

            record.setIssue_time(issueDate.getTime());
            record.setIssue_timeString(issueDate.toString());

            validPeriod = TafPeriod.parseValidPeriod(matcher.group(6), header);
            if (validPeriod != null) {

                Calendar startDate = validPeriod.getStartDate();
                TimeRange range = new TimeRange(startDate,
                        validPeriod.getEndDate());
                DataTime tafPeriod = new DataTime(issueDate, range);

                record.setDataTime(tafPeriod);
            }
        }
    }

    /**
     * Given a string in the form of DDHHMMZ, convert that string to a calendar
     * referenced to the current system calendar.
     * 
     * @param issueDateString
     *            Date/Time string to convert.
     * @return The created calendar.
     */
    private Calendar transformDate(String issueDateString, WMOHeader header) {

        Calendar tDate = TimeUtil.newGmtCalendar(header.getYear(),
                header.getMonth(), header.getDay());

        int maxDay = tDate.getActualMaximum(Calendar.DAY_OF_MONTH);

        int currDay = tDate.get(Calendar.DAY_OF_MONTH);

        int day = Integer.parseInt(issueDateString.substring(0, 2).trim());

        int dayDelta = currDay - day;

        if (dayDelta < 0) {
            if ((maxDay + dayDelta) == 1) {
                tDate.add(Calendar.DAY_OF_MONTH, -1);
            } else {
                tDate.add(Calendar.DAY_OF_MONTH, -1);
                tDate.set(Calendar.DAY_OF_MONTH, day);
            }
            logger.debug(" Less " + TafPeriod.formatDate(tDate));
        } else if (dayDelta > 0) {
            if ((maxDay - dayDelta) == 1) {
                tDate.add(Calendar.DAY_OF_MONTH, 1);
            } else {
                tDate.set(Calendar.DAY_OF_MONTH, currDay);
            }
            logger.debug(" Greater " + TafPeriod.formatDate(tDate));
        }

        int hour = Integer.parseInt(issueDateString.substring(2, 4).trim());
        int minute = Integer.parseInt(issueDateString.substring(4, 6).trim());

        tDate.set(Calendar.HOUR_OF_DAY, hour);
        tDate.set(Calendar.MINUTE, minute);
        tDate.set(Calendar.SECOND, 0);

        return tDate;
    }

    /**
     * Check if the given TAF body contains amendment/correction time
     * information. If so, use the supplied issue time to create an issue time
     * based on the AMD/COR time information.
     * 
     * @param issueDate
     *            An issue time for the TAF.
     * @param tafBody
     *            The TAF body information.
     * @return A calendar containing a possible new issue time.
     */
    private Calendar checkAMDCOR(Calendar issueDate, String tafBody) {
        Pattern amdcor = Pattern.compile(AMD_COR_TIME);
        Matcher m = amdcor.matcher(tafBody);
        if (m.find()) {
            String s = m.group();
            int hour = Integer.parseInt(s.substring(HOUR_START, HOUR_START + 2)
                    .trim());
            int minute = Integer.parseInt(s.substring(MINUTE_START,
                    MINUTE_START + 2).trim());

            int issueHour = issueDate.get(Calendar.HOUR_OF_DAY);

            if (hour == issueHour) {
                issueDate.set(Calendar.MINUTE, minute);
            } else if (hour < issueHour) {
                // still same day!
                issueDate.set(Calendar.HOUR_OF_DAY, hour);
                issueDate.set(Calendar.MINUTE, minute);
            } else {
                int delta = hour - issueHour;

                if (delta < 3) {
                    // Same day,
                    issueDate.set(Calendar.HOUR_OF_DAY, hour);
                    issueDate.set(Calendar.MINUTE, minute);
                } else if (delta > 22) {
                    // Go back one day
                    issueDate.add(Calendar.DAY_OF_MONTH, -1);
                    issueDate.set(Calendar.HOUR_OF_DAY, hour);
                    issueDate.set(Calendar.MINUTE, minute);
                } else {
                    // more than +/- an hour is probably an error
                    logger.info("COR/AMD time out of range " + s);
                    issueDate = null;
                }
            }
            isCOR = s.indexOf(TafConstants.COR_IND) >= 0;
            isAMD = s.indexOf(TafConstants.AMD_IND) >= 0;
        }
        return issueDate;
    }

    /**
     * Parse the TAF body into its separate change groups.
     * 
     * @param parts
     *            The TAF report header and body object.
     * @return The TAF report reformatted.
     */
    private String separate_groups(TAFParts parts) {

        StringBuilder sb = new StringBuilder(parts.getTafBody());
        for (int i = 0; i < sb.length(); i++) {
            if (sb.charAt(i) == '\r') {
                sb.setCharAt(i, ' ');
            }
        }
        for (int i = 0; i < sb.length(); i++) {
            if (sb.charAt(i) == ' ') {
                i++;
                for (; i < sb.length();) {
                    if (sb.charAt(i) == ' ') {
                        sb.deleteCharAt(i);
                    } else {
                        break;
                    }
                }
            }
        }

        String reportBody = sb.toString();

        Matcher m = TafConstants.CHANGE_GROUP_EXP.matcher(reportBody);

        // Collect all of the start positions
        ArrayList<Integer> positions = new ArrayList<Integer>();
        positions.add(0);
        while (m.find()) {
            positions.add(m.start());
        }
        // Add the report length as the last "start" position.
        positions.add(reportBody.length());
        Collections.sort(positions);

        tafGroups = new ArrayList<String>();

        sb = new StringBuilder(parts.getTafHeader());
        sb.append(" ");

        String indent = "";
        for (int i = 0; i < positions.size() - 1; i++) {

            int start = positions.get(i);
            int end = positions.get(i + 1);

            if (i > 0) {
                sb.append("\n");
                sb.append(indent);
            }

            String group = reportBody.substring(start, end).trim();
            tafGroups.add(group);
            sb.append(group);
            if (i == 0) {
                indent = "     ";
            }
        }
        return sb.toString();
    }

    /**
     * 
     * @param groupValue
     * @return
     */
    public static boolean isChangeGroup(String groupValue) {

        boolean isTemp = TafConstants.CG_FM.equals(groupValue);
        isTemp = isTemp || TafConstants.CG_BECMG.equals(groupValue);
        isTemp = isTemp || TafConstants.CG_INITIAL.equals(groupValue);
        return isTemp;
    }

    /**
     * 
     * @param groupValue
     * @return
     */
    public static boolean isTempGroup(String groupValue) {

        boolean isTemp = TafConstants.CG_TEMPO.equals(groupValue);
        isTemp = isTemp || TafConstants.CG_PROB_TEMPO.equals(groupValue);
        isTemp = isTemp || TafConstants.CG_PROB.equals(groupValue);
        return isTemp;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static Integer cvtInt(String value) {
        int val = 0;
        for (int i = 0; i < value.length(); i++) {
            val *= 10;
            int v = Integer.parseInt(value.substring(i, i + 1));
            val += v;
        }
        return val;
    }

    private static String formatTAF(String taf) {
        StringBuilder sb = new StringBuilder(taf);
        int n = 0;
        String[] find = { "\r " + TafConstants.CG_FM,
                "\r " + TafConstants.CG_BECMG, "\r " + TafConstants.CG_TEMPO,
                "\r " + TafConstants.CG_PROB, };
        String[] replace = { "\r     " + TafConstants.CG_FM,
                "\r     " + TafConstants.CG_BECMG,
                "\r     " + TafConstants.CG_TEMPO,
                "\r     " + TafConstants.CG_PROB, };
        for (int i = 0; i < find.length; i++) {
            while ((n = sb.indexOf(find[i])) >= 0) {
                sb.replace(n, n + find[i].length(), replace[i]);
            }
        }
        Pattern p = Pattern.compile("\\r [^ ]");
        Matcher m = p.matcher(sb);
        while (m.find()) {
            sb.replace(m.start(), m.end() - 1, "\r      ");
            m = p.matcher(sb);
        }
        return sb.toString();
    }

    public static final void main(String[] args) {

        String taf = "TAF AMD KSEA 280902Z 2809/2912 18009KT P6SM SCT008 OVC015\r TEMPO 2809/2810"
                + "5SM -RA BR OVC008\r FM281000 19010KT P6SM -RA SCT008 OVC015"
                + "\r TEMPO 2810/2814 4SM -RA OVC008\r FM281400 19011KT 4SM RA BKN007"
                + " OVC015\r FM281900 20012G18KT 5SM -RA BR OVC010\r FM290000 20011KT"
                + "P6SM VCSH SCT008 BKN015 OVC025=";

        System.out.println(taf);
        System.out.println("---------------------------------------------");
        System.out.println(formatTAF(taf));
    }

}
