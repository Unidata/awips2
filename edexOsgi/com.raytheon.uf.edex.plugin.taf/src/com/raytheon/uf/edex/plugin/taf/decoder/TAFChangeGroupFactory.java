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
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.taf.TAFParts;
import com.raytheon.uf.common.dataplugin.taf.TafConstants;
import com.raytheon.uf.common.dataplugin.taf.TafPeriod;
import com.raytheon.uf.common.dataplugin.taf.TafRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2008       1515 jkorman     Initial implementation to
 *                                     add 30 Hour tafs.
 * Nov 12, 2013 2546       bclement    added check for legacy valid time
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed TimeTools usage
 * May 15, 2014 3002       bgonzale    Moved common taf code to com.raytheon.uf.common.dataplugin.taf.
 * Apr 01, 2015 3722       rjpeter     Updated amd/corindicator to boolean flags.
 * Sep 21, 2015 4890       rferrel     Removal of Change Group from TafRecord and clean up.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TAFChangeGroupFactory {

    private static final String DAY = "(([012]\\d{1,1})|(3[01]))";

    private static final String HOUR = "(([01]\\d{1,1})|(2[0-4]))";

    private static final String MINUTE = "[0-5]\\d{1,1}";

    private static final String DAYHOUR = DAY + HOUR;

    private static final String ISSUE_TIME = DAY + HOUR + MINUTE + "Z";

    static final String VALID_TIME = DAYHOUR + "/" + DAYHOUR;

    static final String PROB = "PROB[34]0 " + DAYHOUR + "/" + DAYHOUR;

    static final String TEMPO = "TEMPO " + DAYHOUR + "/" + DAYHOUR;

    static final String PROBTEMPO = "PROB[34]0 TEMPO " + DAYHOUR + "/"
            + DAYHOUR;

    static final String BECMG = "BECMG " + DAYHOUR + "/" + DAYHOUR;

    static final String FM = "FM" + DAY + HOUR + MINUTE;

    private final Pattern PAT_ISSUE_TIME = Pattern.compile(ISSUE_TIME);

    private final Pattern PAT_VALID_TIME = Pattern.compile(VALID_TIME);

    private static final Pattern PAT_LEGACY_VALID_TIME = Pattern.compile("\\s"
            + DAY + HOUR + HOUR + "\\s");

    private final Pattern PAT_PROB = Pattern.compile(PROB);

    private final Pattern PAT_TEMPO = Pattern.compile(TEMPO);

    private final Pattern PAT_PROBTEMPO = Pattern.compile(PROBTEMPO);

    private final Pattern PAT_BECMG = Pattern.compile(BECMG);

    private final Pattern PAT_FM = Pattern.compile(FM);

    private Calendar issueTime = null;

    private String issueTimeString = null;

    private TafPeriod validPeriod = null;

    private String stationId = null;

    /**
     * 
     * @param tafData
     * @return
     */
    private List<Integer> findPositions(StringBuilder tafData) {
        List<Integer> locations = new ArrayList<Integer>();

        int startPos = 0;
        int currPos = 0;
        do {
            currPos = findPROB_TEMPO(tafData, startPos, locations);
            startPos = (currPos > 0) ? currPos : 0;
        } while (currPos > 0);

        startPos = 0;
        currPos = 0;
        do {
            currPos = findPROB(tafData, startPos, locations);
            startPos = (currPos > 0) ? currPos : 0;
        } while (currPos > 0);

        startPos = 0;
        currPos = 0;
        do {
            currPos = findTEMPO(tafData, startPos, locations);
            startPos = (currPos > 0) ? currPos : 0;
        } while (currPos > 0);

        startPos = 0;
        currPos = 0;
        do {
            currPos = findBECMG(tafData, startPos, locations);
            startPos = (currPos > 0) ? currPos : 0;
        } while (currPos > 0);

        startPos = 0;
        currPos = 0;
        do {
            currPos = findFM(tafData, startPos, locations);
            startPos = (currPos > 0) ? currPos : 0;
        } while (currPos > 0);

        Collections.sort(locations);

        startPos = 0;
        int stopPos = tafData.length();
        // Now we need to find the first position
        if (locations.size() > 2) {
            stopPos = locations.get(0);
        }
        String firstChunk = tafData.substring(startPos, stopPos);
        Matcher m = PAT_VALID_TIME.matcher(firstChunk);
        if (m.find()) {
            startPos = m.start();
            stopPos = m.end();
            locations.add(0, stopPos);
            locations.add(0, startPos);
        }

        return locations;
    }

    /**
     * 
     * @param tafData
     * @param locations
     * @return
     */
    private int findPROB_TEMPO(StringBuilder tafData, int startPos,
            List<Integer> locations) {
        int foundPos = -1;
        Matcher m = PAT_PROBTEMPO.matcher(tafData.substring(startPos));
        if (m.find()) {
            locations.add(m.start() + startPos);
            locations.add(m.end() + startPos);
            clearArea(tafData, m.start() + startPos, m.end() + startPos);
            foundPos = m.end() + 1 + startPos;
        }
        return foundPos;
    }

    /**
     * 
     * @param tafData
     * @param locations
     * @return
     */
    private int findPROB(StringBuilder tafData, int startPos,
            List<Integer> locations) {
        int foundPos = -1;
        Matcher m = PAT_PROB.matcher(tafData.substring(startPos));
        if (m.find()) {
            locations.add(m.start() + startPos);
            locations.add(m.end() + startPos);
            clearArea(tafData, m.start() + startPos, m.end() + startPos);
            foundPos = m.end() + 1 + startPos;
        }
        return foundPos;
    }

    /**
     * 
     * @param tafData
     * @param locations
     * @return
     */
    private int findTEMPO(StringBuilder tafData, int startPos,
            List<Integer> locations) {
        int foundPos = -1;
        Matcher m = PAT_TEMPO.matcher(tafData.substring(startPos));
        if (m.find()) {
            locations.add(m.start() + startPos);
            locations.add(m.end() + startPos);
            clearArea(tafData, m.start() + startPos, m.end() + startPos);
            foundPos = m.end() + 1 + startPos;
        }
        return foundPos;
    }

    /**
     * 
     * @param tafData
     * @param locations
     * @return
     */
    private int findBECMG(StringBuilder tafData, int startPos,
            List<Integer> locations) {
        int foundPos = -1;
        Matcher m = PAT_BECMG.matcher(tafData.substring(startPos));
        if (m.find()) {
            locations.add(m.start() + startPos);
            locations.add(m.end() + startPos);
            clearArea(tafData, m.start() + startPos, m.end() + startPos);
            foundPos = m.end() + 1 + startPos;
        }
        return foundPos;
    }

    /**
     * 
     * @param tafData
     * @param locations
     * @return
     */
    private int findFM(StringBuilder tafData, int startPos,
            List<Integer> locations) {
        int foundPos = -1;
        Matcher m = PAT_FM.matcher(tafData.substring(startPos));
        if (m.find()) {
            locations.add(m.start() + startPos);
            locations.add(m.end() + startPos);
            clearArea(tafData, m.start() + startPos, m.end() + startPos);
            foundPos = m.end() + 1 + startPos;
        }
        return foundPos;
    }

    /**
     * 
     * @param data
     * @param start
     * @param stop
     */
    private void clearArea(StringBuilder data, int start, int stop) {
        for (int i = start; i < stop; i++) {
            data.setCharAt(i, ' ');
        }
    }

    /**
     * 
     * @param tafData
     * @return
     */
    private List<TAFSubGroup> parse30HourTaf(WMOHeader wmo, String tafData)
            throws DecoderException {

        List<TAFSubGroup> groups = null;

        tafData = checkForLegacyFormat(wmo, tafData);

        List<Integer> locations = findPositions(new StringBuilder(tafData));

        if (locations.size() == 2) {
            int stop = locations.get(1);

            groups = new ArrayList<TAFSubGroup>();
            TAFSubGroup group = new TAFSubGroup();
            group.setChangeGroupHeader(tafData.substring(0, stop));
            group.setChangeGroupBody(tafData.substring(stop, tafData.length()));
            groups.add(group);
        } else if (locations.size() > 2) {
            int start = locations.get(0);
            int stop = locations.get(1);

            groups = new ArrayList<TAFSubGroup>();
            TAFSubGroup group = new TAFSubGroup();
            group.setChangeGroupHeader(tafData.substring(0, stop));

            int lastStop = stop;
            for (int i = 2; i < (locations.size() - 1); i += 2) {
                start = locations.get(i);

                if (lastStop > 0) {
                    group.setChangeGroupBody(tafData.substring(lastStop, start));
                    groups.add(group);
                    group = new TAFSubGroup();
                }
                stop = locations.get(i + 1);
                lastStop = stop;
                group.setChangeGroupHeader(tafData.substring(start, stop));

            }
            group.setChangeGroupBody(tafData.substring(lastStop,
                    tafData.length()));
            groups.add(group);
        }
        return groups;
    }

    /**
     * Convert from legacy TAF format for valid times (DDHHHH) to the current
     * extended format for valid times (DDHH/DDHH) if needed.
     * 
     * @param wmo
     * @param tafData
     * @return
     */
    protected String checkForLegacyFormat(WMOHeader wmo, String tafData) {
        Matcher m = PAT_LEGACY_VALID_TIME.matcher(tafData);
        boolean isLegacy = m.find();
        if (!isLegacy) {
            return tafData;
        }
        StringBuilder rval = new StringBuilder();
        int last = 0;
        do {
            int day1 = Integer.parseInt(m.group(1));
            int day2 = day1;
            int hr1 = Integer.parseInt(m.group(4));
            int hr2 = Integer.parseInt(m.group(7));
            if (hr2 == 24) {
                // legacy format uses 00 for valid start but 24 for valid end
                hr2 = 00;
            }
            if (hr2 <= hr1) {
                // valid time crosses midnight
                Calendar cal = wmo.getHeaderDate();
                if (cal == null) {
                    // no month information in header, assume this month
                    cal = TimeUtil.newCalendar(TimeUtil.GMT_TIME_ZONE);
                }
                // cal may be set to a day different than the valid start
                cal.set(Calendar.DAY_OF_MONTH, day1);
                // handles month roll over
                cal.add(Calendar.DAY_OF_MONTH, 1);
                day2 = cal.get(Calendar.DAY_OF_MONTH);
            }
            // +1 to include preceding white space
            rval.append(tafData.substring(last, m.start() + 1));
            rval.append(String
                    .format("%02d%02d/%02d%02d", day1, hr1, day2, hr2));
            // -1 to include following white space
            last = m.end() - 1;
        } while (m.find());
        // handle tail
        rval.append(tafData.substring(last));
        return rval.toString();
    }

    /**
     * This processes the initial taf group containing the taf issue time, any
     * COR/AMD information, and the taf valid period.
     * 
     * @param tafHeader
     */
    private boolean processTafHeader(TAFSubGroup tafHeader, WMOHeader wmoHeader) {
        // success = false by exception.
        boolean success = true;

        String issueTm = null;

        StringBuilder sb = new StringBuilder(tafHeader.getExtension());
        // The pattern regex needs a terminating space.
        sb.append(" ");

        Matcher matcher = TafConstants.REPORT_HEADER30.matcher(sb);
        if (matcher.find()) {
            stationId = matcher.group(TafSeparator.STATION_ID);
        }

        Matcher m = PAT_VALID_TIME.matcher(sb);
        if (m.find()) {
            sb.delete(m.start(), m.end());
        }

        m = PAT_ISSUE_TIME.matcher(sb);
        if (m.find()) {
            issueTm = sb.substring(m.start(), m.end());
            issueTimeString = issueTm;

            int iDay = TAFParser.cvtInt(issueTm.substring(0, 2));
            int iHour = TAFParser.cvtInt(issueTm.substring(2, 4));
            int iMin = TAFParser.cvtInt(issueTm.substring(4, 6));

            issueTime = TimeUtil.newGmtCalendar(wmoHeader.getYear(),
                    wmoHeader.getMonth(), wmoHeader.getDay());
            int sDay = issueTime.get(Calendar.DAY_OF_MONTH);
            if (sDay == iDay) {
                // In the same day so all is well.
                issueTime.set(Calendar.HOUR_OF_DAY, iHour);
                issueTime.set(Calendar.MINUTE, iMin);
                issueTime.set(Calendar.SECOND, 0);
                issueTime.set(Calendar.MILLISECOND, 0);
            } else {
                // Check if its the previous day
                issueTime.add(Calendar.DAY_OF_MONTH, -1);
                sDay = issueTime.get(Calendar.DAY_OF_MONTH);
                if (sDay == iDay) {

                    issueTime.set(Calendar.HOUR_OF_DAY, iHour);
                    issueTime.set(Calendar.MINUTE, iMin);
                    issueTime.set(Calendar.SECOND, 0);
                    issueTime.set(Calendar.MILLISECOND, 0);
                } else {
                    // Move forward to the next day
                    issueTime.add(Calendar.DAY_OF_MONTH, 2);
                    sDay = issueTime.get(Calendar.DAY_OF_MONTH);
                    if (sDay == iDay) {
                        // In the same day so all is well.
                        issueTime.set(Calendar.HOUR_OF_DAY, iHour);
                        issueTime.set(Calendar.MINUTE, iMin);
                        issueTime.set(Calendar.SECOND, 0);
                        issueTime.set(Calendar.MILLISECOND, 0);
                    } else {
                        // we are outside of a +/- one day window.
                        success = false;
                    }
                }
            }
        } else {
            // No issue time found, so we'll have to create one from
            // the WMOHeader data.
            issueTime = wmoHeader.getHeaderDate();
        }

        return success;
    }

    /**
     * 
     * @return
     */
    public TafRecord getTafRecord(WMOHeader wmoHeader, TAFParts tafParts)
            throws DecoderException {
        TafRecord record = null;

        // *********************
        String testData = tafParts.getTafHeader() + tafParts.getTafBody();
        // *********************

        List<TAFSubGroup> groups = parse30HourTaf(wmoHeader, testData);
        if (groups != null) {
            TafPeriod period = new TafPeriod();
            for (TAFSubGroup group : groups) {
                if ("INITIAL".equals(group.getChangeGroupHeader())) {
                    /*
                     * need to handle this group separately so we can set the
                     * TAF valid period.
                     */
                    if (!processTafHeader(group, wmoHeader)) {
                        break;
                    }
                    period.setStartDate(issueTime);

                    validPeriod = TafPeriod.copy(group.createPeriod(period));
                    break;
                }
            }
            if (validPeriod == null) {
                throw new DecoderException("No TAF validPeriod found for TAF: "
                        + testData);
            }

            record = new TafRecord();

            Calendar startDate = validPeriod.getStartDate();
            TimeRange range = new TimeRange(startDate, validPeriod.getEndDate());
            DataTime tafPeriod = new DataTime(startDate, range);

            record.setDataTime(tafPeriod);

            record.setStationId(stationId);

            if (tafParts.getTafHeader().indexOf(TafConstants.COR_IND) >= 0) {
                record.setCorIndicator(true);
            }
            if (tafParts.getTafHeader().indexOf(TafConstants.AMD_IND) >= 0) {
                record.setAmdIndicator(true);
            }

            record.setIssue_time(issueTime.getTime());
            if (issueTimeString == null) {
                issueTimeString = String.format("%1$td%1$tH%1$tMZ", issueTime);
            }
            record.setIssue_timeString(issueTimeString);
            record.setDataTime(new DataTime(issueTime.getTime().getTime(),
                    record.getDataTime().getValidPeriod()));
        }

        if (record == null) {
            throw new DecoderException("Unable to parse TAF: " + testData);
        }

        return record;
    }

}
