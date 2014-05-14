/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.nctaf.decoder;

import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.AMD_IND;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.COR_IND;
import static gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafConstants.REPORT_HEADER30;
import static gov.noaa.nws.ncep.edex.plugin.nctaf.decoder.NcTafParser.cvtInt;
import static gov.noaa.nws.ncep.edex.plugin.nctaf.decoder.NcTafSeparator.STATION_ID;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafBulletinRecord;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafChangeGroup;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafPeriod;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.exception.DecoderException;
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
 * 09/09/2011   458		   sgurung	   Initial Creation from Raytheon's taf plugin
 * 09/23/2011   458		   sgurung	   Converted to HDF5
 * 10/26/2011              sgurung     Copy the omitted conditions from immediate previous change group
 * 									   (for TEMPO, PROB TEMPO or PROB groups)
 * 11/03/2011              sgurung     Split taf data using "=" when multiple records are combined into one (occurs rarely)
 * 									   Fixed a bug while copying the omitted conditions from immediate previous change group
 * 11/04/2011              sgurung     Create change groups from newTafData 
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed TimeTools usage
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class NcTafChangeGroupFactory {

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

    private final Pattern PAT_PROB = Pattern.compile(PROB);

    private final Pattern PAT_TEMPO = Pattern.compile(TEMPO);

    private final Pattern PAT_PROBTEMPO = Pattern.compile(PROBTEMPO);

    private final Pattern PAT_BECMG = Pattern.compile(BECMG);

    private final Pattern PAT_FM = Pattern.compile(FM);

    private Calendar issueTime = null;

    private String issueTimeString = null;

    private NcTafPeriod validPeriod = null;

    private String stationId = null;

    private boolean isCOR = false;

    private boolean isAMD = false;

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
        Matcher m = PAT_VALID_TIME
                .matcher(tafData.substring(startPos, stopPos));
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
    private List<NcTafSubGroup> parse30HourTaf(String tafData)
            throws DecoderException {

        List<NcTafSubGroup> groups = null;
        
        // sometimes the taf messages are not separated into records correctly 
        // (multiple taf records are combined into one, flaw in raw data?)
        // "=" represents the end of a forecast, so split the tafData so that 
        // we get the change groups only for the first one, ignoring the remaining
        String[] newTafData = tafData.split("=");
        tafData = newTafData[0];

        List<Integer> locations = findPositions(new StringBuilder(tafData));

        if (locations.size() == 2) {
            int stop = locations.get(1);

            groups = new ArrayList<NcTafSubGroup>();
            NcTafSubGroup group = new NcTafSubGroup();
            group.setChangeGroupHeader(tafData.substring(0, stop));
            group.setChangeGroupBody(tafData.substring(stop, tafData.length()));
            groups.add(group);
        } else if (locations.size() > 2) {
            int start = locations.get(0);
            int stop = locations.get(1);

            groups = new ArrayList<NcTafSubGroup>();
            NcTafSubGroup group = new NcTafSubGroup();
            group.setChangeGroupHeader(tafData.substring(0, stop));

            int lastStop = stop;
            for (int i = 2; i < locations.size() - 1; i += 2) {
                start = locations.get(i);

                if (lastStop > 0) {
                    group.setChangeGroupBody(tafData.substring(lastStop, start));
                    groups.add(group);
                    group = new NcTafSubGroup();
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
     * This processes the initial taf group containing the taf issue time, any
     * COR/AMD information, and the taf valid period.
     * 
     * @param tafHeader
     */
    private boolean processTafHeader(NcTafSubGroup tafHeader, WMOHeader wmoHeader) {
        // success = false by exception.
        boolean success = true;

        String issueTm = null;

        StringBuilder sb = new StringBuilder(tafHeader.getExtension());
        // The pattern regex needs a terminating space.
        sb.append(" ");
        Pattern headerPattern = Pattern.compile(REPORT_HEADER30);

        Matcher matcher = headerPattern.matcher(sb);
        if (matcher.find()) {
            stationId = matcher.group(STATION_ID);
        }

        Matcher m = PAT_VALID_TIME.matcher(sb);
        if (m.find()) {
            sb.delete(m.start(), m.end());
        }

        m = PAT_ISSUE_TIME.matcher(sb);
        if (m.find()) {
            issueTm = sb.substring(m.start(), m.end());
            issueTimeString = issueTm;

            int iDay = cvtInt(issueTm.substring(0, 2));
            int iHour = cvtInt(issueTm.substring(2, 4));
            int iMin = cvtInt(issueTm.substring(4, 6));

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
            int iDay = wmoHeader.getDay();
            int iHour = wmoHeader.getHour();
            int iMin = wmoHeader.getMinute();

            issueTime = TimeUtil.newGmtCalendar(wmoHeader.getYear(),
                    wmoHeader.getMonth(), wmoHeader.getDay());
            issueTime.add(Calendar.DAY_OF_MONTH, -1);
            for (int i = 0; i < 3; i++) {
                int sDay = issueTime.get(Calendar.DAY_OF_MONTH);
                if (sDay == iDay) {
                    issueTime.set(Calendar.HOUR_OF_DAY, iHour);
                    issueTime.set(Calendar.MINUTE, iMin);
                    issueTime.set(Calendar.SECOND, 0);
                    issueTime.set(Calendar.MILLISECOND, 0);
                    success = true;
                    break;
                }
            }
        }

        return success;
    }

    /**
     * 
     * @param c
     * @return
     */
    public static String fmtTime(Calendar c) {
        String fmt = "%1$tY%<tm%<te%<tH%<tM%<tS";
        return String.format(fmt, c);
    }

    /**
     * 
     * @return
     */
    public NcTafRecord getTafRecord(WMOHeader wmoHeader, NcTafParts tafParts)
            throws DecoderException {
        NcTafRecord record = null;

        // *********************
        String testData = tafParts.getTafHeader() + tafParts.getTafBody();
        // *********************

        List<NcTafSubGroup> groups = parse30HourTaf(testData);
        if (groups != null) {
            List<NcTafChangeGroup> changeGroups = null;

            Set<NcTafChangeGroup> groupSet = new HashSet<NcTafChangeGroup>();
            NcTafPeriod period = new NcTafPeriod();
            for (NcTafSubGroup group : groups) {
                if ("INITIAL".equals(group.getChangeGroupHeader())) {
                    // need to handle this group separately so we can
                    // set the TAF valid period.
                    if (!processTafHeader(group, wmoHeader)) {
                        System.out.println("processHeader failed");
                        break;
                    }
                    period.setStartDate(issueTime);
                    NcTafChangeGroup c = group.toChangeGroup(period);

                    changeGroups = new ArrayList<NcTafChangeGroup>();
                    changeGroups.add(c);
                    // groupSet.add(c);
                    validPeriod = NcTafPeriod.copy(c.getTafChangePeriod());
                    break;
                }
            }
            if (validPeriod == null) {
                throw new DecoderException("No TAF validPeriod found");
            }

            record = new NcTafRecord();

            Calendar startDate = validPeriod.getStartDate();
            TimeRange range = new TimeRange(startDate, validPeriod.getEndDate());
            DataTime tafPeriod = new DataTime(startDate, range);

            record.setDataTime(tafPeriod);
            int currGroup = 0;
            ArrayList<NcTafChangeGroup> cGroups = new ArrayList<NcTafChangeGroup>();
            if (changeGroups != null) {
                for (NcTafSubGroup group : groups) {
                    NcTafChangeGroup c = group.toChangeGroup(period);
                    changeGroups.add(c);
                    c.setSequenceId(currGroup++);
                    groupSet.add(c);
                    // only add BECMG, FM, and the INITIAL group to the change
                    // group
                    // list.
                    if (NcTafParser.isChangeGroup(c.getChange_indicator())) {
                        cGroups.add(c);
                    }
                }
            }
            // ensure that all group parentIDs are set.
            for (NcTafChangeGroup c : groupSet) {
                c.setParentID(record);
            }

            record.setStationId(stationId);

            if (isCOR || (tafParts.getTafHeader().indexOf(COR_IND) >= 0)) {
                record.setCorIndicator("COR");
            }
            if (isAMD || (tafParts.getTafHeader().indexOf(AMD_IND) >= 0)) {
                record.setAmdIndicator("AMD");
            }

            if (cGroups.size() > 1) {
                NcTafChangeGroup group1 = null;
                NcTafChangeGroup group2 = null;

                NcTafPeriod period1 = null;
                NcTafPeriod period2 = null;
                // Ensure that the change group end time is set for each group.
                for (int i = 0; i < cGroups.size() - 1; i++) {
                    group1 = cGroups.get(i);
                    group2 = cGroups.get(i + 1);

                    period1 = group1.getTafChangePeriod();
                    period2 = group2.getTafChangePeriod();

                    period1.setEndDate((Calendar) period2.getStartDate()
                            .clone());

                }
                // The last group gets the TAF end datetime.
                period2.setEndDate((Calendar) validPeriod.getEndDate().clone());
            }

            record.setIssue_time(issueTime.getTime());
            record.setIssue_timeString(issueTimeString);
            record.setDataTime(new DataTime(issueTime.getTime().getTime(),
                    record.getDataTime().getValidPeriod()));
            //record.setChangeGroups(groupSet);
        }

        return record;
    }
    
    /**
     * 
     * @return
     */
    public NcTafBulletinRecord getTafBulletinRecord(WMOHeader wmoHeader, NcTafParts tafParts)
            throws DecoderException {
        NcTafBulletinRecord record = null;

        // *********************
        String testData = tafParts.getTafHeader() + tafParts.getTafBody();
        // *********************

        List<NcTafSubGroup> groups = parse30HourTaf(testData);
        if (groups != null) {
            List<NcTafChangeGroup> changeGroups = null;

            Set<NcTafChangeGroup> groupSet = new LinkedHashSet<NcTafChangeGroup>();
            NcTafPeriod period = new NcTafPeriod();
            for (NcTafSubGroup group : groups) {
                if ("INITIAL".equals(group.getChangeGroupHeader())) {
                    // need to handle this group separately so we can
                    // set the TAF valid period.
                    if (!processTafHeader(group, wmoHeader)) {
                        System.out.println("processHeader failed");
                        break;
                    }
                    period.setStartDate(issueTime);
                    NcTafChangeGroup c = group.toChangeGroup(period);

                    changeGroups = new ArrayList<NcTafChangeGroup>();
                    changeGroups.add(c);
                    // groupSet.add(c);
                    validPeriod = NcTafPeriod.copy(c.getTafChangePeriod());
                    break;
                }
            }
            if (validPeriod == null) {
                throw new DecoderException("No TAF validPeriod found");
            }

            record = new NcTafBulletinRecord();
            record.setTafValidPeriod(validPeriod);

            Calendar startDate = validPeriod.getStartDate();
            TimeRange range = new TimeRange(startDate, validPeriod.getEndDate());
            DataTime tafPeriod = new DataTime(startDate, range);

            record.setDataTime(tafPeriod);
            int currGroup = 0;
            ArrayList<NcTafChangeGroup> cGroups = new ArrayList<NcTafChangeGroup>();
            if (changeGroups != null) {
                for (NcTafSubGroup group : groups) {
                    NcTafChangeGroup c = group.toChangeGroup(period);
                    changeGroups.add(c);
                    c.setSequenceId(currGroup++);
                    groupSet.add(c);                    
                    
                    // only add BECMG, FM, and the INITIAL group to the change
                    // group
                    // list.
                    if (NcTafParser.isChangeGroup(c.getChange_indicator())) {
                        cGroups.add(c);
                    }  
                    
                    // if group equals TEMPO, PROB TEMPO or PROB, copy the omitted conditions  
                    // (such as wind, gust, visibility, skycover etc.) from immediate previous change group
                    if (NcTafParser.isTempGroup(c.getChange_indicator()) && changeGroups.size() >= 2) {
                    	NcTafChangeGroup prevChgGroup = changeGroups.get(changeGroups.size()-2);                    	
                    	copyChangeGroupData(c, prevChgGroup);                      	
                    }                   
                   
                }
            }
         
            record.setStationId(stationId);

            if (isCOR || (tafParts.getTafHeader().indexOf(COR_IND) >= 0)) {
                record.setCorIndicator("COR");
            }
            if (isAMD || (tafParts.getTafHeader().indexOf(AMD_IND) >= 0)) {
                record.setAmdIndicator("AMD");
            }

            if (cGroups.size() > 1) {
                NcTafChangeGroup group1 = null;
                NcTafChangeGroup group2 = null;

                NcTafPeriod period1 = null;
                NcTafPeriod period2 = null;
                // Ensure that the change group end time is set for each group.
                for (int i = 0; i < cGroups.size() - 1; i++) {
                    group1 = cGroups.get(i);
                    group2 = cGroups.get(i + 1);

                    period1 = group1.getTafChangePeriod();
                    period2 = group2.getTafChangePeriod();

                    period1.setEndDate((Calendar) period2.getStartDate()
                            .clone());

                }
                // The last group gets the TAF end datetime.
                period2.setEndDate((Calendar) validPeriod.getEndDate().clone());
            }

            record.setIssue_time(issueTime.getTime());
            record.setIssue_timeString(issueTimeString);
            record.setDataTime(new DataTime(issueTime.getTime().getTime(),
                    record.getDataTime().getValidPeriod()));
            record.setChangeGroups(groupSet);
        }

        return record;
    }
    
    private NcTafChangeGroup copyChangeGroupData(NcTafChangeGroup c, NcTafChangeGroup prevChgGroup) {
    	
    	if (c.getAltim_in_hg() == null || (c.getAltim_in_hg() != null && c.getAltim_in_hg().trim().length() == 0)) {
    		c.setAltim_in_hg(prevChgGroup.getAltim_in_hg());
    	}
    	if (c.getMax_temp_c() == null || c.getMax_temp_c() == IDecoderConstantsN.NEGATIVE_INTEGER_MISSING) {
    		c.setMax_temp_c(prevChgGroup.getMax_temp_c());
    	}
    	if (c.getMin_temp_c() == null || c.getMin_temp_c() == IDecoderConstantsN.NEGATIVE_INTEGER_MISSING) {
    		c.setMin_temp_c(prevChgGroup.getMin_temp_c());
    	}
    	if (c.getRemarks() == null || (c.getRemarks()!=null && c.getRemarks().trim().length()==0)) {
    		c.setRemarks(prevChgGroup.getRemarks());
    	}
    	
    	c.setVert_vis_ft(prevChgGroup.getVert_vis_ft());    	
    	c.setVisibility_mi(prevChgGroup.getVisibility_mi());    	
    	c.setWind_dir_degrees(prevChgGroup.getWind_dir_degrees());
    	c.setWind_gust_kt(prevChgGroup.getWind_gust_kt());
    	c.setWind_speed_kt(prevChgGroup.getWind_speed_kt());    	
    	c.setWind_shear_dir_degrees(prevChgGroup.getWind_shear_dir_degrees());
    	c.setWind_shear_hgt_ft_agl(prevChgGroup.getWind_shear_hgt_ft_agl());
    	c.setWind_shear_speed_kt(prevChgGroup.getWind_shear_speed_kt());
    	
    	if (c.getSky_cover() == null) {
    		c.setSky_cover(prevChgGroup.getSky_cover());
    	} 
    	
    	if (c.getWeather() == null) {
    		c.setWeather(prevChgGroup.getWeather());
    	} 
    	
    	if (c.getIcing_layers() == null) {
    		c.setIcing_layers(prevChgGroup.getIcing_layers());
    	}
    	
    	if (c.getTurbulence_layers() == null) {
    		c.setTurbulence_layers(prevChgGroup.getTurbulence_layers());
    	}
    	
    	if (c.getTemp_forecasts() == null) {
    		c.setTemp_forecasts(prevChgGroup.getTemp_forecasts());
    	}
    	
    	return c;
    }
}
