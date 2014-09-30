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

package com.raytheon.edex.plugin.obs.metar;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.obs.metar.util.VisibilityParser;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.dataplugin.obs.metar.util.SkyCover;
import com.raytheon.uf.common.dataplugin.obs.metar.util.WeatherCondition;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;

/**
 * Decoder implementation for metar plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                     
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 14, 2007 139         bphillip    Initial creation
 * Oct 29, 2007 505         jkorman     Changed setting of DataTime from refhour
 *                                      to observation time.
 * Nov 28, 2007 575         jkorman     Added future obs time threshold check in
 *                                      decode.
 * Dec 07, 2007 452         bphillip    Retrieve lat/lon info from station table
 * Dec 17, 2007 628         bphillip    Discarding data with no station info
 * Dec 17, 2007 453         jkorman     Major restructure of the decode method
 *                                      to ensure that all sections are decoded
 *                                      properly. Added cleanMessage method.
 * Dec 18, 2007 453         jkorman     Added metric winds and visibility.
 * Dec 21, 2007 665         jkorman     Modified metric vis to ensure it is not
 *                                      decoding alstg data. Added checks for
 *                                      NSC, NCD, and CAVOK. Added checks for
 *                                      metric sector vis.
 * Jan 02, 2008 667         jkorman     Added code to properly decode/store
 *                                      clear  sky conditions.
 * Jan 16, 2008 798         jkorman     Changed logging levels.
 * Apr 14, 2008 996         jkorman     Rewrote sky cover decode section to
 *                                      handle  CB/TCU and /// data.
 * Nov 11, 2008 1684        chammack    Camel refactor.
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Sep 17, 2013 2378        njensen     Improve 3/6 hr precip decoding
 * May 12, 2014 DR 17151    D. Friedman Fix 6hr min/max temp decoding.
 * May 14, 2014 2536        bclement    moved WMO Header to common, removed TimeTools usage
 * Jul 23, 2014  3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

public class MetarDecoder extends AbstractDecoder {

    private static final char APOSTROPHE = '\'';

    // 10 minute limit into the future for METAR observations.
    private static final int METAR_FUTURE_LIMIT = 10;

    /** Regular expression for metar type, ICAO and the date */
    private static final Pattern METAR_EXP = Pattern
            .compile("(METAR|SPECI) (\\p{Alnum}{4}) (\\d{6})Z");

    // Regular expression for the wind group - Imperial
    private static final Pattern WIND_GROUP_EXP_KT = Pattern
            .compile("(\\d{3}|VRB)(\\d{2,3})((G)(\\d{2,3}))?KT");

    private static final Pattern WIND_GROUP_EXP_MPS = Pattern
            .compile("(\\d{3}|VRB)(\\d{2,3})((G)(\\d{2,3}))?MPS");

    // Variable wind direction
    private static final Pattern WIND_VAR_DIR_EXP = Pattern
            .compile("\\d{3}V\\d{3}");

    /** Regular expression for the visibility */
    // private final Pattern VISIBILITY_EXP = Pattern
    // .compile("M?(\\d{1,2} ?(\\d/\\d)?)SM");
    private static final Pattern VISIBILITY_EXP_1 = Pattern
            .compile("(M| )\\d{1,2}SM");

    private static final Pattern VISIBILITY_EXP_2 = Pattern
            .compile("((M| \\d)? ?((\\d)/(\\d{1,2})()))SM");

    private static final Pattern RVR_EXP = Pattern
            .compile("R\\d{2}[LR]?/([PM]?\\d{4}V)?[PM]?\\d{4}(FT)?");

    /** Regular expression for the correction notifier */
    private static final Pattern COR_EXP = Pattern.compile("\\b(COR)\\b");

    /** Regular expression for the temperature and dew point */
    public static final Pattern TEMP_EXP = Pattern.compile("\\bM?\\d{2}?/");

    public static final Pattern DWPT_EXP = Pattern.compile("/M?\\d{2}?\\b");

    /** Regular expression for the current sky conditions */
    public static final Pattern SKY_EXP = Pattern
            .compile("(((FEW|SCT|BKN|OVC|VV)((\\d{2,3}|///)((CB|TCU)?)))|((SKC|CLR)))");

    /** Regular expression for the altimeter reading */
    public static final Pattern ALT_IN_EXP = Pattern.compile("A(\\d{4})");

    public static final Pattern ALT_MB_EXP = Pattern.compile("Q(\\d{4})");

    /** Regular expression for the automated station type */
    public static final Pattern AUTO_STATION_EXP = Pattern
            .compile("\\s+A[0O][12](A?)(\\$|=)*\\s+");

    /** Regular expression for the sea level pressure */
    public static final Pattern SEA_LEVEL_PRESS_EXP = Pattern
            .compile("\\bSLP(\\d{3}|NO)\\b");

    /** Regular expression for the temperature and dew point in tenths */
    public static final Pattern TEMP_TENTHS_EXP = Pattern
            .compile("\\bT((\\d)(\\d{3})(\\d)(\\d{3}))\\b");

    /** Regular expression for 24 hr. temperature max and min values */
    public static final Pattern TEMP_MAX_MIN_EXP = Pattern
            .compile("\\b4((\\d)(\\d{3})(\\d)(\\d{3}))\\b");

    /** Regular expression for the 1 hour precipitation */
    public static final Pattern PRECIP_1HR_EXP = Pattern
            .compile("\\bP(\\d{4})\\b");

    /** Regular expression for the 3 hour precipitation */
    public static final Pattern PRECIP_3HR_EXP = Pattern
            .compile("\\s6(\\d{4}|////)\\s");

    /** Regular expression for the 24 hour precipitation */
    public static final Pattern PRECIP_24HR_EXP = Pattern
            .compile("\\s7(\\d{4}|////)\\s");

    /** Regular expression for the 3 hour pressure change */
    public static final Pattern PRESS_CHANGE_EXP = Pattern
            .compile("\\b5(\\d)(\\d{3})\\b");

    /** Regular expression for the 3 hour precipitation */
    public static final Pattern MAXMIN_TEMP_6_HR_EXP = Pattern
            .compile("\\b([12])([01])(\\d{3})\\b");

    public static final Pattern PK_WIND_EXP = Pattern
            .compile("\\b(PK WND|PKWND) (\\d{3})(\\d{2,3})/(\\d{2}|\\d{4})\\b");

    public static final Pattern SNOW_FALL_6HR = Pattern
            .compile("(\\b)931(\\d{3}|///)");

    public static final Pattern SNOW_WATER = Pattern
            .compile("(\\b)933(\\d{3}|///)");

    public static final Pattern SNOW_DEPTH = Pattern
            .compile("(\\b)4/(\\d{3}|///)");

    public static final Pattern SUNSHINE = Pattern
            .compile("(\\b)98(\\d{3}|///)");

    private boolean useMockInfo = false;

    private ObStation mockInfo = null;

    private VisibilityParser VIS_PARSER = null;

    private String traceId = null;

    public MetarDecoder() {
        VIS_PARSER = new VisibilityParser();
    }

    public void setTraceId(String id) {
        traceId = id;
    }

    /**
     * @param inputData
     * @param headers
     * @return
     * @throws DecoderException
     */
    public PluginDataObject[] decode(byte[] inputData, Headers headers)
            throws DecoderException {

        MetarSeparator sep = MetarSeparator.separate(inputData, headers);

        List<PluginDataObject> retVal = new ArrayList<PluginDataObject>();

        Calendar baseTime = TimeUtil.newGmtCalendar();
        WMOHeader wmoHdr = sep.getWMOHeader();
        if (WMOTimeParser.allowArchive()) {
            if ((wmoHdr != null) && (wmoHdr.isValid())) {
                String fileName = (String) headers
                        .get(WMOHeader.INGEST_FILE_NAME);
                baseTime = WMOTimeParser.findDataTime(wmoHdr.getYYGGgg(),
                        fileName);
            } else {
                logger.error("ARCHIVE MODE-No WMO Header found in file"
                        + headers.get(WMOHeader.INGEST_FILE_NAME));
            }
        }

        while (sep.hasNext()) {
            byte[] messageData = sep.next();
            Pattern thePattern;

            String message = new String(messageData);
            StringBuilder sbm = new StringBuilder(message);
            for (int i = 0; i < sbm.length();) {
                switch (sbm.charAt(i)) {
                case 1:
                case 3: {
                    sbm.deleteCharAt(i);
                    break;
                }
                case APOSTROPHE: {
                    sbm.setCharAt(i, ' ');
                    break;
                }
                default: {
                    i++;
                }
                }
            }
            message = sbm.toString().trim();
            MetarRecord record = new MetarRecord();
            record.setMessageData(message);
            message = cleanMessage(message);

            String remarks = null;
            int cutPos = message.indexOf(" RMK ");
            if (cutPos >= 0) {
                remarks = message.substring(cutPos);
                // Now truncate the message data.
                message = message.substring(0, cutPos);
            } else {
                remarks = "";
            }
            StringBuilder trailingData = new StringBuilder(remarks);
            trailingData.append(' ');

            StringBuilder obsMsg = new StringBuilder(message);

            try {
                // Gets the ICAO date and metar type
                Matcher matcher = METAR_EXP.matcher(obsMsg);

                if (matcher.find()) {
                    cutPos = matcher.end();
                    String timeGroup = matcher.group(3) + "Z";
                    record.setReportType(matcher.group(1));

                    String icao = matcher.group(2);
                    ObStation station = getStationInfo(icao);
                    if (station != null) {
                        SurfaceObsLocation loc = new SurfaceObsLocation(icao);
                        float lat = (float) station.getGeometry().getY();
                        float lon = (float) station.getGeometry().getX();
                        loc.assignLocation(lat, lon);
                        loc.setElevation(station.getElevation());

                        record.setLocation(loc);
                    } else {
                        // Couldn't find icao in spatial table
                        logger.info(String
                                .format("%s - Station id not found [%s]",
                                        traceId, icao));
                        continue;
                    }

                    Calendar obsTime = null;
                    Integer da = DecoderTools.getInt(timeGroup, 0, 2);
                    Integer hr = DecoderTools.getInt(timeGroup, 2, 4);
                    Integer mi = DecoderTools.getInt(timeGroup, 4, 6);
                    if ((da != null) && (hr != null) && (mi != null)) {
                        obsTime = (Calendar) baseTime.clone();
                        obsTime.set(Calendar.DAY_OF_MONTH, da);
                        obsTime.set(Calendar.HOUR_OF_DAY, hr);
                        obsTime.set(Calendar.MINUTE, mi);
                        obsTime.set(Calendar.SECOND, 0);
                        obsTime.set(Calendar.MILLISECOND, 0);
                    }
                    if (obsTime != null) {
                        record.setTimeObs(obsTime);
                        record.setDataTime(new DataTime(obsTime));
                        Calendar refHour = TimeTools.copyToNearestHour(obsTime);
                        if (mi >= 45) {
                            refHour.add(Calendar.HOUR_OF_DAY, 1);
                        }
                        record.setRefHour(refHour);
                        // TODO :
                    } else {
                        // couldn't find observation time so exit.
                        logger.warn(traceId
                                + " -Could not find observation time. Discarding data");
                        continue;
                    }
                }
                // We have time, so do a range check. No more than 10 minutes
                // into the future
                Calendar obsTime = record.getTimeObs();
                if (obsTime != null) {
                    Calendar currTime = (Calendar) baseTime.clone();

                    // Do this only for archive mode!!! Otherwise valid data
                    // will not pass if the WMO header
                    // date/time is much less than the obstime. For instance
                    // WMO Header time = dd1200
                    // Observed time = dd1235
                    // To solve this will require greater precision in the file
                    // timestamp.
                    if (WMOTimeParser.allowArchive()) {
                        currTime.add(Calendar.HOUR, 1);
                    }
                    currTime.add(Calendar.MINUTE, METAR_FUTURE_LIMIT);

                    long diff = currTime.getTimeInMillis()
                            - obsTime.getTimeInMillis();
                    // is it in the future?
                    if (diff < 0) {
                        // Too far into the future so return.
                        diff = Math.abs((diff / 60000)) + METAR_FUTURE_LIMIT;

                        String msg = String
                                .format("%s -Reject: %s ObsTime is %d minutes in the future",
                                        traceId, record.getStationId(), diff);
                        logger.info(msg);
                        continue;
                    }
                }
                obsMsg.delete(0, cutPos);

                // Gets the correction notifier
                thePattern = Pattern.compile(" AUTO");
                matcher = thePattern.matcher(obsMsg);
                if (matcher.find()) {
                    obsMsg.delete(0, matcher.end());
                }

                // Gets the correction notifier
                matcher = COR_EXP.matcher(obsMsg);
                if (matcher.find()) {
                    record.setCorrection(matcher.group(1));
                    obsMsg.delete(0, matcher.end());
                }

                // Gets the wind information
                matcher = WIND_GROUP_EXP_KT.matcher(obsMsg);

                if (matcher.find()) {
                    record.setWindDir(matcher.group(1));
                    record.setWindSpeed(Integer.parseInt(matcher.group(2)));
                    if (matcher.group(5) != null) {
                        record.setWindGust(Integer.parseInt(matcher.group(5)));
                    }
                    obsMsg.delete(0, matcher.end());
                }

                // Gets the wind information
                matcher = WIND_GROUP_EXP_MPS.matcher(obsMsg);

                if (matcher.find()) {
                    record.setWindDir(matcher.group(1));

                    Integer ws = Integer.parseInt(matcher.group(2));
                    if (ws != null) {
                        float spd = ws * 1.943f;
                        record.setWindSpeed(Math.round(spd));
                    }
                    if (matcher.group(5) != null) {
                        ws = Integer.parseInt(matcher.group(5));
                        if (ws != null) {
                            float spd = ws * 1.943f;
                            record.setWindGust(Math.round(spd));
                        }
                    }
                    obsMsg.delete(0, matcher.end());
                }

                // Gets the wind information
                matcher = WIND_VAR_DIR_EXP.matcher(obsMsg);

                if (matcher.find()) {
                    obsMsg.delete(0, matcher.end());
                }

                boolean cavokFound = false;
                int cavok = obsMsg.indexOf("CAVOK");
                if (cavok >= 0) {
                    cavokFound = true;
                    SkyCover cover = new SkyCover();
                    cover.setType("CAVOK");
                    cover.setHeight(null);
                    record.addSkyCoverage(cover);

                    obsMsg.delete(cavok, cavok + "CAVOK".length());
                }

                if (!cavokFound) {
                    // Gets the visibility information
                    boolean foundVis = false;
                    String vis = null;
                    matcher = VISIBILITY_EXP_1.matcher(obsMsg);
                    if (matcher.find()) {
                        vis = obsMsg.substring(matcher.start(), matcher.end());
                        obsMsg.delete(0, matcher.end());
                        foundVis = true;
                    } else {
                        matcher = VISIBILITY_EXP_2.matcher(obsMsg);
                        if (matcher.find()) {
                            vis = obsMsg.substring(matcher.start(),
                                    matcher.end());
                            obsMsg.delete(0, matcher.end());
                            foundVis = true;
                        }
                    }
                    if (vis != null) {
                        if (VIS_PARSER.decode(vis.trim())) {
                            record.setVisibility(new Double(VIS_PARSER
                                    .getPrevail_vsbySM()).floatValue());
                        }
                    }
                    if (!foundVis) {
                        thePattern = Pattern.compile("\\d{4}NDV");
                        matcher = thePattern.matcher(obsMsg);
                        if (matcher.find()) {
                            int start = matcher.start();
                            int end = matcher.end();
                            if (VIS_PARSER.decode(obsMsg.substring(start,
                                    end - 3).trim())) {
                                record.setVisibility(new Double(VIS_PARSER
                                        .getPrevail_vsbySM()).floatValue());
                            }
                            obsMsg.delete(start, end);
                        }

                        boolean sectorFound = true;
                        while (sectorFound) {
                            thePattern = Pattern.compile("\\d{4}[NSEW]");
                            matcher = thePattern.matcher(obsMsg);
                            if (matcher.find()) {
                                int start = matcher.start();
                                int end = matcher.end();
                                if (VIS_PARSER.decode(obsMsg.substring(start,
                                        end - 1).trim())) {
                                    record.setVisibility(new Double(VIS_PARSER
                                            .getPrevail_vsbySM()).floatValue());
                                }
                                obsMsg.delete(start, end);
                            } else {
                                sectorFound = false;
                            }
                        }

                        thePattern = Pattern.compile("\\d{4}");
                        matcher = thePattern.matcher(obsMsg);
                        if (matcher.find()) {
                            int start = matcher.start();
                            if (start > 0) {
                                // ensure that we're not looking at the
                                // altimeter
                                // setting.
                                char c = obsMsg.charAt(start - 1);
                                if (('A' != c) && ('Q' != c)) {
                                    if (VIS_PARSER.decode(obsMsg.substring(
                                            start, matcher.end()).trim())) {
                                        record.setVisibility(new Double(
                                                VIS_PARSER.getPrevail_vsbySM())
                                                .floatValue());
                                        obsMsg.delete(0, matcher.end());
                                    }
                                }
                            }
                        }
                    }
                }

                matcher = RVR_EXP.matcher(obsMsg);
                if (matcher.find()) {
                    obsMsg.delete(0, matcher.end());
                }

                // Gets the altimeter setting
                matcher = ALT_IN_EXP.matcher(obsMsg);
                if (matcher.find()) {
                    float alstg = Float.parseFloat(matcher.group(1)) / 100f;
                    record.setAltimeter(alstg);
                    alstg = alstg * 3386.53f;
                    record.setAltimeterInPa(alstg);
                    obsMsg.delete(matcher.start(), obsMsg.length());
                }

                matcher = ALT_MB_EXP.matcher(obsMsg);
                if (matcher.find()) {
                    float alstg = Float.parseFloat(matcher.group(1)) * 100f;
                    record.setAltimeterInPa(alstg);
                    alstg = alstg / 3386.53f;
                    record.setAltimeter(alstg);
                    obsMsg.delete(matcher.start(), obsMsg.length());
                }

                String tempData = null;
                String dwptData = null;

                // wxDataEnd is the last position we would expect to find
                // the wx data. We update the position as various data is found.
                int wxDataEnd = obsMsg.length();

                // Gets the temperature
                matcher = TEMP_EXP.matcher(obsMsg);
                if (matcher.find()) {
                    wxDataEnd = Math.min(wxDataEnd, matcher.start());
                    StringBuilder sb = new StringBuilder(obsMsg.substring(
                            matcher.start(), matcher.end()));
                    fixUpTemp(sb);
                    tempData = sb.toString();
                }
                // Gets the dew point
                matcher = DWPT_EXP.matcher(obsMsg);
                if (matcher.find()) {
                    wxDataEnd = Math.min(wxDataEnd, matcher.start());
                    StringBuilder sb = new StringBuilder(obsMsg.substring(
                            matcher.start(), matcher.end()));
                    fixUpTemp(sb);
                    dwptData = sb.toString();
                }
                if (tempData != null) {
                    try {
                        record.setTemperature(Integer.parseInt(tempData));
                    } catch (NumberFormatException nfe) {
                        logger.error(traceId + " -Error decoding temperature "
                                + obsMsg);
                    }
                }

                if (dwptData != null) {
                    try {
                        record.setDewPoint(Integer.parseInt(dwptData));
                    } catch (NumberFormatException nfe) {
                        logger.error(traceId + " -Error decoding dewpoint "
                                + obsMsg);
                    }
                }
                obsMsg.delete(wxDataEnd, obsMsg.length());

                if (!cavokFound) {
                    int sky = obsMsg.indexOf("NSC");
                    if (sky >= 0) {
                        SkyCover cover = new SkyCover();
                        cover.setType("NSC");
                        record.addSkyCoverage(cover);
                        obsMsg.delete(sky, obsMsg.length());
                    }
                    sky = obsMsg.indexOf("NCD");
                    if (sky >= 0) {
                        SkyCover cover = new SkyCover();
                        cover.setType("NCD");
                        record.addSkyCoverage(cover);
                        obsMsg.delete(sky, obsMsg.length());
                    }
                    if (sky < 0) {
                        matcher = SKY_EXP.matcher(obsMsg);

                        wxDataEnd = obsMsg.length();
                        boolean baseSet = false;
                        while (matcher.find()) {
                            wxDataEnd = Math.min(wxDataEnd, matcher.start());

                            SkyCover cover = null;
                            String grp1 = matcher.group(1);
                            if (("SKC".equals(grp1)) || ("CLR".equals(grp1))) {
                                cover = new SkyCover();
                                cover.setType(grp1);
                                cover.setHeight(null);
                                if (!baseSet) {
                                    record.setSkyLayerBase(-9999);
                                    baseSet = true;
                                }
                            } else {
                                cover = new SkyCover();
                                cover.setType(matcher.group(3));
                                String cldhgt = matcher.group(5);
                                Integer hgt = DecoderTools.getInt(cldhgt, 0,
                                        cldhgt.length());
                                if (IDecoderConstants.VAL_MISSING.equals(hgt)) {
                                    cover.setHeight(-9999);
                                } else if ((hgt != null) && (hgt >= 0)) {
                                    cover.setHeight(hgt * 100);
                                    if (!baseSet) {
                                        record.setSkyLayerBase(hgt * 100);
                                        baseSet = true;
                                    }
                                }
                                cover.setGenus(matcher.group(7));
                            }
                            if (cover != null) {
                                record.addSkyCoverage(cover);
                            }
                        }
                        if (wxDataEnd > 0) {
                            obsMsg.delete(wxDataEnd, obsMsg.length());
                        }
                    }
                    // **************************************************************
                    // Present weather groups.
                    // **************************************************************
                    // Gets the current weather phenomena
                    int wx = obsMsg.indexOf("NSW");
                    if (wx < 0) {
                        List<WeatherCondition> conds = WeatherCondition
                                .parseWeather(obsMsg.toString());
                        if (conds.size() > 0) {
                            record.setWeatherCondition(conds);
                            record.setWeatherKey(obsMsg.toString().trim());
                        }
                    }
                }

                // ******************************************************************
                // * From here to the end are items that decoded from the
                // remarks
                // * section.
                // ******************************************************************
                // Gets the automated station type
                matcher = AUTO_STATION_EXP.matcher(trailingData);

                if (matcher.find()) {
                    String s = matcher.group().trim();
                    if (s.startsWith("AO1")) {
                        s = "AO1";
                    } else if (s.startsWith("AO2")) {
                        s = "AO2";
                    } else if (s.startsWith("A01")) {
                        s = "AO1";
                    } else if (s.startsWith("A02")) {
                        s = "AO2";
                    }
                    record.setAutoStationType(s);
                }

                // Gets the sea level pressure reading
                matcher = SEA_LEVEL_PRESS_EXP.matcher(trailingData);

                if (matcher.find()) {

                    Float slp = -9999.0f;
                    if (matcher.group(1).equals("NO")) {
                        record.setSeaLevelPress(slp);
                    } else {

                        // Puts either a 9 or 10 in front of the number and
                        // which
                        // ever result is closest to 1000 is used
                        try {
                            slp = Float.parseFloat(matcher.group(1)) / 10.0f;

                            if (slp > 50.0f) {
                                slp = 900.0f + slp;
                            } else {
                                slp = 1000.0f + slp;
                            }
                        } catch (NumberFormatException nfe) {
                            // nothing
                        }
                        if (slp != null) {
                            record.setSeaLevelPress(slp);
                        }
                    }
                }

                // Get the peak wind data
                // "\\bPK WND (\\d{3})(\\d{2,3})/(\\d{2}|\\d{4})\\b"
                matcher = PK_WIND_EXP.matcher(trailingData);
                if (matcher.find()) {
                    int pkDir = getInt(matcher.group(2), -9999);
                    record.setPkWndDir(pkDir);

                    int pkSpd = getInt(matcher.group(3), -9999);
                    record.setPkWndSpd(pkSpd);

                    int pkHHmm = getInt(matcher.group(4), -9999);
                    int hh = -1;
                    int mm = pkHHmm % 100;
                    // Check if hours were included.
                    if (pkHHmm > 100) {
                        hh = pkHHmm / 100;
                    }
                    Calendar obsT = record.getTimeObs();
                    Calendar pkTim = (Calendar) obsT.clone();

                    int obsMin = obsT.get(Calendar.MINUTE);
                    int obsHr = obsT.get(Calendar.HOUR_OF_DAY);
                    // Do we need to find the pkwind hour?
                    if (hh == -1) {
                        if (mm > obsMin) {
                            pkTim.add(Calendar.HOUR_OF_DAY, -1);
                        }
                    } else {
                        pkTim.set(Calendar.HOUR_OF_DAY, hh);
                    }
                    pkTim.set(Calendar.MINUTE, mm);
                    int pkHr = pkTim.get(Calendar.HOUR_OF_DAY);
                    if (pkHr > obsHr) {
                        pkTim.add(Calendar.DAY_OF_MONTH, -1);
                    }
                    record.setPkWndTime(pkTim);
                    trailingData.delete(matcher.start(), matcher.end());
                }

                // Gets the temperature and dew point in tenths precision
                matcher = TEMP_TENTHS_EXP.matcher(trailingData);

                if (matcher.find()) {
                    record.setTempFromTenths(Float.parseFloat(matcher.group(3)));
                    if (matcher.group(2).equals("1")) {
                        record.setTempFromTenths(record.getTempFromTenths()
                                / -10);
                    } else {
                        record.setTempFromTenths(record.getTempFromTenths() / 10);
                    }

                    record.setDewPointFromTenths(Float.parseFloat(matcher
                            .group(5)));
                    if (matcher.group(4).equals("1")) {
                        record.setDewPointFromTenths(record
                                .getDewPointFromTenths() / -10);
                    } else {
                        record.setDewPointFromTenths(record
                                .getDewPointFromTenths() / 10);
                    }

                }

                // Gets the 24 hour min and max temperatures
                matcher = TEMP_MAX_MIN_EXP.matcher(trailingData);

                if (matcher.find()) {
                    record.setMaxTemp24Hour(Float.parseFloat(matcher.group(3)));
                    if (matcher.group(2).equals("1")) {
                        record.setMaxTemp24Hour(record.getMaxTemp24Hour() / -10);
                    } else {
                        record.setMaxTemp24Hour(record.getMaxTemp24Hour() / 10);
                    }

                    record.setMinTemp24Hour(Float.parseFloat(matcher.group(5)));
                    if (matcher.group(4).equals("1")) {
                        record.setMinTemp24Hour(record.getMinTemp24Hour() / -10);
                    } else {
                        record.setMinTemp24Hour(record.getMinTemp24Hour() / 10);
                    }

                }

                // Gets the precipitation over the last hour
                matcher = PRECIP_1HR_EXP.matcher(trailingData);
                if (matcher.find()) {
                    String s = matcher.group(1).trim();
                    try {
                        float precip = Integer.parseInt(s) / 100.0f;
                        record.setPrecip1Hour(precip);
                    } catch (NumberFormatException nfe) {
                        // nothing
                    }
                }

                // Gets the percipitation over the last 3 hours
                matcher = PRECIP_3HR_EXP.matcher(trailingData);

                if (matcher.find()) {
                    int obsHr = record.getTimeObs().get(Calendar.HOUR_OF_DAY);
                    int minute = record.getTimeObs().get(Calendar.MINUTE);
                    if ("METAR".equals(record.getReportType())) {
                        if (minute >= 45) {
                            obsHr++;
                        }
                    }

                    if ((obsHr % 3) == 0) {
                        Float precip = null;
                        try {
                            precip = Float.parseFloat(matcher.group(1));
                            precip /= 100.0f;
                        } catch (NumberFormatException nfe) {
                            precip = null;
                        }
                        if (precip != null) {
                            if ((obsHr % 6) == 0) {
                                record.setPrecip6Hour(precip);
                            } else {
                                record.setPrecip3Hour(precip);
                            }
                        }
                    } else {
                        String dt = String
                                .format("3/6 hour precip reported from %s at %2$tY%2$tm%2$td %2$tH%2$tM",
                                        record.getStationId(),
                                        record.getTimeObs());
                        logger.error(dt);
                    }
                }

                matcher = PRECIP_24HR_EXP.matcher(trailingData);
                if (matcher.find()) {
                    String s = matcher.group(1).trim();
                    if (!"////".equals(s)) {
                        try {
                            float precip = Integer.parseInt(s) / 100.0f;
                            record.setPrecip24Hour(precip);
                        } catch (NumberFormatException nfe) {
                            // nothing
                        }
                    }
                }

                matcher = MAXMIN_TEMP_6_HR_EXP.matcher(trailingData);
                while (matcher.find()) {
                    String op = matcher.group(1);
                    String sn = matcher.group(2);
                    String v = matcher.group(3);
                    float t = -9999;
                    try {
                        t = Float.parseFloat(v) / 10.0f;
                        if ("1".equals(sn)) {
                            t *= -1;
                        } else if (!"0".equals(sn)) {
                            t = -9999;
                        }
                    } catch (NumberFormatException nfe) {
                        // Nothing
                    }

                    if ("1".equals(op)) {
                        record.setMaxTemp6Hour(t);
                    } else if ("2".equals(op)) {
                        record.setMinTemp6Hour(t);
                    }
                }

                // Gets the pressure change over the last 3 hours
                matcher = PRESS_CHANGE_EXP.matcher(trailingData);

                if (matcher.find()) {
                    record.setPressChangeChar(matcher.group(1));
                    record.setPressChange3Hour(Float.parseFloat(matcher
                            .group(2)));
                }

                matcher = SNOW_FALL_6HR.matcher(trailingData);
                if (matcher.find()) {

                    String s = matcher.group(2);
                    Float value = null;
                    if ("///".equals(s)) {
                        value = -9999f;
                    } else {
                        s = removeLeadingZeros(s);
                        value = (getInt(s, -99990) / 10.0f);
                    }
                    record.setSnowFall_6Hours(value);
                }

                matcher = SNOW_WATER.matcher(trailingData);
                if (matcher.find()) {
                    String s = matcher.group(2);
                    Float value = null;
                    if ("///".equals(s)) {
                        value = -9999f;
                    } else {
                        s = removeLeadingZeros(s);
                        value = (getInt(s, -99990) / 10.0f);
                    }
                    record.setSnowWater(value);
                }

                matcher = SNOW_DEPTH.matcher(trailingData);
                if (matcher.find()) {
                    String s = matcher.group(2);
                    Integer value = null;
                    if ("///".equals(s)) {
                        value = -9999;
                    } else {
                        s = removeLeadingZeros(s);
                        value = getInt(s, Integer.MIN_VALUE);
                    }
                    record.setSnowDepth(value);
                }

                matcher = SUNSHINE.matcher(trailingData);
                if (matcher.find()) {
                    String s = matcher.group(2);
                    Integer value = null;
                    if ("///".equals(s)) {
                        value = -9999;
                    } else {
                        s = removeLeadingZeros(s);
                        value = getInt(s, Integer.MIN_VALUE);
                    }
                    record.setSunshine(value);
                }

                record.setWmoHeader(sep.getWMOHeader().getWmoHeader());

                retVal.add(record);

            } catch (Exception e) {
                logger.error(traceId + " - Unable to decode METAR/SPECI", e);
            }
        }

        return retVal.toArray(new PluginDataObject[retVal.size()]);
    }

    /**
     * Change "M" minus indicator to a minus sign "-" if found in temperature.
     * data. Also remove a leading or trailing "/" character.
     * 
     * @param sb
     *            String containing temperature data.
     */
    private void fixUpTemp(StringBuilder tempData) {
        int pos = tempData.indexOf("M");
        if (pos >= 0) {
            tempData.setCharAt(pos, '-');
        }
        pos = tempData.indexOf("/");
        if (pos >= 0) {
            tempData.deleteCharAt(pos);
        }
    }

    private ObStation getStationInfo(String icao) {
        ObStation station = null;

        if (!useMockInfo) {
            ObStationDao dao = new ObStationDao();
            if (dao != null) {
                try {
                    station = dao.queryByIcao(icao);
                } catch (Exception e) {
                    logger.error(traceId
                            + " -Could not create datauri in findDuplicate", e);
                }
            }
        } else {
            station = mockInfo;
        }
        return station;
    }

    /**
     * Get rid of any control characters prior to parsing data.
     * 
     * @param message
     * @return
     */
    private String cleanMessage(String message) {
        StringBuilder sb = new StringBuilder();
        char lastChar = 0;
        for (int i = 0; i < message.length(); i++) {
            char c = message.charAt(i);
            if (c < ' ') {
                if (lastChar != c) {
                    sb.append(' ');
                }
            } else {
                sb.append(c);
            }
            lastChar = c;
        }
        return sb.toString();
    }

    void setMockInfo(ObStation station) {
        mockInfo = station;
        useMockInfo = (station != null);
    }

    public static final String removeLeadingZeros(String data) {
        StringBuilder sb = null;
        if (data != null) {
            sb = new StringBuilder(data);
            for (int i = 0; i < (sb.length() - 1); i++) {
                if (sb.charAt(i) == '0') {
                    sb.setCharAt(i, ' ');
                }
            }
        }
        return (sb != null) ? sb.toString().trim() : "";
    }

    public static final int getInt(String val, int defaultVal) {
        int retVal = defaultVal;
        try {
            retVal = Integer.parseInt(val);
        } catch (NumberFormatException nfe) {
            // Nothing
        }
        return retVal;
    }

    /**
     * 
     * @param report
     * @return
     */
    public static String formatMetar(String report) {

        StringBuilder sb = new StringBuilder();
        char lastChar = 0;
        for (int i = 0; i < report.length(); i++) {
            char c = report.charAt(i);
            switch (c) {
            case '\r':
            case '\n': {
                c = ' ';
            }
            default: {
                if (lastChar == ' ') {
                    if (c != ' ') {
                        sb.append(c);
                    }
                } else {
                    sb.append(c);
                }
                lastChar = c;
            }
            }
        }
        String indent = "     ";
        ArrayList<String> parts = new ArrayList<String>();
        // int rmkPos = sb.indexOf("RMK");

        parts.add(sb.toString());
        // if(rmkPos > 0) {
        // parts.add(sb.substring(0,rmkPos));
        // parts.add(indent + sb.substring(rmkPos));
        // } else {
        // parts.add(sb.toString());
        // }
        for (int i = 0; i < parts.size();) {
            String s = parts.get(i);
            // if(s.indexOf("RMK") > 0) {
            // indent += " ";
            // }
            if (s.length() < 74) {
                i++;
            } else {
                // Start at position 70
                int spcPos = 70;
                // and work backwards to find the first space character
                // so we can cut the line there.
                while ((s.charAt(spcPos) != ' ') && (spcPos > 0)) {
                    spcPos--;
                }
                if (s.charAt(spcPos) == ' ') {
                    parts.set(i, s.substring(0, spcPos));
                    i++;
                    parts.add(i, indent + s.substring(spcPos + 1));
                }
            }
        }
        sb.setLength(0);
        sb.append(parts.get(0));
        for (int i = 1; i < parts.size(); i++) {
            sb.append('\r');
            sb.append(parts.get(i));
        }
        return sb.toString();
    }

    public static final Point createPoint(double latitude, double longitude) {

        CoordinateArraySequence caq = new CoordinateArraySequence(
                new Coordinate[] { DecoderTools.createCoordinate(latitude,
                        longitude) });
        Point p = new Point(caq, new GeometryFactory());

        return p;
    }

    public static final void main(String[] args) {

        // boolean perform = false;
        //
        // if (perform) {
        // Pattern VISIBILITY_EXP_1 = Pattern.compile("(M| )\\d{1,2}SM");
        //
        // Pattern VISIBILITY_EXP_2 = Pattern
        // .compile("((M| \\d)? ?((\\d)/(\\d{1,2})()))SM");
        //
        // String[] data = { " 0SM +SN OVC015 ", " 1SM +SN OVC015 ",
        // " 9SM +SN OVC015 ", " 10SM +SN OVC015 ",
        // " 1/16SM +SN OVC015 ", " 1/8SM +SN OVC015 ",
        // " 3/16SM +SN OVC015 ", " 1/4SM +SN OVC015 ",
        // " 5/32SM +SN OVC015 ", " 5/16SM +SN OVC015 ",
        // " 3/8SM +SN OVC015 ", " 1/2SM +SN OVC015 ",
        // " 5/8SM +SN OVC015 ", " 3/4SM +SN OVC015 ",
        // " 7/8SM +SN OVC015 ", " 1 1/4SM +SN OVC015 ",
        // " 1 1/16SM +SN OVC015 ", " 1 1/2SM +SN OVC015 ",
        // " 1 3/4SM +SN OVC015 ", " 2SM +SN OVC015 ", };
        //
        // for (String s : data) {
        // System.out.print(String.format("%32s", s));
        // Matcher m = VISIBILITY_EXP_1.matcher(s);
        // if (m.find()) {
        // System.out.println(" 1 ["
        // + s.substring(m.start(), m.end()).trim());
        // for (int i = 0; i < m.groupCount(); i++) {
        // System.out.println(" ---- " + m.group(i));
        // }
        // } else {
        // m = VISIBILITY_EXP_2.matcher(s);
        // if (m.find()) {
        // System.out.println(" 2 ["
        // + s.substring(m.start(), m.end()).trim());
        //
        // for (int i = 0; i < m.groupCount(); i++) {
        // System.out.println(" ---- " + m.group(i));
        // }
        // } else {
        // System.out.println("  Fail");
        // }
        // }
        // }
        // }
        //
        // perform = false;
        // if (perform) {
        // Pattern p = Pattern
        // .compile("\\bPK WND (\\d{3})(\\d{2,3})/(\\d{2}|\\d{4})\\b");
        //
        // String data = "PWINO TSNO T00500005 PK WND 08027/2104 SLP060 $=";
        //
        // Matcher m = p.matcher(data);
        // if (m.find()) {
        // for (int i = 0; i <= m.groupCount(); i++) {
        // System.out.println(m.group(i));
        // }
        // }
        // }
        //
        // perform = false;
        // if (perform) {
        // String[] data = {
        // "KGFA 281156Z AUTO 26005KT 7SM -RA OVC009 04/03 A2999 RMK AO2 RAB37"
        // + "\r     CIG 007V011 SLP200 P0001 60002 70074 T00390033 10044 20033"
        // + "\r     53003 $=",
        // "KLWT 281154Z AUTO 27005KT 10SM -RA OVC020 04/04 A2997 RMK AO2"
        // +
        // "\r     RAB1057 SLP145 P0003 60011 70013 T00440039 10072 20044 53004"
        // + "\r     $=",
        // "KSIY 281153Z AUTO 19003KT 10SM BKN014 OVC023 07/06 A3006 RMK AO2"
        // + "\r     RAE10B36E48 SLP187 P0001 60003 70014 T00670056 10072 20067"
        // + "\r     53012=",
        // "KFGN 281154Z AUTO 11007KT 080V140 10SM CLR 17/11 A3000 RMK AO2 LTG"
        // + "\r     DSNT W AND NW=",
        // "KUKI 281156Z AUTO 33003KT 10SM BKN006 BKN012 OVC016 06/06 A3010"
        // + "\r     RMK AO2 CIG 002V009 SLP190 70081 T00610056 10072 20061"
        // + "\r     51005=",
        // "PABR 281153Z COR 02003KT 10SM OVC003 M01/M02 A3009 RMK AO2 CIG"
        // + "\r     001V005 SLP189 FZRAB12FZRAE50 P0002 60002 70002 4/012"
        // + "\r     T10111017 11011 21011 56009=",
        // "SAUS70 KWBC 281220 METAR KLUM 281216Z AUTO 00000KT 10SM CLR 13/08 A3014 RMK AO2",
        // };
        //
        // for (String s : data) {
        // System.out.println(formatMetar(s));
        // }
        // }
        // perform = true;
        // if (perform) {
        //
        // try {
        // String obs = "\001023\r\r\nSAUS42 KOAX 191510\r\r\n"
        // + ""
        // // +
        // //
        // "KOMA 281156Z AUTO 26005KT 7SM -FZDZSN FG OVC009 04/03 A2999 RMK AO2 RAB37"
        // // +
        // //
        // "\r\r\n     CIG 007V011 SLP200 P0001 60002 70074 T00390033 10044 20033"
        // // + "\r\r\n     53003 $=\r\r\b\003"
        // // +
        // //
        // "KFET 281156Z AUTO 26005KT 7SM -SNBR OVC009 04/03 A2999 RMK AO2 RAB37"
        // // +
        // //
        // "\r\r\n     CIG 007V011 SLP200 P0001 60002 70074 T00390033 10044 20033"
        // // + "\r\r\n     53003 $=\r\r\b\003"
        // + "VVNB 261830Z 04003KT 3700 TSRA SCT005 SCT030CB BKN040 26/25"
        // + "\r\r\n     Q1005 RMK PKWND 14527/1135 NOSIG";
        //
        // MetarDecoder decoder = new MetarDecoder("obs");
        // ObStation station = new ObStation();
        // station.setIcao("KOMA");
        // station.setLocation(createPoint(41.3, -95.9));
        // station.setElevation(399);
        // station.setCatalogType(ObStation.CAT_TYPE_ICAO);
        // station.setName("OMAHA/EPPLEY FIELD");
        // decoder.setMockInfo(station);
        // Headers headers = new Headers();
        // headers.put(DecoderTools.INGEST_FILE_NAME,
        // "SAUS42KOAX.20110103");
        // PluginDataObject[] m = decoder.decode(obs.getBytes(), headers);
        // if (m != null) {
        // for (PluginDataObject p : m) {
        // MetarRecord mr = (MetarRecord) p;
        //
        // System.out.println(mr);
        // System.out.println(formatMetar(mr.getReport()));
        // List<WeatherCondition> wx = mr.getWeatherCondition();
        // for (WeatherCondition w : wx) {
        // System.out.println(w);
        // }
        // }
        // }
        // } catch (DecoderException e) {
        // // TODO Auto-generated catch block
        // e.printStackTrace();
        // }
        // }

        String trailingData = " SLP124 P0036 60020 7//// T02220211 ";
        Matcher matcher = PRECIP_24HR_EXP.matcher(trailingData);
        // if (matcher.find()) {
        // String s = matcher.group(1);
        // if(!"////".equals(s)) {
        // try {
        // int precip = Integer.parseInt(s);
        // System.out.println(precip);
        // } catch (NumberFormatException nfe) {
        // nfe.printStackTrace();
        // }
        // } else {
        // System.out.println(-99.0f);
        // }
        // }

        trailingData = "SLP124 AO1=\r\r\n";
        matcher = AUTO_STATION_EXP.matcher(trailingData);
        if (matcher.find()) {
            String s = matcher.group().trim();
            System.out.println("[" + s + "]");
        }

    }

}
