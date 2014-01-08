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
package com.raytheon.uf.edex.plugin.lsr.decoder;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.lsr.LSREventType;
import com.raytheon.uf.common.dataplugin.lsr.LocalStormReport;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.lsr.LocalStormReportDao;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Local Storm Report parser
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009 1939       jkorman     Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Dec 09, 2013 2581       njensen     Reuse patterns for efficiency
 *                                      Check entire time line looking for latlon
 * Jan 07, 2013 2581       njensen     Check to end of string for source, not a set length
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class LSRParser {

    private static final int PDV_FILL_INT = -9999;

    private static int TIME = 0; // time

    private static int TIME_LENGTH = 7; // time max length

    private static int EVENT = 12; // event

    private static int EVENT_LENGTH = 16; // event max length

    private static int LOCATION = 29; // city location

    private static int LOCATION_LENGTH = 23; // city location max length

    private static int DATE = 0; // date

    private static int DATE_LENGTH = 10; // date max length

    private static int MAG = 12; // magnitude

    private static int COUNTY = 29; // county

    private static int COUNTY_LENGTH = 18; // county max length

    private static int STATE = 48; // state

    private static int STATE_LENGTH = 2; // state length

    private static int SOURCE = 53; // source

    private static final Pattern LATLON_PTRN = Pattern
            .compile("((([0-8][0-9]|90).\\d{2,2}[NS]) ++(1?+\\d{2,2}.\\d{2,2}[EW])())");

    private static final Pattern RPT_DT_PTRN = Pattern
            .compile(InternalReport.DATETIME);

    private static final Pattern FATAL_INJ_PTRN = Pattern
            .compile("\\*\\*\\*( (\\d*) (FATAL))?,?( (\\d*) (INJ))? \\*\\*\\*(.*)");

    private static final Pattern OFFICE_ID_PTRN = Pattern
            .compile("NWUS5[1-9]\\s([A-Z]{4})\\s\\d{6}");

    private static final Map<String, Integer> TIMEZONE = new HashMap<String, Integer>();
    static {
        TIMEZONE.put("EDT", 4);
        TIMEZONE.put("EST", 5);
        TIMEZONE.put("CDT", 5);
        TIMEZONE.put("CST", 6);
        TIMEZONE.put("MDT", 6);
        TIMEZONE.put("MST", 7);
        TIMEZONE.put("PDT", 7);
        TIMEZONE.put("PST", 8);
        TIMEZONE.put("GMT", 0);
    }

    /** The logger */
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(LSRParser.class);;

    private final PointDataDescription pointDataDescription;

    private final LocalStormReportDao lsrDao;

    private final Map<File, PointDataContainer> containerMap;

    private WMOHeader wmoHeader;

    private String officeid;

    private String traceId;

    private int hour;

    private int minute;

    private int tzOffset = -1;

    private boolean tzFound = false;

    int currentReport = -1;

    private final HashMap<String, Boolean> URI_MAP = new HashMap<String, Boolean>();

    private List<LocalStormReport> reports;

    /**
     * 
     * @param message
     * @param wmoHeader
     * @param pdd
     */
    public LSRParser(LocalStormReportDao dao, PointDataDescription pdd) {
        pointDataDescription = pdd;
        lsrDao = dao;
        containerMap = new HashMap<File, PointDataContainer>();
    }

    /**
     * Set the message data and decode all message reports.
     * 
     * @param message
     *            Raw message data.
     * @param traceId
     *            Trace id for this data.
     */
    public void setData(byte[] message, String traceId, Headers headers) {
        currentReport = -1;
        this.traceId = traceId;
        wmoHeader = new WMOHeader(message, headers);
        if (wmoHeader != null) {
            Matcher m = OFFICE_ID_PTRN.matcher(wmoHeader.getWmoHeader());
            if (m.matches()) {
                officeid = m.group(1);
            }
            reports = findReports(message);
        } else {
            logger.error(traceId + "- Missing or invalid WMOHeader");
        }
        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
        }
    }

    /**
     * Does this parser contain any more reports.
     * 
     * @return Does this parser contain any more reports.
     */
    public boolean hasNext() {
        boolean next = (reports != null);
        if (next) {
            next = ((currentReport >= 0) && (currentReport < reports.size()));
        }
        if (!next) {
            reports = null;
            currentReport = -1;
        }
        return next;
    }

    /**
     * Get the next available report. Returns a null reference if no more
     * reports are available.
     * 
     * @return The next available report.
     */
    public LocalStormReport next() {

        LocalStormReport report = null;
        if (currentReport < 0) {
            return report;
        }
        if (currentReport >= reports.size()) {
            reports = null;
            currentReport = -1;
        } else {
            report = reports.get(currentReport++);
            logger.debug("Getting report " + report);
            if (URI_MAP.containsKey(report.getDataURI())) {
                report = null;
            } else {
                URI_MAP.put(report.getDataURI(), Boolean.TRUE);
            }

            if (report != null) {

                PointDataContainer pdc = getContainer(report);

                // Populate the point data.
                PointDataView view = pdc.append();
                view.setLong("timeObs", report.getDataTime()
                        .getRefTimeAsCalendar().getTimeInMillis());
                view.setFloat("latitude", (float) report.getLatitude());
                view.setFloat("longitude", (float) report.getLongitude());
                view.setString("wmoHeader", report.getWmoHeader());

                view.setInt("eventType", report.getEventType().getValue());
                view.setString("eventUnit", report.getEventType()
                        .getEventUnits().name());
                view.setFloat("magnitude", report.getMagnitude());
                view.setString("countylocation", report.getCountyLoc());
                view.setString("statelocation", report.getStateLoc());
                view.setString("citylocation", report.getCityLoc());
                view.setString("remarks", report.getRemarks());
                view.setString("source", report.getSource());
                view.setInt("injuries", report.getInjuries());
                view.setInt("fatalities", report.getFatalities());

                view.setString("dataURI", report.getDataURI());

                report.setPointDataView(view);
            }
        }
        return report;
    }

    /**
     * 
     * @param obsData
     * @return
     */
    private PointDataContainer getContainer(LocalStormReport obsData) {

        File file = lsrDao.getFullFilePath(obsData);
        PointDataContainer container = containerMap.get(file);
        if (container == null) {
            container = PointDataContainer.build(pointDataDescription);
            containerMap.put(file, container);
        }
        return container;
    }

    /**
     * 
     * @param start
     * @return
     */
    private List<LocalStormReport> findReports(byte[] message) {

        List<LocalStormReport> reports = new ArrayList<LocalStormReport>();

        List<InternalReport> parts = InternalReport.identifyMessage(message);
        if (parts != null) {
            int currPos = 0;
            for (; currPos < parts.size(); currPos++) {
                InternalReport r = parts.get(currPos);
                if (InternalType.DATETIME_ZONE.equals(r.getLineType())) {
                    Matcher m = RPT_DT_PTRN.matcher(r.getReportLine());
                    if (m.find()) {
                        String tz = m.group(4);
                        if (TIMEZONE.containsKey(tz)) {
                            tzFound = true;
                            tzOffset = TIMEZONE.get(tz);
                            break;
                        }
                    } else {

                    }
                }
            }
            // 111111111122222222223333333333444444444455555555556666666666
            // 0123456789012345678901234567890123456789012345678901234567890123456789
            // 0300 AM NON-TSTM WND GST 2 SSW NEWPORT 44.61N 124.07W
            if (tzFound && (currPos < parts.size())) {
                for (; currPos < parts.size(); currPos++) {
                    InternalReport r = parts.get(currPos);
                    if (InternalType.TIME.equals(r.getLineType())) {
                        // We have the Time part. It contains the other
                        // relevant parts of the report.
                        LocalStormReport rpt = new LocalStormReport();
                        String s = r.getReportLine();

                        if (parseTimeLine(s, rpt)) {
                            List<InternalReport> rptLines = r.getSubLines();
                            if (rptLines != null) {
                                r = rptLines.get(0);
                                if (InternalType.DATE.equals(r.getLineType())) {
                                    s = r.getReportLine();
                                    if (parseDateLine(s, rpt)) {
                                        // Now check the remarks section.
                                        parseRemarks(rptLines, rpt);
                                        rpt.setWmoHeader(wmoHeader
                                                .getWmoHeader());
                                        rpt.setOfficeid(officeid);
                                        rpt.setTraceId(traceId);

                                        reports.add(rpt);
                                    }
                                } else {
                                    logger.error("Date Line expected");
                                }
                            }
                        }
                    }
                }
            }
        }
        return reports;
    }

    private boolean parseTimeLine(String timeLine, LocalStormReport rpt) {
        boolean timeOk = false;

        if (timeLine != null) {
            String ss = timeLine.substring(TIME, TIME + TIME_LENGTH).trim();
            if (ss.length() == TIME_LENGTH) {
                hour = Integer.parseInt(ss.substring(0, 2));
                minute = Integer.parseInt(ss.substring(2, 4));
                if ("AM".equals(ss.substring(5, 7))) {
                    if (hour == 12) {
                        hour = 0;
                    }
                    timeOk = true;
                } else if ("PM".equals(ss.substring(5, 7))) {
                    if (hour != 12) {
                        hour += 12;
                    }
                    timeOk = true;
                } else {
                    timeOk = false;
                }
            }
            ss = timeLine.substring(EVENT, EVENT + EVENT_LENGTH).trim();
            rpt.setEventType(LSREventType.lookup(ss));

            ss = timeLine.substring(LOCATION, LOCATION + LOCATION_LENGTH)
                    .trim();
            rpt.setCityLoc(ss);

            parseLatLon(timeLine, rpt);
        }
        return timeOk;
    }

    private boolean parseDateLine(String dateLine, LocalStormReport rpt) {
        int year;
        int month;
        int day;

        boolean timeOk = false;

        if (dateLine != null) {

            String ss = dateLine.substring(DATE, DATE + DATE_LENGTH).trim();
            if (ss.length() == DATE_LENGTH) {
                try {
                    month = Integer.parseInt(ss.substring(0, 2));
                    day = Integer.parseInt(ss.substring(3, 5));
                    year = Integer.parseInt(ss.substring(6, 10));
                    // This is just a gross check. We'll do better later.
                    if ((month >= 1) && (month <= 12)) {
                        if ((day >= 1) && (day <= 31)) {
                            if (year > 2000) {
                                timeOk = true;

                                Calendar c = TimeTools.getBaseCalendar(year,
                                        month, day);
                                c.set(Calendar.HOUR_OF_DAY, hour);
                                c.set(Calendar.MINUTE, minute);
                                c.add(Calendar.HOUR_OF_DAY, tzOffset);
                                rpt.setDataTime(new DataTime(TimeTools.copy(c)));
                            }
                        }
                    }
                    // just get everything from magnitude to county
                    ss = dateLine.substring(MAG, COUNTY);
                    parseMagnitude(ss, rpt);

                    ss = dateLine.substring(COUNTY, COUNTY + COUNTY_LENGTH)
                            .trim();
                    rpt.setCountyLoc(ss);

                    ss = dateLine.substring(STATE, STATE + STATE_LENGTH).trim();
                    rpt.setStateLoc(ss);

                    ss = dateLine.substring(SOURCE).trim();
                    rpt.setSource(ss);
                } catch (Exception e) {
                    logger.error("Bad line " + dateLine);
                }
            }
        }
        return timeOk;
    }

    private boolean parseLatLon(String latlon, LocalStormReport rpt) {
        boolean locOk = false;
        Matcher m = LATLON_PTRN.matcher(latlon);
        if (m.find()) {
            String ss = m.group(2);
            Double lat = Double.parseDouble(ss.substring(0, ss.length() - 1));
            if (ss.endsWith("S")) {
                lat *= -1;
            }

            ss = m.group(4);
            Double lon = Double.parseDouble(ss.substring(0, ss.length() - 1));
            if (ss.endsWith("W")) {
                lon *= -1;
            }

            SurfaceObsLocation loc = new SurfaceObsLocation("LSR");
            loc.assignLocation(lat, lon);
            loc.setElevation(PDV_FILL_INT);
            rpt.setLocation(loc);
            locOk = true;
        }

        return locOk;
    }

    /**
     * Parse the magnitude and units information.
     * 
     * @param magData
     *            String containing the magnitude data.
     * @param rpt
     *            Report to receive the magnitude data.
     */
    private void parseMagnitude(String magData, LocalStormReport rpt) {

        // static const int LSR_MAG = 12; // magnitude
        // static const int LSR_MAG_LENGTH = 3; // magnitude max length
        // static const int LSR_EMAG_LENGTH = 4; // magnitude max length
        // static const int LSR_UNIT = 16; // unit
        // static const int LSR_EUNIT = 17; // emag unit
        // static const int LSR_UNIT_LENGTH = 4; // unit length

        // 111111111122222222223
        // 0123456789012345678901234567890
        // 10/13/2009 M45 MPH GMZ175 TX BUOY020
        // 10/14/2009 M56 MPH PLATTE WY DEPT OF HIGHWAYS
        // 10/13/2009 M0.31 INCH EL DORADO CA TRAINED SPOTTER
        // 10/13/2009 E3.0 INCH MONO CA TRAINED SPOTTER
        //
        //
        //
        // 0 = unknown
        // 1 = estimated
        // 2 = measured
        // 3 =
        // 4 =
        String magQual = null;
        String magUnit = null;
        if (magData != null) {
            if (magData.startsWith("E") || magData.startsWith("M")
                    || magData.startsWith("U")) {
                magQual = magData.substring(0, 1);
                magData = magData.substring(1);
                int pos = magData.indexOf(' ');
                magUnit = magData.substring(pos + 1).trim();
                magData = magData.substring(0, pos);

                try {
                    float val = Float.parseFloat(magData);
                    rpt.setMagnitude(val);
                    switch (magQual.charAt(0)) {
                    case 'E': {
                        rpt.setMagQual(1);
                        break;
                    }
                    case 'M': {
                        rpt.setMagQual(2);
                        break;
                    }
                    case 'U':
                    default: {
                        rpt.setMagQual(0);
                        break;
                    }
                    }
                } catch (NumberFormatException nfe) {
                    logger.info("Unknown magnitude value " + magData);
                }
                String eventUnits = rpt.getEventType().getEventUnits()
                        .toString();
                if (!eventUnits.equals(magUnit)) {
                    logger.info(traceId + "- Units do not match [" + magUnit
                            + "|" + eventUnits + "]");
                }
            } else if (magData.startsWith("F")) {
                // Tornado fujita scale data.
                rpt.setMagQual(0);
                try {
                    magData = magData.substring(1);
                    float val = Float.parseFloat(magData);
                    rpt.setMagnitude(val);
                } catch (NumberFormatException nfe) {
                    // This may not be an error. Not sure what all of the
                    // magnitude values look like yet. E.g. Fujita scale!
                    logger.info("Unknown magnitude value " + magData);
                }
            }
        }
    }

    /**
     * 
     * @param currPos
     * @param rptLines
     * @param rpt
     * @return
     */
    private void parseRemarks(List<InternalReport> rptLines,
            LocalStormReport rpt) {

        List<InternalReport> remarks = new ArrayList<InternalReport>();
        for (InternalReport r : rptLines) {
            logger.debug(r.toString());
            if (InternalType.REMARK.equals(r.getLineType())) {
                logger.debug("Adding " + r);
                remarks.add(r);
            }
        }
        if (remarks.size() > 0) {
            StringBuilder sb = new StringBuilder();
            for (InternalReport r : remarks) {
                if (sb.length() > 0) {
                    sb.append("\n");
                }
                String rmk = r.getReportLine().trim();
                // Check each line just in the event someone did it wrong.
                Matcher m = FATAL_INJ_PTRN.matcher(rmk);
                if (m.find()) {
                    int n;
                    if ("FATAL".equals(m.group(3))) {
                        try {
                            n = Integer.parseInt(m.group(2));
                            rpt.setFatalities(n);
                        } catch (NumberFormatException nfe) {
                            logger.equals("FATAL remark ill-formed " + rmk);
                        }
                    }
                    if ("INJ".equals(m.group(6))) {
                        try {
                            n = Integer.parseInt(m.group(5));
                            rpt.setInjuries(n);
                        } catch (NumberFormatException nfe) {
                            logger.equals("INJ remark ill-formed " + rmk);
                        }
                    }
                }
                sb.append(rmk);
            } // for
            rpt.setRemarks(sb.toString());
        }
    }
}
