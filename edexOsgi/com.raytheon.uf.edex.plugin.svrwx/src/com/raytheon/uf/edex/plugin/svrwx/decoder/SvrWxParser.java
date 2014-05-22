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
package com.raytheon.uf.edex.plugin.svrwx.decoder;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.svrwx.SvrWxRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.plugin.svrwx.SvrWxRecordDao;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * SvrWx Parser
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2010            jsanchez    Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Apr 07, 2014 2971       skorolev    Add condition to avoid malformed parts in the message.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class SvrWxParser {

    /** The logger */
    private static IUFStatusHandler logger = UFStatus
            .getHandler(SvrWxParser.class);

    private final PointDataDescription pointDataDescription;

    private final SvrWxRecordDao svrWxDao;

    private final Map<File, PointDataContainer> containerMap;

    private WMOHeader wmoHeader;

    private final String pluginName;

    private String traceId;

    int currentReport = -1;

    private final HashMap<String, Boolean> URI_MAP = new HashMap<String, Boolean>();

    private List<SvrWxRecord> reports;

    private String eventKey;

    private DataTime refTime;

    private String greenTime;

    private int month;

    private int year;

    private String remarks;

    private Float latitude;

    private Float longitude;

    private String stationId;

    private String reportType;

    public static final String TORN = "*TORN";

    public static final String WNDG = "WNDG";

    private static final HashMap<String, Integer> MONTH_MAP = new HashMap<String, Integer>();

    /** List of lines with non-parsed location. */
    private List<String> badLines;

    private static final Pattern EVENT_KEY_PTRN = Pattern
            .compile(InternalReport.EVENT_KEY);

    private static final Pattern DATE_TIME_PTRN = Pattern
            .compile(InternalReport.TIME);

    private static final Pattern LAT_LON_PTRN = Pattern
            .compile(InternalReport.LATLON);

    private static final Pattern STATION_ID_PTRN = Pattern
            .compile(InternalReport.STATIONID);

    private static final Pattern yearPtrn = Pattern.compile("\\d{4,4}");

    private static final Pattern monthPtrn = Pattern
            .compile("(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)");

    static {
        MONTH_MAP.put("JAN", 1);
        MONTH_MAP.put("FEB", 2);
        MONTH_MAP.put("MAR", 3);
        MONTH_MAP.put("APR", 4);
        MONTH_MAP.put("MAY", 5);
        MONTH_MAP.put("JUN", 6);
        MONTH_MAP.put("JUL", 7);
        MONTH_MAP.put("AUG", 8);
        MONTH_MAP.put("SEP", 9);
        MONTH_MAP.put("OCT", 10);
        MONTH_MAP.put("NOV", 11);
        MONTH_MAP.put("DEC", 12);
    }

    /**
     * SvrWx Parser.
     * 
     * @param dao
     * @param pdd
     * @param name
     */
    public SvrWxParser(SvrWxRecordDao dao, PointDataDescription pdd, String name) {
        pointDataDescription = pdd;
        svrWxDao = dao;
        pluginName = name;
        containerMap = new HashMap<File, PointDataContainer>();
    }

    /**
     * Set the message data and decode all message reports.
     * 
     * @param message
     *            Raw message data.
     * @param traceId
     *            Trace id for this data.
     * @param headers
     */
    public void setData(byte[] message, String traceId, Headers headers) {
        currentReport = -1;
        badLines = new ArrayList<String>();
        this.traceId = traceId;
        wmoHeader = new WMOHeader(message, headers);
        if (wmoHeader != null) {
            reports = findReports(message);
            if (!badLines.isEmpty()) {
                StringBuilder warnMsg = new StringBuilder("Message ");
                warnMsg.append(wmoHeader);
                warnMsg.append(" skipped lines:");
                for (String s : badLines) {
                    warnMsg.append("\nUnrecognized location: ");
                    warnMsg.append(s);
                }
                logger.warn(warnMsg.toString());
                badLines.clear();
            }
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
    public SvrWxRecord next() {

        SvrWxRecord report = null;
        if (currentReport < 0) {
            return report;
        }
        if (currentReport >= reports.size()) {
            reports = null;
            currentReport = -1;
        } else {
            report = reports.get(currentReport++);
            logger.debug("Getting report " + report);
            try {
                report.constructDataURI();
                if (URI_MAP.containsKey(report.getDataURI())) {
                    report = null;
                } else {
                    URI_MAP.put(report.getDataURI(), Boolean.TRUE);
                }
            } catch (PluginException e) {
                logger.error(traceId + "- Unable to construct dataURI", e);
                report = null;
            }
            if (report != null) {

                PointDataContainer pdc = getContainer(report);

                // Populate the point data.
                PointDataView view = pdc.append();

                view.setFloat("latitude", (float) report.getLatitude());
                view.setFloat("longitude", (float) report.getLongitude());
                view.setString("wmoHeader", report.getWmoHeader());
                view.setString("dataURI", report.getDataURI());
                view.setString("eventKey", report.getEventKey());
                view.setString("details", report.getDetails());
                view.setString("greenTime", report.getGreenTime());

                report.setPointDataView(view);
            }
        }
        return report;
    }

    /**
     * Gets Container
     * 
     * @param obsData
     * @return
     */
    private PointDataContainer getContainer(SvrWxRecord obsData) {

        File file = svrWxDao.getFullFilePath(obsData);
        PointDataContainer container = containerMap.get(file);
        if (container == null) {
            container = PointDataContainer.build(pointDataDescription);
            containerMap.put(file, container);
        }
        return container;
    }

    /**
     * Collect Reports from svrWx Records.
     * 
     * @param message
     * @return reports
     */
    private List<SvrWxRecord> findReports(byte[] message) {

        List<SvrWxRecord> reports = new ArrayList<SvrWxRecord>();

        List<InternalReport> parts = InternalReport.identifyMessage(message);
        if (parts != null) {
            SvrWxRecord svrWxRecord;
            clearData();
            for (InternalReport rpt : parts) {
                switch (rpt.getLineType()) {
                case TIME_RANGE:
                    parseTimeRangeLine(rpt.getReportLine());
                    break;
                case REPORT_TYPE:
                    if ((reportType != null && eventKey != null)
                            && isStationOk()) {
                        SurfaceObsLocation location = new SurfaceObsLocation(
                                stationId);
                        location.setLongitude(longitude.doubleValue());
                        location.setLatitude(latitude.doubleValue());
                        svrWxRecord = new SvrWxRecord();
                        svrWxRecord.setReportType(reportType);
                        svrWxRecord.setGreenTime(greenTime);
                        svrWxRecord.setLocation(location);
                        svrWxRecord.setDataTime(refTime);
                        svrWxRecord.setEventKey(eventKey);
                        svrWxRecord.setDetails(getDetails());
                        reports.add(svrWxRecord);
                    }
                    clearData();
                    break;
                case EVENT_LN:
                    if ((reportType != null && eventKey != null)
                            && isStationOk()) {
                        SurfaceObsLocation location = new SurfaceObsLocation(
                                stationId);
                        location.setLongitude(longitude.doubleValue());
                        location.setLatitude(latitude.doubleValue());
                        svrWxRecord = new SvrWxRecord();
                        svrWxRecord.setReportType(reportType);
                        svrWxRecord.setGreenTime(greenTime);
                        svrWxRecord.setLocation(location);
                        svrWxRecord.setDataTime(refTime);
                        svrWxRecord.setEventKey(eventKey);
                        svrWxRecord.setDetails(getDetails());
                        reports.add(svrWxRecord);
                    }
                    clearData();
                    parseEventKeyLine(rpt.getReportLine());
                    break;
                case REMARKS:
                    parseRemarksLine(rpt.getReportLine());
                    break;
                case EXTRA:
                    String s = rpt.getReportLine().trim();
                    if ((s.length() != 0) && (remarks != null)) {
                        remarks += " " + s;
                    }
                    if (s.length() != 0 && eventKey != null && !isStationOk()) {
                        badLines.add(s);
                    }
                    break;
                case END:
                    if ((reportType != null && eventKey != null)
                            && isStationOk()) {
                        SurfaceObsLocation location = new SurfaceObsLocation(
                                stationId);
                        location.setLongitude(longitude.doubleValue());
                        location.setLatitude(latitude.doubleValue());
                        svrWxRecord = new SvrWxRecord();
                        svrWxRecord.setReportType(reportType);
                        svrWxRecord.setGreenTime(greenTime);
                        svrWxRecord.setLocation(location);
                        svrWxRecord.setDataTime(refTime);
                        svrWxRecord.setDetails(getDetails());
                        svrWxRecord.setEventKey(eventKey);
                        reports.add(svrWxRecord);
                    }
                    clearData();
                    break;
                }
            }
        }
        return reports;
    }

    /**
     * Parse Time Range Line.
     * 
     * @param timeRangeLine
     */
    private void parseTimeRangeLine(String timeRangeLine) {
        Matcher m = monthPtrn.matcher(timeRangeLine);
        if (m.find()) {
            month = MONTH_MAP.get(m.group());
        }
        m = yearPtrn.matcher(timeRangeLine);
        if (m.find()) {
            year = Integer.parseInt(m.group());
        }
    }

    /**
     * Parse Event Key Line.
     * 
     * @param eventKeyLine
     */
    private void parseEventKeyLine(String eventKeyLine) {
        Matcher m = EVENT_KEY_PTRN.matcher(eventKeyLine);
        if (m.find()) {
            String type = m.group();
            if (type.equals(TORN)) {
                eventKey = reportType = "T";
            } else if (type.equals(WNDG)) {
                eventKey = reportType = "W";
            } else if (type.startsWith("G")) {
                eventKey = type.replace(" ", "");
                reportType = "W";
            } else if (type.startsWith("A")) {
                eventKey = type.replace(" ", "");
                reportType = "A";
            }
        }

        m = DATE_TIME_PTRN.matcher(eventKeyLine);
        if (m.find()) {
            String time = m.group();
            greenTime = time.replace("/", ".");
            String[] parts = time.split("/");
            int day = Integer.parseInt(parts[0]);
            int hour = Integer.parseInt(parts[1].substring(0, 2)) + 6; // CST to
                                                                       // GMT
            int minute = Integer.parseInt(parts[1].substring(2));

            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.set(Calendar.YEAR, year);
            cal.set(Calendar.MONTH, month - 1);
            cal.set(Calendar.DAY_OF_MONTH, day);
            cal.set(Calendar.HOUR_OF_DAY, hour);
            cal.set(Calendar.MINUTE, minute);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);
            cal.setTimeZone(TimeZone.getTimeZone("GMT"));
            refTime = new DataTime(cal);
        } else {
            refTime = null;
        }

    }

    /**
     * Parse Remarks Line.
     * 
     * @param remarksLine
     */
    private void parseRemarksLine(String remarksLine) {
        Matcher m = LAT_LON_PTRN.matcher(remarksLine);
        if (m.find()) {
            String latLon = m.group();
            String latStr = latLon.substring(0, 4);
            String lonStr = latLon.substring(4).trim();
            latitude = new Float(latStr) / 100;
            longitude = Float.parseFloat(lonStr) / -100;
        }

        m = STATION_ID_PTRN.matcher(remarksLine);
        if (m.find()) {
            stationId = m.group();
        }

        if (stationId != null) {
            remarks = remarksLine.substring(0, remarksLine.indexOf(stationId))
                    .trim();
        }
    }

    /**
     * Clear SvrWx Record.
     */
    private void clearData() {
        eventKey = null;
        refTime = null;
        remarks = null;
        stationId = null;
        latitude = null;
        longitude = null;
        greenTime = null;
    }

    /**
     * Get details of SvrWx Record.
     * 
     * @return details
     */
    private String getDetails() {
        String details = eventKey + " " + greenTime + ":";
        if (stationId != null) {
            details += " " + stationId;
        }
        details += " " + remarks;
        return details;
    }

    /**
     * Checks if parsed location is valid. If it returns false it indicates the
     * station or latitude/longitude was not parsed from the product since it
     * didn't match.
     * 
     * @return true if location is valid.
     */
    private boolean isStationOk() {
        return longitude != null && latitude != null && stationId != null;
    }
}
