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
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.svrwx.SvrWxRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.plugin.svrwx.SvrWxRecordDao;

/**
 * SvrWx Parser
 * 
 * 
 * <pre>
 * 
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2010            jsanchez    Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Apr 07, 2014 2971       skorolev    Add condition to avoid malformed parts in the message.
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed pluginName
 * Jun 25, 2014 3008       nabowle     Refactor for EventReport type
 * Jul 23, 2014 3410       bclement    location changed to floats
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

    private String traceId;

    int currentReport = -1;

    private final HashMap<String, Boolean> URI_MAP = new HashMap<String, Boolean>();

    private List<SvrWxRecord> reports;

    public static final String TORN = "*TORN";

    public static final String WNDG = "WNDG";

    private static final HashMap<String, Integer> MONTH_MAP = new HashMap<String, Integer>();

    private static final Pattern YEAR_PTRN = Pattern.compile("\\d{4,4}");

    private static final Pattern MONTH_PTRN = Pattern
            .compile("(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)");

    private static final String[] EMPTY_ARR = new String[0];

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
        this.traceId = traceId;
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        wmoHeader = new WMOHeader(message, fileName);
        if (wmoHeader != null) {
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
            if (URI_MAP.containsKey(report.getDataURI())) {
                report = null;
            } else {
                URI_MAP.put(report.getDataURI(), Boolean.TRUE);
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
            EventReport eRpt;
            String[] missingFields;
            int month = -1;
            int year = -1;
            int reportCount = 0;
            int invalidCount = 0;
            boolean allDropped = false;
            for (InternalReport rpt : parts) {
                switch (rpt.getLineType()) {
                case TIME_RANGE:
                    month = parseMonth(rpt.getReportLine());
                    year = parseYear(rpt.getReportLine());
                    break;
                case EVENT_REPORT:
                    reportCount++;
                    eRpt = (EventReport) rpt;
                    missingFields = getMissingFields(eRpt);
                    if (missingFields.length == 0 && year != -1 && month != -1) {
                        reports.add(buildRecord(eRpt, month, year));
                    } else {
                        if (year == -1 || month == -1) {
                            if (!allDropped) {
                                logger.warn(this.traceId
                                        + " - No time range found. All records"
                                        + " will be discarded.");
                                allDropped = true;
                            }
                        } else {
                            logInvalidReport(eRpt, missingFields);
                        }
                        invalidCount++;
                    }
                case REPORT_TYPE:
                case EXTRA:
                default:
                    break;
                }
            }

            if (invalidCount > 0) {
                logger.warn("Discarded " + invalidCount + "/" + reportCount
                        + " reports.");
            }
        }
        return reports;
    }

    /**
     * Builds a SvrWxRecord from an EventReport.
     *
     * @param eRpt
     *            The EventReport.
     * @param month
     *            The previously parsed month.
     * @param year
     *            The previously parsed year.
     * @return The constructed SvrWxRecord.
     */
    private SvrWxRecord buildRecord(EventReport eRpt, int month, int year) {
        SurfaceObsLocation location = new SurfaceObsLocation(
                eRpt.getStationId());
        location.setLongitude(getLon(eRpt.getLatLon()));
        location.setLatitude(getLat(eRpt.getLatLon()));

        SvrWxRecord svrWxRecord = new SvrWxRecord();
        svrWxRecord.setReportType(getReportType(eRpt
                .getKey()));
        svrWxRecord.setGreenTime(getGreenTime(eRpt
                .getTime()));
        svrWxRecord.setLocation(location);
        svrWxRecord.setDataTime(getRefTime(eRpt.getTime(),
                month, year));
        svrWxRecord
                .setEventKey(getEventKey(eRpt.getKey()));
        svrWxRecord.setDetails(getDetails(svrWxRecord, eRpt));
        return svrWxRecord;
    }

    /**
     * Logs an EventReport that is invalid.
     *
     * @param eRpt
     *            The EventReport.
     * @param missingFields
     *            The missing fields.
     */
    private void logInvalidReport(EventReport eRpt, String[] missingFields) {
        StringBuilder errorSb = new StringBuilder()
                .append("The following report is missing the required ")
                .append(missingFields.length > 1 ? "fields " : "field ")
                .append(Arrays.toString(missingFields))
                .append(" and will be skipped.\n").append(eRpt.toString());

        logger.warn(errorSb.toString());
    }

    /**
     * Parses the month.
     *
     * @param timeRangeLine
     *            The time range line.
     * @return The month, or 0 if the month cannot be found.
     */
    private int parseMonth(String timeRangeLine) {
        int month;
        Matcher m = MONTH_PTRN.matcher(timeRangeLine);
        if (m.find()) {
            month = MONTH_MAP.get(m.group());
        } else {
            month = 0;
        }
        return month;
    }

    /**
     * Parses the year from the time range line.
     *
     * @param timeRangeLine
     *            The time range line.
     * @return The year, or 0 f the year cannot be found.
     */
    private int parseYear(String timeRangeLine) {
        int year;
        Matcher m = YEAR_PTRN.matcher(timeRangeLine);
        if (m.find()) {
            year = Integer.parseInt(m.group());
        } else {
            year = 0;
        }
        return year;
    }

    /**
     * Gets the green time from the time field.
     *
     * @param time
     *            The time field.
     * @return The green time, or null if the time is null.
     */
    private String getGreenTime(String time) {
        if (time == null) {
            return null;
        }

        return time.replace("/", ".");
    }

    /**
     * Gets the ref time from the time field, month, and year.
     *
     * @param time
     *            The time field.
     * @param month
     *            The month.
     * @param year
     *            The year.
     * @return The ref time, or null if the time field is null.
     */
    private DataTime getRefTime(String time, int month, int year) {
        if (time == null) {
            return null;
        }

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
        return new DataTime(cal);
    }

    /**
     * Gets the report type.
     *
     * @param key
     *            The event key field.
     * @return The report type, or null if the key field is not expected.
     */
    private String getReportType(String key) {
        String reportType;

        if (key.equals(TORN)) {
            reportType = "T";
        } else if (key.equals(WNDG) || key.startsWith("G")) {
            reportType = "W";
        } else if (key.startsWith("A")) {
            reportType = "A";
        } else {
            reportType = null;
        }

        return reportType;
    }

    /**
     * Gets the event key.
     *
     * @param key
     *            The event key field.
     * @return The event key, or null if the event key is not expected.
     */
    private String getEventKey(String key) {
        String eventKey;

        if (key.equals(TORN)) {
            // Tornado
            eventKey = "T";
        } else if (key.equals(WNDG)) {
            // Wind damage
            eventKey = "W";
        } else if (key.startsWith("G") || key.startsWith("A")) {
            // A nnn Hailstones and diameter in inches. 475 would be 4.75 inches
            // G nnn Wind gust and speed in knots
            eventKey = key.replace(" ", "");
        } else {
            eventKey = null;
        }

        return eventKey;
    }

    /**
     * Get the latitude as a float. Northern hemisphere is assumed.
     * 
     * @param latlon
     *            The latitude/longitude String.
     * @return The latitude as a float.
     */
    private float getLat(String latlon) {
        return Float.parseFloat(parseLat(latlon)) / 100.0f;
    }

    /**
     * Get the longitude as a float. Western hemisphere is assumed.
     * 
     * @param latlon
     *            The latitude/longitude String.
     * @return The longitude as a float.
     */
    private float getLon(String latlon) {
        return Float.parseFloat(parseLon(latlon)) / -100.0f;
    }

    /**
     * Parses the latitude from the combined latitude/longitude field.
     *
     * @param latlon
     *            The combined latitude/longitude field.
     * @return The latitude.
     */
    private String parseLat(String latlon) {
        return latlon.substring(0, 4);
    }

    /**
     * Parses the longitude from the combined latitude/longitude field.
     *
     * @param latlon
     *            The combined latitude/longitude field.
     * @return The longitude.
     */
    private String parseLon(String latlon) {
        return latlon.substring(4).trim();
    }

    /**
     * Get the event details.
     *
     * @param record
     *            The SvrWxRecord.
     * @param eRpt
     *            The event report.
     * @return The event details.
     */
    private String getDetails(SvrWxRecord record, EventReport eRpt) {
        StringBuilder details = new StringBuilder()
                .append(record.getEventKey()).append(" ")
                .append(record.getGreenTime()).append(":")
                .append(record.getStationId()).append(" ")
                .append(eRpt.getRemarks());

        return details.toString();
    }

    /**
     * Check for missing fields that are required.
     *
     * @param eRpt
     *            The event report.
     * @return An array of the missing fields' names, or an empty array if no
     *         fields are missing.
     */
    private String[] getMissingFields(EventReport eRpt) {

        List<String> missing = new ArrayList<String>();
        if (eRpt.getStationId() == null) {
            missing.add("StationID");
        }

        if (eRpt.getLatLon() == null) {
            missing.add("Latitude/Longitude");
        }

        return missing.isEmpty() ? EMPTY_ARR : missing.toArray(EMPTY_ARR);
    }
}
