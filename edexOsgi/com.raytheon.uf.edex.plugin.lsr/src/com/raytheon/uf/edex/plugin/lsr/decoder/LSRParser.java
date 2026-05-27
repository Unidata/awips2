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
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.exception.UnrecognizedDataException;
import com.raytheon.uf.common.dataplugin.lsr.LocalStormReport;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.plugin.lsr.LocalStormReportDao;

/**
 * Local Storm Report parser
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Jan 21, 2009  1939        jkorman      Initial creation
 * Aug 30, 2013  2298        rjpeter      Make getPluginName abstract
 * Dec 09, 2013  2581        njensen      Reuse patterns for efficiency Check
 *                                        entire time line looking for latlon
 * Jan 07, 2013  2581        njensen      Check to end of string for source, not
 *                                        a set length
 * Jan 13, 2013  2581        njensen      Improved error handling and logging
 * May 14, 2014  2536        bclement     moved WMO Header to common, removed
 *                                        TimeTools usage
 * Jul 23, 2014  3410        bclement     location changed to floats
 * Jul 30, 2014  3410        bclement     lat, lon and data uri moved to
 *                                        database point data desc
 * Sep 16, 2014  2707        bclement     removed event type from PDV, generated
 *                                        stationId
 * Mar 12, 2018  6824        randerso     Improved parsing using regex and Java
 *                                        DataTimeFormatter
 * Mar 19, 2018  7246        randerso     Restructured parser to do all the work
 *                                        in findReports and removed the
 *                                        next/hasNext methods
 * Sep 23, 2021  8608        mapeters     Handle PDO.traceId changes
 *
 * </pre>
 *
 * @author jkorman
 */
public class LSRParser {

    private static final int PDV_FILL_INT = -9999;

    private static final Pattern TIME_LINE_PTRN = Pattern.compile(
            "(?<time>(?:0[1-9]|1[0-2])[0-5][0-9] [AP]M ) {4}(?<event>.{16}) (?<location>.{23}) +(?<latLon>.*)");

    private static final Pattern DATE_LINE_PTRN = Pattern.compile(
            "(?<date>(?:0[1-9]|1[0-2])/[0-3][0-9]/\\d{4}) {2}(?<magnitude>.{16}) (?<county>.{19})(?<st>\\p{Upper}{2}) +(?<source>.*)");

    private static final Pattern LATLON_PTRN = Pattern.compile(
            "(?<latitude>(?:[0-8][0-9]|90).\\d{2,2})(?<northSouth>[NS]) +(?<longitude>1?\\d{2,2}.\\d{2,2})(?<eastWest>[EW])");

    private static final Pattern MAG_PTRN = Pattern
            .compile("(?<qualifier>[EMUF])(?<value>\\d+\\.?\\d*) (?<units>.*)");

    private static final Pattern RPT_DT_PTRN = InternalReport.DATETIME;

    private static final Pattern FATAL_INJ_PTRN = Pattern.compile(
            "\\*\\*\\*( (\\d*) (FATAL))?,?( (\\d*) (INJ))? \\*\\*\\*(.*)");

    private static final DateTimeFormatter RPT_DT_FORMAT = new DateTimeFormatterBuilder()
            .parseCaseInsensitive().appendPattern("hmm a z EEE MMM d y")
            .toFormatter();

    /** The logger */
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(LSRParser.class);

    private final PointDataDescription pointDataDescription;

    private final LocalStormReportDao lsrDao;

    private final Map<File, PointDataContainer> containerMap;

    private WMOHeader wmoHeader;

    private String traceId;

    private final Map<String, LocalStormReport> URI_MAP = new HashMap<>();

    private List<LocalStormReport> reports;

    /**
     * Constructor
     *
     * @param dao
     * @param pdd
     */
    public LSRParser(LocalStormReportDao dao, PointDataDescription pdd) {
        pointDataDescription = pdd;
        lsrDao = dao;
        containerMap = new HashMap<>();
    }

    /**
     * Set the message data and decode all message reports.
     *
     * @param message
     *            Raw message data.
     * @param traceId
     *            Trace id for this data.
     * @param headers
     *            The message headers
     */
    public void setData(byte[] message, String traceId, Headers headers) {
        this.traceId = traceId;
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        wmoHeader = new WMOHeader(message, fileName);
        if (wmoHeader != null) {
            reports = findReports(message);
        } else {
            logger.error(traceId + "- Missing or invalid WMOHeader");
        }
    }

    /**
     * @return the list of LocalStormReports
     */
    public List<LocalStormReport> getReports() {
        return this.reports;
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

        List<LocalStormReport> reports = new ArrayList<>();

        List<InternalReport> parts = InternalReport.identifyMessage(message);
        if (parts != null) {
            ZonedDateTime productDateTime = null;
            String reportTime = null;
            int currPos = 0;
            for (; currPos < parts.size(); currPos++) {
                InternalReport r = parts.get(currPos);
                if (InternalType.DATETIME_ZONE.equals(r.getLineType())) {
                    Matcher m = RPT_DT_PTRN.matcher(r.getReportLine());
                    if (m.find()) {
                        try {
                            productDateTime = ZonedDateTime.parse(m.group(),
                                    RPT_DT_FORMAT);
                            break;
                        } catch (DateTimeParseException e) {
                            logger.error(String.format(
                                    "Unable to parse date/time string: %s",
                                    m.group()), e);
                        }
                    }
                }
            }

            if (productDateTime != null && (currPos < parts.size())) {
                for (; currPos < parts.size(); currPos++) {
                    InternalReport r = parts.get(currPos);
                    if (InternalType.TIME.equals(r.getLineType())) {
                        /*
                         * We have the Time part. It contains the other relevant
                         * parts of the report.
                         */
                        try {
                            LocalStormReport rpt = new LocalStormReport();
                            rpt.setWmoHeader(wmoHeader.getWmoHeader());
                            rpt.setOfficeid(wmoHeader.getCccc());
                            rpt.setSourceTraceId(traceId);

                            String timeLine = r.getReportLine();
                            reportTime = parseTimeLine(timeLine, rpt);

                            List<InternalReport> rptLines = r.getSubLines();
                            if (rptLines == null || rptLines.isEmpty()) {
                                continue;
                            }

                            r = rptLines.get(0);
                            if (!InternalType.DATE.equals(r.getLineType())) {
                                logger.error("Date Line expected, got "
                                        + r.getLineType() + "\n"
                                        + r.getReportLine());
                                continue;
                            }

                            String dateLine = r.getReportLine();
                            String reportDate = parseDateLine(dateLine, rpt);
                            if (reportTime != null && reportDate != null) {
                                DateTimeFormatter dtf = DateTimeFormatter
                                        .ofPattern("hmm a M/d/yyyy");
                                LocalDateTime ldt = LocalDateTime
                                        .parse(reportTime + reportDate, dtf);
                                if (productDateTime != null) {
                                    ZonedDateTime dateTime = ldt
                                            .atZone(productDateTime.getZone());
                                    Date date = Date.from(dateTime.toInstant());
                                    rpt.setDataTime(new DataTime(date));
                                }

                                // Now check the remarks section.
                                parseRemarks(rptLines, rpt);

                                if (rpt.getLocation() == null) {
                                    logger.warn(traceId
                                            + " - Discarding report with bad location: "
                                            + rpt);
                                } else {
                                    if (URI_MAP.containsKey(rpt.getDataURI())) {
                                        logger.warn(traceId
                                                + "- Duplicate record discarded:\n Original: "
                                                + URI_MAP.get(rpt.getDataURI())
                                                + "\nDuplicate: " + rpt);
                                    } else {
                                        URI_MAP.put(rpt.getDataURI(), rpt);
                                        PointDataContainer pdc = getContainer(
                                                rpt);

                                        // Populate the point data.
                                        PointDataView view = pdc.append();
                                        view.setString("wmoHeader",
                                                rpt.getWmoHeader());

                                        view.setString("eventUnit",
                                                rpt.getEventUnits());
                                        view.setFloat("magnitude",
                                                rpt.getMagnitude());
                                        view.setString("countylocation",
                                                rpt.getCountyLoc());
                                        view.setString("statelocation",
                                                rpt.getStateLoc());
                                        view.setString("citylocation",
                                                rpt.getCityLoc());
                                        view.setString("remarks",
                                                rpt.getRemarks());
                                        view.setString("source",
                                                rpt.getSource());
                                        view.setInt("injuries",
                                                rpt.getInjuries());
                                        view.setInt("fatalities",
                                                rpt.getFatalities());

                                        rpt.setPointDataView(view);

                                        reports.add(rpt);
                                    }
                                }
                            }
                        } catch (UnrecognizedDataException e) {
                            logger.error("Error decoding, skipping this entry",
                                    e);
                        }
                    }
                }
            }
        }
        return reports;

    }

    private String parseTimeLine(String timeLine, LocalStormReport rpt)
            throws UnrecognizedDataException {

        if (timeLine != null) {
            Matcher m = TIME_LINE_PTRN.matcher(timeLine);
            if (m.matches()) {
                String time = m.group("time");
                String event = m.group("event");
                String location = m.group("location");
                String latLon = m.group("latLon");

                rpt.setEventType(event.trim());
                rpt.setCityLoc(location.trim());

                parseLatLon(latLon, rpt);
                return time;
            }
        }

        throw new UnrecognizedDataException(
                "Unparsable time line found: \n" + timeLine);
    }

    private String parseDateLine(String dateLine, LocalStormReport rpt)
            throws UnrecognizedDataException {

        if (dateLine != null) {
            Matcher m = DATE_LINE_PTRN.matcher(dateLine);
            if (m.matches()) {
                String date = m.group("date");
                String magnitude = m.group("magnitude");
                String county = m.group("county");
                String st = m.group("st");
                String source = m.group("source");

                parseMagnitude(magnitude, rpt);
                rpt.setCountyLoc(county.trim());
                rpt.setStateLoc(st);
                rpt.setSource(source.trim());

                return date;
            }
        }

        throw new UnrecognizedDataException(
                "Unparsable date line found: \n" + dateLine);
    }

    private boolean parseLatLon(String latlon, LocalStormReport rpt) {
        boolean locOk = false;
        Matcher m = LATLON_PTRN.matcher(latlon);
        if (m.find()) {
            try {
                float lat = Float.parseFloat(m.group("latitude"));
                if ("S".equals(m.group("northSouth"))) {
                    lat *= -1;
                }

                float lon = Float.parseFloat(m.group("longitude"));
                if ("W".equals(m.group("eastWest"))) {
                    lon *= -1;
                }

                SurfaceObsLocation loc = new SurfaceObsLocation();
                loc.assignLocation(lat, lon);
                loc.setElevation(PDV_FILL_INT);
                loc.generateCoordinateStationId();
                rpt.setLocation(loc);
                locOk = true;
            } catch (NumberFormatException e) {
                logger.error("Invalid lat/lon: " + latlon);
            }
        }

        return locOk;
    }

    /**
     * Parse the magnitude and units information.
     *
     * @param magnitude
     *            String containing the magnitude data.
     * @param rpt
     *            Report to receive the magnitude data.
     */
    private void parseMagnitude(String magnitude, LocalStormReport rpt) {

        Matcher m = MAG_PTRN.matcher(magnitude);
        if (magnitude != null) {
            if (m.matches()) {
                String qualifier = m.group("qualifier");
                String value = m.group("value");
                String units = m.group("units").trim();

                switch (qualifier.charAt(0)) {
                case 'E': // Estimated
                    rpt.setMagQual(1);
                    break;

                case 'M': // Measured
                    rpt.setMagQual(2);
                    break;

                case 'F': // Fujita
                case 'U': // Unknown
                default:
                    rpt.setMagQual(0);
                    break;

                }

                try {
                    float val = Float.parseFloat(value);
                    rpt.setMagnitude(val);
                } catch (NumberFormatException nfe) {
                    logger.info("Unparsable magnitude value " + magnitude);
                }

                if (!units.isEmpty()) {
                    rpt.setEventUnits(units);
                }

            }
        }
    }

    private void parseRemarks(List<InternalReport> rptLines,
            LocalStormReport rpt) {

        List<InternalReport> remarks = new ArrayList<>();
        for (InternalReport r : rptLines) {
            logger.debug(r.toString());
            if (InternalType.REMARK.equals(r.getLineType())) {
                logger.debug("Adding " + r);
                remarks.add(r);
            }
        }
        if (!remarks.isEmpty()) {
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
