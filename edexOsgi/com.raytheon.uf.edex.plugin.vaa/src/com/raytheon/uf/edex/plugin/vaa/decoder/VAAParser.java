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
package com.raytheon.uf.edex.plugin.vaa.decoder;

import java.io.IOException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.NullUtil;
import com.raytheon.uf.common.dataplugin.exception.MalformedDataException;
import com.raytheon.uf.common.dataplugin.vaa.VAARecord;
import com.raytheon.uf.common.dataplugin.vaa.VAASubPart;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * Parser for Volcanic Ash Advisories
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 05, 2009 3267       jkorman     Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Nov 26, 2013 2582       njensen     Cleanup
 * Feb 11, 2014 2763       skorolev    Made LFCR correction of input data.
 * Mar 10, 2014 2807       skorolev    Added MalformedDataException for VAA decoding.
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * Jul 23, 2014 3410       bclement    location changed to floats
 * Jul 21, 2015 4360       rferrel     The {@link VAARecord}'s advisoryNumber can no longer be null.
 * Sep 23, 2021 8608       mapeters    Handle PDO.traceId changes
 *
 * </pre>
 *
 * @author jkorman
 */
public class VAAParser implements Iterable<VAARecord> {

    private static class LatLon {
        public float lat;

        public float lon;

        @Override
        public String toString() {
            return String.format("[%5.2f %6.2f]", lat, lon);
        }
    }

    public static class VAAShape {
        public List<LatLon> points;

        public String shapeType;
    }

    private static final Pattern COR_P = Pattern.compile("(C[A-Z]{2})( +.*)?");

    private static final Pattern SUMMIT_ELEV_P = Pattern
            .compile("(\\d+) +FT +\\((\\d+) +[Mm]\\)");

    private static final Pattern LINE_P = Pattern.compile("WID +LINE +BTN");

    private static final Pattern DTG_P = Pattern.compile("(\\d{8}/\\d{4})(Z)");

    private static final ThreadLocal<SimpleDateFormat> SDF = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            return new SimpleDateFormat("yyyyMMdd/HHmmZ");
        }
    };

    private final WMOHeader wmoHeader;

    private final String traceId;

    private final List<VAARecord> records = new ArrayList<>();

    /**
     * @param message
     * @param traceId
     * @param headers
     * @throws MalformedDataException
     * @throws IOException
     */
    public VAAParser(byte[] message, String traceId, Headers headers)
            throws MalformedDataException, IOException {
        this.traceId = traceId;
        byte[] msg = correctLFCR(message);
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        wmoHeader = new WMOHeader(msg, fileName);
        setData(msg, headers);
    }

    /**
     * Removes Line Feed and Cartridge Return (LFCR) codes after colon in the
     * message. We correct the line breaks because sometimes the products are
     * distributed with a line break splitting the line from the value, e.g.
     * ERUPTION DETAILS: \r\n CONTINUOUS EMISSIONS vs ERUPTION DETAILS:
     * CONTINUOUS EMISSIONS
     *
     * @param bytes
     * @return bytes corrected
     */
    private static byte[] correctLFCR(byte[] bytes) {

        boolean flagLFCR = false;
        StringBuilder sb = new StringBuilder(
                new String(bytes, 0, bytes.length));
        for (int i = 0; i < sb.length(); i++) {
            if (flagLFCR) {
                if (sb.charAt(i) == '\r' || sb.charAt(i) == '\n') {
                    sb.setCharAt(i, ' ');
                } else {
                    flagLFCR = false;
                }
            }
            if (sb.charAt(i) == ':') {
                flagLFCR = true;
            }
        }
        return String.valueOf(sb).getBytes();
    }

    /**
     *
     */
    @Override
    public Iterator<VAARecord> iterator() {
        return records.iterator();
    }

    /**
     * Set the message data and decode all message reports.
     *
     * @param message
     *            Raw message data.
     * @param headers
     * @throws MalformedDataException
     */
    private void setData(byte[] message, Headers headers)
            throws MalformedDataException {

        List<InternalReport> reports = InternalReport.identifyMessage(message,
                headers);

        VAARecord vaa = new VAARecord();
        vaa.setSourceTraceId(traceId);
        vaa.setWmoHeader(wmoHeader.getWmoHeader());
        vaa.setAdvisoryNumber(NullUtil.NULL_STRING);
        String cor = wmoHeader.getBBBIndicator();
        if (cor != null) {
            Matcher m = COR_P.matcher(cor);
            if (m.find()) {
                vaa.setCorIndicator(m.group(1));
            }
        }

        SurfaceObsLocation loc = new SurfaceObsLocation();
        vaa.setLocation(loc);
        for (InternalReport rpt : reports) {
            switch (rpt.getLineType()) {
            case WMO_HEADER: {
                break;
            }
            case MESSAGE: {
                vaa.setMessage(rpt.getReportLine());
                break;
            }
            case NO_ID:
            case ADVISORY_LEAD: {
                break;
            }
            case MESSAGE_DTG: {
                DataTime dt = parseDTG(rpt.getReportLine());
                vaa.setDataTime(dt);
                break;
            }
            case VAAC_CNTR: {
                vaa.setCenterId(rpt.getReportLine());
                break;
            }
            case VOLCANO_ID: {
                loc.setStationId(rpt.getReportLine());
                break;
            }
            case VOLCANO_PSN: {
                LatLon ll = parseLatLon(rpt.getReportLine());
                if (ll != null) {
                    loc.assignLocation(ll.lat, ll.lon);
                }
                break;
            }
            case GEO_AREA: {

                break;
            }
            case SUMMIT_ELEV: {
                int elev = parseSummitElev(rpt.getReportLine());
                if (elev > -9999) {
                    loc.setElevation(elev);
                }
                break;
            }
            case ADVISORY_NR: {
                vaa.setAdvisoryNumber(rpt.getReportLine());
                break;
            }
            case INFO_SOURCE: {
                break;
            }
            case ERUPTION_DETAIL: {
                break;
            }
            case OBS_DTG: {
                break;
            }
            case OBS: {
                parseAnalData(rpt, vaa);
                break;
            }
            case FCST: {
                parseFcstData(rpt, vaa);
                break;
            }
            case RMKS:
            case NXT_ADVISORY:
            default: {
            }
            } // switch
        } // for
        if (vaa.getDataTime() == null) {
            throw new MalformedDataException(
                    "VAA product does not have a date time group");
        }
        records.add(vaa);
    }

    /**
     *
     * @param latLon
     * @return
     */
    private LatLon parseLatLon(String latLon) {
        LatLon latlon = null;
        Matcher m = InternalReport.LAT_LON_P.matcher(latLon);
        if (m.find()) {
            latlon = new LatLon();
            latlon.lat = Float.parseFloat(m.group(2));
            latlon.lat += (Float.parseFloat(m.group(3)) / 60.0f);
            latlon.lat *= ("S".equals(m.group(1))) ? -1 : 1;

            latlon.lon = Float.parseFloat(m.group(5));
            latlon.lon += (Float.parseFloat(m.group(6)) / 60.0f);
            latlon.lon *= ("W".equals(m.group(4))) ? -1 : 1;
        }
        return latlon;
    }

    /**
     *
     * @param summitElev
     * @return
     */
    private int parseSummitElev(String summitElev) {
        int elevation = -9999;
        Matcher m = SUMMIT_ELEV_P.matcher(summitElev);
        if (m.find()) {
            elevation = Integer.parseInt(m.group(2));
        }
        return elevation;
    }

    private DataTime parseDTG(String dtg) {
        DataTime dt = null;

        // 20091104/1708Z
        if (dtg != null) {
            Matcher m = DTG_P.matcher(dtg);
            if (m.find()) {
                if ("Z".equals(m.group(2))) {
                    dtg = m.group(1) + "GMT";
                }
            }
            ParsePosition pos = new ParsePosition(0);
            Date d = SDF.get().parse(dtg, pos);
            if (pos.getErrorIndex() < 0) {
                dt = new DataTime(d);
            }
        }
        return dt;
    }

    /**
     *
     * @param rpt
     * @param vaa
     * @return
     */
    private void parseAnalData(InternalReport rpt, VAARecord vaa) {
        String rptData = unPack(rpt, false);
        Matcher m = InternalReport.ANAL_P.matcher(rptData);
        if (m.find()) {
            if ("OBS".equals(m.group(1))) {
                vaa.setAnal00Hr(unPack(rpt, true));
            }
        }
        List<VAAShape> features = parseFeature(rptData);
        if (features != null && !features.isEmpty()) {
            for (VAAShape feature : features) {
                VAASubPart part = null;
                String type = feature.shapeType;
                if ("LINE".equals(type)) {
                    part = new VAASubPart();
                    part.setShapeType(type);
                    int index = 0;
                    for (LatLon pos : feature.points) {
                        part.addVertex(pos.lat, pos.lon, index);
                        ++index;
                    }
                } else if ("AREA".equals(type)) {
                    part = new VAASubPart();
                    part.setShapeType(type);
                    int index = 0;
                    for (LatLon pos : feature.points) {
                        part.addVertex(pos.lat, pos.lon, index);
                        ++index;
                    }
                }
                if (part != null) {
                    part.setSubText("ANAL00");
                    vaa.addSubPart(part);
                }
            }
        }
    }

    /**
     *
     * @param rpt
     * @param vaa
     * @return
     */
    private void parseFcstData(InternalReport rpt, VAARecord vaa) {
        String rptData = unPack(rpt, false);
        String fcstPd = null;

        Matcher m = InternalReport.FCST_P.matcher(rptData);
        if (m.find()) {
            if ("FCST".equals(m.group(1))) {
                if ("6".equals(m.group(4))) {
                    vaa.setFcst06Hr(unPack(rpt, true));
                    fcstPd = "FCST06";
                } else if ("12".equals(m.group(4))) {
                    vaa.setFcst12Hr(unPack(rpt, true));
                    fcstPd = "FCST12";
                } else if ("18".equals(m.group(4))) {
                    vaa.setFcst18Hr(unPack(rpt, true));
                    fcstPd = "FCST18";
                }
            }
        }
        List<VAAShape> features = parseFeature(rptData);
        if (features != null && !features.isEmpty()) {
            for (VAAShape feature : features) {
                VAASubPart part = null;
                String type = feature.shapeType;
                if ("LINE".equals(type)) {
                    part = new VAASubPart();
                    part.setShapeType(type);
                    int index = 0;
                    for (LatLon pos : feature.points) {
                        part.addVertex(pos.lat, pos.lon, index);
                        ++index;
                    }
                } else if ("AREA".equals(type)) {
                    part = new VAASubPart();
                    part.setShapeType(type);
                    int index = 0;
                    for (LatLon pos : feature.points) {
                        part.addVertex(pos.lat, pos.lon, index);
                        ++index;
                    }
                }
                if (part != null) {
                    part.setSubText(fcstPd);
                    vaa.addSubPart(part);
                }
            }
        }
    }

    /**
     *
     * @param rptData
     * @return
     */
    private List<VAAShape> parseFeature(String rptData) {
        List<VAAShape> features = new ArrayList<>();

        String[] descriptions = rptData.split("SFC/");
        if ((descriptions != null) && (descriptions.length > 1)) {
            for (String description : descriptions) {
                Matcher m = LINE_P.matcher(description);
                if (m.find()) {
                    // parse as a line
                    m = InternalReport.LAT_LON_P.matcher(description);
                    int pos = 0;
                    List<LatLon> points = new ArrayList<>();
                    while (m.find(pos)) {
                        int start = m.start();
                        int stop = m.end();
                        points.add(parseLatLon(
                                description.substring(start, stop)));
                        pos = stop;
                    }
                    if (points.size() == 2) {
                        VAAShape shape = new VAAShape();
                        shape.shapeType = "LINE";
                        shape.points = points;
                        features.add(shape);
                    }
                } else {
                    // handle as an area
                    m = InternalReport.LAT_LON_P.matcher(description);
                    int pos = 0;
                    List<LatLon> points = new ArrayList<>();
                    while (m.find(pos)) {
                        int start = m.start();
                        int stop = m.end();
                        points.add(parseLatLon(
                                description.substring(start, stop)));
                        pos = stop;
                    }
                    if (points.size() > 3) {
                        VAAShape shape = new VAAShape();
                        shape.shapeType = "AREA";
                        shape.points = points;
                        features.add(shape);
                    }
                }
            }
        }
        return features;
    }

    /**
     *
     * @param rpt
     * @return
     */
    private String unPack(InternalReport rpt, boolean addLineFeed) {
        StringBuilder sb = new StringBuilder(rpt.getReportLine());
        if (rpt.getSubLines() != null) {
            for (InternalReport r : rpt.getSubLines()) {
                if (addLineFeed) {
                    sb.append("\n");
                } else {
                    sb.append(" ");
                }
                sb.append(r.getReportLine());
            }
        }
        return sb.toString().trim();
    }
}
