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
package com.raytheon.uf.edex.plugin.cwa.decoder;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.cwa.CWADimension;
import com.raytheon.uf.common.dataplugin.cwa.CWARecord;
import com.raytheon.uf.common.dataplugin.exception.MalformedDataException;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.cwa.CWARecordDao;
import com.raytheon.uf.edex.plugin.cwa.util.TableLoader;
import com.raytheon.uf.edex.plugin.cwa.util.Utility;
import com.raytheon.uf.edex.wmo.message.WMOHeader;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The CWA text Parser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2010            jsanchez    Initial creation
 * Apr 18, 2012 473        dgilling    Modify parser to set  DataTime based on
 *                                     ingest  file name.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Mar 25, 2014 2930       skorolev    Fixed error in distance.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class CWAParser {
    /** The logger */
    private IUFStatusHandler logger = UFStatus.getHandler(CWAParser.class);

    private final PointDataDescription pointDataDescription;

    private final TableLoader pirepTable;

    private final CWARecordDao cwaDao;

    private final Map<File, PointDataContainer> containerMap;

    private final GeodeticCalculator gc = new GeodeticCalculator();

    private String eventId;

    private String text;

    private double size;

    private DataTime startTime;

    private CWADimension dimension;

    private final List<Coordinate> coordinates = new ArrayList<Coordinate>();

    boolean isVicinity;

    private WMOHeader wmoHeader;

    private String traceId;

    int currentReport = -1;

    private final HashMap<String, Boolean> URI_MAP = new HashMap<String, Boolean>();

    private static HashMap<String, Float> dirToDeg = new HashMap<String, Float>();

    private List<CWARecord> reports;

    private static UnitConverter nauticalmileToMeter = NonSI.NAUTICAL_MILE
            .getConverterTo(SI.METER);

    // Maximum possible distance between reference points in nautical miles.
    private static final double MAX_VOR_DIST = 999.0;

    private Headers headers;

    static {
        dirToDeg.put("N", 0f);
        dirToDeg.put("NNE", 22.5f);
        dirToDeg.put("NE", 45f);
        dirToDeg.put("ENE", 67.5f);
        dirToDeg.put("E", 90f);
        dirToDeg.put("ESE", 112.5f);
        dirToDeg.put("SE", 135f);
        dirToDeg.put("SSE", 157.5f);
        dirToDeg.put("S", 180f);
        dirToDeg.put("SSW", 202.5f);
        dirToDeg.put("SW", 225f);
        dirToDeg.put("WSW", 247.5f);
        dirToDeg.put("W", 270f);
        dirToDeg.put("WNW", 292.5f);
        dirToDeg.put("NW", 315f);
        dirToDeg.put("NNW", 337.5f);
    }

    /**
     * @param dao
     * @param pdd
     * @param name
     * @param pirepTable
     */
    public CWAParser(CWARecordDao dao, PointDataDescription pdd, String name,
            TableLoader pirepTable) {
        pointDataDescription = pdd;
        cwaDao = dao;
        containerMap = new HashMap<File, PointDataContainer>();
        this.pirepTable = pirepTable;
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
    public CWARecord next() {

        CWARecord report = null;
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
                view.setString("wmoHeader", report.getWmoHeader());
                view.setString("dataURI", report.getDataURI());
                view.setString("eventId", report.getEventId());
                view.setString("dimension", report.getDimension().toString());
                view.setString("text", report.getText());

                int index = 0;
                if (report.getCoordinates() != null) {
                    for (Coordinate c : report.getCoordinates()) {
                        view.setFloat("latitudes", (float) c.y, index);
                        view.setFloat("longitudes", (float) c.x, index);
                        index++;
                    }
                }
                view.setInt("numOfPoints", index);
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
    private PointDataContainer getContainer(CWARecord obsData) {

        File file = cwaDao.getFullFilePath(obsData);
        PointDataContainer container = containerMap.get(file);
        if (container == null) {
            container = PointDataContainer.build(pointDataDescription);
            containerMap.put(file, container);
        }
        return container;
    }

    /**
     * @param message
     * @return reports
     * @throws MalformedDataException
     */
    private List<CWARecord> findReports(byte[] message)
            throws MalformedDataException {

        List<CWARecord> reports = new ArrayList<CWARecord>();

        List<InternalReport> parts = InternalReport.identifyMessage(message);
        if (parts != null) {
            for (InternalReport iRpt : parts) {
                String s = iRpt.getReportLine();
                switch (iRpt.getLineType()) {
                case ISSUANCE:
                    if ((eventId != null)
                            && (dimension != CWADimension.CANCELED)) {
                        reports.add(getRecord());
                    }
                    clearData();
                    parseIssuanceInfo(s);
                    break;
                case VALID_TO:
                    parseValidToInfo(s);
                    break;
                case VICINITY:
                    parseVicinityInfo(s);
                    break;
                case CANCEL:
                    dimension = CWADimension.CANCELED;
                    break;
                case COORDS:
                    parseCoordinateInfo(s);
                    break;
                case TEXT:
                    if (eventId != null) {
                        if (text.length() > 0) {
                            text += "\n";
                        }
                        text += s;
                        parseGeometryInfo(s);
                    }
                    break;
                case END:
                    if ((eventId != null)
                            && (dimension != CWADimension.CANCELED)) {
                        reports.add(getRecord());
                    }
                    clearData();
                    break;
                }
            }
        }
        return reports;
    }

    /**
     * Set the message data and decode all message reports.
     * 
     * @param message
     *            Raw message data.
     * @param traceId
     *            Trace id for this data.
     * @param headers
     * @throws MalformedDataException
     */
    public void setData(byte[] message, String traceId, Headers headers)
            throws MalformedDataException {
        currentReport = -1;
        this.traceId = traceId;
        this.headers = headers;
        wmoHeader = new WMOHeader(message, headers);
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
     * @param issuanceInfo
     */
    private void parseIssuanceInfo(String issuanceInfo) {
        String[] parts = issuanceInfo.split(" ");
        if ((parts != null) && (parts[0] != null)) {
            eventId = parts[0];
        }

        Pattern p = Pattern.compile(InternalReport.TIME);
        Matcher m = p.matcher(issuanceInfo);
        if (m.find()) {
            String time = m.group();
            startTime = getDataTime(time);
        }
    }

    /**
     * @param validToInfo
     */
    private void parseValidToInfo(String validToInfo) {
        Pattern p = Pattern.compile(InternalReport.TIME);
        Matcher m = p.matcher(validToInfo);
        if (m.find()) {
            String time = m.group();
            getDataTime(time);
        }
    }

    /**
     * @param vicinityInfo
     */
    private void parseVicinityInfo(String vicinityInfo) {
        Pattern vicinityPtrn = Pattern.compile(InternalReport.VICINITY);
        Matcher m = vicinityPtrn.matcher(vicinityInfo);
        if (m.find()) {
            String vicinity = m.group(2);
            if (pirepTable.contains(vicinity)) {
                coordinates.add(pirepTable.get(vicinity));
            }
            dimension = CWADimension.AREA;
            isVicinity = true;
        }
    }

    /**
     * @param coordinateInfo
     * @throws MalformedDataException
     */
    private void parseCoordinateInfo(String coordinateInfo)
            throws MalformedDataException {
        String tokens[] = coordinateInfo.replace("-", " - ").split(" ");

        boolean getMoreCoords = true;
        float distance = 0;
        float direction = 0;
        for (String tok : tokens) {
            if (tok.equals("CANCEL") || tok.endsWith("CNCL")
                    || tok.endsWith("CNCL") || tok.endsWith("CNLD")
                    || tok.endsWith("CANCELLED") || tok.endsWith("CNL")) {
                dimension = CWADimension.CANCELED;
                break;
            } else if (tok.equals("FROM") || tok.equals("FM")
                    || tok.equals("TO") || tok.equals("-")) {
                getMoreCoords = true;
            } else if (getMoreCoords) {
                getMoreCoords = false;
                Pattern p = Pattern.compile(InternalReport.DIRDIST);
                Matcher m = p.matcher(tok);
                if (m.matches()) {
                    try {
                        distance = Integer.parseInt(m.group(1));
                        direction = dirToDeg.get(m.group(2));
                        getMoreCoords = true;
                        continue;
                    } catch (NumberFormatException e) {
                        logger.error("Bad distance: " + m.group(1));
                        distance = 0;
                        continue;
                    } catch (Exception e) {
                        logger.error("Bad direction: " + m.group(2));
                        direction = 0;
                        continue;
                    }
                }
                Coordinate coord = null;
                if (pirepTable.contains(tok)) {
                    coord = pirepTable.get(tok);
                    if (distance != 0) {
                        this.gc.setStartingGeographicPoint(coord.x, coord.y);
                        if (direction > 180) {
                            direction -= 360;
                        }
                        // According to WSOM:
                        // http://www.nws.noaa.gov/wsom/manual/archives/ND259606.HTML#6.1.2%C2%A0%C2%A0%C2%A0%C2%A0%20CWSU%20Meteorologist%20Responsibilities
                        // ..."When using a distance in CWSU products all references are in nautical miles (NM). "
                        // ..."The " (n)nn". is distance and "DD(D)" is a
                        // 16-point compass direction (e.g., VC IAH or 40NNE
                        // LBB)."
                        if (distance > MAX_VOR_DIST) {
                            throw new MalformedDataException("CWA product "
                                    + wmoHeader.getWmoHeader()
                                    + " has a non-valid distance " + distance
                                    + " NM to " + tok + ".");
                        }
                        distance = (float) nauticalmileToMeter
                                .convert(distance);
                        this.gc.setDirection(direction, distance);
                        coordinates
                                .add(new Coordinate(
                                        this.gc.getDestinationGeographicPoint()
                                                .getX(),
                                        this.gc.getDestinationGeographicPoint()
                                                .getY()));
                    } else {
                        coordinates.add(coord);
                    }
                    dimension = CWADimension.AREA;
                    distance = 0;
                } else if ((tok.length() == 0) || tok.equals("")) {
                    getMoreCoords = true;
                } else {
                    logger.error("Bad location. '" + tok
                            + "' not in the pirepTable.txt");
                }
            }
        }
    }

    /**
     * @param geometryInfo
     */
    private void parseGeometryInfo(String geometryInfo) {
        boolean found = false;
        Pattern lineGeomPtrn = Pattern.compile(InternalReport.LINE_GEOM);
        Pattern lineGeom2Ptrn = Pattern.compile(InternalReport.LINE_GEOM2);
        Pattern areaGeomPtrn = Pattern.compile(InternalReport.AREA_GEOM);
        Pattern pointGeom2Ptrn = Pattern.compile(InternalReport.POINT_GEOM2);

        Pattern patterns[] = { lineGeomPtrn, lineGeom2Ptrn, areaGeomPtrn,
                pointGeom2Ptrn };
        for (Pattern p : patterns) {
            Matcher m = p.matcher(geometryInfo);
            if (m.find()) {
                Pattern sizePtrn = Pattern.compile("(\\d{1,3})");
                m = sizePtrn.matcher(geometryInfo);
                if (m.find()) {
                    size = nauticalmileToMeter.convert(Double.parseDouble((m
                            .group())));
                    found = true;
                }
            }
        }

        if (!found && (size == 0) && (isVicinity || (coordinates.size() > 0))) {
            size = 2000; // 2 km = 2000 m
        }
    }

    private void clearData() {
        coordinates.clear();
        isVicinity = false;
        size = 0;
        eventId = null;
        text = "";
        dimension = CWADimension.CANCELED;
        startTime = null;
    }

    /**
     * @param timeStr
     * @return DataTime from header
     */
    private DataTime getDataTime(String timeStr) {
        Calendar cal = TimeTools.findDataTime(timeStr, headers);
        if (cal != null) {
            return new DataTime(cal);
        } else {
            return null;
        }
    }

    /**
     * @return CWA Record
     */
    private CWARecord getRecord() {
        CWARecord record = new CWARecord();
        record.setEventId(eventId);
        record.setDimension(dimension);
        record.setMessageData(text);
        DataTime dataTime = new DataTime(startTime.getRefTimeAsCalendar());
        record.setDataTime(dataTime);
        Coordinate[] coord = null;
        if (coordinates.size() == 1) {
            coord = Utility.makeArea(coordinates.get(0), size);
        } else if ((coordinates.size() == 2) && (size > 0)) {
            coord = Utility.makeArea(
                    coordinates.toArray(new Coordinate[coordinates.size()]),
                    size);
        } else {
            coord = coordinates.toArray(new Coordinate[coordinates.size()]);
        }
        record.setText(text);
        record.setCoordinates(coord);
        return record;
    }
}
