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
package com.raytheon.edex.plugin.recco.decoder;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftFlightLevel;
import com.raytheon.uf.edex.decodertools.core.BasePoint;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;

/**
 * This class parses Pirep reports. Some of the parsing is ported from the NCEP
 * NWS decoder, and other parts use regular expressions based on information in
 * AFMAN 15-124 and FMH No. 12.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080103            384 jkorman     Initial Coding.
 * May 14, 2014 2536       bclement    removed TimeTools usage
 * Jul 23, 2014 3410       bclement    location changed to floats
 * </pre>
 * 
 * @author dweeks
 * @version Feb 25, 2005
 */
public class ReccoParser {

    private static final String RECCO_RPT = "9(222|555|777)9";

    private static final String TIME_PATTERN = "([01]\\d|2[0123])[0-5]\\d[0-7/]";

    private static final String LONG_PATTERN = "[1-7][01235678]([0-8]\\d{2}|900)";

    private static final String LAT_PATTERN = "([0-8]\\d{2}|900)\\d{2}";

    private static final String TOKENIZER_SPLITS = " \r\n";

    // private final Pattern RECCO_RPT_PATTERN = Pattern.compile(RECCO_RPT);

    private String reportData = null;

    private ArrayList<String> reportParts = null;

    private String obsType = null;

    private BasePoint location = null;

    private Integer quadrant = null;

    private float latitude = Float.NaN;

    private float longitude = Float.NaN;

    private Calendar observationTime = null;

    private Integer hour = null;

    private Integer minute = null;

    private Integer dayOfWeek = null;

    private String iSubd = null;

    private AircraftFlightLevel flightLevel = null;

    private String geoIndicator = null;

    private Integer geoHeight = null;

    private Double temperature = null;

    private Double dewpoint = null;

    private Integer windDirection = null;

    private Double windSpeed = null;

    private String presentWx = null;

    /**
     * Construct a ReccoParser from given String data. The report is completely
     * parsed and decoded upon success.
     * 
     * @param aReport
     *            String data containing the possible RECCO observation.
     * @throws DecodeException
     *             An error occurred within the parser.
     */
    public ReccoParser(String report, Headers headers) {
        reportData = report;
        parse(headers);
    }

    /**
     * Construct a ReccoParser from given String data. The report is completely
     * parsed and decoded upon success.
     * 
     * @param aReport
     *            Byte array data containing the possible RECCO observation.
     * @throws DecodeException
     *             An error occurred within the parser.
     */
    public ReccoParser(byte[] report, Headers headers) {
        this(new String(report), headers);
    }

    /**
     * Parse out and decode the text elements in the observation.
     */
    private void parse(Headers headers) {

        populateReportParts();
        int start = findReportStart();
        start = extractTime(start);
        start = extractLat(start);
        start = extractLon(start);
        start = extractAlt(start);
        start = extractWinds(start);
        start = extractTemps(start);
        start = extractGeoHeight(start);

        createObsTime(headers);
        adjustLatLon();

        // From here is section 2

    }

    /**
     * Find the RECCO start token. If found extract the report type and return
     * the index of the next token.
     * 
     * @return The next token or -1 if start token was not found.
     */
    private int findReportStart() {
        int startToken = -1;
        int currIndex = 0;
        Pattern rptPattern = Pattern.compile(RECCO_RPT);

        while ((currIndex < reportParts.size()) && (startToken == -1)) {
            String s = reportParts.get(currIndex);
            Matcher m = rptPattern.matcher(s);

            if (m.find()) {
                startToken = currIndex + 1;
                obsType = s.substring(1, 4);
            } else {
                currIndex++;
            }
        }
        return startToken;
    }

    private int extractTime(int currIndex) {
        int idx = -1;
        if (currIndex > 0) {
            idx = currIndex;
            String s = reportParts.get(currIndex);
            Pattern rptPattern = Pattern.compile(TIME_PATTERN);
            Matcher m = rptPattern.matcher(s);
            if (m.find()) {
                hour = DecoderTools.getInt(s, 0, 2);
                minute = DecoderTools.getInt(s, 2, 4);
                iSubd = s.substring(4, 5);
                idx++;
            } else {
                idx = -1;
            }
        }
        return idx;
    }

    private int extractLat(int currIndex) {
        int idx = -1;
        if (currIndex > 0) {
            idx = currIndex;
            String s = reportParts.get(currIndex);
            Pattern rptPattern = Pattern.compile(LONG_PATTERN);
            Matcher m = rptPattern.matcher(s);
            if (m.find()) {
                dayOfWeek = DecoderTools.getInt(s, 0, 1);
                quadrant = DecoderTools.getInt(s, 1, 2);
                latitude = DecoderTools.getInt(s, 2, 5).floatValue() / 10.0f;
                idx++;
            } else {
                idx = -1;
            }
        }
        return idx;
    }

    private int extractLon(int currIndex) {
        int idx = -1;
        if (currIndex > 0) {
            idx = currIndex;
            String s = reportParts.get(currIndex);
            Pattern rptPattern = Pattern.compile(LAT_PATTERN);
            Matcher m = rptPattern.matcher(s);
            if (m.find()) {
                longitude = DecoderTools.getInt(s, 0, 3).floatValue() / 10.0f;

                idx++;
            } else {
                idx = -1;
            }
        }
        return idx;
    }

    /**
     * Adjust the decoded latitude/longitude for the given quadrant.
     */
    private void adjustLatLon() {
        if (quadrant != null) {
            switch (quadrant) {
            case 0: {
                longitude = -longitude;
                break;
            }
            case 1: {
                longitude = -(longitude + 90);
                break;
            }
            case 2: {
                longitude = longitude + 90;
                break;
            }
            case 3: {
                break;
            }
            case 5: {
                latitude = -latitude;
                longitude = -longitude;
                break;
            }
            case 6: {
                latitude = -latitude;
                longitude = -(longitude + 90);
                break;
            }
            case 7: {
                latitude = -latitude;
                longitude = longitude + 90;
                break;
            }
            case 8: {
                latitude = -latitude;
                break;
            }
            default: {
                // indicate error.
                break;
            }
            } // switch()
        }
    }

    private int extractAlt(int currIndex) {
        int idx = -1;
        if (currIndex > 0) {
            idx = currIndex;
            String s = reportParts.get(currIndex);
            Pattern rptPattern = Pattern.compile("\\d{3}[01/]{2}");
            Matcher m = rptPattern.matcher(s);
            if (m.find()) {
                Integer alt = DecoderTools.getInt(s, 0, 3) * 10;
                flightLevel = new AircraftFlightLevel(alt);

                // TODO: wind statistics
                idx++;
            } else {
                idx = -1;
            }
        }
        return idx;
    }

    private int extractWinds(int currIndex) {
        int idx = -1;
        if (currIndex > 0) {
            idx = currIndex;
            String s = reportParts.get(currIndex);
            Pattern rptPattern = Pattern.compile("\\d{5}");
            Matcher m = rptPattern.matcher(s);
            if (m.find()) {
                windDirection = DecoderTools.getInt(s, 0, 2) * 10;
                windSpeed = DecoderTools.getInt(s, 2, 5).doubleValue();
                idx++;
            } else {
                idx = -1;
            }
        }
        return idx;
    }

    private int extractTemps(int currIndex) {
        int idx = -1;
        if (currIndex > 0) {
            idx = currIndex;
            String s = reportParts.get(currIndex);
            Pattern rptPattern = Pattern
                    .compile("(\\d{2}|//)(\\d{2}|//)(\\d|/)");
            Matcher m = rptPattern.matcher(s);
            if (m.find()) {
                temperature = DecoderTools.getInt(s, 0, 2).doubleValue();
                if (temperature > 50) {
                    temperature = -(temperature - 50);
                }
                dewpoint = DecoderTools.getInt(s, 2, 4).doubleValue();
                if (dewpoint > 50) {
                    dewpoint = -(dewpoint - 50);
                }
                presentWx = s.substring(4, 5);
                idx++;
            } else {
                idx = -1;
            }
        }
        return idx;
    }

    private int extractGeoHeight(int currIndex) {
        int idx = -1;
        if (currIndex > 0) {
            idx = currIndex;
            String s = reportParts.get(currIndex);
            Pattern rptPattern = Pattern.compile("/[0-9/](\\d{3}|///)");
            Matcher m = rptPattern.matcher(s);
            if (m.find()) {
                geoIndicator = s.substring(1, 2);
                geoHeight = DecoderTools.getInt(s, 2, 5);

                // ****************
                // TABLE 9 j
                // 0 Sea level pressure in whole millibars (thousands fig if any
                // omitted)
                // 1 Altitude 200 mb surface in geopotential decameters
                // (thousands fig if any omitted)
                // 2 Altitude 850 mb surface in geopotential meters (thousands
                // fig omitted)
                // 3 Altitude 700 mb surface in geopotential meters (thousands
                // fig omitted)
                // 4 Altitude 500 mb surface in geopotential decameters
                // 5 Altitude 400 mb surface in geopotential decameters
                // 6 Altitude 300 mb surface in geopotential decameters
                // 7 Altitude 250 mb surface in geopotential decameters
                // (thousands fig if any omitted)
                // 8 D - Value in geopotential decameters; if negative 500 is
                // added to HHH
                // 9 Altitude 925 mb surface in geopotential meters
                // / No absolute altitude available or geopotential data not
                // within
                // +/- 30 meters/4 mb accuracy requirements
                // ****************
                switch (geoIndicator.charAt(0)) {
                case '0': {
                    break;
                }
                case '1': {
                    break;
                }
                case '2': {
                    geoHeight = (geoHeight * 10) + 10000;
                    break;
                }
                case '3': {
                    if (geoHeight <= 200) {
                        geoHeight = geoHeight + 3000;
                    } else {
                        geoHeight = geoHeight + 2000;
                    }
                    break;
                }
                case '4': {
                    geoHeight = geoHeight * 10;
                    break;
                }
                case '5': {
                    geoHeight = geoHeight * 10;
                    break;
                }
                case '6': {
                    geoHeight = geoHeight * 10;
                    break;
                }
                case '7': {
                    geoHeight = geoHeight * 10;
                    if (geoHeight < 2000) {
                        geoHeight = geoHeight + 10000;
                    }
                    break;
                }
                case '8': // fall through
                case '9': {
                    // No action for now.
                    break;
                }
                case '/': {
                    geoHeight = -9999;
                    break;
                }
                } // switch()
                idx++;
            } else {
                idx = -1;
            }
        }
        return idx;
    }

    private void createObsTime(Headers headers) {

        observationTime = WMOTimeParser.getSystemCalendar((String) headers
                .get(DecoderTools.INGEST_FILE_NAME));
        int dow = observationTime.get(Calendar.DAY_OF_WEEK);

        if (dow != dayOfWeek) {

            if (dayOfWeek > dow) {
                dow = dayOfWeek - dow;
            } else {
                dow = dow - dayOfWeek;
            }
            observationTime.add(Calendar.DAY_OF_MONTH, -dow);
        }
        observationTime.set(Calendar.HOUR_OF_DAY, hour);
        observationTime.set(Calendar.MINUTE, minute);
        observationTime.set(Calendar.SECOND, 0);
        observationTime.set(Calendar.MILLISECOND, 0);
    }

    /**
     * 
     */
    private void populateReportParts() {
        reportParts = new ArrayList<String>();
        StringTokenizer st = new StringTokenizer(reportData, TOKENIZER_SPLITS,
                false);

        while (st.hasMoreTokens()) {
            reportParts.add(st.nextToken());
        }
    }

    public String getReportData() {
        return reportData;
    }

    public String getObsType() {
        return obsType;
    }

    public BasePoint getLocation() {
        return location;
    }

    public float getLatitude() {
        return latitude;
    }

    public float getLongitude() {
        return longitude;
    }

    public Calendar getObservationTime() {
        return observationTime;
    }

    public AircraftFlightLevel getFlightLevel() {
        return flightLevel;
    }

    public Double getTemperature() {
        return temperature;
    }

    public Double getDewpoint() {
        return dewpoint;
    }

    public Integer getWindDirection() {
        return windDirection;
    }

    public Double getWindSpeed() {
        return windSpeed;
    }

    public Integer getQuadrant() {
        return quadrant;
    }

    public Integer getHour() {
        return hour;
    }

    public Integer getMinute() {
        return minute;
    }

    public Integer getDayOfWeek() {
        return dayOfWeek;
    }

    public String getISubd() {
        return iSubd;
    }

    public String getGeoIndicator() {
        return geoIndicator;
    }

    public Integer getGeoHeight() {
        return geoHeight;
    }

    public String getPresentWx() {
        return presentWx;
    }

}