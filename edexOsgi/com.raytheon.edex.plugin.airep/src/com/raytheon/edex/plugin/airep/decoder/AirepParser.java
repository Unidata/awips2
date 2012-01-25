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
package com.raytheon.edex.plugin.airep.decoder;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftFlightLevel;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftLatitude;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftLongitude;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftRemarks;
import com.raytheon.uf.edex.decodertools.core.BasePoint;
import com.raytheon.uf.edex.decodertools.core.PlatformLocationProxy;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * The AirepParser takes a String that should contain a single AIREP observation
 * and parses the individual elements of the observation, and performs a decode
 * of those elements. The data is made available to clients through a set of get
 * methods for each data item.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           PTR#     Engineer      Description
 * ------------   -------- ------------- -------------------------------------
 * 20080103            384 jkorman     Initial Coding.
 * 20080423           1016 jkorman     Added range checking for wind speed.
 *                                     Zero'd second/milliseconds on timeObs.
 * </pre>
 */
public class AirepParser {
    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    private static final boolean WANT_DELIMITERS = false;

    // Only going to delimit aireps with spaces and carriage control.
    private static final String DELIMITER = " ;$=\r\n";

    private static final int MAX_WIND_SPEED = 500;

    // Once set the obs data cannot be changed!
    private final String reportData;

    //
    // Determine if the input data corresponds to a hour minute time
    //
    // i.e. 0000 to 2359
    //
    // "^([0..1]{1}[0..9]{1})|(2[0..3]{1})[0..5]{1}[0..9]{1}"
    //
    // Start of token "^"
    // 0 or 1 followed by 0 to 9 or
    // 2 followed by 0 to 3
    //
    // 0 to 5 followed by 0 to 9
    // End of token "$"
    //

    final Pattern TIME = Pattern
            .compile("^(([0-1]{1}[0-9]{1})|(2[0-3]{1}))([0-5]{1}[0-9]{1})$");

    final Pattern WX_GROUP = Pattern.compile("^[0-9/]{3}$");

    // Two different flight level patterns
    final Pattern FL_SHORT = Pattern.compile("F\\d{3}");

    final Pattern FL_LONG = Pattern.compile("FL\\d{3}");

    // MStt, Mtt, PStt, Ptt

    final Pattern TEMP_SHORT = Pattern.compile("^(M|P)\\d{2}");

    final Pattern TEMP_LONG = Pattern.compile("^(MS|PS)\\d{2}");

    // Parsed and/or decoded observation elements.
    private ArrayList<Object> theElements = new ArrayList<Object>();

    private String aircraftId = null;

    private Integer reportType = null;

    private Double latitude = null;

    private Double longitude = null;

    private Calendar observationTime = null;

    private AircraftFlightLevel flightLevel = null;

    private Double temperature = null;

    private AIREPWeather weatherGroup = null;

    private Integer windDirection = null;

    private Double windSpeed = null;

    private AircraftRemarks rptRemarks = null;

    private Calendar refTime;
    

    /**
     * Create the parser for and decode an observation from a String.
     * 
     * @param anObservation
     *            A string containing the observation.
     */
    public AirepParser(String anObservation, Calendar refTime) {
        this.refTime = refTime;
        reportData = anObservation;
        parseElements();
    } // AirepParser()

    /**
     * Create the parser for and decode an observation from a byte array.
     * 
     * @param anObservation
     *            A string containing the observation.
     */
    public AirepParser(byte[] anObservation, Calendar refTime) {
        this(new String(anObservation), refTime);
    } // AirepParser()

    /**
     * Expose the internal parsed elements for testing.
     * 
     * @return A collection of the parsed elements.
     */
    ArrayList<?> parseElementsTestPoint() {
        ArrayList<Object> retElements = new ArrayList<Object>();
        retElements.addAll(theElements);
        return retElements;
    } // parseElementsTestPoint()

    /**
     * Parse the AIREP observation into individual elements. Note that during
     * parse or decode the order of the elements does not change. The only
     * exception to this rule is that all elements identified as
     * comments/remarks are placed at the end of the observation elements.
     */
    private void parseElements() {
        StringTokenizer st = new StringTokenizer(reportData, DELIMITER,
                WANT_DELIMITERS);

        // Now get the elements
        while (st.hasMoreTokens()) {
            String s = st.nextToken();
            if (!DELIMITER.equals(s)) {
                Object o = AIREPObsType.obsTypeFactory(s);
                if ((o != null) && (reportType == null)) {
                    reportType = ((AIREPObsType) o).getValue();
                } else {
                    theElements.add(s);
                }
            }
        }
        decodeMID();
        splitLatLon();
        decodeLatitude();
        decodeLongitude();
        decodeTime();
        // By now we should have found lat lon information. If not then
        // run back through the data to see if there is a waypoint. If we
        // get to the time information, quit, it's not there.
        if ((latitude == null) && (longitude == null)) {
            for (int i = 0; i < theElements.size(); i++) {
                Object o = theElements.get(i);
                if (o instanceof String) {
                    BasePoint wayPoint = PlatformLocationProxy.lookup(
                            (String) o, null);
                    // found a waypoint
                    if (wayPoint != null) {
                        // lat =
                        // AircraftLatitude.createLatitude(wayPoint);
                        // Longitude lon =
                        // AircraftLongitude.createLongitude(wayPoint);
                        // theElements.set(i,lat);
                        // theElements.add(i+1,lon);
                        // theLatitude = lat;
                        // longitude = lon;
                        // break;
                    }
                } else if (o instanceof Calendar) {
                    break;
                }
            }
        }
        // Need to have lat / lon data
        if ((latitude == null) || (longitude == null)) {
            // set the observation time to null, to signal that this ob
            // is invalid and quit.
            observationTime = null;
            return;
        }
        decodeFlightLevel();
        decodeTemperature();
        decodeWeatherGroup();
        decodeWinds();
        collectRemarks();
        determineAircraftId();
    } // parseElements()

    /**
     * When the primary decode is complete the aircraft ID should be the only
     * data left prior to the latitude/longitude data. If found then set this
     * data as the id.
     */
    private void determineAircraftId() {
        for (int i = 0; i < theElements.size(); i++) {
            Object o = theElements.get(i);
            if (o instanceof Double) {
                break;
            }
            if (o instanceof String) {
                aircraftId = (String) o;
                break;
            }
        }
    } // determineAircraftId()

    /**
     * Determine if a latitude/longitude group is run-together or may be a
     * navigation waypoint. As an example
     * 
     * <pre>
     * <code>5802N02015W</code>
     * </pre>
     * 
     * is split into 2 groups
     * 
     * <pre>
     * <code>5802N 02015W</code>
     * </pre>
     * 
     * which is then processed normally.
     */
    private void splitLatLon() {
        for (int i = 0; i < theElements.size(); i++) {
            Object o = theElements.get(i);
            if (o instanceof String) {
                String[] latLon = AircraftLatitude.splitLatLon((String) o);
                if ((latLon != null)&&(latLon.length ==2)) {
                    theElements.add(i, latLon[0]);
                    theElements.set(i, latLon[1]);
                    break;
                }
            }
        } // for
    } // splitLatLon()

    /**
     * Attempt to locate and decode the latitude information within this AIREP
     * observation. The decode object replaces the string data within the
     * observation elements collection.
     */
    private void decodeLatitude() {
        if (latitude == null) {
            for (int i = 0; i < theElements.size(); i++) {
                Object o = theElements.get(i);
                if (o instanceof String) {
                    AircraftLatitude lat = AircraftLatitude
                            .aircraftLatitudeFactory((String) o);
                    if (lat != null) {
                        theElements.set(i, lat);
                        latitude = lat.decodeLatitude();
                        break;
                    }
                }
            }
        }
    } // decodeLatitude()

    /**
     * Attempt to locate and decode the longitude information within this AIREP
     * observation. The decode object replaces the string data within the
     * observation elements collection.
     */
    private void decodeLongitude() {
        if (longitude == null) {
            for (int i = 0; i < theElements.size(); i++) {
                Object o = theElements.get(i);
                if (o instanceof String) {
                    AircraftLongitude lon = AircraftLongitude
                            .aircraftLongitudeFactory((String) o);
                    if (lon != null) {
                        theElements.set(i, lon);
                        longitude = lon.decodeLongitude();
                        break;
                    }
                }
            }
        }
    } // decodeLongitude()

    /**
     * Attempt to locate and decode the time information within this AIREP
     * observation. The decode object replaces the string data within the
     * observation elements collection.
     */
    private void decodeTime() {
        for (int i = 0; i < theElements.size(); i++) {
            Object o = theElements.get(i);
            if (o instanceof String) {
                String s = (String) o;
                if (TIME.matcher(s).matches()) {

                    int hour = Integer.parseInt(s.substring(0, 2));
                    int minute = Integer.parseInt(s.substring(2));

                    if (refTime != null) {
                        
                        observationTime = TimeTools.copy(refTime);
                        observationTime.set(Calendar.HOUR_OF_DAY, hour);
                        observationTime.set(Calendar.MINUTE, minute);
                        observationTime.set(Calendar.SECOND, 0);
                        observationTime.set(Calendar.MILLISECOND, 0);
                        // If the observation time ends up greater than
                        // the reference time, back up a day.
                        if (observationTime.compareTo(refTime) > 0) {
                            observationTime.add(Calendar.DAY_OF_MONTH, -1);
                        }

                        theElements.set(i, observationTime);
                    }
                    break;
                }
            }
        }
    }

    private void decodeFlightLevel() {
        for (int i = 0; i < theElements.size(); i++) {
            Object o = theElements.get(i);
            if (o instanceof String) {
                String s = (String) o;
                if (FL_SHORT.matcher(s).matches()) {
                    double fLevel = Integer.parseInt(s.substring(1)) * 100;

                    flightLevel = new AircraftFlightLevel(fLevel);
                    theElements.set(i, flightLevel);
                    break;
                } else if (FL_LONG.matcher(s).matches()) {
                    double fLevel = Integer.parseInt(s.substring(1)) * 100;

                    flightLevel = new AircraftFlightLevel(fLevel);
                    theElements.set(i, flightLevel);
                    break;
                }
            }
        }
    }

    /**
     * Decode the temperature information in this observation.
     */
    private void decodeTemperature() {
        for (int i = 0; i < theElements.size(); i++) {
            Object o = theElements.get(i);
            if (o instanceof String) {
                double temp = Double.NaN;
                String s = (String) o;
                if (TEMP_LONG.matcher(s).matches()) {
                    String ss = s.substring(2);
                    if (!"//".equals(ss)) {
                        temp = Double.parseDouble(ss);

                        if ("MS".equals(s.substring(0, 2))) {
                            temp *= -1;
                        }
                        temperature = new Double(temp);
                        theElements.set(i, temperature);
                    }
                    break;
                } else if (TEMP_SHORT.matcher(s).matches()) {
                    String ss = s.substring(1);
                    if (!"//".equals(ss)) {
                        temp = Double.parseDouble(ss);

                        if ("M".equals(s.substring(0, 1))) {
                            temp *= -1;
                        }
                        temperature = new Double(temp);
                        theElements.set(i, temperature);
                    }
                    break;
                }
            }
        }
    }

    /**
     * Attempt to locate and decode the 3 digit hazards and weather group.
     */
    private void decodeWeatherGroup() {
        for (int i = 0; i < theElements.size(); i++) {
            Object o = theElements.get(i);
            if (o instanceof String) {
                String s = (String) o;
                if (s.length() == 3) {
                    if (WX_GROUP.matcher(s).find()) {
                        weatherGroup = new AIREPWeather(s);
                        theElements.set(i, weatherGroup);
                        break;
                    }
                }
            }
        }
    } // decodeWeatherGroup()

    /**
     * Decode the wind data group in the following forms. 16080 160080 163/080
     * 163/080KT
     * 
     * @throws DecodeException
     *             if winds are bad
     */
    private void decodeWinds() {
        // By now we should have found the flight level data.
        int i = 0;
        for (; i < theElements.size(); i++) {
            if (theElements.get(i) instanceof AircraftFlightLevel) {
                i++;
                break;
            }
        } // for()

        for (; i < theElements.size(); i++) {
            Object o = theElements.get(i);
            if (o instanceof String) {
                String s = (String) o;
                if (s != null) {
                    if (s.startsWith("M") || s.startsWith("P")
                            || s.startsWith("/")) {
                        // These are temperatures. Some temps are
                        // being reported as 5 digits (MS513 which is -51.3)
                        continue;
                    } else if (s.endsWith("KT")) {
                        s = s.substring(0, s.length() - 2);
                    } else if (s.endsWith("KTS")) {
                        s = s.substring(0, s.length() - 3);
                    }
                    int solidusPos = s.indexOf("/");

                    String windDir = null;
                    String windSpd = null;
                    if (solidusPos > 0) {
                        windDir = s.substring(0, solidusPos);
                        windSpd = s.substring(solidusPos + 1);
                    } else if (s.length() == 5) {
                        windDir = s.substring(0, 2) + "0";
                        windSpd = s.substring(2);
                    } else if (s.length() == 6) {
                        windDir = s.substring(0, 3);
                        windSpd = s.substring(3);
                    }
                    if ((windSpd != null) && (windDir != null)) {
                        try {
                            double value = Double.parseDouble(windSpd);
                            if ((value >= 0) && (value < MAX_WIND_SPEED)) {
                                windSpeed = value;
                            } else {
                                windSpeed = null;
                            }

                            value = Double.parseDouble(windDir);
                            // Database constraint is 1 - 360.
                            if (value == 0) {
                                value = 360;
                            }
                            windDirection = new Double(value).intValue(); // windDirection.fromDegree(value);

                            theElements.set(i, windDirection);
                            theElements.add(i + 1, windSpeed);
                        } catch (Exception nothing) {
                            String msg = String.format(
                                    "Error decoding winds: [%s] [%s]", windSpd,
                                    windDir);

                            logger.info(msg);
                        }
                        break;
                    }
                }
            }
        }
    }

    /**
     * The "MID" section only occurs in the AIREP remarks section. So if we find
     * a "MID" token we create a remarks object using all data from the MID
     * token to the end of the data.
     */
    private void decodeMID() {
        for (int i = 0; i < theElements.size(); i++) {
            Object o = theElements.get(i);
            if (o instanceof String) {
                String s = (String) o;
                if ("MID".equals(s)) {
                    AircraftRemarks remarks = new AircraftRemarks(s);
                    for (i++; i < theElements.size();) {
                        remarks.addRemarks(" ");
                        remarks.addRemarks((String) theElements.get(i));
                        theElements.remove(i);
                    }
                    rptRemarks = remarks;
                }
            }
        }
    }

    /**
     * Iterate over any remaining data left in the observation that is AFTER the
     * time information. These data are bundled together as a remarks string
     * that will be appended to the end of the observation data.
     */
    private void collectRemarks() {
        boolean timeFound = false;
        int i = 0;
        for (; i < theElements.size(); i++) {
            Object o = theElements.get(i);
            if (timeFound = (o instanceof Calendar)) {
                break;
            }
        } // for
        if (timeFound) {
            StringBuffer remarksBuffer = new StringBuffer();
            for (; i < theElements.size(); i++) {
                Object o = theElements.get(i);
                if (o instanceof String) {
                    theElements.remove(i);
                    i--;
                    remarksBuffer.append(o);
                    remarksBuffer.append(" ");
                }
            } // for
            if (remarksBuffer.length() > 0) {
                if (rptRemarks != null) {
                    remarksBuffer.insert(0, " ");
                    remarksBuffer.insert(0, rptRemarks.getValue());
                }
                rptRemarks = new AircraftRemarks(remarksBuffer.toString());
            }
        }
    } // collectRemarks()

    /**
     * @return the reportData
     */
    public String getReportData() {
        return reportData;
    }

    /**
     * @return the reportType
     */
    public Integer getReportType() {
        return reportType;
    }

    /**
     * @param reportType
     *            the reportType to set
     */
    public void setReportType(Integer reportType) {
        this.reportType = reportType;
    }

    /**
     * Get the decoded Aircraft id.
     * 
     * @return The decoded Aircraft id.
     */
    public String getAircraftId() {
        return aircraftId;
    } // getAircraftId()

    /**
     * Get the decoded Latitude instance.
     * 
     * @return The decoded Latitude
     */
    public Double getLatitude() {
        return latitude;
    } // getLatitude()

    /**
     * Get the decoded Longitude instance.
     * 
     * @return The decoded Longitude.
     */
    public Double getLongitude() {
        return longitude;
    } // getLongitude()

    /**
     * Get the AIREP observation time.
     * 
     * @return The AIREP observation time.
     */
    public Calendar getObservationTime() {
        return observationTime;
    } // getObservationTime()

    /**
     * Get the decoded aircraft flight level data.
     * 
     * @return The decoded aircraft flight level data.
     */
    public Integer getFlightLevel() {
        Integer retValue = null;
        if (flightLevel != null) {
            retValue = flightLevel.getFlightLevel();
        }
        return retValue;
    } // getFlightLevel()

    /**
     * Get the decoded temperature data.
     * 
     * @return The decoded temperature.
     */
    public Double getTemperature() {
        return temperature;
    } // getAirTemperature()

    public AIREPWeather getWeatherGroup() {
        return weatherGroup;
    } // getWeatherGroup()

    /**
     * Get the decoded wind direction data.
     * 
     * @return The decoded wind direction data.
     */
    public Integer getWindDirection() {
        return windDirection;
    } // getWindDirection()

    /**
     * Get the decoded wind speed data.
     * 
     * @return The decoded wind speed data.
     */
    public Double getWindSpeed() {
        return windSpeed;
    } // getWindSpeed()

    /**
     * Get any remarks information found.
     * 
     * @return The remarks information. An empty string is returned if no
     *         remarks data was found.
     */
    public String getRemarks() {
        return (rptRemarks != null) ? rptRemarks.toString() : "";
    } // getRemarks()

    
    public static void main(String [] args) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("ZULU"));
        
        Calendar refTime = TimeTools.getBaseCalendar(2011, 12, 14);
        refTime.set(Calendar.HOUR_OF_DAY, 17);
        refTime.set(Calendar.MINUTE, 15);
        refTime.set(Calendar.SECOND, 00);
        refTime.set(Calendar.MILLISECOND, 0);
        
        String data = "ARP UAL121 4400N 05700W 1640 F390 MS00 000/099KT TB MOD SK CLEAR=";
        AirepParser p = new AirepParser(data,refTime);
        System.out.println(sdf.format(p.getObservationTime().getTime()));
        
        data = "ARP UAL121 4400N 05700W 1840 F390 MS00 000/099KT TB MOD SK CLEAR=";
        p = new AirepParser(data,refTime);
        System.out.println(sdf.format(p.getObservationTime().getTime()));
    }
    
} // AirepParser
