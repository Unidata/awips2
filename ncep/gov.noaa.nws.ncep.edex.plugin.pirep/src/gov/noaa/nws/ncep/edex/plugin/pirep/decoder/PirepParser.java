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
package gov.noaa.nws.ncep.edex.plugin.pirep.decoder;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftCloudLayer;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftFlightCondition;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftFlightLevel;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftRemarks;
import com.raytheon.uf.edex.decodertools.core.BasePoint;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * This class parses Pirep reports. Some of the parsing is ported from the NCEP
 * NWS decoder, and other parts use regular expressions based on information in
 * AFMAN 15-124 and FMH No. 12.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         PR#         Engineer    Description
 * -----------  ---------- ------------ --------------------------
 * 05/26/2011				F.J.Yen		Initial creation from pirep.  Change
 * 										temperature and windSpeed from Double to
 * 										Float; windDirection from Integer to Float.
 * 20110830     286        qzhou        Added fields for TB, IC, SK. Remove general fileds.
 *                                      Append intensity2 to intensity1 for TB, IC, SK.
 *                                      Use IdecoderConstantsN.UAIR_INTEGER_MISSING instead.
 *                                      Added PirepTools from raytheon for decoding TB. No need of FL_COND_WORDS.
 *                                      ICING_COND_WORDS instead of FL_COND_WORDS for icing.
 * 09/22/2011   286        qzhou        Fixed obs time: Added checkDayInTime().
 * 									    Modified decodeTimeData(), handled suspectTimeflag here.
 *                                      Modified parse() to handle time and station id.
 * 09.2/2011    286        qzhou        Fixed ice layer. Do regex twice. The secondtime is for intensity alone.
 * 										Fixed visibility.
 * 10/13/2011   286        qzhou        Remove \r \n \s in the reportData in parse().
 *                                      Handle abnormal location. /OV TBE 265040... Modified decodeLocationData() and parseLatLon()								
 * 10/20/2011   472        qzhou        Added WX_COND_WORDS. Removed SKY_SKC, SKY_CLR and other _COND_WORDS
 * 11/01/2011   286       Q.Zhou        Added month and year to decodetime
 * Sep 05, 2013 2316       bsteffen     Unify pirep and ncpirep.
 * Jul 23, 2014 3410       bclement     location changed to floats
 * 
 * </pre>
 * 
 * @author jkorman
 * @version Dec. 27, 2007
 */
public class PirepParser {
    private static final double ONE_NM_RADIANS = (2.0 * Math.PI) / 21637.5;
    
    // Allowable future time in milliseconds (15 minutes).
    private static final int ALLOWABLE_TIME = 15;
    
    private static final float RMISSD = -9999.f;
    
    private static final int IMISSD = IDecoderConstants.VAL_MISSING;;

    private static final String[] RPIDS = { "UUA", "UA" };

    /**
     * Note that there are intentionally spaces after some of the teis.
     */
    private static final String[] TEIS = { "/OV ", "/TM", "/FL", "/TP", "/SK ",
        "/WX", "/TA", "/WV", "/TB", "/IC", "/RM" };

    private static final String BLO_HGT = "BLO";
    private static final String ABV_HGT = "ABV";
    private static final String NEG_ENTRY = "NEG";
    private static final String NULL_ENTRY = "---";
    
    private static final Map<String,String> WX_COND_WORDS = new HashMap<String,String>();
    static {
    	WX_COND_WORDS.put("NEG", "NEG");
        WX_COND_WORDS.put("NONE", "NULL_ENTRY");
        WX_COND_WORDS.put("NIL", "NULL_ENTRY");
        WX_COND_WORDS.put("SMOOTH", "NEG");
        WX_COND_WORDS.put("SMTH", "NEG");
        WX_COND_WORDS.put("TRACE", "TRACE");
        WX_COND_WORDS.put("LT", "LGT");
        WX_COND_WORDS.put("LGT", "LGT");
        WX_COND_WORDS.put("MOD", "MOD");
        WX_COND_WORDS.put("MDT", "MOD");
        WX_COND_WORDS.put("SEV", "SEV");
        WX_COND_WORDS.put("SVR", "SEV");
        WX_COND_WORDS.put("SEVR", "SEV");
        WX_COND_WORDS.put("SEVERE", "SEV");
        WX_COND_WORDS.put("EXTRM", "EXTRM");
        WX_COND_WORDS.put("XTRM", "EXTRM");
        WX_COND_WORDS.put("HVY", "EXTRM");

        WX_COND_WORDS.put("OCN", "OCN");
        WX_COND_WORDS.put("ISO", "ISO");
        WX_COND_WORDS.put("INT", "INT");
        WX_COND_WORDS.put("INTMT", "INT");
        WX_COND_WORDS.put("INTERMIT", "INT");
        WX_COND_WORDS.put("CON", "CON");
        WX_COND_WORDS.put("CONS", "CON");
        
        WX_COND_WORDS.put("RIME", "RIME");
        //WX_COND_WORDS.put("CLR", "CLR");
        WX_COND_WORDS.put("MXD", "MXD");
        WX_COND_WORDS.put("MIX", "MXD");
        WX_COND_WORDS.put("MX", "MXD");
        WX_COND_WORDS.put("CLEAR", "CLR");
        WX_COND_WORDS.put("CHOP", "CHOP");
        WX_COND_WORDS.put("CAT", "CAT");
        
//      WX_COND_WORDS.put("IMC", "IMC"); //SK, RM
//      WX_COND_WORDS.put("IN CLDS", "NULL_ENTRY"); //TB
//      WX_COND_WORDS.put("MTN WAVE", NULL_ENTRY); //TB

        WX_COND_WORDS.put("OVC", "OVC");
        WX_COND_WORDS.put("BKN", "BKN");
        WX_COND_WORDS.put("SCT", "SCT");
        WX_COND_WORDS.put("FEW", "FEW");
        WX_COND_WORDS.put("VV", "VV");
        WX_COND_WORDS.put("CLR", "CLR");
        WX_COND_WORDS.put("SKC", "CLR");
        WX_COND_WORDS.put("BLNK", "CLR");
        WX_COND_WORDS.put("OBS", "OBS");
        
        WX_COND_WORDS.put("UNKN", "UNKN");  
        WX_COND_WORDS.put("UNKNOWN", "UNKN");

    }
    
    private static final Map<String,Integer> DIR_POINTS = new HashMap<String,Integer>();
    static {
        DIR_POINTS.put("N", 0);
        DIR_POINTS.put("NNE", 22);
        DIR_POINTS.put("NE", 45);
        DIR_POINTS.put("ENE", 67);
        DIR_POINTS.put("E", 90);
        DIR_POINTS.put("ESE", 112);
        DIR_POINTS.put("SE", 135);
        DIR_POINTS.put("SSE", 157);
        DIR_POINTS.put("S", 180);
        DIR_POINTS.put("SSW", 202);
        DIR_POINTS.put("SW", 225);
        DIR_POINTS.put("WSW", 247);
        DIR_POINTS.put("W", 270);
        DIR_POINTS.put("WNW", 292);
        DIR_POINTS.put("NW", 315);
        DIR_POINTS.put("NNW", 337);
    }
    
    private static final String ICE_TURB_PREFIX = "([A-Z]{3,5})(?:((?:-)([A-Z]{3,5})))?";

    private static final String ABV_BLO = BLO_HGT + "|" + ABV_HGT;
    private static final String ICE_TURB_SUFFIX = "(?: )?(((" + ABV_BLO + ")(?: )(\\d{3}))|((\\d{3})(?:(-(\\d{3}))?)))?(?:/)?()";
    
    private static final String ICE_REGEX = ICE_TURB_PREFIX + "((?: )(RIME|MXD|CLR))" + ICE_TURB_SUFFIX;//?

    private static final String TRB_REGEX = ICE_TURB_PREFIX
            + "((?: )(CHOP|CAT))?" + ICE_TURB_SUFFIX;

    
    // 3901N 08446W
    private static final String LATLON_PTRN = "((([0-8]\\d[0-5]\\d)|(9000))[NS]((0\\d{2}[0-5]\\d)|([1][0-7]\\d[0-5]\\d)|(18000))[EW])";
    
    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    // Codes used in groupLikeTypes()
    // Was the character whitespace?
    private static final int NONE = 0;

    // Was character a digit?
    private static final int DIGIT = 1;

    // Was character a letter?
    private static final int LETTER = 2;

    // Was character non-alphanumeric but not whitespace?
    private static final int NONALPHANUMERIC = 3;

    // The last search position when looking for TEIs
    private int theSearchPos = -1;

    // Where was the Token found
    private int theTokenPos;

    private String reportData = null;

    private String theReportingStationId = null;

    private BasePoint location = null;

    private LatLonPoint interLocation = null;
    
    private Calendar observationTime = null;

    private AircraftFlightLevel flightLevel = null;
    
    private String aircraftType = "UNKN";

//    private Double temperature = null;
    private Float temperature = RMISSD;

//    private Integer windDirection = null;
    private Float windDirection = RMISSD;

//    private Double windSpeed = null;
    private Float windSpeed = RMISSD;

    private Integer horzVisibility = IMISSD;

    private List<AircraftCloudLayer> cloudLayers = null;

    private AircraftRemarks theRemarks = null;

    private List<AircraftFlightCondition> theTurbulenceLayers = null;

    private List<AircraftFlightCondition> theIcingLayers = null;

    private String[] theWeatherCodes = null;
    private String suspectTimeFlag = null;
    private String headerTime = "";
    
    // these values are only populated if the pirep contained explicit lat/lon;
    // theLocation holds the location of the pirep; values in radians
//    private double latitude = Double.NaN;
//
//    private double longitude = Double.NaN;

    private String traceId;
    
    /**
     * Construct a PirepParser from given String data. The report is completely
     * parsed and decoded upon success.
     * 
     * @param report
     *            String data containing the possible PIREP.
     * @throws DecodeException
     *             An error occurred within the parser.
     */
    public PirepParser(String report, String traceId) {
        reportData = report;
        this.traceId = traceId;
        parse();
    }

    /**
     * Construct a PirepParser from given byte array data. The report is
     * completely parsed and decoded upon success.
     * 
     * @param report
     *            Byte array data containing the possible PIREP.
     * @throws DecodeException
     *             An error occurred within the parser.
     */
    public PirepParser(byte[] report, String traceId) {
        this(new String(report),traceId);
    }

    /**
     * @return the reportData
     */
    public String getReportData() {
        return reportData;
    }

    public String getReportType() {
        return "PIREP";
    }
    
    public String getSuspectTimeFlag() {
        return suspectTimeFlag;
    }
    
    /**
     * Get the station reporting id.
     * 
     * @return Returns the theReportingStationId.
     */
    public String getReportingStationId() {
        return theReportingStationId;
    }

    /**
     * Get the reporting aircraft type.
     * 
     * @return The aircraft type.
     */
    public String getAircraftType() {
        return aircraftType;
    }

    /**
     * Get the decoded flight level information.
     * 
     * @return The decoded flight level in feet.
     */
    public Integer getFlightLevel() {
        //Integer retValue = null;
        Integer retValue = PirepTools.UAIR_INTEGER_MISSING; // -9999;
        if (flightLevel != null) {
            retValue = flightLevel.getFlightLevel();
        }
        return retValue;
    } // getFlightLevel()

    /**
     * Get the aircraft location or midpoint location.
     * 
     * @return Returns the aircraft location.
     */
    public BasePoint getLocation() {
        return location;
    }

    /**
     * Get the PIREP observation time.
     * 
     * @return Returns the observation time.
     */
    public Calendar getObservationTime() {
        return observationTime;
    }

    
    /**
     * Get the air temperature at altitude.
     * 
     * @return Returns the air temperature. (May return null)
     */
//    public Double getTemperature() {
    public Float getTemperature() {
        return temperature;
    }

    /**
     * Get the reported wind direction.
     * 
     * @return Returns the wind direction. (May return null)
     */
//    public Integer getWindDirection() {
    public Float getWindDirection() {  
        return windDirection;
    }

    /**
     * Get the reported wind speed.
     * 
     * @return Returns the wind speed. (May return null)
     */
//    public Double getWindSpeed() {
    public Float getWindSpeed() {
        return windSpeed;
    }

    /**
     * Get any remarks found in the report.
     * 
     * @return Returns the remarks. (May return null)
     */
    public String getRemarks() {
        return (theRemarks != null) ? theRemarks.toString() : "";
    }

    /**
     * Get the cloud layer information.
     * 
     * @return Returns the decoded cloud layers. (May return null)
     */
    public List<AircraftCloudLayer> getCloudLayers() {
        return cloudLayers;
    }

    /**
     * Get the turbulence layer information.
     * 
     * @return Returns the turbulence layers. (May return null)
     */
    public List<AircraftFlightCondition> getTurbulenceLayers() {
        return theTurbulenceLayers;
    }

    /**
     * Get the icing layer information.
     * 
     * @return Returns the icing layers. (May return null)
     */
    public List<AircraftFlightCondition> getIcingLayers() {
        return theIcingLayers;
    }

    /**
     * Get the flight visibility if decoded.
     * 
     * @return Returns the flight visibility. (May return null)
     */
    public Integer getHorzVisibility() {
        return horzVisibility;
    }

    /**
     * Get the decoded weather information.
     * 
     * @return Returns the weather codes. (May return null)
     */
    public String[] getWeatherCodes() {
        return theWeatherCodes;
    }

    /**
     * This method parses the pirep based on TEIs and delegates the data for
     * each to a different method. This method ported from NCEP decoder.
     * "HJO UA /OV AUN-CRO /TM 2351 /FL075 /TP M20P /WX FV30SM HZ FU /WV 15015KT /TB NEG /RM SMTH="
     * @throws DecodeException
     *             An error occurred within this method.
     */
    protected void parse() {
        int pirepIndex = 0;
        int endTimeId = 6;
        
        // remove \r \n white space
        reportData = reportData.replaceAll("[\r\n]", " ");
        reportData = reportData.replaceAll("[ ]{2,}", " ");
        
    	// reportData start with hmo_header time, then statioId
    	if (reportData != null && reportData.length() >=10 ) //time + stationId =10
    		headerTime = reportData.substring(pirepIndex, endTimeId);    	
    	
    	int stationStartIndex = endTimeId + 1;
    	
        String theStationId = reportData.substring(stationStartIndex, stationStartIndex +3);
        decodeReportingStationId(theStationId);
        
        // Look for a 'UUA' or 'UA' indicator to denote the start of
        // the report.
        if (!nextString(reportData, ( stationStartIndex + 3 + 1 ), reportData.length(), RPIDS)) {
            return;
        }
        
        // if failed to decode reporting station id, this must be Canadian pirep
        // TODO convert!

        // Find the first TEI within the report.
        if (!nextString(reportData, pirepIndex, reportData.length(), TEIS)) {
            return;
        }
        
        int pos1 = this.theSearchPos;
        int tei1 = this.theTokenPos;
        
        int teiIndex = pos1 + TEIS[tei1].length();
        int pos2, tei2;

        while (teiIndex < reportData.length()) {
        	
            // Find the end of the data for this TEI by finding the next
            // TEI within the report.
            int endTeiIndex;
            boolean lastTei = !nextString(reportData, teiIndex, reportData
                .length(), TEIS);
            pos2 = this.theSearchPos;
            tei2 = this.theTokenPos;
            //System.out.println("%%%%%%%%%tei2 "+tei1 +" "+pos1 +" "+teiIndex);
            if (lastTei) {
                // This is the last TEI within the report, so set the end
                // of the data for this TEI equal to the end of the report.
                endTeiIndex = reportData.length();
            } else {
                endTeiIndex = pos2;
            }

            boolean success = false;
            switch (tei1) {
                case 0:
                    // Decode and store the "/OV" (i.e. location) data.
                    success = decodeLocationData(reportData.substring(teiIndex,
                        endTeiIndex));
                    break;
                case 1:
                    // Decode and store the "/TM" (i.e. time) data.
                    success = decodeTimeData(reportData.substring(teiIndex,
                        endTeiIndex));
                    break;
                case 2:
                    // Decode and store the "/FL" (i.e. flight level) data.
                    success = decodeFlightLevelData(reportData.substring(
                        teiIndex, endTeiIndex));
                    break;
                case 3:
                    // Decode and store the "/TP" (i.e. aircraft type) data.
                	success = decodeAircraftTypeData(reportData.substring(
                        teiIndex, endTeiIndex));
                    break;
                case 4:
                    // Decode and store the "/SK" (i.e. sky cover) data.
                	success = decodeSkyCoverData(reportData.substring(
                    	teiIndex, endTeiIndex));
                    break;
                case 5:
                    // Decode and store the "/WX" (i.e. weather) data.
                    success = decodeWeatherData(reportData.substring(teiIndex,
                        endTeiIndex));
                    break;
                case 6:
                    // Decode and store the "/TA" (i.e. temperature) data.
                    success = decodeTemperatureData(reportData.substring(
                        teiIndex, endTeiIndex));
                    break;
                case 7:
                    // Decode and store the "/WV" (i.e. wind) data.
                    success = decodeWindData(reportData.substring(teiIndex,
                        endTeiIndex));
                    break;
                case 8:
                    // Decode and store the "/TB" (i.e. turbulence) data.
                	success = decodeTurbulenceData(reportData.substring(
                        teiIndex, endTeiIndex));
                    break;
                case 9:
                    // Decode and store the "/IC" (i.e. icing) data.
                	success = decodeIcingData(reportData.substring(teiIndex,
                        endTeiIndex));
                    break;
                case 10:
                    // Decode and store the "/RM" (i.e. remarks) data.
                    success = decodeRemarksData(reportData.substring(teiIndex,
                        endTeiIndex));
                    break;
            } // switch()

            // discontinue if processing of any data fails
            if (!success) {
                // TODO: add logging
            }

            if (lastTei) {
                teiIndex = reportData.length();
            } else {
                pos1 = pos2;
                tei1 = tei2;
                teiIndex = pos1 + TEIS[tei1].length();
            }
        } // while()
    } // parse()

    /**
     * This method gets the reporting station id for a PIREP. The reporting
     * station id is the station id which immediately precedes the characters UA
     * or UUA. This method ported from NCEP decoder.
     * 
     * @param str
     *            String preceding UA or UUA
     * @return success or failure
     */
    protected boolean decodeReportingStationId(String str) {
        //String id = str.trim();

        this.theReportingStationId = str;
        return true;
    } // decodeReportingStationId()

    /**
     * This method decodes and stores the location data from within a PIREP
     * report. The location data may be a set of one or two "location points"
     * (i.e. a "location point" is a navaid or a navaid/bearing/distance), or it
     * may be a latitude/longitude combination such as is found in AIREP
     * reports.
     * 
     * @param locData
     *            All data located in the PIREP report "OV" section.
     * @return Decode success or failure status.
     * @throws DecodeException
     *             If a decode error occured.
     */
    protected boolean decodeLocationData(String locData) {
        boolean decodeStatus = false;
        final int maxOVParts = 2;

        // Remove space in locData. handle abormal input. e.g. 9000N 18000E,  /OV AEF 200067
        String[] split = locData.split(" ");
        if (split != null && split.length ==2)
        	locData = split[0] + split[1];
        
        Pattern p = Pattern.compile(LATLON_PTRN);
        Matcher m = p.matcher(locData);
        if(m.find()) {
            location = parseLatLon(m.group());
            decodeStatus = (location != null);
        } 
        else {
            ArrayList<String> ovParts = new ArrayList<String>();

            StringTokenizer st = new StringTokenizer(locData, "-", false);
            while (st.hasMoreTokens()) {
                String s = st.nextToken();
                ovParts.add(s.toUpperCase());
            }
            // If there are more than 2 parts, this location data is in error BUT
            // we will use the first two points and ignore trailing!
            if (ovParts.size() == 1) {
                decodeStatus = decodeLocation(ovParts.get(0).trim());
            } else if (ovParts.size() >= maxOVParts) {
                // If valid, this will leave the first location in theLocation.
                if (decodeLocation(ovParts.get(0).trim())) {
                    // Make sure to save the previous location. decodeLocation
                    // will trash it.
                    LatLonPoint firstPoint = interLocation;
                    // now get the second point.
                    if (decodeLocation(ovParts.get(1).trim())) {
                        LatLonPoint secondPoint = interLocation;

                        // Now we have two valid points.
                        // get the midpoint distance from point 1 to point 2.
                        double dist = firstPoint.distanceTo(secondPoint) / 2;
                        // and use the bearing between the points to construct the
                        // midpoint location.
                        secondPoint = firstPoint.positionOf(firstPoint
                                .bearingTo(secondPoint), dist);
                        location = new BasePoint(secondPoint
                                .getLatitude(LatLonPoint.INDEGREES), secondPoint
                                .getLongitude(LatLonPoint.INDEGREES));
                        decodeStatus = true;
                        if(ovParts.size() > maxOVParts) {
                            logger.info(traceId + "- More than 2 locations reported, first 2 used");
                        }
                    } else {
                        logger.error(traceId + "- Could not decode location " + ovParts.get(1).trim());
                    }
                } else {
                    logger.error(traceId + "- Could not decode location " + ovParts.get(0).trim());
                }
            }
        }
        return decodeStatus;
    } // decodeLocationData()

    /**
     * Decode the individual location points found in an "OV" section.
     * 
     * @param aLocation
     *            A possible location point.
     * @return Was the decode successful (true|false).
     */
    private boolean decodeLocation(String aLocation) {
        final String navaidPattern = "^[A-Z,0-9]{3,4}$";
        final String bearingDistPattern = "^[A-Z,0-9]{3,4}\\d{6}$";

        Pattern p = Pattern.compile(navaidPattern);
        location = null;
        boolean decodeStatus = false;
        Matcher m = p.matcher(aLocation);
        if (m.find()) {
            BasePoint navaidLoc = getNavaidLocation(aLocation);
            if (navaidLoc != null) {
                location = navaidLoc;
                
                interLocation = new LatLonPoint(navaidLoc
                        .getLatitude(), navaidLoc.getLongitude(),
                        LatLonPoint.INDEGREES);
                
                decodeStatus = true;
            }
        } else {
            // try bearing and distance.
            p = Pattern.compile(bearingDistPattern);
            m = p.matcher(aLocation);
            if (m.find()) {
                String staId = aLocation.substring(0, aLocation.length() - 6);

                BasePoint navaidLoc = getNavaidLocation(staId);
                if (navaidLoc != null) {
                    int len = aLocation.length();

                    String s = aLocation.substring(len - 6, len - 3);
                    Double bearing = Math.toRadians(parseInteger(s));

                    s = aLocation.substring(len - 3);
                    Double dist = ONE_NM_RADIANS * parseInteger(s);
                    
                    
                    LatLonPoint point1 = new LatLonPoint(navaidLoc
                            .getLatitude(), navaidLoc.getLongitude(),
                            LatLonPoint.INDEGREES);

                    LatLonPoint point2 = point1.positionOf(-bearing, dist);
                    interLocation = point2;

                    location = new BasePoint(
                            point2.getLatitude(LatLonPoint.INDEGREES),
                            point2.getLongitude(LatLonPoint.INDEGREES));

                    decodeStatus = true;
                } else {
                    decodeStatus = false;
                }
            } else {
            }
        }
        return decodeStatus;
    } // decodeLocation()

    /**
     * Attempt to convert a string to an integer value.
     * 
     * @param aNumber
     *            A possible integer value.
     * @return The converted number or null if conversion failed.
     */
    private static Integer parseInteger(String aNumber) {
        Integer retValue = null;
        try {
            retValue = new Integer(aNumber.trim());
        }
        catch (NumberFormatException nfe) {
            retValue = null;
        }
        return retValue;
    } // parseInteger()

    /**
     * Get the latitude/longitude pair for a given navigation point.
     * 
     * @param aNavaid
     *            A possible navaid to look up.
     * @return The location of the navaid or null if not found.
     */
    private BasePoint getNavaidLocation(String navaid) {
        BasePoint navaidLocation = null;

        ObStationDao obSta = null;
        ObStation stationInfo = null;
        if ((navaid != null) && (navaid.length() >= 0)) {
            logger.debug(traceId + " - Processing Navaid [" + navaid + "]");
            try {
                obSta = new ObStationDao();
                if(navaid.length() == 3) {
                    String gid = ObStation.createGID(ObStation.CAT_TYPE_ACFT_PIREP, navaid);
                    
                    stationInfo = obSta.queryByGid(gid);
                } else if (navaid.length() == 4) {
                    String gid = ObStation.createGID(ObStation.CAT_TYPE_ICAO, navaid);
                    stationInfo = obSta.queryByGid(gid);
                }
            } catch(DataAccessLayerException e){
                logger.error(traceId + " - Unable to retrieve station info",e);
            } 
        } else {
            logger.info(traceId + " - Invalid navaid [" + navaid + "]");
        }

        if (stationInfo != null) {
            Geometry g = stationInfo.getGeometry();
            if(g instanceof Point) {
                Point p = (Point) g;
                navaidLocation = new BasePoint(p.getY(),p.getX());
            }
        } else {
            logger.info(traceId + " - Station id not found [" + navaid + "]");
        }
        return navaidLocation;
    } // getNavaidLocation()

    /**
     * 
     */
    private static BasePoint parseLatLon(String latlon) {
        BasePoint point = null;
        
        // 012345678901
        // lldds llldds    
        
        Integer lat_dd = parseInteger(latlon.substring(0,2));
        Integer lat_mm = parseInteger(latlon.substring(2,4));
        Integer lon_dd = 0;
        Integer lon_mm = 0;
        int lastLetterIdx = 0;
        
        if (latlon.split(" ").length ==2) {
        	lon_dd = parseInteger(latlon.substring(6,9));
        	lon_mm = parseInteger(latlon.substring(9,11));
        	lastLetterIdx = 11;
        }
        else {
        	lon_dd = parseInteger(latlon.substring(5,8));
        	lon_mm = parseInteger(latlon.substring(8,10));
        	lastLetterIdx = 10;
        }
        
        if((lat_dd != null)&&(lat_mm) != null) {
            if((lon_dd != null)&&(lon_mm) != null) {

                Double lat = lat_dd + (lat_mm / 60.0d);
                Double lon = lon_dd + (lon_mm / 60.0d);
                if(lat_dd.equals(0)&&(lat_mm.equals(0))) {
                    lat = 0.0;
                } else {
                    switch(latlon.charAt(4)) {
                    case 'N' : {
                        break;
                    }
                    case 'S' : {
                        lat = lat * -1;
                        break;
                    }
                    default : {
                        lat = null;
                    }
                    }
                }
                if(lon_dd.equals(0)&&(lon_mm.equals(0))) {
                    lon = 0.0;
                } else {
                    switch(latlon.charAt(lastLetterIdx)) {
                    case 'E' : {
                        break;
                    }
                    case 'W' : {
                        lon = lon * -1;
                        break;
                    }
                    default : {
                        lon = null;
                    }
                    }
                }
                if(lat != null && lon != null) {
                    point = new BasePoint(lat, lon);
                }
            }            
        }
        return point;
    }
    
    /**
     * This method decodes a location point (i.e. a "location point" is either a
     * navaid or a navaid/bearing/distance) from within the location data of a
     * PIREP report. This method ported from NCEP decoder.
     * 
     * @param fields
     *            "like-type" fields which contain location data
     * @param startIndex
     *            Index of "like-type" group which contains start of location
     *            point
     * @param aLoc
     *            latitude and longitude of location
     * @return Index of "like-type" group which contains end of location point,
     *         -1 on failure
     * @throws DecodeException
     *             If a decode error occured.
     */
    protected int decodeLocationPoint(String[] fields, int startIndex,
        BasePoint aLoc) {
        boolean offset = false;
        boolean other = false;
        boolean bad = false;
        int endIndex = -1;

        // LatLonPoint point1Loc = null;
        
        // The "like-type" group should be a 3-letter or 4-letter navaid
        // identifier.
        BasePoint navaidLoc = null;
        int i = startIndex;
        if ((Character.isLetter(fields[i].charAt(0)) && ((fields[i].length() == 3) || (fields[i]
            .length() == 4)))
            || (Character.isDigit(fields[i].charAt(0)) && (fields[i].length() <= 2))) {
            // Setting conditions if the navaid came after the reported
            // distance and range.
            if (Character.isDigit(fields[i].charAt(0))) {
                if ((i + 2) < fields.length) {
                    i += 2;
                    endIndex = i;
                    other = true;
                    if (fields[i].length() <= 2) {
                        bad = true;
                    }
                } else {
                    bad = true;
                }
            }

            // Locate the entry for this navaid in the PIREP navaids table
            if (!bad) {
                navaidLoc = getNavaidLocation(fields[i]);
                if (navaidLoc == null) {
                    // LOG.error("Could not find " + fields[i] + " in Pirep
                    // Navaids table.");
                    return -1;
                }

                // store the first location navaid as the platform station
                if (navaidLoc == null) {
                    // thePlatformStation = platformStation;
                }

                // create location for the navaid
//                point1Loc = new LatLonPoint(navaidLoc.getLatitude(),
//                        navaidLoc.getLongitude(),
//                LatLonPoint.INDEGREES);
            }
        } else {
            bad = true;
        }

        if (bad) {
            // throw new DecodeException("Invalid Navaid in Pirep Location: "
            // + fields[i]);
        }

        // Looking for the bearing and distance offset, if there is one.
        int bearingDegrees = -1;
        int distanceNm = -1;
        if (!other && ((i + 1) < fields.length)
            && Character.isDigit(fields[i + 1].charAt(0))
            && (fields[i + 1].length() == 6)) {
            endIndex = i + 1;
            bearingDegrees = Integer.parseInt(fields[i + 1].substring(0, 3));
            distanceNm = Integer.parseInt(fields[i + 1].substring(3, 6));
            offset = true;
            if ((bearingDegrees < 0) || (bearingDegrees > 360)
                || (distanceNm < 0)) {
                // throw new DecodeException("Invalid bearing / distance: "
                // + bearingDegrees + " / " + distanceNm + " data: "
                // + fields[i + 1]);
            }
        } else if (other) {
            // getting bearing and distance of aircraft
            distanceNm = Integer.parseInt(fields[i - 2]);
            bearingDegrees = decodeLocationDirection(fields[i - 1]);
            offset = true;
        }

        BasePoint loc = null;
        if (offset) {
        // // convert distance from nautical miles to kilometers
        // Length distance = distanceNm;
        // // convert bearing from degrees to radians
        // AngularMeasure bearing = new AngularMeasure()
        // .fromDegree(bearingDegrees);
        //
        // // Compute the latitude and longitude of the location
        // // point by applying the bearing and distance offsets
        // // to the navaid latitude and longitude.
        // // (assume spherical earth)
        // double distanceRad = distance.getKilometers()
        // / PhysPar.EARTH_RADIUS_KM;
        // loc = LatLonPoint.positionOf(navaidLoc.getLatitude(), navaidLoc
        // .getLongitude(), bearing.getRadians(), distanceRad);
        // } else {
        // // The location point is simply the navaid itself, so the
        // // latitude and longitude of the location point are equal
        // // to the latitude and longitude of the navaid.
        // endIndex = i;
        // loc = navaidLoc;
        }
        aLoc.setLatitude(loc.getLatitude());
        aLoc.setLongitude(loc.getLongitude());

        return endIndex;
    }

    /**
     * This method decodes a location direction from a given compass point from
     * within the location data of a PIREP report. This method ported from NCEP
     * decoder.
     * 
     * @param aField
     *            A possible location direction to decode.
     * @return location direction in degrees, -1 on failure
     */
    protected int decodeLocationDirection(String aField) {
        int bearingDegrees = -1;
        if(DIR_POINTS.containsKey(aField)) {
            bearingDegrees = DIR_POINTS.get(aField).intValue();
        }
        return bearingDegrees;
    }

    /**
     * This subroutine decodes and stores the time (i.e. report hour and report
     * minutes) from AMDAR, AIREP, PIREP, and RECCO reports. This method ported
     * from NCEP decoder.
     * 
     * @param str
     *            time data
     * @return true on success
     * @throws DecodeException
     *             If a decode error occured.
     */
    protected boolean decodeTimeData(String str) {   	
    	//get current calendar and set day, hour and minute
        str = str.trim();
        Calendar oTime = TimeTools.getSystemCalendar();
        
    	Calendar issuTime = TimeTools.copy(oTime);
    	int day = Integer.parseInt(headerTime.substring(0, 2));
    	int hour = Integer.parseInt(headerTime.substring(2, 4));
    	int minute = Integer.parseInt(headerTime.substring(4));
    	
    	int year = oTime.get(Calendar.YEAR); 
    	int month = oTime.get(Calendar.MONTH);
    	//System.out.println("DAY "+day +" "+ oTime.get(Calendar.DAY_OF_MONTH));
        if ( day - oTime.get(Calendar.DAY_OF_MONTH ) >1) { //if obs day is much bigger than current day
        	month = month - 1;
        	if(month == 0) {
        		month = 12;
        		year = year-1;
        	}
        	issuTime.set(Calendar.MONTH, month);
        	issuTime.set(Calendar.YEAR, year);
        }
        
        issuTime.set(Calendar.MONTH, month);
        issuTime.set(Calendar.DAY_OF_MONTH, day);
    	issuTime.set(Calendar.HOUR_OF_DAY,hour);
    	issuTime.set(Calendar.MINUTE,minute);
    	issuTime.set(Calendar.SECOND,0);
    	issuTime.set(Calendar.MILLISECOND,0);
        
        if (str.length() == 4) {
        	hour = Integer.parseInt(str.substring(0, 2));
        	minute = Integer.parseInt(str.substring(2, 4));
        	
        	observationTime = TimeTools.copy(oTime);   
        	observationTime.set(Calendar.YEAR, year);
        	observationTime.set(Calendar.MONTH, month);
        	observationTime.set(Calendar.DAY_OF_MONTH, day);
        	observationTime.set(Calendar.HOUR_OF_DAY,hour);
        	observationTime.set(Calendar.MINUTE,minute);
        	observationTime.set(Calendar.SECOND,0);
        	observationTime.set(Calendar.MILLISECOND,0);
        	
        	if (observationTime.getTime() != issuTime.getTime())
        		observationTime = checkDayInTime (observationTime, issuTime);
        	//System.out.println("***********time2 "+observationTime.getTime());
        	oTime.add(Calendar.MINUTE, ALLOWABLE_TIME);
        	if(observationTime.compareTo(oTime) > 0) {
        		observationTime.add(Calendar.DAY_OF_MONTH,-1);
        	}
        	
        	return true;
        }
        else if (str.length() == 6) {
        	day = Integer.parseInt(str.substring(0, 2));
        	hour = Integer.parseInt(str.substring(2, 4));
        	minute = Integer.parseInt(str.substring(4, 6));
        
        	observationTime = TimeTools.copy(oTime);   
        	observationTime.set(Calendar.YEAR, year);
        	observationTime.set(Calendar.MONTH, month);
        	observationTime.set(Calendar.DAY_OF_MONTH, day);
        	observationTime.set(Calendar.HOUR_OF_DAY,hour);
        	observationTime.set(Calendar.MINUTE,minute);
        	observationTime.set(Calendar.SECOND,0);
        	observationTime.set(Calendar.MILLISECOND,0);
        
        	oTime.add(Calendar.MINUTE, ALLOWABLE_TIME);
        	if(observationTime.compareTo(oTime) > 0) {
        		observationTime.add(Calendar.DAY_OF_MONTH,-1);
        	}

        	return true;
        }
        else
        	return false;
    }
    /*
     * In 'ENA UA /OV PDN288051/TM 2348/FL340/TP B737/TA M56/WV 218035/TB CONT LGHT OCNL MOD/RM CONT CHOP FL350 ZAN =', 'TM 2348' is the obs time.
     * Get the day from wmoHeader and add to front of '2348'
     */
    private Calendar checkDayInTime (Calendar obs, Calendar issue) {
    	
		long tdif = Math.abs(obs.getTime().getTime() - issue.getTime().getTime());

		final long MIN_HOUR = 60 * 60 * 1000;	    	
		final long MAX_HOUR = 23 * MIN_HOUR;

		if ( obs.compareTo(issue) > 0 ) {    			 
			if ( tdif <= MIN_HOUR ) {
				;
			} else if ( tdif >= MAX_HOUR ) {
				obs.add(Calendar.DATE, -1);
			} else if ( tdif > MIN_HOUR && tdif < MAX_HOUR ) {
				obs.add(Calendar.DATE, -1);
				suspectTimeFlag = "true";
			}
		} 
		else {
			if ( tdif <= MIN_HOUR ) {
				;
			} else if ( tdif >= MAX_HOUR ) {
				obs.add(Calendar.DATE, 1);
			} else if ( tdif > MIN_HOUR && tdif < MAX_HOUR ) {
				suspectTimeFlag = "true";
			}                 
		}
		
		return obs;		
    }


    /**
     * This method decodes PIREP flight level.
     * 
     * @param str
     *            Flight Level Data
     * @return true on success
     */
    protected boolean decodeFlightLevelData(String str) {
        String regex = "([0-9]{3})(?:-([0-9]{3}))?";
        Pattern pattern = Pattern.compile(regex);

        // Get a Matcher based on the target string.
        Matcher matcher = pattern.matcher(str);
        if (matcher.find()) {
            Integer fltLevel = decodeHeight(matcher.group(1));

            if (matcher.group(2) != null) {
                Integer upperLevel = decodeHeight(matcher.group(2));

                fltLevel = (fltLevel + upperLevel) / 2;
            }
            flightLevel = new AircraftFlightLevel(fltLevel);
        }

        // if the flight level has anything non-numeric such as UNK, UNKN, DURC,
        // DURD, VFR, etc.
        // the flight level will be left null and stored by the decoder as
        // unknown

        return true;
    }

    /**
     * This method decodes a 3-digit string containing a height value in units
     * of hundreds-of-feet into a real height value in units of feet. This
     * method ported from NCEP decoder.
     * 
     * @param aHeight
     *            Height data to decode.
     * @return The aircraft height information or null if the decode failed.
     */
    protected static Integer decodeHeight(String aHeight) {
        Integer altHundredsFeet = null;
        if (aHeight.length() == 3) {
            if(!"UNKN".equals(aHeight)) {
                // unknown heights
                if ((aHeight.indexOf("UNK") != -1) || (aHeight.indexOf("ABV") != -1)
                    || (aHeight.indexOf("BLO") != -1)) {
                    altHundredsFeet = null;
                } else {
                    altHundredsFeet = Integer.parseInt(aHeight) * 100;
                }
            }
        }
        return altHundredsFeet;
    }

    /**
     * This method decodes and stores the aircraft type data from within a PIREP
     * report. This method ported from NCEP decoder.
     * 
     * @param aType
     *            Aircraft type data
     * @return Was the aircraft type decoded. (always true)
     */
    protected boolean decodeAircraftTypeData(String aType) {
        aType = aType.trim();

        // truncate at 8 characters
        if (aType.length() <= 8) {
            aircraftType = new String(aType);
        } else {
            aircraftType = new String(aType.substring(0, 8));
        }

        return true;
    }

    /**
     * This method decodes and stores the temperature data from within a PIREP
     * report. This method ported from NCEP decoder.
     * 
     * @param aTemperature
     *            A possible temperature to be decoded.
     * @return Was the temperature data decoded.
     * @throws DecodeException
     *             If a decode error occured.
     */
    protected boolean decodeTemperatureData(String aTemperature) {
        // Break up the input string into groups of "like-type" in order
        // to facilitate decoding.
        String[] fields = groupLikeTypes(aTemperature);
        if (fields.length == 0) {
            return false;
        }

        // Locate, decode, and store the temperature data from within
        // the first 3 "like-type" groups. It is identifiable as a
        // numeric "like-type" group of at least 1 digit, possibly
        // preceded by a "like-type" group containing the sign of
        // the temperature.
        int numGroups = Math.min(3, fields.length);
        for (int i = 0; i < numGroups; i++) {
            if ((fields[i].length() >= 1)
                && (Character.isDigit(fields[i].charAt(0)))) {
//                double temp;
                float temp = RMISSD;
                if (i == 0) {
                    // This is the first "like-type" group, so assume
                    // that the sign of the temperature is positive.
//                	temp = decodeTemperatureData("+", fields[i];
                    temp = (float)decodeTemperatureData("+", fields[i]);
                } else {
                    // Assume that the previous "like-type" group
                    // contains the sign of the temperature.
//                    temp = decodeTemperatureData(fields[i - 1], fields[i]);
                    temp = (float)decodeTemperatureData(fields[i - 1], fields[i]);
                }

//qu                if (Double.isNaN(temp)) {
//                	/* 8888888888888888888 */              	
//                	temp = RMISSD;
//                	System.out.println("   temp is NaN So set to RMISSD???");
//                	/* 888 */
//                    return false;
//                }

                // Check if temperature was reported in Fahrenheit.
                if ((i < (fields.length - 1))
                    && ((fields[i + 1].length() == 1) && (fields[i + 1]
                        .charAt(0) == 'F'))) {
                    // temperature = new Temperature().fromFahrenheit(temp);
                } else {
                    temperature = temp;
                }
                break;
            }
        }

        return true;
    }

    /**
     * Helper method to decode temperature in PIREP. This method was ported from
     * NCEP decoder.
     * 
     * @param sign
     *            Sign of the data i.e. {"PS, P, +, MS, M, -}.
     * @param temp
     *            Candidate temperature value.
     * @return temperature on success, NaN on failure
     * @throws DecodeException
     *             If a decode error occured.
     */
    protected double decodeTemperatureData(String sign, String temp) {
        double rmult = Double.NaN;
        double rdiv = Double.NaN;

        // validate length
        if ((temp.length() < 1) || (temp.length() > 3) || (sign.length() < 1)
            || (sign.length() > 2)) {
            // throw new DecodeException("Unrecognized temperature data format
            // in Pirep: " + sign + temp);
        }

        // Determine divisor based on the length of the input temperature
        // string.
        if (temp.length() == 3) {
            rdiv = 10.0;
        } else {
            rdiv = 1.0;
        }

        // Determine the length of the input temperature sign string,
        // and then decode the sign of the temperature accordingly.
        if (sign.length() == 2) {
            if ("PS".equals(sign)) {
                rmult = 1.0;
            } else if ("MS".equals(sign)) {
                rmult = -1.0;
            }
        } else {
            if ("P".equals(sign) || "+".equals(sign)) {
                rmult = 1.0;
            } else if ("M".equals(sign) || "-".equals(sign)) {
                rmult = -1.0;
            }
        }
        if (Double.isNaN(rmult)) {
            // throw new DecodeException("Unrecognized temperature sign data
            // format in Pirep: " + sign);
        }

        double t = (double) Integer.parseInt(temp) * rmult / rdiv;
        return t;
    }

    /**
     * This method decodes the sky cover data.
     * 
     * @param str
     *            Sky cover data to be decoded.
     * @return Was the sky cover correctly decoded?
     * @throws DecodeException
     *             If a decode error occured.
     */
    protected boolean decodeSkyCoverData(String sky) {
        // Format is NNN(-NNN)bbb(-TOPttt)/NNN(-NNN)bbb, etc.
        // NNN is the three-letter contraction for the amount of cloud cover
        // bbb is the height of the base of a layer of clouds in hundreds of
        // feet
        // ttt is the top of the layer in hundreds of feet
    	
    	String[] str2 = sky.split("/");
    	for (String str :str2) {
    		   		
//    		String regex = "([A-Z]{3}|UNKN?)(?:-([A-Z]{3}))?([0-9]{3}|UNKN?)(?:-TOP([0-9]{3}|UNKN?))?/?";
//            Pattern pattern = Pattern.compile(regex);
//            // Get a Matcher based on the target string.
//            Matcher matcher = pattern.matcher(str);
//            while (matcher.find()) {
//                if (cloudLayers == null) {
//                    cloudLayers = new ArrayList<AircraftCloudLayer>();
//                }
//
//                // NNN
//                String cloud_1 = matcher.group(1);
//                // (NNN)
//                String cloud_2 = matcher.group(2);
//                // bbb
//                String baseHeight = matcher.group(3);
//                // (ttt)
//                String topHeight = matcher.group(4);
//
//                if (cloud_1 == null) {
//                    // throw new DecodeException("Unknown format of Sky Cover Data
//                    // in Pirep: " + str);
//                }
//
//                // TODO this should be mapped in the parameter lookups
//                if (SK_SKC.equals(cloud_1)) {
//                    cloud_1 = SK_CLR;
//                }
//                if (SK_SKC.equals(cloud_2)) {
//                    cloud_2 = SK_CLR;
//                }
//
//                AircraftCloudLayer layer = new AircraftCloudLayer();
//                layer.setCloudCover1(cloud_1);
//                layer.setCloudCover2(cloud_2);
//
//                if ((baseHeight != null)) {
//                    layer.setCloudBaseHeight(decodeHeight(baseHeight));
//                }
//                if ((topHeight != null)) {
//                    layer.setCloudTopHeight(decodeHeight(topHeight));
//                }
//                cloudLayers.add(layer);
                
    		AircraftCloudLayer layer = new AircraftCloudLayer();
    		
    		//Separate following regex to 2 part to fix all cases
    		//String regex = "([A-Z]{3}|UNKN?)(?:-([A-Z]{3}))?([0-9]{3}|UNKN?)(?:-TOP([0-9]{3}|UNKN?))?/?";
    		String regex = "([A-Z]{3}|UNKN?)(?:-([A-Z]{3}))?";
            
            Pattern pattern = Pattern.compile(regex);
            Matcher matcher = pattern.matcher(str);
            
            if (matcher.find()) {  
            	
            	if (cloudLayers == null) {          		
            		cloudLayers = new ArrayList<AircraftCloudLayer>();
            	}
            	
            	String cloud_1 = matcher.group(1);
            	String cloud_2 = matcher.group(2);
            	      
            	if (WX_COND_WORDS.get(cloud_1) != null)
            		cloud_1 = WX_COND_WORDS.get(cloud_1);  
            	if (WX_COND_WORDS.get(cloud_2) != null)
            		cloud_2 = WX_COND_WORDS.get(cloud_2);
                
                if (cloud_1 != null )
                	layer.setCloudCover1(cloud_1);                
                if (cloud_2 != null)
                	layer.setCloudCover1(cloud_1 + cloud_2);
                
            }
               
            regex = "([0-9]{3}|UNKN?)(?:-TOP([0-9]{3}|UNKN?))?";
            pattern = Pattern.compile(regex);
            matcher = pattern.matcher(str);
            
            if (matcher.find()) {
            	if (cloudLayers == null) {          		
            		cloudLayers = new ArrayList<AircraftCloudLayer>();
            	}
            	String baseHeight = matcher.group(1);
            	String topHeight = matcher.group(2);
            	
            	if ((baseHeight != null)) {
            		layer.setCloudBaseHeight(decodeHeight(baseHeight));
            	}
            	if ((topHeight != null)) {
            		layer.setCloudTopHeight(decodeHeight(topHeight));
            	}
            }	
            
            if (cloudLayers != null)
            	cloudLayers.add(layer);         
            
        // TODO alternate sky cover format and separator of cloud cover can be
        // other than -
        // also other possibilities in afpsky.f
        // also TOP can be TPS, and can be second top height
    	}
        return true;
    }

    /**
     * This method decodes the wind data.
     * 
     * @param str
     *            Wind data to be decoded.
     * @return Was the wind data properly decoded?
     * @throws DecodeException
     *             If a decode error occured.
     */
    protected boolean decodeWindData(String str) {
        // Format is dddff(f)KT
        // ddd is three digit true direction in whole degrees from
        // which the wind is blowing
        // ff(f) is the wind speed in knots, followed by KT
        String regex = "([0-9]{3})([0-9]{2,3})(?:KT)?";
        Pattern pattern = Pattern.compile(regex);
        // Get a Matcher based on the target string.
        Matcher matcher = pattern.matcher(str);
        if (matcher.find()) {
//            windDirection = Integer.parseInt(matcher.group(1));
            windDirection = Float.parseFloat(matcher.group(1));

            int windSpeedKnots = Integer.parseInt(matcher.group(2));
//            windSpeed = new Double(windSpeedKnots); // Speed().fromKnots(windSpeedKnots);
            windSpeed = new Float(windSpeedKnots); // Speed().fromKnots(windSpeedKnots);
        } else {
            // throw new DecodeException("Unrecognized wind data format in
            // Pirep: " + str);
        }

        return true;
    }

    /**
     * This method decodes the turbulence data.
     * 
     * @param str
     *            Turbulence data to be decoded.
     * @return Was the turbulence properly decoded?
     * @throws DecodeException
     *             If a decode error occured.
     */
    protected boolean decodeTurbulenceData(String turbLayers) {
        // Format is III(-III)( CAT or CHOP ) (bbb-ttt)/III(-III) etc.
        // III is the intensity of the turbulence
        // CAT or CHOP is the type of turbulence
        // bbb is the height of the base of a layer of clouds in hundreds of
        // feet
        // ttt is the top of the layer in hundreds of feet
//        String regex = "([A-Z]{3,5})(?:((?:-)([A-Z]{3,5})))?((?: )"
//                + "(CHOP|CAT))?(?: )?(((ABV|BLO)(?: )(\\d{3}))|((\\d{3})"
//                + "(?:(-(\\d{3}))?)))?(?:/)?()";
    	
//    	String[] str2 = turbLayers.split("/");
//    	for (String str :str2) {
//        theTurbulenceLayers = new ArrayList<AircraftFlightCondition>();
//        
//        if (str.indexOf(NEG_ENTRY) > 0) {
//            AircraftFlightCondition at = new AircraftFlightCondition();
//
//            // NEG should be the only value! Used to indicate forecasted but
//            // not observed!
//            at.setIntensity1(NEG_ENTRY);
//            if (flightLevel != null) {
//                at.setBaseHeight(flightLevel.getFlightLevel());
//                theTurbulenceLayers.add(at);
//            }
//        } else {
//            Pattern pattern = Pattern.compile(TRB_REGEX);
//            // Get a Matcher based on the target string.
//            Matcher matcher = pattern.matcher(str);
//            // add each turbulence
//            while (matcher.find()) {
//                addFlightCondition(matcher, theTurbulenceLayers);
//            }
//        }
    	
    	//turbLayers = turbLayers.trim();
    	
        PirepTools tools = new PirepTools(turbLayers);
        theTurbulenceLayers = tools.decodeTurbulenceData();
        
        
        if (theTurbulenceLayers.size() == 0) {
            theTurbulenceLayers = null;
        }
    	
        return true;
    }

    /**
     * This method decodes the icing data.
     * 
     * @param str
     *            Icing data to be decoded.
     * @return Was the icing data decoded?
     * @throws DecodeException
     *             If a decode error occurred.
     */
    protected boolean decodeIcingData(String icing) {
        // Format is III(-III) (type) (bbb-ttt)/III(-III) etc.
        // III is the intesity of the icing
        // type is the type of icing
        // bbb is the height of the base of a layer of clouds in hundreds of
        // feet
        // ttt is the top of the layer in hundreds of feet
//        String regex = "([A-Z]{3,5})(?:((?:-)([A-Z]{3,5})))?((?: )"
//                + "(RIME|MXD|CLR))(?: )?(((ABV|BLO)(?: )(\\d{3}))|((\\d{3})"
//                + "(?:(-(\\d{3}))?)))?(?:/)?()";
    	
        String[] str2 = icing.split("/");
    	for (String str :str2) {
        theIcingLayers = new ArrayList<AircraftFlightCondition>();
        
        if (str.indexOf(NEG_ENTRY) > 0) {
            AircraftFlightCondition at = new AircraftFlightCondition();

            // NEG should be the only value! Used to indicate forecasted but
            // not observed!
            at.setIntensity1(NEG_ENTRY);
            if (flightLevel != null) {
                at.setBaseHeight(flightLevel.getFlightLevel());
                theIcingLayers.add(at);
            }
        } else {
            Pattern pattern = Pattern.compile(ICE_REGEX);
            Matcher matcher = pattern.matcher(str);
            
            if (matcher.find()) { 
                matcher = pattern.matcher(str); //need to re-match.  LGT RIME 014-083
            	while (matcher.find()) { 
            		//System.out.println("***str "+str);               	
                	addFlightCondition(matcher, theIcingLayers);                	
            	}
            }
            else {
            	Pattern pattern2 = Pattern.compile("([A-Z]{3,5})"); //only intensity.  LGT
            	Matcher matcher2 = pattern2.matcher(str);
            	while (matcher2.find()) {
            		
            		String s1 = str.substring(matcher2.start(), matcher2.end());           
                    
                    // check against ICING_COND_WORDS
                    s1 = WX_COND_WORDS.get(s1);
                    if((s1 == null) || (NULL_ENTRY.equals(s1))) {
                        return false;
                    }
                    
            		AircraftFlightCondition at = new AircraftFlightCondition();

                    // NEG should be the only value! Used to indicate forecasted but
                    // not observed!
                    if(NEG_ENTRY.equals(s1)) {
                        at.setIntensity1(s1);
                        if(flightLevel != null) {
                            at.setBaseHeight(flightLevel.getFlightLevel());
                        }
                    } else {
                        at.setIntensity1(s1);
                    }
                    
                    theIcingLayers.add(at);
            	}
            }
        }

        if (theIcingLayers.size() == 0) {
            theIcingLayers = null;
        }
    	}
        return true;
    }

    /**
     * This method decodes the weather data.
     * 
     * @param str
     *            Weather data to be decoded.
     * @return Was the weather information decoded?
     */
    protected boolean decodeWeatherData(String str) {
        // Format is (FVvvSM )ww( ww)( ww)
        // FVvvSM is flight visibility; vv is the value, SM indicates statute
        // miles (km otherwise)
        // type is the type of icing
        // bbb is the height of the base of a layer of clouds in hundreds of
        // feet
        // ttt is the top of the layer in hundreds of feet
        String regex = "^ ?(?:(FV)([0-9]{2})(SM)?)? ?(.*)";
        Pattern pattern = Pattern.compile(regex);
        // Get a Matcher based on the target string.
        Matcher matcher = pattern.matcher(str);
        matcher.find();

        if (matcher.group(1) != null) {
            // parse flight visibility
            if (matcher.group(3) != null) {
                horzVisibility = Integer.parseInt(matcher.group(2));
            } else {
                // theFlightVisibility = new
                // Length().fromKilometers(Integer.parseInt(matcher.group(2)));
            }
        }

        // parse weather codes
        regex = "([+|-]?[A-Z]{2,4})([0-9]{3}|UNKN?)?(?:-TOP([0-9]{3}|UNKN?))? ?";
        pattern = Pattern.compile(regex);
        matcher = pattern.matcher(matcher.group(4));
        ArrayList<String> codes = new ArrayList<String>();
        while (matcher.find()) {
            codes.add(matcher.group(1));
            // the level information is in groups 2 and 3 when provided,
            // but the metoc model does not store this data
        }
        theWeatherCodes = codes.toArray(new String[0]);

        return true;
    }

    /**
     * This method decodes data in remarks section.
     * 
     * @param str
     *            Remarks data to be decoded.
     * @return Was the remarks data decoded? (Always returns true!)
     */
    protected boolean decodeRemarksData(String str) {
        theRemarks = new AircraftRemarks(str);
        return true;
    }

    /**
     * Utility method for decoding flight level information for turbulence and
     * icing layers.
     * 
     * @param matcher
     *            A Matcher instance for the specific source data.
     * @param layers
     *            The decoded layer information.
     * @throws DecodeException
     *             If a decode error occured.
     */
    protected void addFlightCondition(Matcher matcher, List<AircraftFlightCondition> layers) {
        
        if(matcher.groupCount() >= 13) {
            String s1 = matcher.group(1);           
            String s2 = matcher.group(3);
            //System.out.println("***matcher "+matcher.group(0)); //MOD null null RIME RIME null null null
            // Some words that may show up in group 1 or 3 that need to be
            // thrown away!
            s1 = WX_COND_WORDS.get(s1);
            if((s1 == null) || (NULL_ENTRY.equals(s1))) {
                return;
            }
            // Need to allow s2 to be null,
            if(s2 != null) {
                // but not null after lookup!
                s2 = WX_COND_WORDS.get(s2);
                if((s2 == null) || (NULL_ENTRY.equals(s2))) {
                    return;
                }
            }
            //******************************************************************
            
            AircraftFlightCondition at = new AircraftFlightCondition();

            // NEG should be the only value! Used to indicate forecasted but
            // not observed!
            if(NEG_ENTRY.equals(s1)) {
                at.setIntensity1(s1);
                if(flightLevel != null) {
                    at.setBaseHeight(flightLevel.getFlightLevel());
                }
            } else {
                at.setIntensity1(s1);
                at.setIntensity2(s2);
                
                s1 = matcher.group(5);
                s1 = WX_COND_WORDS.get(s1);
                if((s1 != null) && (!s1.equals(NULL_ENTRY))) {
                    at.setType(s1);
                }

                s1 = matcher.group(8);
                s2 = matcher.group(9);
                if(BLO_HGT.equals(s1)) {
                    at.setBaseHeight(PirepTools.UAIR_INTEGER_MISSING);
                    at.setTopHeight(decodeHeight(s2));
                    
                } else if (ABV_HGT.equals(s1)) {
                    at.setBaseHeight(decodeHeight(s2));
                    at.setTopHeight(PirepTools.UAIR_INTEGER_MISSING);
                } else {
                    // Check for one or more levels
                    s1 = matcher.group(11);
                    s2 = matcher.group(13);
                    if(s1 != null) {
                        at.setBaseHeight(decodeHeight(s1));
                    }
                    if(s2 != null) {
                        at.setTopHeight(decodeHeight(s2));
                    }
                    if((s1 != null)&&(s2 != null)) {
                        Integer base = at.getBaseHeight();
                        Integer top = at.getTopHeight();
                        if (base != PirepTools.UAIR_INTEGER_MISSING) {
                            if (top != PirepTools.UAIR_INTEGER_MISSING) {
                                if(base > top) {
                                    logger.debug(traceId + "- BASE-TOP inversion fixed");
                                    at.setBaseHeight(top);
                                    at.setTopHeight(base);
                                }
                            }
                        }
                    }
                    if((s1 == null)&&(s2 == null)) {
                        // Use the flight level if heights are not specified.
                        if(flightLevel != null) {
                            at.setBaseHeight(flightLevel.getFlightLevel());
                        }
                    }
                }
            }
            
            layers.add(at);
        }
    }

    /**
     * The nextString method determines the position of the first occurrence of
     * any of a list of substrings within the input string. This method provides
     * equivalent functionality to the routine ST_NXTS in the ported code. The
     * outputs are provided in member variables.
     * 
     * @param str
     *            Input string
     * @param firstPos
     *            First position to check
     * @param lastPos
     *            Last position to check
     * @param subStrings
     *            List of substrings
     * @return success or failure
     */
    protected boolean nextString(String str, int firstPos, int lastPos,
        String subStrings[]) {
        this.theTokenPos = -1;
        String tstr = str.substring(0, lastPos);
        for (int i = 0; i < subStrings.length; i++) {
            if ((this.theSearchPos = tstr.indexOf(subStrings[i], firstPos)) != -1) {
                this.theTokenPos = i;
                return true;
            }
        }

        return false;
    }

    /**
     * This method finds a string in a string array and returns its index.
     * 
     * @param str
     *            String to find
     * @param strs
     *            Strings to compare
     * @return index of matching string or -1 on failure
     */
    protected int findString(String str, String[] strs) {
        for (int i = 0; i < strs.length; i++) {
            if (strs[i].equals(str)) {
                return i;
            }
        }
        return -1;
    } // findString()

    /**
     * This method groups "like-types" of characters to facilitate decoding.
     * This method provides equivalent functionality to the UT_BKGP subroutine
     * in the ported code.
     * 
     * @param str
     *            String data to check.
     * @return An array of "like type" strings.
     */
    protected String[] groupLikeTypes(String str) {
        char[] chars = str.toCharArray();
        ArrayList<String> strs = new ArrayList<String>();
        int prevGroup = NONE;
        int curGroup = NONE;
        String group = new String();
        for (int i = 0; i < chars.length; i++) {
            if (Character.isWhitespace(chars[i])) {
                curGroup = NONE;
            } else if (Character.isDigit(chars[i])) {
                curGroup = DIGIT;
            } else if (Character.isLetter(chars[i])) {
                curGroup = LETTER;
            } else { // non-alphanumeric
                curGroup = NONALPHANUMERIC;
            }

            if (prevGroup != curGroup) {
                if (prevGroup != NONE) {
                    strs.add(group);
                    group = new String();
                }
            }
            if (curGroup != NONE) {
                group = group.concat(String.valueOf(chars[i]));
            }
            prevGroup = curGroup;
        }

        if (group.length() > 0) {
            strs.add(group);
        }

        return strs.toArray(new String[0]);
    }
    
    public static final void main(String [] args) {
        //test1: latlon format
        String [] latlons = {
                "0000N 00000W",
                "0000S 00000E",
                "9000S 00000W",
                "9000N 00000W",
                "0000N 09000W",
                "9000S 09000W",
                "9000N 09000W",

                "0000N 09000W",
                "4500S 09000W",
                "9000N 09000W",

                "9000N 09959W",
                "0000N 10000W",

                "4500S 09000W",
                "9000N 09000W",

                "90N 18000E",  // no match
                "9000S 18000E",
                "9000N 18000W",
                "9000S 18000W",
                "90N 179W",
                "9000S 17959W",                           
        };
 
        for (int i=0; i<latlons.length; i++) {
        	String[] split = latlons[i].split(" ");
        	if (split != null && split.length ==2)
        		latlons[i] = split[0] + split[1];
        	System.out.println("latlons[i] " + latlons[i]);
        }
        
        Pattern p = Pattern.compile(LATLON_PTRN);
        
        for(String s : latlons) {
            Matcher m = p.matcher(s);
            if(m.find()) {
                BasePoint b = parseLatLon(m.group());
                if(b != null) {
                    System.out.println(String.format("%16s %10.6f %11.6f",s, b.getLatitude(),b.getLongitude()));
                } else {
                    System.out.println("Invalid parse " + s);
                }
            } else {
                System.out.println("no match for " + s);
            }
        }

        //test2 replace "[\r\n]", " "
        String str = "123 123  123 \r SCT";        
        str = str.replaceAll("[\r\n]", " ");
        str = str.replaceAll(" {2,}", " ");
        System.out.println("[" + str + "]");
        
        //test3: parse SK. BKN pased in first regex
        String inputStr = "BKN-SCT046-TOPUNKN/OVC089/ SKC";
        String[] string = inputStr.split("/");
    	for (String mystr :string) {
    		
    		mystr.trim(); 
       
        String regex = "([A-Z]{3}|UNKN?)(?:-([A-Z]{3}))?";
        
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(mystr);
        
       if (matcher.find()) {  
        	String cloud_1 = matcher.group(1);
        	String cloud_2 = matcher.group(2);
        	System.out.println("FFFF "+cloud_1+ " "+cloud_2);
     
        }
           
        regex = "([0-9]{3}|UNKN?)(?:-TOP([0-9]{3}|UNKN?))?";
        pattern = Pattern.compile(regex);
        matcher = pattern.matcher(mystr);
        
        if (matcher.find()) {
        	
        	String baseHeight = matcher.group(1);
        	String topHeight = matcher.group(2);
        	System.out.println("DDDD "+baseHeight+" "+topHeight);
        	
        }	
    	}
        
    	//test4: TB
        String str1 = "SEV/LGT/MOD"; //"INTMT MOD-SEV ABV 014"; //"TB MOD-SEV CHOP 220/NEG BLO 095";
        //String strIc = "MOD RIME BLO 095";
        String[] str2 = str1.split("/");
        
        for(String strTmp : str2) {
        	PirepTools tools = new PirepTools(strTmp);
        	tools.decodeTurbulenceData();
        }
//        	if (str.indexOf(NEG_ENTRY) > 0) {
//                AircraftFlightCondition at = new AircraftFlightCondition();
//
//                // NEG should be the only value! Used to indicate forecasted but not observed!
//                at.setIntensity1(NEG_ENTRY);
////                if (flightLevel != null) {
////                    at.setBaseHeight(flightLevel.getFlightLevel());
////                    theTurbulenceLayers.add(at);
////                }
//            } else {
//                pattern = Pattern.compile(TRB_REGEX);
//                // Get a Matcher based on the target string.
//                matcher = pattern.matcher(str);
//                // add each turbulence
//                
//                // decodeFlightLevelData()
//                AircraftFlightLevel flightLevel = null;
//                String regex1 = "([0-9]{3})(?:-([0-9]{3}))?";
//                Pattern pattern1 = Pattern.compile(regex1);
//
//                // Get a Matcher based on the target string.
//                Matcher matcher1 = pattern1.matcher(str);
//                if (matcher1.find()) {
//                    Integer fltLevel = decodeHeight(matcher1.group(1));
//
//                    if (matcher1.group(2) != null) {
//                        Integer upperLevel = decodeHeight(matcher1.group(2));
//
//                        fltLevel = (fltLevel + upperLevel) / 2;
//                    }
//                    flightLevel = new AircraftFlightLevel(fltLevel);
//                }
//                // end decodeFlightLevelData()
//                while (matcher.find()) {
//                    //addFlightCondition(matcher, theTurbulenceLayers);
//                	for (int i=0; i<matcher.groupCount(); i++)
//                		System.out.print(matcher.group(i)+" ");
//                	if(matcher.groupCount() >= 13) {
//                        String s1 = matcher.group(1);
//                        String s2 = matcher.group(3);
//                        // Some words that may show up in group 1 or 3 that need to be
//                        // thrown away!
//                        s1 = FL_COND_WORDS.get(s1);
//                        //if (s1.equalsIgnoreCase(cont, int,ocal))
//                        if((s1 == null) || (NULL_ENTRY.equals(s1))) {
//                            return;
//                        }
//                        // Need to allow s2 to be null,
//                        if(s2 != null) {
//                            // but not null after lookup!
//                            s2 = FL_COND_WORDS.get(s2);
//                            if((s2 == null) || (NULL_ENTRY.equals(s2))) {
//                                return;
//                            }
//                        }
//                        //******************************************************************
//                        
//                        AircraftFlightCondition at = new AircraftFlightCondition();
//
//                        // NEG should be the only value! Used to indicate forecasted but
//                        // not observed!
//                        if(NEG_ENTRY.equals(s1)) {
//                            at.setIntensity1(s1);
//                            if(flightLevel != null) {
//                                at.setBaseHeight(flightLevel.getFlightLevel());
//                            }
//                        } else {
//                            at.setIntensity1(s1);
//                            at.setIntensity2(s2);
//                            
//                            s1 = matcher.group(5);
//                            s1 = COND_TYPES.get(s1);
//                            if((s1 != null) && (!s1.equals(NULL_ENTRY))) {
//                                at.setType(s1);
//                            }
//
//                            s1 = matcher.group(8);
//                            s2 = matcher.group(9);
//                            if(BLO_HGT.equals(s1)) {
//                                at.setBaseHeight(IDecoderConstantsN.UAIR_INTEGER_MISSING);
//                                at.setTopHeight(decodeHeight(s2));
//                                
//                            } else if (ABV_HGT.equals(s1)) {
//                                at.setBaseHeight(decodeHeight(s2));
//                                at.setTopHeight(IDecoderConstantsN.UAIR_INTEGER_MISSING);
//                            } else {
//                                // Check for one or more levels
//                                s1 = matcher.group(11);
//                                s2 = matcher.group(13);
//                                if(s1 != null) {
//                                    at.setBaseHeight(decodeHeight(s1));
//                                }
//                                if(s2 != null) {
//                                    at.setTopHeight(decodeHeight(s2));
//                                }
//                                if((s1 != null)&&(s2 != null)) {
//                                    Integer base = at.getBaseHeight();
//                                    Integer top = at.getTopHeight();
//                                    if(base != IDecoderConstantsN.UAIR_INTEGER_MISSING) {
//                                        if(top != IDecoderConstantsN.UAIR_INTEGER_MISSING) {
//                                            if(base > top) {
//                                               // logger.debug(traceId + "- BASE-TOP inversion fixed");
//                                                at.setBaseHeight(top);
//                                                at.setTopHeight(base);
//                                            }
//                                        }
//                                    }
//                                }
//                                if((s1 == null)&&(s2 == null)) {
//                                    // Use the flight level if heights are not specified.
//                                    if(flightLevel != null) {
//                                        at.setBaseHeight(flightLevel.getFlightLevel());
//                                    }
//                                }
//                            }
//                        }
//                       // layers.add(at);
//                    }
//                	//System.out.println(theTurbulenceLayers);
//                }
//            }
//        }
        		
//        String report = "BIG UA /OV BIG095040/TM 2343/FL090/TP SR22/TA M05/IC LGT RIME 090-100/RM CWSU ZAN=";
//        String traceId = "1";
//        new PirepParser(report, traceId);
    }
    
}