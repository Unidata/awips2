/**
 * This software was modified from Raytheon's airep plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.ncairep.decoder;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.decodertools.aircraft.AircraftFlightLevel;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftLatitude;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftLongitude;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftRemarks;
import com.raytheon.uf.edex.decodertools.core.BasePoint;
import com.raytheon.uf.edex.decodertools.core.PlatformLocationProxy;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

/**
 * The NcAirepParser takes a String that should contain a single AIREP observation
 * and parses the individual elements of the observation, and performs a decode
 * of those elements. The data is made available to clients through a set of get
 * methods for each data item.
 * <pre>
 * SOFTWARE HISTORY
 * Date           PTR#     Engineer      Description
 * ------------   -------- ------------- -------------------------------------
 * 04/27/2011              F.J.Yen       Initial creation from airep.  Change
 * 										 temperature and windSpeed from Double to
 * 										 Float; windDirection from Integer to Float.
 * 09/20/2011    286       Q.Zhou        Modified splitLatLon() to solve 3606S15942E. 
 * 									     Modified decodeTime()
 *									     Added 8 new fields and functions. 
 *									     Added decodeCloud()
 *										 Changed reportType to string, in parseElements
 *										 Added suspectTimeFlag field.
 *										 get reportType and data for ether airep or amdar.
 * 09/29/2011    286       Q.Zhou        Modified decodeTempreture to work for both amdar and airep.
 * 10/04/2011    286       Q.Zhou        Removed wmoHeader parameter. Modified splitLatLon() to solve 3606S 15942E
 * 10/17/2011    286       Q.Zhou        Added WX_COND_WORDS and checking.
 * 11/01/2011    286       Q.Zhou        Added month and year to decodetime
 * </pre>
 */
public class NcAirepParser
{
    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    private static final boolean WANT_DELIMITERS = false;
    // Only going to delimit aireps with spaces and carriage control.
    private static final String DELIMITER = " ;$=\r\n";
    
    private static final int MAX_WIND_SPEED = 500;
    
    // Once set the obs data cannot be changed!
    private final String reportData;
    
    private static final float RMISSD = IDecoderConstantsN.UAIR_FLOAT_MISSING;
    private static final int IMISSD = IDecoderConstantsN.INTEGER_MISSING;
    
    private static final Map<String,String> WX_COND_WORDS = new HashMap<String,String>();
    static {
        WX_COND_WORDS.put("NEG", "NEG");
        WX_COND_WORDS.put("NONE", "NONE");
        WX_COND_WORDS.put("NIL", "NONE");
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

 	//
 	// Determine if the input data corresponds to a hour minute time
 	//
 	//  i.e. 0000 to 2359
 	//
 	//   "^([0..1]{1}[0..9]{1})|(2[0..3]{1})[0..5]{1}[0..9]{1}"    
 	//         
 	//  Start of token "^"
 	//    0 or 1 followed by 0 to 9 or
 	//    2      followed by 0 to 3
 	//
 	//    0 to 5 followed by 0 to 9
 	//  End of token   "$"
 	//
    final Pattern TIME_Airep = Pattern.compile("[0-9]{4}");
    final Pattern TIME_Amdar = Pattern.compile("[0-9]{6}");
 	    //Pattern.compile("^(([0-1]{1}[0-9]{1})|(2[0-3]{1}))([0-5]{1}[0-9]{1})$");
    	
    final Pattern WX_GROUP = Pattern.compile("^[0-9/]{3}$");
    // Two different flight level patterns
    final Pattern FL_SHORT = Pattern.compile("F\\d{3}");
    final Pattern FL_LONG = Pattern.compile("FL\\d{3}");

    final Pattern TEMP_SHORT = Pattern.compile("^(M|P)\\d{2}");
    final Pattern TEMP_LONG = Pattern.compile("^(MS|PS)\\d{2}");
    final Pattern TEMP_SHORT2 = Pattern.compile("^(M|P)\\d{3}");
    final Pattern TEMP_LONG2 = Pattern.compile("^(MS|PS)\\d{3}");
    
    // Parsed and/or decoded observation elements.
    private ArrayList<Object> theElements = new ArrayList<Object>();
    
    private String aircraftId = null;
    private String reportType = null;
    private Double latitude = null;
    private Double longitude = null;
    private Calendar observationTime = null;
    private AircraftFlightLevel flightLevel = null;
    private Float temperature = RMISSD;
    private NcAIREPWeather weatherGroup = null;
    private Float windDirection = RMISSD;
    private Float windSpeed = RMISSD;
    private AircraftRemarks rptRemarks = null;
    
    private String turbInten;
    private String turbType;
    private String turbFreq;
    private String iceInten;
    private String iceType;
    private String skyCover;
    private int skyBaseHeight;
    private int skyTopHeight;
    private String suspectTimeFlag;
    private String headerTime;
    
    /**
     * Create the parser for and decode an observation from a String.  
     * @param anObservation A string containing the observation.
     */
    public NcAirepParser(String anObservation) {
    	reportData = anObservation;
        parseElements();
    } 
    
    /**
     * Create the parser for and decode an observation from a String.  
     * @param anObservation A string containing the observation.
     */
    public NcAirepParser(byte [] anObservation) {
        reportData = new String(anObservation);
        parseElements();
    } 
    
    /**
     * Expose the internal parsed elements for testing.
     * @return A collection of the parsed elements.
     */
    ArrayList<?> parseElementsTestPoint() {
        ArrayList<Object> retElements = new ArrayList<Object>();
        retElements.addAll(theElements);
        return retElements;
    } 
    
    /**
     * Parse the AIREP observation into individual elements.
     * Note that during parse or decode the order of the elements does not
     * change. The only exception to this rule is that all elements identified
     * as comments/remarks are placed at the end of the observation elements.
     */
    private void parseElements() {
    	
        StringTokenizer st =
            new StringTokenizer(reportData,DELIMITER,WANT_DELIMITERS);
        
        // parse first record in st --headerTime
        if (st.hasMoreTokens())
        	headerTime = st.nextToken().trim();
        
        // parse second record in st -- AMDAR or ARP
        // if it is AMDAR, remove the third st ( DES|ASC|LVR)
        String temp = "";
        if (st.hasMoreTokens()) {// && reportData.startsWith("AMDAR"))
        	temp = st.nextToken();
        	if (temp.equalsIgnoreCase("AMDAR") && st.hasMoreTokens())
        		st.nextToken();
        }
        
        // Now get the elements
        while(st.hasMoreTokens()) {       	
            String s = st.nextToken();
            
            if (s!= null )              	          	
            	theElements.add(s);
            //System.out.println("**********theElements "+s);
            
//            if(!DELIMITER.equals(s)) {
//                Object o = NcAIREPObsType.obsTypeFactory(s);
//                if((o != null)&&(reportType == null)) {
//                    reportType = ((NcAIREPObsType) o).getValue();
//                } else {
//                    theElements.add(s);
//                }
//            }
        }
        
        decodeReportType();
        decodeHazard();
        decodeMID();
        splitLatLon();
        decodeLatitude();
        decodeLongitude();
        decodeTime();
        
        // By now we should have found lat lon information. If not then
        // run back through the data to see if there is a waypoint. If we
        // get to the time information, quit, it's not there.
        if((latitude == null)&&(longitude == null))
        {
            for(int i = 0;i < theElements.size();i++)
            {
                Object o = theElements.get(i);
                if(o instanceof String)
                {
                    BasePoint wayPoint = PlatformLocationProxy.lookup((String)o,null); 
        			// found a waypoint
        			if(wayPoint != null)
        			{
        				latitude = AircraftLatitude.createLatitude(wayPoint);
        				longitude = AircraftLongitude.createLongitude(wayPoint);
        				
        				theElements.set(i,latitude);
        				theElements.add(i+1,longitude);       				
        				break;
        			}
                }
                else if(o instanceof Calendar)
                {
                    break;
                }
            }
        }
        // Need to have lat / lon data
        if((latitude == null)||(longitude == null)) {
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
    
    /*
     * get ReportType
     */
    private void decodeReportType()
    {
    	if (reportData.contains("ARP"))
    		reportType = "AIREP"; 
    	else if (reportData.contains("AMDAR")) 
    		reportType = "AMDAR"; 
    }
    /**
     * When the primary decode is complete the aircraft ID should be the only
     * data left prior to the latitude/longitude data. If found then set this
     * data as the id.
     */
    private void determineAircraftId()
    {
        for(int i = 0;i < theElements.size();i++)
        {
            Object o = theElements.get(i);
            if(o instanceof Double)
            {
                break;
            }
            if(o instanceof String)
            {
                aircraftId = (String) o;
                break;
            }
        }
    } // determineAircraftId()
    
    /**
     * Determine if a latitude/longitude group is run-together or may be a
     * navigation waypoint. As an example
     * <pre><code>5802N02015W</code></pre>
     * is split into 2 groups
     * <pre><code>5802N 02015W</code></pre>
     * which is then processed normally. 
     */
    private void splitLatLon()
    {
    	int latlonCnt =0;
        for(int i = 0;i < theElements.size();i++)
        {
            Object o = theElements.get(i);
            if(o !=null && o instanceof String)
            {
            	String s = theElements.get(i).toString().trim(); //e.g. UAE412 3606S 15942E 1150 F410 MS47 281/75=
            	
            	if ( !s.equalsIgnoreCase("")) {
            		String[] split =s.split("[A-Z]"); //this can get 3606S, 15942E, 1150, 281/75, so need return. 
            		
            		if (split.length != 0) {
            		
            			int splitPos = split[0].length();
            			
            			if (splitPos != 0 ){				// not start with a letter
            				if (splitPos < s.length()-1) {  //s is like 3606S15942E, no space
            					String s11 = s.substring( 0, splitPos +1 );
            					String s22 = s.substring( splitPos +1 );
            			
            					theElements.set(i, s11);
            					theElements.add(i+1, s22);
            					return;
            				}
            				else {
            					theElements.set(i, s);
            					latlonCnt++;
            					
            					if (latlonCnt >1)
            						return;
            				}
            			}  
            		}
            	}
            }
        } // for
        
    } // splitLatLon()
    
    /**
     * Attempt to locate and decode the latitude information within this
     * AIREP observation. The decode object replaces the string data within
     * the observation elements collection.
     */
    private void decodeLatitude()
    {
        if(latitude == null)
        {
            for(int i = 0;i < theElements.size();i++)
            {
                Object o = theElements.get(i);
                if(o instanceof String)
                {
                    AircraftLatitude lat = AircraftLatitude.aircraftLatitudeFactory((String) o);
                    if(lat != null)
                    {
                        theElements.set(i,lat);
                        latitude = lat.decodeLatitude();
                        break;
                    }
                }
            }
        }
    } // decodeLatitude()

    /**
     * Attempt to locate and decode the longitude information within this
     * AIREP observation. The decode object replaces the string data within
     * the observation elements collection.
     */
    private void decodeLongitude()
    {
        if(longitude == null)
        {
            for(int i = 0;i < theElements.size();i++)
            {
                Object o = theElements.get(i);
                if(o instanceof String)
                {
                    AircraftLongitude lon = AircraftLongitude.aircraftLongitudeFactory((String) o);
                    if(lon != null)
                    {
                        theElements.set(i,lon);
                        longitude = lon.decodeLongitude();
                        break;
                    }
                }
            }
        }
    } // decodeLongitude()
    
    
    /**
     * Attempt to locate and decode the time information within this
     * AIREP observation. The decode object replaces the string data within
     * the observation elements collection.
     */
    private void decodeTime()
    {
    	Calendar oTime = TimeTools.getSystemCalendar();
    	
    	Calendar issuTime = TimeTools.copy(oTime);
    	
    	int day = Integer.parseInt(headerTime.substring(0,2));
        int hour = Integer.parseInt(headerTime.substring(2,4));
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
        	
    	issuTime.set(Calendar.DAY_OF_MONTH, day);
    	issuTime.set(Calendar.HOUR_OF_DAY,hour);
    	issuTime.set(Calendar.MINUTE,minute);
    	issuTime.set(Calendar.SECOND,0);
    	issuTime.set(Calendar.MILLISECOND,0);
    	
        for(int i = 1; i < theElements.size();i++) //start from second record
        {
            Object o = theElements.get(i);
            if(o instanceof String)
            {
                String s = (String) o;
                if(TIME_Airep.matcher(s).matches())
                {                      		   
                    hour = Integer.parseInt(s.substring(0, 2));
                    minute = Integer.parseInt(s.substring(2, 4));
                    	
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
                    if(observationTime.compareTo(oTime) > 0) 
                            observationTime.add(Calendar.DAY_OF_MONTH,-1);                                            	
                }
                else if (TIME_Amdar.matcher(s).matches()) {
                    day = Integer.parseInt(s.substring(0, 2));
                    hour = Integer.parseInt(s.substring(2, 4));
                    minute = Integer.parseInt(s.substring(4, 6));
                    
                    observationTime = TimeTools.copy(oTime); 
                    observationTime.set(Calendar.YEAR, year);
                    observationTime.set(Calendar.MONTH, month);
                    observationTime.set(Calendar.DAY_OF_MONTH, day);
                    observationTime.set(Calendar.HOUR_OF_DAY,hour);
                    observationTime.set(Calendar.MINUTE,minute);
                    observationTime.set(Calendar.SECOND,0);
                    observationTime.set(Calendar.MILLISECOND,0);
                    	
                    if(observationTime.compareTo(oTime) > 0) 
                            observationTime.add(Calendar.DAY_OF_MONTH,-1);                        
                }
                //System.out.println("***********time2 "+observationTime.getTime());    	
                
                theElements.set(i,observationTime);
                break;
                
            }
        }
    }
 
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
    
    private void decodeFlightLevel() {
        for(int i = 0;i < theElements.size();i++) {
            Object o = theElements.get(i);
            if(o instanceof String) {
                String s = (String) o;
                if(FL_SHORT.matcher(s).matches()) {
                    double fLevel = Integer.parseInt(s.substring(1))*100;
                    
                    flightLevel = new AircraftFlightLevel(fLevel);
                    theElements.set(i,flightLevel);
                    break;
                } else if(FL_LONG.matcher(s).matches()) {
                    double fLevel = Integer.parseInt(s.substring(1))*100;
                    
                    flightLevel = new AircraftFlightLevel(fLevel);
                    theElements.set(i,flightLevel);
                    break;
                }
            }
        }
    }
    
    /**
     * Decode the temperature information in this observation.
     * e.g. Airep MS45, AMDAR MS456
     */
    private void decodeTemperature()
    {
        for(int i = 0;i < theElements.size();i++)
        {
            Object o = theElements.get(i);
            if(o instanceof String)
            {
            	float temp = RMISSD;
            	temperature = RMISSD;
                String s = (String) o;
                
                if(TEMP_LONG.matcher(s).matches() ||TEMP_LONG2.matcher(s).matches())
                {
                    String ss = s.substring(2);
                    
                    if(!"//".equals(ss))
                    {
                    	if (ss.length()>2)
                        	ss = ss.substring(0, 2) + "." + ss.substring(2);
                    	
                        temp = Float.parseFloat(ss);
                        
                        if("MS".equals(s.substring(0,2)))
                        {
                            temp *= -1;
                        }
                        temperature = new Float(temp);
                        theElements.set(i,temperature);
                    }                    
                    break;
                }
                else if(TEMP_SHORT.matcher(s).matches() ||TEMP_SHORT2.matcher(s).matches())
                {
                    String ss = s.substring(1);
                    if(!"//".equals(ss))
                    {
                    	if (ss.length()>2)
                        	ss = ss.substring(0, 2) + "." + ss.substring(2);
                    	
                        temp = Float.parseFloat(ss);

                        if("M".equals(s.substring(0,1)))
                        {
                            temp *= -1;
                        }
                        temperature = new Float(temp);
                        theElements.set(i,temperature);                    
                    }
                    break;
                }
            }
        }
    }
    
    /**
     * Attempt to locate and decode the 3 digit hazards and weather group.
     */
    private void decodeWeatherGroup()
    {
        for(int i = 0;i < theElements.size();i++)
        {
            Object o = theElements.get(i);
            if(o instanceof String)
            {
                String s = (String) o;
                if(s.length() == 3)
                {
                    if(WX_GROUP.matcher(s).find())
                    {
                        weatherGroup = new NcAIREPWeather(s);
                        theElements.set(i,weatherGroup);
                        break;
                    }
                }
            }
        }
    } // decodeWeatherGroup()
    
    /**
     * Decode the wind data group in the following forms.
     * 16080
     * 160080
     * 163/080
     * 163/080KT
     * @throws DecodeException if winds are bad
     */
    private void decodeWinds() {
    	
        // By now we should have found the flight level data.
        int i = 0;
        for(;i < theElements.size();i++) {
            if(theElements.get(i) instanceof AircraftFlightLevel) {
                i++;
                break;
            }
        } // for()
 
        
        for(;i < theElements.size();i++) {
            Object o = theElements.get(i);
            windSpeed = RMISSD;
            windDirection = RMISSD;
            if(o instanceof String) {
                String s = (String) o;
                if(s != null) {
                    if(s.startsWith("M") || s.startsWith("P") || s.startsWith("/")) {
                        // These are temperatures.  Some temps are
                        // being reported as 5 digits (MS513 which is -51.3)
                        continue;
                    } else if(s.endsWith("KT")) {
                        s = s.substring(0,s.length() - 2);
                    } else if(s.endsWith("KTS")) {
                        s = s.substring(0,s.length() - 3);
                    }
                    int solidusPos = s.indexOf("/");
                    
                    String windDir = null;

                    String windSpd = null;
                    if(solidusPos > 0) {
                        windDir = s.substring(0,solidusPos);
                        windSpd = s.substring(solidusPos+1);
                    } else if(s.length() == 5) {
                        windDir = s.substring(0,2) + "0";
                        windSpd = s.substring(2);
                    } else if(s.length() == 6) {
                        windDir = s.substring(0,3);
                        windSpd = s.substring(3);
                    }
                    if((windSpd != null)&&(windDir != null)) {
                        try {
                            float value = Float.parseFloat(windSpd);
                            if((value >= 0)&&(value < MAX_WIND_SPEED)) {
                                windSpeed = value;
                            } else {                              
                                windSpeed = RMISSD;
                            }

                            
                            value = Float.parseFloat(windDir);
                            // Database constraint is 1 - 360.
                            if (value == 0)
                            {
                                value = 360;
                            }
                            windDirection = value; // windDirection.fromDegree(value);
                            // Database constraint is 1 - 360.
                            
                            theElements.set(i,windDirection);
                            theElements.add(i+1,windSpeed);
                        }
                        catch(Exception nothing) {
                            String msg = String.format("Error decoding winds: [%s] [%s]",windSpd,windDir);
                            
                            logger.info(msg);
                        }
                        break;
                    }
                    if (windDir ==null) {
                    	windDirection = RMISSD;
                    	theElements.set(i,windDirection);
                    }
                    if (windSpeed ==null) {
                    	windSpeed = RMISSD;
                    	theElements.set(i,windSpeed);
                    }
                }
            }
            
            if (windDirection ==null) {
            	windDirection = RMISSD;
            	theElements.set(i,windDirection);
            }
        }
    }
    
    /* Observed hazard in TB, SK, IC order in Airep.  Observed appear once at most for each. 
     * e.g. airep: ARP ASA858 3948N 13807W 2354 F370 MS53 263/046KT TB SMTH=
     *      amdar: AMDAR LVR AU0144 2903S 15252E 202133 F350 MS440 193/081 TB3 S031
     * 
     */
    private void decodeHazard() {
    	
        int iTb = 0; //index of TB contents in airep, not in amdar
        int iIc = 0; //index of IC contents in airep
        int iSk = 0; //index of SK contents in airep
        int size = theElements.size();
        
        for(int i=0; i < size; i++) {
        	String elem = theElements.get(i).toString().trim();
        	
            if(elem.startsWith("TB") ) {
            	if (elem.length() ==3) //Amdar TB1
            		turbInten = findTurbAmdar(elem);
            	else	
            		iTb = i+1;
            }
            
            if(elem.startsWith("IC") ) {
            	if (elem.length() ==3) //Amdar
            		iceInten = findTurbAmdar(elem);
            	else
            		iIc = i+1;              
            }
            
            if(elem.startsWith("SK") ) {
            	iSk = i+1;
            }
        } // for()
 
        // Airep: Only observed that hazards in the TB, SK, IC order
        // TB format: a number or CONT|OCTL intensity CHOP|CAT
        if (iTb !=0 && iTb < size ) { //iTb !=0 eliminate header
        	if ( !theElements.get(iTb).toString().equalsIgnoreCase("SK")
        			&& !theElements.get(iTb).toString().equalsIgnoreCase("IC")
        			&& !theElements.get(iTb).toString().equalsIgnoreCase("RM")){
        		
        		String temp = theElements.get(iTb).toString();
        		if (WX_COND_WORDS.get(temp) != null)
        			temp = WX_COND_WORDS.get(temp);
        			
        		if (temp.startsWith("CO") || temp.startsWith("OC") || temp.startsWith("INT")) //INTMT
        			turbFreq = temp;
        		else if (temp.startsWith("CHO") || temp.startsWith("CA"))
        			turbType = temp;
        		else
        			turbInten = temp;
        		
        		iTb++;        		
        	}
        }
        // second elem in TB
        if (iTb !=0 && iTb < size ) {
        	if ( !theElements.get(iTb).toString().equalsIgnoreCase("SK")
        			&& !theElements.get(iTb).toString().equalsIgnoreCase("IC")
        			&& !theElements.get(iTb).toString().equalsIgnoreCase("RM")){
        		
        		String temp = theElements.get(iTb).toString();   
        		if (WX_COND_WORDS.get(temp) != null)
        			temp = WX_COND_WORDS.get(temp);
        		
        		if (temp.startsWith("CHO") || temp.startsWith("CA"))
        			turbType = temp;
        		else
        			turbInten = temp;
        		
        		iTb++;        		
        	}
        }
        // third elem in TB
        if (iTb !=0 && iTb < size ) {
        	if ( !theElements.get(iTb).toString().equalsIgnoreCase("SK")
        			&& !theElements.get(iTb).toString().equalsIgnoreCase("IC")
        			&& !theElements.get(iTb).toString().equalsIgnoreCase("RM")){
        		
        		String temp = theElements.get(iTb).toString();
        		if (WX_COND_WORDS.get(temp) != null)
        			temp = WX_COND_WORDS.get(temp);
        		
        		if (temp.startsWith("CHOP") || temp.startsWith("CAT"))
        			turbType = temp;
        	}
        }
        	
        // Airep: handle turbInten number value
        if (iTb !=0 && turbInten != null )
        	turbInten = checkTurbNumAirep(turbInten);	  
        
        // Airep: handle turbInten with "-". e.g. LGT-MOD => LGTMOD
        if (iTb !=0 && turbInten != null) {
        	String[] s = turbInten.split("-");
        	if (s.length ==2)
        		turbInten = s[0] + s[1];
        }
        	
        // Airep: IC format: a number or intensity (MX|CLR|RIME, didn't see)
        if (iIc !=0 && iIc < size) {
        	
        	if ( !theElements.get(iIc).toString().equalsIgnoreCase("RM") 
            		&& !theElements.get(iIc).toString().equalsIgnoreCase("SK")) { //didn't see, in case...
        		
        		iceInten = theElements.get(iIc).toString();
        		if (WX_COND_WORDS.get(iceInten) != null)
        			iceInten = WX_COND_WORDS.get(iceInten);
        		// handle iceInten number value
        		iceInten = checkIceNumAirep(iceInten);
        	}
        }      	
        
        // Airep: handle iceInten with "-". e.g. LGT-MOD => LGTMOD
        if (iIc !=0 ) {
        	String[] s = iceInten.split("-");
        	if (s.length ==2)
        		iceInten = s[0] + s[1];
        }
        
        // SK format: CLEAR,CLR, RM
        if (iSk !=0 && iSk < size) {
        	if ( !theElements.get(iSk).toString().equalsIgnoreCase("RM")
        		&& !theElements.get(iSk).toString().equalsIgnoreCase("IC")) {
        		
        		skyCover = theElements.get(iSk).toString();
        		if (WX_COND_WORDS.get(skyCover) != null)
        			skyCover = WX_COND_WORDS.get(skyCover);
        	}
        }

    }

    /**
     * The "MID" section only occurs in the AIREP remarks section. So if we find
     * a "MID" token we create a remarks object using all data from the MID token
     * to the end of the data.
     */
    private void decodeMID()
    {
        for(int i = 0;i < theElements.size();i++)
        {
            Object o = theElements.get(i);
            if(o instanceof String)
            {
                String s = (String) o;
                if("MID".equals(s))
                {
                    AircraftRemarks remarks = new AircraftRemarks(s);
                    for(i++;i < theElements.size();)
                    {
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
     * Iterate over any remaining data left in the observation that is AFTER
     * the time information. These data are bundled together as a remarks
     * string that will be appended to the end of the observation data.
     */
    private void collectRemarks() {
        boolean timeFound = false;
        int i = 0;
        for(;i < theElements.size();i++) {
            Object o = theElements.get(i);
            if(timeFound = (o instanceof Calendar)) {
                break;
            }
        } // for
        if(timeFound) {
            StringBuffer remarksBuffer = new StringBuffer();
            for(;i < theElements.size();i++) {
                Object o = theElements.get(i);
                if(o instanceof String) {
                    theElements.remove(i);
                    i--;
                    remarksBuffer.append(o);
                    remarksBuffer.append(" ");
                }
            } // for
            if(remarksBuffer.length() > 0) {
                if(rptRemarks != null) {
                    remarksBuffer.insert(0," ");
                    remarksBuffer.insert(0,rptRemarks.getValue());
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
    public String getReportType() {
        return reportType;
    }

    /**
     * @param reportType the reportType to set
     */
    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    /**
     * Get the decoded Aircraft id.
     * @return The decoded Aircraft id.
     */
    public String getAircraftId()
    {
        return aircraftId;
    } // getAircraftId()
    
    /**
     * Get the decoded Latitude instance.
     * @return The decoded Latitude
     */
    public Double getLatitude()
    {
        return latitude;
    } // getLatitude()
    
    /**
     * Get the decoded Longitude instance.
     * @return The decoded Longitude.
     */
    public Double getLongitude()
    {
        return longitude;
    } // getLongitude()
    
    /**
     * Get the AIREP observation time.
     * @return The AIREP observation time.
     */
    public Calendar getObservationTime()
    {
        return observationTime;
    } // getObservationTime()
    
    /**
     * Get the decoded aircraft flight level data.
     * @return The decoded aircraft flight level data.
     */
    public Integer getFlightLevel() {
//        Integer retValue = null;
        Integer retValue = IMISSD;
        if(flightLevel != null) {
            retValue = flightLevel.getFlightLevel();
        }
        return retValue;
    } // getFlightLevel()
    
    /**
     * Get the decoded temperature data.
     * @return The decoded temperature.
     */
    public Float getTemperature()
    {
        return temperature;
    } // getAirTemperature()
    
    public NcAIREPWeather getWeatherGroup()
    {
        return weatherGroup;
    } // getWeatherGroup()
    
    /**
     * Get the decoded wind direction data.
     * @return The decoded wind direction data.
     */
    public Float getWindDirection()
    {
        return windDirection;
    } // getWindDirection()
    
    /**
     * Get the decoded wind speed data.
     * @return The decoded wind speed data.
     */
    public Float getWindSpeed()
    {
        return windSpeed;
    } // getWindSpeed()

    public String getTurbInten() {
        return turbInten;
    }
    public String getTurbType() {
        return turbType;
    }
    public String getTurbFreq() {
        return turbFreq;
    }
    public String getIceInten() {
        return iceInten;
    }
    public String getIceType() {
        return iceType;
    }
    public String getSkyCover() {
        return skyCover;
    }
    public int getSkyBaseHeight() {
        return skyBaseHeight;
    }
    public int getSkyTopHeight() {
        return skyTopHeight;
    }
    public String getSuspectTimeFlag() {
        return suspectTimeFlag;
    }
    
    /**
     * Get any remarks information found.
     * @return The remarks information. An empty string is returned if no
     * remarks data was found.
     */
    public String getRemarks()
    {
        return (rptRemarks != null) ? rptRemarks.toString() : "";
    } // getRemarks()
    
    /*
     *  Amdar: match TB number with hazard intensity.
     */
    private String findTurbAmdar(String elem) {
    	String amdar = "";
    	if (elem.charAt(2) =='0')  //e.g. TB0
    		amdar = "NEG";
    	else if (elem.charAt(2) =='1')
    		amdar = "LGT";
    	else if (elem.charAt(2) =='2')
    		amdar = "MOD";
    	else if (elem.charAt(2) =='3')
    		amdar = "SEV";
    	else if (elem.charAt(2) =='/')
    		amdar = "";
    	
    	return amdar;
    }
    
    private String checkTurbNumAirep(String turb) {
    	String turbInten = "";
    	try {
    		int tb = Integer.parseInt(turb);
    		if (tb == 0)
    			turbInten = "NEG";
    		else if (tb == 1)
    			turbInten = "LGT";
    		else if (tb == 2)
    			turbInten = "MOD";
    		else if (tb == 3)
    			turbInten = "SEV";
    		else if (tb == 4)
    			turbInten = "EXTRM";
    		
    	} catch (NumberFormatException e) {
    		// is not a number, no change.
    		return turb;
    	} 
    	
    	return turbInten;
    }
    
    private String checkIceNumAirep(String ice) {
    	String iceInten = "";
    	try {
    		int tb = Integer.parseInt(ice);
    		if (tb == 0)
    			iceInten = "NEG";
    		else if (tb == 5)
    			iceInten = "LGT";
    		else if (tb == 6)
    			iceInten = "MOD";
    		else if (tb == 7)
    			iceInten = "SEV";
    		else if (tb == 8)
    			iceInten = "EXTRM";
    		
    	} catch (NumberFormatException e) {
    		// is not a number, no change.
    		return ice;
    	} 
    	
    	return iceInten;
    }
    
    //test
    public static void main(String [] args) {
    	String cloud = "test";
    	cloud = WX_COND_WORDS.get(cloud);
    	System.out.println("cloud "+cloud);
    			
    	ArrayList<Object> theElements = new ArrayList<Object>();
    	String reportData = "AMDAR LVR CNFNXL 3315N 11850E 190113 F226 MS123 278/038 TB/ S//1=";
    	//String s = "ARP COA1193 3358N 07218W 0209 F360 MS48 221/058KT TB SMTH SK CLEAR=";
    	String s2 = "AMDAR LVR CNFNXL 3315N 11850E 190113 F226 MS123 278/038 TB/ S//1=";
    	
    	// test1 theElements
    	StringTokenizer st =
            new StringTokenizer(reportData,DELIMITER,WANT_DELIMITERS);
        
        // parse first record in st --headerTime
    	String headerTim = "";
        if (st.hasMoreTokens())
        	headerTim = st.nextToken().trim();
        
        // parse second record in st -- AMDAR or ARP
        // if it is AMDAR, remove the third st
        String temp = "";
        if (st.hasMoreTokens()) {// && reportData.startsWith("AMDAR"))
        	temp = st.nextToken();
        	if (temp.equalsIgnoreCase("AMDAR") && st.hasMoreTokens())
        		st.nextToken();
        }
        
        // Now get the elements
        while(st.hasMoreTokens()) {       	
            String s = st.nextToken();
            
            if (s!= null )              	          	
            	theElements.add(s);
        }
        System.out.println(theElements);
        
        //test2 splitLatlon
        int latlonCnt =0;
        for(int i = 0;i < theElements.size();i++)
        {
            Object o = theElements.get(i);
            
            if(o !=null && o instanceof String)
            {
            	String s = theElements.get(i).toString().trim(); //e.g. UAE412 3606S 15942E 115 F410 MS47 281/75=
            	
            	if ( !s.equalsIgnoreCase("")) {
            		String[] split =s.split("[A-Z]");   
            		
            		if (split.length != 0) {
            		
            			int splitPos = split[0].length();
            			System.out.println("***********splitPos "+splitPos +" "+split[0]);
            			if (splitPos != 0 ){				// not start with a letter
            				if (splitPos < s.length()-1) {  //s is like 3606S15942E, no space
            					System.out.println("***********splitPos "+splitPos +" "+split[0]);
            					String s11 = s.substring( 0, splitPos +1 );
            					String s22 = s.substring( splitPos +1 );
            			
            					theElements.set(i, s11);
            					theElements.add(i+1, s22);
            					System.out.println("****latlon "+s11 +" "+s22);
            					return;
            				}
            				else {
            					
            					theElements.set(i, s);
            					latlonCnt++;
            					System.out.println("****lon "+s);
            					if (latlonCnt >1)
            						return;
            					
            				}
            			}  
            		}
            	}
            }
        }
        
        //test3 time
        for(int i = 0;i < theElements.size();i++)
        {
            Object o = theElements.get(i);
            
            if(o !=null && o instanceof String)
            {
            	String s = theElements.get(i).toString().trim(); //e.g. UAE412 3606S 15942E 115 F410 MS47 281/75=
            	
            	if(Pattern.compile("[0-9]{4}").matcher(s).matches())
                {      System.out.println("****Airep "+s);
            	}
            	else if(Pattern.compile("[0-9]{6}").matcher(s).matches())
                {      System.out.println("****Amdar "+s);
            	}
            }
        }

    }
} 