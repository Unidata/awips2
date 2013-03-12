/**
 * AwwParser
 * 
 * This java class intends to serve as an utility only for AWW decoder.
 * 
 * HISTORY
 *
 * * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 12/2008      38				L. Lin     	Initial coding
 * 07/2009		38			    L. Lin		Migration to TO11
 * 11/2009      38              L. Lin      Fix county fips.
 * 01/11/2011   N/A             M. Gao      Add mndTime as the 5th element to construct 
 *                                          dataUri value that is used as a unique constraint 
 *                                          when the aww record is inserted into relational DB
 *                                          The reason mndTime is used is because the combination 
 *                                          of original 4 elements is not unique in some scenarios.  
 *                                          The change is made in the method processWMO(...)  
 * 01/26/2011   N/A				M. Gao 		Refactor: 
 * 											change the WMO regular expression more flexible. 
 *                                    
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */


package gov.noaa.nws.ncep.edex.plugin.aww.util;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwHVtec;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwLatlons;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Scanner;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class AwwParser {
	
	private static Logger logger = Logger.getLogger(AwwParser.class.getName()); 
	
	/**
     * Constructor
     */
    public AwwParser() {
    }
    
    /**
	 * Parse the WMO line and store WMO header, OfficeID, issue time, designatorBBB,...
	 * 
	 * @param wmoline The bulletin message
	 * 
	 * @return a AwwRecord
	 */
	public static AwwRecord processWMO(String wmoline, Calendar mndTime) {
		AwwRecord record=null;
		 //Regular expression for WMO/ICAO, station ID, and issue date (and maybe designator BBB) 
//	    final String WMO_EXP = "([A-Z]{4}[0-9]{2}) ([A-Z]{4}) ([0-9]{6})( ([A-Z]{3}))?";
//	    final String WMO_EXP = "([A-Z]{4}[0-9]{2}) +([A-Z]{4}) +([0-9]{6})( +([A-Z]{3}))?";
//	    final String WMO_EXP = "([A-Z]{4}[0-9]{2}) +([A-Z]{4}) +([0-9]{6})( +[A-Z]{3})? *\\x0d\\x0d\\x0a( *[[A-Z]{6}|[A-Z]{3}[0-9]{1}|[A-Z]{4}[0-9]{2}|[A-Z]{5}[[0-9]{1}| ]])";
	    final String WMO_EXP = "([A-Z]{4}[0-9]{2}) +([A-Z]{4}) +([0-9]{6})( +[A-Z]{3})? *\\x0d\\x0d\\x0a *([A-Z]{6}|[A-Z]{3}[0-9]{1}|[A-Z]{4}[0-9]{2}|[A-Z]{5}[0-9]{1}|[A-Z]{5} *)?";
	
	    // Pattern used for extracting WMO header, officeID, product purge time, and issue date and designatorBBB
		final Pattern wmoPattern = Pattern.compile(WMO_EXP);
		Matcher theMatcher = wmoPattern.matcher( wmoline );

		if (theMatcher.find()) {
			record = new AwwRecord();

            String wmoHeader = theMatcher.group(1);
            if(wmoHeader == null)
            	wmoHeader = ""; 
            String issueOffice = theMatcher.group(2); 
            if(issueOffice == null)
            	issueOffice = ""; 
            String designatorBBB = theMatcher.group(5); 
            if(designatorBBB == null)
            	designatorBBB = ""; 
            
			record.setWmoHeader( wmoHeader );
			record.setIssueOffice( issueOffice );
			record.setDesignatorBBB( designatorBBB );
						
			// Decode the issue time.
			Calendar issueTime = null; 
			issueTime = UtilN.findDataTime(theMatcher.group(3), mndTime);
			record.setIssueTime(issueTime);

		    DataTime dataTime=new DataTime(issueTime);
			record.setDataTime(dataTime);
			
			/*
			 * set mndTime to record object, if mndTime is NULL, 
			 * set mndtimeString to an empty string
			 */
			String mndTimeString = ""; 
			if(mndTime != null) 
				TimeUtil.formatCalendar(mndTime); 
			record.setMndTime(mndTimeString); 
		} 
		return record;
	}
    
	public static String processSevereWeatherStatusEventTrackNumber(String wmoline) {
//		String severeWeatherStatusStringMatchPattern = "STATUS REPORT ON WW"; 
//	    final String SEVERE_WEATHER_STATUS_EXP = "STATUS REPORT ON WW ([0-9]{3})? *\\x0d\\x0d\\x0a"; 
	    final String SEVERE_WEATHER_STATUS_EXP = "STATUS REPORT ON WW ([0-9]{3})+\\x0d\\x0d\\x0a)"; 
//	    final String ATTN_EXP = "ATTN...([A-Z]{3}...)+\\x0d\\x0d\\x0a(([A-Z]{3}...)+)?";
		Pattern matchPattern = Pattern.compile(SEVERE_WEATHER_STATUS_EXP); 
		Matcher theMatcher = matchPattern.matcher(wmoline);
		String severeWeatherStatusEventTrackNumber = theMatcher.group(1); 
//		System.out.println("====, severeWeatherStatusEventTrackNumber=" + severeWeatherStatusEventTrackNumber); 
		return severeWeatherStatusEventTrackNumber; 
		
	}
	
	private static boolean isSevereWeatherStatusInfo(String stringInfo, String strPatternToBeSearched) {
		boolean isSereveWeatherStatus = false; 
		if(stringInfo.indexOf(strPatternToBeSearched) >= 0)
			isSereveWeatherStatus = true; 
		return isSereveWeatherStatus; 
	}
	
	private static String getSevereWeatherStatusEventTrackNumber(String wmoLine, String matchPatternString)  { 
		Pattern matchPattern = Pattern.compile(matchPatternString); 
		Matcher theMatcher = matchPattern.matcher(wmoLine);
		String severeWeatherStatusEventTrackNumber = theMatcher.group(1); 
//		System.out.println("====, severeWeatherStatusEventTrackNumber=" + severeWeatherStatusEventTrackNumber); 
		return severeWeatherStatusEventTrackNumber; 
	}
 	
	/**
	 * Get the event time with format "YYMMDDThhmm" into a Calendar object.
	 * The year is in 2 digits; so get the 4-digit year from system time.
	 * If the timeString equals "000000T0000", return a null Calendar.
	 * 
	 * @param timeString The event time with format "YYMMDDThhmm".
	 * @return a calendar for event time
	 */
	public static Calendar findEventTime(String timeString) {
		
		final String zeroTime = "000000T0000";
		Calendar timeGroup = null;
		
		if ( ! timeString.equals(zeroTime)) {
		
			timeGroup = Calendar.getInstance();
			
			// Get the year from system.
			int sysYear = (timeGroup.get(Calendar.YEAR)/100)*100;
			int year = Integer.parseInt(timeString.substring(0, 2).trim());
			int month = Integer.parseInt(timeString.substring(2, 4).trim());
			int day = Integer.parseInt(timeString.substring(4, 6).trim());
			int hour = Integer.parseInt(timeString.substring(7, 9).trim());
			int minute = Integer.parseInt(timeString.substring(9, 11).trim());
			
			timeGroup.set(Calendar.YEAR, year + sysYear);
			timeGroup.set(Calendar.MONTH, month - 1);
			timeGroup.set(Calendar.DAY_OF_MONTH, day);
			timeGroup.set(Calendar.HOUR_OF_DAY, hour);
			timeGroup.set(Calendar.MINUTE, minute);
			timeGroup.set(Calendar.SECOND, 0);
			timeGroup.set(Calendar.MILLISECOND, 0);
		}

		return timeGroup;
		
	}

	/**
	 * Parse the Attention line and store the attention WFOs (active WFOs).
	 * 
	 * @param bulletin The bulletin message
	 * @return a string for active WFOs.
	 */
	public static String processATTN(String bulletin) {
	
	    String attnToken=null;
        String collectAttn = null;
	    
	    ArrayList<String> attnList = new ArrayList<String>();
	    
	    // Regular expression for attention WFO 
	    final String ATTN_EXP = "ATTN...([A-Z]{3}...)+\\x0d\\x0d\\x0a(([A-Z]{3}...)+)?";
    
	    // Pattern used to extract ATTN line
		final Pattern attnPattern = Pattern.compile(ATTN_EXP);
	    
		Matcher attnMatcher = attnPattern.matcher( bulletin );

		if (attnMatcher.find()) {
	        String attentionLine = attnMatcher.group();
	        Scanner sc = new Scanner (attentionLine).useDelimiter("\\x2e\\x2e\\x2e|\\x0d\\x0d\\x0a");
	        
	        while (sc.hasNext()) {
	        	attnToken = sc.next();
	        	if (attnToken.length() == 3 && (attnToken.compareTo("WFO") != 0)) {
	        		attnList.add(attnToken);
	        	}
	        }
	        
	        for ( int idxAttn=0; idxAttn < attnList.size() ; idxAttn++) {

				if ( idxAttn == 0 ) {
					collectAttn = attnList.get(idxAttn);
				} else {
					collectAttn = collectAttn.concat(";").concat(attnList.get(idxAttn));
				}
			}
	        
			//record.setAttentionWFO( collectAttn );
			
		} 
		
		return collectAttn;
	}
	
	public static String processUgcToRetrieveWatchNumberForThunderstormOrTornado(String segment) {
		String watchNumber = ""; 
//		"WW\\s\\d{3}\\sTORNADO\\s([A-Z]{2}\\s)+CW\\s(\\d{6}Z)\\s\\-\\s(\\d{6}Z)"
		// Pattern used to extract Thunderstorm watch number for WATCH  
//		String THUNDERSTORM_WATCH_NUMBER_EXP = "WW\\s(\\d{3})\\sTHUNDERSTORM";		
		String THUNDERSTORM_WATCH_NUMBER_EXP = "WW\\s(\\d{1,4})\\sSEVERE\\sTHUNDERSTORM";		//T976 - replaced (\\d{3}) with (\\d{1,4}
		Pattern thunderstormWatchNumberPattern = Pattern.compile(THUNDERSTORM_WATCH_NUMBER_EXP);
		Matcher thunderstormWatchNumberMatcher = thunderstormWatchNumberPattern.matcher(segment);		

		String THUNDERSTORM_WATCH_NUMBER_EXP2 = "WW\\s(\\d{1,4})\\sSEVERE\\sTSTM";		//T976 - replaced (\\d{3}) with (\\d{1,4}
		Pattern thunderstormWatchNumberPattern2 = Pattern.compile(THUNDERSTORM_WATCH_NUMBER_EXP2);
		Matcher thunderstormWatchNumberMatcher2 = thunderstormWatchNumberPattern2.matcher(segment);		
		
		if(thunderstormWatchNumberMatcher.find()) {
			watchNumber = thunderstormWatchNumberMatcher.group(1).trim(); 
//			System.out.println("in processUgcToRetrieveWatchNumber - find THUNDERSTORM_WATCH_NUMBER using 'SEVERE THUNDERSTORM' key word=" + watchNumber);
		} else if(thunderstormWatchNumberMatcher2.find()) {
			watchNumber = thunderstormWatchNumberMatcher2.group(1).trim(); 
//			System.out.println("in processUgcToRetrieveWatchNumber - find THUNDERSTORM_WATCH_NUMBER using 'SEVERE TSTM' key word=" + watchNumber);
		} else {
			String TORNADO_WATCH_NUMBER_EXP = "WW\\s(\\d{1,4})\\sTORNADO";		//T976 - replaced (\\d{3}) with (\\d{1,4}
			Pattern tornadoWatchNumberPattern = Pattern.compile(TORNADO_WATCH_NUMBER_EXP);
			Matcher tornadoWatchNumberMatcher = tornadoWatchNumberPattern.matcher(segment);		
			if(tornadoWatchNumberMatcher.find()) {
				watchNumber = tornadoWatchNumberMatcher.group(1).trim(); 
//				System.out.println("in processUgcToRetrieveWatchNumber - find TORNADO_WATCH_NUMBER=" + watchNumber);
			}
		}

		return watchNumber; 
	}

	public static String processUgcToRetrieveWatchNumberForStatusReport(String segment) {
		String watchNumber = ""; 
		String STATUS_REPORT_WATCH_NUMBER_EXP = "STATUS REPORT ON WW\\s(\\d{1,4})";		//T976 - replaced (\\d{3}) with (\\d{1,4}
		Pattern statusReportWatchNumberPattern = Pattern.compile(STATUS_REPORT_WATCH_NUMBER_EXP);
		Matcher statusReportWatchNumberMatcher = statusReportWatchNumberPattern.matcher(segment);		
		if(statusReportWatchNumberMatcher.find()) {
			watchNumber = statusReportWatchNumberMatcher.group(1).trim(); 
//			System.out.println("in processUgcToRetrieveWatchNumber - find STATUS_REPORT_WATCH_NUMBER=" + watchNumber);
		} 
		return watchNumber; 
	}

	public static boolean isSegmentTextValid(String segment) {
		boolean isSegmentValid = false; 
		String STATUS_REPORT_WATCH_NUMBER_EXP = "STATUS REPORT ON WW\\s(\\d{1,4})";		//T976 - replaced (\\d{3}) with (\\d{1,4}
		Pattern statusReportWatchNumberPattern = Pattern.compile(STATUS_REPORT_WATCH_NUMBER_EXP);
		Matcher statusReportWatchNumberMatcher = statusReportWatchNumberPattern.matcher(segment);		
		if(statusReportWatchNumberMatcher.find()) {
			isSegmentValid = true; 
		} 
		return isSegmentValid; 
	}

	/**
	 * Process UGC line; then process VTEC, HVTEC, FIPS, and LAT/LON.
	 * 
	 * @param ugcline The UGC information
	 * @param segment The entire segment of this UGC line
	 * @param mndTime The calendar from of MND remark.
	 * @param watchList The collection of watch numbers
	 * @return a AwwUgc table
	 */
	public static AwwUgc processUgc(String ugcline, String segment, Calendar mndTime, ArrayList<String> watchList) {	
		
		final String delim="\n";
		String pvtecLine;
		Boolean trackingContinue = false;
		String trackingNumber;
		int[] latlonIndex = new int[1];

		// Regular expression for VTEC line
		final String VTEC_EXP = "(/([A-Z]{1}).([A-Z]{3}).([A-Z]{4}).([A-Z]{2}).([A-Z]{1}).([0-9]{4}).([0-9]{6}T[0-9]{4})Z-([0-9]{6}T[0-9]{4})Z/\\x0d\\x0d\\x0a)+";
		
		// Pattern used to extract VTEC line
		final Pattern vtecPattern = Pattern.compile(VTEC_EXP);
		
		// Regular expression for LAT/LON polygon
		final String LATLON_EXP = "LAT...LON( (\\d{5}|\\d{4}) (\\d{5}|\\d{4}))+";
		
		// Regular expression for the continuation of LAT/LON polygon
		final String CON_LATLON_EXP = "(( )+( (\\d{5}|\\d{4}) (\\d{5}|\\d{4}))+\\x0d\\x0d\\x0a)+";
		
		// Pattern used to extract LAT/LON line
		final Pattern latlonPattern = Pattern.compile(LATLON_EXP);

		// Pattern used to extract the continuation pairs of LAT/LON
		final Pattern conlatlonPattern = Pattern.compile(CON_LATLON_EXP);
		
		StringBuffer trackingNO = new StringBuffer("");
		
		AwwUgc currentUgc = new AwwUgc();
		
		Matcher vtecMatcher = vtecPattern.matcher(segment);
		
	    //System.out.println("in Ugc - ugcline=" + ugcline);

	    if (vtecMatcher.find()) {
	    	StringTokenizer vtecToken = new StringTokenizer(vtecMatcher.group(), delim);

	    	// Each bulletin may have multiple VTEC lines
		     while (vtecToken.hasMoreTokens()) {
		    	 pvtecLine = vtecToken.nextToken();
			     AwwVtec currentVtec=AwwParser.processVtec(pvtecLine, segment);
			     
			     trackingNumber = currentVtec.getEventTrackingNumber();
			     
			     if (trackingContinue) {
						trackingNO.append("/");
					}
					trackingNO.append(trackingNumber);

					/*
					 * Store the event tracking number as a primary key in the
					 * AWW record recognized as "watch number"
					 */
					if ( ! watchList.contains(trackingNumber)) {
						watchList.add(trackingNumber);
					}
					trackingContinue = true;
					
					// Add current P-VTEC message to set.
		           currentUgc.addAwwVtecLine(currentVtec);			         
		     }
	    }else{
	    	return null; //T976
	    }
	    
		AwwParser.processFips(ugcline, currentUgc, mndTime);
		
		latlonIndex[0] = 0;
		
		Matcher latlonMatcher = latlonPattern.matcher(segment);
		
		// Find the LAT/LON polygon
		if (latlonMatcher.find()) {
								
			// New AwwLatlons record to hold LAT/LON values
			AwwParser.processLatlons(latlonMatcher.group(), currentUgc, latlonIndex);
			
			Matcher conlatlonMatcher = conlatlonPattern.matcher(segment);

			/*
			 * find the continuation of LAT/LON polygon pairs if any
			 * note that LAT/LON may have more than one line
			 */
			if (conlatlonMatcher.find()) {
				AwwParser.processLatlons(conlatlonMatcher.group(), currentUgc, latlonIndex);
			}
		}
		
		// Set UGC line
		currentUgc.setUgc(ugcline);
		// Set the collection of event tracking numbers
		currentUgc.setEventTrackingNumber(trackingNO.toString());
	    // Replace special characters with a blank; it is more readable and set the segment
	    currentUgc.setSegment(UtilN.removeLeadingWhiteSpaces(segment.replace('\r', ' ').replace('\003', ' ').replace('\000', ' ').replace('\001', ' ').replace('\036', ' ')));
		
		return currentUgc;
			
	}
	
	/**
	 * Process UGC line; then process VTEC, HVTEC, FIPS, and LAT/LON.
	 * 
	 * @param ugcline The UGC information
	 * @param segment The entire segment of this UGC line
	 * @param mndTime The calendar from of MND remark.
	 * @param watchList The collection of watch numbers
	 * @return a AwwUgc table
	 */
//	public static AwwUgc processUgcForSereveWeatherStatus(String wmoline, Calendar mndTime, String issueOfficeId, ArrayList<String> watchList) {	
//		
//		final String delim="\n";
//		String pvtecLine;
//		Boolean trackingContinue = false;
//		String trackingNumber;
//		int[] latlonIndex = new int[1];
//
//		// Regular expression for VTEC line
//		final String VTEC_EXP = "(/([A-Z]{1}).([A-Z]{3}).([A-Z]{4}).([A-Z]{2}).([A-Z]{1}).([0-9]{4}).([0-9]{6}T[0-9]{4})Z-([0-9]{6}T[0-9]{4})Z/\\x0d\\x0d\\x0a)+";
//		
//		// Pattern used to extract VTEC line
//		final Pattern vtecPattern = Pattern.compile(VTEC_EXP);
//		
//		// Regular expression for LAT/LON polygon
//		final String LATLON_EXP = "LAT...LON( (\\d{5}|\\d{4}) (\\d{5}|\\d{4}))+";
//		
//		// Regular expression for the continuation of LAT/LON polygon
//		final String CON_LATLON_EXP = "(( )+( (\\d{5}|\\d{4}) (\\d{5}|\\d{4}))+\\x0d\\x0d\\x0a)+";
//		
//		// Pattern used to extract LAT/LON line
//		final Pattern latlonPattern = Pattern.compile(LATLON_EXP);
//
//		// Pattern used to extract the continuation pairs of LAT/LON
//		final Pattern conlatlonPattern = Pattern.compile(CON_LATLON_EXP);
//		
//		StringBuffer trackingNO = new StringBuffer("");
//		
//		AwwUgc currentUgc = new AwwUgc();
//		
//		Matcher vtecMatcher = vtecPattern.matcher(segment);
//		
//	    //System.out.println("in Ugc - ugcline=" + ugcline);
//
//	    if (vtecMatcher.find()) {
//	    	StringTokenizer vtecToken = new StringTokenizer(vtecMatcher.group(), delim);
//
//	    	// Each bulletin may have multiple VTEC lines
//		     while (vtecToken.hasMoreTokens()) {
//		    	 pvtecLine = vtecToken.nextToken();
//			     AwwVtec currentVtec=AwwParser.processVtec(pvtecLine, segment);
//			     
//			     trackingNumber = currentVtec.getEventTrackingNumber();
//			     
//			     if (trackingContinue) {
//						trackingNO.append("/");
//					}
//					trackingNO.append(trackingNumber);
//
//					/*
//					 * Store the event tracking number as a primary key in the
//					 * AWW record recognized as "watch number"
//					 */
//					if ( ! watchList.contains(trackingNumber)) {
//						watchList.add(trackingNumber);
//					}
//					trackingContinue = true;
//					
//					// Add current P-VTEC message to set.
//		           currentUgc.addAwwVtecLine(currentVtec);			         
//		     }
//	    }
//	    
//		AwwParser.processFips(ugcline, currentUgc, mndTime);
//		
//		latlonIndex[0] = 0;
//		
//		Matcher latlonMatcher = latlonPattern.matcher(segment);
//		
//		// Find the LAT/LON polygon
//		if (latlonMatcher.find()) {
//								
//			// New AwwLatlons record to hold LAT/LON values
//			AwwParser.processLatlons(latlonMatcher.group(), currentUgc, latlonIndex);
//			
//			Matcher conlatlonMatcher = conlatlonPattern.matcher(segment);
//
//			/*
//			 * find the continuation of LAT/LON polygon pairs if any
//			 * note that LAT/LON may have more than one line
//			 */
//			if (conlatlonMatcher.find()) {
//				AwwParser.processLatlons(conlatlonMatcher.group(), currentUgc, latlonIndex);
//			}
//		}
//		
//		// Set UGC line
//		currentUgc.setUgc(ugcline);
//		// Set the collection of event tracking numbers
//		currentUgc.setEventTrackingNumber(trackingNO.toString());
//	    // Replace special characters with a blank; it is more readable and set the segment
//	    currentUgc.setSegment(UtilN.removeLeadingWhiteSpaces(segment.replace('\r', ' ').replace('\003', ' ').replace('\000', ' ').replace('\001', ' ').replace('\036', ' ')));
//		
//		return currentUgc;
//			
//	}
	
	/**
	 * Process UGC line; then process VTEC, HVTEC, FIPS, and LAT/LON.
	 * 
	 * @param ugcline The UGC information
	 * @param segment The entire segment of this UGC line
	 * @param mndTime The calendar from of MND remark.
	 * @param watchList The collection of watch numbers
	 * @return a AwwUgc table
	 */
	public static AwwUgc processUgcForWtch(String ugcline, String segment, Calendar mndTime, String issueOfficeId, ArrayList<String> watchList) {	
		
		final String delim="\n";
		String pvtecLine;
		Boolean trackingContinue = false;
		String trackingNumber;
		int[] latlonIndex = new int[1];

		// Regular expression for VTEC line
		final String VTEC_EXP = "(/([A-Z]{1}).([A-Z]{3}).([A-Z]{4}).([A-Z]{2}).([A-Z]{1}).([0-9]{4}).([0-9]{6}T[0-9]{4})Z-([0-9]{6}T[0-9]{4})Z/\\x0d\\x0d\\x0a)+";
//final String WTCH_VTEC_EXP = "WW\\s\\d{3}\\sTORNADO\\s([A-Z]{2}\\s)+CW\\s(\\d{6}Z)\\s\\-\\s(\\d{6}Z)";		
final String WTCH_VTEC_EXP = "WW\\s(\\d{1,4})\\s(TORNADO|SEVERE TSTM)\\s((\\w+|\\s+)*)\\s(\\d{6}Z)\\s\\-\\s(\\d{6}Z)";		
		// Pattern used to extract VTEC line
//		final Pattern vtecPattern = Pattern.compile(VTEC_EXP);
//final Pattern wtchVtecPattern = Pattern.compile(WTCH_VTEC_EXP);		
		// Regular expression for LAT/LON polygon
		final String LATLON_EXP = "LAT...LON( (\\d{5}|\\d{4}) (\\d{5}|\\d{4}))+";
final String WTCH_LATLON_EXP = "LAT...LON( (\\d{5}|\\d{4})(\\d{5}|\\d{4}))+", WTCH_CON_LATLON_EXP = "(( )+( (\\d{5}|\\d{4})(\\d{5}|\\d{4}))+\\x0d\\x0d\\x0a)+";		
		// Regular expression for the continuation of LAT/LON polygon
		final String CON_LATLON_EXP = "(( )+( (\\d{5}|\\d{4}) (\\d{5}|\\d{4}))+\\x0d\\x0d\\x0a)+";
final Pattern wtchLatLonPattern = Pattern.compile(WTCH_LATLON_EXP), wtchConLatlonPattern = Pattern.compile(WTCH_CON_LATLON_EXP);		
		// Pattern used to extract LAT/LON line
		final Pattern latlonPattern = Pattern.compile(LATLON_EXP);

		// Pattern used to extract the continuation pairs of LAT/LON
		final Pattern conlatlonPattern = Pattern.compile(CON_LATLON_EXP);
		
		StringBuffer trackingNO = new StringBuffer("");
		
		AwwUgc currentUgc = new AwwUgc();
		
		final Pattern vtecPattern = Pattern.compile(VTEC_EXP);
		Matcher vtecMatcher = vtecPattern.matcher(segment);

final Pattern wtchVtecPattern = Pattern.compile(WTCH_VTEC_EXP);		
Matcher wtchVtecMatcher = wtchVtecPattern.matcher(segment);		
	    //System.out.println("in Ugc - ugcline=" + ugcline);
if(wtchVtecMatcher.find()) { 
	AwwParser.processVtecWtch(wtchVtecMatcher, issueOfficeId, currentUgc);
}
	    if (vtecMatcher.find()) {
	    	StringTokenizer vtecToken = new StringTokenizer(vtecMatcher.group(), delim);

	    	// Each bulletin may have multiple VTEC lines
		     while (vtecToken.hasMoreTokens()) {
		    	 pvtecLine = vtecToken.nextToken();
			     AwwVtec currentVtec=AwwParser.processVtec(pvtecLine, segment);
			     
			     trackingNumber = currentVtec.getEventTrackingNumber();
			     
			     if (trackingContinue) {
						trackingNO.append("/");
					}
					trackingNO.append(trackingNumber);

					/*
					 * Store the event tracking number as a primary key in the
					 * AWW record recognized as "watch number"
					 */
					if ( ! watchList.contains(trackingNumber)) {
						watchList.add(trackingNumber);
					}
					trackingContinue = true;
					
					// Add current P-VTEC message to set.
		           currentUgc.addAwwVtecLine(currentVtec);			         
		     }
	    }
	    
	    if (currentUgc.getAwwVtecLine() == null || currentUgc.getAwwVtecLine().size() == 0 ) //T976 - If the UGC has no VTEC/HVTEC info, return null.
	    	 return null;
	    
		AwwParser.processFips(ugcline, currentUgc, mndTime);
		
		latlonIndex[0] = 0;
Matcher wtchLatLonMatcher = wtchLatLonPattern.matcher(segment); //T456		
		Matcher latlonMatcher = latlonPattern.matcher(segment);
		
		// Find the LAT/LON polygon
		if (latlonMatcher.find()) {
								
			// New AwwLatlons record to hold LAT/LON values
			AwwParser.processLatlons(latlonMatcher.group(), currentUgc, latlonIndex);
			
			Matcher conlatlonMatcher = wtchConLatlonPattern.matcher(segment);

			/*
			 * find the continuation of LAT/LON polygon pairs if any
			 * note that LAT/LON may have more than one line
			 */
			if (conlatlonMatcher.find()) {
				AwwParser.processLatlons(conlatlonMatcher.group(), currentUgc, latlonIndex);
			}
		}
		
if(wtchLatLonMatcher.find()){			
			AwwParser.processLatlonsWtch(wtchLatLonMatcher.group(), currentUgc, latlonIndex);			
			Matcher wtchConlatlonMatcher = conlatlonPattern.matcher(segment);
			if (wtchConlatlonMatcher.find()) {
				AwwParser.processLatlonsWtch(wtchConlatlonMatcher.group(), currentUgc, latlonIndex);
			}
		}
		
		// Set UGC line
		currentUgc.setUgc(ugcline);
		// Set the collection of event tracking numbers
		currentUgc.setEventTrackingNumber(trackingNO.toString());
	    // Replace special characters with a blank; it is more readable and set the segment
	    currentUgc.setSegment(UtilN.removeLeadingWhiteSpaces(segment.replace('\r', ' ').replace('\003', ' ').replace('\000', ' ').replace('\001', ' ').replace('\036', ' ')));
		
		return currentUgc;
			
	}

	/**
	 * Parse VTEC line; then process H-VTEC if any. 
	 * 
	 * @param pvtecLine The primary VTEC line
	 * @param segment The segment contains this primary VTEC line
	 * return a AwwVtec table
	 */
	public static AwwVtec processVtec(String pvtecLine, String segment) {
		
		Calendar startTime;
		Calendar endTime;
		
		final String NEW_ACTION_VALUE = "NEW"; 
		
		// Regular expression for P-VTEC message
		final String PVTOKEN_EXP = "/([A-Z]{1}).([A-Z]{3}).([A-Z]{4}).([A-Z]{2}).([A-Z]{1}).([0-9]{4}).([0-9]{6}T[0-9]{4})Z-([0-9]{6}T[0-9]{4})Z/";
		
		// Regular expression for H-VTEC message 
		final String HVTOKEN_EXP = "/([A-Z|0-9]{5}).([A-Z|0-9]{1}).([A-Z]{2}).([0-9]{6}T[0-9]{4})Z.([0-9]{6}T[0-9]{4})Z.([0-9]{6}T[0-9]{4})Z.([A-Z|0-9]{2})/";
		
		// Pattern used to extract data from the P-VTEC line
		final Pattern pvtokenPattern = Pattern.compile(PVTOKEN_EXP);
		
		// Pattern used to extract data from the H-VTEC line 
		final Pattern hvtokenPattern = Pattern.compile(HVTOKEN_EXP);
			         
	         
		AwwVtec currentVtec = new AwwVtec();
		
		// Add entire P-VTEC line to set
	    currentVtec.setVtecLine(pvtecLine);;
	   	 
	   	Matcher pvtokenMatcher = pvtokenPattern.matcher(pvtecLine);
	   	
	   	if (pvtokenMatcher.find()) {			
   	       
	   		// Set the product class
	   		String productClass = pvtokenMatcher.group(1); 
			currentVtec.setProductClass(productClass);
			
			// Set the "action"
			String action = pvtokenMatcher.group(2); 
			currentVtec.setAction(action);

			// Set the office ID
			String officeId = pvtokenMatcher.group(3); 
			currentVtec.setOfficeID(officeId);
			
			// Set the phenomena
			String phenomena = pvtokenMatcher.group(4); 
			currentVtec.setPhenomena(phenomena);
			
			// Set the significance
			String significance = pvtokenMatcher.group(5); 
			currentVtec.setSignificance(significance);
			
			// Set the event tracking number
			String eventTrackingNumber = pvtokenMatcher.group(6); 
			currentVtec.setEventTrackingNumber(eventTrackingNumber);

			/*
			 * comment out the old way of setting both event start/end times 
			 */
//			// Set the event start time as yymmddThhnn
//			startTime = AwwParser.findEventTime(pvtokenMatcher.group(7));
//			currentVtec.setEventStartTime(startTime);
//			
//			// Set the event end time as yymmddThhnn
//			endTime = AwwParser.findEventTime(pvtokenMatcher.group(8));
//			currentVtec.setEventEndTime(endTime);

			
			/*
			 * The follow logic is the modified way of setting the 
			 * event start/end times. Basically it tries to look at the previous 
			 * records of the same event in DB and fill the missed values
			 */
			String eventStartTimeString = pvtokenMatcher.group(7); 
			String eventEndTimeString = pvtokenMatcher.group(8); 
			
			AwwVtecDataInfo awwVtecEventTimeInfoObject = getEventTimeInfo(eventStartTimeString,
					eventEndTimeString, productClass, officeId, phenomena, significance, eventTrackingNumber); 

			startTime = awwVtecEventTimeInfoObject.getEventStartTime();
			currentVtec.setEventStartTime(startTime);

			endTime = awwVtecEventTimeInfoObject.getEventEndTime();
			currentVtec.setEventEndTime(endTime);

			/*
			 * Now we add one more logic to try to fill the holes of eventstarttime is null
			 */
			if(isEventStartTimeValidToBeUsedToFillNullValues(action, NEW_ACTION_VALUE, awwVtecEventTimeInfoObject.getEventStartTime())) {
				fillNullEventStartTimeValues(awwVtecEventTimeInfoObject.getEventStartTime(), 
						productClass, officeId, phenomena, significance, eventTrackingNumber); 
			}
			
			// Check if any H-VTEC line after the P-VTEC line
			Matcher hvtokenMatcher = hvtokenPattern.matcher(segment);

		    if (hvtokenMatcher.find()) {
		    	AwwHVtec currentHVtec=AwwParser.processHVtec(hvtokenMatcher.group());
		    	
		    	// Add current H-VTEC message to set.
	            currentVtec.addAwwHVtecLine(currentHVtec);	
		    }
		    			    
		} 

		return currentVtec;	
		
	}
	
	/**
	 * 
	 * @param action
	 * @param newActionValue
	 * @param eventStartTime
	 * @return
	 */
	private static boolean isEventStartTimeValidToBeUsedToFillNullValues(String action, String newActionValue, 
			Calendar eventStartTime) {
		boolean isValid = false; 
		if(newActionValue.equalsIgnoreCase(action) && eventStartTime != null) {
			isValid = true; 
		}
		return isValid; 
	}
	
	/**
	 * retrieve the previous records of the same event in DB
	 * @param eventStartTimeString
	 * @param eventEndTimeString
	 * @param productClass
	 * @param officeId
	 * @param phenomena
	 * @param significance
	 * @param eventTrackingNumber
	 * @return
	 */
	private static AwwVtecDataInfo getEventTimeInfo(String eventStartTimeString,
			String eventEndTimeString, String productClass, String officeId, 
			String phenomena, String significance, String eventTrackingNumber) {
		AwwVtecDataInfo awwVtecDataInfo = new AwwVtecDataInfo(); 
		Calendar eventStartTime = AwwParser.findEventTime(eventStartTimeString); 
		Calendar eventEndTime = AwwParser.findEventTime(eventEndTimeString); 
		if(eventStartTime != null)
			awwVtecDataInfo.setEventStartTime(eventStartTime); 
		if(eventEndTime != null)
			awwVtecDataInfo.setEventEndTime(eventEndTime); 
		awwVtecDataInfo = AwwVtecDataUtil.populateAwwVtecEventTimeInfo(awwVtecDataInfo, productClass, officeId, 
				phenomena, significance, eventTrackingNumber); 
		return awwVtecDataInfo; 
	}
	
	private static void fillNullEventStartTimeValues(Calendar eventStartTime,
			String productClass, String officeId, 
			String phenomena, String significance, String eventTrackingNumber) {
		try {
			AwwVtecDataUtil.populateAwwVtecEventStartTimeWithValidValue(eventStartTime, productClass, officeId, 
					phenomena, significance, eventTrackingNumber); 
		} catch (DataAccessLayerException dale) {
			System.out.println("======Caught DataAccessLayerException in method fillNullEventStartTimeValues(...), error=" + dale.getMessage()); 
		}
	}
	
	/**
	 * Set each LAT...LON record with index.
	 * 
	 * @param flat The latitude 
	 * @param flong The longitude
	 * @param latlonIndex The index of this pair
	 * @return a AwwLatlons table
	 */
	public static AwwLatlons setLatlon(float flat, float flong, int latlonIndex) {
	
		 // New AwwLatlons record to hold lat/lon values
		 AwwLatlons currentLatlons = new AwwLatlons();
		 currentLatlons.setIndex(latlonIndex);
		 
		 currentLatlons.setLat(flat);
		 currentLatlons.setLon(flong);
		 
		 return currentLatlons;	 
	}
	
	/**
	 * Parse the LAT...LON lines.
	 * 
	 * @param latlons The latlon lines
	 * @param UGC The AwwUgc table
	 * @param latlonIndex The index of Lat/Long
	 * 
	 */
	public static void processLatlons(String latlons, AwwUgc UGC, int[] latlonIndex) {
		
		String currentToken=null;
		String latlon="LAT...LON";
		String latitude = null;
		String longitude = null;
		boolean pair = false;
		Float flat, flong;
		
		StringTokenizer latlonTokens = new StringTokenizer(latlons);
				
	     while (latlonTokens.hasMoreTokens()) {
	    	 currentToken = latlonTokens.nextToken();
	         if ( ! currentToken.equals(latlon)) {
	        	 if ( pair ) {
	        		 longitude = currentToken;
	        		 latlonIndex[0]++;
	        		 pair = false;
	        		 
	        		 // New AwwLatlons record to hold LAT/LON values.
	        		 flat = (float)(Integer.parseInt(latitude) / 100.0);
	        		 flong = (float)((Integer.parseInt(longitude) / 100.0) * (-1.0));
	        		 AwwLatlons currentLatlons= AwwParser.setLatlon(flat, flong, latlonIndex[0]);
	        		  
	        		 // Add current LAT/LON and index to set.
		        	 UGC.addAwwLatLon(currentLatlons);		        	 
	        	 }
	        	 else {
	        		 latitude = currentToken;
	        		 pair = true;
	        	 }
	         }
	     }	     
	}
	
	/**
	 * Decode the county FIPS, and find the production purge date.
	 * 
	 * @param ugc The UGC line which contains FIPS
	 * @param UGC The AwwUgc table
	 * @param mndTIme The calendar from MND remark
	 */
	public static void processFips(String ugc, AwwUgc UGC, Calendar mndTime) {
		
	    String countyFips=null;
	    String county=null;
	    String fipsToken=null;
	    
	    final String delim="-,\n";
		final String inclusiveDelim=">";
	    
	    /*
		 * Here are many possible cases:
		 * 1. PAC055-057-140130-
		 * 2. ILZ027>031-036>038-040>045-047>054-061-141015-
		 * 3. LAC001-003-009-011-019-023-039-045-053-055-079-097-099-101-113-^M
		 *    115-131500-^M
		 * 4. ILC129-221523-
		 * 5. LCZ422-423-460-LEZ444-LHZ421-422-441-442-443-462-463-464-^M
		 *    191000-^M
		 * 6. ILZ000-INZ000-MIZ000-LHZ000-LMZ000-190400-^M
		 * 7. KYC001-003-005-009-015-017-021-023-027-029-031-037-041-045-049-^M
		 *    053-057-061-067-073-077-079-081-085-087-093-097-099-103-111-113-^M
		 *    117-123-137-141-151-155-161-163-167-169-171-179-181-185-187-191-^M
		 *    201-207-209-211-213-215-217-223-227-229-239-190900-^M
		 */
		StringTokenizer fipsTokens = new StringTokenizer(ugc, delim);
		while (fipsTokens.hasMoreTokens()) {

			fipsToken = fipsTokens.nextToken();

			if ( fipsToken.length() == 6 && Character.isLetter(fipsToken.toCharArray()[0] ))  {
				// A brand new county FIPS with format NAMDDD
				countyFips =fipsToken;
				county = fipsToken.substring(0,3);

				AwwFips currentFips=AwwFips.setFIPS(countyFips);
				UGC.addAwwFIPS(currentFips);
			} else if ( fipsToken.length() == 10 ) {
				String intervalToken = fipsToken.substring(3,10);
				county = fipsToken.substring(0,3);

				// Format in NAMDDD1>DDD2
				StringTokenizer twoTokens = new StringTokenizer(intervalToken, inclusiveDelim); 
				String firstToken = twoTokens.nextToken();
				String secondToken = twoTokens.nextToken();

				Integer countyBegin = Integer.parseInt( firstToken );
				Integer countyEnd = Integer.parseInt( secondToken );

				for ( int counter = countyBegin; counter <= countyEnd; counter++) {

					String inclusiveToken = Integer.toString(counter);

					// set "1" to "001" ...etc
					if (counter < 10) {
						inclusiveToken = "00".concat(inclusiveToken);
					} 

					// set "10" to "010" ...etc
					else if (counter < 100) {
						inclusiveToken = "0".concat(inclusiveToken);
					}
					countyFips = county.concat( inclusiveToken );

					AwwFips currentFips=AwwFips.setFIPS(countyFips);
					UGC.addAwwFIPS(currentFips);
				}
			} else if ( fipsToken.length() == 3 && Character.isDigit(fipsToken.toCharArray()[0]) ) {
				// A continuation of previous county FIPS with format DDD
				countyFips = county.concat( fipsToken );

				AwwFips currentFips=AwwFips.setFIPS(countyFips);
				UGC.addAwwFIPS(currentFips);	 
			} 
			else if ( fipsToken.length() == 7 && Character.isDigit(fipsToken.toCharArray()[0]) ) {
				// A continuation of previous county FIPS with format DDD1>DDD2
				StringTokenizer twoTokens = new StringTokenizer(fipsToken, inclusiveDelim); 
				String firstToken = twoTokens.nextToken();
				String secondToken = twoTokens.nextToken();

				Integer countyBegin = Integer.parseInt( firstToken );
				Integer countyEnd = Integer.parseInt( secondToken );

				for ( int counter = countyBegin; counter <= countyEnd; counter++) {

					String inclusiveToken = Integer.toString(counter);

					// set "1" to "001" ...etc
					if (counter < 10) {
						inclusiveToken = "00".concat(inclusiveToken);
					} 

					// set "10" to "010" ...etc
					else if (counter < 100) {
						inclusiveToken = "0".concat(inclusiveToken);
					}
					countyFips = county.concat( inclusiveToken );

					AwwFips currentFips=AwwFips.setFIPS(countyFips);
					UGC.addAwwFIPS(currentFips);
				}
			} 
			else if ( fipsToken.length() == 6 && Character.isDigit(fipsToken.toCharArray()[0]) ) {
				// The last item is the UGC product purge time
				try {
					Calendar purgeDate = UtilN.findDataTime(fipsToken, mndTime);
					UGC.setProdPurgeTime(purgeDate);
				} catch(Exception e) {
					//do nothing
				}
			}

	     }
	    	    	     	     
	}

	/**
	 * Process H-VTEC line. 
	 * 
	 * @param hvtecs The H-VTEC line
	 * @return a AwwHVtec table
	 */
	public static AwwHVtec processHVtec(String hvtecs) {
	    
		Calendar startTime = null;
		Calendar endTime = null;
		Calendar crestTime = null;
	
		// Regular expression for H-VTEC message
		final String HVTOKEN_EXP = "/([A-Z|0-9]{5}).([A-Z|0-9]{1}).([A-Z]{2}).([0-9]{6}T[0-9]{4})Z.([0-9]{6}T[0-9]{4})Z.([0-9]{6}T[0-9]{4})Z.([A-Z|0-9]{2})/";
		
		// Pattern used for extracting data from the H-VTEC line
		final Pattern hvtokenPattern = Pattern.compile(HVTOKEN_EXP);
	    	         
	         AwwHVtec currentHVtec = new AwwHVtec();
	         
	         // Add entire H-VTEC line to set
	         currentHVtec.setHvtecLine(hvtecs);
        	 
        	 Matcher hvtokenMatcher = hvtokenPattern.matcher(hvtecs);

			if (hvtokenMatcher.find()) {
        	 
	        	currentHVtec.setHvtecLine(hvtokenMatcher.group());
				
	        	// Set the NWS location identifier
				currentHVtec.setLocationIdentifier(hvtokenMatcher.group(1));
			
				// Set the flood severity
				currentHVtec.setFloodSeverity(hvtokenMatcher.group(2));
				
				// Set the Immediate Cause
				currentHVtec.setImmediateCause(hvtokenMatcher.group(3));
				
				// Set the flood begin time
				startTime = AwwParser.findEventTime(hvtokenMatcher.group(4));
				currentHVtec.setEventStartTime(startTime);
				
				// Set the flood crest time
				crestTime = AwwParser.findEventTime(hvtokenMatcher.group(5));
				currentHVtec.setEventCrestTime(crestTime);

				// Set the end time as yymmddThhnn
				endTime = AwwParser.findEventTime(hvtokenMatcher.group(6));
				currentHVtec.setEventEndTime(endTime);
				
				// Set the flood record
				currentHVtec.setFloodRecord(hvtokenMatcher.group(7));
								
			} 
			
			return currentHVtec;

	}
	
	/**
	 * Get report type from bulletin. 
	 * 
	 * @param bull The bulletin message
	 * @return a report type
	 */
	public static String getReportType(String bull) {
		
		/*
		 * There are many report types as follows:
		 * 1. SEVERE THUNDERSTORM WARNING
		 * 2. SEVERE THUNDERSTORM WATCH
		 * 3. TORNADO WARNING
		 * 4. TORNADO WATCH
		 * 5. SEVERE THUNDERSTORM OUTLINE UPDATE
		 * 6. TORNADO WATCH OUTLINE UPDATE
		 * 7. FLASH FLOOD WARNING
		 * 8. FLASH FLOOD WATCH
		 * 9. FLOOD WARNING
		 * 10. FLOOD WATCH
		 * 11. FLOOD STATEMENT
		 * 12. WINTER STORM WARNING
		 * 13. WINTER STORM WATCH
		 * 14. WATCH COUNTY NOTIFICATION 
		 * 15. SEVERE WEATHER STATEMENT
		 * 16. WIND ADVISORY 
		 * 17. FOG ADVISORY
		 * 18. HEAT ADVISORY
		 * 19. FROST ADVISORY
		 * 20. SMOKE ADVISORY
		 * 21. WEATHER ADVISORY
		 * 22. WINTER WEATHER ADVISORY
		 * 23. SIGNIGICANT WEATHER ADVISORY
		 * 24. SPECIAL WEATHER STATEMENT
		 * 25. RED FLAG WARNING
		 * 26. TORNADO REPORT
		 * 27. HIGH WIND WARNING
		 * 28. FREEZE WARNING
		 * 29. ADVERTENCIA DE INUNDACIONES
		 * 30. HYDROLOGIC STATEMENT
		 * 31. URGENT WEATHER MESSAGE
		 */
	    
		String reportType = null;
		
	
		// Regular expression for report type
//		final String REPORT_EXP = "(SEVERE THUNDERSTORM|TORNADO|FLOOD|WINTER STORM|WATCH COUNTY NOTIFICATION|ADVISORY|WEATHER STATEMENT|RED FLAG WARNING)";
		/*
		 * add the key words "STATUS REPORT" at the beginning of the regular expression string pattern
		 */
		final String REPORT_EXP = "(STATUS REPORT|SEVERE THUNDERSTORM|SEVERE TSTM|TORNADO|FLOOD|WINTER STORM|WATCH COUNTY NOTIFICATION|ADVISORY|WEATHER STATEMENT|RED FLAG WARNING)";
		
		// Pattern used for extracting data from the report type
		final Pattern reportPattern = Pattern.compile(REPORT_EXP);
	    	                 	 
		Matcher reportMatcher = reportPattern.matcher(bull);

		if (reportMatcher.find()) {
    	    String type = reportMatcher.group(1);
    	    if ( type.compareTo("STATUS REPORT") == 0 ) {
    	    	reportType = getStatusReportType(bull);
    	    } 
    	    else if ( type.compareTo("SEVERE THUNDERSTORM") == 0 || 
    	    		type.compareTo("SEVERE TSTM") == 0) {
    	    	reportType = getThunderstormType(bull);
    	    } 
    	    else if ( type.compareTo("TORNADO") == 0 ) {
    	    	reportType = getTornadoType(bull);
    	    }
    	    else if ( type.compareTo("FLOOD") == 0 ) {
    	    	reportType = getFloodType(bull);
    	    }
    	    else if ( type.compareTo("WINTER STORM") == 0 ) {
    	    	reportType = getWinterstormType(bull);
    	    }
    	    else if ( type.compareTo("ADVISORY") == 0 ) {
    	    	reportType = getAdvisoryType(bull);
    	    }
    	    else if ( type.compareTo("WATCH COUNTY NOTIFICATION") == 0 ) {
    	    	reportType = "WATCH COUNTY NOTIFICATION";
    	    }
    	    else if ( type.compareTo("RED FLAG WARNING") == 0 ) {
    	    	reportType = "RED FLAG WARNING";
    	    }
    	    else if ( type.compareTo("WEATHER STATEMENT") == 0 ) {
    	    	reportType = getWeatherstatementType(bull);
    	    }
		} else {
	    	reportType = getOtherType(bull);
	    }
			
		return reportType;
			
	}
	
	/**
	 * Get report type from thunderstorm report.
	 * 
	 * @param bull The bulletin message
	 * @return a report type
	 */
	public static String getThunderstormType(String bull) {
		String thunderstormType = "SEVERE THUNDERSTORM ";
		// Regular expression for report type
		final String REPORT_EXP = "SEVERE THUNDERSTORM (WARNING|WATCH|OUTLINE UPDATE)";
		final Pattern reportPattern = Pattern.compile(REPORT_EXP);
		Matcher reportMatcher = reportPattern.matcher(bull);
		if (reportMatcher.find()) {
			thunderstormType = thunderstormType.concat(reportMatcher.group(1));
		} else {
			thunderstormType = "THUNDERSTORM REPORT"; 
		}
		return thunderstormType;
	}
		
	/**
	 * Get report type from status report.
	 * @param bull, The bulletin message
	 * @return a report type
	 */
	public static String getStatusReportType(String bull) {
		String tornadoType = "STATUS REPORT";
		return tornadoType;
	}
	
	/**
	 * Get report type from tornado report.
	 * 
	 * @param bull The bulletin message
	 * @return a report type
	 */
	public static String getTornadoType(String bull) {
		String tornadoType = "TORNADO ";
		final String REPORT_EXP = "TORNADO (WARNING|WATCH OUTLINE UPDATE|WATCH)";
		final Pattern reportPattern = Pattern.compile(REPORT_EXP);
		Matcher reportMatcher = reportPattern.matcher(bull);
		if (reportMatcher.find()) {
			tornadoType = tornadoType.concat(reportMatcher.group(1));
		} else {
			tornadoType = "TORNADO REPORT";
		}
		return tornadoType;
	}
	
	/**
	 * Get report type from flood report.
	 * 
	 * @param bull The bulletin message
	 * @return a report type
	 */
	public static String getFloodType(String bull) {
		String floodType = "FLOOD ";
		final String REPORT_EXP = "(FLASH )?FLOOD (WARNING|WATCH|STATEMENT|ADVISORY)";
		final Pattern reportPattern = Pattern.compile(REPORT_EXP);
		Matcher reportMatcher = reportPattern.matcher(bull);
		
		final String FLASH_EXP = "(FLASH )FLOOD (WARNING|WATCH|STATEMENT)";
		final Pattern flashPattern = Pattern.compile(FLASH_EXP);
		Matcher flashMatcher = flashPattern.matcher(bull);
		
		final String HYDRO_EXP = "(HYDROLOGIC STATEMENT)";
		final Pattern hydroPattern = Pattern.compile(HYDRO_EXP);	                 	 
		Matcher hydroMatcher = hydroPattern.matcher(bull);
		
		if (reportMatcher.find()) {
			floodType = floodType.concat(reportMatcher.group(2));
			if (flashMatcher.find()) {
				floodType = flashMatcher.group(1).concat(floodType);
			}
		} else if (hydroMatcher.find()) {
    	    floodType = hydroMatcher.group(1);
		}
		
		return floodType;
	}
	
	/**
	 * Get report type from winter storm report.
	 * 
	 * @param bull The bulletin message
	 * @return a report type
	 */
	public static String getWinterstormType(String bull) {
		String winterstormType = "WINTER STORM ";
		final String REPORT_EXP = "WINTER STORM (WARNING|WATCH)";
		final Pattern reportPattern = Pattern.compile(REPORT_EXP);
		Matcher reportMatcher = reportPattern.matcher(bull);
		if (reportMatcher.find()) {
			winterstormType = winterstormType.concat(reportMatcher.group(1));
		}
		return winterstormType;
	}
	
	/**
	 * Get report type from advisory report.
	 * 
	 * @param bull The bulletin message
	 * @return a report type
	 */
	public static String getAdvisoryType(String bull) {
		String advisoryType = "ADVISORY";
		final String REPORT_EXP = "(FOG|WIND|HEAT|FROST|SMOKE|WINTER WEATHER|SIGNIFICANT WEATHER|WEATHER) ADVISORY";
		final Pattern reportPattern = Pattern.compile(REPORT_EXP);
		Matcher reportMatcher = reportPattern.matcher(bull);
		if (reportMatcher.find()) {
			advisoryType = reportMatcher.group(1).concat(" ADVISORY");
		}
		return advisoryType;
	}
	
	/**
	 * Get report type from weather statement report.
	 * 
	 * @param bull The bulletin message
	 * @return a report type
	 */
	public static String getWeatherstatementType(String bull) {
		String weatherstatementType = " WEATHER STATEMENT";
		final String REPORT_EXP = "(SPECIAL|SEVERE) WEATHER STATEMENT";
		final Pattern reportPattern = Pattern.compile(REPORT_EXP);
		Matcher reportMatcher = reportPattern.matcher(bull);

		if (reportMatcher.find()) {
			weatherstatementType = reportMatcher.group(1).concat(weatherstatementType);
		}
		return weatherstatementType;
	}
	
	/**
	 * Get report type from other weather message report.
	 * 
	 * @param bull The bulletin message
	 * @return a report type
	 */
	public static String getOtherType(String bull) {
		
		String otherType = "URGENT WEATHER MESSAGE";
		
		// Regular expression for report type
//		final String REPORT_EXP = "(HIGH WIND WARNING|FREEZE WARNING|ADVERTENCIA DE INUNDACIONES|HYDROLOGIC STATEMENT|RIVER STATEMENT)";
		/*
		 * add one more report type for "STATUS REPORT". Comment by M. Gao
		 */
		final String REPORT_EXP = "(HIGH WIND WARNING|FREEZE WARNING|ADVERTENCIA DE INUNDACIONES|HYDROLOGIC STATEMENT|RIVER STATEMENT|STATUS REPORT)";
		
		// Pattern used for extracting data from the report type
		final Pattern reportPattern = Pattern.compile(REPORT_EXP);
	    	                 	 
		Matcher reportMatcher = reportPattern.matcher(bull);

		if (reportMatcher.find()) {
    	    otherType = reportMatcher.group(1);
		}
		
//		/*
//		 * if the key words 'SEVERE TSTM' is used, convert report type to 'SEVERE_THUNDERSTORM'
//		 */
//		if("SEVERE TSTM".equalsIgnoreCase(otherType)) {
//			otherType = "SEVERE THUNDERSTORM"; 
//		}
		
		return otherType;
	}
	
//---------------------------------Ticket 456:
	public static final String WTCH_BOX_UGC_LINE = "";
	
	public static void processLatlonsWtch(String latlons, AwwUgc UGC, int[] latlonIndex) {
		
		String currentToken=null;
		String latlon="LAT...LON";
		String latitude = null;
		String longitude = null;
		boolean pair = false;
		Float flat, flong;
		
		StringTokenizer latlonTokens = new StringTokenizer(latlons);
				
	     while (latlonTokens.hasMoreTokens()) {
	    	 currentToken = latlonTokens.nextToken();
	         if ( ! currentToken.equals(latlon)) {
	        	 
	 			latitude = currentToken.substring(0,4);//lat: 4-digit
				longitude = currentToken.substring(4);

				flat = (float)(Integer.parseInt(latitude) / 100.0);
				flong = (float)((Integer.parseInt(longitude) / 100.0) * (-1.0));
				latlonIndex[0]++;
				
				AwwLatlons currentLatlons= AwwParser.setLatlon(flat, flong, latlonIndex[0]);
				UGC.addAwwLatLon(currentLatlons);

/*	        	 
	        	 if ( pair ) {
	        		 longitude = currentToken;
	        		 latlonIndex[0]++;
	        		 pair = false;
	        		 
	        		 // New AwwLatlons record to hold LAT/LON values.
	        		 flat = (float)(Integer.parseInt(latitude) / 100.0);
	        		 flong = (float)((Integer.parseInt(longitude) / 100.0) * (-1.0));
	        		 AwwLatlons currentLatlons= AwwParser.setLatlon(flat, flong, latlonIndex[0]);
	        		  
	        		 // Add current LAT/LON and index to set.
		        	 UGC.addAwwLatLon(currentLatlons);		        	 
	        	 }
	        	 else {
	        		 latitude = currentToken;
	        		 pair = true;
	        	 }
*/	        	 
	        	 
	         }
	     }	     
	}
	
	public static AwwVtec processVtectForSevereWeatherStatus(String wmoline, Calendar recordIssueTime, String issueOfficeId) {
		//STATUS REPORT ON WW 865^M
		//
		//SEVERE WEATHER THREAT CONTINUES RIGHT OF A LINE FROM 50 E JAX TO^M
		//20 WSW JAX TO 20 SE GNV.^M
	
		AwwVtec awwVtec = new AwwVtec();
		final String TRACK_NUMBER_VTEC_EXP = "STATUS REPORT ON WW\\s(\\d{3}\\s)";		//(\\d{6}
		// Pattern used to extract track number for VTEC 
		final Pattern trackNumberPattern = Pattern.compile(TRACK_NUMBER_VTEC_EXP);
		Matcher trackNumberMatcher = trackNumberPattern.matcher(wmoline);		
		if(trackNumberMatcher.find()) {
			String eventTrackingNumber = trackNumberMatcher.group(1).trim(); 
//			System.out.println("in processVtectForSevereWeatherStatus - trackNumber=" + eventTrackingNumber);
			awwVtec.setEventTrackingNumber(eventTrackingNumber); 
		}

	    String SEVERE_WEATHER_THREAT_LINE_EXP = "SEVERE WEATHER THREAT CONTINUES RIGHT OF A LINE FROM\\s((\\w|\\s)*)."; 
	    Pattern severeWeatherThreatLinePattern = Pattern.compile(SEVERE_WEATHER_THREAT_LINE_EXP); 
	    Matcher severeWeatherThreatLineMatcher = severeWeatherThreatLinePattern.matcher(wmoline); 
	    if(severeWeatherThreatLineMatcher.find()) {
//	    	String group = severeWeatherThreatLineMatcher.group(); 
	    	String severeWeatherThreatLine = severeWeatherThreatLineMatcher.group(1); 
//	    	System.out.println("====, line info parser value group = " + group); 
//	    	System.out.println("====, line info parser value severeWeatherThreatLine = " + severeWeatherThreatLine); 
	    	String vtecLine = severeWeatherThreatLine.replace("\t", " ").replace("\r", " ").replace("\n", " "); 
//	    	System.out.println("====, vtecLine = " + vtecLine); 
	    	awwVtec.setVtecLine(vtecLine); 
	    }
	    
	    awwVtec.setOfficeID(issueOfficeId);
	    awwVtec.setSignificance("WW");
//	    awwVtec.setEventStartTime(recordIssueTime); 
	    
	    String SEVERE_WEATHER_THREAT_EVENT_END_TIME_EXP = "WW-[a-zA-Z]\\s(\\d{6})\\s"; 
	    Pattern severeWeatherThreatEventEndTimePattern = Pattern.compile(SEVERE_WEATHER_THREAT_EVENT_END_TIME_EXP); 
	    Matcher severeWeatherThreatEventEndTimeMatcher = severeWeatherThreatEventEndTimePattern.matcher(wmoline); 
	    String severeWeatherThreatEventEndTimeString = null; 
	    if(severeWeatherThreatEventEndTimeMatcher.find()) {
	    	severeWeatherThreatEventEndTimeString = severeWeatherThreatEventEndTimeMatcher.group(1);
//	    	Calendar severeWeatherThreatEventEndTime = convertStringToCalendar(severeWeatherThreatEventEndTimeString); 
//	    	if(severeWeatherThreatEventEndTime != null)
//	    		awwVtec.setEventEndTime(severeWeatherThreatEventEndTime); 
	    }
	    
	    String SEVERE_WEATHER_THREAT_MONTH_DAY_YEAR_EXP = "..(\\d{2}/\\d{2}/\\d{2})\\s"; 
	    Pattern severeWeatherThreatMonthDayYearPattern = Pattern.compile(SEVERE_WEATHER_THREAT_MONTH_DAY_YEAR_EXP); 
	    Matcher severeWeatherThreatMonthDayYearMatcher = severeWeatherThreatMonthDayYearPattern.matcher(wmoline); 
	    String monthString = null;
	    String yearString = null; 
	    if(severeWeatherThreatMonthDayYearMatcher.find()) {
	    	String monthDayYearString = severeWeatherThreatMonthDayYearMatcher.group(1);
	    	String[] monthDayYearStringArray = monthDayYearString.split("/"); 
	    	monthString = monthDayYearStringArray[0]; 
//	    	String dayString = monthDayYearStringArray[1]; 
	    	yearString = monthDayYearStringArray[2]; 
	    }

    	Calendar severeWeatherThreatEventStartTime = addMonthAndYearValueToCalendar(recordIssueTime, 
    			monthString, yearString); 
    	awwVtec.setEventStartTime(severeWeatherThreatEventStartTime);
    	
    	Calendar severeWeatherThreatEventEndTime = convertStringToCalendar(severeWeatherThreatEventEndTimeString, 
    			monthString, yearString); 
    	if(severeWeatherThreatEventEndTime != null)
    		awwVtec.setEventEndTime(severeWeatherThreatEventEndTime); 

	    return awwVtec; 
	}

	public static void processVtecWtch(Matcher wtchVtecMatcher, String issueOfficeId, AwwUgc awwugc){
		
		//WW 865 TORNADO FL GA CW 100825Z - 101700Z
		//WW 893 SEVERE TSTM AR OK TX 202140Z - 210500Z

//		int groupCount = wtchVtecMatcher.groupCount(); 
//		String groupString3 = wtchVtecMatcher.group(3); 
		String eventTrackingNumber = wtchVtecMatcher.group(1);
		String eventPhenomenaString = wtchVtecMatcher.group(2);
		String eventStartTimeString = wtchVtecMatcher.group(5);
		String eventEndTimeString = wtchVtecMatcher.group(6);
		
		AwwVtec awwvtec = new AwwVtec();
		awwvtec.setEventTrackingNumber(eventTrackingNumber);
//		awwvtec.setOfficeID("KWNS");//spc
		awwvtec.setOfficeID(issueOfficeId);//spc
		awwvtec.setPhenomena(eventPhenomenaString);
		awwvtec.setSignificance("WW");//WTCH");//watch box
		
		Calendar eventStartTime = convertStringToCalendar(eventStartTimeString);
		awwvtec.setEventStartTime(eventStartTime);
		
		Calendar eventEndTime = convertStringToCalendar(eventEndTimeString);
		awwvtec.setEventEndTime(eventEndTime);
		
		awwugc.addAwwVtecLine(awwvtec);
		
	}

	public static void processVtecWtch2(String wtchLine, String issueOfficeId, AwwUgc awwugc){
		
		//WW 865 TORNADO FL GA CW 100825Z - 101700Z
		
		String[] elements = wtchLine.split(" ");
		
		int size = elements.length;		
		if( size < 7){
			logger.info("Not a valid Wtch2 line");
			return;
		}
		
		AwwVtec awwvtec = new AwwVtec();
		awwvtec.setEventTrackingNumber(elements[1]);
//		awwvtec.setOfficeID("KWNS");//spc
		awwvtec.setOfficeID(issueOfficeId);//spc
		awwvtec.setPhenomena(elements[2]);
		awwvtec.setSignificance("WW");//WTCH");//watch box
		
		Calendar eventStartTime = convertStringToCalendar(elements[size-3]);
		awwvtec.setEventStartTime(eventStartTime);
		
		Calendar eventEndTime = convertStringToCalendar(elements[size-1]);
		awwvtec.setEventEndTime(eventEndTime);
		
		awwugc.addAwwVtecLine(awwvtec);
		
	}

	/*
	 * timeString is in format: e.g. 101102, 10 - day in the month, 11 - Hour of Day, 02 - minute
	 */
	public static Calendar addMonthAndYearValueToCalendar(Calendar calendar, String monthString, String yearString) {

		if(calendar != null) {
			/*
			 * set up the month value
			 */
			if(isMonthValueValid(monthString)) {
				int month = Integer.parseInt(monthString);
//				if(month == 12)
//					month = 0; 
				calendar.set(Calendar.MONTH, month - 1); 
			}

			/*
			 * set up the year value
			 */
			if(isYearValueValid(yearString)) {
				int year = Integer.parseInt(yearString); 
				year += 2000; 
				calendar.set(Calendar.YEAR, year); 
			}
		}

		return calendar;
	}

	/*
	 * timeString is in format: e.g. 101102, 10 - day in the month, 11 - Hour of Day, 02 - minute
	 */
	public static Calendar convertStringToCalendar(String timeString, String monthString, String yearString) {
		
		final String zeroTimePattern = "000000T0000";
		Calendar calendar = null;
		
		if ( ! timeString.equals(zeroTimePattern)) {
		
			calendar = Calendar.getInstance();
			
			int day = Integer.parseInt(timeString.substring(0, 2).trim());
			int hour = Integer.parseInt(timeString.substring(2, 4).trim());
			int minute = Integer.parseInt(timeString.substring(4, 6).trim());
			
			calendar.set(Calendar.DAY_OF_MONTH, day);
			calendar.set(Calendar.HOUR_OF_DAY, hour);
			calendar.set(Calendar.MINUTE, minute);
			calendar.set(Calendar.SECOND, 0);
			calendar.set(Calendar.MILLISECOND, 0);
			
			/*
			 * set up the month value
			 */
			if(isMonthValueValid(monthString)) {
				int month = Integer.parseInt(monthString);
//				if(month == 12)
//					month = 0; 
				calendar.set(Calendar.MONTH, month - 1); 
			}
			
			/*
			 * set up the year value
			 */
			if(isYearValueValid(yearString)) {
				int year = Integer.parseInt(yearString); 
				year += 2000; 
				calendar.set(Calendar.YEAR, year); 
			}
		}

		return calendar;
		
	}
	
	private static boolean isMonthValueValid(String monthString) {
		boolean isMonthValueValid = false; 
		if(!isStringEmpty(monthString)) {
			try {
				int monthInInteger = Integer.parseInt(monthString);
				if(monthInInteger > 0 && monthInInteger < 13)
					isMonthValueValid = true; 
			} catch(NumberFormatException nfe) {
				//do nothing
			}
		}
		return isMonthValueValid; 
	}
	
	private static boolean isYearValueValid(String yearString) {
		boolean isYearValueValid = false; 
		if(!isStringEmpty(yearString)) {
			try {
				int yearInInteger = Integer.parseInt(yearString);
				/*
				 * The assumption is users may use either 4 digits or 2 dogits to
				 * represent the value of the year
				 */
//				if((yearInInteger > 1900 && yearInInteger < 2150) ||
//						(yearInInteger >= 0 && yearInInteger < 100))
				if(yearInInteger >= 0 && yearInInteger < 100)
					isYearValueValid = true; 
			} catch(NumberFormatException nfe) {
				//do nothing
			}
		}
		return isYearValueValid; 
	}
	
	private static boolean isStringEmpty(String str) {
		boolean isStringEmpty = true; 
		if(str != null && str.trim().length() != 0)
			isStringEmpty = false; 
		return isStringEmpty; 
	}

	/**
	 * This method is based on findEventTime(String timeString).
	 * .WTCH2 file line of times: WW 865 TORNADO FL GA CW 100825Z - 101700Z
	 * 
	 * @param timeString
	 * @return
	 */
	public static Calendar convertStringToCalendar(String timeString) {
//		System.out.println("=========== method convertStringToCalendar(...), input timeString="+timeString); 
		final String zeroTimePattern = "000000T0000";
		Calendar calendar = null;
		
		if ( ! timeString.equals(zeroTimePattern)) {
		
			calendar = Calendar.getInstance();
			
			int day = Integer.parseInt(timeString.substring(0, 2).trim());
			int hour = Integer.parseInt(timeString.substring(2, 4).trim());
			int minute = Integer.parseInt(timeString.substring(4, 6).trim());
			
			calendar.set(Calendar.DAY_OF_MONTH, day);
			calendar.set(Calendar.HOUR_OF_DAY, hour);
			calendar.set(Calendar.MINUTE, minute);
			calendar.set(Calendar.SECOND, 0);
			calendar.set(Calendar.MILLISECOND, 0);
		}

		return calendar;
		
	}
}
