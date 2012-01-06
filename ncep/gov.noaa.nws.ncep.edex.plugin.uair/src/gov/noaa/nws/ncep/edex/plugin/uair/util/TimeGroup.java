/**
 * TimeGroup
 * 
 * This java class intends to serve as an utility only for UAIR decoder.
 * 
 * HISTORY
 *
 * * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2010      210				L. Lin     	Initial coding
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.uair.util;

import gov.noaa.nws.ncep.edex.util.UtilN;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Calendar;

public class TimeGroup {
	
	private static String obsUTC;
	
	private static String UTC;
	
	private static int iutc;
	
	private static Calendar observationTime;
	
	private static Calendar synopticTime;
	
	private static int topwind;
	
	private static Boolean windKnot;

	public static String getUTC() {
		return UTC;
	}

	public static void setUTC(String utc) {
		UTC = utc;
	}

	public static int getIutc() {
		return iutc;
	}

	public static void setIutc(int iutc) {
		TimeGroup.iutc = iutc;
	}

	public static String getObsUTC() {
		return obsUTC;
	}

	public static void setObsUTC(String obsUTC) {
		TimeGroup.obsUTC = obsUTC;
	}

	public static Calendar getObservationTime() {
		return observationTime;
	}

	public static void setObservationTime(Calendar observationTime) {
		TimeGroup.observationTime = observationTime;
	}

	public static Calendar getSynopticTime() {
		return synopticTime;
	}

	public static void setSynopticTime(Calendar synopticTime) {
		TimeGroup.synopticTime = synopticTime;
	}

	public static int getTopwind() {
		return topwind;
	}

	public static void setTopwind(int topwind) {
		TimeGroup.topwind = topwind;
	}

	public static Boolean getWindKnot() {
		return windKnot;
	}

	public static void setWindKnot(Boolean windKnot) {
		TimeGroup.windKnot = windKnot;
	}

	/**
	 * Docodes time group data from a given report.
	 * Get the UTC with format "YYGGI" into a Calendar object.
	 * YY indicates the day of the month; when YY>50, day=YY-50.
	 * GG indecates the hour of the sounding in UTC
	 * 
	 * @param theReport The input upper air data report
	 * @return 
	 */
	public static void TimeField(String report) {
		
		obsUTC = null;
		UTC = null;
		iutc = IDecoderConstantsN.INTEGER_MISSING;
		topwind = IDecoderConstantsN.INTEGER_MISSING;
		windKnot = false;

		int day = IDecoderConstantsN.INTEGER_MISSING;
		
		/* Regular expression for UTC */
    	final String utcpat  = "(TT|PP)(AA|BB|CC|DD) ( )?(\\d{2})(\\d{2})(\\d{1}|/)";
    	Pattern UTCPattern = Pattern.compile(utcpat);
    	Matcher utcMatcher = UTCPattern.matcher(report);

    	final String STATIONNUMBER2  = "(TT|PP)(AA|BB|CC|DD) (\\d{5}) NIL";
    	Pattern stationNumberPattern2 = Pattern.compile(STATIONNUMBER2);
    	Matcher stationNumberMatcher2 = stationNumberPattern2.matcher(report);
    	if ( stationNumberMatcher2.find()) {
    		//System.out.println("NIL report NO UTC");
    	} else if ( utcMatcher.find()) {
    		String topwindIndicator = utcMatcher.group(6);
    		if ( topwindIndicator.equals("/") ) {
    			topwind = IDecoderConstantsN.INTEGER_MISSING;
    		} else {
    			topwind = Integer.parseInt(topwindIndicator);
    		}
    		obsUTC = utcMatcher.group(5);
    		day = Integer.parseInt(utcMatcher.group(4));
    		//indicator = Integer.parseInt(observationTimeMatcher.group(6));
    		if ( day > 50 ) {
    			day = day -50;
    			windKnot = true;
    		}	
    		// Adjusts the time to the nearest 3-hourly interval.
    		int hour = Integer.parseInt(obsUTC);
    		int idiff = hour % 3;
    		if ( idiff == 1 ) {
    			hour = hour - 1;
    		} else if ( idiff == 2 ) {
    			/*
    			 * If hour is one hour early, add hour and
    			 * change day if hour
    			 */
    			hour = hour + 1;
    			if ( hour == 24 ) {
    				hour = 0;
    			} 
    		}
    		iutc = hour;
    		UTC = Integer.toString(hour);
    		if ( hour < 10 ) {
    			UTC = "0" + UTC;
    		}

    		// Get observation time
    		if ( obsUTC != null && day > 0 && day <= 31 ) {
    			String oday = Integer.toString(day);
    			if ( day < 10 ) {
    				oday = "0" + oday;
    			}
    			String obsDate = oday + obsUTC + "00";
    			Calendar cal = null;
    			observationTime = UtilN.findDataTime(obsDate,cal);
    		}

    		// Get synoptic time
    		if ( UTC != null && day > 0 && day <= 31 ) {
    			String sday = Integer.toString(day);
    			if ( day < 10 ) {
    				sday = "0" + sday;
    			}
    			String synopticDate = sday + UTC + "00";
    			Calendar cal = null;
    			synopticTime = UtilN.findDataTime(synopticDate,cal);

    			if ( obsUTC.equals("23") ) {
    				// Adjust day to next day
    				synopticTime.set(Calendar.DAY_OF_MONTH, day + 1 );
    			}
    		}
    	} 
	}

}
