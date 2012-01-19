/*
 * 
 * Util
 * 
 * This java class contains edex generic utility methods for use.
 *  
 * T. Lee	11/2008	Creation
 * T. Lee	 3/2009	Fixed roll-over cases; added String functions
 * T. Lee	 4/2009	Added date to the base time
 * T. Lee
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 11/2008		14			T. Lee		Creation
 *  3/2009		14			T. Lee		Fixed roll-over cases; added String functions
 *  4/2009		14			T. Lee		Added date to the base time
 *  5/2009		128			T. Lee		Used UTC in findDataTime
 * </pre>
 * 
 * @author T.Lee
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.util;

import java.util.Calendar;
import java.util.TimeZone;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import com.raytheon.edex.util.Util;

public class UtilN {
    private static final Log logger = LogFactory.getLog(UtilN.class);

	/**
	 * Constructor
	 */
	public UtilN() {
		
	}
	/**
	 * Convert a string in ddhhmm format to a standard {@link Calendar} format where
	 * ddhhmm is the GMT format while the standard time is in Calendar format with 
	 * Year and Month information.  Usage: ddhhmm is the issue time whereas utcTime
	 * can be the MDN time.  The former comes "after" the latter.
	 * 
	 * @parm ddhhmm  day-hour-minute in GMT
	 * @parm local Time UTC time in Calendar
	 */
	public static Calendar findDataTime (String ddhhmm, Calendar utcTime) {
		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT")); ;
		if ( utcTime == null ) {
			try {
				return Util.findCurrentTime(ddhhmm);
			} catch (Exception e) {
				if ( logger.isInfoEnabled()) {
				    logger.info( " Error in processing MND time; return current time ");
				}
				return cal;
			}

		} else {
			int iDay = Integer.parseInt(ddhhmm.substring(0, 2).trim());
			int iHour = Integer.parseInt(ddhhmm.substring(2, 4).trim());
			int iMinute = Integer.parseInt(ddhhmm.substring(4, 6).trim());
			int iMonth = utcTime.get(Calendar.MONTH);
			int iYear = utcTime.get(Calendar.YEAR);
			
			/*
			 *  adjust the month and year for roll-over situations
			 */
			if (iDay < utcTime.get(Calendar.DAY_OF_MONTH) ) {
				iMonth++;
				if ( iMonth == 12 ) {
					iMonth = Calendar.JANUARY;
					iYear++;
				}
			}
			cal.set(iYear,iMonth,iDay,iHour,iMinute);
			cal.set(Calendar.SECOND, 0);
			cal.set(Calendar.MILLISECOND, 0);	
			return cal;
		}	
	}

	/**
     * Remove the leading spaces and tabs in a string.
     */
    public static String removeLeadingWhiteSpaces (String str) {
    	int i;
    	for ( i = 0; i < str.length(); i++ ) {
    		if ( !Character.isWhitespace(str.charAt(i))) {
    			break;
    		}
    	}
    	return str.substring(i);
    }

    /**
     * Remove multiple white spaces in a string.
     */
    public static String removeExtraWhiteSpaces (String str) {
    	StringBuffer sb = new StringBuffer();
    	int i;
    	char first = str.charAt(0);
    	char second;
    	for ( i = 1; i < str.length(); i++ ) {
    		second = str.charAt(i);
    		if ( !Character.isWhitespace(first) || !Character.isWhitespace(second)) {
    			sb.append(first);
    			first = second;
    		}
    		if ( i == ( str.length()-1) ) {
    			sb.append(second);
    		}
    	}
    	return sb.toString();
    }
}
