/*
 * IdftUtil
 * 
 * This java class intends to serve as an utility only for AWW decoder.
 * 
 * <pre>
 * 
 * HISTORY
 *
 * Date     	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01Jun2009	   100		F. Yen		Initial creation
 * 27May2010	   100		F. Yen		Migrated for to11dr3 to to11dr11	
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.edex.plugin.idft.util;

import gov.noaa.nws.ncep.common.dataplugin.idft.IdftRecord;
import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IdftUtil {	
	/**
	 * processValidTime: Process the "HR FORECAST VT" line returning
	 * the Calendar valTime
	 */
	public static Calendar processValidTime(String valTimeString,
				IdftRecord record) {
		/*
		 * Regular expression for the VT string (having the valid time)
		 */
		final String VT_EXP = "(HR FORECAST VT (\\d{2})/(\\d{2})/"
					+ "(\\d{2}) (\\d{2})(\\d{2}) UTC)";
		final Pattern vTPattern = Pattern.compile(VT_EXP);
		
		Calendar valTime = Calendar.getInstance();
		Matcher vTMatcher = vTPattern.matcher(valTimeString);
		if (vTMatcher.find()) {
			valTime.set(Calendar.YEAR, Integer.parseInt(vTMatcher
						.group(4)) + 2000);
			valTime.set(Calendar.MONTH, Integer.parseInt(vTMatcher.group(2)) - 1);
			valTime.set(Calendar.DAY_OF_MONTH, Integer
						.parseInt(vTMatcher.group(3)));
			valTime.set(Calendar.HOUR_OF_DAY, Integer
						.parseInt(vTMatcher.group(5)));
			valTime.set(Calendar.MINUTE, Integer.parseInt(vTMatcher
						.group(6)));
			valTime.set(Calendar.SECOND, 0);
			valTime.set(Calendar.MILLISECOND, 0);
		}
		return valTime;	
    }
}
