/*
 * StormAdvisoryNumber
 * 
 * Date created: 15 SEPTEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

/**
 * Class contains static methods used to query various information about a given
 * tropical cyclone advisory number
 * @author sgilbert
 *
 */
public class StormAdvisoryNumber {

	/**
	 * Determines whether a given storm advisory number is valid.
	 * @param advisory number
	 * @return true, if advisory is valid
	 */
	public static boolean isValid(String advisory) {
		
		String number;
		String adv = advisory.toLowerCase();
		
		/*
		 * Intermediate advisory numbers end with "a" or "b".  There may be up to 
		 * two intermediate advisories between regular advisories. 
		 */
		if ( adv.endsWith("a") || adv.endsWith("b") ) {
			number = advisory.substring(0, advisory.length()-1);
		}
		else {
			number = advisory;
		}
		
		/*
		 * make sure advisory number is a positive integer
		 */
		try {
			if  ( Integer.parseInt(number) > 0 ) return true;
		}
		catch ( NumberFormatException nfe ) {
			return false;
		}
		
		return false;
	}

	/**
	 * Determines whether the given storm advisory number is an intermediate advisory
	 * @param advisory storm advisory number
	 * @return
	 */
	public static boolean isIntermediate(String advisory) {
		
		String adv = advisory.toLowerCase();

		/*
		 * Intermediate advisory numbers end with "a" or "b".  There may be up to 
		 * two intermediate advisories between regular advisories. 
		 */
		if ( adv.endsWith("a") || adv.endsWith("b") ) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 * Returns the regular advisory number from the given storm advisory number 
	 * @param advisory storm advisory number
	 * @return
	 */
	public static int getRegularAdvisory(String advisory) {
	
		int num;
		String number;
		String adv = advisory.toLowerCase();
		
		/*
		 * Intermediate advisory numbers end with "a" or "b".  There may be up to 
		 * two intermediate advisories between regular advisories. 
		 */
		if ( adv.endsWith("a") || adv.endsWith("b") ) {
			number = advisory.substring(0, advisory.length()-1);
		}
		else {
			number = advisory;
		}

		/*
		 * make sure advisory number is an integer
		 */
		try {
			num = Integer.parseInt(number);
		}
		catch ( NumberFormatException nfe ) {
			num = 0;
		}
		
		return num;
	}
	
}
