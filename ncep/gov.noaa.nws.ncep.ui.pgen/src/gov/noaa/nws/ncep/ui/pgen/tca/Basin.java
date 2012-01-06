/*
 * Basin
 * 
 * Date created 23 OCTOBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

/**
 * Class contains static methods that convert different representations of Tropical Cyclone basins.
 * @author sgilbert
 *
 */
public class Basin {

	private static String ATLANTIC = "Atlantic";
	private static String EAST_PACIFIC = "E. Pacific";
	private static String CENTRAL_PACIFIC = "C. Pacific";
	private static String WEST_PACIFIC = "W. Pacific";

	/**
	 * Returns a two character abbreviation for the given Basin.
	 * @param basin tropical cyclone basin name
	 * @return basin abbreviation
	 */
	public static String getBasinAbbrev(String basin) {
		
		String abbrev = null;
		
		if ( basin.equals(ATLANTIC) ) {
			abbrev = new String("al");
		}
		else if ( basin.equals(EAST_PACIFIC) ) {
			abbrev = new String("ep");
		}
		else if ( basin.equals(CENTRAL_PACIFIC) ) {
			abbrev = new String("cp");
		}
		else if ( basin.equals(WEST_PACIFIC) ) {
			abbrev = new String("wp");
		}
		else {
			abbrev = new String("xx");
		}
		
		return abbrev;
	}

	/**
	 * Returns a integer for a given tropical cyclone basin.
	 * This number is used in the Tropical Cylcone VTEC (TCV) message
	 * @param basin tropical cyclone basin name
	 * @return basin number
	 */
	public static int getBasinNumber(String basin) {
		
		int num=0;
		
		if ( basin.equals(ATLANTIC) || basin.equalsIgnoreCase("al") ) {
			num = 1;
		}
		else if ( basin.equals(EAST_PACIFIC) || basin.equalsIgnoreCase("ep") ) {
			num = 2;
		}
		else if ( basin.equals(CENTRAL_PACIFIC) || basin.equalsIgnoreCase("cp") ) {
			num = 3;
		}
		else if ( basin.equals(WEST_PACIFIC) || basin.equalsIgnoreCase("wp") ) {
			num = 4;
		}
		
		return num;
	}

}
