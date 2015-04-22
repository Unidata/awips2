/*
 * gov.noaa.nws.ncep.common.staticDataProvider.BoundsUtil
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.staticdataprovider;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Utility class for the static data provider
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?			B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class BoundsUtil {
	
	/**
	 * 
	  * Convert center location String in format "(lat,lon)" to Coordinate
	*/
	static public Coordinate parseCtrloc( String ctrloc ){
		try {
			//remove "(" and ")"
			String locStr = ctrloc.substring(1, ctrloc.length()-1);
			int idx = locStr.indexOf(",");
			double lat = Double.parseDouble(locStr.substring(0, idx));
			double lon = Double.parseDouble(locStr.substring(idx+1));
			return new Coordinate( lon, lat);
			
		}
		catch (Exception e ){
			return null;
		}
	}
}
