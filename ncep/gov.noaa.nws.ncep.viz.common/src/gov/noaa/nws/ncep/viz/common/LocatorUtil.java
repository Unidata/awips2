package gov.noaa.nws.ncep.viz.common;

import java.text.DecimalFormat;
import java.text.NumberFormat;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.vividsolutions.jts.geom.Coordinate;


public class LocatorUtil {
		
/**
 * Format locator displays.
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/2008  	22    		M. Li      	Initial Creation
 * 01/2009		48			M. Li 		Add ConvertTO16PointDir
 * 02/2009		64		  	M. Li		Add distanceDisplay & directionDisplay
 *                       
 * </pre>
 * 
 * @author M. Li
 * @version 1
 */
    public static final double METERS_PER_NM = 1852.0;
    
    public static final double METERS_PER_SM = 1609.344;
    
    public static final double METERS_PER_KM = 1000.0;
    

	
	/**
	 * Convert a direction (degrees from N) to 16-point compass 
	 * direction (N, NNE, NE, ENE, etc.)
	 * 
	 * @param angle
	 * @return
	 */
	public static String ConvertTO16PointDir(double angle) {
		String[] dirs = {"N","NNE","NE","ENE","E","ESE","SE","SSE",
				"S","SSW","SW","WSW","W","WNW","NW","NNW","N"};
		
		if ( angle < 0.0 || angle > 360.0 ) {
			return null;
		}
		
		int idir = (int)Math.rint(angle/22.5);
		return dirs[idir];
	}

	/**
	 * Convert distance in meter to request unit
	 * 
	 * @param meters   -- distance in meter
	 * @param rounding -- see ROUNDING_OPTIONS
	 * @param unit     -- see DISTANCEUNIT_OPTIONS
	 * @return
	 */
	public static String distanceDisplay(double meters, int rounding, String unit) {
		if (unit == null ) {// || unit.equals(DISTANCEUNIT_OPTIONS[0])) 
			return "";
		}
		else if( rounding <= 0 ) {
			rounding = 1; //Integer.valueOf(ROUNDING_OPTIONS[0]);
		}
		
		
		double factor = 1.0;

		if (unit.equalsIgnoreCase( "NM" ) ) { // NonSI.NAUTICAL_MILE.toString().toUpperCase() )) {
			factor = METERS_PER_NM;
		}
		else if (unit.equalsIgnoreCase( "SM" ) ) {
			factor = METERS_PER_SM;
		}
		else if (unit.equalsIgnoreCase( SI.KILOMETER.toString() ) ) {
			factor = METERS_PER_KM;
		}
		else {
			unit = "m";			
		}
			
		double dist = meters/factor;
		double round = (double)rounding;
		dist = (dist+round/2)/round*round;  
		return String.valueOf((int)dist) +" "+ unit;
	}

	/**
	 * Convert direction in degree to request unit
	 * 
	 * @param degrees -- input direction in degree
	 * @param unit    -- see DIRECTIONUNIT_OPTIONS
	 * @return
	 */
	public static String directionDisplay(double degrees, String unit) {
		if (unit == null ) { //|| unit.equalsIgnoreCase(DIRECTIONUNIT_OPTIONS[0])) {
			return "";
		}
		else if( unit.equalsIgnoreCase( "16 point") ||
				 unit.equalsIgnoreCase("compass") ) {
			return ConvertTO16PointDir(degrees);
		}
		else {
			return String.valueOf((int)degrees) + NonSI.DEGREE_ANGLE.toString();
		}
	}
}
