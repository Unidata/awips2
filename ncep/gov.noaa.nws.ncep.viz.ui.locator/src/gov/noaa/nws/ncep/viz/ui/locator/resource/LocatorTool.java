package gov.noaa.nws.ncep.viz.ui.locator.resource;

import java.text.DecimalFormat;
import java.text.NumberFormat;

import com.vividsolutions.jts.geom.Coordinate;


public class LocatorTool {
		
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
	
    public static final String ROUNDING_OPTIONS[] = {"1", "5", "10"};
    
    public static final String DISTANCEUNIT_OPTIONS[] = {"omit", "NM", "SM", "KM"};
    
    public static final String DIRECTIONUNIT_OPTIONS[] = {"omit", "16 point", "degrees"};
    
    public static final String STATIONDISPLAY_OPTIONS[] = {"name", "ID"};
    
    public static final String LATLONUNIT_OPTIONS[] = {"degrees", "decimal/minutes"};
    
    public static final double METERS_PER_NM = 1852.0;
    
    public static final double METERS_PER_SM = 1609.344;
    
    public static final double METERS_PER_KM = 1000.0;
    

    public static String formatCoordinate(Coordinate theLatLon, String unit) {
		String label = null;
        
        if (unit == null || unit.equalsIgnoreCase(LATLONUNIT_OPTIONS[0])) {
        	NumberFormat nf = DecimalFormat.getInstance();
            nf.setMinimumFractionDigits(2);
            nf.setMaximumFractionDigits(2);
            
        	label = nf.format(theLatLon.y) + ", " + nf.format(theLatLon.x);
        }
        else {
        	NumberFormat intFormat = NumberFormat.getInstance();
            intFormat.setMinimumIntegerDigits(2);
            intFormat.setMaximumIntegerDigits(2);
            
        	double x = Math.abs(theLatLon.x - (int)theLatLon.x);
        	double y = Math.abs(theLatLon.y - (int)theLatLon.y);
        	
        	x = x * 60;
        	y = y * 60;
        	
        	label = (int)theLatLon.y  + ":" + intFormat.format((int)y) +
        	 ", " + (int)theLatLon.x  + ":" + intFormat.format((int)x);
        }

        return label;
	}
	
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
		if (unit == null || unit.equals(DISTANCEUNIT_OPTIONS[0])) return "";
		if (rounding <= 0) rounding = Integer.valueOf(ROUNDING_OPTIONS[0]);
		
		double factor = METERS_PER_SM;
		if (unit.equalsIgnoreCase(DISTANCEUNIT_OPTIONS[1])) {
			factor = METERS_PER_NM;
		}
		else if (unit.equalsIgnoreCase(DISTANCEUNIT_OPTIONS[3])) {
			factor = METERS_PER_KM;
		}
			
		return String.valueOf((int)(meters / factor / rounding ) * rounding);
	}

	/**
	 * Convert direction in degree to request unit
	 * 
	 * @param degrees -- input direction in degree
	 * @param unit    -- see DIRECTIONUNIT_OPTIONS
	 * @return
	 */
	public static String directionDisplay(double degrees, String unit) {
		if (unit == null || unit.equalsIgnoreCase(DIRECTIONUNIT_OPTIONS[0])) {
			return "";
		}
		else if (unit.equalsIgnoreCase(DIRECTIONUNIT_OPTIONS[1])) {
			return ConvertTO16PointDir(degrees);
		}
		else {
			return String.valueOf((int)degrees);
		}
	}
}
