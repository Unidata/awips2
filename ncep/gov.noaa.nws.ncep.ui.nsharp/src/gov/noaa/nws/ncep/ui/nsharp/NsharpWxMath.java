package gov.noaa.nws.ncep.ui.nsharp;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpWxMath
 * 
 * This java class performs the surface station locator functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/13/2012   			Chin Chen	Initial coding, port from WxMath class
 * 										using Nsharp own configured tempOffset
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpWxMath {
	public static int tempOffset = 0;
	
	public static void setTempOffset(int tempOffset) {
		NsharpWxMath.tempOffset = tempOffset;
	}

	/**
     * Convert a pressure and temperature to a skew-t x,y coordinate in
     * centimeters where 0,0 occurs at 1000 hPa and 0 degrees Celsius.
     * 
     * @param pressure
     *            The pressure in hectoPascals (millibars).
     * @param temperature
     *            The temperature in degrees Celsius.
     * @return The calculated coordinate in centimeters.
     */
    public static final Coordinate getSkewTXY(double pressure,
            double temperature) {
        temperature -= tempOffset;
        Coordinate point = new Coordinate();

        point.y = 132.182 - 44.061 * Math.log10(pressure);
        point.x = (0.54 * temperature) + (0.90692 * point.y);

        return point;
    }

    /**
     * Reverse a skewT coordinate (in centimeters) to the corresponding
     * temperature and pressure.
     * 
     * @param point
     * @return The temperature and pressure. coordinate.x = temperature in
     *         Celsius, coordinate.y = the pressure in hectoPascals (millibars).
     */
    public static final Coordinate reverseSkewTXY(Coordinate point) {
        Coordinate tempPressure = new Coordinate();
        tempPressure.y = Math.pow(10, ((point.y - 132.182) / -44.061));
        tempPressure.x = (point.x - (0.90692 * point.y)) / 0.54;

        tempPressure.x += tempOffset;
        
        return tempPressure;
    }
    
    /*
     * Get pressure Y coordinate from available temp and temp's X coordinate
     */
    public static double getPressureYFromTemp(double temp, double tempX) {
    	double pressureY;
    	temp -= tempOffset;  //TT605593
    	pressureY = (tempX - (temp * 0.54))/0.90692;
    	return pressureY;
    }
}
