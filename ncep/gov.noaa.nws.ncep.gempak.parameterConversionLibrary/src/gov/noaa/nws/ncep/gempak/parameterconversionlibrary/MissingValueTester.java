/**
 * 
 */
package gov.noaa.nws.ncep.gempak.parameterconversionlibrary;
import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.GempakConstants;
/**
 * @author archana
 *
 */
public class MissingValueTester {

	public static boolean isDataValueMissing(float dataVal){
		return (Math.abs(dataVal - GempakConstants.RMISSD) < GempakConstants.RDIFFD  ? true : false);
	}
	
}
