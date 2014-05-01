package gov.dambreak.util;

/**
 * Insert the type's description here.
 * Creation date: (7/23/2003 2:54:28 PM)
 * @author: 
 */
public class NumFormat {
	
/** 
 * Format a float as a string the given number of decimal places.
 * Creation date: (7/23/2003 2:55:12 PM)
 * @return java.lang.String
 * @param value float
 * @param numDecimals int
 */
public static String format(float value, int numDecimals) {
	java.text.DecimalFormat df;
	
	if (numDecimals == 0)
		df = new java.text.DecimalFormat("########0.");
	else if (numDecimals == 2)
		df = new java.text.DecimalFormat("######0.00");
	else if (numDecimals == 3)
		df = new java.text.DecimalFormat("#####0.000");
	else
		df = new java.text.DecimalFormat("####0.0000");

	String retVal = null;
	try {
		retVal = df.format(value);
	} catch (Exception e) {
		retVal = "NaN";
	}	
	return retVal;
}
/**
 * Insert the method's description here.
 * Creation date: (7/29/2003 10:36:23 AM)
 * @return float
 * @param str java.lang.String
 */
public static float toFloat(String str) {
	float retVal;
	
	try {
		retVal = Float.parseFloat(str);
	} catch (NumberFormatException e) {
		retVal = -999.0f;
	}
	return retVal;
}
}
