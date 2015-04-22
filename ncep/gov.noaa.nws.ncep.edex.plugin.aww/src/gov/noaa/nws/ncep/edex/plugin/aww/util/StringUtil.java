package gov.noaa.nws.ncep.edex.plugin.aww.util;

public class StringUtil {
	public static boolean isStringEmpty(String str) {
		boolean result = false; 
		if(str == null || str.trim().length() == 0)
			result = true; 
		return result; 
	}

	public static String[] parseString(String str, String parsingStringPattern) {
		String[] stringArray = str.split(parsingStringPattern); 
		return stringArray; 
	}
}
