package gov.noaa.nws.ncep.viz.localization;

public class StringUtil {
	public static boolean isStringEmpty(String str) {
		boolean result = false; 
		if(str == null || str.trim().length() == 0)
			result = true; 
		return result; 
	}
	
	public static boolean isSubStringPatternIncluded(String parentString, String subStringPattern) {
		boolean checkResult = false; 
		if(parentString != null && subStringPattern != null) {
			if(parentString.indexOf(subStringPattern) > -1)
				checkResult = true; 
		}
		return checkResult;
	}

}
