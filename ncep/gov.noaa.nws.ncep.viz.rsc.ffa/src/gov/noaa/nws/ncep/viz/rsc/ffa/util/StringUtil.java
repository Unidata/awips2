package gov.noaa.nws.ncep.viz.rsc.ffa.util;


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
	
	public static String searchStringPattern(String stringToBeSearched, String stringPrefixPattern, 
			String stringEndmarker) {
		String result = null; 
		if(!StringUtil.isStringEmpty(stringToBeSearched) && 
				!StringUtil.isStringEmpty(stringPrefixPattern) &&
				!StringUtil.isStringEmpty(stringEndmarker)) {
			int tempIndex = stringToBeSearched.indexOf(stringPrefixPattern); 
			if(tempIndex >= 0) {
				int beginIndex = tempIndex + stringPrefixPattern.length(); 
				int endIndex = stringToBeSearched.indexOf(stringEndmarker, beginIndex); 
				if(endIndex >= 0) {
					result = stringToBeSearched.substring(beginIndex, endIndex); 
				}
			}
		}
		return result; 
	}

	public static boolean isNumber(String str) {
		boolean isNumber = false; 
		try {
			Integer.parseInt(str); 
			isNumber = true; 
		} catch(NumberFormatException nfe) {
			//do nothing
		}
		return isNumber; 
	}
	
}
