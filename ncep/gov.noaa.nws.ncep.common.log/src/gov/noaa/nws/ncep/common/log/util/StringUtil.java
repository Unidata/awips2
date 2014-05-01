package gov.noaa.nws.ncep.common.log.util;

public class StringUtil {

	public static boolean isStringEmpty(String str) {
		boolean isEmpty = false; 
		if(str == null || str.trim().length() == 0)
			isEmpty = true; 
		return isEmpty; 
	}
	
	public static boolean isSecondStringSubStringOfFirstString(String firstStr, String secondStr) {
		boolean isSecondStringSubStringOfFirstString = false; 
		if(!isStringEmpty(firstStr)) {
			int index = firstStr.indexOf(secondStr); 
			if(index > -1)
				isSecondStringSubStringOfFirstString = true; 
		}
		return isSecondStringSubStringOfFirstString; 
	}

	public static boolean isSecondStringStartingPortionOfFirstString(String firstStr, String secondStr) {
		boolean isSecondStringStartingPortionOfFirstString = false; 
		if(!isStringEmpty(firstStr)) {
			boolean checkResult = firstStr.startsWith(secondStr); 
			if(checkResult)
				isSecondStringStartingPortionOfFirstString = true; 
		}
		return isSecondStringStartingPortionOfFirstString; 
	}
}
