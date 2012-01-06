package gov.noaa.nws.ncep.viz.tools.customProjection;

public class StringUtil {

	public static boolean isStringEmpty(String str) {
		boolean valid = true; 
		if(str != null && str.trim().length() != 0) 
			valid = false; 
		return valid; 
	}

}
