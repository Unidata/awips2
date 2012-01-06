package gov.noaa.nws.ncep.viz.rsc.ffa.util;

public class UGCUtil {

	public static int getUgcIndicator(String ugcString) {
		int ugcIndicator = FFAConstant.UGC_PATTERN_UNKNOWN; 
		if(!StringUtil.isStringEmpty(ugcString) && ugcString.length() == 6) {
			String thirdCharacter = ugcString.substring(2, 3); 
			String lastFourCharacters = ugcString.substring(2); 

			if(isGreatLake(ugcString))
				ugcIndicator = FFAConstant.UGC_GREAT_LAKE_INDICATOR; 
			else if(isUgcNewPattern(thirdCharacter))
				ugcIndicator = FFAConstant.UGC_NEW_PATTERN_INDICATOR;  
			else if(isUgcAllStateZonePattern(lastFourCharacters))
				ugcIndicator = FFAConstant.UGC_ALL_STATE_ZONE_INDICATOR; 
			else if(isUgcZonePattern(thirdCharacter))
				ugcIndicator = FFAConstant.UGC_ZONE_INDICATOR; 
			else if(isUgcCountyPattern(thirdCharacter))
				ugcIndicator = FFAConstant.UGC_COUNTY_INDICATOR; 
		}
		
		return ugcIndicator; 
	}
	
	private static boolean isGreatLake(String ugcString) {
		boolean isGreatLake = false; 
		for(String eachLakePattern : FFAConstant.UGC_GREAT_LAKES_STRING_PATTERN_ARRAY) {
			if(eachLakePattern.equalsIgnoreCase(ugcString)) {
				isGreatLake = true;
				break; 
			}
		}
		return isGreatLake; 
	}
	
	private static boolean isUgcNewPattern(String thirdCharacter) {
		boolean isUgcNewPattern = false;
//		if(FFAConstant.UGC_COUNTY_PATTERN.equalsIgnoreCase(thirdCharacter)) 
		if(StringUtil.isNumber(thirdCharacter)) 
			isUgcNewPattern = true; 
		return isUgcNewPattern; 
	}

	private static boolean isUgcCountyPattern(String thirdCharacter) {
		boolean isUgcCountyPattern = false;
		if(FFAConstant.UGC_COUNTY_PATTERN.equalsIgnoreCase(thirdCharacter)) 
			isUgcCountyPattern = true; 
		return isUgcCountyPattern; 
	}

	private static boolean isUgcAllStateZonePattern(String lastFourCharacters) {
		boolean isUgcAllStateZonePattern = false;
		if(FFAConstant.UGC_ALL_STATE_ZONE_PATTERN.equalsIgnoreCase(lastFourCharacters)) 
			isUgcAllStateZonePattern = true; 
		return isUgcAllStateZonePattern; 
	}

	private static boolean isUgcZonePattern(String thirdCharacter) {
		boolean isUgcZonePattern = false;
		if(FFAConstant.UGC_ZONE_PATTERN.equalsIgnoreCase(thirdCharacter)) 
			isUgcZonePattern = true; 
		return isUgcZonePattern; 
	}

}
