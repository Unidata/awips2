package gov.noaa.nws.ncep.viz.localization;

import java.io.File;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import gov.noaa.nws.ncep.viz.localization.impl.LocalizationConstants;

public class LocalizationUtil {
	public static int translateLocalizationLevel(String localizationLevelStringValue) {
		int localizationLevel = LocalizationConstants.DEFAULT; 
		if(StringUtil.isStringEmpty(localizationLevelStringValue))
			return localizationLevel; 
		String[] levelStringArray = localizationLevelStringValue.split("\\|"); 
		int index = 1; 
		for(String eachLevelStringValue : levelStringArray) {
			int eachLevel = getLevel(eachLevelStringValue); 
			localizationLevel = localizationLevel | eachLevel; 
			index++; 
		}

		return localizationLevel; 
	}

	public static int getLevel(String levelStringValue) {
		int level = LocalizationConstants.BASE; 
		if("site".equalsIgnoreCase(levelStringValue)) 
			level = LocalizationConstants.SITE; 
		else if("desk".equalsIgnoreCase(levelStringValue)) 
			level = LocalizationConstants.DESK;
		else if("user".equalsIgnoreCase(levelStringValue)) 
			level = LocalizationConstants.USER; 
		else if("user_site".equalsIgnoreCase(levelStringValue)) 
			level = LocalizationConstants.USER_SITE; 
		else if("user_site_desk".equalsIgnoreCase(levelStringValue)) 
			level = LocalizationConstants.USER_SITE_DESK; 
		else if("local".equalsIgnoreCase(levelStringValue)) 
			level = LocalizationConstants.LOCAL; 
		return level; 
	}

	public static int translateNamePatternMatchStyle(String namePatternMatchStyleStringValue) {
		int matchStyle = LocalizationConstants.EXACT_MATCH; 
		if(!StringUtil.isStringEmpty(namePatternMatchStyleStringValue)) {
			if(LocalizationConstants.PREFIX_MATCH_STRING_VALUE.equalsIgnoreCase(namePatternMatchStyleStringValue))
				matchStyle = LocalizationConstants.PREFIX_MATCH; 
			else if(LocalizationConstants.POSTFIX_MATCH_STRING_VALUE.equalsIgnoreCase(namePatternMatchStyleStringValue))
				matchStyle = LocalizationConstants.POSTFIX_MATCH; 
		}

		return matchStyle; 
	}

	public static boolean isResourceNameOnly(int arrayLength) {
		return arrayLength == 1; 
	}
	
	public static String[] getParsedNameAndPathValues(String nameValue) {
		String[] strArray = null; 
		int lastIndexOfFileSeparator = nameValue.lastIndexOf(File.separator); 
		if(lastIndexOfFileSeparator <= 0) {
			strArray = new String[1];
			if(lastIndexOfFileSeparator < 0)
				strArray[0] = nameValue;
			else 
				strArray[0] = nameValue.substring(0); 
		} else {
			strArray = new String[2];
			strArray[0] = nameValue.substring(lastIndexOfFileSeparator);
			
			int startIndexOfPath = 0; 
			if(nameValue.startsWith(File.separator)) {
				startIndexOfPath = 1; 
			}
			strArray[1] = nameValue.substring(startIndexOfPath, lastIndexOfFileSeparator); 
		}
		return strArray; 
	}
	
	public static String getResourcePath(String resourceLocation, String parsedPathValue) {
		if(StringUtil.isStringEmpty(resourceLocation))
			return parsedPathValue; 
		if(StringUtil.isStringEmpty(parsedPathValue)) {
			return resourceLocation; 
		} else {
			return resourceLocation + File.separator + parsedPathValue; 
		}
	}
	
	public static boolean isMatched(String resourceName, String stringPattern, int matchStyle) {
		boolean matchResult = false; 
		if(!StringUtil.isStringEmpty(resourceName) && !StringUtil.isStringEmpty(stringPattern)) {
			switch(matchStyle) {
			case LocalizationConstants.EXACT_MATCH: 
				if(resourceName.equalsIgnoreCase(stringPattern))
					matchResult = true; 
				break; 
			case LocalizationConstants.PREFIX_MATCH: 
				if(resourceName.startsWith(stringPattern))
					matchResult = true; 
				break; 
			case LocalizationConstants.POSTFIX_MATCH: 
				if(resourceName.endsWith(stringPattern))
					matchResult = true; 
				break; 
			case LocalizationConstants.CONTAIN_MATCH: 
				if(StringUtil.isSubStringPatternIncluded(resourceName, stringPattern))
					matchResult = true; 
				break; 
			default: 
				if(resourceName.equalsIgnoreCase(stringPattern))
					matchResult = true; 
			}
		}
		
		return matchResult; 
	}

	public static File[] getAvailableMultiFileInfoInArray(Map<String, File> fileInfoMap) {
		Collection<File> collection = fileInfoMap.values(); 
		File[] genericArray = convertToArray(collection); 
		if(genericArray != null)
			Arrays.sort(genericArray);
		return genericArray; 
	}
	
	private static File[] convertToArray(Collection<File> collection) {
		if(collection == null)
			return null; 
		File object = getGenericObject(collection); 
		if(object == null)
			return null; 
		
		File[] array = (File[])Array.newInstance(object.getClass(), collection.size()); 
		populateArray(array, collection); 
		return array; 
	}
	
	private static File getGenericObject(Collection<File> collection) {
		Iterator<File> itr = collection.iterator(); 
		File object = null; 
		if(itr.hasNext())
			object = itr.next(); 
		return object; 
	}
	
	private static void populateArray(File[] array, Collection<File> collection) {
		if(collection == null)
			return; 
		int arrayIndex = 0; 
		for(File currentT : collection) {
			array[arrayIndex++] = currentT; 
		}
	}
}
