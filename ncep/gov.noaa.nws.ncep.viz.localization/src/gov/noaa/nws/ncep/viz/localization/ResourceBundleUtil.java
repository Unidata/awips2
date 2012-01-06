package gov.noaa.nws.ncep.viz.localization;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class ResourceBundleUtil {
	private static final String BUNDLE_NAME = "NcepResourceInfo"; 

	private static final ResourceBundle RESOURCE_BUNDLE =
		ResourceBundle.getBundle(BUNDLE_NAME);

	private ResourceBundleUtil() {}

	public static String[] getNcepResourceDirInfo(String parserString) {
		String ncepResourceDir = getResourceBundleValue("ncep.resource.dir"); 
		String [] rscDirStringArray = ncepResourceDir.split(parserString); 
		return rscDirStringArray; 
	}
	
	public static String getResourceBundleValue(String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} catch (MissingResourceException e) {
			return "!" + key + "!";
		}
	}

	public static ResourceBundle getResourceBundle() {
		return RESOURCE_BUNDLE; 
	}
}
