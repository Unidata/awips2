package gov.noaa.nws.ncep.common.log.logger;

import gov.noaa.nws.ncep.common.log.Activator;
import gov.noaa.nws.ncep.common.log.impl.NcepLogCategory;
import gov.noaa.nws.ncep.common.log.impl.NcepLoggerImpl;
import gov.noaa.nws.ncep.common.log.util.StringUtil;

import java.util.Hashtable;


import org.apache.log4j.Logger;

/**
 * @deprecated Use logback xml files to manage configurations, and use SLF4J
 * or UFStatus APIs for logging
 */
@Deprecated
public class NcepLoggerManager  {
	
	private static final String EDEX_RUN_MODE_ENV = "EDEX_RUN_MODE"; 

	private static NcepLoggerManager ncepLoggerManager; 
		
	private Hashtable<String, NcepLogger> ncepLoggerHashtable; 

	private NcepLoggerManager() {
		ncepLoggerHashtable = new Hashtable<String, NcepLogger>(); 
	}
	
	static {
		ncepLoggerManager = new NcepLoggerManager(); 
	}
	
	@SuppressWarnings("rawtypes")
	public static NcepLogger getNcepLogger(Class clazz) {
		NcepLogger ncepLogger = getNcepLogger(clazz.getName()); 
		return ncepLogger; 
	}

	public static NcepLogger getNcepLogger(String logName) {
		NcepLogger ncepLogger = ncepLoggerManager.doGetNcepLogger(logName); 
		return ncepLogger; 
	}

	
	private NcepLogger doGetNcepLogger(String logName) {
		NcepLogCategory necpLogCategory = getNcepLoggerCategory(); 
		NcepLogger ncepLogger = getNcepLogger(necpLogCategory, logName); 
		return ncepLogger; 
	}
	
	private NcepLogCategory getNcepLoggerCategory() {
		NcepLogCategory ncepLogCategory = null; 
		if(!isLogForEdex()) {
			ncepLogCategory = NcepLogCategory.NCEP_CAVE_LOGGER_CATEGORY; 
		} else {
			String edexInstanceMode = getEdexInstanceMode(); 
			if(NcepLogCategory.EDEX_REQUEST_LOGGER_CATEGORY.getCategory().equalsIgnoreCase(edexInstanceMode)) 
				ncepLogCategory = NcepLogCategory.EDEX_REQUEST_LOGGER_CATEGORY; 
			else if(NcepLogCategory.EDEX_INGEST_LOGGER_CATEGORY.getCategory().equalsIgnoreCase(edexInstanceMode))
				ncepLogCategory = NcepLogCategory.EDEX_INGEST_LOGGER_CATEGORY; 
			else if(NcepLogCategory.EDEX_INGEST_GRIB_LOGGER_CATEGORY.getCategory().equalsIgnoreCase(edexInstanceMode))
				ncepLogCategory = NcepLogCategory.EDEX_INGEST_GRIB_LOGGER_CATEGORY; 
		}
		return ncepLogCategory; 
	}		
	
	private NcepLogger getNcepLogger(NcepLogCategory logCategory, String logName) {
		String logKey = getLogKeyForNcepLogger(logCategory.getCategory(), logName); 
		NcepLogger ncepLogger = ncepLoggerHashtable.get(logKey); 
		if(ncepLogger == null)  {  
			Logger log4jLogger = getLog4jLogger(logCategory, logName); 
			ncepLogger = new NcepLoggerImpl(log4jLogger); 
			ncepLoggerHashtable.put(logKey, ncepLogger); 
		}
		return ncepLogger; 
	}
	
	private String getLogKeyForNcepLogger(String logCategory, String logName) {
		String logKey = logCategory + "_" + logName; 
		return logKey; 
	}

	private Logger getLog4jLogger(NcepLogCategory ncepLogCategory, String logName) {
		Logger logger = Logger.getLogger(logName); 
		return logger; 
	}											
	
	private String getEdexInstanceMode() {
		String edexRunMode = System.getenv(EDEX_RUN_MODE_ENV); 
		return edexRunMode; 
	}
	
	private boolean isLogForEdex() {
		boolean isEdexLog = true; 
		String edexRunMode = getEdexInstanceMode(); 
		if(StringUtil.isStringEmpty(edexRunMode))
			isEdexLog = false; 
		return isEdexLog; 
	}
	
}
