package gov.noaa.nws.ncep.common.log.logger;

import gov.noaa.nws.ncep.common.log.Activator;
import gov.noaa.nws.ncep.common.log.impl.NcepLogCategory;
import gov.noaa.nws.ncep.common.log.impl.NcepLoggerImpl;
import gov.noaa.nws.ncep.common.log.util.Log4jUtil;
import gov.noaa.nws.ncep.common.log.util.StringUtil;

import java.io.File;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Appender;
import org.apache.log4j.DailyRollingFileAppender;
import org.apache.log4j.Logger;
import org.apache.log4j.Level;
import org.apache.log4j.PatternLayout;


public class NcepLoggerManager  {
	private static final String EDEX_HOME = "edex.home";
	
	private static final String PREFIX_OF_EDEX_LOGGER_FILE_NAME = "ncep-edex-"; 

	private static final String SUFFIX_OF_LOGGER_FILE_NAME = ".log"; 

	private static final String EDEX_RUN_MODE_ENV = "EDEX_RUN_MODE"; 

	private static NcepLoggerManager ncepLoggerManager; 
	
	private Hashtable<String, DailyRollingFileAppender> dailyRollingFileAppenderHashtable; 
	
	private Hashtable<String, NcepLogger> ncepLoggerHashtable; 

	private NcepLoggerManager() {
		dailyRollingFileAppenderHashtable = new Hashtable<String, DailyRollingFileAppender>(10); 
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

	public static void setNcepLoggerLevel(String logLevelString) {
		setNcepLoggerLevel(null, logLevelString); 
	}
	
	public static void setNcepLoggerLevel(String logName, String logLevelString) {
		ncepLoggerManager.doSetNcepLoggerLevel(logName, logLevelString); 
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
	
//	private void doSetNcepLoggerLevel(String logName, String logLevelString) {
//		NcepLogCategory ncepLogCategory = getNcepLoggerCategory(); 
//		if(StringUtil.isStringEmpty(logName) || 
//				!loggerAlreadyExist(ncepLogCategory, logName)) {
//			Logger rootLog4jLogger = Logger.getRootLogger(); 
//			Level logLevelToBeSet = Log4jUtil.getLevel(logLevelString);
//			rootLog4jLogger.setLevel((Level)logLevelToBeSet); 
//		} else {
//			NcepLogger ncepLogger = getNcepLogger(ncepLogCategory, logName); 
//			ncepLogger.setLogLevel(logLevelString); 
//		}
//	}
	private void doSetNcepLoggerLevel(String logName, String logLevelString) {
		NcepLogCategory ncepLogCategory = getNcepLoggerCategory(); 
		if(StringUtil.isStringEmpty(logName)) { 
			Logger rootLog4jLogger = Logger.getRootLogger(); 
			Level logLevelToBeSet = Log4jUtil.getLevel(logLevelString);
			rootLog4jLogger.setLevel((Level)logLevelToBeSet); 
		} else {
			List<NcepLogger> ncepLoggerList = getNcepLoggerList(ncepLogCategory, logName); 
			for(NcepLogger eachNcepLogger : ncepLoggerList) {
				eachNcepLogger.setLogLevel(logLevelString); 
			}
		}
	}
	
//	private boolean loggerAlreadyExist(NcepLogCategory ncepLogCategory, String logName) {
//		boolean isLogExist = false; 
//		String logKey = getLogKeyForNcepLogger(ncepLogCategory.getCategory(), logName); 
//		NcepLogger ncepLogger = ncepLoggerHashtable.get(logKey); 
//		if(ncepLogger != null)   
//			isLogExist = true; 
//		return isLogExist; 
//	}
	
	private List<NcepLogger> getNcepLoggerList(NcepLogCategory ncepLogCategory, String logName) {
		String logKeyPrefix = getLogKeyForNcepLogger(ncepLogCategory.getCategory(), logName); 
//		System.out.println("================ logKeyPrefix = " + logKeyPrefix); 
		List<NcepLogger> ncepLoggerList = new ArrayList<NcepLogger>(); 
		Iterator<Map.Entry<String, NcepLogger>> ncepLoggerEntryItr = ncepLoggerHashtable.entrySet().iterator(); 
		while(ncepLoggerEntryItr.hasNext()) {
			Map.Entry<String, NcepLogger> entry = ncepLoggerEntryItr.next(); 
			String entrykey = entry.getKey(); 
//			System.out.println("================ Found an existing NCEP Logger with key = " + entrykey); 
			if(StringUtil.isSecondStringSubStringOfFirstString(entrykey, logKeyPrefix))  { 
//				System.out.println("================######## The entrykey matches!!!!, the entrykey = " + entrykey + "  and the logKeyPrefix = " + logKeyPrefix); 
				ncepLoggerList.add(entry.getValue()); 
			}
		}
		return ncepLoggerList; 
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
		addDailyLogFileAppender(logger, ncepLogCategory); 
		return logger; 
	}
	
	private void addDailyLogFileAppender(Logger logger, NcepLogCategory ncepLogCategory) {
		Appender loggerAppender = logger.getAppender(ncepLogCategory.getCategory()); 
		if(!isDailyRollingFileAppender(loggerAppender, ncepLogCategory.getCategory())) {
			DailyRollingFileAppender dailyFileAppender = dailyRollingFileAppenderHashtable.get(ncepLogCategory.getCategory()); 
			if(dailyFileAppender == null) {
				dailyFileAppender = createNewDailyRollingFileAppender(ncepLogCategory); 
				dailyRollingFileAppenderHashtable.put(ncepLogCategory.getCategory(), dailyFileAppender); 
			}
			logger.addAppender(dailyFileAppender); 
		}
	}
	
	private boolean isDailyRollingFileAppender(Appender loggerAppender, String logCategory) {
		boolean isDailyFileAppender = false; 
		if(loggerAppender != null && loggerAppender instanceof DailyRollingFileAppender) {
			isDailyFileAppender = true; 
			addDailyRoolingFileAppendeToMap((DailyRollingFileAppender)loggerAppender, logCategory); 
		}
		return isDailyFileAppender; 
	}
	
	private void addDailyRoolingFileAppendeToMap(DailyRollingFileAppender dailyRollingFileAppender, 
			String logCategory) {
		DailyRollingFileAppender storedDailyRollingFileAppender = dailyRollingFileAppenderHashtable.get(logCategory); 
		if(storedDailyRollingFileAppender == null)
			dailyRollingFileAppenderHashtable.put(logCategory, dailyRollingFileAppender); 
	}
	
	private DailyRollingFileAppender createNewDailyRollingFileAppender(NcepLogCategory ncepLogCategory) {
		// Create appender
		DailyRollingFileAppender dailyRollingFileAppender = new DailyRollingFileAppender();
		
		dailyRollingFileAppender.setLayout(getDefaultPatternLayout());

		dailyRollingFileAppender.setAppend(true); 
		dailyRollingFileAppender.setDatePattern("'.'yyyy-MM-dd"); 

		String logPath = getLogFilePath(ncepLogCategory); 
		dailyRollingFileAppender.setFile(logPath);
		
		dailyRollingFileAppender.activateOptions(); // It didn't work without this
		
		return dailyRollingFileAppender; 
	}
	
	private PatternLayout getDefaultPatternLayout() {
		// Define layout
		PatternLayout layout = new PatternLayout();
//		layout.setConversionPattern("%d %-5p [%t] %-17c{2} (%13F:%L) %3x - %m%n");
//		layout.setConversionPattern("%d %-5p [%t] %-17c (%13F:%L) %x - %m%n");
		layout.setConversionPattern("%d %-5p [%t] %-17c %x - %m%n");
		return layout; 
	}
	
	private String getLogFilePath(NcepLogCategory ncepLogCategory) {
		String logFilePathPrefix = null; 
		String logFileName = null; 
		if(isLogForEdex()) {
			logFilePathPrefix = getEdexHomeDir();
			String edexInstanceMode = getEdexInstanceMode(); 
			logFileName = PREFIX_OF_EDEX_LOGGER_FILE_NAME + edexInstanceMode; 
		} else {
			logFilePathPrefix = Activator.getDefault().getCaveDataDir();  //Not EDEX log, thus it is a CAVE log
			logFileName = ncepLogCategory.getCategory(); 
		}
		String logFilePath = logFilePathPrefix + File.separator 
			+ "logs" + File.separator + logFileName 
			+ SUFFIX_OF_LOGGER_FILE_NAME; 
		return logFilePath; 
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
	
	private String getEdexHomeDir() {
		String edexHomeDir = System.getProperty(NcepLoggerManager.EDEX_HOME);
		return edexHomeDir; 
	}
	
//	private String getCurrentDateString() {
//		Calendar cal = Calendar.getInstance(); 
//		StringBuilder builder = new StringBuilder(10);
//		int month = cal.get(Calendar.MONTH); 
//		if(month == 12)
//			month = 1; 
//		else
//			month ++; 
//		String monthStr = String.valueOf(month); 
//		if(month < 10)
//			monthStr = "0" + month; 
//		builder.append(cal.get(Calendar.YEAR))
//		 	   .append("-")
//		 	   .append(monthStr)
//		 	   .append("-")
//		 	   .append(cal.get(Calendar.DATE)); 
//		return builder.toString(); 
//	}
}
