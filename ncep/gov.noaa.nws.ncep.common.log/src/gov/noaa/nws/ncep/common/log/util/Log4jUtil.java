package gov.noaa.nws.ncep.common.log.util;

import gov.noaa.nws.ncep.common.log.impl.NcepLogLevelConstants;

import org.apache.log4j.Level;

public class Log4jUtil {

	public static Level getLevel(String logLevelString, Level defaultLevel) {
		Level logLevel = defaultLevel; 
		String levelString = logLevelString.trim(); 
		if(levelString.equalsIgnoreCase(NcepLogLevelConstants.DEBUG.getLogLevelString()))
			logLevel = Level.DEBUG; 
		else if(levelString.equalsIgnoreCase(NcepLogLevelConstants.WARN.getLogLevelString()))
			logLevel = Level.WARN; 
		else if(levelString.equalsIgnoreCase(NcepLogLevelConstants.INFO.getLogLevelString()))
			logLevel = Level.INFO; 
		else if(levelString.equalsIgnoreCase(NcepLogLevelConstants.ERROR.getLogLevelString()))
			logLevel = Level.ERROR; 
		else if(levelString.equalsIgnoreCase(NcepLogLevelConstants.OFF.getLogLevelString()))
			logLevel = Level.OFF; 
		return logLevel; 
	}
	
	public static Level getLevel(String logLevelString) {
		Level logLevel = getLevel(logLevelString, Level.ERROR); 
		return logLevel; 
	}

}
