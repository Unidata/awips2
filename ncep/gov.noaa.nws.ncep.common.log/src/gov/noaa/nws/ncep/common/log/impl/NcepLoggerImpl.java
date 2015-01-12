package gov.noaa.nws.ncep.common.log.impl;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.util.Log4jUtil;

public class NcepLoggerImpl implements NcepLogger {

	private Logger log4jLogger; 
	
	public NcepLoggerImpl(Logger _log4jLogger) {
		this.log4jLogger = _log4jLogger; 
	}
	
	@Override
	public void debug(Object message) {
		log4jLogger.debug(message); 
	}

	@Override
	public void debug(Object message, Throwable t) {
		log4jLogger.debug(message, t); 
	}

	@Override
	public void warn(Object message) {
		log4jLogger.warn(message); 
	}

	@Override
	public void warn(Object message, Throwable t) {
		log4jLogger.warn(message, t); 
	}

	@Override
	public void info(Object message) {
		log4jLogger.info(message); 
	}

	@Override
	public void info(Object message, Throwable t) {
		log4jLogger.info(message, t); 
	}

	@Override
	public void error(Object message) {
		log4jLogger.error(message); 
	}

	@Override
	public void error(Object message, Throwable t) {
		log4jLogger.error(message, t); 
	}

}
