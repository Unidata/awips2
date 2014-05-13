package gov.noaa.nws.ncep.common.log.logger;

public interface NcepLogger {
	void debug(Object message); 
	void debug(Object message, Throwable t); 
	
	void warn(Object message); 
	void warn(Object message, Throwable t); 
	
	void info(Object message); 
	void info(Object message, Throwable t); 
	
	void error(Object message); 
	void error(Object message, Throwable t); 
	
}
