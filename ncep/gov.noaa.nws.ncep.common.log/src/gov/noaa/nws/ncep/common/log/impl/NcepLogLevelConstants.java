package gov.noaa.nws.ncep.common.log.impl;

public enum NcepLogLevelConstants {
	DEBUG  ("debug"), 
	
	WARN  ("warn"), 

	INFO  ("info"),
	
	ERROR ("error"),
	
	OFF  ("off"); 


	private String logLevelString; 
	
	private NcepLogLevelConstants(String logLevelString) {
		this.logLevelString = logLevelString; 
	}
	
	public String getLogLevelString() {
		return logLevelString; 
	}

}
