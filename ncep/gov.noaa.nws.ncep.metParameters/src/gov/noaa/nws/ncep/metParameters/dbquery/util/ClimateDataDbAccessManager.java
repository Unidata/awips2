package gov.noaa.nws.ncep.metparameters.dbquery.util;

public class ClimateDataDbAccessManager {
	private static ClimateDataDbAccessManager instance; 
	
	static { 
		instance = new ClimateDataDbAccessManager(); 
	}
	
	private ClimateDataDbAccess climateDataDbAccess; 
	
	private ClimateDataDbAccessManager() {
		climateDataDbAccess = new ClimateDataDbAccessImpl(); 
	}
	
	public static ClimateDataDbAccessManager getInstance() {
		return instance; 
	}
	
	public ClimateDataDbAccess getClimateDataDbAccess() {
		return climateDataDbAccess; 
	}
}
