package gov.noaa.nws.ncep.metparameters.dbquery.util;

public interface ClimateDataDbAccess {
	double getTDYF(String stationId, String monthString, String monthDayString); 
	double getTNTF(String stationId, String monthString, String monthDayString); 
	double getPPNT(String stationId, String monthString, String monthDayString); 
	double getPP24(String stationId, String monthString, String monthDayString); 
}
