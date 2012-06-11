package gov.noaa.nws.ncep.metparameters.dbquery.util;

//import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
//import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

//import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
//import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
//import com.raytheon.uf.viz.core.exception.VizException;

public class ClimateDataDbAccessImpl implements ClimateDataDbAccess {

//	private NcepLogger logger = NcepLoggerManager.getNcepLogger(this.getClass()); 

	@Override
	public double getTDYF(String stationId, String monthString, String monthDayString) {
		String queryString = createClimateDataQueryForTDYF(stationId, monthString, monthDayString);
		double tdyfValue = retrieveValue(queryString); 
//		logger.info("=== Now input stationid="+stationId); 
//		logger.info("=== Now retrieved tdyf="+tdyfValue); 
		return tdyfValue;
	}

	@Override
	public double getTNTF(String stationId, String monthString, String monthDayString) {
		String queryString = createClimateDataQueryForTNTF(stationId, monthString, monthDayString);
		double tntfValue = retrieveValue(queryString); 
//		logger.info("=== Now input stationid="+stationId); 
//		logger.info("=== Now retrieved tntf="+tntfValue); 
		return tntfValue;
	}

	@Override
	public double getPPNT(String stationId, String monthString, String monthDayString) {
		String queryString = createClimateDataQueryForPPNT(stationId, monthString, monthDayString);
		double ppntValue = retrieveValue(queryString); 
//		logger.info("=== Now input stationid="+stationId); 
//		logger.info("=== Now retrieved ppnt="+ppntValue); 
		return ppntValue;
	}

	@Override
	public double getPP24(String stationId, String monthString, String monthDayString) {
		String queryString = createClimateDataQueryForPP24(stationId, monthString, monthDayString);
		double ppdyValue = retrieveValue(queryString); 
//		logger.info("=== Now input stationid="+stationId); 
//		logger.info("=== Now retrieved ppdy="+ppdyValue); 
		return ppdyValue;
	}


	private double retrieveValue(String queryString) {
		List<Object[]> objectArrayList = new ArrayList<Object[]>(); 
		try {
//			objectArrayList = DirectDbQuery.executeQuery(queryString, "ncep", QueryLanguage.SQL);
		} catch (Exception e1) {
//			logger.error("VizException is thrown when trying to do DirectDbQuery.executeQuery to query stns.climo_data table, error="+e1.getMessage()); 
			e1.printStackTrace();
		}
		double retrievedValue = 0.0; 
		if(!objectArrayList.isEmpty()) {
			Object[] objectArray = objectArrayList.get(0); 
			
			String retrievedStationId = (String)objectArray[0]; 
//			logger.info("=== Now retrieved stationid="+retrievedStationId); 
			BigDecimal retrievedValueInBigDecimal = (BigDecimal)objectArray[1]; 
			retrievedValue = retrievedValueInBigDecimal.doubleValue(); 
		}
		
		return retrievedValue; 
	}
	
	private String createClimateDataQueryForTDYF(String stationId, String monthString, String monthDayString) {
		StringBuilder queryBuilder = new StringBuilder(
			"select station_id, tdyf from stns.climo_data where station_id ='");
		queryBuilder.append(stationId)
					.append("' and ")
					.append("month = '")
					.append(monthString)
					.append("' and day = '")
					.append(monthDayString)
					.append("'");
		String queryString = queryBuilder.toString(); 
//		System.out.println("climate data query string ="+queryString); 
		return queryString; 
	}

	private String createClimateDataQueryForTNTF(String stationId, String monthString, String monthDayString) {
		StringBuilder queryBuilder = new StringBuilder(
			"select station_id, tntf from stns.climo_data where station_id ='");
		queryBuilder.append(stationId)
					.append("' and ")
					.append("month = '")
					.append(monthString)
					.append("' and day = '")
					.append(monthDayString)
					.append("'");
		String queryString = queryBuilder.toString(); 
//		System.out.println("climate data query string ="+queryString); 
		return queryString; 
	}

	private String createClimateDataQueryForPPNT(String stationId, String monthString, String monthDayString) {
		StringBuilder queryBuilder = new StringBuilder(
			"select station_id, ppnt from stns.climo_data where station_id ='");
		queryBuilder.append(stationId)
					.append("' and ")
					.append("month = '")
					.append(monthString)
					.append("' and day = '")
					.append(monthDayString)
					.append("'");
		String queryString = queryBuilder.toString(); 
//		System.out.println("climate data query string ="+queryString); 
		return queryString; 
	}

	private String createClimateDataQueryForPP24(String stationId, String monthString, String monthDayString) {
		StringBuilder queryBuilder = new StringBuilder(
			"select station_id, pp24 from stns.climo_data where station_id ='");
		queryBuilder.append(stationId)
					.append("' and ")
					.append("month = '")
					.append(monthString)
					.append("' and day = '")
					.append(monthDayString)
					.append("'");
		String queryString = queryBuilder.toString(); 
//		System.out.println("climate data query string ="+queryString); 
		return queryString; 
	}

}
