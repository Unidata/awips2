package gov.noaa.nws.ncep.viz.rsc.wtch.util;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;

public class WtchUtil {
	public static boolean isWtchRecord(AwwRecord awwRecord) {
		boolean result = false; 
		String reportType = awwRecord.getReportType(); 
		if(awwRecord != null && reportType != null) {
			/*
			 * This IF condition may be not necessary if a constrain condition added in WTCH.xml
			 */
			if(WtchConstant.SEVERE_WEATHER_TORNADO_WATCH.equalsIgnoreCase(reportType) 
					|| WtchConstant.SEVERE_WEATHER_THUNDERSTORM_WATCH.equalsIgnoreCase(reportType) 
					|| WtchConstant.SEVERE_WEATHER_STATUS_REPORT.equalsIgnoreCase(reportType))  
				result = true; 
		}
		return result; 
	}


}
