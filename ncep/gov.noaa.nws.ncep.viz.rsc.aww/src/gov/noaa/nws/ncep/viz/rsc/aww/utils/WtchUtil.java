package gov.noaa.nws.ncep.viz.rsc.aww.utils;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord.AwwReportType;

public class WtchUtil {
	public static boolean isWtchRecord(AwwRecord awwRecord) {

		if( awwRecord == null ) {
			return false;
		}
			/*
			 * This IF condition may be not necessary if a constrain condition added in WTCH.xml
			 */
		AwwReportType rt = AwwReportType.getReportType( awwRecord.getReportType() );

//			if(WtchConstant.SEVERE_WEATHER_TORNADO_WATCH.equalsIgnoreCase(reportType) 
//					|| WtchConstant.SEVERE_WEATHER_THUNDERSTORM_WATCH.equalsIgnoreCase(reportType) 
//					|| WtchConstant.SEVERE_WEATHER_STATUS_REPORT.equalsIgnoreCase(reportType))  
//				result = true;
		
		return ( rt == AwwReportType.TORNADO_REPORT   || 
				 rt == AwwReportType.SEVERE_THUNDERSTORM_WATCH || //???
				 rt == AwwReportType.STATUS_REPORT );  //???
	}


}
