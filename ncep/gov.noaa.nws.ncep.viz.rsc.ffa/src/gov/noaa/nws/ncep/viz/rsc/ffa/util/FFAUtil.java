package gov.noaa.nws.ncep.viz.rsc.ffa.util;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;

public class FFAUtil {

	public static boolean isFFARecord(AwwRecord awwRecord) {
		boolean result = false; 
		String reportType = null; 
		if(awwRecord != null && (reportType = awwRecord.getReportType()) != null) {
			/*
			 * This IF condition may be not necessary if a constrain condition added in FFA.xml
			 */
			if(FFAConstant.FLASH_FLOOD_ADVISORY.equalsIgnoreCase(reportType) 
					|| FFAConstant.FLASH_FLOOD_STATEMENT.equalsIgnoreCase(reportType)  
					|| FFAConstant.FLASH_FLOOD_WARNING.equalsIgnoreCase(reportType)
					|| FFAConstant.FLASH_FLOOD_WATCH.equalsIgnoreCase(reportType)
|| FFAConstant.FLOOD_WATCH.equalsIgnoreCase(reportType))//T456
				result = true; 
		}
		return result; 
	}

}
