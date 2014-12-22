/*
 * gov.noaa.nws.ncep.common.staticDataProvider.ContinuingWatch
 * 
 * 2012.8
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.staticdataprovider;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord.AwwReportType;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/12		#770		Q. Zhou   	Initial Creation.
 * 09/12		#770		Q. Zhou   	Clean up and change selectedWatch to a collection
 * 08/13        #1028       G. Hull     rm dependency on viz.rsc.wtch project
 * 12/14        ?           B. Yin      Remove ScriptCreator, use Thrift Client.
 * </pre>
 * 
 * @author	Q. Zhou
 */

public class ContinuingWatch {
	
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContinuingWatch.class);
    
	/*
	 * WtchRscDataObj class
	 */
	public static class WtchRscDataObj {
		String 				dataUri;       
		DataTime        	issueTime;     // issue time from bulletin
		DataTime        	eventTime;    
		String 				reportType;
		
		int 				watchNumber; 		
		String 				actionType;
		String 				officeId; 
		String 				eventTrackingNumber; 
		String 				phenomena; 
		String 				significance; 

		public DataTime getDataTime() {
			return eventTime;
		}
	}
	
	/*
	 *  Get individual fields from layers. Then compare endTime
	 *  @return string of watch number
	 */
	private static Collection<String> getAwwRecord(AwwRecord awwRecord) {
		
		Collection<String> selectedWatch = new ArrayList<String>();
    	GregorianCalendar currCal = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
    	Calendar endTime = null;
		
    	
    	if( isWtchRecord(awwRecord)) {
    		WtchRscDataObj wtchData = new WtchRscDataObj(); 
    		wtchData.issueTime = new DataTime(awwRecord.getIssueTime());
    		wtchData.reportType = awwRecord.getReportType();
    		
    		try {
    			int watchNumber = Integer.parseInt(awwRecord.getWatchNumber()); 
    			wtchData.watchNumber = watchNumber; 
    		} catch(NumberFormatException nfe) {
    			//do nothing
    		}
    		
			Set<AwwUgc> awwUgcSet = awwRecord.getAwwUGC();
			
			for (AwwUgc eachAwwUgc : awwUgcSet) {
				wtchData.eventTrackingNumber = eachAwwUgc.getEventTrackingNumber(); 
				 				
				Set<AwwVtec> awwVtecSet = eachAwwUgc.getAwwVtecLine(); 
				
				for (AwwVtec eachAwwVtec : awwVtecSet) {
					wtchData.actionType = eachAwwVtec.getAction();
					
	    			endTime = eachAwwVtec.getEventEndTime();
	    			
	    			/* if the endtime is not specified, assume the event is still active.
		               unspecified can occur if the endtime has a missing value indicator
		               or is set to 0.  A 0 value can occur because the time may be NULL in
		               the database, which is converted to a 0 value by the time conversion functions. */
	    			if (endTime == null && wtchData.watchNumber != 0)
		    			selectedWatch.add(Integer.toString(wtchData.watchNumber));
	    			
	    			if (wtchData.actionType != null && !wtchData.actionType.equals("CAN") && !wtchData.actionType.equals("EXP") ) { //&& !wtchData.actionType.equals("COR")
	    				if (endTime != null && endTime.getTime().compareTo(currCal.getTime()) >0 ) 
	    					selectedWatch.add( Integer.toString(wtchData.watchNumber));	
	    			}
	    			else if (wtchData.actionType == null) {
	    				if (endTime != null && endTime.getTime().compareTo(currCal.getTime()) >0 ) 
	    					selectedWatch.add( Integer.toString(wtchData.watchNumber));	
	    			}
				}
			} 
    	}
    	return selectedWatch; 
    }


	/*
	 * Load data from DB, then get the active watch number
	 * @return: list of watch numbers
	 */
	public static List<String> loadContWatchNum() throws VizException {
		List<String> contWatch = new ArrayList<String>();
		AwwRecord awwRecord = null;
		
		HashMap<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>();
       
		// TODO : from WtchContant but I don't think these were all correct.
		
		String wtch[]= { "TORNADO REPORT", "THUNDERSTORM REPORT", "STATUS REPORT" };  				
		//WtchConstant.SEVERE_WEATHER_STATUS_REPORT, WtchConstant.SEVERE_WEATHER_THUNDERSTORM_WATCH, WtchConstant.SEVERE_WEATHER_TORNADO_WATCH};  
		
		RequestConstraint ids = new RequestConstraint();
        ids.setConstraintType(ConstraintType.IN);
        ids.setConstraintValueList(wtch);              
        // query everything and let WtchUtil.isWtchRecord filter out the others.
		metadataMap.put( "reportType",ids );		
		metadataMap.put( "pluginName", new RequestConstraint("aww") );     
		
        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(metadataMap);
        
        try {
            DbQueryResponse response = (DbQueryResponse) ThriftClient.sendRequest(request);

            for (Map<String, Object> result : response.getResults()) {

                for (Object pdo : result.values()) {
                    awwRecord = (AwwRecord) pdo;
                    Collection<String> num = getAwwRecord( awwRecord );
                    if (num != null && !num.isEmpty())
                        contWatch.addAll( num );
                }
            }
        }
        catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }
        
	    //Retrieving unique items from the list
	    Set<String> set = new HashSet<String>(contWatch);	    
	    contWatch = new ArrayList<String>(set);
		
		return contWatch;
	}
	
	// from WtchUtil
	private static boolean isWtchRecord(AwwRecord awwRecord) {

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
		
		return ( rt == AwwReportType.TORNADO_REPORT ); // || 
//				 rt == AwwReportType.THUNDERSTORM_REPORT ???
//				 rt == AwwReportType.STATUS_REPORT );  ???
	}

}
