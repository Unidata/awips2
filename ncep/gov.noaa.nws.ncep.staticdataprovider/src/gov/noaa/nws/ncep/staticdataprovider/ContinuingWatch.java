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
import gov.noaa.nws.ncep.viz.rsc.wtch.util.WtchConstant;
import gov.noaa.nws.ncep.viz.rsc.wtch.util.WtchUtil;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/12		#770		Q. Zhou   	Initial Creation.
 * 09/12		#770		Q. Zhou   	Clean up and change selectedWatch to a collection
 * </pre>
 * 
 * @author	Q. Zhou
 */

public class ContinuingWatch {
	
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
		
    	if(WtchUtil.isWtchRecord(awwRecord)) {
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
        String wtch[]={WtchConstant.SEVERE_WEATHER_STATUS_REPORT, WtchConstant.SEVERE_WEATHER_THUNDERSTORM_WATCH, WtchConstant.SEVERE_WEATHER_TORNADO_WATCH};  
		
		RequestConstraint ids = new RequestConstraint();
        ids.setConstraintType(ConstraintType.IN);
        ids.setConstraintValueList(wtch);              
		metadataMap.put("reportType",ids);		
		metadataMap.put( "pluginName", new RequestConstraint("aww") );     
		
		HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(metadataMap);

		LayerProperty prop = new LayerProperty();
		prop.setDesiredProduct(ResourceType.PLAN_VIEW);
		prop.setEntryQueryParameters(queryList, false);
		prop.setNumberOfImages(15000); // TODO: max # records ?? should we cap this ?
		
		String script = null;
		script = ScriptCreator.createScript(prop);
		if (script == null)
			return null;
		
		Object[] pdoList = Connector.getInstance().connect(script, null, 60000);	
		for (Object pdo : pdoList) {
			awwRecord = (AwwRecord) pdo;
			Collection<String> num = getAwwRecord( awwRecord );
			if (num != null && !num.isEmpty())
				contWatch.addAll( num);
		}
		
	    //Retrieving unique items from the list
	    Set<String> set = new HashSet<String>(contWatch);	    
	    contWatch = new ArrayList<String>(set);
		
		return contWatch;
	}
	
}
