package gov.noaa.nws.ncep.viz.rsc.ffa.rsc;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwLatlons;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwHVtec; 
//import gov.noaa.nws.ncep.edex.plugin.aww.util.AwwVtecDataUtil; 
import gov.noaa.nws.ncep.edex.common.stationTables.IStationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
//import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.AbstractFrameData;
//import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;
import gov.noaa.nws.ncep.viz.rsc.ffa.util.AwwImmediateCauseUtil;
import gov.noaa.nws.ncep.viz.rsc.ffa.util.FFAConstant;
import gov.noaa.nws.ncep.viz.rsc.ffa.util.FFAUtil;
import gov.noaa.nws.ncep.viz.rsc.ffa.util.StringUtil;
import gov.noaa.nws.ncep.viz.rsc.ffa.util.UGCUtil;
//import gov.noaa.nws.ncep.viz.rsc.warn.rsc.WarnResource.FrameData.WarnData;

import java.awt.Color;
import java.math.BigDecimal;
//import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;

import org.apache.log4j.Logger;
import org.eclipse.swt.graphics.RGB;
import org.geotools.geometry.jts.ReferencedEnvelope;

//import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * FFA resourceResource - Display Flash Flood data from aww data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 21 June 2010  254        M. Gao  	Initial creation.
 * 04 Oct  2010  307        G. Hull     timeMatch FfaRscDataObjs
 * 10 Jan. 2011  N/A        M. Gao      Event Time display includes date + time now since some 
 *                                      events start and then end in different dates                                     
 * 
 * </pre>
 * 
 * @author mgao 
 * @version 1.0
 */


public class FFAResource extends AbstractNatlCntrsResource<FFAResourceData, IMapDescriptor> 
	implements     INatlCntrsResource, IStationField { 

	private Logger logger = Logger.getLogger(this.getClass()); 
	
	private IFont font;
	private StationTable countyStationTable, zoneStationTable; 
	private FFAResourceData ffaRscData;

	private class FfaRscDataObj implements IRscDataObject {
		String 				dataUri;       //used as a key string
		DataTime        	issueTime;     //  issue time from bulletin
		DataTime        	eventTime;   //  Event start time of Vtec with validPeriod
		DataTime        	endTime;     // Event  end time of of Vtec
		String 				reportType;   
		int             	polyNumPoints; //,countyOrZoneNumPoints;
		int 				ugcIndicator; 
		float[]      		polyLatArray, countyOrZoneOrStateOrLakeLatArray;
		float[]      		polyLonArray, countyOrZoneOrStateOrLakeLonArray;
		LatLonPoint[]   	polygonLatLonPointArray;        
		List<LatLonPoint>	countyOrZoneOrStateOrLakeLatLonPointList;
		Set<String>			ugcCodeStringSet, allStateAbbreviationSet, greatLakeNameSet; //countyOrZoneAndStateNameSet, 
		List<String>		countyOrZoneOrStateOrLakeNameList; 
		String 				immediateCause;
		
		@Override
		public DataTime getDataTime() {
			return eventTime;
		} 
	}

	protected class FrameData extends AbstractFrameData {
		HashMap<String, FfaRscDataObj> ffaDataMap;  

		
		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
			ffaDataMap = new HashMap<String,FfaRscDataObj>();
		}
		
		@Override
		public boolean updateFrameData( IRscDataObject rscDataObj ) {
			if( !(rscDataObj instanceof FfaRscDataObj ) ) {
				System.out.println("FFAResource:updateFrameData() processing.....\n" +
						"Data belongs to a different class :"+ rscDataObj.getClass().toString() );
				return false;
			}
			FfaRscDataObj ffaRscDataObj = (FfaRscDataObj)rscDataObj;
			
//			FfaRscDataObj existingFFAData = ffaDataMap.get( ffaRscDataObj.dataUri );
//
//			if( existingFFAData == null || 
//				ffaRscDataObj.issueTime.greaterThan( existingFFAData.issueTime ) ) {//I doubt this condition
//				ffaDataMap.put( ffaRscDataObj.dataUri, ffaRscDataObj );
//			}
			updateFfaDataMap(ffaDataMap, ffaRscDataObj); //modified the above logic and include the new logic in this new private method. 
			
			return true;
		}		
		
		/**
		 * 1.if the eventTime of the FfaRscDataObj is null, the FfaRscDataObj is considered to be invalid record and thus 
		 *   will not be added to the map.  
		 * 2. If the imported FfaRscDataObj is valid, 
		 *    a. If existing FfaRscDataObj is null, the imported ffaRscDataObj is added to the map. 
		 *    b. If existing FfaRscDataObj is not null, the imported ffaRscDataObj is added to the map only when 
		 *       the issueTime of the imported ffaRscDataObj is newer than the issueTime of the existing FfaRscDataObj
		 *       be skipped 
		 * @param ffaDataMap
		 * @param ffaRscDataObj
		 */
		private void updateFfaDataMap(HashMap<String, FfaRscDataObj> ffaDataMap, FfaRscDataObj ffaRscDataObj) {
			if(isFfaRscDataObjValid(ffaRscDataObj)) {
				FfaRscDataObj existingFFAData = ffaDataMap.get( ffaRscDataObj.dataUri );
//				if(existingFFAData == null || 
//						ffaRscDataObj.issueTime.greaterThan( existingFFAData.issueTime )) {
				if(existingFFAData == null || 
						newIssueTimeGreaterThanExistingIssueTime(ffaRscDataObj.issueTime, existingFFAData.issueTime )) {
//					if(ffaRscDataObj.eventTime == null) 
//						System.out.println("@@@@@@@@@@@@@@@@, find a ffaRscDataObj with eventTime of NULL value is added to the MAP!!!!!!"); 
					ffaDataMap.put( ffaRscDataObj.dataUri, ffaRscDataObj );
				}
			}
		}
		
		private boolean newIssueTimeGreaterThanExistingIssueTime(DataTime newIssueTime, DataTime existingIssueTime) {
			boolean isNewIssueTimeGreaterThanExistingIssueTime = false; 
			if(newIssueTime != null && existingIssueTime != null) { 
				if(newIssueTime.greaterThan(existingIssueTime))
					isNewIssueTimeGreaterThanExistingIssueTime = true; 
			}
			return isNewIssueTimeGreaterThanExistingIssueTime; 	
		}
		
		private boolean isFfaRscDataObjValid(FfaRscDataObj newFfaRscDataObj) {
			boolean isFfaRscDataObjValid = false; 
			if(newFfaRscDataObj != null && newFfaRscDataObj.eventTime != null)
				isFfaRscDataObjValid = true; 
			return isFfaRscDataObjValid; 
		}
		
		public Map<String, FfaRscDataObj> getFfaDataMap() {
			return ffaDataMap; 
		}
	}
	
	/**
	 * a debug method to display a populated frameData object
	 */
	private void displayFrameData(FrameData frameData) {
		if(frameData == null) {
			displayLine(); 
			System.out.println("######, the input frameData object is NULL"); 
			displayLine(); 
			return; 
		}
		Map<String, FfaRscDataObj> storedFfaDataMap = frameData.getFfaDataMap(); 
		Set<Entry<String, FfaRscDataObj>> mapEntrySet = storedFfaDataMap.entrySet(); 
		displayTotalStoredFfaRscDataObj(mapEntrySet); 
		displayEachFrameInfo(frameData); 
		int objectIndex = 1; 
		for(Entry<String, FfaRscDataObj> eachMapEntry : mapEntrySet) {
			displayLine(); 
			System.out.println("===      Object No." + objectIndex + "     mapKeyString=" + eachMapEntry.getKey()); 
			displayFfaRscDataObj(eachMapEntry.getValue()); 
			displayLine(); 
			objectIndex++; 
		}
	}

	private void displayEachFrameInfo(FrameData frameData) {
		System.out.println("=============, Frame Time value:");
		displayRaytheonDataTime(frameData.getFrameTime(), "FrameTime"); 
		System.out.println("=============, Associated Frame Start Time value:");
		displayRaytheonDataTime(frameData.getFrameStartTime(), "FrameStartTime"); 
		System.out.println("=============, Associated Frame End Time value:");
		displayRaytheonDataTime(frameData.getFrameEndTime(), "FrameEndTime"); 
	}

	private void displayFfaRscDataObj(FfaRscDataObj ffaRscDataObj) {
		System.out.println("====, dataUri = " + ffaRscDataObj.dataUri); 
		System.out.println("====, reportType = " + ffaRscDataObj.reportType); 
		System.out.println("====, issueTime value:");
		displayRaytheonDataTime(ffaRscDataObj.issueTime, "IssueTime"); 
		System.out.println("====, eventTime value:"); 
		displayRaytheonDataTime(ffaRscDataObj.eventTime, "EventTime"); 
		System.out.println("====, endTime value:"); 
		displayRaytheonDataTime(ffaRscDataObj.endTime, "EndTime"); 
	}
	
	private void displayRaytheonDataTime(DataTime raytheonDataTime, String dataTimeDescMsg) {
		System.out.println("\t ==== " + dataTimeDescMsg +" value starts ======="); 
		System.out.println("\t\t  refTime value=" + getGMTDateString(raytheonDataTime.getRefTime())); 
		System.out.println("\t\t  forecast Time value (Seconds from the refTime)=" + raytheonDataTime.getFcstTime()); 
		System.out.println("\t\t  validPeriod value (as TimeRange Object):");
		displayRaytheonTimeRange(raytheonDataTime.getValidPeriod()); 
		System.out.println("\t ==== " + dataTimeDescMsg +" value ends ======="); 
	}
	
	private void displayRaytheonTimeRange(TimeRange timeRange) {
		System.out.println("\t\t ==== TimeRange value starts ======="); 
		System.out.println("\t\t\t start time = " + getGMTDateString(timeRange.getStart())); 
		System.out.println("\t\t\t end time = " + getGMTDateString(timeRange.getEnd())); 
		System.out.println("\t\t\t valid flag = " + timeRange.isValid()); 
		System.out.println("\t\t\t during = " + timeRange.getDuration()); 
		System.out.println("\t\t ==== TimeRange value ends ======="); 
	}
	
	private void displayTotalStoredFfaRscDataObj(Set<Entry<String, FfaRscDataObj>> mapEntrySet) {
		displayLine(); 
		System.out.println("               The total number of ffaRscDataObj stored in the map is: " + mapEntrySet.size()); 
		displayLine(); 
	}
	
	private void displayLine() {
		System.out.println("=============================================="); 
	}
	
	private String getGMTDateString(Date date) {
//		SimpleDateFormat simpleDateFormat = createDateFormat("UTC"); 
////		SimpleDateFormat simpleDateFormat = createDateFormat("GMT"); 
//		String dateString = simpleDateFormat.format(date); 
//		return dateString; 
		return date.toString(); 
	}
	
	private SimpleDateFormat createDateFormat(String timezoneId) {
		String displayPattern = "EEE, MMM d yyyy HH:mm:ss z"; 
		TimeZone timeZone = TimeZone.getTimeZone(timezoneId); 
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat(displayPattern); 
		simpleDateFormat.setTimeZone(timeZone); 
		return simpleDateFormat; 
	}
	
	/**
	 * This overrides the default which works for PluginDataObjects.
	 * This method is called by queryRecords to turn the records from the database
	 * into FfaRscDataObj objects.
	 */
    @Override
    public IRscDataObject[] processRecord( Object awwObj ) {
    	if( !( awwObj instanceof AwwRecord) ) {
    		System.out.println("FFAResource.processRecord: object is not a "+
    				"AwwRecord: "+ awwObj.getClass().getName() );
    		return new IRscDataObject[]{};
    	}

    	FfaRscDataObj ffaRscDataObj = getFFAData( (AwwRecord) awwObj );

//    	if( ffaRscDataObj == null ) {
    	if(!isRetrievedFfaRscDataObjValid(ffaRscDataObj)) {
    		return new IRscDataObject[]{};
    	}

    	return new FfaRscDataObj[] { ffaRscDataObj };
    }
    
    /*
     * Conditions that a FfaRscDataObj to be invalid object
     *   1. FfaRscDataObj is NULL
     *   2. FfaRscDataObj.eventTime is NULL
     */
    private boolean isRetrievedFfaRscDataObjValid(FfaRscDataObj ffaRscDataObj) {
    	boolean isFfaRscDataObjValid = false; 
    	if(ffaRscDataObj != null && ffaRscDataObj.eventTime != null)
    		isFfaRscDataObjValid = true; 
    	return isFfaRscDataObjValid; 
    }
	
	private FfaRscDataObj getFFAData( AwwRecord awwRecord) {
		FfaRscDataObj ffaData = null;

		if(FFAUtil.isFFARecord(awwRecord)) {
			ffaData = new FfaRscDataObj();
			ffaData.issueTime =new DataTime(awwRecord.getIssueTime());
			ffaData.reportType=awwRecord.getReportType();
			ffaData.dataUri=awwRecord.getDataURI();
			
			Set<AwwUgc> awwUgcSet = awwRecord.getAwwUGC();
//			logger.debug("The retrieved total number of AWWUGCRecord for FFA is:"+awwUgcSet.size());  
			for (AwwUgc eachAwwUgc : awwUgcSet) {
				if(eachAwwUgc.getAwwVtecLine() != null){ //		TO OBTAIN THE EVENT START AND END TIME
					for (AwwVtec awwVtec : eachAwwUgc.getAwwVtecLine()) {//This will be looped only once since the 
						//relationship between tables is one to one
//
//						if(isVtecActionCorrectionOrEventCancelled(awwVtec.getAction())) {
//							ffaData.eventStartTime = ffaData.issueTime;   
//							ffaData.eventEndTime = ffaData.issueTime;
//						} else {
//							/*
//							 * In the real data, sometimes EventStartTime or EventEndTime is missed
//							 */
//							if(awwVtec.getEventStartTime() != null)
//								ffaData.eventStartTime = new DataTime(awwVtec.getEventStartTime());
//							if(awwVtec.getEventEndTime() != null)
//								ffaData.eventEndTime = new DataTime(awwVtec.getEventEndTime());
//						}
						fillEventStartAndEndTime(awwVtec, awwRecord, ffaData); 
						/*
						 * retrieve and then set ImmediateCause value to FFAData
						 */
						ffaData.immediateCause = getImmediateCauseValue(awwVtec); 
					}
				}

				String ugcLine = eachAwwUgc.getUgc();//get the ugc line to find the counties
				if(!StringUtil.isStringEmpty(ugcLine)){
					ffaData.ugcCodeStringSet = getCountyUgcSet(eachAwwUgc.getAwwFIPS()); 
					ffaData = populateCountyOrZoneOrStateOrLakeInfo(ffaData);//get the lat lon too NEW METHOD
				}

				ffaData.polyNumPoints = eachAwwUgc.getAwwLatLon().size();
				if(ffaData.polyNumPoints > 0){
					ffaData.polygonLatLonPointArray = new LatLonPoint[ffaData.polyNumPoints];
					ffaData.polyLatArray = new float[ffaData.polyNumPoints];
					ffaData.polyLonArray = new float[ffaData.polyNumPoints];
					int index;// =warnStatusData.polyNumPoints;
					for (AwwLatlons awwLatLon : eachAwwUgc.getAwwLatLon()) {
						LatLonPoint point = new LatLonPoint (awwLatLon.getLat(), 
								awwLatLon.getLon(),LatLonPoint.INDEGREES);
						index=awwLatLon.getIndex();
						ffaData.polyLatArray[index-1]=awwLatLon.getLat();
						ffaData.polyLonArray[index-1]=awwLatLon.getLon();
						logger.debug("the index of this lat lon is "+index );

						ffaData.polygonLatLonPointArray[index-1] = point;
					}
				}
			}
		}

		return ffaData;
	}
	
	private void fillEventStartAndEndTime(AwwVtec awwVtec, AwwRecord awwRecord, FfaRscDataObj ffaData) {
		/*
		 * In the real data, sometimes EventStartTime or EventEndTime is missed
		 */
		Calendar eventStartCalendar = getEventStartTimeFromAwwVtec(awwVtec); 
		Calendar eventEndCalendar = getEventEndTimeFromAwwVtec(awwVtec); 
		
		/*
		 * If startEventTime is still NULL, use the issueTime of AwwRecord to substitute it
		 * This solution may not be 100% accurate for some scenarios. 
		 */
		if(eventStartCalendar == null && awwRecord.getIssueTime() != null)
			eventStartCalendar = awwRecord.getIssueTime(); 
		
		if( eventStartCalendar != null && eventEndCalendar != null ) {
			ffaData.eventTime = new DataTime(eventStartCalendar,
					               new TimeRange( eventStartCalendar, eventEndCalendar));
			ffaData.endTime = new DataTime(eventEndCalendar);
		}			
	}
	
	private Calendar getEventStartTimeFromAwwVtec(AwwVtec awwVtec) {
		Calendar startTime = null; 
		if(awwVtec != null)
			startTime = awwVtec.getEventStartTime(); 
		return startTime; 
	}
	
	private Calendar getEventEndTimeFromAwwVtec(AwwVtec awwVtec) {
		Calendar endTime = null; 
		if(awwVtec != null)
			endTime = awwVtec.getEventEndTime(); 
		return endTime; 
	}
	
	private Set<String> getCountyUgcSet(Set<AwwFips> awwFipsSet) {
		Set<String> countyUgcSet = null; 
		if(awwFipsSet == null) 
			countyUgcSet = new HashSet<String>(); 
		else {
			countyUgcSet = new HashSet<String>(awwFipsSet.size()); 
			for(AwwFips eachAwwFips : awwFipsSet) {
				String eachFips = eachAwwFips.getFips(); 
				countyUgcSet.add(eachFips); 
			}
		}

		/*
		 * The following line is for testing purpose only. The reason we
		 * add some fake data is our test data is not sufficient to cover
		 * all of scenarios of UGC (county, zone, all states, great lakes 
		 * and new UGC patterns. after test, the line of code needs to be 
		 * commented out. 
		 */
//		countyUgcSet = addTestUgcCodeString(countyUgcSet); 
		
		return countyUgcSet; 
	}
	
	private Set<String> addTestUgcCodeString(Set<String> countyUgcSet) {
		/*
		 * add some all states for test
		 */
		countyUgcSet.add("VAZ000");  //all Virginia 
		countyUgcSet.add("CAZ000");  //all California 
		
		/*
		 * add some great lakes for test
		 */
		countyUgcSet.add("LMZ000");  //Lake Michigan 
		countyUgcSet.add("LEZ000");  //Lake Erie
		
		/*
		 * add some ugc new pattern for test
		 */
		countyUgcSet.add("NC5015");  //all Bertie, NC 
		countyUgcSet.add("IL0087");  //all Johnson, IL 
		countyUgcSet.add("KY9039");  //all Carlisle, KY 

		/*
		 * add some ugc zones for test
		 */
		countyUgcSet.add("ALZ002");  //Colbert, AL 
		countyUgcSet.add("NJZ008");  //Morris, NJ
		countyUgcSet.add("TXZ159");  //McLennan, TX
		
		/*
		 * add some bad ugc zones for test
		 */
		countyUgcSet.add("KKZ002");  //Colbert, AL 
		countyUgcSet.add("KKC008");  //Morris, NJ
		countyUgcSet.add("KKZ000");  //McLennan, TX
		countyUgcSet.add("KA5120");  //McLennan, TX
		countyUgcSet.add("LMC000");  //McLennan, TX
		return countyUgcSet; 
	}
	
	private String getImmediateCauseValue(AwwVtec awwVtec) {
		StringBuilder causeBuilder = new StringBuilder(); 
		if(awwVtec.getAwwHVtecLine() != null) {
			for(AwwHVtec eachHVtec : awwVtec.getAwwHVtecLine()) {
				if(!StringUtil.isStringEmpty(eachHVtec.getImmediateCause()))
					causeBuilder.append(eachHVtec.getImmediateCause())
								.append("; "); 
			}
		}
		return causeBuilder.toString(); 
	}
	
	private FfaRscDataObj populateCountyOrZoneOrStateOrLakeInfo(FfaRscDataObj ffaData){
		ffaData.countyOrZoneOrStateOrLakeLatLonPointList = new ArrayList<LatLonPoint>();
		ffaData.countyOrZoneOrStateOrLakeNameList = new ArrayList<String>();
		ffaData.allStateAbbreviationSet = new HashSet<String>();
		ffaData.greatLakeNameSet = new HashSet<String>();
		ffaData.countyOrZoneOrStateOrLakeLatArray = new float[ffaData.ugcCodeStringSet.size()];
		ffaData.countyOrZoneOrStateOrLakeLonArray = new float[ffaData.ugcCodeStringSet.size()];

		int arrayIndex = 0; 
		for (String eachCountyOrZoneOrStateOrLakeUgcString : ffaData.ugcCodeStringSet) {
			int ugcIndicator = UGCUtil.getUgcIndicator(eachCountyOrZoneOrStateOrLakeUgcString); 
			ffaData.ugcIndicator = ugcIndicator; 
			switch(ugcIndicator) {
			case FFAConstant.UGC_GREAT_LAKE_INDICATOR:
				String greatLakeAbbreviation = eachCountyOrZoneOrStateOrLakeUgcString.substring(0, 2); 
				if(!ffaData.greatLakeNameSet.contains(greatLakeAbbreviation)) {
					List<Object[]> objectArrayList = getGreatLakeInfo(greatLakeAbbreviation); 
					if(!objectArrayList.isEmpty()) {
						Object[] objectArray = objectArrayList.get(0); 
						
						String lakeAreaName = (String)objectArray[0]; 
						String greatLakeName = getGreatLakeName(lakeAreaName); 
						
						Double[] latLonArrayOfLake = getLatLonArrayOfLake((String)objectArray[1]); 
						if(latLonArrayOfLake != null) {
							ffaData.greatLakeNameSet.add(greatLakeAbbreviation); //select area, ctrloc from bounds.greatlakesbnds where id ='
							
							LatLonPoint greatLakePoint = new LatLonPoint (latLonArrayOfLake[0], latLonArrayOfLake[1], LatLonPoint.INDEGREES);
							
							ffaData.countyOrZoneOrStateOrLakeLatLonPointList.add(greatLakePoint);
							ffaData.countyOrZoneOrStateOrLakeNameList.add(getCountyOrZoneAndStateName(greatLakeName, "", FFAConstant.UGC_GREAT_LAKE_INDICATOR));
							ffaData.countyOrZoneOrStateOrLakeLatArray[arrayIndex] = latLonArrayOfLake[0].floatValue();
							ffaData.countyOrZoneOrStateOrLakeLonArray[arrayIndex] = latLonArrayOfLake[1].floatValue();
							arrayIndex++; 
						}
					}						
				}
				break; 
			case FFAConstant.UGC_ALL_STATE_ZONE_INDICATOR:
				String stateAbbreviation = eachCountyOrZoneOrStateOrLakeUgcString.substring(0, 2); 
				if(!ffaData.allStateAbbreviationSet.contains(stateAbbreviation)) {
					List<Object[]> objectArrayList = getStateInfo(stateAbbreviation); 
					if(!objectArrayList.isEmpty()) {
						ffaData.allStateAbbreviationSet.add(stateAbbreviation); 
						
						Object[] objectArray = objectArrayList.get(0); 
						
						String stateFullName = (String)objectArray[0]; 
						BigDecimal stateLatitude = (BigDecimal)objectArray[1]; 
						BigDecimal stateLongitude = (BigDecimal)objectArray[2]; 
						LatLonPoint statePoint = new LatLonPoint (stateLatitude.doubleValue(), stateLongitude.doubleValue(), LatLonPoint.INDEGREES);

						ffaData.countyOrZoneOrStateOrLakeLatLonPointList.add(statePoint);
						ffaData.countyOrZoneOrStateOrLakeNameList.add(getCountyOrZoneAndStateName(stateFullName, "", FFAConstant.UGC_ALL_STATE_ZONE_INDICATOR));
						ffaData.countyOrZoneOrStateOrLakeLatArray[arrayIndex] = stateLatitude.floatValue();
						ffaData.countyOrZoneOrStateOrLakeLonArray[arrayIndex] = stateLongitude.floatValue();
						arrayIndex++; 
					}
				}
				break; 
			case FFAConstant.UGC_ZONE_INDICATOR: 
				Station zoneStation = zoneStationTable.getStation(StationField.STID, eachCountyOrZoneOrStateOrLakeUgcString);
				if (zoneStation != null) {
					String zoneAndStateName = getCountyOrZoneAndStateName(zoneStation.getStnname(), zoneStation.getState(), FFAConstant.UGC_ZONE_INDICATOR); 
					if(!ffaData.countyOrZoneOrStateOrLakeNameList.contains(zoneAndStateName)) {
						LatLonPoint point = new LatLonPoint (zoneStation.getLatitude(),zoneStation.getLongitude(),LatLonPoint.INDEGREES);
						ffaData.countyOrZoneOrStateOrLakeLatLonPointList.add(point);
						ffaData.countyOrZoneOrStateOrLakeNameList.add(zoneAndStateName);
						ffaData.countyOrZoneOrStateOrLakeLatArray[arrayIndex] = zoneStation.getLatitude();
						ffaData.countyOrZoneOrStateOrLakeLonArray[arrayIndex] = zoneStation.getLongitude();
						arrayIndex++; 
					}
				}
				break; 
			case FFAConstant.UGC_NEW_PATTERN_INDICATOR: 
				eachCountyOrZoneOrStateOrLakeUgcString = convertToOldUgcStringPattern(eachCountyOrZoneOrStateOrLakeUgcString); 
			case FFAConstant.UGC_COUNTY_INDICATOR: 
				Station countyStation = countyStationTable.getStation(StationField.STID, eachCountyOrZoneOrStateOrLakeUgcString);
				if (countyStation != null) {
					String countyAndStateName = getCountyOrZoneAndStateName(countyStation.getStnname(), countyStation.getState(), FFAConstant.UGC_COUNTY_INDICATOR); 
					if(!ffaData.countyOrZoneOrStateOrLakeNameList.contains(countyAndStateName)) {
						LatLonPoint point = new LatLonPoint (countyStation.getLatitude(),countyStation.getLongitude(),LatLonPoint.INDEGREES);
						ffaData.countyOrZoneOrStateOrLakeLatLonPointList.add(point);
						ffaData.countyOrZoneOrStateOrLakeNameList.add(countyAndStateName);
						ffaData.countyOrZoneOrStateOrLakeLatArray[arrayIndex] = countyStation.getLatitude();
						ffaData.countyOrZoneOrStateOrLakeLonArray[arrayIndex] = countyStation.getLongitude();
						arrayIndex++; 
					}
				}
				break; 
			default: 
				logger.error("Unknown ugc string pattern, UGC string ="+eachCountyOrZoneOrStateOrLakeUgcString); 
			break; 
			}

		}

		return ffaData;
	}

	private List<Object[]> getStateInfo(String stateAbbreviation) {
		StringBuilder query = new StringBuilder(
				"select name, lat, lon from mapdata.states where state ='");
		query.append(stateAbbreviation)
			 .append("';");
		List<Object[]> results = new ArrayList<Object[]>(); 
		try {
			results = DirectDbQuery.executeQuery(query.toString(), "maps", QueryLanguage.SQL);
		} catch (VizException e1) {
			logger.debug("VizException is thrown when trying to do DirectDbQuery.executeQuery to query mapdata.states table, error="+e1.getMessage()); 
			e1.printStackTrace();
		}
		return results; 
	}
	
	private List<Object[]> getGreatLakeInfo(String greatLakeAbbreviation) {
		StringBuilder query = new StringBuilder(
				"select area, ctrloc from bounds.greatlakesbnds where id ='");
		query.append(greatLakeAbbreviation)
			 .append("';");
		List<Object[]> results = new ArrayList<Object[]>(); 
		try {
			results = DirectDbQuery.executeQuery(query.toString(), "ncep", QueryLanguage.SQL);
		} catch (VizException e1) {
			logger.debug("VizException is thrown when trying to do DirectDbQuery.executeQuery to query bounds.greatlakesbnds table, error="+e1.getMessage()); 
			e1.printStackTrace();
		}
		return results; 
	}
	
	private String getCountyOrZoneAndStateName(String countyOrZoneOrOrStateLakeName, String stateName, int zoneRangeCategoryIndicator) {
		String countyOrZoneAndStateName = "unknown_countyOrZoneName"; 
		if(!StringUtil.isStringEmpty(countyOrZoneOrOrStateLakeName) && stateName != null) {
			switch(zoneRangeCategoryIndicator) {
			case FFAConstant.UGC_ALL_STATE_ZONE_INDICATOR: 
				countyOrZoneAndStateName = countyOrZoneOrOrStateLakeName + 
					FFAConstant.HYPHEN + FFAConstant.ALL_STATES_MARKER;
				break; 
			case FFAConstant.UGC_COUNTY_INDICATOR: 
				countyOrZoneAndStateName = countyOrZoneOrOrStateLakeName + FFAConstant.UNDERSTORE + stateName + 
					FFAConstant.HYPHEN + FFAConstant.COUNTY_MARKER;
				break; 
			case FFAConstant.UGC_ZONE_INDICATOR: 
				countyOrZoneAndStateName = countyOrZoneOrOrStateLakeName + FFAConstant.UNDERSTORE + stateName + 
					FFAConstant.HYPHEN + FFAConstant.ZONE_MARKER;
				break; 
			case FFAConstant.UGC_GREAT_LAKE_INDICATOR: 
				countyOrZoneAndStateName = countyOrZoneOrOrStateLakeName + 
					FFAConstant.HYPHEN + FFAConstant.GREAT_LAKES_MARKER;
				break; 
			}
		}
		return countyOrZoneAndStateName; 
	}

	private boolean isVtecActionCorrectionOrEventCancelled(String vtecAction) {
		boolean result = false; 
		if(vtecAction != null && (vtecAction.equalsIgnoreCase("COR") || vtecAction.equalsIgnoreCase("CAN")))
			result = true; 
		return result; 
	}
	
	/**
	 * Create a FFA resource.
	 * 
	 * @throws VizException
	 */
	public FFAResource(FFAResourceData rscData, 
			LoadProperties loadProperties ) throws VizException {
		super(rscData, loadProperties);
		ffaRscData = (FFAResourceData) resourceData;	
	}


	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		return (AbstractFrameData) new FrameData( frameTime, timeInt );
	}


	public void initResource(IGraphicsTarget grphTarget) throws VizException {
		font = grphTarget.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD });
		countyStationTable = new StationTable( LocalizationManager.getInstance().getFilename("countyStnFile") );
		zoneStationTable = new StationTable( LocalizationManager.getInstance().getFilename("countyZonesFile") );
		queryRecords();
	}

	@Override
	public void disposeInternal() {

	}
	
	public void paintFrame( AbstractFrameData frameData, 
			IGraphicsTarget target, PaintProperties paintProps) throws VizException {

		if( paintProps == null ) {
			return;
		}
		
		FrameData currFrameData = (FrameData) frameData;
		
		RGB color = new RGB (155, 155, 155);
		LineStyle lineStyle = LineStyle.SOLID;
		int outlineWidth = 2;
		int symbolSize  = 2;
		

		
		Collection<FfaRscDataObj> ffaDataValues = currFrameData.ffaDataMap.values();

		for( FfaRscDataObj eachFFAData : ffaDataValues ) {

			if(FFAConstant.FLASH_FLOOD_ADVISORY.equalsIgnoreCase(eachFFAData.reportType)){
				color       = ffaRscData.getFlashFloodAdvisoryColor();
				outlineWidth = ffaRscData.getFlashFloodAdvisorySymbolWidth();
				symbolSize  = ffaRscData.getFlashFloodAdvisorySymbolSize();
				drawingFFAData(target, paintProps, eachFFAData, ffaRscData.getFlashFloodAdvisoryEnable(),  
						color, outlineWidth, lineStyle, symbolSize); 
			} 
			else if (FFAConstant.FLASH_FLOOD_WARNING.equalsIgnoreCase(eachFFAData.reportType)){
				color       = ffaRscData.getFlashFloodWarningColor();
				outlineWidth = ffaRscData.getFlashFloodWarningSymbolWidth();
				symbolSize  = ffaRscData.getFlashFloodWarningSymbolSize();
				drawingFFAData(target, paintProps, eachFFAData, ffaRscData.getFlashFloodWarningEnable(),  
						color, outlineWidth, lineStyle, symbolSize); 
			}
			else if (FFAConstant.FLASH_FLOOD_WATCH.equalsIgnoreCase(eachFFAData.reportType)){
				color       = ffaRscData.getFlashFloodWatchColor(); 
				outlineWidth = ffaRscData.getFlashFloodWatchSymbolWidth(); 
				symbolSize  = ffaRscData.getFlashFloodWatchSymbolSize(); 
				drawingFFAData(target, paintProps, eachFFAData, ffaRscData.getFlashFloodWatchEnable(),  
						color, outlineWidth, lineStyle, symbolSize); 
			}
			else if (FFAConstant.FLASH_FLOOD_STATEMENT.equalsIgnoreCase(eachFFAData.reportType)){
				color       = ffaRscData.getFlashFloodStatementColor(); 
				outlineWidth = ffaRscData.getFlashFloodStatementSymbolWidth(); 
				symbolSize  = ffaRscData.getFlashFloodStatementSymbolSize(); 
				drawingFFAData(target, paintProps, eachFFAData, ffaRscData.getFlashFloodStatementEnable(),  
						color, outlineWidth, lineStyle, symbolSize); 
			}			
		}

	}

	private void drawingFFAData(IGraphicsTarget target, PaintProperties paintProps, 
			FfaRscDataObj ffaData, Boolean drawingEnabled,  
			RGB rgbColor, int outlineWidth, LineStyle lineStyle, int symbolSize) throws VizException {
		if(!drawingEnabled)
			return; 
		if(ffaRscData.getOutlineEnable())
			drawOutline(ffaData, target, rgbColor, outlineWidth, lineStyle, paintProps); 
		drawLabel(ffaData, target, paintProps, rgbColor, symbolSize, 
				symbolSize);   
	}
	
	public void drawOutline(FfaRscDataObj ffaData, IGraphicsTarget target, RGB color, int outlineWidth, 
			LineStyle lineStyle, PaintProperties paintProps) throws VizException{
		Envelope env = null;	
		try {
			PixelExtent extent = (PixelExtent) paintProps.getView().getExtent();
			Envelope e = descriptor.pixelToWorld(extent, descriptor.getCRS());
			ReferencedEnvelope referencedEnvelope = new ReferencedEnvelope(e, descriptor.getCRS());
			env = referencedEnvelope.transform(MapUtil.LATLON_PROJECTION, true);
		} catch (Exception e) {
			throw new VizException("Error transforming extent", e);
		}

		/*
		 * draw county outline if there is any
		 */
		drawCountyOutline(ffaData, target, color, outlineWidth, lineStyle, paintProps, env); 
		
		/*
		 * draw zone outline if there is any
		 */
		drawZoneOutline(ffaData, target, color, outlineWidth, lineStyle, paintProps);
		
		/*
		 * draw all state outline if there is any
		 */
		drawStateOutline(ffaData, target, color, outlineWidth, lineStyle, paintProps); 
		
		/*
		 * draw great lakes outline if there is any
		 */
		drawGreatLakeOutline(ffaData, target, color, outlineWidth, lineStyle, paintProps);
		
	}

  

	public void drawLabel(FfaRscDataObj ffaData, IGraphicsTarget graphicsTarget, PaintProperties paintProps, 
			RGB color, float symbolLineWidth, double symbolSizeScale){
		try{
			int index = 0; 
			for(String eachCountyOrZoneAndStateNameWithMaker : ffaData.countyOrZoneOrStateOrLakeNameList) {
				String eachCountyOrZoneAndStateName = removeMarker(eachCountyOrZoneAndStateNameWithMaker); 
				double[] labelLatLon = { ffaData.countyOrZoneOrStateOrLakeLonArray[index], ffaData.countyOrZoneOrStateOrLakeLatArray[index] }; 
				double[] labelPix = descriptor.worldToPixel( labelLatLon );
			
				if( labelPix != null ){
					/*
					 * If outline is not enabled, draw a small circle above the strings
					 */
					if(!ffaRscData.getOutlineEnable()) {
						drawSymbol(FFAConstant.FILLED_DIAMOND_SYMBOL, graphicsTarget, paintProps, labelLatLon, 
								color, symbolLineWidth, symbolSizeScale); 
					}
					
					String[] textArray = new String[3];

					if(ffaRscData.getCountyOrZoneNameEnable() ){
						textArray[0]=getCountyOrZoneAndStateNameValue(eachCountyOrZoneAndStateName); 
					}

					if(ffaRscData.getTimeEnable() ){
						textArray[1] = getEventTimeStringValue(ffaData.eventTime, ffaData.endTime);
					}

					if(ffaRscData.getImmediateCauseEnable() ){
						textArray[2] = getImmediateCauseDesc(ffaData.immediateCause); 
					}
					
					IExtent screenExtentInPixels = paintProps.getView().getExtent();

			        double ratio = screenExtentInPixels.getWidth()
			                / paintProps.getCanvasBounds().width;

					graphicsTarget.drawStrings(font, textArray,   
							labelPix[0], labelPix[1]+3*ratio, 0.0, TextStyle.NORMAL,
							new RGB[] {color, color, color},
							HorizontalAlignment.LEFT, 
							VerticalAlignment.MIDDLE );
				}
				index++; 
			}
		}
		catch(VizException vize){
			logger.error("VizException is thrown when trying to drawLabel, error=" + vize.getMessage());
		}
	}
	
	private String getImmediateCauseDesc(String immediateCauseAbbreviation) {
		String desc = ""; 
		if(!StringUtil.isStringEmpty(immediateCauseAbbreviation)) {
			String [] abbreviationArray = immediateCauseAbbreviation.split(";"); 
			StringBuilder builder = new StringBuilder(immediateCauseAbbreviation.length()); 
			for(String eachAbbreviation : abbreviationArray) {
				builder.append(AwwImmediateCauseUtil.getImmediateCauseDesc(eachAbbreviation)).append("  "); 
			}
			desc = builder.toString(); 
		}
		return desc; 
	}
	
	private void drawSymbol(String symbolType, IGraphicsTarget graphicsTarget, PaintProperties paintProps, 
			double[] latLonArray, RGB color, float symbolLineWidth, double symbolSizeScale)  {
		Color symbolColor = new Color(color.red, color.green, color.blue); 
    	com.vividsolutions.jts.geom.Coordinate coordinate = new com.vividsolutions.jts.geom.Coordinate(latLonArray[0], latLonArray[1]);
		Symbol symbol = new Symbol(
    			null,
    			new Color[]{symbolColor},
    			symbolLineWidth,// lineWidth, same as arrow's
    			symbolSizeScale,
    			true,
    			coordinate,
    			"Symbol",
    			symbolType);

    	DisplayElementFactory df = new DisplayElementFactory( graphicsTarget, this.descriptor );
		ArrayList<IDisplayable> displayElsPoint = df.createDisplayElements(symbol, paintProps );
		for ( IDisplayable each : displayElsPoint ) {
			      each.draw(graphicsTarget);
			      each.dispose();
		}
	}
	
	private void drawCountyOutline(FfaRscDataObj ffaData, IGraphicsTarget target, RGB color, int outlineWidth, 
			  LineStyle lineStyle, PaintProperties paintProps, Envelope env) {
		String geoConstraint = String.format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
				env.getMinX(), env.getMinY(), env.getMaxX(), env.getMaxY());
		for(String eachCountyOrZoneAndStateNameWithMarker : ffaData.countyOrZoneOrStateOrLakeNameList) {
			if(!isCountyName(eachCountyOrZoneAndStateNameWithMarker))
				continue; 
			String eachCountyOrZoneAndStateName = removeMarker(eachCountyOrZoneAndStateNameWithMarker); 
			String [] countyOrZoneAndStateNameArray = eachCountyOrZoneAndStateName.split(FFAConstant.UNDERSTORE); 
			String countyName = countyOrZoneAndStateNameArray[0];
			String stateName= countyOrZoneAndStateNameArray[1];
			StringBuilder query = new StringBuilder(
				"select AsBinary(the_geom_0_001) from mapdata.county where countyname ='");
			query.append(countyName)
				 .append("' AND  state ='")
				 .append(stateName)
				 .append("' AND ")
				 .append(geoConstraint)
				 .append(";");
			
			doDrawOutLine(query.toString(), "maps", target, color, outlineWidth, lineStyle, paintProps); 
		}
	}

	private void drawZoneOutline(FfaRscDataObj ffaData, IGraphicsTarget target, RGB color, int outlineWidth, 
			  LineStyle lineStyle, PaintProperties paintProps) {
		String queryPrefix = "select AsBinary(the_geom_0_001) from mapdata.zone where state_zone ='";
		for(String eachUgcValue : ffaData.ugcCodeStringSet) {
			if(!isZoneName(eachUgcValue))
				continue; 
			StringBuilder query = new StringBuilder(queryPrefix);
			String stateZone = eachUgcValue.substring(0,2) + eachUgcValue.substring(3).trim(); 
			query.append(stateZone)
				 .append("';");
			
			doDrawOutLine(query.toString(), "maps", target, color, outlineWidth, lineStyle, paintProps); 
		}
	}

	private void drawStateOutline(FfaRscDataObj ffaData, IGraphicsTarget target, RGB color, int outlineWidth, 
			  LineStyle lineStyle, PaintProperties paintProps) {
		String queryPrefix = "select AsBinary(the_geom_0_001) from mapdata.states where state ='";
		for(String eachUgcValue : ffaData.ugcCodeStringSet) {
			if(!isAllStateName(eachUgcValue))
				continue; 
			StringBuilder query = new StringBuilder(queryPrefix);
			String stateAbbreviation = eachUgcValue.substring(0, 2); 
			query.append(stateAbbreviation)
				 .append("';");
			
			doDrawOutLine(query.toString(), "maps", target, color, outlineWidth, lineStyle, paintProps); 
		}
		
	}

	private void drawGreatLakeOutline(FfaRscDataObj ffaData, IGraphicsTarget target, RGB color, int outlineWidth, 
			  LineStyle lineStyle, PaintProperties paintProps) {
		String queryPrefix = "select AsBinary(the_geom_0_001) from bounds.greatlakesbnds where id ='";
		for(String eachUgcValue : ffaData.ugcCodeStringSet) {
			if(!isGreatLakeName(eachUgcValue))
				continue; 
			StringBuilder query = new StringBuilder(queryPrefix);
			String greatLakeAbbreviation = eachUgcValue.substring(0, 2); 
			query.append(greatLakeAbbreviation)
				 .append("';");
			
			doDrawOutLine(query.toString(), "ncep", target, color, outlineWidth, lineStyle, paintProps); 
		}
	}

	private void doDrawOutLine(String query, String dbName, IGraphicsTarget target, RGB color, int outlineWidth, 
			  LineStyle lineStyle, PaintProperties paintProps) {
		try {
			List<Object[]> results = DirectDbQuery.executeQuery(query, dbName, QueryLanguage.SQL);
			IWireframeShape newOutlineShape = target.createWireframeShape(false, descriptor, 0.0f);
			IShadedShape newShadedShape = target.createShadedShape(false,descriptor, true);
			JTSCompiler jtsCompiler = new JTSCompiler(newShadedShape, newOutlineShape, descriptor, PointStyle.CROSS);

			WKBReader wkbReader = new WKBReader();
			for (Object[] result : results) {
				int k = 0;
				byte[] wkb = (byte[]) result[k++];
				Geometry g;
				try {
					g = wkbReader.read(wkb);
					if (!(g instanceof Point)) {
						jtsCompiler.handle(g, color);
					}
				}
				catch (VizException e) {
					logger.error("VizException is thrown when trying to reproject map outline:"+e.getMessage());
				}
				catch (ParseException e) {
					logger.error("ParseException is thrown when trying to reproject map outline:"+e.getMessage());
				}
			}
			newOutlineShape.compile();
//????			float alpha = paintProps.getAlpha();  //Why do i need this alpha value?

			/*if (newShadedShape != null && newShadedShape.isDrawable() ) {
   				target.drawShadedShape(newShadedShape, alpha);
			}*/

			if (newOutlineShape != null && newOutlineShape.isDrawable()){
				target.drawWireframeShape(newOutlineShape, color, outlineWidth, lineStyle );
			}
			//target.setNeedsRefresh(true);  IF NEEDED
		} catch (VizException e1) {
			logger.debug("VizException is thrown when trying to do DirectDbQuery.executeQuery to query "+dbName+" table, error="+e1.getMessage()); 
			e1.printStackTrace();
		}
	}
	
	private String convertToOldUgcStringPattern(String newUgcStringPattern) {
		/*
		 * we convert the new string pattern to old county pattern
		 */
		StringBuilder builder = new StringBuilder(); 
		if(!StringUtil.isStringEmpty(newUgcStringPattern) &&
				newUgcStringPattern.trim().length() == 6) {
			builder.append(newUgcStringPattern.subSequence(0, 2))
				   .append("C")
				   .append(newUgcStringPattern.substring(3)); 
		}
		return builder.toString(); 
	}
	
	private String removeMarker(String countyOrZoneAndStateNameWithMaker) {
		String[] countyOrZoneAndStateNameArray = countyOrZoneAndStateNameWithMaker.split(FFAConstant.HYPHEN); 
		return countyOrZoneAndStateNameArray[0]; 
	}
	
	private String getCountyOrZoneAndStateNameValue(String combinedName) {
		StringBuilder builder = new StringBuilder(); 
		if(!StringUtil.isStringEmpty(combinedName)) {
			String[] countyOrZoneAndStateNameArray = combinedName.split(FFAConstant.UNDERSTORE); 
			builder.append(" ")
				   .append(countyOrZoneAndStateNameArray[0]); 
			if(countyOrZoneAndStateNameArray.length == 2) {
				builder.append(", ")
					   .append(countyOrZoneAndStateNameArray[1]); 
			}
		}
		return builder.toString(); 
	}

	private String getEventTimeStringValue(DataTime eventStartTime, DataTime eventEndTime) {
		StringBuilder builder = new StringBuilder(16); 
		builder.append(" "); 
		if(eventStartTime != null)
			builder.append(eventStartTime.toString().substring(0, 16)); //.substring(11, 16)); 
		else
			builder.append(" - ");
		builder.append(" -- "); 
		if(eventEndTime != null)
			builder.append(eventEndTime.toString().substring(0, 16)); //.substring(11, 16)); 
		else
			builder.append(" - ");
		return builder.toString(); 
	}
	
	private String getGreatLakeName(String lakeAreaName) {
		String greatLakeName = "Unknown great lake name"; 
		if(!StringUtil.isStringEmpty(lakeAreaName))
			greatLakeName = lakeAreaName.trim().replace(FFAConstant.UNDERSTORE, " "); 
		return greatLakeName; 
	}
	
	private Double[] getLatLonArrayOfLake(String lakeAreaStringValue) {
		Double [] latLonDoubleArray = null; 
		if(isLakeAreaStringValid(lakeAreaStringValue)) {
			String trimmedString = lakeAreaStringValue.trim(); 
			int stringLength = trimmedString.length(); 
			String[] latLonStringArray = trimmedString.substring(1, stringLength-1).split(","); 
			if(latLonStringArray != null && latLonStringArray.length == 2) {
				try {
					latLonDoubleArray = new Double[2]; 
					latLonDoubleArray[0] = Double.parseDouble(latLonStringArray[0]); 
					latLonDoubleArray[1] = Double.parseDouble(latLonStringArray[1]); 
				} catch(NumberFormatException nfe) {
					latLonDoubleArray = null; 
				}
			}
		}
		return latLonDoubleArray; 
	}
	
	private boolean isLakeAreaStringValid(String lakeAreaStringValue) {
		boolean isValid = false; 
		if(!StringUtil.isStringEmpty(lakeAreaStringValue)) {
			int strLength = lakeAreaStringValue.trim().length(); 
			if(lakeAreaStringValue.trim().indexOf('(') == 0 &&
					lakeAreaStringValue.indexOf(')') == (strLength-1) && 
					lakeAreaStringValue.indexOf(',') > 0)
				isValid = true; 
		}
		return isValid; 
	}
	
	private boolean isCountyName(String countyOrZoneOrStateOrLakeNameWithMarker) {
		boolean isCounty = false; 
		String[] nameStringArray = countyOrZoneOrStateOrLakeNameWithMarker.split(FFAConstant.HYPHEN); 
		if(nameStringArray != null && nameStringArray.length ==2) {
			if(FFAConstant.COUNTY_MARKER.equals(nameStringArray[1]))
				isCounty = true; 
		}
		return isCounty; 
	}
	
	private boolean isZoneName(String eachUgcValue) {
		boolean isZone = false; 
		int ugcIndicator = UGCUtil.getUgcIndicator(eachUgcValue);
		if(FFAConstant.UGC_ZONE_INDICATOR == ugcIndicator) {
				isZone = true; 
		}
		return isZone; 
	}
	
	private boolean isAllStateName(String eachUgcValue) {
		boolean isAllState = false; 
		int ugcIndicator = UGCUtil.getUgcIndicator(eachUgcValue);
		if(FFAConstant.UGC_ALL_STATE_ZONE_INDICATOR == ugcIndicator) {
			isAllState = true; 
		}
		return isAllState; 
	}
	
	private boolean isGreatLakeName(String eachUgcValue) {
		boolean isGreatLake = false; 
		int ugcIndicator = UGCUtil.getUgcIndicator(eachUgcValue);
		if(FFAConstant.UGC_GREAT_LAKE_INDICATOR == ugcIndicator) {
			isGreatLake = true; 
		}
		return isGreatLake; 
	}
	

}