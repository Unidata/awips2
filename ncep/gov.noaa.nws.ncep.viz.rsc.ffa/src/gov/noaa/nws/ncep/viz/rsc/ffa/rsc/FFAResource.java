package gov.noaa.nws.ncep.viz.rsc.ffa.rsc;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwLatlons;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwHVtec; 
import gov.noaa.nws.ncep.edex.common.stationTables.IStationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.rsc.ffa.util.AwwImmediateCauseUtil;
import gov.noaa.nws.ncep.viz.rsc.ffa.util.FFAConstant;
import gov.noaa.nws.ncep.viz.rsc.ffa.util.FFAUtil;
import gov.noaa.nws.ncep.viz.rsc.ffa.util.StringUtil;
import gov.noaa.nws.ncep.viz.rsc.ffa.util.UGCUtil;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.awt.Color;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.eclipse.swt.graphics.RGB;
import org.geotools.geometry.jts.ReferencedEnvelope;

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
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
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
 * 16 Feb 2012    555       S. Gurung   Added call to setAllFramesAsPopulated() in queryRecords()                                 
 * 05/23/12       785       Q. Zhou     Added getName for legend.
 * 17 Aug 2012    655       B. Hebbard  Added paintProps as parameter to IDisplayable draw
 * 09/11/12      852        Q. Zhou     Modified time string and alignment in drawLabel().
 * 02/01/13      972        G. Hull     define on NcMapDescriptor instead of IMapDescriptor
 * </pre>
 * 
 * @author mgao 
 * @version 1.0
 */


public class FFAResource extends AbstractNatlCntrsResource<FFAResourceData, NCMapDescriptor> 
	implements     INatlCntrsResource, IStationField { 

	private Logger logger = Logger.getLogger(this.getClass()); 
	
	private IFont font;
	private StationTable countyStationTable, zoneStationTable; 
	private FFAResourceData ffaRscData;

	public class FfaRscDataObj implements IRscDataObject {
		String 				dataUri,vtecline;       //used as a key string //T456 vtecline
		DataTime        	issueTime;     //  issue time from bulletin
		DataTime        	eventTime;   //  Event start time of Vtec with validPeriod
		DataTime        	endTime;     // Event  end time of of Vtec
		String 				reportType, actionType, officeId, eTrackingNo, phenomena, significance;  //T456: last five 
		int             	polyNumPoints; //,countyOrZoneNumPoints;
		int 				ugcIndicator; 
		float[]      		polyLatArray, countyOrZoneOrStateOrLakeLatArray;
		float[]      		polyLonArray, countyOrZoneOrStateOrLakeLonArray;
		LatLonPoint[]   	polygonLatLonPointArray;        
		List<LatLonPoint>	countyOrZoneOrStateOrLakeLatLonPointList;
		Set<String>			ugcCodeStringSet, allStateAbbreviationSet, greatLakeNameSet; //countyOrZoneAndStateNameSet, 
		List<String>		countyOrZoneOrStateOrLakeNameList, fips;//T456: last one; 
		String 				immediateCause;
//HashMap<String,FipsInfo> sublist = new HashMap<String,FipsInfo>();//key: county fips 
		@Override public DataTime getDataTime() {return eventTime;}
		
		public String getKey(){ return getFfaRscDataObjKey(this);}	
		 
	}

	protected class FrameData extends AbstractFrameData {
		HashMap<String, FfaRscDataObj> ffaDataMap;  
//Map<String, ArrayList<FfaRscDataObj>> fDataMap = new HashMap<String,ArrayList<FfaRscDataObj>>();//T456: NEW,EXA/EXB all share the same key with a list.
		
		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
			ffaDataMap = new HashMap<String,FfaRscDataObj>();
		}
		
		@Override
		public boolean updateFrameData( IRscDataObject rscDataObj ) {
//if(true) return updateFrameData2(rscDataObj, fDataMap);			
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
//	private void displayFrameData(FrameData frameData) {
//		if(frameData == null) {
//			displayLine(); 
//			System.out.println("######, the input frameData object is NULL"); 
//			displayLine(); 
//			return; 
//		}
//		Map<String, FfaRscDataObj> storedFfaDataMap = frameData.getFfaDataMap(); 
//		Set<Entry<String, FfaRscDataObj>> mapEntrySet = storedFfaDataMap.entrySet(); 
//		displayTotalStoredFfaRscDataObj(mapEntrySet); 
//		displayEachFrameInfo(frameData); 
//		int objectIndex = 1; 
//		for(Entry<String, FfaRscDataObj> eachMapEntry : mapEntrySet) {
//			displayLine(); 
//			System.out.println("===      Object No." + objectIndex + "     mapKeyString=" + eachMapEntry.getKey()); 
//			displayFfaRscDataObj(eachMapEntry.getValue()); 
//			displayLine(); 
//			objectIndex++; 
//		}
//	}

//	private void displayEachFrameInfo(FrameData frameData) {
//		System.out.println("=============, Frame Time value:");
//		displayRaytheonDataTime(frameData.getFrameTime(), "FrameTime"); 
//		System.out.println("=============, Associated Frame Start Time value:");
//		displayRaytheonDataTime(frameData.getFrameStartTime(), "FrameStartTime"); 
//		System.out.println("=============, Associated Frame End Time value:");
//		displayRaytheonDataTime(frameData.getFrameEndTime(), "FrameEndTime"); 
//	}
//
//	private void displayFfaRscDataObj(FfaRscDataObj ffaRscDataObj) {
//		System.out.println("====, dataUri = " + ffaRscDataObj.dataUri); 
//		System.out.println("====, reportType = " + ffaRscDataObj.reportType); 
//		System.out.println("====, issueTime value:");
//		displayRaytheonDataTime(ffaRscDataObj.issueTime, "IssueTime"); 
//		System.out.println("====, eventTime value:"); 
//		displayRaytheonDataTime(ffaRscDataObj.eventTime, "EventTime"); 
//		System.out.println("====, endTime value:"); 
//		displayRaytheonDataTime(ffaRscDataObj.endTime, "EndTime"); 
//	}
//	
//	private void displayRaytheonDataTime(DataTime raytheonDataTime, String dataTimeDescMsg) {
//		System.out.println("\t ==== " + dataTimeDescMsg +" value starts ======="); 
//		System.out.println("\t\t  refTime value=" + getGMTDateString(raytheonDataTime.getRefTime())); 
//		System.out.println("\t\t  forecast Time value (Seconds from the refTime)=" + raytheonDataTime.getFcstTime()); 
//		System.out.println("\t\t  validPeriod value (as TimeRange Object):");
//		displayRaytheonTimeRange(raytheonDataTime.getValidPeriod()); 
//		System.out.println("\t ==== " + dataTimeDescMsg +" value ends ======="); 
//	}
	
//	private void displayRaytheonTimeRange(TimeRange timeRange) {
//		System.out.println("\t\t ==== TimeRange value starts ======="); 
//		System.out.println("\t\t\t start time = " + getGMTDateString(timeRange.getStart())); 
//		System.out.println("\t\t\t end time = " + getGMTDateString(timeRange.getEnd())); 
//		System.out.println("\t\t\t valid flag = " + timeRange.isValid()); 
//		System.out.println("\t\t\t during = " + timeRange.getDuration()); 
//		System.out.println("\t\t ==== TimeRange value ends ======="); 
//	}
//	
//	private void displayTotalStoredFfaRscDataObj(Set<Entry<String, FfaRscDataObj>> mapEntrySet) {
//		displayLine(); 
//		System.out.println("               The total number of ffaRscDataObj stored in the map is: " + mapEntrySet.size()); 
//		displayLine(); 
//	}
//	
//	private void displayLine() {
//		System.out.println("=============================================="); 
//	}
	
//	private String getGMTDateString(Date date) {
////		SimpleDateFormat simpleDateFormat = createDateFormat("UTC"); 
//////		SimpleDateFormat simpleDateFormat = createDateFormat("GMT"); 
////		String dateString = simpleDateFormat.format(date); 
////		return dateString; 
//		return date.toString(); 
//	}
//	
//	private SimpleDateFormat createDateFormat(String timezoneId) {
//		String displayPattern = "EEE, MMM d yyyy HH:mm:ss z"; 
//		TimeZone timeZone = TimeZone.getTimeZone(timezoneId); 
//		SimpleDateFormat simpleDateFormat = new SimpleDateFormat(displayPattern); 
//		simpleDateFormat.setTimeZone(timeZone); 
//		return simpleDateFormat; 
//	}
	
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
//List<FfaRscDataObj> frdos = getFFAData2( (AwwRecord) awwObj );//T456
//    	if( ffaRscDataObj == null ) {//this line commented out in OLD code
    	if(!isRetrievedFfaRscDataObjValid(ffaRscDataObj)) {
    		return new IRscDataObject[]{};
    	}

    	return new FfaRscDataObj[] { ffaRscDataObj };//frdos.toArray(new FfaRscDataObj[]{});//
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
ffaData.fips = getFips(eachAwwUgc);//2011-10-05
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
	
//	private Set<String> addTestUgcCodeString(Set<String> countyUgcSet) {
//		/*
//		 * add some all states for test
//		 */
//		countyUgcSet.add("VAZ000");  //all Virginia 
//		countyUgcSet.add("CAZ000");  //all California 
//		
//		/*
//		 * add some great lakes for test
//		 */
//		countyUgcSet.add("LMZ000");  //Lake Michigan 
//		countyUgcSet.add("LEZ000");  //Lake Erie
//		
//		/*
//		 * add some ugc new pattern for test
//		 */
//		countyUgcSet.add("NC5015");  //all Bertie, NC 
//		countyUgcSet.add("IL0087");  //all Johnson, IL 
//		countyUgcSet.add("KY9039");  //all Carlisle, KY 
//
//		/*
//		 * add some ugc zones for test
//		 */
//		countyUgcSet.add("ALZ002");  //Colbert, AL 
//		countyUgcSet.add("NJZ008");  //Morris, NJ
//		countyUgcSet.add("TXZ159");  //McLennan, TX
//		
//		/*
//		 * add some bad ugc zones for test
//		 */
//		countyUgcSet.add("KKZ002");  //Colbert, AL 
//		countyUgcSet.add("KKC008");  //Morris, NJ
//		countyUgcSet.add("KKZ000");  //McLennan, TX
//		countyUgcSet.add("KA5120");  //McLennan, TX
//		countyUgcSet.add("LMC000");  //McLennan, TX
//		return countyUgcSet; 
//	}
	
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

//	private boolean isVtecActionCorrectionOrEventCancelled(String vtecAction) {
//		boolean result = false; 
//		if(vtecAction != null && (vtecAction.equalsIgnoreCase("COR") || vtecAction.equalsIgnoreCase("CAN")))
//			result = true; 
//		return result; 
//	}
	
	/**
	 * Create a FFA resource.
	 * 
	 * @throws VizException
	 */
	public FFAResource(FFAResourceData rscData, LoadProperties loadProperties ) throws VizException {
		super(rscData, loadProperties);	
		ffaRscData = (FFAResourceData) resourceData;
		addRDChangedListener();//T456: handling Area (IGLTarget) change	
	}


	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		return (AbstractFrameData) new FrameData( frameTime, timeInt );
	}


	public void initResource(IGraphicsTarget grphTarget) throws VizException {
		font = grphTarget.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD });
		countyStationTable = new StationTable( 
				NcPathManager.getInstance().getStaticFile(
						NcPathConstants.COUNTY_STN_TBL ).getAbsolutePath() );
		zoneStationTable = new StationTable( 
				NcPathManager.getInstance().getStaticFile(
						NcPathConstants.FFG_ZONES_STN_TBL ).getAbsolutePath() );
		queryRecords();
	}

	@Override
	public void disposeInternal() {

	}
	
	public void paintFrame( AbstractFrameData frameData, 
			IGraphicsTarget target, PaintProperties paintProps) throws VizException {

		if( paintProps == null ) { return; }
			
if( areaChangeFlag ){ areaChangeFlag = false; postProcessFrameUpdate(); }//T456: dispose old outlineShape? TODO		

		FrameData currFrameData = (FrameData) frameData;
		
		RGB color = new RGB (155, 155, 155);
		LineStyle lineStyle = LineStyle.SOLID;
		int outlineWidth = 2;
		int symbolSize  = 2;
//isFirstRound=true; Collection<ArrayList<FfaRscDataObj>> fdvs = currFrameData.fDataMap.values();//T456		
//for(ArrayList<FfaRscDataObj> fdvList : fdvs)//T456, NO brack needed since there is ONLY one for-each loop block inside.
//	for(FfaRscDataObj eachFFAData : fdvList){//T456
		Collection<FfaRscDataObj> ffaDataValues = currFrameData.ffaDataMap.values();

		for( FfaRscDataObj eachFFAData : ffaDataValues ) {
/*2011-10-05
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
			else*/ if (FFAConstant.FLASH_FLOOD_WATCH.equalsIgnoreCase(eachFFAData.reportType)){
				color       = ffaRscData.getFlashFloodWatchColor(); 
				outlineWidth = ffaRscData.getFlashFloodWatchSymbolWidth(); 
				symbolSize  = ffaRscData.getFlashFloodWatchSymbolSize(); 
				drawingFFAData(target, paintProps, eachFFAData, ffaRscData.getFlashFloodWatchEnable(),  
						color, outlineWidth, lineStyle, symbolSize); 
			}
else if (FFAConstant.FLOOD_WATCH/*.FLASH_FLOOD_STATEMENT*/.equalsIgnoreCase(eachFFAData.reportType)){//T456
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
			Envelope e = getNcMapDescriptor().pixelToWorld(extent, descriptor.getCRS());
			ReferencedEnvelope referencedEnvelope = new ReferencedEnvelope(e, descriptor.getCRS());
			env = referencedEnvelope.transform(MapUtil.LATLON_PROJECTION, true);
		} catch (Exception e) {
			throw new VizException("Error transforming extent", e);
		}

		/*
		 * draw county outline if there is any
		 */
//		drawCountyOutline(ffaData, target, color, outlineWidth, lineStyle, paintProps, env); 
/*if(isFirstRound)*/ drawZoneOutline2(ffaData, target, color, outlineWidth, lineStyle, paintProps, env); //isFirstRound=false;
		/*
		 * draw zone outline if there is any
		 */
//		drawZoneOutline(ffaData, target, color, outlineWidth, lineStyle, paintProps);
		
		/*
		 * draw all state outline if there is any
		 */
//		drawStateOutline(ffaData, target, color, outlineWidth, lineStyle, paintProps); 
		
		/*
		 * draw great lakes outline if there is any
		 */
//		drawGreatLakeOutline(ffaData, target, color, outlineWidth, lineStyle, paintProps);
		
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
								color, symbolLineWidth, symbolSizeScale*0.4); 
					}
					
					String[] text = new String[3];
					List<String> enabledText = new ArrayList<String>();
					
					if(ffaRscData.getCountyOrZoneNameEnable() ){
						enabledText.add(getCountyOrZoneAndStateNameValue(eachCountyOrZoneAndStateName)); 
					}

					if(ffaRscData.getTimeEnable() ){
						enabledText.add(getEventTimeStringValue(ffaData.eventTime, ffaData.endTime));
					}

					if(ffaRscData.getImmediateCauseEnable() ){
						enabledText.add(getImmediateCauseDesc(ffaData.immediateCause)); 
					}
					
					for (int j=enabledText.size(); j<3; j++)
						enabledText.add("");
					
					text = enabledText.toArray(text);

					IExtent screenExtentInPixels = paintProps.getView().getExtent();
			        double ratio = screenExtentInPixels.getWidth()
			                / paintProps.getCanvasBounds().width;

					graphicsTarget.drawStrings(font, text,   
							labelPix[0], labelPix[1]+ 3*ratio, 0.0, TextStyle.NORMAL,
							new RGB[] {color, color, color},
							HorizontalAlignment.LEFT, 
							VerticalAlignment.TOP );
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

    	DisplayElementFactory df = new DisplayElementFactory( graphicsTarget, getNcMapDescriptor() );
		ArrayList<IDisplayable> displayElsPoint = df.createDisplayElements(symbol, paintProps );
		for ( IDisplayable each : displayElsPoint ) {
			      each.draw(graphicsTarget, paintProps);
			      each.dispose();
		}
	}
	
//	private void drawCountyOutline(FfaRscDataObj ffaData, IGraphicsTarget target, RGB color, int outlineWidth, 
//			  LineStyle lineStyle, PaintProperties paintProps, Envelope env) {
//		String geoConstraint = String.format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
//				env.getMinX(), env.getMinY(), env.getMaxX(), env.getMaxY());
//		for(String eachCountyOrZoneAndStateNameWithMarker : ffaData.countyOrZoneOrStateOrLakeNameList) {
//			if(!isCountyName(eachCountyOrZoneAndStateNameWithMarker))
//				continue; 
//			String eachCountyOrZoneAndStateName = removeMarker(eachCountyOrZoneAndStateNameWithMarker); 
//			String [] countyOrZoneAndStateNameArray = eachCountyOrZoneAndStateName.split(FFAConstant.UNDERSTORE); 
//			String countyName = countyOrZoneAndStateNameArray[0];
//			String stateName= countyOrZoneAndStateNameArray[1];
//			StringBuilder query = new StringBuilder(
//				"select AsBinary(the_geom_0_001) from mapdata.county where countyname ='");
//			query.append(countyName)
//				 .append("' AND  state ='")
//				 .append(stateName)
//				 .append("' AND ")
//				 .append(geoConstraint)
//				 .append(";");
//			
//			doDrawOutLine(query.toString(), "maps", target, color, outlineWidth, lineStyle, paintProps); 
//		}
//	}
//
//	private void drawZoneOutline(FfaRscDataObj ffaData, IGraphicsTarget target, RGB color, int outlineWidth, 
//			  LineStyle lineStyle, PaintProperties paintProps) {
//		String queryPrefix = "select AsBinary(the_geom_0_001) from mapdata.zone where state_zone ='";
//		for(String eachUgcValue : ffaData.ugcCodeStringSet) {
//			if(!isZoneName(eachUgcValue))
//				continue; 
//			StringBuilder query = new StringBuilder(queryPrefix);
//			String stateZone = eachUgcValue.substring(0,2) + eachUgcValue.substring(3).trim(); 
//			query.append(stateZone)
//				 .append("';");
//			
//			doDrawOutLine(query.toString(), "maps", target, color, outlineWidth, lineStyle, paintProps); 
//		}
//	}

//	private void drawStateOutline(FfaRscDataObj ffaData, IGraphicsTarget target, RGB color, int outlineWidth, 
//			  LineStyle lineStyle, PaintProperties paintProps) {
//		String queryPrefix = "select AsBinary(the_geom_0_001) from mapdata.states where state ='";
//		for(String eachUgcValue : ffaData.ugcCodeStringSet) {
//			if(!isAllStateName(eachUgcValue))
//				continue; 
//			StringBuilder query = new StringBuilder(queryPrefix);
//			String stateAbbreviation = eachUgcValue.substring(0, 2); 
//			query.append(stateAbbreviation)
//				 .append("';");
//			
//			doDrawOutLine(query.toString(), "maps", target, color, outlineWidth, lineStyle, paintProps); 
//		}
//		
//	}
//
//	private void drawGreatLakeOutline(FfaRscDataObj ffaData, IGraphicsTarget target, RGB color, int outlineWidth, 
//			  LineStyle lineStyle, PaintProperties paintProps) {
//		String queryPrefix = "select AsBinary(the_geom_0_001) from bounds.greatlakesbnds where id ='";
//		for(String eachUgcValue : ffaData.ugcCodeStringSet) {
//			if(!isGreatLakeName(eachUgcValue))
//				continue; 
//			StringBuilder query = new StringBuilder(queryPrefix);
//			String greatLakeAbbreviation = eachUgcValue.substring(0, 2); 
//			query.append(greatLakeAbbreviation)
//				 .append("';");
//			
//			doDrawOutLine(query.toString(), "ncep", target, color, outlineWidth, lineStyle, paintProps); 
//		}
//	}

//	private void doDrawOutLine(String query, String dbName, IGraphicsTarget target, RGB color, int outlineWidth, 
//			  LineStyle lineStyle, PaintProperties paintProps) {
//		try {
//			List<Object[]> results = DirectDbQuery.executeQuery(query, dbName, QueryLanguage.SQL);
//			IWireframeShape newOutlineShape = target.createWireframeShape(false, descriptor, 0.0f);
//			IShadedShape newShadedShape = target.createShadedShape(false,descriptor, true);
//			JTSCompiler jtsCompiler = new JTSCompiler(newShadedShape, newOutlineShape, descriptor, PointStyle.CROSS);
//
//			WKBReader wkbReader = new WKBReader();
//			for (Object[] result : results) {
//				int k = 0;
//				byte[] wkb = (byte[]) result[k++];
//				Geometry g;
//				try {
//					g = wkbReader.read(wkb);
//					if (!(g instanceof Point)) {
//						jtsCompiler.handle(g, color);
//					}
//				}
//				catch (VizException e) {
//					logger.error("VizException is thrown when trying to reproject map outline:"+e.getMessage());
//				}
//				catch (ParseException e) {
//					logger.error("ParseException is thrown when trying to reproject map outline:"+e.getMessage());
//				}
//			}
//			newOutlineShape.compile();
////????			float alpha = paintProps.getAlpha();  //Why do i need this alpha value?
//
//			/*if (newShadedShape != null && newShadedShape.isDrawable() ) {
//   				target.drawShadedShape(newShadedShape, alpha);
//			}*/
//
//			if (newOutlineShape != null && newOutlineShape.isDrawable()){
//				target.drawWireframeShape(newOutlineShape, color, outlineWidth, lineStyle );
//			}
//			//target.setNeedsRefresh(true);  IF NEEDED
//		} catch (VizException e1) {
//			logger.debug("VizException is thrown when trying to do DirectDbQuery.executeQuery to query "+dbName+" table, error="+e1.getMessage()); 
//			e1.printStackTrace();
//		}
//	}
	
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
		 
		if(eventStartTime != null)
			builder.append(eventStartTime.toString().substring(8, 10) +"/" +eventEndTime.toString().substring(11, 13) +eventStartTime.toString().substring(14, 16));
		else
			builder.append("-");
		builder.append("-"); 
		if(eventEndTime != null)
			builder.append(eventEndTime.toString().substring(8, 10) +"/" +eventEndTime.toString().substring(11, 13) +eventEndTime.toString().substring(14, 16)); 
		else
			builder.append("-");
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
	
//	private boolean isCountyName(String countyOrZoneOrStateOrLakeNameWithMarker) {
//		boolean isCounty = false; 
//		String[] nameStringArray = countyOrZoneOrStateOrLakeNameWithMarker.split(FFAConstant.HYPHEN); 
//		if(nameStringArray != null && nameStringArray.length ==2) {
//			if(FFAConstant.COUNTY_MARKER.equals(nameStringArray[1]))
//				isCounty = true; 
//		}
//		return isCounty; 
//	}
//	
//	private boolean isZoneName(String eachUgcValue) {
//		boolean isZone = false; 
//		int ugcIndicator = UGCUtil.getUgcIndicator(eachUgcValue);
//		if(FFAConstant.UGC_ZONE_INDICATOR == ugcIndicator) {
//				isZone = true; 
//		}
//		return isZone; 
//	}
//	
//	private boolean isAllStateName(String eachUgcValue) {
//		boolean isAllState = false; 
//		int ugcIndicator = UGCUtil.getUgcIndicator(eachUgcValue);
//		if(FFAConstant.UGC_ALL_STATE_ZONE_INDICATOR == ugcIndicator) {
//			isAllState = true; 
//		}
//		return isAllState; 
//	}
//	
//	private boolean isGreatLakeName(String eachUgcValue) {
//		boolean isGreatLake = false; 
//		int ugcIndicator = UGCUtil.getUgcIndicator(eachUgcValue);
//		if(FFAConstant.UGC_GREAT_LAKE_INDICATOR == ugcIndicator) {
//			isGreatLake = true; 
//		}
//		return isGreatLake; 
//	}
	
//---------------------------------------------------------------------T456
	
	//there may be multiple EXT for the same officeid-etn-phenomena-significance
	//so a TreeMap used to get the latest EXT which prevails.
//	private Map<String,java.util.TreeMap<java.util.Calendar, FfaRscDataObj>> frdoExtActMap = 
//									new HashMap<String,java.util.TreeMap<java.util.Calendar,FfaRscDataObj>>();
	
	//there may be multiple EXA for the same officeid-etn-phenomena-significance
	//so a list used for later to sum up all additional areas.
//	private Map<String,ArrayList<FfaRscDataObj>> frdoExaActMap = 
//									new HashMap<String, ArrayList<FfaRscDataObj>>();
	
	//for pre-query the database 
	private FfaZoneQueryResult queryResult;
	
	//for storing result of pre-calculation
	private IWireframeShape outlineShape;
	
	//for pre-calculate the IWiredframeShape
	private ZoneResultJob zrJob = new ZoneResultJob("");
	
//	//if it is 1st round in the loop then draw outline since it pre-calculated for all zones
//	private boolean isFirstRound = true;
	
	//Area change flag
	private boolean areaChangeFlag = false;
	
	@Override
	public void queryRecords() throws VizException {
		// this method is almost similar to its super class's queryRecords(), may need to be modified later
		// to use the super class's version for the common part
		
		HashMap<String, com.raytheon.uf.common.dataquery.requests.RequestConstraint> queryList = 
			new HashMap<String, com.raytheon.uf.common.dataquery.requests.RequestConstraint>(resourceData.getMetadataMap());
		
		com.raytheon.uf.viz.core.catalog.LayerProperty prop = new com.raytheon.uf.viz.core.catalog.LayerProperty();
		prop.setDesiredProduct(com.raytheon.uf.viz.core.rsc.ResourceType.PLAN_VIEW);
		prop.setEntryQueryParameters(queryList, false);
		prop.setNumberOfImages(15000); // TODO: max # records ?? should we cap this ?
		
		String script = null;
		script = com.raytheon.uf.viz.core.catalog.ScriptCreator.createScript(prop);
		
		if (script == null)
			return;

		Object[] pdoList = com.raytheon.uf.viz.core.comm.Connector.getInstance().connect(script, null, 60000);
		
		queryResult = new FfaZoneQueryResult();

		for (Object pdo : pdoList) {
			for( IRscDataObject dataObject : processRecord( pdo ) )	{	
				newRscDataObjsQueue.add(dataObject);
//FfaRscDataObj frdo=(FfaRscDataObj)dataObject; System.out.println("_______^^^ queryRecords(): action: "+frdo.actionType+ " vtec: "+frdo.vtecline);				
				queryResult.buildQueryPart2(dataObject);
			}
		}
		
//TODO: handle EXA, EXB here: add newly extended areas
		
		queryResult.populateMap();		   
    	setAllFramesAsPopulated();
	}
	
    @Override
	protected boolean postProcessFrameUpdate() {
    	
    	AbstractEditor ncme = 
    				NcDisplayMngr.getActiveNatlCntrsEditor();
    	
    	zrJob.setRequest(ncme.getActiveDisplayPane().getTarget(), 
    					 getNcMapDescriptor(), null, false, false, null); 
    	 
    	return true;
    }
	
//	private List<FfaRscDataObj> getFFAData2( AwwRecord awwRecord) {
//		
//List<FfaRscDataObj> list = new ArrayList<FfaRscDataObj>();//T456
//				
//		FfaRscDataObj ffaData = null;
//
//		if(FFAUtil.isFFARecord(awwRecord)) {
//			
//			ffaData = new FfaRscDataObj();
//			ffaData.issueTime =new DataTime(awwRecord.getIssueTime());
//			ffaData.reportType=awwRecord.getReportType();
//			ffaData.dataUri=awwRecord.getDataURI();
//			
//			Set<AwwUgc> awwUgcSet = awwRecord.getAwwUGC();
//  
//			for (AwwUgc eachAwwUgc : awwUgcSet) {
///*//T456: functionality moved to addFfaRscDataObj()				
//				if(eachAwwUgc.getAwwVtecLine() != null){ //		TO OBTAIN THE EVENT START AND END TIME
//					for (AwwVtec awwVtec : eachAwwUgc.getAwwVtecLine()) {//This will be looped only once is one to one
//
//						fillEventStartAndEndTime(awwVtec, awwRecord, ffaData); 
//						
//						ffaData.immediateCause = getImmediateCauseValue(awwVtec); 
//					}
//				}
//*/
//				String ugcLine = eachAwwUgc.getUgc();//get the ugc line to find the counties
//				if(!StringUtil.isStringEmpty(ugcLine)){
//					ffaData.ugcCodeStringSet = getCountyUgcSet(eachAwwUgc.getAwwFIPS()); 
//					ffaData = populateCountyOrZoneOrStateOrLakeInfo(ffaData);//get the lat lon too NEW METHOD
//				}
//
//				ffaData.polyNumPoints = eachAwwUgc.getAwwLatLon().size();
//				if(ffaData.polyNumPoints > 0){
//					ffaData.polygonLatLonPointArray = new LatLonPoint[ffaData.polyNumPoints];
//					ffaData.polyLatArray = new float[ffaData.polyNumPoints];
//					ffaData.polyLonArray = new float[ffaData.polyNumPoints];
//					int index;// =warnStatusData.polyNumPoints;
//					for (AwwLatlons awwLatLon : eachAwwUgc.getAwwLatLon()) {
//						LatLonPoint point = new LatLonPoint (awwLatLon.getLat(), 
//								awwLatLon.getLon(),LatLonPoint.INDEGREES);
//						index=awwLatLon.getIndex();
//						ffaData.polyLatArray[index-1]=awwLatLon.getLat();
//						ffaData.polyLonArray[index-1]=awwLatLon.getLon();
//						logger.debug("the index of this lat lon is "+index );
//
//						ffaData.polygonLatLonPointArray[index-1] = point;
//					}
//				}
//				
//list = addFfaRscDataObj(ffaData, awwRecord,eachAwwUgc);//T456: handles EXA/EXB/EXT and vtec stuff
//			}// end of for (AwwUgc eachAwwUgc : awwUgcSet) 
//		}
//
//		return list;//ffaData;
//	}
	
//	private List<FfaRscDataObj> addFfaRscDataObj(FfaRscDataObj ffaData, AwwRecord awwRecord,AwwUgc eachAwwUgc){
//		List<FfaRscDataObj> list = new ArrayList<FfaRscDataObj>();
//		
//		if(eachAwwUgc.getAwwVtecLine() == null)
//			return list;
//		
//		for (AwwVtec awwVtec : eachAwwUgc.getAwwVtecLine()) {
//
//			Calendar esTime = awwVtec.getEventStartTime();
//			Calendar eeTime = awwVtec.getEventEndTime();			
//
//			//end times, some maybe null (if eventstarttime null, use issuetime: Scott )
//			//in getFFAData(), this is done with getfillEventStartAndEndTime(,,);
//			if(eeTime == null){//esTime == null || 
//				logger.info("addFfaRscDataObj():  eventendtime is null");
//				
//				//2011-09-26: checking aww_vtec table in db, we see if the related NEW is in
//				//the db, then actions like CAN can have a valid eventstarttime;
//				//if there is NO eventstarttime then maybe the related NEW is NOT in the db
//				//or purged already. use issuetime for null eventstarttime handled in fillEventStartAndEndTime()  
//				//----old comment 2011-09-20
//				//Therefore we skip over null times for now since if
//				//the related NEW is NOT in we have no way to figure out the valid start time,
//				//otherwise we can add this line: if("NEW".equalsIgnoreCase(awwVtec.getAction()))
//				continue;
//			}			
//			
//			FfaRscDataObj frdo = new FfaRscDataObj();
//			
//			fillEventStartAndEndTime(awwVtec, awwRecord, frdo);//frdo.eventTime = new DataTime(esTime);				
//			//frdo.endTime = new DataTime(eeTime);			
//			
//			//assign fips
//			
//			//frdo.fips = getFips(eachAwwUgc);			
//			
//			//assign immediateCause
//			//in getFFAData(), this is done with ffaData.immediateCause = getImmediateCauseValue(awwVtec); 
//			
//			frdo.immediateCause = getImmediateCauseValue(awwVtec);			
//						
//			//assign actionType, etn, id, phen, sig
//			
//			frdo.actionType = awwVtec.getAction();
//			frdo.eTrackingNo = awwVtec.getEventTrackingNumber();
//			frdo.officeId = awwVtec.getOfficeID();
//			frdo.phenomena = awwVtec.getPhenomena();
//			frdo.significance = awwVtec.getSignificance();	
//			
//			//assign others			
//			 
//			frdo.allStateAbbreviationSet = ffaData.allStateAbbreviationSet;
//			frdo.countyOrZoneOrStateOrLakeLatArray = ffaData.countyOrZoneOrStateOrLakeLatArray;
//			frdo.countyOrZoneOrStateOrLakeLonArray = ffaData.countyOrZoneOrStateOrLakeLonArray;
//			frdo.countyOrZoneOrStateOrLakeLatLonPointList = ffaData.countyOrZoneOrStateOrLakeLatLonPointList;
//			frdo.countyOrZoneOrStateOrLakeNameList = ffaData.countyOrZoneOrStateOrLakeNameList;
//			frdo.greatLakeNameSet = ffaData.greatLakeNameSet;
//			frdo.polygonLatLonPointArray = ffaData.polygonLatLonPointArray;
//			frdo.polyLatArray = ffaData.polyLatArray;
//			frdo.polyLonArray = ffaData.polyLonArray;
//			frdo.ugcCodeStringSet = ffaData.ugcCodeStringSet;
//			frdo.dataUri = ffaData.dataUri;
//			frdo.issueTime = ffaData.issueTime;			
//			frdo.polyNumPoints = ffaData.polyNumPoints;
//			frdo.reportType = ffaData.reportType;			
//			frdo.ugcIndicator = ffaData.ugcIndicator;
//			
//			frdo.vtecline = awwVtec.getVtecLine();//debug use
//			
//			
//				
////handle EXT action			
//if("EXT".equalsIgnoreCase(frdo.actionType)){
//	String key = frdo.getKey();//get4StringConcat(frdo.officeId, frdo.eTrackingNo, frdo.phenomena, frdo.significance);
////System.out.println("000_______ addFfaRscDataObj(): action: "+frdo.actionType+ " vtec: "+frdo.vtecline);	
//	if(frdoExtActMap.containsKey(key)){		
//		frdoExtActMap.get(key).put(awwRecord.getIssueTime(), frdo);
//	}else{
//		
//		java.util.TreeMap<java.util.Calendar, FfaRscDataObj> tmap = new java.util.TreeMap<java.util.Calendar, FfaRscDataObj>();
//		tmap.put(awwRecord.getIssueTime(), frdo);
//		frdoExtActMap.put(key, tmap);
//	}
//	list.add(frdo);
//}
//
////handle EXA action
//if("EXA".equalsIgnoreCase(frdo.actionType)){
///*	
//	String key = frdo.getKey();//get4StringConcat(frdo.officeId, frdo.eTrackingNo, frdo.phenomena, frdo.significance);
//	
//	if(frdoExaActMap.containsKey(key)){
//		
//		frdoExaActMap.get(key).add(frdo);
//	}else{
//		
//		ArrayList<FfaRscDataObj> tlist = new ArrayList<FfaRscDataObj>();
//		tlist.add(frdo);
//		frdoExaActMap.put(key, tlist);
//	}
//*/
//	//EXA should be directly put into the list since 
//	//it MUST have an eventstarttime at this point
//	//and we can just handle it as a NEW.	
//	//But the key is the same as NEW, is this OK???
//	//TODO: re-check this.
//	list.add(frdo);
//}
//
////handle EXB action
//if("EXB".equalsIgnoreCase(frdo.actionType)){
////System.out.println("_______11111 addFfaRscDataObj(): action: "+frdo.actionType+ " vtec: "+frdo.vtecline);
//	//if two segments (EXB and EXT) are used
//	//then we just add EXB to the list since
//	//EXT is handled early: Page 15 of NWSI 10-1703.
//	list.add(frdo);
//
//	//TODO: one segment contains both extended areas and time.
//}
//
// CAN is NOT handled in Nmap2, neither here !
//
// only NEW get added to the list
//if("NEW".equalsIgnoreCase(frdo.actionType)) 	list.add(frdo);
//
//	
//		}//end for-loop
//		
//		return list;
//	}
//	
    public String getFfaRscDataObjKey(FfaRscDataObj f){
    	if(f == null ) return "";
//    	if(true) 
    	return f.dataUri;/*2011-10-05*/    
    	
//		StringBuilder sb = new StringBuilder(f.officeId);
//    	sb.append(f.eTrackingNo);
//    	sb.append(f.phenomena);
//    	sb.append(f.significance);
//    	sb.append(f.actionType).append(f.ugcCodeStringSet.toString());
//    	return sb.toString();
    }
    
    public List<String> getFips(AwwUgc eachAwwUgc){
    	List<String> list = new ArrayList<String>();
		
    	if(eachAwwUgc==null || eachAwwUgc.getAwwFIPS()==null)
    		return list;
		
    	for(AwwFips afips : eachAwwUgc.getAwwFIPS()){
    		StringBuilder sb = new StringBuilder(afips.getFips().substring(0,2));
    		list.add(sb.append( afips.getFips().substring(3) ).toString());//
    	}
    	
    	return list;
    }
    
    //see comment in the else block for the Map values
    public boolean updateFrameData2( IRscDataObject rscDataObj, Map<String, ArrayList<FfaRscDataObj>> fDataMap  ){
    	
    	if( ! (rscDataObj instanceof FfaRscDataObj) ) 	return false;
 				
		FfaRscDataObj frdo = (FfaRscDataObj)rscDataObj;		
//for(String s : frdo.fips) System.out.println("_______ updateFrameData2(): action: "+frdo.actionType+ " fips: "+s);	
		String key = frdo.getKey();
/*	if("EXT".equalsIgnoreCase(frdo.actionType)){//	
		if(frdoExtActMap.containsKey(key)){//this is an EXT			
			if( ! frdoExtActMap.containsKey(key)) return false;
			
			java.util.TreeMap<java.util.Calendar, FfaRscDataObj> map = frdoExtActMap.get(key);
			
			if(map != null){
				//FfaRscDataObj extWrdo = map.get(map.lastKey());//latest issue time prevail
for(FfaRscDataObj extWrdo : map.values())//no bracket needed				
				if(extWrdo != null){
					ArrayList<FfaRscDataObj> list = fDataMap.get(key);
					if( list == null ){	
						list = new ArrayList<FfaRscDataObj>(); 
						fDataMap.put(key, list);
					}					
					list.add(extWrdo);
				}
			}
		}else{ //2011-09-26: EXB may have the same key as EXT so a frdo can in frdoExtActMap yet is EXB
*/		 
			//this is a NEW or EXA/EXB with eventstarttime filled by the decoder
			//so we use a List, and this is the part different with WarnResource
			if(fDataMap.containsKey(key)){
				
				fDataMap.get(key).add(frdo);
			}else{
				
				ArrayList<FfaRscDataObj> list = new ArrayList<FfaRscDataObj>();
				list.add(frdo);
				fDataMap.put(key, list);//NOT datauri
			}
				
			
//		}
    	
    	return true;
    }
    
    
    /**
     * handles the IWireframeShape pre-calculation
     * 
     * @author gzhang     
     */
    private class ZoneResultJob extends org.eclipse.core.runtime.jobs.Job {
    	    	
    	private Map<String,Result> keyResultMap = new java.util.concurrent.ConcurrentHashMap<String,Result>();
    	
    	private IGraphicsTarget target;
    	private IMapDescriptor descriptor;
    	private RGB symbolColor = new RGB (155, 155, 155);
    	
        public class Result {
        	
            public IWireframeShape outlineShape;            
//            public Map<Object, RGB> colorMap;

            private Result(IWireframeShape outlineShape,IWireframeShape nuShape,
                     			IShadedShape shadedShape,Map<Object, RGB> colorMap){
            	
            	this.outlineShape = outlineShape;
                
 //               this.colorMap = colorMap;
            }
        }
    	
    	public ZoneResultJob(String name) {
			super(name);			
		}

		public void setRequest(IGraphicsTarget target, IMapDescriptor descriptor,
        		String query, boolean labeled, boolean shaded, Map<Object, RGB> colorMap){
			
			this.target = target;
			this.descriptor = descriptor;					
			this.run(null);//this.schedule();
			
    	}
    	
    	@Override
		protected org.eclipse.core.runtime.IStatus run(org.eclipse.core.runtime.IProgressMonitor monitor){
    		
 //   		List<Object[]> results;
    		
    		for(AbstractFrameData afd : frameDataMap.values())	{
    			
    			FrameData fd = (FrameData)afd;
    			
    			/**
    			 * list elements FfaRscDataObj.getKey() are all the same
    			 * since they are just different action types: NEW, EXA/EXB 
    			 */    			
    			//for(ArrayList<FfaRscDataObj> list : fd.fDataMap.values()){
    				
    				//if(list==null || list.size()==0) continue;
    				
    				//Collection<Geometry> gw = new ArrayList<Geometry>();
    				
    				for(FfaRscDataObj frdo : fd.ffaDataMap.values()){//list){    					
    					Collection<Geometry> gw = new ArrayList<Geometry>();//2011-10-05 FrameData's fDataMap not used   					
    					for(int i=0; i<frdo.fips.size(); i++){
    						
    						for(ArrayList<Object[]> zones : queryResult.getZoneResult(frdo.fips.get(i))){
    							
    							if(zones == null) continue;
//System.out.println("_______++++++---run(): action: "+frdo.actionType+ " fips: "+frdo.fips.get(i));    							
    							WKBReader wkbReader = new WKBReader();
    							
    							for (Object[] result : zones) {
    								
    								int k = 0;
    								byte[] wkb1 = (byte[]) result[k];
    								
    								com.vividsolutions.jts.geom.MultiPolygon countyGeo = null;
    								
    								try{
    									
    									countyGeo= (com.vividsolutions.jts.geom.MultiPolygon)wkbReader.read(wkb1);
    									
    									if ( countyGeo != null && !countyGeo.isEmpty()){
    										gw.add(countyGeo);
    									}
    									
    								}catch(Exception e){
    									logger.info("Exception: "+e.getMessage());
    								}
    							}
    							
    						}
    						
    					}
    				//}2011-10-05
    				if(gw.size() == 0) 
						continue;
    				else
    					keyResultMap.put(frdo.getKey()/*list.get(0).getKey()*/, new Result(getEachWrdoShape(gw),null,null,null));
    				}//2011-10-05: replace above closing bracket	
    			//}// end of for(ArrayList<FfaRscDataObj> list : fd.fDataMap.values())
    		}
    		
    		return org.eclipse.core.runtime.Status.OK_STATUS;
    	}
    	
    	public IWireframeShape getEachWrdoShape(Collection<Geometry> gw){
	    	
	    	IWireframeShape newOutlineShape = target.createWireframeShape(false, descriptor, 0.0f);
			
			JTSCompiler jtsCompiler = new JTSCompiler(null,newOutlineShape, descriptor, PointStyle.CROSS);
	    	
			com.vividsolutions.jts.geom.GeometryCollection gColl=
				(com.vividsolutions.jts.geom.GeometryCollection) new com.vividsolutions.jts.geom.GeometryFactory().buildGeometry( gw );
			
			try{	
				gColl.normalize();
				
				jtsCompiler.handle(gColl, symbolColor);				
						
				newOutlineShape.compile();	
											
			}catch (Exception e) {	logger.info("_____Error: "+e.getMessage());	}
	    	
	    	return newOutlineShape;
	    }
    }
    
    
    private void drawZoneOutline2(FfaRscDataObj ffaData, IGraphicsTarget target, RGB color, int outlineWidth, 
			  LineStyle lineStyle, PaintProperties paintProps, Envelope env){
    	
    	ZoneResultJob.Result result = zrJob.keyResultMap.get(ffaData.getKey());
    	
    	if (result != null) {
    		if (outlineShape == null) {   
    			outlineShape = result.outlineShape;   
    		}else{									 
		//if ( outlineShape.hashCode() != result.outlineShape.hashCode()) { //TODO: do NOT use outlineShape.hashCode !!!   
			//outlineShape.dispose(); 
    			outlineShape = result.outlineShape;
		//}
    		}    
    	}else {
    		return;
    	}
    	
    	if (outlineShape != null && outlineShape.isDrawable() ){
    		try{
    			target.drawWireframeShape(outlineShape,  color,outlineWidth,lineStyle );
    		} catch (VizException e) {
    			logger.info("VizException in drawCountyOutline2() of FFAResource"); 
    			//e.printStackTrace();
    		}

    	} else if (outlineShape == null){
		
  		//target.setNeedsRefresh(true);
    	}
    	
    }
    
	/**
	 *  called in the constructor.
	 */
	private void addRDChangedListener(){
		AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
		editor.addRenderableDisplayChangedListener(this.new FfaDCListener());
	}
    
    /**
	 * change the flag so outlineShape can be re-calculated
	 */
	private class FfaDCListener implements com.raytheon.uf.viz.core.IRenderableDisplayChangedListener{

		@Override
		public void renderableDisplayChanged(com.raytheon.uf.viz.core.IDisplayPane pane,
				com.raytheon.uf.viz.core.drawables.IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
			
			areaChangeFlag = true;
			
		}
		
	}
	
    /**
     * avoid null pointers exception in super class  
     */
    @Override
	protected long getDataTimeMs(IRscDataObject rscDataObj) {
		//			long dataTimeMs = rscDataObj.getDataTime().getValidTime().getTime().getTime();
		if(rscDataObj == null)
			return 0;
		
    	java.util.Calendar validTimeInCalendar = null; 
		DataTime dataTime = rscDataObj.getDataTime(); 
		if(dataTime != null) {
			validTimeInCalendar = dataTime.getValidTime(); 
			
		} else {
			logger.info("===== find IRscDataObject rscDataObj.getDataTime() return NULL!!!"); 
		}
		long dataTimeInMs = 0; 
		if(validTimeInCalendar != null)
			dataTimeInMs = validTimeInCalendar.getTimeInMillis(); 
		return dataTimeInMs; 
	}
   
    @Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.ffaDataMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}