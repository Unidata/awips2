//<<<<<<< .working
package gov.noaa.nws.ncep.viz.common.soundingQuery;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer.DataType;
import javax.measure.unit.Unit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A general-purpose, user-friendly interface to the NcSoundingDataRequest 
 * uengine script. This is intended to replace the NcSoundingQuery class  
 * when nsharp is migrated to use it.  No changes were made to the uengine
 * script.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- -----------  --------------------------
 * 09/20/2011     #459     Greg Hull    Initial creation 
 * 02/21/2012              Chin Chen    Modified several areas for performance improvement
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */

public class NcSoundingQuery2 {
	
	// TODO : change this to an enum or preferably a pluginName
	private String pluginName = null;
	
	private Boolean merge = false;
	
	private String level = "-1";

	// TODO : add support requested parameters
	// Cyclical dependencies when AbstractMetParameter was in viz.common.
	//private List<AbstractMetParameter> requestedMetParameters = null;
	
	private Date refTime = null;

	private int  forecastHour = 0; 
	
	private TimeRange timeRange = null;
	// if true then the timeRange is used for the valid time of the data. 
	private Boolean   applyTimeRangeToValidTime = false; // not implemented
	
	private List<Coordinate> latLonCoords= null;

	private List<String> stationIds = null;
	
	private List<String> stationNums = null;
	
	private List<Long> rangeTimeList = null;
	
	// only applies to UAIR data (true?)
	// TODO : change this to be a list of levelTypes. 
	private NcSoundingLayer.DataType  uairLevelType = DataType.ALLDATA;
	
	// only for "ncgrib", "grib" and "modelsounding" plugins
	// (for modelsounding this is the reportType in the database)
	private String modelName = null;
	
	private List<String> supportedPlugins = 
			Arrays.asList( new String[] {
			"ncuair", "uair", 
//			"drop", // what is the drop sounding plugin name?
//			"tamdar", // plugin name? 
			"bufrua",			
			"modelsounding", // for NAMSND and GFSSND
			"ncgrib", "grib" 
	} );
	
	private List<String> soundingTypesForPlugins = 
		Arrays.asList( new String[] {
			"NCUAIR", "UAIR", 
			//"DROP",  // are DROP and TAMDAR really supported now? 
			//"TAMDAR",  
			"BUFRUA", 
			"modelsounding", // reportTypes of "NAMSND", "GFSSND", "RUC2SND", "RUCPTYPSND",
			"ncgrib", "grib" // no soundingType, uengine uses the pluginName itself 
	} );
	
	// A few convienence constructors for common constraints.
	public NcSoundingQuery2( String plugin ) throws Exception {
		this( plugin, false, null );
	}
	
	public NcSoundingQuery2( String plugin, Boolean m ) throws Exception {
		this( plugin, m, null );
	}
	
	public NcSoundingQuery2( String plgn, Boolean m, String lvl ) throws Exception {
		if( !supportedPlugins.contains( plgn ) ) {
			System.out.println("NcSoundingQuery2 doesn't support plugin: "+plgn );
			throw new Exception("NcSoundingQuery2 doesn't support plugin: "+plgn);
		}
		pluginName = plgn;
		merge = m;
		level = lvl;
	}

//	public void setRequestedParameters( List<AbstractMetParameter> reqParams ) {
//		requestedMetParameters = reqParams; // copy list?
//	}
	

	public void setLatLonConstraints( List<Coordinate> coords ) {
		if( stationNums != null ) {
			System.out.println("LatLon constraint is replacing stationNums constraint");
			stationNums = null;
		}
		else if( stationIds != null ) {
			System.out.println("Station Numbers constraint is replacing stationIds constraint");
			stationIds = null;
		}
		latLonCoords = new ArrayList<Coordinate>( coords );
	}
	
	public List<Long> getRangeTimeList() {
		return rangeTimeList;
	}

	public void setRangeTimeList(List<Long> rangeTimeList) {
		this.rangeTimeList = rangeTimeList;
	}

	// TODO : could allow for both stationId and stationNum constraint if needed. 
	public void setStationIdConstraints( List<String> stnIds ) {
		if( stationNums != null ) {
			System.out.println("Station Ids constraint is replacing stationNums constraint");
			stationNums = null;
		}
		else if( latLonCoords != null ) {
			System.out.println("Station Numbers constraint is replacing LatLon constraint");
			latLonCoords = null;
		}
		stationIds = new ArrayList<String>( stnIds );
	}

	public void setStationNumConstraints( List<String> stnNums ) {
		if( stationIds != null ) {
			System.out.println("Station Numbers constraint is replacing stationIds constraint");
			stationIds = null;
		}
		else if( latLonCoords != null ) {
			System.out.println("Station Numbers constraint is replacing LatLon constraint");
			latLonCoords = null;
		}
		stationNums = new ArrayList<String>( stnNums );		
	}
	
//	public setSoundingTypeConstraint( ); // have to set in constructor
	
	// 
	public void setRefTimeConstraint( Date rTime ) {
		if( timeRange != null ) {
			System.out.println("Ref Time constraint is overriding timeRange constraint.");
			timeRange = null;
		}
		refTime = rTime;
	}
	
	//
	public void setForecastHourConstraint( int fcstHr ) {
		// The refTime must be set in order for fcstHr to be used. Both are used to 
		// create a valid time passed to the script.
		// (this need not be a real restriction but the current implementation won't allow it.)
		// 
		if( refTime != null ) {
			System.out.println("Ref Time should be set before the forecast hour.");			
		}
		
		forecastHour = fcstHr;		
	}
	
	// Add additional constraints for the validTime and/or the forecast hour (used with the ref time), 
	// also 
	// It wasn't obvious from the original NcSoundingQuery

	// TODO : add flag whether the timeRange applies to the valid time or the refTime.
	// 
	public void setTimeRangeConstraint( TimeRange tRange, Boolean valTime ) {
		setTimeRangeConstraint( tRange );
		applyTimeRangeToValidTime = valTime;
	}
	
	public void setTimeRangeConstraint( TimeRange tRange ) { 
		//if( refTime != null ) {
		//	System.out.println("TimeRange constraint is overriding refTime constraint.");
		//	refTime = null;
		//}
		timeRange = tRange;
	}

	// this is used by 
	public void setModelName( String mdlName ) {
		modelName = mdlName;
		
		if( !pluginName.equals("ncgrib") && 
			!pluginName.equals("grib" ) &&
			!pluginName.equals("modelsounding") ) {
			
			System.out.println("modelName is not applicable for plugin:"+pluginName );
		}
	}
	
	// 
	public void setLevelType( NcSoundingLayer.DataType uaLvl ) {
		uairLevelType = uaLvl;
	}
	
	public void setMerge( Boolean m ) {
		merge = m;
	}
	
	public void setLevelConstraint( String lvl ) {
		// TODO : check if this is a non-standard level and print warning if 
		// merge is not true
		level = lvl;
	}
	
	public NcSoundingCube query() {// throws Exception {
		// change this to allow returning all available profiles.
		if( latLonCoords == null && stationIds == null && stationNums == null ) {
			System.out.println("The query must have either a lat/lon, or stations constraints.");
			return null;
		}
		// 
		else if( refTime == null && timeRange == null ) {
			System.out.println("The query must have either a timeRange, or refTime constraint.");
			return null;
		}
		
    	NcSoundingCube cube = null;
    	StringBuilder query = new StringBuilder();

    	query.append("import NcSoundingDataRequest\n");
    	query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
    	
    	// TODO : modify the uengine script to accept the pluginName instead of
    	// a soundingType
    	//
    	if( pluginName.equals( "ncuair" ) ) {
        	query.append("sndRq.setSndType('NCUAIR')\n");    		    		
    	}
    	else if( pluginName.equals( "uair" ) ) {
        	query.append("sndRq.setSndType('UAIR')\n");    		    		    		
    	}
    	else if( pluginName.equals( "bufrua" ) ) {
        	query.append("sndRq.setSndType('BUFRUA')\n");    		    		    		
    	}
    	else if( pluginName.equals( "tamdar" ) ) {
        	query.append("sndRq.setSndType('TAMDAR')\n");    		    		    		
    	}
    	else if( pluginName.equals("modelsounding") ) {
    		// the uengine script uses a soundingType which is the based
    		// on the pluginName and the modelName/reportType
    		if( modelName == null ) {
    			System.out.println("ModelName is not set for modelsounding plugin?");
    		}
    		else if( modelName.startsWith("NAM") ||
    				 modelName.startsWith("ETA") ) {
    			query.append("sndRq.setSndType('NAMSND')\n");
    		}
    		else if( modelName.startsWith("GFS") ) {
    			query.append("sndRq.setSndType('GFSSND')\n");
    		}

    		// RUC currently not supported but pass it thru anyway
    		else if( modelName.startsWith("RUC2") ) {
    			query.append("sndRq.setSndType('RUC2SND')\n");
    		}
    		// ?? is this right
    		else if( modelName.startsWith("RUCPTY") ) {
    			query.append("sndRq.setSndType('RUCPTYSND')\n");
    		}
    		else {
    			System.out.println("Unrecognized ModelName for modelsounding plugin: "+modelName);
    		}
    	}
    	// for 
    	else if( pluginName.equals( "ncgrib" ) ||
    			 pluginName.equals( "grib" ) ) {
         	query.append("sndRq.setPluginName('" +pluginName + "')\n");

         	// sanity check that modelName is set?
        	if( modelName != null ) {
            	query.append("sndRq.setModelName('" +modelName+ "')\n");
        	}
        	else {
        		System.out.println("ModelName is not set for grib or ncgrib plugin???");
        	}
    	}
    	else {
			System.out.println("NcSoundingQuery2 doesn't support plugin: "+pluginName );
    	}


    	query.append("sndRq.setDataType('" + uairLevelType.toString() + "')\n");

    	if( refTime != null ) {
    		query.append("sndRq.setRefTime(" +refTime.getTime() + "L)\n"); 
    		
    		// use the forecast hour to create the valid time. 
    		// TODO : change the uengine script to have a separate 
    		// method to set the validTime (or better, the forecast hour)
    		// instead of overloading the setValidTimeStart.
    		//
    		DataTime validTime = new DataTime( refTime, forecastHour );    		
    		query.append("sndRq.setValidTimeStart(" + 
    				validTime.getValidTime().getTimeInMillis()+ "L)\n"); 
    		
    		query.append("sndRq.setValidTimeEnd(0L)\n");
    		System.out.println("refT="+refTime.toString()+" StartT="+ validTime.getValidTime().getTime().toString()+" StartT(GMT)="+ validTime.getValidTime().getTime().toGMTString());
    	}
    	else if( timeRange != null ) {
    		// TODO : should set the refTime to 0 but since NcSoundingQuery
    		// is setting this to the startTime I am having this do the same.
    		//
//    		query.append("sndRq.setRefTime(0L)\n"); 
    		query.append("sndRq.setRefTime(" + timeRange.getStart().getTime()+ "L)\n"); 
    		query.append("sndRq.setValidTimeStart(" + timeRange.getStart().getTime()+ "L)\n"); 
    		query.append("sndRq.setValidTimeEnd(" + timeRange.getEnd().getTime() + "L)\n");
    		System.out.println("refT="+timeRange.getStart().toGMTString()+" StartT="+ timeRange.getStart().toGMTString()+ " endT="+timeRange.getEnd().toGMTString());
    	}
    		
    	if(rangeTimeList != null){
    		String rtStr="[";
    		for(int i=0; i < rangeTimeList.size(); i ++){
    			rtStr = rtStr +  rangeTimeList.get(i)+"L";
    			if(i <rangeTimeList.size()-1)
    				rtStr = rtStr + "," ;
    		}
    		rtStr = rtStr + "]";
    		query.append("sndRq.setRangeTimeArr("+rtStr+")\n");
    	}
    	query.append("sndRq.setMerge("+ (merge ? "1" : "0")+ ")\n");
    	
    	query.append("sndRq.setLevel('" + level + "')\n");
    	query.append("sndRq.setNcSoundingLayer2(1)\n");
    	
    	// set either lat/lon, stationId or stationNum 
    	if( latLonCoords != null ) {
    		String latLonStr = "[";
    		//double maxLat= 0;double minLat=0; double maxLon=0; double minLon=0;
    		for( int i=0 ; i<latLonCoords.size() ; i++ ) {
    			Coordinate latlon = latLonCoords.get(i);
    			
    			if( i == latLonCoords.size()-1 ) {
        			latLonStr = latLonStr +  latlon.y + ","+ latlon.x + "]";
    			}
    			else {
    				latLonStr = latLonStr +  latlon.y + ","+ latlon.x + ",";    			
    			}
    		}
    		System.out.println("1query stn siz="+latLonCoords.size());/*\+ " maxLon ="+maxLon+" minLon="+minLon+ " maxLat ="+maxLat+" minLat="+minLat);*/
    		query.append("return sndRq.getSoundingData2ByLatLonArray("+latLonStr+")");
    	}
    	else if( stationIds != null ) {
        	String stnStr = "[";
        	
        	for( int i=0; i < stationIds.size(); i ++){

        		if( i == stationIds.size()-1 ) {
        			stnStr = stnStr + "'" + stationIds.get(i) + "'" + "]";
        		}
        		else {
        			stnStr = stnStr + "'" + stationIds.get(i) + "'" + ",";
        		}
        	}
        	//System.out.println("2query stn siz="+stationIds.size());
        	query.append("return sndRq.getSoundingData2ByStnIdArray("+stnStr+")");
        		
    	}
    	else {
    		return cube;
    		/*if( stationNums != null ) {
        	String stnStr = "[";
        	
        	for( int i=0; i < stationNums.size(); i ++){

        		if( i == stationNums.size()-1 ) {
        			stnStr = stnStr + "'" + stationNums.get(i) + "'" + "]";
        		}
        		else {
        			stnStr = stnStr + "'" + stationNums.get(i) + "'" + ",";
        		}
        	}
        	//System.out.println("3query stn siz="+stationNums.size());
        	query.append("return sndRq.getSoundingDataByStnNumArray("+stnStr+")");*/
    	}

    	System.out.println(query.toString());
    	
    	Object[] pdoList;
		try {
			//query DB from EDEX
			NcUnits.register();
			long t01 = System.currentTimeMillis();
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			if (pdoList[0] instanceof NcSoundingCube)
				cube = (NcSoundingCube) pdoList[0];
			long t02 = System.currentTimeMillis();
			//System.out.println("return from edex...takes "+(t02-t01)+" msec with # of profile = "+ cube.getSoundingProfileList().size());
			/*for(int i =0; i < cube.getSoundingProfileList().size(); i++){
				System.out.println("lat/lon="+ cube.getSoundingProfileList().get(i).getStationLatitude()+"/"+cube.getSoundingProfileList().get(i).getStationLongitude()+
						" temp="+cube.getSoundingProfileList().get(i).getSoundingLyLst2().get(0).getTemperature()+" dewp="+cube.getSoundingProfileList().get(i).getSoundingLyLst2().get(0).getDewpoint()+" press="+
						cube.getSoundingProfileList().get(i).getSoundingLyLst2().get(0).getPressure() + " height="+cube.getSoundingProfileList().get(i).getSoundingLyLst2().get(0).getGeoHeight()+
						" windSp="+cube.getSoundingProfileList().get(i).getSoundingLyLst2().get(0).getWindSpeed()+" windDir="+cube.getSoundingProfileList().get(i).getSoundingLyLst2().get(0).getWindDirection()+
						" omega="+cube.getSoundingProfileList().get(i).getSoundingLyLst2().get(0).getOmega());
			}*/
		
		} catch (VizException e) {
			System.out.println("query() failed: "+e.getMessage() );
//			e.printStackTrace();
			return cube;
		}
		return cube;

	}
	

//	public static NcSoundingTimeLines soundingTimeLineQuery (String sndType){
//		NcSoundingTimeLines timeLines = null;
//		StringBuilder query = new StringBuilder();
//		query.append("import NcSoundingDataRequest\n");
//		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
//		query.append("sndRq.setSndType('"+sndType+"')\n");
//		query.append("return sndRq.getSoundingTimeLine()");
//		//System.out.println(query.toString());
//		Object[] pdoList;
//		try {
//			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
//			if (pdoList[0] instanceof NcSoundingTimeLines)
//				timeLines = (NcSoundingTimeLines) pdoList[0];
//			//else
//			//	System.out.println((String)pdoList[0]);
//
//			//System.out.println("return from edex...");
//			return timeLines;
//		}catch (VizException e) {
//			System.out.println("soundingTimeLineQuery failed");
//			return timeLines;
//		}	
//	}
//
//	public static NcSoundingTimeLines mdlSoundingTimeLineQuery (String sndType, String tableName){
//		NcSoundingTimeLines timeLines = null;
//		StringBuilder query = new StringBuilder();
//		query.append("import NcSoundingDataRequest\n");
//		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
//		query.append("sndRq.setSndType('"+sndType+"')\n");
//		query.append("sndRq.setTableName('"+tableName+"')\n");
//		query.append("return sndRq.getMdlSoundingTimeLine()");
//		//System.out.println(query.toString());
//		Object[] pdoList;
//		try {
//			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
//			if (pdoList[0] instanceof NcSoundingTimeLines)
//				timeLines = (NcSoundingTimeLines) pdoList[0];
//			//else
//			//	System.out.println((String)pdoList[0]);
//
//			//System.out.println("return from edex...");
//			return timeLines;
//		}catch (VizException e) {
//			System.out.println("soundingTimeLineQuery failed");
//			return timeLines;
//		}	
//	}
//	public static NcSoundingTimeLines soundingRangeTimeLineQuery (String sndType, String refTime){
//		NcSoundingTimeLines timeLines = null;
//		StringBuilder query = new StringBuilder();
//		query.append("import NcSoundingDataRequest\n");
//		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
//		query.append("sndRq.setSndType('"+sndType+"')\n");
//		query.append("sndRq.setRefTimeStr('"+refTime+"')\n");
//		query.append("return sndRq.getSoundingRangeTimeLine()");
//		//System.out.println(query.toString());
//		Object[] pdoList;
//		try {
//			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
//			if (pdoList[0] instanceof NcSoundingTimeLines)
//				timeLines = (NcSoundingTimeLines) pdoList[0];
//
//			//System.out.println("return from edex...");
//			return timeLines;
//		}catch (VizException e) {
//			System.out.println("soundingRangeTimeLineQuery failed");
//			return timeLines;
//		}	
//	}
//	
//	public static NcSoundingTimeLines mdlSoundingRangeTimeLineQuery (String sndType, String refTime, String tableName){
//		NcSoundingTimeLines timeLines = null;
//		StringBuilder query = new StringBuilder();
//		query.append("import NcSoundingDataRequest\n");
//		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
//		query.append("sndRq.setSndType('"+sndType+"')\n");
//		query.append("sndRq.setTableName('"+tableName+"')\n");
//		query.append("sndRq.setRefTimeStr('"+refTime+"')\n");
//		query.append("return sndRq.getMdlSoundingRangeTimeLine()");
//		//System.out.println(query.toString());
//		Object[] pdoList;
//		try {
//			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
//			if (pdoList[0] instanceof NcSoundingTimeLines)
//				timeLines = (NcSoundingTimeLines) pdoList[0];
//
//			//System.out.println("return from edex...");
//			return timeLines;
//		}catch (VizException e) {
//			System.out.println("soundingRangeTimeLineQuery failed");
//			return timeLines;
//		}	
//	}
//	public static NcSoundingStnInfoCollection soundingStnInfoQuery (String sndType,String selectedSndTime){
//		NcSoundingStnInfoCollection stnInfos = null;
//		StringBuilder query = new StringBuilder();
//		query.append("import NcSoundingDataRequest\n");
//		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
//		query.append("sndRq.setSndType('"+sndType+"')\n");
//		query.append("sndRq.setTimeLine('"+selectedSndTime+"')\n");
//		query.append("return sndRq.getSoundingStnInfoCol()");
//		//System.out.println(query.toString());
//		Object[] pdoList;
//		try {
//			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
//			if(pdoList[0] instanceof NcSoundingStnInfoCollection)
//				stnInfos = (NcSoundingStnInfoCollection) pdoList[0];
//
//			//System.out.println("return from edex...stnInfos ");
//		    return stnInfos;
//		} catch (VizException e) {
//			System.out.println("soundingStnInfoQuery failed");
//			e.printStackTrace();
//			return stnInfos;
//		}
//	}
//	public static Object[] soundingModelNameQuery(String pluginName){
//		StringBuilder query = new StringBuilder();
//		query.append("import NcSoundingDataRequest\n");
//		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
//		query.append("return sndRq.getSoundingModelNames('"+pluginName+"')");
//		Object[] pdoList;
//		try {
//			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
//						//System.out.println("return from edex...soundingModelNameQuery ");
//			//for(Object str: pdoList){
//			//	System.out.println("model name:"+str);
//			//}
//			return pdoList;
//		} catch (VizException e) {
//			System.out.println("soundingModelNameQuery failed");
//			e.printStackTrace();
//			return null;
//		}
//	}	
//	public static Calendar convertTimeStrToCalendar(String intimeStr){
//		int year, mon, date, hr;
//		String timeStr = new String(intimeStr);
//		int index = timeStr.indexOf('-');
//		
//		if (index >= 4 ){
//			year = Integer.parseInt(timeStr.substring(index-4, index));
//			timeStr = timeStr.substring(index+1);
//			index = timeStr.indexOf('-');
//			if(index >= 2 ){
//				mon = Integer.parseInt(timeStr.substring(index-2, index));
//				timeStr = timeStr.substring(index+1);
//				index = timeStr.indexOf(' ');
//				if(index >= 2 ){
//					date = Integer.parseInt(timeStr.substring(index-2, index));
//					timeStr = timeStr.substring(index+1);
//					//index = refTimeStr.indexOf(':');
//					if(timeStr.length() >= 2 ){
//						hr = Integer.parseInt(timeStr.substring(0, 2));
//						Calendar cal;
//						cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));		
//						// reset time
//						cal.setTimeInMillis(0);
//						// set new time
//						cal.set(year, mon-1, date, hr, 0,0);
//						return cal;
//					}
//				}
//			}
//		}
//		return null;
//	}
}
