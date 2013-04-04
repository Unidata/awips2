package gov.noaa.nws.ncep.viz.common.soundingQuery;
/**
 * 
 * 
 * 
 * This java class performs the NSHARP pfc sounding data query functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 11/1/2010	362			Chin Chen	Initial coding
 * 12/16/2010   362         Chin Chen   add support of BUFRUA observed sounding and PFC (NAM and GFS) model sounding data
 * 02/15/2012               Chin Chen   modify several sounding query algorithms for better performance
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer2;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingModel;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.MdlSndType;

import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;


public class NcSoundingQuery {
	public static int counter = 1;
	public static String NCGRIB_PLUGIN_NAME = "ncgrib";
	public static String GRIB_PLUGIN_NAME = "grid";
	public static long convertRefTimeStr(String refTimeStr) {
		int year, mon, date, hr;
		int index = refTimeStr.indexOf('-');
		if (index >= 4 ){
			year = Integer.parseInt(refTimeStr.substring(index-4, index));
			refTimeStr = refTimeStr.substring(index+1);
			index = refTimeStr.indexOf('-');
			if(index >= 2 ){
				mon = Integer.parseInt(refTimeStr.substring(index-2, index));
				refTimeStr = refTimeStr.substring(index+1);
				index = refTimeStr.indexOf(' ');
				if(index >= 2 ){
					date = Integer.parseInt(refTimeStr.substring(index-2, index));
					refTimeStr = refTimeStr.substring(index+1);
					//index = refTimeStr.indexOf(':');
					if(refTimeStr.length() >= 2 ){
						hr = Integer.parseInt(refTimeStr.substring(0, 2));
						Calendar refTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));		
						// reset time
						refTimeCal.setTimeInMillis(0);
						// set new time
						refTimeCal.set(year, mon-1, date, hr, 0,0);
						//System.out.println("set time Str " + refTimeStr + " cal time in GMT " + refTimeCal.getTime().toGMTString() + " in msec = " + refTimeCal.getTimeInMillis());
						return refTimeCal.getTimeInMillis();
					}
				}
			}
		}
		return 0;
	}
	public static String convertSoundTimeDispStringToRangeStartTimeFormat(String displayStr){
		//Note: time line display string has format of, e.x. 111208/2130V003, convert to 2011-12-08 21:30:00.
		//first 2 digits is year
		String rangeStartStr= "20"+displayStr.substring(0, 2);
		//3rd and 4th digits is month
		rangeStartStr = rangeStartStr + "-"+ displayStr.substring(2, 4);
		//5th and 6th digits is day
		rangeStartStr = rangeStartStr + "-"+ displayStr.substring(4, 6);
		//8th and 9th digits is hour, 10th and 11th are min, and seconds should be 0s
		rangeStartStr = rangeStartStr + " "+ displayStr.substring(7, 9)+":"+ displayStr.substring(9, 11)+ ":00";
		return rangeStartStr;
	}
	
	//return  refTimeStr   
	public static String convertSoundTimeDispStringToRefTime(String displayStr){
		//Note: time line display string has format of, e.x. 111208/2100V003, convert to 2011-12-08 21:00:00.
		String year, mon, day, hour, min, Vhour;
		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		//first 2 digits is year
		year ="20"+displayStr.substring(0, 2);
		//3rd and 4th digits is month
		mon =displayStr.substring(2, 4);
		//5th and 6th digits is day
		day=displayStr.substring(4, 6);
		//8th and 9th digits is hour
		hour= displayStr.substring(7, 9);
		//10-11 is minutes, second should be 0s
		min = displayStr.substring(9, 11);
		//Vhour starting digit 13
		Vhour = displayStr.substring(12);
		cal.set(Integer.parseInt(year), Integer.parseInt(mon)-1,Integer.parseInt(day), Integer.parseInt(hour),Integer.parseInt(min));
		//from VXXX and rangeStart time get  referTime
		long reftimeMs = cal.getTimeInMillis() - (Integer.parseInt(Vhour)*3600000);
		cal.setTimeInMillis(reftimeMs);
		String ref = String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:00",  cal);
		
		
		return ref;
	}
	public static NcSoundingCube soundingQueryByLatLon(String refTimeStr,List<Coordinate> coords, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		if(coords.size()>0){
			double[][] latLon = new double[coords.size()][2];
			for (int i=0; i< coords.size(); i++){
				latLon[i][0]= (double)coords.get(i).y; //lat
				latLon[i][1]= (float)coords.get(i).x; //lon
			}	
			return (soundingQueryByLatLon(refTimeStr, latLon, sndType, dataType, merge, level));
		}
		return null;
	}
	public static NcSoundingCube soundingQueryByLatLon(String refTimeStr, double[][] latLon, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		if(latLon.length <=0 )
			return null;
		//convert refTimeStr to in msec unit and query
		long refTime = convertRefTimeStr(refTimeStr);

		return soundingQueryByLatLon(refTime,latLon, sndType, dataType, merge, level);

	}
	public static NcSoundingCube soundingQueryByLatLon(long refTime, List<Coordinate> coords, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		if(coords.size()>0){
			double[][] latLon = new double[coords.size()][2];
			for (int i=0; i< coords.size(); i++){
				latLon[i][0]= coords.get(i).y; //lat
				latLon[i][1]= coords.get(i).x; //lon
			}	
			return (soundingQueryByLatLon(refTime, latLon, sndType, dataType, merge, level));
		}
		return null;
	}	
	public static NcSoundingCube soundingQueryByLatLon(long refTime, double[][] latLon, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		return genericSoundingQueryByLatLon(refTime, 0,0, latLon, sndType, dataType, merge, level);
	}

	public static NcSoundingCube soundingQueryByLatLonForMetParams(long refTime, List<Coordinate> coords, String sndType, NcSoundingLayer2.DataType dataType, boolean merge, String level) {
		if(coords.size()>0){
			float[][] latLon = new float[coords.size()][2];
			for (int i=0; i< coords.size(); i++){
				latLon[i][0]= (float)coords.get(i).y; //lat
				latLon[i][1]= (float)coords.get(i).x; //lon
			}	
			return (soundingQueryByLatLonForMetParams(refTime, latLon, sndType, dataType, merge, level));
		}
		return null;
	}

	public static NcSoundingCube soundingQueryByLatLonForMetParams(long refTime, float[][] latLon, String sndType, NcSoundingLayer2.DataType dataType, boolean merge, String level) {
		return genericSoundingQueryByLatLonForMetParams(refTime, 0,0, latLon, sndType, dataType, merge, level);
	}

	public static NcSoundingCube soundingQueryByLatLon(long refTimeStart, long refTimeEnd,List<Coordinate> coords, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		if(coords.size()>0){
			double[][] latLon = new double[coords.size()][2];
			for (int i=0; i< coords.size(); i++){
				latLon[i][0]= (double)coords.get(i).y; //lat
				latLon[i][1]= (double)coords.get(i).x; //lon
			}	
			return (soundingQueryByLatLon(refTimeStart,refTimeEnd, latLon, sndType, dataType, merge, level));
		}
		return null;
	}
	public static NcSoundingCube soundingQueryByLatLon(long refTimeStart,  long refTimeEnd,double[][] latLon, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		return genericSoundingQueryByLatLon(refTimeStart, refTimeStart,refTimeEnd, latLon, sndType, dataType, merge, level);
	}

	public static NcSoundingCube pfcSoundingQueryByLatLon(String refTimeStr,String validTimeStr, List<Coordinate> coords, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		if(coords.size()>0){
			double[][] latLon = new double[coords.size()][2];
			for (int i=0; i< coords.size(); i++){
				latLon[i][0]= (double)coords.get(i).y; //lat
				latLon[i][1]= (double)coords.get(i).x; //lon
			}	
			return (pfcSoundingQueryByLatLon(refTimeStr,validTimeStr, latLon, sndType, dataType, merge, level));
		}
		return null;
	}
	public static NcSoundingCube pfcSoundingQueryByLatLon(String refTimeStr,String validTimeStr, double[][] latLon, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		if(latLon.length <=0 )
			return null;
		//convert refTimeStr to in msec unit and query
		long refTime = convertRefTimeStr(refTimeStr);
		long validTime = convertRefTimeStr(validTimeStr);

		return genericSoundingQueryByLatLon(refTime,validTime,0, latLon, sndType, dataType, merge, level);

	}
	public static NcSoundingCube pfcSoundingQueryByLatLon(long refTime, long validTime, double[][] latLon, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		return (genericSoundingQueryByLatLon( refTime,  validTime,  validTime,  latLon,  sndType, dataType,  merge,  level) );
	}
	public static NcSoundingCube pfcSoundingQueryByLatLon(long refTime, long validTimeStart, long validTimeEnd, double[][] latLon, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {

		return (genericSoundingQueryByLatLon( refTime,  validTimeStart,  validTimeEnd,  latLon,  sndType, dataType,  merge,  level) );

	}
	//Chin: 2/14/2012: support one query for multiple sounding time of a same station lat/lon
	public static NcSoundingCube pfcSoundingQueryByRangeTimeArray(long refTime, long[] soundingRangeTime, double lat, double lon, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {

		
		NcSoundingCube cube = null;
		StringBuilder query = new StringBuilder();

		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('" +sndType + "')\n");
		query.append("sndRq.setDataType('" +dataType.toString() + "')\n");
		query.append("sndRq.setRefTime(" +refTime+ "L)\n"); 
		query.append("sndRq.setLatLonArr([" +lat+ ","+lon+"])\n");
        //query.append("sndRq.setLon(" + lon + "])\n");
        query.append("sndRq.setNcSoundingLayer2(0)\n");
		if(merge == true)
			query.append("sndRq.setMerge(1)\n");
		else
			query.append("sndRq.setMerge(0)\n");

		query.append("sndRq.setLevel('" + level + "')\n");
		String rtStr = "[";
		//new way
		for(int i=0; i < soundingRangeTime.length; i ++){
			rtStr = rtStr +  soundingRangeTime[i]+"L";
			if(i <soundingRangeTime.length-1)
				rtStr = rtStr + "," ;
		}
		rtStr = rtStr + "]";
		query.append("return sndRq.getSoundingDataByRangeTimeArray("+rtStr+")");
		
		//System.out.println(query.toString());

		Object[] pdoList;
		try {
			//query DB from EDEX
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			//			System.out.println( "Called genericSoundingQueryByLatLon()" );
			if ( ( pdoList != null)  && (pdoList.length > 0 )   && (   pdoList[0] instanceof NcSoundingCube) )
				cube = (NcSoundingCube) pdoList[0];
			//System.out.println("return from edex...");

		} catch (VizException e) {
			System.out.println("getSoundingDataByRangeTimeArray failed");
			e.printStackTrace();
			return cube;
		}
		return cube;

	}
	
	/*
	 * Chin: This API used for multiple lat/lon but with only one time line query
	 */
	private static NcSoundingCube genericSoundingQueryByLatLon(long refTime, long validTimeStart, long validTimeEnd, double[][] latLon, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {

		if(latLon.length <=0 )
			return null;
		NcSoundingCube cube = null;
		StringBuilder query = new StringBuilder();

		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('" +sndType + "')\n");
		query.append("sndRq.setNcSoundingLayer2(0)\n");
		query.append("sndRq.setDataType('" +dataType.toString() + "')\n");
		query.append("sndRq.setRefTime(" +refTime+ "L)\n"); 
		query.append("sndRq.setValidTimeStart(" +validTimeStart+ "L)\n"); 
		query.append("sndRq.setValidTimeEnd(" +validTimeEnd+ "L)\n"); 
		query.append("sndRq.setRangeTimeArr([" +validTimeStart+ "])\n"); 
		if(merge == true)
			query.append("sndRq.setMerge(1)\n");
		else
			query.append("sndRq.setMerge(0)\n");

		query.append("sndRq.setLevel('" + level + "')\n");
		String latLonStr = "[";
		//new way
		for(int i=0; i < latLon.length; i ++){
			latLonStr = latLonStr +  latLon[i][0] + ","+ latLon[i][1] ;
			if(i+1 < latLon.length)
				latLonStr = latLonStr + ",";
		}
		latLonStr = latLonStr + "]";
		query.append("return sndRq.getSoundingDataByLatLonArray("+latLonStr+")");
		//System.out.println(query.toString());

		Object[] pdoList;
		try {
			//query DB from EDEX
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			//			System.out.println( "Called genericSoundingQueryByLatLon()" );
			if ( ( pdoList != null)  && (pdoList.length > 0 )   && (   pdoList[0] instanceof NcSoundingCube) )
				cube = (NcSoundingCube) pdoList[0];
			//System.out.println("return from edex...");

		} catch (VizException e) {
			System.out.println("soundingQueryByLatLon failed");
			e.printStackTrace();
			return cube;
		}
		return cube;

	}
	/*
	 * 
	 */
	
	public static NcSoundingCube uaGenericSoundingQuery(Long[] refTime, double[][] latLon, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {

		if(latLon.length <=0 )
			return null;
		NcSoundingCube cube = null;
		StringBuilder query = new StringBuilder();

		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('" +sndType + "')\n");
		query.append("sndRq.setNcSoundingLayer2(0)\n");
		query.append("sndRq.setDataType('" +dataType.toString() + "')\n");
		String refTimeStr = "[";
		for(int i=0; i < refTime.length; i ++){
			refTimeStr = refTimeStr+refTime[i];
			if(i+1 < refTime.length)
				refTimeStr= refTimeStr + ",";
		}
		refTimeStr= refTimeStr + "]";
		query.append("sndRq.setRangeTimeArr(" +refTimeStr+ ")\n"); 
		if(merge == true)
			query.append("sndRq.setMerge(1)\n");
		else
			query.append("sndRq.setMerge(0)\n");

		query.append("sndRq.setLevel('" + level + "')\n");
		String latLonStr = "[";
		
		for(int i=0; i < latLon.length; i ++){
			latLonStr = latLonStr +  latLon[i][0] + ","+ latLon[i][1] ;
			if(i+1 < latLon.length)
				latLonStr = latLonStr + ",";
		}
		latLonStr = latLonStr + "]";
		query.append("return sndRq.getSoundingDataByLatLonArray("+latLonStr+")");
		
		//System.out.println(query.toString());

		Object[] pdoList;
		try {
			//query DB from EDEX
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			//			System.out.println( "Called genericSoundingQueryByLatLon()" );
			if ( ( pdoList != null)  && (pdoList.length > 0 )   && (   pdoList[0] instanceof NcSoundingCube) )
				cube = (NcSoundingCube) pdoList[0];
			//System.out.println("return from edex...");

		} catch (VizException e) {
			System.out.println("soundingQueryByLatLon failed");
			e.printStackTrace();
			return cube;
		}
		return cube;

	}

	private static NcSoundingCube genericSoundingQueryByLatLonForMetParams(long refTime, long validTimeStart, long validTimeEnd, float[][] latLon, String sndType, NcSoundingLayer2.DataType dataType, boolean merge, String level) {

		if(latLon.length <=0 )
			return null;
		NcSoundingCube cube = null;
		StringBuilder query = new StringBuilder();

		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('" +sndType + "')\n");
		query.append("sndRq.setDataType('" +dataType.toString() + "')\n");
		query.append("sndRq.setRefTime(" +refTime+ "L)\n"); 
		query.append("sndRq.setValidTimeStart(" +validTimeStart+ "L)\n"); 
		query.append("sndRq.setValidTimeEnd(" +validTimeEnd+ "L)\n"); 
		if(merge == true)
			query.append("sndRq.setMerge(1)\n");
		else
			query.append("sndRq.setMerge(0)\n");

		query.append("sndRq.setLevel('" + level + "')\n");
		String latLonStr = "[";
		//new way
		for(int i=0; i < latLon.length; i ++){
			latLonStr = latLonStr +  latLon[i][0] + ","+ latLon[i][1] ;
			if(i+1 < latLon.length)
				latLonStr = latLonStr + ",";
		}
		latLonStr = latLonStr + "]";
		query.append("return sndRq.getSoundingLayer2DataByLatLonArray("+latLonStr+")");
		/* old way
    	for(int i=0; i < latLon.length; i ++){
    		latLonStr = latLonStr + "[" + latLon[i][0] + ","+ latLon[i][1] + "]";
    		if(i+1 < latLon.length)
    			latLonStr = latLonStr + ",";
}
    	latLonStr = latLonStr + "]";
    	query.append("return sndRq.getSoundingDataByLatLonArray("+latLonStr+")");
		 */
		//System.out.println(query.toString());

		Object[] pdoList;
		try {
			//query DB from EDEX

			pdoList = Connector.getInstance().connect(query.toString(), null,60000);
			//System.out.println( "-->> Called the python script NcSoundingDataRequest " + counter + " times");
			//counter++;
			//			System.out.println( "Called genericSoundingQueryByLatLonForMetParams()" );
			if ( ( pdoList != null)  && (pdoList.length > 0 )   && (   pdoList[0] instanceof NcSoundingCube) )
				cube = (NcSoundingCube) pdoList[0];
			//System.out.println("return from edex...");
			if ( pdoList == null )
				System.out.println("NcSoundingCube is null");

		} catch (VizException e) {
			System.out.println("soundingQueryByLatLonForMetParams failed");
			e.printStackTrace();
			return cube;
		}
		return cube;

	}


	public static NcSoundingCube mdlSoundingQueryByLatLon(String refTimeStr, String validTimeStr, List<Coordinate> coords, String pluginName, String mdlName, boolean merge, String level) {
		if(coords.size()>0){
			float[][] latLon = new float[coords.size()][2];
			for (int i=0; i< coords.size(); i++){
				latLon[i][0]= (float)coords.get(i).y; //lat
				latLon[i][1]= (float)coords.get(i).x; //lon
			}	
			return (mdlSoundingQueryByLatLon(refTimeStr, validTimeStr, latLon, pluginName, mdlName, merge, level));
		}
		return null;
	}	
	public static NcSoundingCube mdlSoundingQueryByLatLon(String refTimeStr, String validTimeStr, float[][] latLon, String pluginName, String mdlName, boolean merge, String level) {
		//if(latLon.length <=0 )
		//	return null;
		//convert refTimeStr to in msec unit and query
		//long refTime = convertRefTimeStr(refTimeStr);
		//long validTime = convertRefTimeStr(validTimeStr);
		NcSoundingCube cube = null;
		StringBuilder query = new StringBuilder();

		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setPluginName('" +pluginName + "')\n");
		query.append("sndRq.setModelName('" +mdlName+ "')\n");
		query.append("sndRq.setRefTimeStr('" +refTimeStr+ "')\n"); 
		query.append("sndRq.setValidTimeStartStr('" +validTimeStr+ "')\n"); 
		query.append("sndRq.setSndType('" +MdlSndType.ANY.toString() + "')\n");
		query.append("sndRq.setNcSoundingLayer2(0)\n");
		if(merge == true)
			query.append("sndRq.setMerge(1)\n");
		else
			query.append("sndRq.setMerge(0)\n");

		query.append("sndRq.setLevel('" + level + "')\n");
		//query.append("sndRq.setLat(" + lat + ")\n");
		//query.append("sndRq.setLon(" + lon + ")\n");
		
		String latLonStr = "[";
		for(int i=0; i < latLon.length; i ++){
			latLonStr = latLonStr +  latLon[i][0] + ","+ latLon[i][1] ;
			if(i+1 < latLon.length)
				latLonStr = latLonStr + ",";
		}
		latLonStr = latLonStr + "]";
		/* Chin's Note:
		 *  This chunk of code is for testing query multiple grid Points with same query parameters
		String latLonStr = "[";
		float limit = 20;
		for(float i=0; i <limit; i=i+1f){
			for(float j=0; j < limit; j=j+1f){
			latLonStr = latLonStr +  (latLon[0][0]+i) + ","+ (latLon[0][1]+j) ;
			if(i+1f < limit || j+1f <limit)
				latLonStr = latLonStr + ",";
			}
		}
		latLonStr = latLonStr + "]";*/
		
		query.append("return sndRq.getSoundingDataByLatLonArray("+latLonStr+")");

		//query.append("return sndRq.getModelSoundingData()");
		//System.out.println(query.toString());

		Object[] pdoList;
		try {
			//query DB from EDEX
			pdoList = Connector.getInstance().connect(query.toString(), null, 30000);
			if (pdoList[0] instanceof NcSoundingCube)
				cube = (NcSoundingCube) pdoList[0];
			//System.out.println("return from edex...");

		} catch (VizException e) {
			System.out.println("soundingQueryByLatLon failed");
			e.printStackTrace();
			return cube;
		}
		return cube;
	}

	public static NcSoundingCube soundingQueryByStnId(long refTime, String[] stnArray, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		return soundingQueryByStn(refTime, 0,0,stnArray,  sndType,  dataType,  merge, false, level);

	}
	public static NcSoundingCube soundingQueryByStnId(String refTimeStr, String[] stnArray, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		//convert refTimeStr to in msec unit and query
		long refTime = convertRefTimeStr(refTimeStr);

		return soundingQueryByStn(refTime, 0, 0,stnArray,  sndType,  dataType,  merge, false, level);

	}
	public static NcSoundingCube soundingQueryByStnNum(long refTime, String[] stnArray, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		return soundingQueryByStn(refTime,0, 0,stnArray,  sndType,  dataType,  merge, true, level);

	}
	public static NcSoundingCube soundingQueryByStnNum(String refTimeStr, String[] stnArray, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		//convert refTimeStr to in msec unit and query
		long refTime = convertRefTimeStr(refTimeStr);

		return soundingQueryByStn(refTime,0, 0,stnArray,  sndType,  dataType,  merge, true, level);

	}

	public static NcSoundingCube soundingQueryByStnId(long refTime, long validTime,String[] stnArray, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		return soundingQueryByStn(refTime, refTime,validTime,stnArray,  sndType,  dataType,  merge, false, level);

	}
	public static NcSoundingCube soundingQueryByStnId(String refTimeStr,String validTimeStr, String[] stnArray, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		//convert refTimeStr to in msec unit and query
		long refTime = convertRefTimeStr(refTimeStr);
		long validTime = convertRefTimeStr(validTimeStr);

		return soundingQueryByStn(refTime, refTime,validTime, stnArray,  sndType,  dataType,  merge, false, level);

	}
	public static NcSoundingCube soundingQueryByStnNum(long refTime, long validTime,String[] stnArray, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		return soundingQueryByStn(refTime,refTime,validTime, stnArray,  sndType,  dataType,  merge, true, level);

	}
	public static NcSoundingCube soundingQueryByStnNum(String refTimeStr, String validTimeStr, String[] stnArray, String sndType, NcSoundingLayer.DataType dataType, boolean merge, String level) {
		//convert refTimeStr to in msec unit and query
		long refTime = convertRefTimeStr(refTimeStr);
		long validTime = convertRefTimeStr(validTimeStr);

		return soundingQueryByStn(refTime,refTime,validTime, stnArray,  sndType,  dataType,  merge, true, level);

	}

	private static NcSoundingCube soundingQueryByStn(long refTime, long validTimeStart, long validTimeEnd, String[] stnArray, String sndType, NcSoundingLayer.DataType dataType, boolean merge, boolean isStnNum, String level) {
		if(stnArray.length <=0 )
			return null;
		NcSoundingCube cube = null;
		StringBuilder query = new StringBuilder();

		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('" +sndType + "')\n");
		query.append("sndRq.setDataType('" +dataType.toString() + "')\n");
		query.append("sndRq.setRefTime(" +refTime+ "L)\n"); 
		query.append("sndRq.setValidTimeStart(" +validTimeStart+ "L)\n"); 
		query.append("sndRq.setValidTimeEnd(" +validTimeEnd+ "L)\n"); 
		if(merge == true)
			query.append("sndRq.setMerge(1)\n");
		else
			query.append("sndRq.setMerge(0)\n");

		query.append("sndRq.setLevel('" + level + "')\n");
		
		String refTimeStr = "[" + refTime +  "]";
		query.append("sndRq.setRangeTimeArr(" +refTimeStr+ ")\n"); 
		
		String stnStr = "[";
		for(int i=0; i < stnArray.length; i ++){
			stnStr = stnStr + "'" + stnArray[i] + "'";
			if(i+1 < stnArray.length)
				stnStr = stnStr + ",";
		}
		stnStr = stnStr + "]";
		if(isStnNum == true)
			query.append("return sndRq.getSoundingDataByStnNumArray("+stnStr+")");
		else
			query.append("return sndRq.getSoundingDataByStnIdArray("+stnStr+")");

		//System.out.println(query.toString());

		Object[] pdoList;
		try {
			//query DB from EDEX
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			if (pdoList[0] instanceof NcSoundingCube)
				cube = (NcSoundingCube) pdoList[0];
			//System.out.println("return from edex...");

		} catch (VizException e) {
			System.out.println("soundingQueryByStn failed");
			e.printStackTrace();
			return cube;
		}
		return cube;
	}
	public static NcSoundingTimeLines soundingTimeLineQuery (String sndType){
		NcSoundingTimeLines timeLines = null;
		StringBuilder query = new StringBuilder();
		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('"+sndType+"')\n");
		query.append("return sndRq.getSoundingTimeLine()");
		//System.out.println(query.toString());
		Object[] pdoList;
		try {
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			if (pdoList[0] instanceof NcSoundingTimeLines)
				timeLines = (NcSoundingTimeLines) pdoList[0];
			//else
			//	System.out.println((String)pdoList[0]);

			//System.out.println("return from edex...");
			return timeLines;
		}catch (VizException e) {
			System.out.println("soundingTimeLineQuery failed");
			return timeLines;
		}	
	}
	
	public static NcSoundingTimeLines mdlSoundingTimeLineQuery (String sndType, String tableName){
		NcSoundingTimeLines timeLines = null;
		StringBuilder query = new StringBuilder();
		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('"+sndType+"')\n");
		query.append("sndRq.setTableName('"+tableName+"')\n");
		query.append("return sndRq.getMdlSoundingTimeLine()");
		//System.out.println(query.toString());
		Object[] pdoList;
		try {
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			if (pdoList[0] instanceof NcSoundingTimeLines)
				timeLines = (NcSoundingTimeLines) pdoList[0];
			//else
			//	System.out.println((String)pdoList[0]);

			//System.out.println("return from edex...");
			return timeLines;
		}catch (VizException e) {
			System.out.println("soundingTimeLineQuery failed");
			return timeLines;
		}	
	}
	public static NcSoundingTimeLines soundingRangeTimeLineQuery (String sndType, String refTime){
		NcSoundingTimeLines timeLines = null;
		StringBuilder query = new StringBuilder();
		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('"+sndType+"')\n");
		query.append("sndRq.setRefTimeStr('"+refTime+"')\n");
		query.append("return sndRq.getSoundingRangeTimeLine()");
		//System.out.println(query.toString());
		Object[] pdoList;
		try {
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			if (pdoList[0] instanceof NcSoundingTimeLines)
				timeLines = (NcSoundingTimeLines) pdoList[0];

			//System.out.println("return from edex...");
			return timeLines;
		}catch (VizException e) {
			System.out.println("soundingRangeTimeLineQuery failed");
			return timeLines;
		}	
	}
	public static NcSoundingTimeLines mdlSoundingRangeTimeLineQuery (String sndType, String refTime, String tableName){
		NcSoundingTimeLines timeLines = null;
		StringBuilder query = new StringBuilder();
		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('"+sndType+"')\n");
		query.append("sndRq.setTableName('"+tableName+"')\n");
		query.append("sndRq.setRefTimeStr('"+refTime+"')\n");
		query.append("return sndRq.getMdlSoundingRangeTimeLine()");
		//System.out.println(query.toString());
		Object[] pdoList;
		try {
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			if (pdoList[0] instanceof NcSoundingTimeLines)
				timeLines = (NcSoundingTimeLines) pdoList[0];

			//System.out.println("return from edex...");
			return timeLines;
		}catch (VizException e) {
			System.out.println("soundingRangeTimeLineQuery failed");
			return timeLines;
		}	
	}
	//for ncuair/bufrua
	public static NcSoundingStnInfoCollection soundingStnInfoQuery (String sndType,String selectedSndTime){
		NcSoundingStnInfoCollection stnInfos = null;
		StringBuilder query = new StringBuilder();
		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('"+sndType+"')\n");
		query.append("sndRq.setTimeLine('"+selectedSndTime+"')\n");
		query.append("return sndRq.getSoundingStnInfoCol()");
		//System.out.println(query.toString());
		Object[] pdoList;
		try {
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			if(pdoList[0] instanceof NcSoundingStnInfoCollection)
				stnInfos = (NcSoundingStnInfoCollection) pdoList[0];

			//System.out.println("return from edex...stnInfos ");
			return stnInfos;
		} catch (VizException e) {
			System.out.println("soundingStnInfoQuery failed");
			e.printStackTrace();
			return stnInfos;
		}
	}
	//for PFC sounding query that refTime and rangeStart are required input parameters
	public static NcSoundingStnInfoCollection soundingStnInfoQuery (String sndType,String selectedSndTime, String referTimeStr){
		NcSoundingStnInfoCollection stnInfos = null;
		StringBuilder query = new StringBuilder();
		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setSndType('"+sndType+"')\n");
		query.append("sndRq.setTimeLine('"+selectedSndTime+"')\n");
		query.append("sndRq.setRefTimeStr('"+referTimeStr+"')\n");
		query.append("return sndRq.getSoundingStnInfoCol()");
		//System.out.println(query.toString());
		Object[] pdoList;
		try {
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			if(pdoList[0] instanceof NcSoundingStnInfoCollection)
				stnInfos = (NcSoundingStnInfoCollection) pdoList[0];

			//System.out.println("return from edex...stnInfos ");
			return stnInfos;
		} catch (VizException e) {
			System.out.println("soundingStnInfoQuery failed");
			e.printStackTrace();
			return stnInfos;
		}
	}
	public static NcSoundingModel soundingModelNameQuery(String pluginName){
		StringBuilder query = new StringBuilder();
		query.append("import NcSoundingDataRequest\n");
		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
		query.append("sndRq.setPluginName('"+pluginName+"')\n");
		query.append("return sndRq.getModelSoundingModelNames()");
		Object[] pdoList;
		NcSoundingModel model=null;
		try {
			pdoList = Connector.getInstance().connect(query.toString(), null, 60000);
			if(pdoList[0] instanceof NcSoundingModel)
				model = (NcSoundingModel) pdoList[0];
			return model;
			
		} catch (VizException e) {
			System.out.println("soundingModelNameQuery failed");
			e.printStackTrace();
			return null;
		}
	}
}
