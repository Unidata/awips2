package gov.noaa.nws.ncep.edex.uengine.tasks.profile;
/**
 * 
 * gov.noaa.nws.ncep.edex.uengine.tasks.profile.ObservedSoundingQuery
 * 
 * This java class performs the observed sounding data query functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 09/13/2010	301			Chin Chen	Initial coding
 * 10/2010		301			T. Lee		Checked missing value
 * 11/05/2010   301         Chin Chen   Update to support uairSnd query to all data types, except ALLDATA  
 * 12/16/2010   301         Chin Chen   add support of BUFRUA observed sounding data
 * 09/14/2011   457         S. Gurung   Renamed h5 to nc
 * 10/20/2011               S. Gurung   Added ncuair changes related to replacing slat/slon/selv with location of type SurfaceObsLocation
 * Nov 2011                 Chin Chen   changed Ncuair table query algorithm for performance improvement
 * 01/05/2012               S. Gurung   Removed references to UAIR (performed cleanup)
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.dao.NcUairToRecord;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfo;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.ObsSndType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.SndQueryKeyType;

import java.io.File;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.edex.plugin.bufrua.dao.BufrUADao;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.pointdata.PointDataQuery;
import com.vividsolutions.jts.geom.Coordinate;

public class ObservedSoundingQuery {
	private static final UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND.getConverterTo(NonSI.KNOT);
	private static final UnitConverter kelvinToCelsius = SI.KELVIN.getConverterTo(SI.CELSIUS);
	private static final String UAIR_TBL_NAME = "uair";
	private static final String NCUAIR_TBL_NAME = "ncuair";
	private static final String BURFUA_TBL_NAME = "bufrua";
	private static String currentDBTblName = "nil";
    
	/*
	 * This method is for caller to get sounding station's info lat/lon/stn id/stn num/elevation
	 * key: stn's lat/lon or stn ( either stn id or stn num) 
	 */
	@SuppressWarnings("unchecked")
	public static NcSoundingProfile getObservedSndStnInfo(Double lat, Double lon,String stn, String obType,Calendar refTimeCal,SndQueryKeyType queryType ) {
		NcSoundingProfile pf = new NcSoundingProfile();
		CoreDao dao;
		List<String> fields = new ArrayList<String>();
		List<Object> values = new ArrayList<Object>();
		if(obType.equals(ObsSndType.NCUAIR.toString())){
			List<String> operands = new ArrayList<String>();
			List<NcUairRecord> lUairRecords = null;	
			if(queryType==SndQueryKeyType.STNID){
				fields.add("location.stationId");// the location.stationId field name defined in NcUairRecord
				values.add(stn);
				operands.add("=");
			}
			else if(queryType==SndQueryKeyType.STNNUM){
				fields.add("stnum");// the stnNum field name defined in NcUairRecord
				values.add(stn);
				operands.add("=");
			}
			else if(queryType==SndQueryKeyType.LATLON){
				fields.add("location.latitude"); // the location.latitude field name defined in NcUairRecord
				values.add(lat);
				operands.add(">=");
				fields.add("location.latitude");
				values.add(lat); 
				operands.add("<=");
				fields.add("location.longitude"); // the location.longitude field name defined in NcUairRecord
				values.add(lon); 
				operands.add(">=");
				fields.add("location.longitude"); 
				values.add(lon);
				operands.add("<=");
			}
			else {
				//*System.out.println("request query type "+ queryType+ " is not supported in this API" );
				return pf;
			}
			fields.add("synopticTime");// the synoptic time field name defined in UairRecord
			values.add(refTimeCal); 
			operands.add("=");
			dao = new CoreDao(DaoConfig.forClass(NcUairRecord.class));
			try {
				lUairRecords = (List<NcUairRecord>) dao.queryByCriteria(fields, values,operands);
				System.out.println("Nc uair at lat="+ lat+" lon="+lon+ " recorde size="+lUairRecords.size());
				if(lUairRecords.size() > 0){
					pf.setStationElevation((float)lUairRecords.get(0).getElevation());
					pf.setStationId(lUairRecords.get(0).getStationId());
					if(lUairRecords.get(0).getStnum() != null && lUairRecords.get(0).getStnum().length()>0)
						pf.setStationNum(Integer.parseInt(lUairRecords.get(0).getStnum()));
					pf.setStationLatitude((float)lUairRecords.get(0).getLatitude());
					pf.setStationLongitude((float)lUairRecords.get(0).getLongitude());
									}
			}catch (DataAccessLayerException e) {
				//*System.out.println("obs sounding query exception");
				e.printStackTrace();
			}
		} 
		else if(obType.equals(ObsSndType.BUFRUA.toString())) {
			List<UAObs> lUairRecords = null;
			if(queryType==SndQueryKeyType.STNID){
				fields.add("stationName");// the stationName String field name defined in UAObs, dont be confused with UAIRRecord definition
				values.add(stn);
			}
			else if(queryType==SndQueryKeyType.STNNUM){
				fields.add("location.stationId");// the location.stationId String field name defined in UAObs. dont be confused with UAIRRecord definition
				values.add(stn);
			}
			else if(queryType==SndQueryKeyType.LATLON){
				fields.add("location.latitude");// the location.latitude field name defined in UAObs
				values.add(lat); 
				fields.add("location.longitude");// the location.longitude field name defined in UAObs
				values.add(lon); 

			}
			else {
				return pf;
			}
			fields.add("validTime");// the synoptic time field name defined in UAObs
			values.add(refTimeCal); 
			dao = new CoreDao(DaoConfig.forClass(UAObs.class));
			try {
				lUairRecords = (List<UAObs>) dao.queryByCriteria(fields, values);
				if(lUairRecords.size() > 0){
					pf.setStationLatitude((float)lUairRecords.get(0).getLatitude());
					pf.setStationLongitude((float)lUairRecords.get(0).getLongitude());
					pf.setStationElevation((float)lUairRecords.get(0).getElevation());
					if(lUairRecords.get(0).getStationId()!=null && lUairRecords.get(0).getStationId().length()>0)
						pf.setStationNum(Integer.parseInt(lUairRecords.get(0).getStationId()));
					pf.setStationId(lUairRecords.get(0).getStationName());
				}
			}catch (DataAccessLayerException e) {
				//*System.out.println("obs sounding query exception");
				e.printStackTrace();
			}
		}
		return pf;
	}
	public static NcSoundingStnInfoCollection getObservedSndStnInfoCol(String obType, String selectedSndTime) {
		NcSoundingStnInfoCollection stnInfoCol = new NcSoundingStnInfoCollection();
		List<NcSoundingStnInfo> stationInfoList= new ArrayList<NcSoundingStnInfo>();
		String queryStr, queryStr1;
		Object [] rtnobjArray, rtnobjArray1;
		CoreDao dao;
		if(obType.equals(ObsSndType.BUFRUA.toString())){
			currentDBTblName = BURFUA_TBL_NAME;
			queryStr = new String("Select Distinct latitude, longitude, id, stationname, elevation, reftime FROM "+ currentDBTblName + " where reftime='" +
					selectedSndTime+"' AND latitude BETWEEN -89.9 AND 89.9 AND longitude BETWEEN -179.9 AND 179.9");
			queryStr1 = new String("Select Distinct latitude, longitude FROM "+ currentDBTblName + " where reftime='" +
					selectedSndTime+"' AND latitude BETWEEN -89.9 AND 89.9 AND longitude BETWEEN -179.9 AND 179.9");
			dao = new CoreDao(DaoConfig.forClass(UAObs.class));
			
		}
		else if(obType.equals(ObsSndType.NCUAIR.toString())){
			currentDBTblName = NCUAIR_TBL_NAME;
			queryStr = new String("Select Distinct latitude, longitude, id, stationId, elevation, synoptictime FROM "+ currentDBTblName + " where nil='FALSE' AND synoptictime='" +
				selectedSndTime+"' AND latitude BETWEEN -89.9 AND 89.9 AND longitude BETWEEN -179.9 AND 179.9");
			queryStr1 = new String("Select Distinct latitude, longitude FROM "+ currentDBTblName + " where nil='FALSE' AND synoptictime='" +
				selectedSndTime+"' AND latitude BETWEEN -89.9 AND 89.9 AND longitude BETWEEN -179.9 AND 179.9");
			dao = new CoreDao(DaoConfig.forClass(NcUairRecord.class));
		}
		else {
			return stnInfoCol;
		}
		rtnobjArray = dao.executeSQLQuery(queryStr);

		//*System.out.println("size of rtnobjArray " + rtnobjArray.length);
		/*
			Object[] obj;
			for (int i =0; i <rtnobjArray.length; i++){
				System.out.println(" obj contents = "+ rtnobjArray[i]);
				obj = (Object[] )rtnobjArray[i];
				for (int j =0; j <rtnobjArray.length; j++){
					System.out.println(" obj contents = "+ obj[j].toString());
				}
			}*/

		rtnobjArray1 = dao.executeSQLQuery(queryStr1);

		//*System.out.println("size of rtnobjArray1 " + rtnobjArray1.length);
		if((rtnobjArray1.length>0) && (rtnobjArray.length > 0)){
			double lat, lon, elv;
			String stnInfo;

			//System.out.println("queryAndMarkStn called mapresource = "+ nsharpMapResource.toString());
			//Note: A same station may have many reports and at some reports they dont provide elv and or stnid
			// this implementation is "try" to make sure we get those info.
			//If, all reports does not have elv or stnid, then we still can not report it.
			for (int i =0; i <rtnobjArray1.length; i++){
				Object[] objArray1 = (Object[] )rtnobjArray1[i];
				Timestamp synoptictime=null;
				stnInfo="";
				elv=-999;
				/*if(obType.equals(ObsSndType.NCUAIR.toString())){
					lat = (Float)objArray1[0];
					lon = (Float)objArray1[1];
				}
				else{*/
					lat = (Double)objArray1[0];
					lon = (Double)objArray1[1];
				//}
				//System.out.println("lat = "+ lat +" lon= "+lon+"\n\n");
				for (int j =0; j <rtnobjArray.length; j++){
					Object[] objArray = (Object[] )rtnobjArray[j];
					if((obType.equals(ObsSndType.NCUAIR.toString())&&(lat == (Double)objArray[0])&&(lon == (Double)objArray[1])) ||
					   ((obType.equals(ObsSndType.BUFRUA.toString()))&&(lat == (Double)objArray[0])&&(lon == (Double)objArray[1]))){
						//ids.add(((Integer)objArray[2]));
						//System.out.println("id=" + (Integer)objArray[2]);
						if(stnInfo == "")
							stnInfo = (String)objArray[3];
						if(elv==-999){
							if(obType.equals(ObsSndType.NCUAIR.toString()))
								elv =(Integer)objArray[4];
							else  if(obType.equals(ObsSndType.BUFRUA.toString()))
								elv =(Integer)objArray[4];
						}
						synoptictime = (Timestamp)objArray[5];
					}

				}
				
				NcSoundingStnInfo stn = stnInfoCol.getNewStnInfo();
				stn.setStnId(stnInfo);
				stn.setStationLongitude((float)lon);
				stn.setStationLatitude((float)lat);
				stn.setStationElevation((float)elv);
				stn.setSynopTime(synoptictime);
				stationInfoList.add((NcSoundingStnInfo)stn);
				//System.out.println("stn "+ stnInfo + " lon "+ lon + " lat "+ lat);
			}
		}

		
		NcSoundingStnInfo [] stationInfoAry = new NcSoundingStnInfo [stationInfoList.size()] ;
		stnInfoCol.setStationInfo(stationInfoList.toArray(stationInfoAry));
		//*System.out.println("stn size = "+ stnInfoCol.getStationInfo().length);
		return stnInfoCol;
	}
	public static NcSoundingTimeLines getObservedSndTimeLine(String obType) {
		Object[] synopTimeAry = null;
		NcSoundingTimeLines tl = new NcSoundingTimeLines();
		String queryStr;
		CoreDao dao;
		if(obType.equals(ObsSndType.BUFRUA.toString())) {
			currentDBTblName = BURFUA_TBL_NAME;
			queryStr = new String("Select Distinct reftime FROM "+ currentDBTblName + " ORDER BY reftime DESC");
			dao = new CoreDao(DaoConfig.forClass(UAObs.class));

		}
		else if(obType.equals(ObsSndType.NCUAIR.toString())){
			currentDBTblName = NCUAIR_TBL_NAME;
			queryStr = new String("Select Distinct synoptictime FROM "+ currentDBTblName + " where nil='FALSE' ORDER BY synoptictime DESC");
			dao = new CoreDao(DaoConfig.forClass(NcUairRecord.class));
		} 
		else{
			return tl;
		}
		synopTimeAry = (Object[]) dao.executeSQLQuery(queryStr);
		//*System.out.println("size of synoptictime " + synopTimeAry.length);

		//for(int i=0; i < synopTimeAry.length; i++){
		//	if(synopTimeAry[i] != null)
		//		System.out.println("synoptictime ="+synopTimeAry[i] );
		//}
		tl.setTimeLines(synopTimeAry);

		return tl;
	}
	public static List<Calendar>  getObservedSndTimeRangeList(String obType,Calendar startTime, Calendar endTime) {
		Object[] synopTimeAry = null;
		String startTimeStr = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(startTime.getTime());
		String endTimeStr = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(endTime.getTime());
		String queryStr;
		CoreDao dao;
		if(obType.equals(ObsSndType.BUFRUA.toString())) {
			currentDBTblName = BURFUA_TBL_NAME;
			queryStr = new String("Select Distinct reftime FROM "+ currentDBTblName + " ORDER BY reftime DESC");
			dao = new CoreDao(DaoConfig.forClass(UAObs.class));

		}
		else if(obType.equals(ObsSndType.NCUAIR.toString())){
			currentDBTblName = NCUAIR_TBL_NAME;
			queryStr = new String("Select Distinct synoptictime FROM "+ currentDBTblName + " where nil='FALSE' AND synoptictime >= '"+ startTimeStr+"' AND synoptictime <= '"+endTimeStr+"' ORDER BY synoptictime DESC");
			dao = new CoreDao(DaoConfig.forClass(NcUairRecord.class));
		} 
		else{
			return null;
		}
		synopTimeAry = (Object[]) dao.executeSQLQuery(queryStr);
		List<Calendar> timeLst = new ArrayList<Calendar>();
		//*System.out.println("size of synoptictime " + synopTimeAry.length);

		for(int i=0; i < synopTimeAry.length; i++){
			if(synopTimeAry[i] != null){
				System.out.println("synoptictime ="+synopTimeAry[i] );
				Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
				cal.setTimeInMillis(((Timestamp)synopTimeAry[i]).getTime());
				//String gmtTimeStr = String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS",  cal);
				timeLst.add(cal);
			}
		}

		return timeLst;
	}
	//static long totalRqTime=0;
	/*
	 * Chin: Note: get NcUair based on stn is NOT supported yet!!!
	 * NEW: This function requests ONE station's all dataTypes data at once
	 * Nsharp is using this metod...dont remove it without consulting Nsharp developer
	 */
	public static NcUairRecord[] getObservedSndNcUairData(Double lat, Double lon, String stn, String refTime){
		
		PointDataQuery request = null;
        PointDataContainer result = null;
        NcUairRecord[] h5Records=null;
        
        List<NcUairRecord> pickedH5Records = new ArrayList<NcUairRecord>();
        try {
			request = new PointDataQuery("ncuair");	
			request.setParameters(NcUairToRecord.MAN_PARAMS_LIST);
			request.addParameter("location.latitude", String.valueOf(lat), "=");
			request.addParameter("location.longitude", String.valueOf(lon), "=");
			/*request.addParameter("location.latitude", String.valueOf(lat-0.1), ">=");
			request.addParameter("location.latitude", String.valueOf(lat+0.1), "<=");
			request.addParameter("location.longitude", String.valueOf(lon-0.1), ">=");
			request.addParameter("location.longitude", String.valueOf(lon+0.1), "<=");*/
			request.addParameter("dataTime.refTime",refTime, "=");
			request.addParameter("nil", String.valueOf(false), "=");
			//Chin newQ request.addParameter("dataType", dataType, "=");
			//long t001 = System.currentTimeMillis();
			request.requestAllLevels();
			result = request.execute();
			//long t002 = System.currentTimeMillis();
			//totalRqTime=totalRqTime+(t002-t001);
			//System.out.println("getObservedSndNcUairData request at lat="+ lat+ " lon="+lon+" took "+(t002-t001)+"ms total Qtime="+totalRqTime);
            if (result != null) {
            	//System.out.println("getObservedSndNcUairData: result is not null, getAllocatedSz= "+ result.getAllocatedSz());
            	//System.out.println("getObservedSndNcUairData:getting "+dataType);
            	h5Records = NcUairToRecord.toNcUairRecords(result);
            	            	
            	if(h5Records!= null && h5Records.length > 0){
            		// Chin: need to search through record list and keep one record for each record type
            		// If a record type comes with more than one record, then make decision to keep one based on
            		// its "issuetime" and "corr" columns in the ncuair table, then keep one record only
            		NcUairRecord orignalRd;
            		boolean addToList = true;
            		for(int i=0; i< h5Records.length; i++){
            			orignalRd = h5Records[i];
            			addToList = true;
            			for(NcUairRecord pickedRd: pickedH5Records){
            				if(orignalRd.getDataType().equals( pickedRd.getDataType())){
            					//the two records have same data type
            					//this records will either replace the one in list or be dropped
            					addToList = false;        				
            					if ((pickedRd.getIssueTime().compareTo(orignalRd.getIssueTime())<0) ||
            						(pickedRd.getIssueTime().compareTo(orignalRd.getIssueTime())==0 && orignalRd.getCorr()!=null && pickedRd.getCorr() != null && pickedRd.getCorr().compareTo(orignalRd.getCorr())<0 )||
            						(pickedRd.getIssueTime().compareTo(orignalRd.getIssueTime())==0 && orignalRd.getCorr()!=null && pickedRd.getCorr() == null)
            						)
            					{
            						// decide to replace picked with original record, based on the following cases, in (priority) order
            						//case 1: original record has "later" issue time than picked record
            						//case 2: original record has "larger" correction "corr" than picked record
            						//case 3: original record has  correction "corr", picked record does not have
            						//System.out.println("getObservedSndNcUairData: at lat="+ lat+ " lon="+lon+ " ori= " + orignalRd.getDataURI()+
            						//		"  picked="+ pickedRd.getDataURI());
            						int pickedIndex = pickedH5Records.indexOf(pickedRd);
            						pickedH5Records.set(pickedIndex, orignalRd);
            						//System.out.println("getObservedSndNcUairData: at lat="+ lat+ " lon="+lon+ " afterreplaced picked record ="+pickedH5Records.get(pickedIndex).getDataURI());
            						
            					}
            					break;
            				}
            			}
            			if(addToList==true){
            				// add this original record to picked list
            				pickedH5Records.add(orignalRd);
            				//System.out.println("getObservedSndNcUairData: at lat="+ lat+ " lon="+lon+ " add ori to picked record ="+orignalRd.getDataURI());
    						
            			}
            		}
            		
            	}
            	
            }
        } catch (Exception e) {
        	e.printStackTrace();
        }
        if(pickedH5Records.size()>0){
        	//System.out.println("getObservedSndNcUairData: at lat="+ lat+ " lon="+lon+ " number of  picked records = "+pickedH5Records.size());
        	return pickedH5Records.toArray(new NcUairRecord[pickedH5Records.size()]);
        }
        return null;
	}
	/*
	 * Chin: Note: get NcUair based on stn is NOT supported yet!!!
	 * NEW: This function requests ALL stn with all dataTypes and returns all good ("corrected")records at once to improve performance
	 */
	public static List<NcUairRecord[]> getAllStnObservedSndNcUairData(float[][] latLonArray, String stn, String refTime){
		//List<NcSoundingProfile>  soundingProfileList= new ArrayList<NcSoundingProfile>();
		PointDataQuery request = null;
        PointDataContainer result = null;
        //NcUairRecord[] h5Records=null;
        Double maxLat,  minLat,  maxLon,  minLon, lat, lon;
        List<NcUairRecord> returnedDbRecords = new ArrayList<NcUairRecord>();
        List<NcUairRecord[]> finalRecordArrayList = new ArrayList<NcUairRecord[]>();
        maxLat=minLat=0.0;
		maxLon=minLon=0.0;
        for ( int i=0; i < latLonArray.length ; i++)
		{

			//make sure we have right precision...
			lat = Double.parseDouble(Float.toString(latLonArray[i][0]));
			lon = Double.parseDouble(Float.toString(latLonArray[i][1]));
			if(i==0){
				maxLat=minLat=lat;
				maxLon=minLon=lon;
			}
			maxLat = Math.max(lat, maxLat);
			minLat = Math.min(lat, minLat);
			maxLon = Math.max(lon, maxLon);
			minLon = Math.min(lon, minLon);
		}
        try {
        	request = new PointDataQuery("ncuair");	
        	request.setParameters(NcUairToRecord.MAN_PARAMS_LIST);

        	request.addParameter("location.latitude", String.valueOf(minLat-0.1), ">=");
        	request.addParameter("location.latitude", String.valueOf(maxLat+0.1), "<=");
        	request.addParameter("location.longitude", String.valueOf(minLon-0.1), ">=");
        	request.addParameter("location.longitude", String.valueOf(maxLon+0.1), "<=");
        	request.addParameter("dataTime.refTime",refTime, "=");
        	request.addParameter("nil", String.valueOf(false), "=");
        	request.requestAllLevels();
        	result = request.execute();
        	//long t002 = System.currentTimeMillis();
        	//totalRqTime=totalRqTime+(t002-t001);
        	//System.out.println("getObservedSndNcUairData request at lat="+ lat+ " lon="+lon+" took "+(t002-t001)+"ms total Qtime="+totalRqTime);
        	if (result != null) {
        		returnedDbRecords = NcUairToRecord.toNcUairRecordsList(result); 

        		if(returnedDbRecords!= null && returnedDbRecords.size() > 0){

        			//Chin: keep list of records for same station 
        			//search through all returned records and keep same staion's records in one list
        			for ( int i=0; i < latLonArray.length ; i++)
        			{
        				//for each station
        				lat = Double.parseDouble(Float.toString(latLonArray[i][0]));
        				lon = Double.parseDouble(Float.toString(latLonArray[i][1]));
        				NcUairRecord record;
        				List<NcUairRecord> stnRecords = new ArrayList<NcUairRecord>();
        				//System.out.println("Before loop: Number of records in returnedDbRecords="+returnedDbRecords.size());
        				for(int j=returnedDbRecords.size()-1; j >=0; j--){
        					record =returnedDbRecords.get(j);
        					if(record.getLatitude() >= lat - 0.1 && record.getLatitude() <= lat + 0.1 && record.getLongitude() >= lon-0.1&& record.getLongitude() <= lon+0.1){
        						//remove this record from return list and add it to this stn list
        						stnRecords.add(returnedDbRecords.remove(j));
        					}
        				}
        				if(stnRecords.size()>0){
        					//System.out.println("Before  checking:stn lat="+lat +"stn record size="+stnRecords.size());
        					List<NcUairRecord> pickedUairRecords = new ArrayList<NcUairRecord>();
        					NcUairRecord orignalRd;
        					boolean addToList = true;
        					for(int ii=0; ii< stnRecords.size(); ii++){
        						orignalRd = stnRecords.get(ii);
        						addToList = true;
        						for(NcUairRecord pickedRd: pickedUairRecords){
        							if(orignalRd.getDataType().equals( pickedRd.getDataType())){
        								//System.out.println("getObservedSndNcUairData: at lat="+ lat+ " lon="+lon+ " find a same datatype="+pickedRd.getDataType()+ " orignalRd corr="+orignalRd.getCorr()+
        								//		" pickedRd Corr="+pickedRd.getCorr());

        								//the two records have same data type
        								//this records will either replace the one in list or be dropped
    									addToList = false; 
    									if ((pickedRd.getIssueTime().compareTo(orignalRd.getIssueTime())<0) ||
        										(pickedRd.getIssueTime().compareTo(orignalRd.getIssueTime())==0 && orignalRd.getCorr()!=null && pickedRd.getCorr() != null && pickedRd.getCorr().compareTo(orignalRd.getCorr())<0 )||
        										(pickedRd.getIssueTime().compareTo(orignalRd.getIssueTime())==0 && orignalRd.getCorr()!=null && pickedRd.getCorr() == null)
        								)
        								{
        									// decide to replace picked with original record, based on the following cases, in (priority) order
        									//case 1: original record has "later" issue time than picked record
        									//case 2: original record has "larger" correction "corr" than picked record
        									//case 3: original record has  correction "corr", picked record does not have
        									//System.out.println("getObservedSndNcUairData: at lat="+ lat+ " lon="+lon+ " ori= " + orignalRd.getDataURI()+
        									//		"  picked="+ pickedRd.getDataURI());
        									int pickedIndex = pickedUairRecords.indexOf(pickedRd);
        									pickedUairRecords.set(pickedIndex, orignalRd);
        									//System.out.println("getObservedSndNcUairData: at lat="+ lat+ " lon="+lon+ " afterreplaced picked record ="+pickedH5Records.get(pickedIndex).getDataURI());
        								}
    									break;
            						}
        						}
        						if(addToList==true){
        							// add this original record to picked list
        							pickedUairRecords.add(orignalRd);
        							//System.out.println("getObservedSndNcUairData: at lat="+ lat+ " lon="+lon+ " add ori to picked record ="+orignalRd.getDataURI());

        						}
        					}
        					//pickedUairRecords.get(0).setNil(false); // set for special handling for its caller
        					finalRecordArrayList.add(pickedUairRecords.toArray(new NcUairRecord[pickedUairRecords.size()]));
        					//System.out.println("After  checking: stn record size="+pickedUairRecords.size());
        				}/*Chin:  If caller need all query stns returned (no matter there is no data), then we may need to add this code...	
        				else {
        					//just add a null record array
        					NcUairRecord dummy = new NcUairRecord();
        					dummy.setNil(true);// set for special handling for its caller
        					SurfaceObsLocation location = new SurfaceObsLocation();
        					location.setLatitude(lat);
        					location.setLongitude(lon);
        					dummy.setLocation(location);
        					
        					NcUairRecord[] dummys= new NcUairRecord[1];
        					dummys[0] = dummy;
        					finalRecordArrayList.add(dummys);
        				}*/
        			}
        		}
        	}
        } catch (Exception e) {
        	e.printStackTrace();
        }
        return finalRecordArrayList;
	}
	/*
	 * This method query ONE station's specific one dataType data only
	 */
	public static NcUairRecord getObservedSndNcUairData(Double lat, Double lon, String stn, String refTime, String dataType, SndQueryKeyType queryType){
		NcUairRecord effectRecord=null;
		PointDataQuery request = null;
        PointDataContainer result = null;
        
        try {
			request = new PointDataQuery("ncuair");	
			request.setParameters(NcUairToRecord.MAN_PARAMS_LIST);
			request.addParameter("location.latitude", String.valueOf(lat-0.1), ">=");
			request.addParameter("location.latitude", String.valueOf(lat+0.1), "<=");
			request.addParameter("location.longitude", String.valueOf(lon-0.1), ">=");
			request.addParameter("location.longitude", String.valueOf(lon+0.1), "<=");
			request.addParameter("dataTime.refTime",refTime, "=");
			//System.out.println("getObservedSndNcUairData: lat="+lat+ " lon="+lon + " refTime="+refTime);
			request.addParameter("nil", String.valueOf(false), "=");
			request.addParameter("dataType", dataType, "=");
			
			request.requestAllLevels();
			result = request.execute();
            if (result != null) {
            	//System.out.println("getObservedSndNcUairData: result is not null, getAllocatedSz= "+ result.getAllocatedSz());
            	//System.out.println("getObservedSndNcUairData:getting "+dataType);
            	NcUairRecord[] h5Records = NcUairToRecord.toNcUairRecords(result);
            	//System.out.println("getObservedSndNcUairData:  number of "+dataType+" records = "+h5Records.length);
            	
            	if(h5Records.length == 1){
            		//effectRecord = h5Records[0];
            		//System.out.println("getObservedSndNcUairData: real dataType =" + h5Records[0].getDataType());
            		return h5Records[0];
            	}
            	else if (h5Records.length > 1){
            		// need to find out the latest corrected record
            		int lastCorrectedRecord=0;
					String currentCorInd = "";
					Calendar curIssueTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));		
					// set init time
					curIssueTime.setTimeInMillis(0);
            		for(int i=0; i< h5Records.length; i++){
            			//System.out.println("getObservedSndNcUairData2: real dataType =" + h5Records[i].getDataType());
            			if( ( curIssueTime.compareTo(h5Records[i].getIssueTime())<0 ) ||
            				(curIssueTime.compareTo(h5Records[i].getIssueTime())==0 && h5Records[i].getCorr()!= null && currentCorInd.compareTo(h5Records[i].getCorr()) < 0 )
            			)
            			{
							currentCorInd = h5Records[i].getCorr();
							curIssueTime = h5Records[i].getIssueTime();
							lastCorrectedRecord = i;
							//System.out.println("getObservedSndNcUairData: corr=" + h5Records[i].getCorr());
						}
            		}
            		//effectRecord = h5Records[lastCorrectedRecord];
            		return h5Records[lastCorrectedRecord];
            	}
            	
            }
        } catch (Exception e) {
        	e.printStackTrace();
        }
		return effectRecord;
	}

	/*
     * this api is provided for  applications  and for testing to retrieve observed uair data from PostgreSql DB
     * dataType should use "enum DataType" defined in NcSoundingLayer.java
     * Support  "ALLDATA" data type only
     */
	public static NcSoundingProfile getObservedSndAllData(Double lat, Double lon, String stn, long refTimeL, String obType, SndQueryKeyType queryType){
		Calendar refTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		// for testing ...refTimeCal.setTimeInMillis(1276581600000L);		
		//refTimeCal.setTimeInMillis(refTime.getTime());
		refTimeCal.setTimeInMillis(refTimeL);
		return getObservedSndAllData(lat, lon, stn, refTimeCal, obType, queryType);
	}
	
	  /*
     * this api is provided for  applications  and for testing to retrieve observed uair data from PostgreSql DB
     * dataType should use "enum DataType" defined in NcSoundingLayer.java
     * Support  "ALLDATA" data type only
     */
	public static NcSoundingProfile getObservedSndAllData(Double lat, Double lon, String stn, Calendar refTimeCal, String obType, SndQueryKeyType queryType){
		NcSoundingProfile pfAll= new NcSoundingProfile();
		List<NcSoundingLayer> soundingLyLst, finalsoundingLyLst;
		/*if(obType.equals(ObsSndType.UAIR.toString())){
			NcSoundingProfile pf = getObservedSndData(lat, lon, stn, refTimeCal, obType, "TTAA", queryType);
			pfAll.setStationElevation(pf.getStationElevation());
			finalsoundingLyLst = pf.getSoundingLyLst();
			if (finalsoundingLyLst.size() == 0) {
				finalsoundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "UUAA", queryType).getSoundingLyLst();
			}

			soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "TTBB", queryType).getSoundingLyLst();
			if (soundingLyLst.size() == 0) {
				soundingLyLst = getObservedSndData(lat, lon,stn, refTimeCal, obType, "UUBB", queryType).getSoundingLyLst();
			}
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "TTCC", queryType).getSoundingLyLst();
			if (soundingLyLst.size() == 0) {
				soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "UUCC", queryType).getSoundingLyLst();
			}
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "TTDD", queryType).getSoundingLyLst();
			if (soundingLyLst.size() == 0) {
				soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "UUDD", queryType).getSoundingLyLst();
			}
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "PPAA", queryType).getSoundingLyLst();
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "PPBB", queryType).getSoundingLyLst();
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "PPCC", queryType).getSoundingLyLst();
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "PPDD", queryType).getSoundingLyLst();
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			soundingLyLst = getObservedSndData(lat, lon,stn, refTimeCal, obType, "MAXWIND_A", queryType).getSoundingLyLst();
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "MAXWIND_C", queryType).getSoundingLyLst();
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "TROPOPAUSE_A", queryType).getSoundingLyLst();
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			soundingLyLst = getObservedSndData(lat, lon, stn,refTimeCal, obType, "TROPOPAUSE_C", queryType).getSoundingLyLst();
			if (soundingLyLst.size() >= 0){
				finalsoundingLyLst.addAll(soundingLyLst);
			}
			pfAll.setSoundingLyLst(finalsoundingLyLst);
		}
		*/
		return pfAll;
	}
    /*
     * this api is provided for  applications  and for testing to retrieve observed uair data from PostgreSql DB
     * dataType should use "enum DataType" defined in NcSoundingLayer.java
     * Support all dataType except "ALLDATA" data type
     * using either lat/lon, stnId or stnNum and synopticTime as key
     * refTime is with unit of msec as input
     */
	public static NcSoundingProfile getObservedSndData(Double lat, Double lon,String stn, long refTimeL, String obType, String dataType, SndQueryKeyType queryType){
		Calendar refTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		// for testing ...refTimeCal.setTimeInMillis(1276581600000L);		
		//refTimeCal.setTimeInMillis(refTime.getTime());
		refTimeCal.setTimeInMillis(refTimeL);
		return getObservedSndData(lat, lon, stn, refTimeCal, obType,  dataType,  queryType);
	}
    /*
     * this api is provided for  applications  and for testing to retrieve observed uair data from PostgreSql DB
     * dataType should use "enum DataType" defined in NcSoundingLayer.java
     * Support all dataType except "ALLDATA" data type
     * using either lat/lon, stnId or stnNum and synopticTime as key
     * reference time is with Calendar data type as input 
     */
	@SuppressWarnings("unchecked")
	public static NcSoundingProfile getObservedSndData(Double lat, Double lon,String stn, Calendar refTimeCal, String obType, String dataType, SndQueryKeyType queryType){
		NcSoundingProfile pf = new NcSoundingProfile();
		//one StnPt represent one data time line 
		List<NcSoundingLayer> soundLyLst = new ArrayList<NcSoundingLayer>();
		NcSoundingLayer soundingLy;
		soundLyLst.clear();		
		
		//String gmtTimeStr = String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM",  refTimeCal);
		//*System.out.println("sndType= "+ obType+" data type="+ dataType+ " lat " + lat+ " lon "+ lon+" GMT time " + gmtTimeStr  );

		/*obType = ObsSndType.UAIR.toString(); // currently assume all uair sounding type
		if(obType.equals(ObsSndType.UAIR.toString())){
			if(dataType.equals(NcSoundingLayer.DataType.ALLDATA.toString())){
				//System.out.println("request all data is not supported in this API");
			}
			else {
				List<String> fields = new ArrayList<String>();
				List<Object> values = new ArrayList<Object>();
				List<String> operands = new ArrayList<String>();
				List<UairRecord> lUairRecords = null;
				if(queryType==SndQueryKeyType.STNID){
					fields.add("stationId");// the stnId field name defined in UairRecord
					values.add(stn);
					operands.add("=");
				}
				else if(queryType==SndQueryKeyType.STNNUM){
					fields.add("stationNumber");// the stnNum field name defined in UairRecord
					values.add(stn);
					operands.add("=");
				}
				else if(queryType==SndQueryKeyType.LATLON){
					fields.add("slat");// the lat field name defined in UairRecord
					values.add(lat-0.1); 
					operands.add(">=");
					fields.add("slat");// the lat field name defined in UairRecord
					values.add(lat+0.1); 
					operands.add("<=");
					fields.add("slon");// the lon field name defined in UairRecord
					values.add(lon-0.1); 
					operands.add(">=");
					fields.add("slon");// the lon field name defined in UairRecord
					values.add(lon+0.1); 
					operands.add("<=");
					
				}
				else {
					//System.out.println("request query type "+ queryType+ " is not supported in this API" );
					return pf;
				}
				fields.add("dataTime.refTime");// the synoptic time field name defined in UairRecord
				//fields.add("synopticTime");// the synoptic time field name defined in UairRecord
				values.add(refTimeCal.getTime()); 
				operands.add("=");
				fields.add("nil");// nil field name defined in UairRecord
				values.add(false); 
				operands.add("=");
				fields.add("dataType");// the record dataType field name defined in UairRecord
				String realDataType; //when query, need to use TTAA for MAXWIND_A, or TROPOPAUSE_A, and TTCC for MAXWIND_C, or TROPOPAUSE_C
				if(dataType.equals(NcSoundingLayer.DataType.MAXWIND_A.toString()) ||
								dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_A.toString())){
					realDataType = NcSoundingLayer.DataType.TTAA.toString();
				} else if(dataType.equals(NcSoundingLayer.DataType.MAXWIND_C.toString()) ||
						dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_C.toString())){
					realDataType = NcSoundingLayer.DataType.TTCC.toString();
				} else
					realDataType = dataType;
				values.add(realDataType); 
				operands.add("=");

				CoreDao dao = new CoreDao(DaoConfig.forClass(UairRecord.class));
				try {
					lUairRecords = (List<UairRecord>) dao.queryByCriteria(fields, values, operands);
					if(lUairRecords.size() > 0){
						//*System.out.println("size of uairrecord " + lUairRecords.size());
						int lastCorrectedRecord=0;
						String currentCorInd = "";
						Calendar curIssueTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));		
						// set init time
						curIssueTime.setTimeInMillis(0);
	            		
						if(lUairRecords.size() > 1){
							for(int i=0; i< lUairRecords.size(); i++){
								//Since we are using lat/lon/refTime to query uair table. We may have several records returned for
								// one query. It indicates there is a correction report, then we should use the newest one report. 
								// we compare corIndicator to find the latest record.
								//System.out.println("id ="+lUairRecords.get(i).getId() + " datauri=" +lUairRecords.get(i).getDataURI());
								//System.out.println("size of ObsLevels ="+lUairRecords.get(i).getObsLevels().size() );
								//System.out.println("size of maxWind ="+lUairRecords.get(i).getMaxWind().size() );
								//System.out.println("size of Tropopause ="+lUairRecords.get(i).getTropopause().size() );
								//System.out.println("correction ind ="+lUairRecords.get(i).getCorIndicator() );
								if((curIssueTime.compareTo(lUairRecords.get(i).getIssueTime())==0 && lUairRecords.get(i).getCorIndicator()!= null && currentCorInd.compareTo(lUairRecords.get(i).getCorIndicator()) < 0)||
										(curIssueTime.compareTo(lUairRecords.get(i).getIssueTime())<0))	{
									currentCorInd = lUairRecords.get(i).getCorIndicator();
									curIssueTime = lUairRecords.get(i).getIssueTime();
									lastCorrectedRecord = i;
								}
							}	
						}
						pf.setStationElevation((float)lUairRecords.get(lastCorrectedRecord).getSelv());
						pf.setStationId(lUairRecords.get(lastCorrectedRecord).getStationId());
						if(lUairRecords.get(lastCorrectedRecord).getStationNumber()!= null && lUairRecords.get(lastCorrectedRecord).getStationNumber().length() >0){	
							//System.out.println("stn num = "+ lUairRecords.get(lastCorrectedRecord).getStationNumber());
							pf.setStationNum(Integer.parseInt(lUairRecords.get(lastCorrectedRecord).getStationNumber()));
						}else{
							pf.setStationNum(-1);
						}
						pf.setStationLatitude((float)lUairRecords.get(lastCorrectedRecord).getSlat());
						pf.setStationLongitude((float)lUairRecords.get(lastCorrectedRecord).getSlon());
						//System.out.println("stn elevation ="+ pf.getStationElevation() );


						if((lUairRecords.get(lastCorrectedRecord).getObsLevels().size()>0) && 
								!dataType.equals(NcSoundingLayer.DataType.MAXWIND_A.toString()) &&
								!dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_A.toString()) &&
								!dataType.equals(NcSoundingLayer.DataType.MAXWIND_C.toString()) &&
								!dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_C.toString())){
							//*System.out.println("getting "+ dataType);

							java.util.Iterator<ObsLevels> it=lUairRecords.get(lastCorrectedRecord).getObsLevels().iterator();
							while(it.hasNext())
							{
								ObsLevels value=(ObsLevels)it.next();
								soundingLy = new NcSoundingLayer();
								soundingLy.setGeoHeight(value.getGeoHeight());
								soundingLy.setTemperature(value.getTemp());
								soundingLy.setPressure(value.getPressure());
								soundingLy.setWindDirection(value.getWindDirection());
								if ( value.getWindSpeed() != NcSoundingLayer.MISSING ) {
									soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)value.getWindSpeed()));
								} else {
									soundingLy.setWindSpeed((float)value.getWindSpeed());
								}
								soundingLy.setDewpoint(value.getDwpt());	
								//							System.out.println("pressure :"+value.getPressure() + " temp:" + value.getTemp() + " H:"+value.getGeoHeight() + " D:" +
								//									value.getWindDirection() + " F: " + value.getWindSpeed());
								soundLyLst.add(soundingLy);
							}
						}
						else if(lUairRecords.get(lastCorrectedRecord).getMaxWind().size() >0 &&
								(dataType.equals(NcSoundingLayer.DataType.MAXWIND_A.toString())||
										dataType.equals(NcSoundingLayer.DataType.MAXWIND_C.toString()))){
							//*System.out.println("getting "+ dataType);
							java.util.Iterator<MaxWind> itWind=lUairRecords.get(lastCorrectedRecord).getMaxWind().iterator();

							while(itWind.hasNext())
							{
								MaxWind value=(MaxWind)itWind.next();
								soundingLy = new NcSoundingLayer();
								soundingLy.setPressure(value.getPressure());
								soundingLy.setWindDirection(value.getWindDirection());
								if ( value.getWindSpeed() != NcSoundingLayer.MISSING ) {
									soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)value.getWindSpeed()));
								} else {
									soundingLy.setWindSpeed((float)value.getWindSpeed());
								}
								//*System.out.println("WIND pressure :"+value.getPressure() + " WD:" + value.getWindDirection() + " WS:"+(float)metersPerSecondToKnots.convert((float)value.getWindSpeed()));
								soundLyLst.add(soundingLy);
							}
						}
						else if(lUairRecords.get(lastCorrectedRecord).getTropopause().size() >0 &&
								(dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_A.toString())||
										dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_C.toString()))){
							//*System.out.println("getting "+ dataType);
							java.util.Iterator<Tropopause> it=lUairRecords.get(lastCorrectedRecord).getTropopause().iterator();
							while(it.hasNext())
							{
								Tropopause value=(Tropopause)it.next();
								soundingLy = new NcSoundingLayer();
								soundingLy.setTemperature(value.getTemp());
								soundingLy.setPressure(value.getPressure());
								soundingLy.setWindDirection(value.getWindDirection());
								if ( value.getWindSpeed() != NcSoundingLayer.MISSING ) {
									soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)value.getWindSpeed()));
									//*System.out.println("Tropopause pressure :"+value.getPressure() + " temp:" + value.getTemp()+" WD:" + value.getWindDirection() + " WS:"+(float)metersPerSecondToKnots.convert((float)value.getWindSpeed()));

								} else {
									soundingLy.setWindSpeed((float)value.getWindSpeed());
									//*System.out.println("Tropopause pressure :"+value.getPressure() + " temp:" + value.getTemp()+" WD:" + value.getWindDirection() + " WS:"+(float)value.getWindSpeed());

								}
								soundingLy.setDewpoint(value.getDwpt());	
								soundLyLst.add(soundingLy);
							}
						}
					}

				} catch (DataAccessLayerException e) {
					//*System.out.println("obs sounding query exception");
					//e.printStackTrace();
				}
			}

		}//end ObsSndType.UAIR
*/
		
		pf.setSoundingLyLst(soundLyLst);
		return pf;
		
	}

	/*
	 * NOT used currently
	 * 
	 * obsType: UAIR, TAMDAR or DROP, dataType: ALLDATA ONLY
	 */
	/*@SuppressWarnings("unchecked")
	public static NcSoundingProfile getObservedSndData(int[]  parentIds, String obType,String dataType) {
		NcSoundingProfile pf = new NcSoundingProfile();
		//one StnPt represent one data time line 
		List<NcSoundingLayer> soundLyLst = new ArrayList<NcSoundingLayer>();
		NcSoundingLayer soundingLy;
		soundLyLst.clear();	
		
		obType = ObsSndType.UAIR.toString(); // currently assume all uair sounding type
		if(obType.equals(ObsSndType.UAIR.toString())){
			//*System.out.println("parentIds length = " + parentIds.length );
			for(int i=0; i <parentIds.length; i++){
				int id = parentIds[i];
				List<String> fields = new ArrayList<String>();
				List<Object> values = new ArrayList<Object>();
				List<UairRecord> lObsLevels = null;
				fields.add("id");// the record id field name defined in UairRecord
				values.add(id); //testing 994872);
				if(!dataType.equals(NcSoundingLayer.DataType.ALLDATA.toString()) && 
						!dataType.equals(NcSoundingLayer.DataType.MAXWIND_A.toString()) &&
						!dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_A.toString())){
					fields.add("dataType");// the record dataType field name defined in UairRecord
					values.add(dataType); //testing "TTAA"

				}
				CoreDao dao = new CoreDao(DaoConfig.forClass(UairRecord.class));
				try {
					lObsLevels = (List<UairRecord>) dao.queryByCriteria(fields, values);
					if(lObsLevels.size() > 0){
						//since we are using Records id to query uair table. lObsLevels.size() should always be one.
						//*System.out.println("size of uairrecord " + lObsLevels.size());
						//*System.out.println("id ="+lObsLevels.get(0).getId() + " datauri=" +lObsLevels.get(0).getDataURI());
						//*System.out.println("size of ObsLevels ="+lObsLevels.get(0).getObsLevels().size() );
						//*System.out.println("size of maxWind ="+lObsLevels.get(0).getMaxWind().size() );
						//*System.out.println("size of Tropopause ="+lObsLevels.get(0).getTropopause().size() );
						if((lObsLevels.get(0).getObsLevels().size()>0) && 
								!dataType.equals(NcSoundingLayer.DataType.MAXWIND_A.toString()) &&
								!dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_A.toString())){
							java.util.Iterator<ObsLevels> it=lObsLevels.get(0).getObsLevels().iterator();
							while(it.hasNext())
							{
								ObsLevels value=(ObsLevels)it.next();
								soundingLy = new NcSoundingLayer();
								soundingLy.setGeoHeight(value.getGeoHeight());
								soundingLy.setTemperature(value.getTemp());
								soundingLy.setPressure(value.getPressure());
								soundingLy.setWindDirection(value.getWindDirection());
								if ( value.getWindSpeed() != NcSoundingLayer.MISSING ) {
									soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)value.getWindSpeed()));
								} else {
									soundingLy.setWindSpeed((float)value.getWindSpeed());
								}
								soundingLy.setDewpoint(value.getDwpt());	
								//*System.out.println("pressure :"+value.getPressure() + " temp:" + value.getTemp() + " H:"+value.getGeoHeight() );
								soundLyLst.add(soundingLy);
							}
						}
					}
					
					if(lObsLevels.get(0).getMaxWind().size() >0 &&
							!dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_A.toString())){
						java.util.Iterator<MaxWind> itWind=lObsLevels.get(0).getMaxWind().iterator();

						while(itWind.hasNext())
						{
							MaxWind value=(MaxWind)itWind.next();
							soundingLy = new NcSoundingLayer();
							soundingLy.setPressure(value.getPressure());
							soundingLy.setWindDirection(value.getWindDirection());
							if ( value.getWindSpeed() != NcSoundingLayer.MISSING ) {
								soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)value.getWindSpeed()));
							} else {
								soundingLy.setWindSpeed((float)value.getWindSpeed());
							}

							soundLyLst.add(soundingLy);
						}
					}
					
					if(lObsLevels.get(0).getTropopause().size() >0 &&
							!dataType.equals(NcSoundingLayer.DataType.MAXWIND_A.toString())){
						java.util.Iterator<Tropopause> it=lObsLevels.get(0).getTropopause().iterator();
						while(it.hasNext())
						{
							Tropopause value=(Tropopause)it.next();
							soundingLy = new NcSoundingLayer();
							soundingLy.setTemperature(value.getTemp());
							soundingLy.setPressure(value.getPressure());
							soundingLy.setWindDirection(value.getWindDirection());
							if ( value.getWindSpeed() != NcSoundingLayer.MISSING ) {
								soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)value.getWindSpeed()));
								//*System.out.println("Tropopause pressure :"+value.getPressure() + " temp:" + value.getTemp()+" WD:" + value.getWindDirection() + " WS:"+(float)metersPerSecondToKnots.convert((float)value.getWindSpeed()));

							} else {
								soundingLy.setWindSpeed((float)value.getWindSpeed());
								//*System.out.println("Tropopause pressure :"+value.getPressure() + " temp:" + value.getTemp()+" WD:" + value.getWindDirection() + " WS:"+(float)value.getWindSpeed());

							}

							soundingLy.setDewpoint(value.getDwpt());	

							soundLyLst.add(soundingLy);
						
						}
					}
				} catch (DataAccessLayerException e) {
					//*System.out.println("obs sounding query exception");
					//e.printStackTrace();
				}

			}	
		}
		pf.setSoundingLyLst(soundLyLst);
		return pf;
		
	}
	*/
	  /*
     * this api is provided for  applications  and for testing to retrieve observed bufruair data from PostgreSql DB & HDF5
     * dataType should use "enum DataType" defined in NcSoundingLayer.java
     * Support  "ALLDATA" data type only
     */
	public static NcSoundingProfile getObservedSndBufruaAllData(Double lat, Double lon, String stn, long refTimeL,  SndQueryKeyType queryType){
		Calendar refTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		// for testing ...refTimeCal.setTimeInMillis(1276581600000L);		
		//refTimeCal.setTimeInMillis(refTime.getTime());
		refTimeCal.setTimeInMillis(refTimeL);
		return getObservedSndBufruaAllData(lat, lon, stn, refTimeCal,  queryType);
	}
	
	  /*
     * this api is provided for  applications  and for testing to retrieve observed bufruair data from PostgreSql DB & HDF5
     * dataType should use "enum DataType" defined in NcSoundingLayer.java
     * Support  "ALLDATA" data type only
     */
	public static NcSoundingProfile getObservedSndBufruaAllData(Double lat, Double lon, String stn, Calendar refTimeCal,  SndQueryKeyType queryType){
		NcSoundingProfile pfAll= new NcSoundingProfile();
		List<NcSoundingLayer> soundingLyLst, finalsoundingLyLst;
		NcSoundingProfile pf = getObservedSndBufruaData(lat, lon, stn, refTimeCal,  "TTAA", queryType);
		pfAll.setStationElevation(pf.getStationElevation());
		finalsoundingLyLst = pf.getSoundingLyLst();
		soundingLyLst = getObservedSndBufruaData(lat, lon, stn,refTimeCal,  "TTBB", queryType).getSoundingLyLst();
		if (soundingLyLst.size() >= 0){
			finalsoundingLyLst.addAll(soundingLyLst);
		}
		soundingLyLst = getObservedSndBufruaData(lat, lon, stn,refTimeCal,  "TTCC", queryType).getSoundingLyLst();
		if (soundingLyLst.size() >= 0){
			finalsoundingLyLst.addAll(soundingLyLst);
		}
		soundingLyLst = getObservedSndBufruaData(lat, lon, stn,refTimeCal,  "TTDD", queryType).getSoundingLyLst();
		if (soundingLyLst.size() >= 0){
			finalsoundingLyLst.addAll(soundingLyLst);
		}
		//soundingLyLst = getObservedSndBufruaData(lat, lon, stn,refTimeCal, "PPAA", queryType).getSoundingLyLst();
		//if (soundingLyLst.size() >= 0){
		//	finalsoundingLyLst.addAll(soundingLyLst);
		//}
		soundingLyLst = getObservedSndBufruaData(lat, lon, stn,refTimeCal,  "PPBB", queryType).getSoundingLyLst();
		if (soundingLyLst.size() >= 0){
			finalsoundingLyLst.addAll(soundingLyLst);
		}
		//soundingLyLst = getObservedSndBufruaData(lat, lon, stn,refTimeCal,  "PPCC", queryType).getSoundingLyLst();
		//if (soundingLyLst.size() >= 0){
		//	finalsoundingLyLst.addAll(soundingLyLst);
		//}
		soundingLyLst = getObservedSndBufruaData(lat, lon, stn,refTimeCal, "PPDD", queryType).getSoundingLyLst();
		if (soundingLyLst.size() >= 0){
			finalsoundingLyLst.addAll(soundingLyLst);
		}
		soundingLyLst = getObservedSndBufruaData(lat, lon,stn, refTimeCal, "MAXWIND_A", queryType).getSoundingLyLst();
		if (soundingLyLst.size() >= 0){
			finalsoundingLyLst.addAll(soundingLyLst);
		}
		soundingLyLst = getObservedSndBufruaData(lat, lon, stn,refTimeCal, "MAXWIND_C", queryType).getSoundingLyLst();
		if (soundingLyLst.size() >= 0){
			finalsoundingLyLst.addAll(soundingLyLst);
		}
		soundingLyLst = getObservedSndBufruaData(lat, lon, stn,refTimeCal, "TROPOPAUSE_A", queryType).getSoundingLyLst();
		if (soundingLyLst.size() >= 0){
			finalsoundingLyLst.addAll(soundingLyLst);
		}
		soundingLyLst = getObservedSndBufruaData(lat, lon, stn,refTimeCal,  "TROPOPAUSE_C", queryType).getSoundingLyLst();
		if (soundingLyLst.size() >= 0){
			finalsoundingLyLst.addAll(soundingLyLst);
		}
		pfAll.setSoundingLyLst(finalsoundingLyLst);
		return pfAll;
	}

    /*
     * this api is provided for  applications  and for testing to retrieve observed BufrUA data from PostgreSql DB and HDF5
     * dataType should use "enum DataType" defined in NcSoundingLayer.java
     * Support all dataType except "ALLDATA" data type
     * using either lat/lon, stnId or stnNum and validTime as key
     * refTime is with unit of msec as input
     */
	public static NcSoundingProfile getObservedSndBufruaData(Double lat, Double lon,String stn, long refTimeL,  String dataType, SndQueryKeyType queryType){
		Calendar refTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		// for testing ...refTimeCal.setTimeInMillis(1276581600000L);		
		//refTimeCal.setTimeInMillis(refTime.getTime());
		refTimeCal.setTimeInMillis(refTimeL);
		return getObservedSndBufruaData(lat, lon, stn, refTimeCal,   dataType,  queryType);
	}

	/*
	 * This API is provided for retrieving bufrUA data from PostgresSQL and HDF5
     * dataType should use "enum DataType" defined in NcSoundingLayer.java
     * Support all dataType except "ALLDATA" data type
     * using either lat/lon, stnId or stnNum and synopticTime as key
     * reference time is with Calendar data type as input 
     */
	@SuppressWarnings("unchecked")
	public static NcSoundingProfile getObservedSndBufruaData(Double lat, Double lon, String stn, Calendar refTimeCal, String dataType,SndQueryKeyType queryType){
		//*System.out.println("getObservedSndBufruaData lat= " + lat+" lon="+lon+" refTime="+refTimeCal  );
    	//Timestamp refTime = new Timestamp(refTimeL);
    	//System.out.println("GMT ref time = "+ refTime.toGMTString());
    	NcSoundingProfile pf = new NcSoundingProfile();
    	List<NcSoundingLayer> soundLyList = new ArrayList<NcSoundingLayer>();
    	if(dataType.equals(NcSoundingLayer.DataType.ALLDATA.toString())){
			//*System.out.println("request all data is not supported in this API");
    		return pf;
		}
    	else {
    		List<String> fields = new ArrayList<String>();
			List<Object> values = new ArrayList<Object>();
			List<UAObs> lUairRecords = null;
			if(queryType==SndQueryKeyType.STNID){
				fields.add("stationName");// the stationName String field name defined in UAObs, dont be confused with UAIRRecord definition
				values.add(stn);
			}
			else if(queryType==SndQueryKeyType.STNNUM){
				fields.add("location.stationId");// the location.stationId String field name defined in UAObs. dont be confused with UAIRRecord definition
				values.add(stn);
			}
			else if(queryType==SndQueryKeyType.LATLON){
				fields.add("location.latitude");// the location.latitude field name defined in UAObs
				values.add(lat); 
				fields.add("location.longitude");// the location.longitude field name defined in UAObs
				values.add(lon); 

			}
			else {
				System.out.println("request query type "+ queryType+ " is not supported in this API" );
				return pf;
			}
			fields.add("dataTime.refTime");// the synoptic time field name defined in UAObs
			//fields.add("validTime");// the synoptic time field name defined in UAObs
			values.add(refTimeCal.getTime()); 
			fields.add("reportType");// the record dataType field name defined in UAObs
			int intDataType = NcSoundingLayer.dataTypeMap.get(dataType);
			values.add(intDataType); 

			//for (int i=0; i < fields.size(); i++) {
			//	System.out.println("field "+ fields.get(i) + " value "+ values.get(i));
			//}
			CoreDao dao = new CoreDao(DaoConfig.forClass(UAObs.class));
			try {
				lUairRecords = (List<UAObs>) dao.queryByCriteria(fields, values);
				if(lUairRecords.size() > 0){
					//set pf data
					//System.out.println("record size = "+ lUairRecords.size() + " reportType="+dataType);
					
					int lastCorrectedRecord=0;
					String currentCorInd = "";
					
					if(lUairRecords.size() > 1){
						for(int i=0; i< lUairRecords.size(); i++){
							//Since we are using lat/lon/refTime to query uair table. We may have several records returned for
							// one query. It indicates there is a correction report, then we should use the newest one report. 
							// we compare corIndicator to find the latest record.
							
							if(lUairRecords.get(i).getCorIndicator()!= null && currentCorInd.compareTo(lUairRecords.get(i).getCorIndicator()) < 0){
								currentCorInd = lUairRecords.get(i).getCorIndicator();
								lastCorrectedRecord = i;
							}
						}	
					}
					UAObs uairRecord = lUairRecords.get(lastCorrectedRecord);
					pf.setStationLatitude((float)uairRecord.getLatitude());
					pf.setStationLongitude((float)uairRecord.getLongitude());
					pf.setStationElevation((float)uairRecord.getElevation());
					if(uairRecord.getStationId()!= null && uairRecord.getStationId().length()>0)
						pf.setStationNum(Integer.parseInt(uairRecord.getStationId()));
					pf.setStationId(uairRecord.getStationName());
					int hdfIndex = uairRecord.getIdx();
					if(hdfIndex >= 0) {
						//System.out.println("selected stn lon= " + lon +
						//		" lat = "+ lat + " elv = "+ pf.getStationElevation() + " h5 table Y index ="+ hdfIndex);
						BufrUADao uadao =  new BufrUADao("bufrua");
						uairRecord.setPluginName("bufrua");
						File hdf5loc = uadao.getFullFilePath(uairRecord);
						//System.out.println("hdf5 path = " + hdf5loc.getAbsolutePath());
						IDataStore dataStore = DataStoreFactory.getDataStore(hdf5loc);

						FloatDataRecord sfcPressurefloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "sfcPressure", Request.buildYLineRequest(new int[] {hdfIndex}));
						float[] sfcPressuredata = sfcPressurefloatData.getFloatData();
						if(sfcPressuredata.length>0)
							pf.setSfcPress(sfcPressuredata[0]/100F);

						NcSoundingLayer soundingLy;
						//based on requested data type:
						// get temp, dew point, pressure, wind u/v components, and height
						//they are 2-D tables
						if(dataType.equals(NcSoundingLayer.DataType.TTAA.toString()) ||
								dataType.equals(NcSoundingLayer.DataType.TTCC.toString()))
						{
							//get mandatory data size
							IntegerDataRecord numManIntData = (IntegerDataRecord) dataStore.retrieve(
									"/", "numMand", Request.buildYLineRequest(new int[] {hdfIndex}));
							int[] sizes = numManIntData.getIntData();
							// sizes is a 1x1 2d table. Only first (0) element is valid.
							if(sizes[0] >0){
								FloatDataRecord pressurefloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "prMan", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] pressuredata = pressurefloatData.getFloatData();
								FloatDataRecord temperaturefloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "tpMan", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] temperaturedata = temperaturefloatData.getFloatData();
								FloatDataRecord dewptfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "tdMan", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] dewptdata = dewptfloatData.getFloatData();
								FloatDataRecord windDfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "wdMan", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] windDdata = windDfloatData.getFloatData();
								FloatDataRecord windSfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "wsMan", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] windSdata = windSfloatData.getFloatData();
								FloatDataRecord htfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "htMan", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] htdata = htfloatData.getFloatData();
								for (int i=0; i<sizes[0]; i++)
								{
									soundingLy = new NcSoundingLayer();	
									//if data is not available, dont convert it and just use default setting data
									if(temperaturedata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setTemperature((float)kelvinToCelsius.convert(temperaturedata[i]));
									if(pressuredata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setPressure(pressuredata[i]/100F);
									if(windSdata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)windSdata[i]));
									soundingLy.setWindDirection(windDdata[i]);
									if(dewptdata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setDewpoint((float)kelvinToCelsius.convert(dewptdata[i]));						
									soundingLy.setGeoHeight(htdata[i]);						
									soundLyList.add(soundingLy);
								}
								//debug
								//for(NcSoundingLayer ly: soundLyList){
								//	System.out.println("Mandatory "+ dataType + ":: Pre= "+ly.getPressure()+ " Dew= "+ ly.getDewpoint()+ " T= "+ ly.getTemperature() + " WS= " + ly.getWindSpeed() + " WD= " + ly.getWindDirection());
								//}
							}
							else{
								System.out.println("Mandatory data is not available! request data tye is "+ dataType);
							}
						}
						else if(dataType.equals(NcSoundingLayer.DataType.TTBB.toString()) ||
								dataType.equals(NcSoundingLayer.DataType.TTDD.toString()))
						{
							//get significantT data size
							IntegerDataRecord numSigtIntData = (IntegerDataRecord) dataStore.retrieve(
									"/", "numSigT", Request.buildYLineRequest(new int[] {hdfIndex}));
							int[] sizes = numSigtIntData.getIntData();
							// sizes is a 1x1 2d table. Only first (0) element is valid.
							if(sizes[0] >0){
								FloatDataRecord pressurefloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "prSigT", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] pressuredata = pressurefloatData.getFloatData();
								FloatDataRecord temperaturefloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "tpSigT", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] temperaturedata = temperaturefloatData.getFloatData();
								FloatDataRecord dewptfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "tdSigT", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] dewptdata = dewptfloatData.getFloatData();
								for (int i=0; i<sizes[0]; i++)
								{
									soundingLy = new NcSoundingLayer();	
									//if data is not available, dont convert it and just use default setting data
									if(temperaturedata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setTemperature((float)kelvinToCelsius.convert(temperaturedata[i]));
									if(pressuredata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setPressure(pressuredata[i]/100F);
									if(dewptdata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setDewpoint((float)kelvinToCelsius.convert(dewptdata[i]));						
									soundLyList.add(soundingLy);
								}
								//for(NcSoundingLayer ly: soundLyList){
								//	System.out.println("SigT "+ dataType + ":: Pre= "+ly.getPressure()+ " Dew= "+ ly.getDewpoint()+ " T= "+ ly.getTemperature());
								//}
							}
							else{
								System.out.println("SigT data is not available! request data tye is "+ dataType);
							}
							
						}
						else if(dataType.equals(NcSoundingLayer.DataType.PPBB.toString()) ||
								dataType.equals(NcSoundingLayer.DataType.PPDD.toString()))
						{
							//get significantW data size
							IntegerDataRecord numSigwIntData = (IntegerDataRecord) dataStore.retrieve(
									"/", "numSigW", Request.buildYLineRequest(new int[] {hdfIndex}));
							int[] sizes = numSigwIntData.getIntData();
							// sizes is a 1x1 2d table. Only first (0) element is valid.
							if(sizes[0] >0){
								FloatDataRecord htfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "htSigW", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] htdata = htfloatData.getFloatData();
								FloatDataRecord windDfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "wdSigW", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] windDdata = windDfloatData.getFloatData();
								FloatDataRecord windSfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "wsSigW", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] windSdata = windSfloatData.getFloatData();
								for (int i=0; i<sizes[0]; i++)
								{
									soundingLy = new NcSoundingLayer();	
									//if data is not available, dont convert it and just use default setting data
									soundingLy.setGeoHeight(htdata[i]);						
									if(windSdata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)windSdata[i]));
									soundingLy.setWindDirection(windDdata[i]);						
									soundLyList.add(soundingLy);
								}
								//for(NcSoundingLayer ly: soundLyList){
								//	System.out.println("SigW "+ dataType + ":: Ht= "+ly.getGeoHeight()+" WS= " + ly.getWindSpeed() + " WD= " + ly.getWindDirection());
								//}
							}
							else{
								System.out.println("SigW data is not available! request data tye is "+ dataType);
							}
						}
						else if(dataType.equals(NcSoundingLayer.DataType.MAXWIND_A.toString()) ||
								dataType.equals(NcSoundingLayer.DataType.MAXWIND_C.toString()))
						{
							//get max wind data size
							IntegerDataRecord numMwndIntData = (IntegerDataRecord) dataStore.retrieve(
									"/", "numMwnd", Request.buildYLineRequest(new int[] {hdfIndex}));
							int[] sizes = numMwndIntData.getIntData();
							// sizes is a 1x1 2d table. Only first (0) element is valid.
							if(sizes[0] >0){
								FloatDataRecord pressurefloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "prMaxW", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] pressuredata = pressurefloatData.getFloatData();
								FloatDataRecord windDfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "wdMaxW", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] windDdata = windDfloatData.getFloatData();
								FloatDataRecord windSfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "wsMaxW", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] windSdata = windSfloatData.getFloatData();
								for (int i=0; i<sizes[0]; i++)
								{
									soundingLy = new NcSoundingLayer();	
									//if data is not available, dont convert it and just use default setting data
									if(pressuredata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setPressure(pressuredata[i]/100F);
									if(windSdata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)windSdata[i]));
									soundingLy.setWindDirection(windDdata[i]);						
									soundLyList.add(soundingLy);
								}
								//for(NcSoundingLayer ly: soundLyList){
								//	System.out.println("MAXwind "+ dataType + ":: Pre= "+ly.getPressure()+ " WS= " + ly.getWindSpeed() + " WD= " + ly.getWindDirection());
								//}
							}
							else{
								System.out.println("max wind data is not available! request data tye is "+ dataType);
							}
						}
						else if(dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_A.toString()) ||
								dataType.equals(NcSoundingLayer.DataType.TROPOPAUSE_C.toString()))
						{
							//get troppause data size
							IntegerDataRecord numTropIntData = (IntegerDataRecord) dataStore.retrieve(
									"/", "numTrop", Request.buildYLineRequest(new int[] {hdfIndex}));
							int[] sizes = numTropIntData.getIntData();
							// sizes is a 1x1 2d table. Only first (0) element is valid.
							if(sizes[0] >0){
								FloatDataRecord pressurefloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "prTrop", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] pressuredata = pressurefloatData.getFloatData();
								FloatDataRecord temperaturefloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "tpTrop", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] temperaturedata = temperaturefloatData.getFloatData();
								FloatDataRecord dewptfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "tdTrop", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] dewptdata = dewptfloatData.getFloatData();
								FloatDataRecord windDfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "wdTrop", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] windDdata = windDfloatData.getFloatData();
								FloatDataRecord windSfloatData = (FloatDataRecord) dataStore.retrieve(
										"/", "wsTrop", Request.buildYLineRequest(new int[] {hdfIndex}));
								float[] windSdata = windSfloatData.getFloatData();
								for (int i=0; i<sizes[0]; i++)
								{
									soundingLy = new NcSoundingLayer();	
									//if data is not available, dont convert it and just use default setting data
									if(temperaturedata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setTemperature((float)kelvinToCelsius.convert(temperaturedata[i]));
									if(pressuredata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setPressure(pressuredata[i]/100F);
									if(windSdata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)windSdata[i]));
									soundingLy.setWindDirection(windDdata[i]);
									if(dewptdata[i]!= NcSoundingLayer.MISSING)
										soundingLy.setDewpoint((float)kelvinToCelsius.convert(dewptdata[i]));						
									soundLyList.add(soundingLy);
								}
								//debug
								//for(NcSoundingLayer ly: soundLyList){
								//	System.out.println("Troppause "+ dataType + ":: Pre= "+ly.getPressure()+ " Dew= "+ ly.getDewpoint()+ " T= "+ ly.getTemperature() + " WS= " + ly.getWindSpeed() + " WD= " + ly.getWindDirection());
								//}
							}
							else{
								System.out.println("Troppause data is not available! request data tye is "+ dataType);
							}
						}

					}
					else{
						System.out.println("hdf5 index (idx) is less than 0!!!");
						return pf;
					}
				}
				else {
					System.out.println("buffrua (UAOb) record is not available!! request type "+dataType);
					return pf;
				}

			} catch (Exception e) {
				//*System.out.println("exception=" + e );
				e.printStackTrace();
				return pf;
			}
			//*System.out.println("sounding layer size = "+ soundLyList.size());
			
			pf.setSoundingLyLst(soundLyList);

    		return pf;
    		/*
    		List<NcSoundingLayer> soundLyList = new ArrayList<NcSoundingLayer>();
    		UAObs uaRecord = new UAObs();
    		uaRecord.setPluginName("bufrua");
    		
    		

    		// refTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
    		// for testing ...refTimeCal.setTimeInMillis(1276581600000L);

    		//refTimeCal.setTimeInMillis(refTime.getTime());
    		DataTime refTimeDataTime = new DataTime(refTimeCal);
    		uaRecord.setDataTime(refTimeDataTime);

    		// for testing ... validTime = new Timestamp(1277013600000L);
    		//validTime.setTime(1277013600000L);
			
    		try {
    			BufrUADao uadao =  new BufrUADao("bufrua");
    			File hdf5loc = uadao.getFullFilePath(lUairRecords.get(0));
    			System.out.println("hdf5 path = " + hdf5loc.getAbsolutePath());
    			IDataStore dataStore = DataStoreFactory.getDataStore(hdf5loc);
    			
    			try {
    				LongDataRecord longData = (LongDataRecord) dataStore.retrieve(
    						"/", "validTime", Request.ALL);
    				long[] validtimedata = longData.getLongData();

    				FloatDataRecord latfloatData = (FloatDataRecord) dataStore.retrieve(
    						"/", "latitude", Request.ALL);
    				float[] latdata = latfloatData.getFloatData();

    				FloatDataRecord lonfloatData = (FloatDataRecord) dataStore.retrieve(
    						"/", "longitude", Request.ALL);
    				float[] londata = lonfloatData.getFloatData();

    				FloatDataRecord elvfloatData = (FloatDataRecord) dataStore.retrieve(
    						"/", "staElev", Request.ALL);
    				float[] elvdata = elvfloatData.getFloatData();

    				StringDataRecord stnIdStrData = (StringDataRecord) dataStore.retrieve(
    						"/", "staName", Request.ALL);
    				String[] stnIddata = stnIdStrData.getStringData();

    				IntegerDataRecord stnNumIntData = (IntegerDataRecord) dataStore.retrieve(
    						"/", "wmoStaNum", Request.ALL);
    				int[] stnNumdata = stnNumIntData.getIntData();

    				IntegerDataRecord rptIntData = (IntegerDataRecord) dataStore.retrieve(
    						"/", "rptType", Request.ALL);
    				int[] rptData = rptIntData.getIntData();

    				int selectedTimeIndex=-1; 

    				for (int j=0; j<validtimedata.length; j++)
    				{

    					if(queryType==SndQueryType.LATLON){

    						//find the index of user picked data time line, data type  and lat/lon
    						if((validtimedata[j] == refTimeCal.getTimeInMillis()) &&
    								(latdata[j] == (float)lat)	&& 
    								(londata[j] == (float)lon)	&&
    								(rptData[j] == NcSoundingLayer.dataTypeMap.get(dataType)))
    						{
    							selectedTimeIndex = j;
    							break;
    						}
    					}
    					else if(queryType==SndQueryType.STNID){
    						//find the index of user picked data time line and lat/lon
    						if((validtimedata[j] == refTimeCal.getTimeInMillis()) &&
    								stn.equals(stnIddata[j])	)
    						{
    							selectedTimeIndex = j;
    							break;
    						}
    					}
    					else if(queryType==SndQueryType.STNNUM){
    						//find the index of user picked data time line and lat/lon
    						if((validtimedata[j] == refTimeCal.getTimeInMillis()) &&
    						   Integer.toString(stnNumdata[j]).equals(stn))
    						{
    							selectedTimeIndex = j;
    							break;
    						}
    					}
    					else {
    						return pf;
    					}
    				}
    				if(selectedTimeIndex != -1) {
						//*System.out.println("selected stn lon= " + lon +
						//*		" lat = "+ lat + " elv = "+ elvdata[j] + " h5 table Y index ="+ j);
    					
    					//set pf data
    					pf.setStationLatitude(latdata[selectedTimeIndex]);
						pf.setStationLongitude(londata[selectedTimeIndex]);
						pf.setStationElevation(elvdata[selectedTimeIndex]);
						pf.setStationNum(stnNumdata[selectedTimeIndex]);
						pf.setStationId(stnIddata[selectedTimeIndex]);
						
						FloatDataRecord sfcPressurefloatData = (FloatDataRecord) dataStore.retrieve(
    							"/", "sfcPressure", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
    					float[] sfcPressuredata = sfcPressurefloatData.getFloatData();
    					if(sfcPressuredata.length>0)
    						pf.setSfcPress(sfcPressuredata[0]/100F);
    					
    					NcSoundingLayer soundingLy;

    					// get temp, dew point, pressure, wind u/v components, and height
    					//they are 2-D tables
    					FloatDataRecord pressurefloatData = (FloatDataRecord) dataStore.retrieve(
    							"/", "prMan", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
    					float[] pressuredata = pressurefloatData.getFloatData();
    					FloatDataRecord temperaturefloatData = (FloatDataRecord) dataStore.retrieve(
    							"/", "tpMan", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
    					float[] temperaturedata = temperaturefloatData.getFloatData();
    					FloatDataRecord dewptfloatData = (FloatDataRecord) dataStore.retrieve(
    							"/", "tdMan", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
    					float[] dewptdata = dewptfloatData.getFloatData();
    					FloatDataRecord windDfloatData = (FloatDataRecord) dataStore.retrieve(
    							"/", "wdMan", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
    					float[] windDdata = windDfloatData.getFloatData();
    					FloatDataRecord windSfloatData = (FloatDataRecord) dataStore.retrieve(
    							"/", "wsMan", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
    					float[] windSdata = windSfloatData.getFloatData();
    					FloatDataRecord htfloatData = (FloatDataRecord) dataStore.retrieve(
    							"/", "htMan", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
    					float[] htdata = htfloatData.getFloatData();
    					long[] sizes = pressurefloatData.getSizes();
    					//int dim = pressurefloatData.getDimension();
    					for (int i=0; i<sizes[0]; i++)
    					{
    						soundingLy = new NcSoundingLayer();	
    						//if data is not available, dont convert it and just use default setting data
    						if(temperaturedata[i]!= NcSoundingLayer.MISSING)
    							soundingLy.setTemperature((float)kelvinToCelsius.convert(temperaturedata[i]));
    						if(pressuredata[i]!= NcSoundingLayer.MISSING)
    							soundingLy.setPressure(pressuredata[i]/100F);
    						if(windSdata[i]!= NcSoundingLayer.MISSING)
    							soundingLy.setWindSpeed((float)metersPerSecondToKnots.convert((float)windSdata[i]));
    						soundingLy.setWindDirection(windDdata[i]);
    						if(dewptdata[i]!= NcSoundingLayer.MISSING)
    							soundingLy.setDewpoint((float)kelvinToCelsius.convert(dewptdata[i]));						
    						soundingLy.setGeoHeight(htdata[i]);						
    						soundLyList.add(soundingLy);
    					}
    					//*System.out.println("sounding layer size = "+ soundLyList.size());
    					//debug
    					for(NcSoundingLayer ly: soundLyList){
    						System.out.println("Pre= "+ly.getPressure()+ " Dew= "+ ly.getDewpoint()+ " T= "+ ly.getTemperature());
    					}

    				}

    			} catch (Exception e) {
    				//*System.out.println("exception=" + e );
    				e.printStackTrace();
    			}

    		} catch (PluginException e1) {
    			// TODO Auto-generated catch block
    			e1.printStackTrace();
    		}
    		pf.setSoundingLyLst(soundLyList);

    		return pf;
    		*/
    	
    	}
	}

}
