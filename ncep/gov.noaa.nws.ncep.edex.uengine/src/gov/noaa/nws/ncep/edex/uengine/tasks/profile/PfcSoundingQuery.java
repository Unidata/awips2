package gov.noaa.nws.ncep.edex.uengine.tasks.profile;

/**
 * 
 * gov.noaa.nws.ncep.edex.uengine.tasks.profile.PfcSoundingQuery
 * 
 * This java class performs the pfc model sounding data query functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 09/13/2010	301			Chin Chen	Initial coding
 * 12/16/2010   301         Chin Chen   add support of PFC (NAM and GFS) model sounding data
 * 02/28/2012               Chin Chen   modify several sounding query algorithms for better performance
 * 12/20/2013   2537        bsteffen    Update ModelSoundingPointDataTransform
 *  *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.PfcSndType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.SndQueryKeyType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfo;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingParameters;
import com.raytheon.uf.common.dataplugin.modelsounding.ModelSoundingPointDataTransform;
import com.raytheon.uf.common.dataplugin.modelsounding.SoundingLevel;
import com.raytheon.uf.common.dataplugin.modelsounding.SoundingSite;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.vividsolutions.jts.geom.Coordinate;

public class PfcSoundingQuery {
	private static final String PFC_TBL_NAME = "modelsounding";
	private static String currentDBTblName = "nil";
	private static String reportType;
	private static UnitConverter kelvinToCelsius = SI.KELVIN.getConverterTo(SI.CELSIUS);
	private static UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND.getConverterTo(NonSI.KNOT);
	public static NcSoundingStnInfoCollection getPfcSndStnInfoCol(String sndType, String selectedSndTime, String refTimeStr) {
		NcSoundingStnInfoCollection stnInfoCol = new NcSoundingStnInfoCollection();
		List<NcSoundingStnInfo> stationInfoList= new ArrayList<NcSoundingStnInfo>();
		String queryStr="";
		Object [] rtnobjArray;
		if(sndType.equals(PfcSndType.GFSSND.toString())){
			currentDBTblName = PFC_TBL_NAME;
			reportType = "GFS";
		}
		else if(sndType.equals(PfcSndType.NAMSND.toString())){
			currentDBTblName = PFC_TBL_NAME;
			reportType = "ETA";
		}
		else{
			return stnInfoCol;
		}
		queryStr = new String("Select Distinct latitude, longitude, stationid, elevation, reftime, rangestart FROM "+ currentDBTblName + " where rangestart='" +
				selectedSndTime+ "' AND reftime ='"+ refTimeStr+ "' AND reporttype ='" + reportType + "' AND latitude BETWEEN -89.9 AND 89.9 AND longitude BETWEEN -179.9 AND 179.9");

		//System.out.println(queryStr);
		CoreDao dao = new CoreDao(DaoConfig.forClass(SoundingSite.class));
		rtnobjArray = dao.executeSQLQuery(queryStr);
		//System.out.println("size of rtnobjArray " + rtnobjArray.length);
		if(rtnobjArray.length > 0){
			double lat, lon, elv;
			//System.out.println("queryAndMarkStn called mapresource = "+ nsharpMapResource.toString());
			//Note: A same station may have many reports
			for (int i =0; i <rtnobjArray.length; i++){
				
				Object[] objArray = (Object[] )rtnobjArray[i];
				lat = (Double)objArray[0];
				lon = (Double)objArray[1];
				//System.out.println("lat = "+ lat +" lon= "+lon);
				elv = (Integer)objArray[3];
				
				NcSoundingStnInfo stn = stnInfoCol.getNewStnInfo();
				stn.setStnId((String)objArray[2]);
				stn.setStationLongitude(lon);
				stn.setStationLatitude(lat);
				stn.setStationElevation((float)elv);
				stn.setSynopTime((Timestamp)objArray[4]);
				stn.setRangeStartTime((Timestamp)objArray[5]);
				stationInfoList.add(stn);
				//System.out.println("stn id "+ stn.getStnId() + " lon "+ lon + " lat "+ lat);
			}
			NcSoundingStnInfo [] stationInfoAry = new NcSoundingStnInfo [stationInfoList.size()] ;
			stnInfoCol.setStationInfo(stationInfoList.toArray(stationInfoAry));
		}
		//*System.out.println("stn size = "+ stnInfoCol.getStationInfo().length);
		return stnInfoCol;
	}
	
	public static NcSoundingTimeLines getPfcSndTimeLine(String pfcType) {
		Object[] refTimeAry = null;
		NcSoundingTimeLines tl = new NcSoundingTimeLines();
		
		if(pfcType.equals(PfcSndType.NAMSND.toString())){
			currentDBTblName = PFC_TBL_NAME;
			reportType = "ETA"; 
		} else if(pfcType.equals(PfcSndType.GFSSND.toString())){
			currentDBTblName = PFC_TBL_NAME;
			reportType = "GFS";
		} else {
			System.out.println("pfc type is not supported: "+ pfcType);
			return tl;
		}
		//query table in metadata db
		String queryStr = new String("Select Distinct reftime FROM "+ currentDBTblName + " where reporttype='" +reportType+"' ORDER BY reftime DESC");


		CoreDao dao = new CoreDao(DaoConfig.forClass(SoundingSite.class));
		refTimeAry = (Object[]) dao.executeSQLQuery(queryStr);
		tl.setTimeLines(refTimeAry);

		return tl;
	}
	public static NcSoundingTimeLines getPfcSndRangeTimeLine(String pfcType,  String refTimeStr) {
		Object[] refTimeAry = null;
		NcSoundingTimeLines tl = new NcSoundingTimeLines();
		
		if(pfcType.equals(PfcSndType.NAMSND.toString())){
			currentDBTblName = PFC_TBL_NAME;
			reportType = "ETA"; 
		} else if(pfcType.equals(PfcSndType.GFSSND.toString())){
			currentDBTblName = PFC_TBL_NAME;
			reportType = "GFS";
		} else {
			System.out.println("pfc type is not supported: "+ pfcType);
			return tl;
		}
		//query  table in metadata db
		String queryStr = new String("Select Distinct rangestart FROM "+ currentDBTblName +
				" where reporttype='" +reportType+ "' AND "+ "reftime='"+refTimeStr+":00:00'"+" ORDER BY rangestart");// DESC");
		System.out.println("queryStr  "+ queryStr);


		CoreDao dao = new CoreDao(DaoConfig.forClass(SoundingSite.class));
		refTimeAry = (Object[]) dao.executeSQLQuery(queryStr);
		tl.setTimeLines(refTimeAry);

		return tl;
	}
	/*
	 * Chin Note: 02/27/12 obsoleting this one. Use getPfcSndDataGeneric()
	 *
	public static NcSoundingProfile getPfcSndData(double lat, double lon, String stn, long refTimeL, long validTimeL, String sndTypeStr, SndQueryKeyType queryType) {
		//*System.out.println("getPfcSndData input ref time = "+ refTimeL+" valid time is " + validTimeL);
		Calendar refTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		refTimeCal.setTimeInMillis(refTimeL);
		Calendar validTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		validTimeCal.setTimeInMillis(validTimeL);
		return getPfcSndData( lat,  lon,  stn, refTimeCal,  validTimeCal,  sndTypeStr, queryType);
	}
	*/
	/*
	 * Chin Note: 02/27/12 obsoleting this one. Use getPfcSndDataGeneric()
	 *
	@SuppressWarnings("unchecked")
	public static NcSoundingProfile getPfcSndData(double lat, double lon, String stn, Calendar refTimeCal, Calendar validTimeCal, String sndTypeStr, SndQueryKeyType queryType) {
		//System.out.println("getPfcSndData input ref time = "+ refTimeCal+" valid time is " + validTimeCal);
    	//NcSoundingProfile.PfcSndType sndType; //yes, it is not used now..
    	//Timestamp validTime = new Timestamp(validTimeL);
    	//System.out.println("getPfcSndData input ref time = "+ refTime.toGMTString()+" valid time is " + validTime.toGMTString());
		long t01 = System.currentTimeMillis();
		NcSoundingProfile pf = new NcSoundingProfile();
		if(validTimeCal == null || refTimeCal == null )
			return pf;
		List<NcSoundingLayer> soundLyList = new ArrayList<NcSoundingLayer>();
		
		if(sndTypeStr.equals(PfcSndType.GFSSND.toString())  || sndTypeStr.equals(PfcSndType.NAMSND.toString())){
    		List<String> fields = new ArrayList<String>();
			List<Object> values = new ArrayList<Object>();
			List<SoundingSite> lSndSiteRecords = null;
			List<String> operands = new ArrayList<String>();
			if(queryType==SndQueryKeyType.STNID){
				fields.add("location.stationId");// the location.stationId String field name defined in SoundingSite class and decoded modelsounding table.
				values.add(stn);
				operands.add("=");
			}
			else if(queryType==SndQueryKeyType.STNNUM){
				fields.add("siteid");// the siteid String field name defined in SoundingSite class and decoded in modelsounding table.
				values.add(stn);
				operands.add("=");
			}
			else if(queryType==SndQueryKeyType.LATLON){
				fields.add("location.latitude");// the location.latitude field name defined in SoundingSite class and decoded modelsounding table
				values.add(lat-0.1); 
				operands.add(">=");
				fields.add("location.latitude");// the location.latitude field name defined in SoundingSite class and decoded modelsounding table
				values.add(lat+0.1); 
				operands.add("<=");
				fields.add("location.longitude");// the location.longitude field name defined in SoundingSite class and decoded modelsounding table
				values.add(lon-0.1); 
				operands.add(">=");
				fields.add("location.longitude");// the location.longitude field name defined in SoundingSite class and decoded modelsounding table
				values.add(lon+0.1); 
				operands.add("<=");
				
			}
			else {
				System.out.println("request query type "+ queryType+ " is not supported in this API" );
				return pf;
			}
			
			fields.add("dataTime.refTime");// the refTime time field name defined in SoundingSite and decoded modelsounding table
			values.add(refTimeCal.getTime()); //refTime data type defined in SoundingSite is "Date"
			operands.add("=");
			fields.add("dataTime.validPeriod.start");// the rangeStart field name defined in SoundingSite and decoded modelsounding table
			values.add(validTimeCal.getTime());  //rangestart data type defined in SoundingSite is "Date"
			operands.add("=");
			//for (int i=0; i < fields.size(); i++) {
			//	System.out.println("field "+ fields.get(i) + " value "+ values.get(i));
			//}
			CoreDao dao = new CoreDao(DaoConfig.forClass(SoundingSite.class));
			try {
				lSndSiteRecords = (List<SoundingSite>) dao.queryByCriteria(fields, values, operands);
				if(lSndSiteRecords.size() > 0){
					//System.out.println("SoundingSite record received size="+lSndSiteRecords.size());
					//for(SoundingSite site: lSndSiteRecords)
					//	System.out.println("SoundingSite record Idx="+ site.getIdx());
					//set pf data
					pf.setStationLatitude(lSndSiteRecords.get(0).getLatitude());
					pf.setStationLongitude(lSndSiteRecords.get(0).getLongitude());
					pf.setStationElevation((float)lSndSiteRecords.get(0).getElevation());
					if(lSndSiteRecords.get(0).getSiteId()!=null && lSndSiteRecords.get(0).getSiteId().length()>0)
						pf.setStationNum(Integer.parseInt(lSndSiteRecords.get(0).getSiteId()));
					pf.setStationId(lSndSiteRecords.get(0).getStationId());
					//System.out.println("SoundingSite record station ID=" + lSndSiteRecords.get(0).getStationId() + 
					//		" sta Num =" + Integer.parseInt(lSndSiteRecords.get(0).getSiteId()));
					
					//Chin's Note: since idx (hdfIndex) getter function is not available from SoundingSite
					// I have to find a way to find idx myslef.
					// Because of using the following code to find hdfIndex (idx), average total query time doubled.
					// from around 80 ms to around 185 ms.
					lSndSiteRecords.get(0).setPluginName("modelsounding");
					ModelSoundingDAO mdldao1 =  new ModelSoundingDAO("modelsounding");
					File hdf5loc1 = mdldao1.getFullFilePath(lSndSiteRecords.get(0));
					IDataStore dataStore = DataStoreFactory.getDataStore(hdf5loc1);
					LongDataRecord validTimeData = (LongDataRecord) dataStore.retrieve(
							"/", "validTime", Request.ALL);
					long[] validTimedata = validTimeData.getLongData();
					StringDataRecord staIdData = (StringDataRecord) dataStore.retrieve(
							"/", "stationId", Request.ALL);
					String[] stnIddata = staIdData.getStringData();
					
					String stnId = lSndSiteRecords.get(0).getStationId();
					long startTime = validTimeCal.getTimeInMillis()/1000;
					int hdfIndex=-1;
					for(int i=0; i< stnIddata.length ; i++){
						if(startTime ==  validTimedata[i] && 
								stnId.equals(stnIddata[i])	){
							hdfIndex = i;
							//System.out.println("found SoundingSite record idx = "+ hdfIndex);
							break;
						}
					}
					//Chin Note: the following getter Not available, as Raytheon does not agree to put in
					// to SoundingSite
					//hdfIndex = lSndSiteRecords.get(0).getIdx(); 
					//System.out.println("SoundingSite record idx = "+ hdfIndex);
					if(hdfIndex >= 0) {

						//lSndSiteRecords.get(0).setPluginName("modelsounding");

						//DataTime refTimeDataTime = new DataTime(refTimeCal);
						//sndSite.setDataTime(refTimeDataTime);

						//ModelSoundingDAO mdldao =  new ModelSoundingDAO("modelsounding");
						//File hdf5loc = mdldao.getFullFilePath(lSndSiteRecords.get(0));
						//System.out.println("hdf5 path = " + hdf5loc.getAbsolutePath());
						//IDataStore dataStore = DataStoreFactory.getDataStore(hdf5loc);

						FloatDataRecord sfcPressurefloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "sfcPress", Request.buildYLineRequest(new int[] {hdfIndex}));
						float[] sfcPressuredata = sfcPressurefloatData.getFloatData();
						if(sfcPressuredata.length>0)
							pf.setSfcPress(sfcPressuredata[0]/100F);

						NcSoundingLayer soundingLy;
						//get data size - number of available level
						IntegerDataRecord numProfLvlsIntData = (IntegerDataRecord) dataStore.retrieve(
								"/", "numProfLvls", Request.buildYLineRequest(new int[] {hdfIndex}));
						int[] sizes = numProfLvlsIntData.getIntData();
						
						// get temp, dew point, pressure, wind u/v components, specHum, omega
						FloatDataRecord omegafloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "omega", Request.buildYLineRequest(new int[] {hdfIndex}));
						float[] omegadata = omegafloatData.getFloatData();
						
						//int dim = omegafloatData.getDimension();
						FloatDataRecord pressurefloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "pressure", Request.buildYLineRequest(new int[] {hdfIndex}));
						float[] pressuredata = pressurefloatData.getFloatData();
						FloatDataRecord temperaturefloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "temperature", Request.buildYLineRequest(new int[] {hdfIndex}));
						float[] temperaturedata = temperaturefloatData.getFloatData();
						FloatDataRecord specHumfloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "specHum", Request.buildYLineRequest(new int[] {hdfIndex}));
						float[] specHumdata = specHumfloatData.getFloatData();
						FloatDataRecord vCompfloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "vComp", Request.buildYLineRequest(new int[] {hdfIndex}));
						float[] vCompdata = vCompfloatData.getFloatData();
						FloatDataRecord uCompfloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "uComp", Request.buildYLineRequest(new int[] {hdfIndex}));
						float[] uCompdata = uCompfloatData.getFloatData();

						for (int i=0; i<sizes[0]; i++)
						{
							soundingLy = new NcSoundingLayer();	
							soundingLy.setOmega(omegadata[i]);
							soundingLy.setTemperature((float)kelvinToCelsius.convert(temperaturedata[i]));
							//System.out.println("level pressure 2 ="+pressuredata[i]);
							soundingLy.setPressure(pressuredata[i]/100F);
							soundingLy.setWindU(uCompdata[i]); // HDF5 data in unit of Knots, no conversion needed
							soundingLy.setWindV(vCompdata[i]);
							soundingLy.setSpecHumidity(specHumdata[i]);						
							soundLyList.add(soundingLy);
						}
						//System.out.println("sounding layer size = "+ soundLyList.size());
						//debug
						//int k=1;
						//for(NcSoundingLayer lvl: soundLyList){
						//	System.out.println("Sounding Level "+ k + " pres="+lvl.getPressure()+ " temp="+
						//			lvl.getTemperature()+ " u="+lvl.getWindU() + " SH="+ lvl.getSpecHumidity());
						//	k++;
						//}
					}
					
				}
				else
					System.out.println("SoundingSite record NOT received");
			} catch (Exception e) {
				//*System.out.println("exception=" + e );
				e.printStackTrace();
				return pf;
			}
				
		}

		pf.setSoundingLyLst(soundLyList);
		long t02 = System.currentTimeMillis();
		System.out.println("PFC profile retreival took " + (t02 - t01));
		return pf;
    }

	*/
	//@SuppressWarnings("unchecked")
	/*
	 * Chin: using ModelSoundingPointDataTransform for query.
	 */
	
	public static NcSoundingProfile getPfcSndData2(double lat, double lon, String stn, Calendar refTimeCal, Calendar validTimeCal, String sndTypeStr, SndQueryKeyType queryType) {
		//System.out.println("getPfcSndData  lat="+lat+" lon="+lon);
    	//NcSoundingProfile.PfcSndType sndType; //yes, it is not used now..
    	//Timestamp validTime = new Timestamp(validTimeL);
    	//System.out.println("getPfcSndData2 input ref time = "+ refTimeCal.getTime());
		//long t01 = System.currentTimeMillis();
		NcSoundingProfile pf = new NcSoundingProfile();
		if(validTimeCal == null || refTimeCal == null )
			return pf;
		List<NcSoundingLayer> soundLyList = new ArrayList<NcSoundingLayer>();
		
		if(sndTypeStr.equals(PfcSndType.GFSSND.toString())  || sndTypeStr.equals(PfcSndType.NAMSND.toString())){
            Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
            List<SoundingSite> lSndSiteRecords = null;
			if(queryType==SndQueryKeyType.STNID){
                // the location.stationId String field name defined in SoundingSite class and decoded modelsounding table.
                constraints.put("location.stationId", new RequestConstraint(stn));
			}
			else if(queryType==SndQueryKeyType.STNNUM){
				// the siteid String field name defined in SoundingSite class and decoded in modelsounding table.
                constraints.put("siteid", new RequestConstraint(stn));

			}
			else if(queryType==SndQueryKeyType.LATLON){
				// the location.latitude field name defined in SoundingSite class and decoded modelsounding table
				constraints.put("location.latitude", new RequestConstraint(Double.toString(lat-0.1), Double.toString(lat+0.1)));
				// the location.longitude field name defined in SoundingSite class and decoded modelsounding table
                constraints.put("location.longitude", new RequestConstraint(Double.toString(lon-0.1), Double.toString(lon+0.1)));
			}
			else {
				System.out.println("request query type "+ queryType+ " is not supported in this API" );
				return pf;
			}
			
			// the refTime time field name defined in SoundingSite and decoded modelsounding table
            // refTime data type defined in SoundingSite is "Date"
			constraints.put("dataTime.refTime", new RequestConstraint(TimeUtil.formatCalendar(refTimeCal)));
			// the rangeStart field name defined in SoundingSite and decoded modelsounding table
			//rangestart data type defined in SoundingSite is "Date"
	        constraints.put("dataTime.validPeriod.start", new RequestConstraint(TimeUtil.formatCalendar(validTimeCal)));

			
			//String d="";
			//String d1 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(validTimeCal.getTime());
			//d = d+d1;
			//for (int i=1; i< 85; i++){
			//	Date date2 = new Date(validTimeCal.getTimeInMillis()-3600000*i);
			//	String d2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date2);
			//	d = d+","+d2;
			//}
			
			// //rangestart data type defined in SoundingSite is "Date"
			// constraints.put("dataTime.validPeriod.start", new RequestConstraint(d, ConstraintType.IN));
			for (Entry<String,RequestConstraint> entry : constraints.entrySet()) {
				System.out.println("field "+ entry.getKey() + " value "+ entry.getValue().getConstraintValue());
			}
			List<String> parameters = new ArrayList<String>(12);
            parameters.addAll(ModelSoundingParameters.LVL_PARAMETERS);
            parameters.add(ModelSoundingParameters.LATITUDE);
            parameters.add(ModelSoundingParameters.LONGITUDE);
            parameters.add(ModelSoundingParameters.ELEVATION);
            parameters.add(ModelSoundingParameters.STATION_ID);
            parameters.add(ModelSoundingParameters.STATION_NUMBER);
            parameters.add(ModelSoundingParameters.DATAURI);
			
			try {
                lSndSiteRecords = ModelSoundingPointDataTransform.getSoundingSites(constraints, parameters);
				System.out.println("sounding site record size = "+ lSndSiteRecords.size());
				if(lSndSiteRecords.size() > 0){
					//set pf data
					
					pf.setStationLatitude(lSndSiteRecords.get(0).getLatitude());
					pf.setStationLongitude(lSndSiteRecords.get(0).getLongitude());
					pf.setStationElevation((float)lSndSiteRecords.get(0).getElevation());
					if(lSndSiteRecords.get(0).getSiteId()!=null && lSndSiteRecords.get(0).getSiteId().length()>0)
						pf.setStationNum(Integer.parseInt(lSndSiteRecords.get(0).getSiteId()));
					pf.setStationId(lSndSiteRecords.get(0).getStationId());
					//System.out.println("SoundingSite record station ID=" + lSndSiteRecords.get(0).getStationId() + 
					//		" sta Num =" + Integer.parseInt(lSndSiteRecords.get(0).getSiteId()));
					
					
						//for (int i=0; i<sizes[0]; i++)
						for (SoundingLevel level : lSndSiteRecords.get(0).getLevels())
						{
							NcSoundingLayer soundingLy = new NcSoundingLayer();	
							soundingLy.setOmega(level.getOmega());
							soundingLy.setTemperature((float)kelvinToCelsius.convert(level.getTemperature()));
							soundingLy.setPressure(level.getPressure()/100);
							soundingLy.setWindU((float)metersPerSecondToKnots.convert(level.getUcWind())); // HDF5 data in unit of m/s, convert to Knots 4/12/2012 
							soundingLy.setWindV((float)metersPerSecondToKnots.convert(level.getVcWind()));
							soundingLy.setSpecHumidity(level.getSpecificHumidity());						
							soundLyList.add(soundingLy);
						}
						
						//debug
						
						//for(NcSoundingLayer ly: soundLyList1){
						//	System.out.println("P= "+ly.getPressure()+ " Hm= "+ ly.getSpecHumidity()+ " T= "+ ly.getTemperature());
						//}
						for(int i =0; i<lSndSiteRecords.size();i++){
							System.out.println("sounding Data uri = "+ lSndSiteRecords.get(i).getDataURI());
						}
					
				}
				else
					System.out.println("SoundingSite record NOT received");
			} catch (Exception e) {
				//System.out.println("exception=" + e );
				e.printStackTrace();
				return pf;
			}
				
		}
		Collections.sort(soundLyList,reversePressureComparator());
		//debug
		/*int k=1;
		for(NcSoundingLayer lvl: soundLyList){
			System.out.println("data2 Sounding Level "+ k + " pres="+lvl.getPressure()+ " temp="+
					lvl.getTemperature()+ " u="+lvl.getWindU() + " SH="+ lvl.getSpecHumidity());
			k++;
		}*/
		pf.setSoundingLyLst(soundLyList);
		//long t02 = System.currentTimeMillis();
		//System.out.println("getPfcSndData2 PFC profile retreival took " + (t02 - t01));
		return pf;
    }
	/*
	 * getPfcSndDataGeneric()
	 * Chin:2012-2-17
	 * using ModelSoundingPointDataTransform for query.
	 * Using Lat/lon array OR StnId array, AND soundingTimeAry (fcst time array) as input.
	 * This function is to be generic for all cases.
	 * One and only one of latLonArray and stnIdArr should be not null and the other one should be null
	 * soundingTimeAry should be not null 
	 * 
	 */
	public static List<NcSoundingProfile>  getPfcSndDataGeneric(Coordinate[]  coordinateArray,String[] stnIdArr, String refTimeStr,List<String> soundingTimeAry, String sndTypeStr, String level) {
		List<NcSoundingProfile> pfs = new ArrayList<NcSoundingProfile>();
		
		if(sndTypeStr.equals(PfcSndType.GFSSND.toString())  || sndTypeStr.equals(PfcSndType.NAMSND.toString())){
		    Map<String,RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
            List<SoundingSite> lSndSiteRecords = null;
			MergeSounding ms = new MergeSounding();			
			
			
			if(coordinateArray != null){
				String latStr="", lonStr="";
				for ( int i=0; i < coordinateArray.length ; i++)
				{
						latStr = latStr+String.valueOf(coordinateArray[i].y)+",";
						lonStr = lonStr+String.valueOf(coordinateArray[i].x)+",";
				}
				latStr=latStr.substring(0, latStr.length()-1);//get rid of last ","
				lonStr=lonStr.substring(0, lonStr.length()-1);//get rid of last ","
				constraints.put("location.latitude", new RequestConstraint(latStr, ConstraintType.IN));
                constraints.put("location.longitude", new RequestConstraint(lonStr, ConstraintType.IN));
			}
			else if(stnIdArr != null){
				String stnIdStr="";
				for (String stnStr: stnIdArr){
					stnIdStr = stnIdStr+stnStr;
					stnIdStr= stnIdStr+",";
				}
				stnIdStr=stnIdStr.substring(0, stnIdStr.length()-1);//get rid of last ","
				// the rangeStart field name defined in SoundingSite and decoded modelsounding table
				//rangestart data type defined in SoundingSite is "Date"
                constraints.put("location.stationId", new RequestConstraint(stnIdStr, ConstraintType.IN));
			}
			else {
				return pfs;
			}

			// the refTime time field name defined in SoundingSite and decoded modelsounding table
			//refTime data type defined in SoundingSite is "Date"
            constraints.put("dataTime.refTime", new RequestConstraint(refTimeStr));

			String d="";
			for (String timeStr: soundingTimeAry){
				d = d+timeStr;
				d= d+",";
			}
			d=d.substring(0, d.length()-1);//get rid of last ","
	         // the rangeStart field name defined in SoundingSite and decoded modelsounding table. It is forcast time.
			//rangestart data type defined in SoundingSite is "Date"
            constraints.put("dataTime.validPeriod.start", new RequestConstraint(d, ConstraintType.IN));
            //for (Entry<String,RequestConstraint> entry : constraints.entrySet()) {
            //  System.out.println("getPfcSndDataGeneric: field "+ entry.getKey() + " value "+ entry.getValue().getConstraintValue() + " operand= "+ entry.getValue().getConstraintType());
            //}
			List<String> parameters = new ArrayList<String>(12);
            parameters.addAll(ModelSoundingParameters.LVL_PARAMETERS);
            parameters.add(ModelSoundingParameters.LATITUDE);
            parameters.add(ModelSoundingParameters.LONGITUDE);
            parameters.add(ModelSoundingParameters.ELEVATION);
            parameters.add(ModelSoundingParameters.STATION_ID);
            parameters.add(ModelSoundingParameters.STATION_NUMBER);
            parameters.add(ModelSoundingParameters.REF_TIME);
            parameters.add(ModelSoundingParameters.FORECAST_HOUR);
			
			try {
				long t01 = System.currentTimeMillis();
				lSndSiteRecords = ModelSoundingPointDataTransform.getSoundingSites(constraints, parameters);
				long t02 = System.currentTimeMillis();
				//System.out.println("getPfcSndDataGeneric sounding site record size = "+ lSndSiteRecords.size()+
				//		" took "+(t02-t01)+ " ms");
                for (SoundingSite sndSite : lSndSiteRecords) {
					//set pf data
					NcSoundingProfile pf = new NcSoundingProfile();
					pf.setStationLatitude(sndSite.getLatitude());
					pf.setStationLongitude(sndSite.getLongitude());
					pf.setStationElevation((float)sndSite.getElevation());
					pf.setFcsTime((sndSite.getDataTime().getFcstTime()*1000)+ sndSite.getDataTime().getRefTime().getTime());
					if(sndSite.getSiteId()!=null && sndSite.getSiteId().length()>0)
						pf.setStationNum(Integer.parseInt(sndSite.getSiteId()));
					pf.setStationId(sndSite.getStationId());


					List<NcSoundingLayer> soundLyList = new ArrayList<NcSoundingLayer>();

					//for (int i=0; i<sizes[0]; i++)
					for (SoundingLevel sndLevel : sndSite.getLevels())
					{
						NcSoundingLayer soundingLy = new NcSoundingLayer();	
						soundingLy.setOmega(sndLevel.getOmega());
						soundingLy.setTemperature((float)kelvinToCelsius.convert(sndLevel.getTemperature()));
						soundingLy.setPressure(sndLevel.getPressure()/100);
						soundingLy.setWindU((float)metersPerSecondToKnots.convert(sndLevel.getUcWind())); // HDF5 data in unit of m/s, convert to Knots 4/12/2012 
						soundingLy.setWindV((float)metersPerSecondToKnots.convert(sndLevel.getVcWind()));
						soundingLy.setSpecHumidity(sndLevel.getSpecificHumidity());						
						soundLyList.add(soundingLy);
					}
					Collections.sort(soundLyList,reversePressureComparator());
					pf.setSoundingLyLst(soundLyList);

					ms.nativeModelSounding(pf.getSoundingLyLst(), pf.getStationElevation());
					if ( ms.isNumber (level) == 0 ) {
						//level is an integer >=0. It means user request a single level
						float rlev = new Integer(Integer.parseInt(level.trim())).floatValue();
						pf.setSoundingLyLst(ms.getSingLevel(rlev, pf.getSoundingLyLst()));
					} else if ( ms.isNumber (level) == 1 ) {      	
						//level is an float >=0. It also means user request a single level
						float rlev = new Float(Float.parseFloat(level.trim()));
						pf.setSoundingLyLst(ms.getSingLevel(rlev, pf.getSoundingLyLst()));
					} 

					pfs.add(pf);
					//System.out.println("sounding fcs time = "+ pf.getFcsTime());
					//System.out.println("sounding ref time = "+ sndSite.getDataTime().getRefTime().getTime());

				}
				
			} catch (Exception e) {
				//System.out.println("exception=" + e );
				e.printStackTrace();
				return pfs;
			}
				
		}
		
		//debug
		
		//long t02 = System.currentTimeMillis();
		//System.out.println("getPfcSndData2 PFC profile retreival took " + (t02 - t01));
		return pfs;
    }
	private static Comparator<NcSoundingLayer> reversePressureComparator() {

        return new Comparator<NcSoundingLayer>() {

            @Override
            public int compare(NcSoundingLayer layerA, NcSoundingLayer layerB) {
                int retValue = 0;
                if (layerA != layerB) {
                    // reverse sort relative to pressure!
                    retValue = Double.compare(layerB.getPressure(), layerA
                            .getPressure());
                }
                return retValue;
            }
        };
    }
    /*
	private static void readH5FileTest(String fname) {
		   // retrieve an instance of H5File
     FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);

     if (fileFormat == null)
     {
     	System.out.print("Cannot find HDF5 FileFormat.");
         return;
     }

     // open the file with read access
     FileFormat testFile;
		try {
			testFile = fileFormat.open(fname, FileFormat.READ);
		       if (testFile == null)
		        {
		    	   System.out.print("Failed to open file: "+fname);
		            return;
		        }


     // open the file and retrieve the file structure
     testFile.open();
     //Group root = (Group)((javax.swing.tree.DefaultMutableTreeNode)testFile.getRootNode()).getUserObject();

     // retrieve the dataset "2D 32-bit integer 20x10"
     //Dataset dataset = (Dataset)root.getMemberList().get(4);
     Dataset dataset = (Dataset)testFile.get("validTime");
     dataset.init();
     //long[] dim = new long[2];
     long rank = dataset.getRank();
     long dim[] = dataset.getDims();
     for (int i=0; i<rank; i++)
     {
     	System.out.print("dim"+i+"= "+ dim[i]);
     }
     long [] dataRead = (long [])dataset.read();
     
     
     

     // print out the data values
     System.out.println("\nData Values");
     if(rank == 1){
         for (int j=0; j<dim[0]; j++)
         {
             System.out.print(", "+dataRead[j]);
         }
         
     }
     if(rank == 2)
     {
     	
     	for (int j=0; j<dim[0]; j++)
         {
     		System.out.println("Line "+ j);
     		for (int i=0; i<dim[1]; i++)
             {
     			System.out.print(", "+dataRead[j*(int)dim[1]+i]);
             }
     		System.out.println();
         }
         
     }

     // close file resource
     testFile.close();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
*/
	/*
	 * 
	 * Chin: using ModelSoundingPointDataTransform for query.
	 * Sounding time array is an array of user picked PFC sounding time String(s) in this format "2012-02-11 12:00:00"
	 * Replaced by getPfcSndDataGeneric() 
	 *
	
	public static List<NcSoundingProfile>  getPfcSndDataBySoundTimeRangeArray(double lat, double lon, String stn, String refTimeStr, List<String> soundingTimeAry, String sndTypeStr, SndQueryKeyType queryType) {
		//System.out.println("getPfcSndData  lat="+lat+" lon="+lon);
    	//NcSoundingProfile.PfcSndType sndType; //yes, it is not used now..
    	//Timestamp validTime = new Timestamp(validTimeL);
		List<NcSoundingProfile> pfs = new ArrayList<NcSoundingProfile>();
		
		if(sndTypeStr.equals(PfcSndType.GFSSND.toString())  || sndTypeStr.equals(PfcSndType.NAMSND.toString())){
    		List<String> fields = new ArrayList<String>();
			List<Object> values = new ArrayList<Object>();
			List<SoundingSite> lSndSiteRecords = null;
			List<String> operands = new ArrayList<String>();
			if(queryType==SndQueryKeyType.STNID){
				fields.add("location.stationId");// the location.stationId String field name defined in SoundingSite class and decoded modelsounding table.
				values.add(stn);
				operands.add("=");
			}
			else if(queryType==SndQueryKeyType.STNNUM){
				fields.add("siteid");// the siteid String field name defined in SoundingSite class and decoded in modelsounding table.
				values.add(stn);
				operands.add("=");
			}
			else if(queryType==SndQueryKeyType.LATLON){
				//fields.add("location.latitude");// the location.latitude field name defined in SoundingSite class and decoded modelsounding table
				//values.add(lat-0.1); 
				//operands.add(">=");
				//fields.add("location.latitude");// the location.latitude field name defined in SoundingSite class and decoded modelsounding table
				//values.add(lat+0.1); 
				//operands.add("<=");
				//fields.add("location.longitude");// the location.longitude field name defined in SoundingSite class and decoded modelsounding table
				//values.add(lon-0.1); 
				//operands.add(">=");
				//fields.add("location.longitude");// the location.longitude field name defined in SoundingSite class and decoded modelsounding table
				//values.add(lon+0.1); 
				//operands.add("<=");
				fields.add("location.latitude");// the location.latitude field name defined in SoundingSite class and decoded modelsounding table
				values.add(lat); 
				operands.add("=");
				fields.add("location.longitude");// the location.longitude field name defined in SoundingSite class and decoded modelsounding table
				values.add(lon); 
				operands.add("=");
				
				
			}
			else {
				System.out.println("request query type "+ queryType+ " is not supported in this API" );
				return pfs;
			}
			
			fields.add("dataTime.refTime");// the refTime time field name defined in SoundingSite and decoded modelsounding table
			values.add(refTimeStr); //refTime data type defined in SoundingSite is "Date"
			operands.add("=");
			fields.add("dataTime.validPeriod.start");// the rangeStart field name defined in SoundingSite and decoded modelsounding table
			String d="";
			for (String timeStr: soundingTimeAry){
				d = d+timeStr;
				d= d+",";
			}
			d=d.substring(0, d.length()-1);//get rid of last ","
			values.add(d);  //rangestart data type defined in SoundingSite is "Date"
			operands.add("in");
			//for (int i=0; i < fields.size(); i++) {
			//	System.out.println("field "+ fields.get(i) + " value "+ values.get(i));
			//}
			List<String> parameters = new ArrayList<String>(12);
            parameters.addAll(ModelSoundingPointDataTransform.LVL_PARAMETERS);
            parameters.add(ModelSoundingPointDataTransform.P_LATITUDE);
            parameters.add(ModelSoundingPointDataTransform.P_LONGITUDE);
            parameters.add(ModelSoundingPointDataTransform.P_ELEVATION);
            parameters.add(ModelSoundingPointDataTransform.P_STATION_ID);
            parameters.add(ModelSoundingPointDataTransform.P_STATION_NUMBER);
            parameters.add(ModelSoundingPointDataTransform.P_REF_TIME);
            parameters.add(ModelSoundingPointDataTransform.P_FORECAST_HOUR);
			
			try {
				long t01 = System.currentTimeMillis();
				lSndSiteRecords = ModelSoundingPointDataTransform.getSoundingSites(fields, values, operands, parameters);
				long t02 = System.currentTimeMillis();
				System.out.println("getPfcSndDataBySoundTimeRangeArray sounding site record size = "+ lSndSiteRecords.size()+
						" took "+(t02-t01)+ " ms");
				for(SoundingSite sndSite:lSndSiteRecords){
					//set pf data
					List<NcSoundingLayer> soundLyList = new ArrayList<NcSoundingLayer>();
					NcSoundingProfile pf = new NcSoundingProfile();
					pf.setStationLatitude(sndSite.getLatitude());
					pf.setStationLongitude(sndSite.getLongitude());
					pf.setStationElevation((float)sndSite.getElevation());
					pf.setFcsTime((sndSite.getDataTime().getFcstTime()*1000)+ sndSite.getDataTime().getRefTime().getTime());
					if(sndSite.getSiteId()!=null && sndSite.getSiteId().length()>0)
						pf.setStationNum(Integer.parseInt(sndSite.getSiteId()));
					pf.setStationId(sndSite.getStationId());
					//for (int i=0; i<sizes[0]; i++)
					for (SoundingLevel level : sndSite.getLevels())
					{
						NcSoundingLayer soundingLy = new NcSoundingLayer();	
						soundingLy.setOmega(level.getOmega().floatValue());
						soundingLy.setTemperature((float)kelvinToCelsius.convert(level.getTemperature()));
						soundingLy.setPressure(level.getPressure().floatValue()/100);
						soundingLy.setWindU(level.getUcWind().floatValue()); // HDF5 data in unit of Knots, no conversion needed
						soundingLy.setWindV(level.getVcWind().floatValue());
						soundingLy.setSpecHumidity(level.getSpecificHumidity().floatValue());						
						soundLyList.add(soundingLy);
					}
					Collections.sort(soundLyList,reversePressureComparator());
					pf.setSoundingLyLst(soundLyList);
					pfs.add(pf);
					//System.out.println("sounding fcs time = "+ pf.getFcsTime());
					//System.out.println("sounding ref time = "+ sndSite.getDataTime().getRefTime().getTime());

				}
				
			} catch (Exception e) {
				//System.out.println("exception=" + e );
				e.printStackTrace();
				return pfs;
			}
				
		}
		
		//debug
		
		//long t02 = System.currentTimeMillis();
		//System.out.println("getPfcSndData2 PFC profile retreival took " + (t02 - t01));
		return pfs;
    }
	 */
}
