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
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfo;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.PfcSndType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.SndQueryKeyType;

import java.io.File;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;


import com.raytheon.edex.plugin.modelsounding.common.ModelSoundingPointDataTransform;
import com.raytheon.edex.plugin.modelsounding.common.SoundingLevel;
import com.raytheon.edex.plugin.modelsounding.common.SoundingSite;
import com.raytheon.edex.plugin.modelsounding.dao.ModelSoundingDAO;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

public class PfcSoundingQuery {
	private static final String PFC_TBL_NAME = "modelsounding";
	private static String currentDBTblName = "nil";
	private static String reportType;
	public static UnitConverter kelvinToCelsius = SI.KELVIN.getConverterTo(SI.CELSIUS);
	
	public static NcSoundingStnInfoCollection getPfcSndStnInfoCol(String sndType, String selectedSndTime) {
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
				selectedSndTime+ "' AND reporttype ='" + reportType + "' AND latitude BETWEEN -89.9 AND 89.9 AND longitude BETWEEN -179.9 AND 179.9");

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
				stn.setStationLongitude((float)lon);
				stn.setStationLatitude((float)lat);
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
				" where reporttype='" +reportType+ "' AND "+ "reftime='"+refTimeStr+":00:00'"+" ORDER BY rangestart DESC");
		System.out.println("queryStr  "+ queryStr);


		CoreDao dao = new CoreDao(DaoConfig.forClass(SoundingSite.class));
		refTimeAry = (Object[]) dao.executeSQLQuery(queryStr);
		tl.setTimeLines(refTimeAry);

		return tl;
	}
	
	public static NcSoundingProfile getPfcSndData(double lat, double lon, String stn, long refTimeL, long validTimeL, String sndTypeStr, SndQueryKeyType queryType) {
		//*System.out.println("getPfcSndData input ref time = "+ refTimeL+" valid time is " + validTimeL);
		Calendar refTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		refTimeCal.setTimeInMillis(refTimeL);
		Calendar validTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		validTimeCal.setTimeInMillis(validTimeL);
		return getPfcSndData( lat,  lon,  stn, refTimeCal,  validTimeCal,  sndTypeStr, queryType);
	}

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
					pf.setStationLatitude((float)lSndSiteRecords.get(0).getLatitude());
					pf.setStationLongitude((float)lSndSiteRecords.get(0).getLongitude());
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
						//*System.out.println("hdf5 path = " + hdf5loc.getAbsolutePath());
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
			
			/*
			SoundingSite sndSite = new SoundingSite();
			sndSite.setPluginName("modelsounding");

			DataTime refTimeDataTime = new DataTime(refTimeCal);
			sndSite.setDataTime(refTimeDataTime);
			
				ModelSoundingDAO mdldao =  new ModelSoundingDAO("modelsounding");
				File hdf5loc = mdldao.getFullFilePath(sndSite);
				//*System.out.println("hdf5 path = " + hdf5loc.getAbsolutePath());
				IDataStore dataStore = DataStoreFactory.getDataStore(hdf5loc);
				try {
					LongDataRecord validtimeLongData = (LongDataRecord) dataStore.retrieve(
							"/", "validTime", Request.ALL);
					long[] validtimedata = validtimeLongData.getLongData();

					FloatDataRecord latfloatData = (FloatDataRecord) dataStore.retrieve(
							"/", "latitude", Request.ALL);
					float[] latdata = latfloatData.getFloatData();

					FloatDataRecord lonfloatData = (FloatDataRecord) dataStore.retrieve(
							"/", "longitude", Request.ALL);
					float[] londata = lonfloatData.getFloatData();

					FloatDataRecord elvfloatData = (FloatDataRecord) dataStore.retrieve(
							"/", "elevation", Request.ALL);
					float[] elvdata = elvfloatData.getFloatData();



					int selectedTimeIndex=-1; 

					for (int j=0; j<validtimedata.length; j++)
					{
						//find the index of user picked data time line and lat/lon
						if((    queryType.equals(SndQueryKeyType.LATLON.toString())&&
								(validtimedata[j] == validTimeL/1000) &&
								(latdata[j] ==(float) lat)	&& 
								(londata[j] == (float)lon)	)
							||
							(	queryType.equals(SndQueryKeyType.STNID.toString())&&
								(validtimedata[j] == validTimeL/1000) &&
								(latdata[j] ==(float) lat)	&& 
								(londata[j] == (float)lon)	)
							||
							(	queryType.equals(SndQueryKeyType.STNNUM.toString())&&
									(validtimedata[j] == validTimeL/1000) &&
									(latdata[j] ==(float) lat)	&& 
									(londata[j] == (float)lon)	)
						)
						{
							selectedTimeIndex = j;
							pf.setStationLatitude(latdata[j]);
							pf.setStationLongitude(londata[j]);
							pf.setStationElevation(elvdata[j]);
							break;
						}
					}
					if(selectedTimeIndex != -1) {
						NcSoundingLayer soundingLy;

						// get temp, dew point, pressure, wind u/v components, specHum, omega
						FloatDataRecord omegafloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "omega", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
						float[] omegadata = omegafloatData.getFloatData();
						long[] sizes = omegafloatData.getSizes();
						//int dim = omegafloatData.getDimension();
						FloatDataRecord pressurefloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "pressure", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
						float[] pressuredata = pressurefloatData.getFloatData();
						FloatDataRecord temperaturefloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "temperature", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
						float[] temperaturedata = temperaturefloatData.getFloatData();
						FloatDataRecord specHumfloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "specHum", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
						float[] specHumdata = specHumfloatData.getFloatData();
						FloatDataRecord vCompfloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "vComp", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
						float[] vCompdata = vCompfloatData.getFloatData();
						FloatDataRecord uCompfloatData = (FloatDataRecord) dataStore.retrieve(
								"/", "uComp", Request.buildYLineRequest(new int[] {selectedTimeIndex}));
						float[] uCompdata = uCompfloatData.getFloatData();

						for (int i=0; i<sizes[0]; i++)
						{
							soundingLy = new NcSoundingLayer();	
							soundingLy.setOmega(omegadata[i]);
							soundingLy.setTemperature((float)kelvinToCelsius.convert(temperaturedata[i]));
							soundingLy.setPressure(pressuredata[i]/100F);
							soundingLy.setWindU(uCompdata[i]);
							soundingLy.setWindV(vCompdata[i]);
							soundingLy.setSpecHumidity(specHumdata[i]);						
							soundLyList.add(soundingLy);
						}
						//*System.out.println("sounding layer size = "+ soundLyList.size());
						//debug
						for(NcSoundingLayer ly: soundLyList){
							//*System.out.println("P= "+ly.getPressure()+ " Hm= "+ ly.getSpecHumidity()+ " T= "+ ly.getTemperature());
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

		}*/

		//IHDFFilePathProvider pathProvider = sndSite.getHDFPathProvider();
		
		//File hdf5loc = HDF5Util.findHDF5Location(sndSite);
		
		//IDataStore dataStore = DataStoreFactory.getDataStore(hdf5loc);

		
		/*
		//readH5FileTest("/usr1/cchen/to11dr11/awips/edex/data/hdf5/modelsounding/2010/06/15/06/modelsounding-0.h5");
        String hdf5File ="/usr1/cchen/to11dr11/awips/edex/data/hdf5/modelsounding/2010/06/15/06/modelsounding-0.h5";
        //String group = sndSite.getDataURI();
        String dataset = "omega";//TBD
        IDataRecord dr;
        
        
        int file_id = -1;
		int dataset_id = -1;
		float[][] dset_data = new float[182][64];
		// Open file using the default properties.
		try {
			file_id = H5.H5Fopen(hdf5File, HDF5Constants.H5F_ACC_RDONLY,
					HDF5Constants.H5P_DEFAULT);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Open dataset using the default properties.
		try {
			if (file_id >= 0)
				dataset_id = H5.H5Dopen(file_id, dataset);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Read the data using the default properties.
		try {
			if (dataset_id >= 0)
				
				H5.H5Dread(dataset_id, HDF5Constants.H5T_FLOAT,
						HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
						HDF5Constants.H5P_DEFAULT, dset_data);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		// Output the data to the screen.
		System.out.println(dataset + ":");
		for (int indx = 0; indx < 182; indx++) {
			System.out.print(" [ ");
			for (int jndx = 0; jndx < 64; jndx++)
				System.out.print(dset_data[indx][jndx] + " ");
			System.out.println("]");
		}
		System.out.println();

        
		// Close the dataset.
		try {
			if (dataset_id >= 0)
				H5.H5Dclose(dataset_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		try {
			if (file_id >= 0)
				H5.H5Fclose(file_id);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		*/
		pf.setSoundingLyLst(soundLyList);
		long t02 = System.currentTimeMillis();
		System.out.println("PFC profile retreival took " + (t02 - t01));
		return pf;
    }

	
	//@SuppressWarnings("unchecked")
	/*
	 * Chin: using ModelSoundingPointDataTransform for query. Need test after 11.5
	 */
	
	public static NcSoundingProfile getPfcSndData2(double lat, double lon, String stn, Calendar refTimeCal, Calendar validTimeCal, String sndTypeStr, SndQueryKeyType queryType) {
		//System.out.println("getPfcSndData  lat="+lat+" lon="+lon);
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
			List<String> parameters = new ArrayList<String>(12);
            parameters.addAll(ModelSoundingPointDataTransform.LVL_PARAMETERS);
            parameters.add(ModelSoundingPointDataTransform.P_LATITUDE);
            parameters.add(ModelSoundingPointDataTransform.P_LONGITUDE);
            parameters.add(ModelSoundingPointDataTransform.P_ELEVATION);
            parameters.add(ModelSoundingPointDataTransform.P_STATION_ID);
            parameters.add(ModelSoundingPointDataTransform.P_STATION_NUMBER);
            
			
			try {
				lSndSiteRecords = ModelSoundingPointDataTransform.getSoundingSites(fields, values, operands, parameters);
			
				if(lSndSiteRecords.size() > 0){
					//set pf data
					
					pf.setStationLatitude((float)lSndSiteRecords.get(0).getLatitude());
					pf.setStationLongitude((float)lSndSiteRecords.get(0).getLongitude());
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
							soundingLy.setOmega(level.getOmega().floatValue());
							soundingLy.setTemperature((float)kelvinToCelsius.convert(level.getTemperature()));
							soundingLy.setPressure(level.getPressure().floatValue()/100);
							soundingLy.setWindU(level.getUcWind().floatValue()); // HDF5 data in unit of Knots, no conversion needed
							soundingLy.setWindV(level.getVcWind().floatValue());
							soundingLy.setSpecHumidity(level.getSpecificHumidity().floatValue());						
							soundLyList.add(soundingLy);
						}
						//System.out.println("sounding layer size = "+ soundLyList.size());
						//debug
						//for(NcSoundingLayer ly: soundLyList){
							//*System.out.println("P= "+ly.getPressure()+ " Hm= "+ ly.getSpecHumidity()+ " T= "+ ly.getTemperature());
						//}
					
					
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
}
