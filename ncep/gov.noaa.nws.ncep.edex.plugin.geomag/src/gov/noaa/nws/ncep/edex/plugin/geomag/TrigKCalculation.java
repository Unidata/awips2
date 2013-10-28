package gov.noaa.nws.ncep.edex.plugin.geomag;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagAvg;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK3hr;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagAvgDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagK1minDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagK3hrDao;
import gov.noaa.nws.ncep.edex.plugin.geomag.calculation.CalcEach1min;
import gov.noaa.nws.ncep.edex.plugin.geomag.calculation.CalcEach3hr;
import gov.noaa.nws.ncep.edex.plugin.geomag.calculation.CalcKp;
import gov.noaa.nws.ncep.edex.plugin.geomag.calculation.CalcUtil;

import java.io.FileNotFoundException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * This java class calculates magnetometer k index and related values.
 * 
 * <pre>
 * OFTWARE HISTORY
 *                   
 * date         Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 06/07/2013   #989        qzhou       Initial Creation
 * </pre>
 * 
 * @author qzhou
 * @version 1
 * */

public class TrigKCalculation {
	private final Log logger = LogFactory.getLog(getClass());
	private static final String GeoMag = "geomag";
	private static final float MISSING_VAL = 99999.99f;
	private static final int MISSING_INT = 99999;
	private static final int DAYS = 30;
	private static final int HOURS = 24;
	private static final int MINUTES = 60;
	
	private static final int AVG_DATA_RANGE = 30;
	private static final int HD_DATA_RANGE = 3;
	private static final int MAX_SOURCES = 3;
	
	private static final int ITERATIONS = 5;
	private static final int MAX_GAP_LENGTH = 15;
	private static final int SMOOTH_WINDOW = 60;
	private static final int TRANSITION_TIME = 60;
	private static final int PHASE_POWER = 3;
	private static final int HARM_ORDER = 5;
	private GeoMagDao dao; //PluginDao dao;    
	private float[] defLength = new float[HOURS]; 
	
	private Map<String, List<float[]>> stationMap = new HashMap<String, List<float[]>>(); //station, arrays
	String format = "yyyy-MM-dd'_'HH:mm:ss.s";     	    		
	SimpleDateFormat sdf = new SimpleDateFormat(format);
	
	
	public TrigKCalculation() {		
//		KStationCoefficientLookup look = KStationCoefficientLookup.getInstance();
//		Map<String, KStationCoefficient> stationMap = look.getStationsByCodeMap();
//		int size = 1+31;
//		stations = new ArrayList<List>();//String[]: station, uri0, ...uri31
//		for (Map.Entry<String, KStationCoefficient> entry : stationMap.entrySet()) {
//			List<String> astation = new ArrayList<String>();
//			astation.add( entry.getKey());
//			stations.add(astation);
////			System.out.println("***entry "+ entry.getKey());
//		}
	}
	
	public void trig1min(Object obj) throws StorageException { 
		
		if( !(obj instanceof DataURINotificationMessage) ){
//			GeoMag.logError("Received msg that is not a DataURINotificationMessage? msg is "+
//					obj.getClass().getName() );
			logger.info("Received msg that is not a DataURINotificationMessage.");
		}
		
		DataURINotificationMessage uriMsg = (DataURINotificationMessage)obj;

		String[] dataURIs = uriMsg.getDataURIs();

		//sort
		Arrays.sort(dataURIs);
//		for (int i=0; i<dataURIs.length; i++)
//		System.out.println("**sort dataUri " +dataURIs[i]);
		
		logger.info("******** Start meganetometer calculation trig.");

//		long t0 = Calendar.getInstance().getTimeInMillis();
//		System.out.println("*****T0 "+ t0);
		
		try {
			dao = (GeoMagDao) PluginFactory.getInstance().getPluginDao(GeoMag);
		} catch (PluginException e) {
			e.printStackTrace();
		}
		
		calcSimpleHourAvg(dataURIs);
//		long t2 = Calendar.getInstance().getTimeInMillis();
//		System.out.println("*****T2 "+ t2);
		
//		calcBy3hr( dataURIs);
//		long t3 = Calendar.getInstance().getTimeInMillis();
//		System.out.println("*****T3 "+ t3);
//							
//		Map<String, List<float[]>> kIndexMap = calcBy1min(dataURIs);	
//		long t4 = Calendar.getInstance().getTimeInMillis();
//		System.out.println("*****T4 "+ t4);
//		
//		calcK3h(dataURIs, kIndexMap);
//		long t5 = Calendar.getInstance().getTimeInMillis();
//		System.out.println("*****T5 "+ t5);
	 }
	
	/*
	 * 
	 */
	public List<GeoMagAvg> retrieveSingleAvg(String dataUri, Date time) {
		GeoMagAvgDao avgDao = new GeoMagAvgDao();
		String station = CalcUtil.getStationFromUri(dataUri);
		
		DatabaseQuery query = new DatabaseQuery(GeoMagRecord.class.getName());
		query.addReturnedField("id");
		query.addQueryParam("avgTime", time);
		query.addQueryParam("stationCode", station);
		 
		List<GeoMagAvg> resultsList = null;				
		resultsList = avgDao.getSingleAvg(station, time);	
			
		return resultsList;             
        
	}
	
	/*
	 * 
	 */
	public List<?> retrieveUriForAvg(String dataUri, Date time) {
		String station = CalcUtil.getStationFromUri(dataUri);
		
		DatabaseQuery query = new DatabaseQuery(GeoMagRecord.class.getName());
		//query.addReturnedField("id");
		query.addReturnedField("component_1");
		query.addReturnedField("component_2");
		query.addReturnedField("dataTime.refTime");
		query.addReturnedField("badDataPoint");
		query.addReturnedField("sourceId");
		query.addQueryParam("dataTime.refTime", time, QueryParam.QueryOperand.LESSTHANEQUALS);
		Calendar cal = Calendar.getInstance();
		cal.setTime(time);
		cal.add(Calendar.HOUR_OF_DAY, -1); // at least one day is needed for gt, lt
		query.addQueryParam("dataTime.refTime", cal.getTime(), QueryParam.QueryOperand.GREATERTHAN);
		query.addQueryParam("stationCode", station);
		 
		List<?> resultsList = null;
		
		try {
			resultsList = dao.queryByCriteria(query);	// 60			
		} catch (DataAccessLayerException e1) {
			e1.printStackTrace();
		}
		
		return resultsList;             		
	}
	
	/*
	 * 
	 */
	public List<GeoMagAvg> retrieveUriBy3hr(String dataUri, Date spTime){
		GeoMagAvgDao avgDao = new GeoMagAvgDao();
//		long t0 = Calendar.getInstance().getTimeInMillis();
//    	System.out.println("*****ttt0 "+ t0);
		String station = CalcUtil.getStationFromUri(dataUri);
		
		Calendar cal = Calendar.getInstance();
		cal.setTime(spTime);
		cal.add(Calendar.DAY_OF_YEAR, -AVG_DATA_RANGE); // at least one day is needed for gt, lt
		
		List<GeoMagAvg> resultsList = null;				
		resultsList = avgDao.getAvgForStation(station, cal.getTime(), spTime);	//720			
		
//		long t1 = Calendar.getInstance().getTimeInMillis();
//    	System.out.println("*****ttt1 "+ t1);
    	
		return resultsList;             		
	}
	
	/*
	 * 
	 */
	public List<?> retrieveUriForK1min(String dataUri, Date epTime){
//		long t0 = Calendar.getInstance().getTimeInMillis();
//    	System.out.println("*****tttt0 "+ t0+" "+epTime);
    	
		String station = CalcUtil.getStationFromUri(dataUri);
		
		DatabaseQuery query = new DatabaseQuery(GeoMagRecord.class.getName());
		//query.addReturnedField("id");
		query.addReturnedField("component_1");
		query.addReturnedField("component_2");
		query.addReturnedField("dataTime.refTime");
		query.addReturnedField("badDataPoint");
		query.addReturnedField("sourceId");
		query.addQueryParam("dataTime.refTime", epTime, QueryParam.QueryOperand.LESSTHANEQUALS);
		Calendar cal = Calendar.getInstance();
		cal.setTime(epTime);
		cal.add(Calendar.HOUR_OF_DAY, -48); // at least one day is needed for gt, lt
		query.addQueryParam("dataTime.refTime", cal.getTime(), QueryParam.QueryOperand.GREATERTHAN);
		query.addQueryParam("stationCode", station);
		 
		List<?> resultsList = null;
		
		try {
			resultsList =  dao.queryByCriteria(query);	// 2880			
		} catch (DataAccessLayerException e1) {
			e1.printStackTrace();
		}
		
//		long t1 = Calendar.getInstance().getTimeInMillis();
//    	System.out.println("*****tttt1 k1min "+ t1);
    	
		return resultsList;             
	}
	
	/*
	 * 
	 */
	public List<GeoMagK3hr> retrieveUriForK3hr(String dataUri, Date time){
		GeoMagK3hrDao k3hrDao = new GeoMagK3hrDao();
		String station = CalcUtil.getStationFromUri(dataUri);

		List<GeoMagK3hr> resultsList = null;				
		resultsList = k3hrDao.getK3hrForStation(station, time);	//1			
		
		return resultsList;             		
	}
	
	/*
	 * 
	 */
	public IDataRecord[] getDataRecords(String uri){
		IDataRecord[] dataRec = null;
		IDataStore dataStore = null;
		
		GeoMagRecord record = new GeoMagRecord(uri);   		   			
	    if (record != null)
	    	dataStore = dao.getDataStore((IPersistable) record);
	    
	    try {				   
			dataRec = dataStore.retrieve(uri); //obs_time, compx...//size 7
		} catch (FileNotFoundException e1) {
			//e1.printStackTrace();
			System.out.println("This uri didn't find the records.");
		} catch (StorageException e1) {
			System.out.println("This uri didn't find place to store the records.");
			//e1.printStackTrace();
		}	
		
		return dataRec;
	}
	
	/*
	 * sort n lists
	 */
	static void sort(List... lists) {
	    assert lists.length > 0;
	    
	    Object[][] objects = new Object[lists[0].size()][lists.length];

	    for (int i = 0; i < lists.length; i++) {
	        int j = 0;
	        for (Object object : lists[i]) {
	            objects[j++][i] = object;
	        }
	    }

	    Arrays.sort(objects, new Comparator<Object[]>() {
	        @SuppressWarnings("unchecked")
			public int compare(Object[] o1, Object[] o2) {
	            return ((Comparable)o1[0]).compareTo(o2[0]);
	        }
	    });

	    for (int i = 0; i < lists.length; i++) {
	        lists[i].clear();
	        for (Object[] tuple : objects) {
	            lists[i].add(tuple[i]);
	        }
	    }
	}
	
	/*
	 * Input data of all source, output with higher priority source data
	 */
	public List<List> getBestObserv(List<?> dataList ) {
		
	    //List<Integer> idList = new ArrayList<Integer>();
	    List<Float> comp1List = new ArrayList<Float>();
	    List<Float> comp2List = new ArrayList<Float>();
	    List<String> badPointList = new ArrayList<String>();
		List<Date> dateList = new ArrayList<Date>();
		List<Integer> sourceList = new ArrayList<Integer>();
		
		List<List> bestList = new ArrayList<List>();
		List<List> fullBestList = new ArrayList<List>();
		
		if (dataList != null ) { 
			for (int i = 0; i < dataList.size(); i++) {  
				
				Object[] row = (Object[]) dataList.get(i);
				
				//idList.add( (Integer) row[0]);
				comp1List.add( (Float) row[0]);
				comp2List.add( (Float) row[1]);
				dateList.add( (Date) row[2]);
				badPointList.add( (String) row[3]);
				sourceList.add( (Integer) row[4]);				
				//System.out.println("***row "+dateList.get(i)+" "+sourceList.get(i)+" " +comp2List.get(i));
			}
		
			sort(dateList, sourceList, comp1List, comp2List, badPointList); //, idList); 			
			
			int count = 0;
			int size = dateList.size();
			
			/*
			 * tempList combine all lists for the first 4 items. size=4
			 * newList put tempList ordered by source. size=3
			 * bestList construct newList with best source
			 * bestListFull filled time gaps
			 */ 		
			for (int i = 0; i < size; i=i+count) {
				count = 0;
				
				List tempList1 = new ArrayList();
				List tempList2 = new ArrayList();
				List tempList3 = new ArrayList();
				
				List<List> newList = new ArrayList<List>();   			
				newList.add(0, new ArrayList());//init 3
				newList.add(1, new ArrayList());
				newList.add(2, new ArrayList());
				
				//tempList1.add(0, idList.get(i));
				tempList1.add( dateList.get(i));    				
				if (badPointList.get(i) != null && badPointList.get(i) != ""){   					
					tempList1.add(MISSING_VAL);
					tempList1.add(MISSING_VAL);    					
				}
				else {
					tempList1.add(comp1List.get(i));
					tempList1.add(comp2List.get(i));
				}    					
				newList.set(sourceList.get(i)%100 -1, tempList1);
				count++;
				
				if (i+1 < size && dateList.get(i).compareTo( dateList.get(i+1)) ==0) {
					//tempList2.add(idList.get(i+1));
					tempList2.add(dateList.get(i+1));
					if (badPointList.get(i+1) != null && badPointList.get(i+1) != ""){   					
						tempList2.add(MISSING_VAL);
						tempList2.add(MISSING_VAL);    					
					}
					else {
						tempList2.add(comp1List.get(i+1));
						tempList2.add(comp2List.get(i+1));
					}    					
					newList.set(sourceList.get(i+1)%100 -1, tempList2);
					count++;
				}
				
				if (i+2 < size && dateList.get(i).compareTo( dateList.get(i+2)) ==0) {
					//tempList3.add(idList.get(i+2));
					tempList3.add(dateList.get(i+2));
					if (badPointList.get(i+2) != null && badPointList.get(i+2) != ""){   					
						tempList3.add(MISSING_VAL);
						tempList3.add(MISSING_VAL);    					
					}
					else {
						tempList3.add(comp1List.get(i+2));
						tempList3.add(comp2List.get(i+2));
					}    					
					newList.set(sourceList.get(i+2)%100 -1, tempList3);
					count++;
				}
				
				if (newList.get(2) == null || newList.get(2).isEmpty()) //newList.get(0)= [3281750, 2013-05-06 00:00:00.0, 20829.85, -297.05]
					newList.remove(2);
				if (newList.get(1) == null || newList.get(1).isEmpty())
					newList.remove(1);
				if (newList.get(0) == null || newList.get(0).isEmpty()) 
					newList.remove(0);    				   								
				//System.out.println("***newList "+i+" "+count+" "+newList.size()+" "+newList.get(0));  				
				
				//  Now only check if comp2 (...get(2)) is MISSING_VAL				
				if (newList.get(0).get(2) != null && (Float) newList.get(0).get(2) != MISSING_VAL ) {
					bestList.add( newList.get(0));    					
				}
				else if (newList.size() >1 && (Float) newList.get(0).get(2) == MISSING_VAL && i+1< size) { 
					// if date i = date(i+1) && comp1 (i+1) != missing
					if ((Date) newList.get(0).get(1) == (Date) newList.get(1).get(1) && newList.get(1).get(2) != null && (Float) newList.get(1).get(2) !=  MISSING_VAL) {
						bestList.add( newList.get(1));         					
					}
					else if (newList.size() >2 && (Float) newList.get(1).get(2) ==  MISSING_VAL && i+2 < size) {
						if ((Date) newList.get(0).get(1) == (Date) newList.get(2).get(1) && (Float) newList.get(2).get(2) !=  MISSING_VAL) {
							bestList.add( newList.get(2));    	    					
						}
						else {
							bestList.add( newList.get(0));   
						}
					}
				}
			}
		}
		//System.out.println("***bestList best "+bestList.size());
//	
//		long t2 = Calendar.getInstance().getTimeInMillis();
//    	System.out.println("*****tt2 "+ t2);		
		return bestList;
	}

	public List<List> fillHDTimeGaps(List<List> bestList) {
		List<List> fullBestList= new ArrayList<List>();
		// fill time gaps, get bestListFull
		// fill missing in the beginning		
		Date date = (Date) bestList.get(0).get(0);    //bestList.get(i) eq. newList.  	
		int min0 = date.getMinutes();
				
		if ( min0 != 0 ) {
			for (int k = 0; k < min0; k++) {
				List newList2 = new ArrayList(); // eq. newList
				
				Date dateNew = (Date)date.clone();
				dateNew.setMinutes(k);

				newList2.add(dateNew);
				newList2.add(MISSING_VAL);
				newList2.add(MISSING_VAL);				
				fullBestList.add( newList2);
		
			}			
		}
			// fill missing in the middle
			for (int j = 0; j < bestList.size(); j++ ) { //i=0 first non missing data		
				
				Date date0 = (Date) bestList.get(j).get(0);//dateList.get(i);
				fullBestList.add( bestList.get(j));
				
				if (j+1 < bestList.size()) {	
					Date date1 = (Date) bestList.get(j+1).get(0);//dateList.get(i+1);
					int diffMin = (int)(date1.getTime() - date0.getTime())/ (60*1000);
					
					if (diffMin != 1) {
						for (int k = 0; k < diffMin-1; k++) { 
							List newList2 = new ArrayList(); // eq. newList
							
							newList2.add(new Date(date0.getTime() + 60*1000*(k+1)));
							newList2.add(MISSING_VAL);
							newList2.add(MISSING_VAL);
							fullBestList.add( newList2);
							
						}
					}
				}	
			}
		
		
			// fill missing in the end
			date = (Date) bestList.get(bestList.size()-1).get(0);
			int minEnd = date.getMinutes();
			
			if ( minEnd < 59 ) {
				for (int k = minEnd+1; k < 60; k++) {
					List newList2 = new ArrayList(); // eq. newList
					
					Date dateNew = (Date)date.clone();					
					dateNew.setMinutes(k);
					
					newList2.add(dateNew);
					newList2.add(MISSING_VAL);
					newList2.add(MISSING_VAL);
					fullBestList.add( newList2);
					
				}
			}
			
//		for (int i = 0; i < fullBestList.size(); i++) {
//			System.out.println("***fullBestList "+fullBestList.size()+" "+fullBestList.get(i));
//		}
		
		return fullBestList;
	}
	
	
	/*
	 * when uri time is 59 min past the hour, calculate the averages and append to db
	 */
	public void calcSimpleHourAvg(String[] dataURIs) throws StorageException {	  
		
		if (dao != null && dataURIs != null) {
			for (String dataURI : dataURIs ) {
    			String stationCode = CalcUtil.getStationFromUri(dataURI);
    			
    			Date time = null;;
				try {
					time = CalcUtil.getTimeFromUri(dataURI);
				} catch (ParseException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				//int hour = time.getHours();
				int min = time.getMinutes();	
			
    			List<?> dataList = null;
    			if (min == 59)
    				dataList = retrieveUriForAvg(dataURI, time);
    			else
    				continue;
    				
    			
    			if (dataList != null && dataList.size() != 0) {
    				List<List> bestList = getBestObserv( dataList );
    			    	    	
	    			float[] hrAvg = CalcEach3hr.getSimpleHourAvg(bestList);	
	    			
	    			GeoMagAvg recAvg = new GeoMagAvg();

	    			// look the avg table to see if the avg already exists
	    			time.setMinutes(30);
	    			List<GeoMagAvg> avgList = retrieveSingleAvg(dataURI, time);
	    			
	    			if (avgList != null && avgList.size() != 0) {//String newUri = dataURI.substring(0, 21) +":30:00.0"+ dataURI.substring(29, 34)+ "100/GEOMAG";
	    				for (int i = 0; i < avgList.size(); i++) {  //1 	    					
	    					GeoMagAvg row = avgList.get(i);
	    					List<Integer> idList = new ArrayList<Integer>();
	    					idList.add( (Integer) row.getId());
	    					recAvg.setId((int) idList.get(0));
	    					
	    				}
	    			}
//	    			else {	
//	    				List<Integer> idList = bestList.get(bestList.size()-1); //last data id						
//	    				recAvg.setId((int) idList.get(0));
//	    				System.out.println("**idList "+idList);
//	    			}	    			
	    			
					recAvg.setAvgTime(time);
					recAvg.setInsertTime(Calendar.getInstance().getTime());
					recAvg.setStationCode(stationCode);
					recAvg.setHHrAvg(hrAvg[0]);
					recAvg.setDHrAvg(hrAvg[1]);
					
					GeoMagAvgDao avgDao = new GeoMagAvgDao();
					avgDao.persist(recAvg);
		
//					long t3 = Calendar.getInstance().getTimeInMillis();
//	    	    	System.out.println("*****tt3 "+ t3);	
    			}
        	} 
		}
		
		// if min=59 record=missing, look the avg table to insert missing avg
//		time.setMinutes(30);
//		List<GeoMagAvg> avgList = retrieveSingleAvg(dataURI, time);
    }

	
	/*
	 * 
	 */
	public Map<String, List<float[]>> calcBy3hr(String[] dataURIs) {
		GeoMagAvgDao avgDao = new GeoMagAvgDao();
		if (avgDao != null && dataURIs != null) {			
    		for (String dataURI : dataURIs ) {
    			String stationCode = CalcUtil.getStationFromUri(dataURI);
    			
    			Date timeBy3 = null;;
				try {
					timeBy3 = CalcUtil.getTimeFromUri(dataURI);
				} catch (ParseException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				int hour = timeBy3.getHours();
				int min = timeBy3.getMinutes();
				
				Date spTime = CalcUtil.getSPTime( timeBy3);
//				int spHour = spTime.getHours();
				
				List<GeoMagAvg> dataList = null;
				if ((hour%3 == 0 && min == 0 )
					|| stationMap.entrySet().isEmpty() )
    				dataList = retrieveUriBy3hr(dataURI, CalcUtil.getSPTime(timeBy3));
    			else
    				continue;

    			System.out.println("**resultsListby3.size() "+dataList.size()+" "+hour);	

    			
    			//List<Integer> idList = new ArrayList<Integer>();
    			List<Date> dateList = new ArrayList<Date>();
			    List<Float> hHrAvgList = new ArrayList<Float>();
			    List<Float> dHrAvgList = new ArrayList<Float>();
			      		    		
    			if (dataList != null && dataList.size() >= 5) { 
    				for (int i = 0; i < dataList.size(); i++) {  //1 extra
    					
    					GeoMagAvg row = dataList.get(i);
    					
    					dateList.add( (Date) row.getAvgTime());
    					hHrAvgList.add( (Float) row.getHHrAvg());
    					dHrAvgList.add( (Float) row.getDHrAvg());    					
	    				//System.out.println("***row "+dateList.get(i)+" "+hHrAvgList.get(i)+" " +dHrAvgList.get(i));
    				}
    			
					
	    			sort(dateList, hHrAvgList, dHrAvgList); 
	    			
	    			for (int i = 0; i < dateList.size(); i++) {
	    				System.out.println("***rowsortBy3 "+dateList.size()+" "+dateList.get(i)+" "+hHrAvgList.get(i)+" " +dHrAvgList.get(i));
	    			}
    			    	    			 
//	    			List<List> recList = new ArrayList();
//	    			recList.add(dateList);
//	    			recList.add(hHrAvgList);
//	    			recList.add(dHrAvgList);
//	    			List<List> recListFinal = fillAvgTimeGaps(recList);
	    			
	    			// fill missing
	    			//List<Integer> idListFinal = new ArrayList<Integer>();
	    			List<Date> dateListFinal = new ArrayList<Date>();
	    		    List<Float> hHrAvgListFinal = new ArrayList<Float>();
	    		    List<Float> dHrAvgListFinal = new ArrayList<Float>();
	    			
	    		    // fill missing in the beginning		
	    			Date date = (Date) dateList.get(0);//.get(0);    //bestList.get(i) eq. newList.  	
	    			int hr0 = date.getHours();
	    					
	    			if ( hr0 != 0 ) {
	    				for (int k = 0; k < hr0; k++) {
	    					List newList2 = new ArrayList(); // eq. newList
	    					
	    					Date dateNew = (Date)date.clone();
	    					dateNew.setMinutes(k);
			
	    					dateListFinal.add( dateNew);
	    					hHrAvgListFinal.add( MISSING_VAL);
							hHrAvgListFinal.add( MISSING_VAL);								
	    				}			
	    			}
	    			//// fill missing in the middle
	    			for (int i = 0; i < dateList.size(); i++) {  
	    				if (i+1 < dateList.size()) {
	    					Date date0 = dateList.get(i);
	    					dateListFinal.add(date);
	    					hHrAvgListFinal.add( hHrAvgList.get(i));
							hHrAvgListFinal.add( dHrAvgList.get(i));
	    					
	    					Date date1 = (Date)dateList.get(i+1);
	    					int diffHr = (int)(date1.getTime() - date.getTime())/ (3600*1000);

	    					if (diffHr != 1) {
	    						for (int j = 0; j < diffHr-1; j++) {  
	    							dateListFinal.add( new Date(date.getTime() + 3600*1000*(j+1))); //append after i, i+1
	    							//idList.add( idList.get(i)+1);
	    							hHrAvgListFinal.add( MISSING_VAL);
	    							hHrAvgListFinal.add( MISSING_VAL);
	    								
	    						}
	    					}
	    				}	
	    			}
	    			// fill missing in the end
	    			date = (Date) dateList.get(dateList.size()-1);
	    			int hrEnd = date.getMinutes();
				    
	    			if ( hrEnd < 59 ) {
	    				for (int k = hrEnd+1; k < 60; k++) {
	    					List newList2 = new ArrayList(); // eq. newList
				    
	    					Date dateNew = (Date)date.clone();					
	    					dateNew.setMinutes(k);
				    
	    					dateListFinal.add( new Date(date.getTime() + 3600*1000*(k+1))); //append after i, i+1
							//idList.add( idList.get(i)+1);
							hHrAvgListFinal.add( MISSING_VAL);
							hHrAvgListFinal.add( MISSING_VAL);
	    					
	    				}
	    			}
	    			for (int i = 0; i < dateListFinal.size(); i++) {
//	    				System.out.println("***rowsort2 "+dateListFinal.size()+" "+dateListFinal.get(i)+" "+hHrAvgListFinal.get(i));
	    			}
    			
    			
					float[] hHrAvgs = CalcUtil.toFloatArray(hHrAvgList);
					float[] dHrAvgs = CalcUtil.toFloatArray(dHrAvgList);
	    			float[] dB = CalcEach3hr.getDisturbanceLevel(hHrAvgs, dHrAvgs);
	//    			for ( int k = 0; k < 30; k++ )
	//    	        	System.out.println("*****dB "+ dB[k]);
	    	    	@SuppressWarnings("unchecked")
	    			Map<Integer, Float> dBsmall = CalcEach3hr.getSmallDisturbanceLevel(dB);
	    	    	
	    	    	float[] quietHHrAvg = CalcEach3hr.getQuietLevelHourAvg(dBsmall, hHrAvgs);
	    	    	float[] quietDHrAvg = CalcEach3hr.getQuietLevelHourAvg(dBsmall, dHrAvgs);  
	//	    	    	for (int i=0; i<dHrAvgs.length; i++)
	//			    		System.out.print("dHrAvgs "+dHrAvgs[i]);
	//	    	    	System.out.println("**dHrAvgs.length"+dHrAvgs.length);
			    	for (int i=0; i<quietDHrAvg.length; i++)
			    		System.out.print("quietdHrAvg "+quietDHrAvg[i]);
//		    	    	System.out.println("***hHrAvgs "+ CalcUtil.maxValue(hHrAvgs)+ " "+CalcUtil.minValue(hHrAvgs)+" "+hHrAvgs[0]+" "+hHrAvgs[10]);
//		    	    	System.out.println("***dHrAvgs "+ CalcUtil.maxValue(dHrAvgs)+ " "+CalcUtil.minValue(dHrAvgs)+" "+dHrAvgs[0]+" "+dHrAvgs[10]);
//		    	    	System.out.println("***quietHHrAvg "+ CalcUtil.maxValue(quietHHrAvg)+ " "+CalcUtil.minValue(quietHHrAvg)+" "+quietHHrAvg[0]+" "+quietHHrAvg[10]);
//		    	    	System.out.println("***quietdHrAvg "+ CalcUtil.maxValue(quietDHrAvg)+ " "+CalcUtil.minValue(quietDHrAvg)+" "+quietDHrAvg[0]+" "+quietDHrAvg[10]);
	    	    	
	    	    	// added from FMIQDCRT11_3hr.pro
	    	    	for ( int k = 0; k < quietHHrAvg.length; k++ ){
	    	    		if (quietHHrAvg[k] == MISSING_VAL || quietDHrAvg[k] == MISSING_VAL) {
	    	    			quietHHrAvg[k] = CalcUtil.getMedian(quietHHrAvg);
	    	    			quietDHrAvg[k] = CalcUtil.getMedian(quietDHrAvg);
	    	    		}
	    	    	}
    	    	
	    	    	float[] qha = CalcEach3hr.getQHA(quietHHrAvg);
	    	    	float[] qda = CalcEach3hr.getQHA(quietDHrAvg);
	//	    	    	System.out.println("***qha "+ CalcUtil.maxValue(qha)+ " "+CalcUtil.minValue(qha)+" "+qha[0]+" "+qha[10]);
	//	    	    	System.out.println("***qda "+ CalcUtil.maxValue(qda)+ " "+CalcUtil.minValue(qda)+" "+qda[0]+" "+qda[10]);
	    	    	
	    	    	float[] hQdc = CalcEach1min.getHarmonicFit(qha);//[1440]
			    	float[] dQdc = CalcEach1min.getHarmonicFit(qda);
	//			    	for (int i=0; i<hQdc.length; i++)
	//			    		System.out.print("hQdc "+hQdc[i]);
			    	System.out.println("**hQdc.length"+hQdc.length);
			    	for (int i=0; i<dQdc.length; i++)
			    		System.out.print("dQdc "+dQdc[i]);
	//			    	System.out.println("***hQdc "+ CalcUtil.maxValue(hQdc)+ " "+CalcUtil.minValue(hQdc)+" "+hQdc[0]+" "+hQdc[10]);
	//			    	System.out.println("***dQdc "+ CalcUtil.maxValue(dQdc)+ " "+CalcUtil.minValue(dQdc)+" "+dQdc[0]+" "+dQdc[10]);
			    	
			    	float[] qhaQdc = CalcEach1min.getQHAQDC(hQdc);//[1440]
			    	float[] qdaQdc = CalcEach1min.getQHAQDC(dQdc);
			    	
	//			    	System.out.println("");
	//			    	for (int i=0; i<qdaQdc.length; i++)
	//			    		System.out.print("qdaQdc "+qdaQdc[i]);
	//			    	System.out.println("***qhaQdc "+ CalcUtil.maxValue(qhaQdc)+ " "+CalcUtil.minValue(qhaQdc)+" "+qhaQdc[0]+" "+qhaQdc[10]+" "+station);
	//			    	System.out.println("***qdaQdc "+ CalcUtil.maxValue(qdaQdc)+ " "+CalcUtil.minValue(qdaQdc)+" "+qdaQdc[0]+" "+qdaQdc[10]);
			    	
	    			List<float[]> hdList = new ArrayList<float[]>();	
				    hdList.add(qhaQdc);
				    hdList.add(qdaQdc);
				    hdList.add(hQdc);
				    hdList.add(dQdc);
				    hdList.add(qha);
				    hdList.add(qda);
				    stationMap.put(stationCode, hdList);//		
	
				    //init again
				    Arrays.fill(hHrAvgs, MISSING_VAL);
				    Arrays.fill(dHrAvgs, MISSING_VAL);
				    
		    		for (Map.Entry<String, List<float[]>> entry : stationMap.entrySet())
		    			System.out.println("***themap "+ entry.getKey()+" "+entry.getValue().size()+" "+CalcUtil.maxValue(entry.getValue().get(0)) +" "+ CalcUtil.minValue(entry.getValue().get(0))+" "+CalcUtil.maxValue(entry.getValue().get(1)));
		    		
    			}
			}
		}

		return stationMap;
	}
	
	
	public Map<String, List<float[]>> calcBy1min( String[] dataURIs) throws StorageException {		
		//Map<String, List<float[]>> stationMap = new HashMap<String, List<float[]>>();
		Map<String, List<float[]>> kIndexMap = new HashMap<String, List<float[]>>();
		 
		float[] qhaQdc = new float[HOURS];
    	float[] qdaQdc = new float[HOURS];
    	float[] hQdc = new float[HOURS];
    	float[] dQdc = new float[HOURS];
    	float[] qha = new float[HOURS];
    	float[] qda = new float[HOURS];
		float[] hdata = new float[HD_DATA_RANGE*HOURS*MINUTES];
		float[] ddata = new float[HD_DATA_RANGE*HOURS*MINUTES];
	    
	    Arrays.fill(hdata, MISSING_VAL);
	    Arrays.fill(ddata, MISSING_VAL);
   
	    if (dao != null && dataURIs != null) {
			for (String dataURI : dataURIs ) {
    			String stationCode = CalcUtil.getStationFromUri(dataURI);
    			
    			Date timeBy1 = null;;
				try {
					timeBy1 = CalcUtil.getTimeFromUri(dataURI);
				} catch (ParseException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				int hour = timeBy1.getHours();
				int min = timeBy1.getMinutes();	
				Date epTime = CalcUtil.getEPTime(timeBy1);
				int epHour = epTime.getHours();
    			
    			List<?> dataList = retrieveUriForK1min(dataURI, epTime);

    			if (dataList != null && dataList.size() != 0) {
	    			// gest best observation data
	    			List<List> bestList = getBestObserv( dataList );
	    			System.out.println("**dataListBy1 size"+dataList.size()+" "+bestList.size()+" "+epTime);	    				
				
				
    			// get hdata, ddata
					for (int i = 0; i < bestList.size(); i++) {	
						List<Float> list = (List<Float>) bestList.get(i);
						if (list != null && !list.isEmpty()) {
							hdata[i] = list.get(1);
							ddata[i] = list.get(2);					
						}			
					}
					
					System.out.println("***hdata "+hdata.length+" "+hdata[0]+" "+hdata[1]+" "+hdata[2880]+" "+hdata[2879]);
					System.out.println("***ddata "+ddata.length+" "+ddata[0]+" "+ddata[1]+" "+ddata[2880]+" "+ddata[2879]);
					
	    			// calculate
		    		//System.out.println("******stationMap " + stationMap.size()+ " "+ stationMap.entrySet().size() );
				    for (Map.Entry<String, List<float[]>> entry : stationMap.entrySet()) {
				    	if (entry.getKey().equalsIgnoreCase(stationCode)) {
				    		List<float[]> list = entry.getValue();
				    		qhaQdc = list.get(0);
				    		qdaQdc = list.get(1);
				    		hQdc = list.get(2);
				    		dQdc = list.get(3);
				    		qha = list.get(4);
				    		qda = list.get(5);
		    			}
		    		}
					 

				    System.out.println("***hdata "+ CalcUtil.maxValue(hdata)+ " "+CalcUtil.minValue(hdata)+" "+hdata[0]+" "+hdata[2879]);
			    	System.out.println("***ddata "+ CalcUtil.maxValue(ddata)+ " "+CalcUtil.minValue(ddata)+" "+ddata[0]+" "+ddata[2879]);
			    	defLength = CalcEach3hr.getDefLength(stationCode, epHour);
			    	
			    	float[] hhdata = CalcEach1min.fillGaps(hdata);
			    	float[] dddata = CalcEach1min.fillGaps(ddata);
			    	
			    	System.out.println("***hhdataGaps "+ CalcUtil.maxValue(hhdata)+ " "+CalcUtil.minValue(hhdata)+" "+hhdata[0]+" "+hhdata[10]);
			    	System.out.println("***dddataGaps "+ CalcUtil.maxValue(dddata)+ " "+CalcUtil.minValue(dddata)+" "+dddata[0]+" "+dddata[10]);		    	
			    	System.out.println("***qhaQdc "+ CalcUtil.maxValue(qhaQdc)+ " "+CalcUtil.minValue(qhaQdc)+" "+qhaQdc[0]+" "+qhaQdc[10]);
			    	System.out.println("***qdaQdc "+ CalcUtil.maxValue(qdaQdc)+ " "+CalcUtil.minValue(qdaQdc)+" "+qdaQdc[0]+" "+qdaQdc[10]);
			    	System.out.println("***hQdc "+ CalcUtil.maxValue(hQdc)+ " "+CalcUtil.minValue(hQdc)+" "+hQdc[0]+" "+hQdc[10]);
			    	System.out.println("***dQdc "+ CalcUtil.maxValue(dQdc)+ " "+CalcUtil.minValue(dQdc)+" "+dQdc[0]+" "+dQdc[10]);
			    	
	//		    	for (int i=0; i<qhaQdc.length; i++)
	//		    		System.out.print("qhaQdc "+qhaQdc[i]);
			    	System.out.println("**qhaQdc.length"+qhaQdc.length);
	//		    	for (int i=0; i<qdaQdc.length; i++)
	//		    		System.out.print("qdaQdc "+qdaQdc[i]);
			    	
			    	int currTimeIndex = CalcEach1min.getCurrTimeIndex(hour, min, epHour);
			    	System.out.println("***index "+currTimeIndex+ " "+hour+" "+epHour+" "+min);//00:01
			    	
			    	hhdata = CalcEach1min.getExtrapolation(hhdata, qhaQdc, currTimeIndex);
			    	dddata = CalcEach1min.getExtrapolation(dddata, qdaQdc, currTimeIndex);
	//		    	for (int i=2880-180; i<2800; i++)
	//		    		System.out.print("hhdate "+i +" "+hhdata[i]);
	//		    	for (int i=2880-180; i<2800; i++)
	//		    		System.out.print("dddata "+i +" "+dddata[i]);
			    	System.out.println("dddata "+dddata.length);
			    	
	//		    	System.out.println("***hhdataExtr "+ CalcUtil.maxValue(hhdata)+ " "+CalcUtil.minValue(hhdata)+" "+hhdata[0]+" "+hhdata[10]);
	//		    	System.out.println("***dddataExtr "+ CalcUtil.maxValue(dddata)+ " "+CalcUtil.minValue(dddata)+" "+dddata[0]+" "+dddata[10]);
	//		    	int l=0;
	//		    	for (l = hhdata.length-1; l >=0; l--) 
	//			    	if (hhdata[l] != MISSING_VAL && dddata[l] != MISSING_VAL)
	//			    		break;
	//			    System.out.println("***lll "+l);
			    	
				    
			    	float[] hDev = CalcEach1min.getDev(hhdata, hQdc);//[1440]
			    	float[] dDev = CalcEach1min.getDev(dddata, dQdc);
	//		    	for (int i=0; i<hDev.length; i++)
	//		    		System.out.print("hDev "+hDev[i]);
			    	for (int i=0; i<dDev.length; i++)
			    		System.out.print("dDev "+dDev[i]);
			    	System.out.println("**dDev.length"+dDev.length);
			    	
			    	System.out.println("***hDev "+ CalcUtil.maxValue(hDev)+ " "+CalcUtil.minValue(hDev)+" "+hDev[0]+" "+hDev[10]);
			    	System.out.println("***dDev "+ CalcUtil.maxValue(dDev)+ " "+CalcUtil.minValue(dDev)+" "+dDev[0]+" "+dDev[10]);
			    	//already considered missing in getDev
			    	
			    	int[] kLimit = CalcUtil.getKLimit(stationCode); 
			    	
			    	int missingFlag = 0;
			    	List<float[]> kList = CalcEach1min.getKIndex(hDev, dDev, kLimit, missingFlag);//[8]
	//		    	System.out.println("***kList "+kList.size());
			    	float[] kIndex =  kList.get(0);
			    	float[] gamma =  kList.get(1);
			    	
			    	float[] kLength = CalcUtil.geKLength();//[8]
			    	float[] fitLength = CalcEach1min.getFitLength(defLength, kIndex, kLength);//[24]
			    	
			    	
			    	float[] hcA = CalcEach1min.getCentHourAvg(hhdata, fitLength, kIndex);//middle [24]
			    	float[] dcA = CalcEach1min.getCentHourAvg(dddata, fitLength, kIndex);
			    	System.out.println("***hcA "+ CalcUtil.maxValue(hcA)+ " "+CalcUtil.minValue(hcA)+" "+hcA[0]+" "+hcA[10]+" "+stationCode);
			    	System.out.println("***dcA "+ CalcUtil.maxValue(dcA)+ " "+CalcUtil.minValue(dcA)+" "+dcA[0]+" "+dcA[10]);
			    	  
			    	hcA = CalcEach1min.adjustHrCentAvg(hcA, qha, gamma, kLimit);
			    	dcA = CalcEach1min.adjustHrCentAvg(dcA, qda, gamma, kLimit);
			    	System.out.println("***hcAAdj "+ CalcUtil.maxValue(hcA)+ " "+CalcUtil.minValue(hcA)+" "+hcA[0]+" "+hcA[10]);
			    	System.out.println("***dcAAdj "+ CalcUtil.maxValue(dcA)+ " "+CalcUtil.minValue(dcA)+" "+dcA[0]+" "+dcA[10]);
			    	
			    	// Harmonic Fit to derive the qdc
			    	for (int i=0; i<hcA.length; i++)
			    		if (hcA[i] == MISSING_VAL) {
			    			System.out.println("**MIssing1");
			    			hQdc = hQdc;
			    			break; 
			    		}
			    		else 
			    			hQdc = CalcEach1min.getHarmonicFit(hcA);
	
			    	for (int i=0; i<hcA.length; i++)
			    		if (hcA[i] == MISSING_VAL) {
			    			System.out.println("**MIssing2");
			    			dQdc = dQdc;
			    			break; 
			    		}
			    		else 
			    			dQdc = CalcEach1min.getHarmonicFit(dcA);
			    	
	//		    	System.out.println("***hQdc2 "+ CalcUtil.maxValue(hQdc)+ " "+CalcUtil.minValue(hQdc)+" "+hQdc[0]+" "+hQdc[10]);
	//		    	System.out.println("***dQdc2 "+ CalcUtil.maxValue(dQdc)+ " "+CalcUtil.minValue(dQdc)+" "+dQdc[0]+" "+dQdc[10]);
			    	
			    	// Do a few iterations. check for convergence of k_index and exit loop 
			    	// Done before ITERATIONS if you see two passes with identical values for k_index
			    	
			    	float[] last_kindex = new float[8];
			    	Arrays.fill(last_kindex, -1);
			    	
			    	/* Check for convergence of k_index and exit loop 
			    	 * before ITERATIONS are done if you see two passes with
			    	 * identical values for k_index
			    	 */
			    	for (int num = 0; num < ITERATIONS; num++) {
			    		float kchange = 0;
			    		hDev = CalcEach1min.getDev(hhdata, hQdc);
			        	dDev = CalcEach1min.getDev(dddata, dQdc);
	//		        	System.out.println("***hhdataLoop "+ CalcUtil.maxValue(hhdata)+ " "+CalcUtil.minValue(hhdata)+" "+hhdata[0]+" "+hhdata[10]);
	//			    	System.out.println("***dddataLoop "+ CalcUtil.maxValue(dddata)+ " "+CalcUtil.minValue(dddata)+" "+dddata[0]+" "+dddata[10]);		    	
	//			    	System.out.println("***hDevLoop "+ CalcUtil.maxValue(hDev)+ " "+CalcUtil.minValue(hDev)+" "+hDev[0]+" "+hDev[10]);
	//		        	System.out.println("***dDevLoop "+ CalcUtil.maxValue(dDev)+ " "+CalcUtil.minValue(dDev)+" "+dDev[0]+" "+dDev[10]);
			        		
			        	kList = CalcEach1min.getKIndex(hDev, dDev, kLimit, missingFlag);
			        	kIndex =  kList.get(0);
			        	gamma =  kList.get(1);
			        	 
			        	// Check for convergence of k_index
			        	if (kIndex.length == 8 && last_kindex.length == 8)
				        	for (int i=0; i<last_kindex.length; i++){
				        		kchange += Math.abs(kIndex[i] - last_kindex[i]);
				        		//System.out.println("**kchange "+ kIndex[i] +" " +last_kindex[i]+" "+(kIndex[i] -last_kindex[i]));
				        	}
			        	if (kchange == 0) 
				        	break;
	
			        	fitLength = CalcEach1min.getFitLength(defLength, kIndex, kLength);
			        	//System.out.println("***fitLength2 "+ fitLength.length+ " "+fitLength[6]+ " "+CalcUtil.minValue(fitLength));
			        	
			        	hcA = CalcEach1min.getCentHourAvg(hhdata, fitLength, kIndex);
			        	dcA = CalcEach1min.getCentHourAvg(dddata, fitLength, kIndex);
			        	System.out.println("***hcALoop "+ CalcUtil.maxValue(hcA)+ " "+CalcUtil.minValue(hcA)+" "+hcA[0]+" "+hcA[10]);
			        	System.out.println("***dcALoop "+ CalcUtil.maxValue(dcA)+ " "+CalcUtil.minValue(dcA)+" "+dcA[0]+" "+dcA[10]);
			        	
			        	hcA = CalcEach1min.adjustHrCentAvg(hcA, qha, gamma, kLimit);
			        	dcA = CalcEach1min.adjustHrCentAvg(dcA, qda, gamma, kLimit);
			        	
	//		        	hQdc = CalcEach1min.getHarmonicFit(hcA);//[1440]
	//		        	dQdc = CalcEach1min.getHarmonicFit(dcA);
			        	
			        	// Harmonic Fit to derive the qdc
				    	for (int i=0; i<hcA.length; i++)
				    		if (hcA[i] == MISSING_VAL) {
				    			System.out.println("**MIssing1");
				    			hQdc = hQdc;
				    			break; 
				    		}
				    		else 
				    			hQdc = CalcEach1min.getHarmonicFit(hcA);
	
				    	for (int i=0; i<hcA.length; i++)
				    		if (hcA[i] == MISSING_VAL) {
				    			System.out.println("**MIssing2");
				    			dQdc = dQdc;
				    			break; 
				    		}
				    		else 
				    			dQdc = CalcEach1min.getHarmonicFit(dcA);
				    	
				    	System.out.println("***hQdcLoop3 "+ CalcUtil.maxValue(hQdc)+ " "+CalcUtil.minValue(hQdc)+" "+hQdc[0]+" "+hQdc[10]);
				    	System.out.println("***dQdcLoop3 "+ CalcUtil.maxValue(dQdc)+ " "+CalcUtil.minValue(dQdc)+" "+dQdc[0]+" "+dQdc[10]);
				    	
				    	last_kindex = kIndex.clone();
			    	}
			    	
			    	// Now do the calculation using the original data (hdata, ddata)
			    	hDev = CalcEach1min.getDev(hdata, hQdc);//[1440]
			    	dDev = CalcEach1min.getDev(ddata, dQdc);
			    	System.out.println("***hDevLast "+ CalcUtil.maxValue(hDev)+ " "+CalcUtil.minValue(hDev)+" "+hDev[0]+" "+hDev[10]);
		        	System.out.println("***dDevLast "+ CalcUtil.maxValue(dDev)+ " "+CalcUtil.minValue(dDev)+" "+dDev[0]+" "+dDev[10]);
		        	
			    	kList = CalcEach1min.getKIndex(hDev, dDev, kLimit, missingFlag);		    	
			    	kIndex =  kList.get(0);
			    	gamma =  kList.get(1);
			    	float[] hkIndex =  kList.get(2);
			    	float[] hGamma =  kList.get(3);
			    	float[] dkIndex =  kList.get(4);
			    	float[] dGamma =  kList.get(5);
			    	int lastHCount = 0;
			    	int lastDCount = 0;
			    	
			    	for (int i = 2700; i < 2880; i++) {
			    		if (hdata[i] != MISSING_VAL)
			    			lastHCount++;
			    		if (hdata[i] != MISSING_VAL)
			    			lastDCount++;
			    	}
			    	
			    	float[] count = new float[2];
			    	count[0] = lastHCount;
			    	count[1] = lastDCount;
			    	kList.add(6, count);
			    	kIndexMap.put(stationCode, kList);
			    	System.out.println("**count "+lastHCount+" "+lastDCount);
			    	
			    	float[] kest = CalcKp.getKest(stationCode, kList.get(0), kList.get(1));
	//		    	for(int i=0;i<8;i++)
	//		    		System.out.println("**kest "+kest[i]);
			    	
			    	float ks = 0;
			    	try {
						ks = CalcKp.getKs(stationCode, (int) kIndex[7], timeBy1); // 7 is last point kIndex					
					} catch (ParseException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
	
			    	int a_est = CalcKp.getAest(stationCode, (int) kIndex[7]);
			    	System.out.println("****ks a "+ ks+" "+a_est);
			    	
					
					// 1 min
					//int id =           (Integer)  bestList.get(bestList.size()-1).get(0);
					Date timeTag =     (Date)  bestList.get(bestList.size()-1).get(3);
					int kest_index =   (int) kIndex[7];
					float kest_real =  CalcKp.getKest(stationCode, (int) kIndex[7], gamma[7]);					
					float kest_gamma = gamma[7];					
					float hk_real =    CalcKp.getKest(stationCode, (int) hkIndex[7], hGamma[7]);
					float hgamma =     hGamma[7];
					float dk_real =    CalcKp.getKest(stationCode, (int) dkIndex[7], dGamma[7]);
					float dgamma =     dGamma[7];	
					int hkindex =      (int) hkIndex[7];
					int dkindex =      (int) dkIndex[7];
					float ksArray = ks;	
					int aestArray = a_est;
	//				int[] kest_index = new int[HOURS/3];
	//				float[] kest_real = new float[HOURS/3];					
	//				float[] kest_gamma = new float[HOURS/3];					
	//				float[] hk_real = new float[HOURS/3];
	//				float[] hgamma = new float[HOURS/3];
	//				float[] dk_real = new float[HOURS/3];
	//				float[] dgamma = new float[HOURS/3];
					
		    		
		    		//String newUri = dataURI.substring(0, 34)+ "100/GEOMAG";
//					System.out.println("**recK1min store "+id+" "+timeTag +" "+timeBy1+" "+kest_index+" "+ks);
					
	    			GeoMagK1min recK1min = new GeoMagK1min();   			
//	    			recK1min.setId(id);
	    			recK1min.setRefTime(timeBy1);
	    			recK1min.setLastUpdate(Calendar.getInstance().getTime());
	    			recK1min.setStationCode(stationCode);
	    			recK1min.setKestIndex(kest_index);
	    			recK1min.setKestReal(kest_real);
	    			recK1min.setKestGamma(kest_gamma);
	    			recK1min.setHKIndex(hkindex);
	    			recK1min.setHKReal(hk_real);
	    			recK1min.setHKGamma(hgamma);
	    			recK1min.setDKIndex(dkindex);
	    			recK1min.setDKReal(dk_real);
	    			recK1min.setDKGamma(dgamma);
	    			recK1min.setKs(ksArray);
	    			recK1min.setAest(aestArray);
	    			    			
					GeoMagK1minDao k1minDao = new GeoMagK1minDao();
					k1minDao.persist(recK1min);
		
					long t3 = Calendar.getInstance().getTimeInMillis();
	    	    	System.out.println("*****tt3 "+ t3);
    	    	
				} // end of for dataURI
		    }
	    }
    	
	    return kIndexMap;   	
	}
		
	public void calcK3h(String[] dataURIs, Map<String, List<float[]>> kIndexMap){
		if (dao != null && dataURIs != null) {
			for (String dataURI : dataURIs ) {
    			String stationCode = CalcUtil.getStationFromUri(dataURI);
    			
    			Date time = null;;
				try {
					time = CalcUtil.getTimeFromUri(dataURI);
				} catch (ParseException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				int hour = time.getHours();
				int min = time.getMinutes();	
				int total = hour*MINUTES +min;
				
				Integer[] synopticPoint = {59, 119, 179, 359, 539, 719, 899, 1079, 1259, 1439};
				System.out.println("**stationMap "+stationMap.entrySet().isEmpty() +" "+stationMap.entrySet().size() +" "+Arrays.asList(synopticPoint).contains(total));
				
    			List<?> dataList = null;
    			if (stationMap.entrySet().isEmpty() || Arrays.asList(synopticPoint).contains(total))
    				dataList = retrieveUriForK3hr(dataURI, time);
    			else
    				continue;

    			System.out.println("**resultsListfor3.size() "+dataList.size());	

    			List<List> bestList = getBestObserv( dataList );
    			
    			int kindexDb = 0;
				float krealDb = 0;
				float kgammaDb = 0;		
    			int k_index = 0;
				float k_real = 0;
				float k_gamma = 0;
			      		    		
    			if (dataList != null ) { 
    				for (int i = 0; i < dataList.size(); i++) {  //1 extra
    					
    					GeoMagK3hr row = (GeoMagK3hr) dataList.get(i);
    					
    					Date date = (Date) row.getRefTime();
    					kindexDb = (Integer) row.getKIndex();
    					krealDb = (Float) row.getKReal();
    					kgammaDb = (Float) row.getKGamma();				
	    				//System.out.println("***row "+dateList.get(i)+" "+hHrAvgList.get(i)+" " +dHrAvgList.get(i));
    				}
    			}

    			List<float[]> list = null;
    			for (Map.Entry<String, List<float[]>> entry : kIndexMap.entrySet()) {
    				System.out.println("**kindex list "+entry);   			
			    	if (entry.getKey().equalsIgnoreCase(stationCode)) 
			    		list = entry.getValue();
//			    		qhaQdc = list.get(0);
//			    		qdaQdc = list.get(1);
	    		}
    			
		    	if (kindexDb == MISSING_VAL || kindexDb == 0)
		    		k_index = (int) list.get(0)[7];
		    	if (krealDb == MISSING_VAL || krealDb == 0)
		    		k_real = CalcKp.getKest(stationCode, (int) list.get(0)[7], list.get(1)[7]); //[7], gamma[7]);
	        	if (kgammaDb == MISSING_VAL || kgammaDb == 0)
		    		kgammaDb = list.get(1)[7];
		    	
    	    	GeoMagK3hr recK3hr = new GeoMagK3hr();   			
    			//recK3hr.setId((int) l.get(0));
    			recK3hr.setRefTime(time);
    			recK3hr.setLastUpdate(Calendar.getInstance().getTime());
    			recK3hr.setStationCode(stationCode);
    			recK3hr.setKIndex(k_index);
    			recK3hr.setKReal(k_real);
    			recK3hr.setKGamma(k_gamma);
    			GeoMagK3hrDao k3hrDao = new GeoMagK3hrDao();
				k3hrDao.persist(recK3hr);

//	    		record.setKestIndex(kest_index);
//	    		record.setKestGamma(kest_gamma);
//	    		record.setKestReal(kest_real);
//	    		record.setHKReal(hk_real);
//	    		record.setHKGamma(hgamma);
//	    		record.setDKReal(dk_real);
//	    		record.setDKGamma(dgamma);
			}
		}
	}

}
