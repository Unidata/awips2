/**
 * 
 * gov.noaa.nws.ncep.ui.nctextui.dbutil.NctextDbQuery
 * 
 * This java class performs the NCTEXT GUI database query.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * 
 * Database : NCEP
 * Schema:  NWX
 * Tables:
 * 
 * 1.Data Type Group List table
 * name :  datatypegrouplist
 * column 1: id, int
 * column 2: datatypegroupname, string
 * column 3: datatypegrouptablename, string
 * reference: dataTypeGpList.xml – use contents of this xml  file for table contents
 * 
 * 2.Data Group Tables:  One table for each entry of datatypegrouplist table.
 * name:   table name should match with column 3 of  datatypegrouplist table.
 * column 1:  id, int
 * column 2: productname, string
 * column 3: producttablename, string
 * column 4: productType, String – use file extension as defined in $GEMTBL/nwx/master.tbl.
 * reference: ObservedData.xml, etc.-  use contents of these xml files for tables contents
 * 
 * 3.Product Tables:   one for each entry in Data Group Table
 * name:   table name should match with column 3 of Data Group Table
 * column 1:  id, int
 * column 2: productid, string
 * column 3: stnid, string
 * column 4: stnname, string
 * column 5: state, string
 * column 6: country, string
 * column 7: latitude, double
 * column 8: longitude, double
 * column 9: elevation, int
 * reference: use contents of $GEMTBL/nwx/*.bull files for tables contents. Need to be carefully to match bull file for each product station table
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 1/10/2010		TBD		Chin Chen	Initial coding
 * 1/26/2010				Chin Chen	Make changes to use NCEP database for station info sources
 * 										instead of using XML files. 
 * 12/10/2012               Chin Chen   Add support for "Observed Data" group
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nctextui.dbutil;

import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.PointDataRequest;


public class NctextDbQuery {

	private final String NCTEXT_DATA_DB_NAME = "metadata";
	private final String NCTEXT_STATIC_DB_NAME = "ncep";
	private final String NCTEXT_STATIC_GP_TABLE_NAME = "nwx.datatypegrouplist";	
	private final String NCTEXT_DATA_DB_TABLE = "awips.nctext";
	private static final String OBS_DATA_GROUP = "Observed Data";
	private static final String OBS_SFC_HRLY_TABLE = "obs";
	private static final String OBS_SFC_HRLY_RAW_DATA_KEY = "rawMETAR";
	private static final String OBS_SFC_HRLY_TIME_KEY = "timeObs";
	private static final String OBS_SFC_HRLY_PRODUCT_NAME = "Surface Hourlies";
	private static final String OBS_SND_DATA_TABLE = "ncuair";
	private static final String OBS_SND_DATA_RAW_DATA_KEY = "RAWDATA";
	private static final String OBS_SND_DATA_TIME_KEY = "OBSTIME";
	private static final String OBS_SND_DATA_COR_KEY = "CORR";
	private static final String OBS_SND_DATA_NIL_KEY = "NIL";
	private static final String OBS_SND_DATA_STATIONID_KEY = "STATIONID";
	private static final String OBS_SND_DATA_STNUM_KEY = "STNUM";
	private static final String OBS_SND_DATA_TYPE_KEY = "DATATYPE";
	private static final String OBS_SND_DATA_PRODUCT_NAME = "Sounding Data";
	private static final String OBS_SYN_DATA_TABLE = "sfcobs";
	private static final String OBS_SYN_DATA_RAW_DATA_KEY = "rawReport";
	private static final String OBS_SYN_DATA_TIME_KEY = "timeNominal";
	private static final String OBS_SYN_DATA_PRODUCT_NAME = "Synoptic Data";
	/*private static final String OBS_TAF_DATA_TABLE = "nctaf";
	private static final String OBS_TAF_DATA_RAW_DATA_KEY = "TAFTEXT";
	private static final String OBS_TAF_DATA_TIME_KEY = "START_DATE";
	private static final String OBS_TAF_DATA_PRODUCT_NAME = "TAFs Decoded";*/
	//private final String NCTEXT_STATION_GP_TABLE_NAME = "awips.nctext_stn_group";
	private final String  NCTEXT_FILE_TYPE_TABLE_NAME = "awips.nctext_inputfile_type";
	//List of data type group
	private static List<String> dataTypeGpStrList = new ArrayList<String>();
	
	
	//Map from data group name to a list of data products for the group.
	private static Map<String, List<String>> gpToProductlistMap = new HashMap<String, List<String>>();//<key: dataTypeGpName, value: productList array>
	
	// Map, from a data productName to all of its stationInfo list, key:productname, value: all of its stn info list
	private static Map<String, List<NctextStationInfo>> productAllStationInfoListMap = new HashMap<String, List<NctextStationInfo>>(); //<key:productName, value:List<NctextStationInfo>>

	// Map, from a data productName to its stationInfo list of a state, key:productname+state, value:its state stn info list
	private static Map<String, List<NctextStationInfo>> productStateStationInfoListMap = new HashMap<String, List<NctextStationInfo>>(); //<key:productName, value:List<NctextStationInfo>>

	private static Map<String, String> productNameToTypeMap = new HashMap<String, String>(); //<key:productName, value: productType>
	private static Map<String, String> fileExtToFileTypeMap = new HashMap<String, String>(); //<key:fileExt, value: file Type>
	
	private boolean autoUpdate;
	

	//read file type info from database
	private List<Object[]> readfileTypeInfoList() {
		//System.out.println("accessed readfileTypeInfoLis"); 
		String queryStr = new String("Select * FROM "+NCTEXT_FILE_TYPE_TABLE_NAME);
		List<Object[]> list = null;
		try {
			list = NcDirectDbQuery.executeQuery( queryStr, NCTEXT_DATA_DB_NAME, QueryLanguage.SQL);
		}
		catch (Exception e ){
			System.out.println("-----DB exception at readfileTypeInfoList: "+e.getMessage());			
		}

 		return list;
		
	}

	//read data type group info from database
	private List<Object[]> readDataTypeGpInfoList() {
         
		String queryStr = new String("Select * FROM "+NCTEXT_STATIC_GP_TABLE_NAME);
		List<Object[]> list = null;
		try {
			list = NcDirectDbQuery.executeQuery( queryStr, NCTEXT_STATIC_DB_NAME, QueryLanguage.SQL);
		}
		catch (Exception e ){
			System.out.println("-----DB exception at readDataTypeGpInfoList: "+e.getMessage());			
		}

 		return list;
		
	}
	//get list Of Stn info from db table for one product
	private List<Object[]> readStnInfo(String productTblName) {
		String queryStr = new String("Select * FROM "+productTblName);
		List<Object[]> stninfolst = null;
		try {
			stninfolst = NcDirectDbQuery.executeQuery( queryStr, NCTEXT_STATIC_DB_NAME, QueryLanguage.SQL);
		}
		catch (Exception e ){
			System.out.println("-----DB exception at readStnInfo: "+e.getMessage());			
		}

		return stninfolst;
	}

	// add item to productAllStationInfoListMap, key=productname, val= all stn list of such product
	private void addToProductAllStationInfoListMap(String productName, List<Object[]> stnInfoLstObj) {
		List<NctextStationInfo> listOfStn = new ArrayList<NctextStationInfo>();
		//System.out.println("Product is adding "+ productName);
		for(Object[] obj: stnInfoLstObj){
			NctextStationInfo stn = new NctextStationInfo();
			stn.setProductid((String)obj[1]);
			stn.setStnid((String)obj[2]);
			stn.setStnname((String)obj[3]);
			stn.setState((String)obj[4]);
			stn.setCountry((String)obj[5]);
			stn.setLatitude((Double)obj[6]);
			stn.setLongitude((Double)obj[7]);
			stn.setElevation((Integer)obj[8]);
			listOfStn.add(stn);
			
		}
		productAllStationInfoListMap.put(productName, listOfStn);
		
	}
	// add item to productStateStationInfoListMap, key=productname+state, val= state stn list of such product
	private void addToProductStateStationInfoListMap (String productName, List<Object[]> stnInfoLstObj) {
		List<NctextStationInfo> listOfAllStn = productAllStationInfoListMap.get(productName);
		 
		Collection<String> setOfState = new HashSet<String>(); 
		
		String state = null;
		
		//create a set of state found from all stations list. Set will only add non-duplicate element in
		for (NctextStationInfo stn : listOfAllStn){
			state = stn.getState(); 
			setOfState.add(state);
			
		}
		//For each state: create state Station list from all stations list and add to map
		List<NctextStationInfo> listOfStateStn;
		
		for (String sta : setOfState){		
			listOfStateStn= new ArrayList<NctextStationInfo>();
			for (NctextStationInfo stn : listOfAllStn){
				if( stn.getState().compareTo(sta) == 0) 
					listOfStateStn.add(stn);				
			}
			productStateStationInfoListMap.put((productName+sta), listOfStateStn);
		}
	}
	
	private List<Object[]> readGpProductInfo(String gpTableName){
		String queryStr = new String("Select * FROM "+gpTableName);
		List<Object[]> gpProductObjList = null;
		try {
			gpProductObjList = NcDirectDbQuery.executeQuery( queryStr, NCTEXT_STATIC_DB_NAME, QueryLanguage.SQL);
		}
		catch (Exception e ){
			System.out.println("-----DB exception at readGpProductInfo: "+e.getMessage());			
		}


		return gpProductObjList;
		
	}
	private void initTables(){
		//Note: see this file header for more DB table information
		List<Object[]> fileTypeObjList = readfileTypeInfoList();
		if(fileTypeObjList != null) {
			for(Object[] fileTypeObjArray : fileTypeObjList){
				String fileExt = (String)fileTypeObjArray[1];
				String fileType = (String)fileTypeObjArray[2];
				fileExtToFileTypeMap.put(fileExt, fileType);
			}
		}
		
		
		//get product gp list
		List<Object[]>/*DataTypeGpInfoListX*/ dataTypeGpObjList = readDataTypeGpInfoList();
		if (dataTypeGpObjList!= null){
			//get gp product list for each gp
			for(Object[] gpObjArray : dataTypeGpObjList){
				String gpName = (String)gpObjArray[1];
				String gpTbl = (String)gpObjArray[2];
				//System.out.println("id = "+gpObjArray[0]+ " gp name = "+gpName+ " gp tbl name = "+gpTbl);
			
				List<Object[]> gpProObjList = readGpProductInfo(gpTbl);
				if (gpProObjList != null){
					List<String> productLst = new ArrayList<String>();
					for (Object[] proObjArray :gpProObjList){
						String productName = (String)proObjArray[1];
						String productTbl =  (String)proObjArray[2];
						String productType =  (String)proObjArray[3];//input file extension used as product type
						//System.out.println("id = "+proObjArray[0]+ " product name = "+productName+ " pro tbl name = "+productTbl);
						//add each product name to list
						productLst.add(productName);
						
						//read station info for each product
						List<Object[]> stnInfoObjList = readStnInfo(productTbl);
						if(stnInfoObjList!= null){
							//add to ProductAllStationInfoListMap
							addToProductAllStationInfoListMap(productName,stnInfoObjList);
							
							//add to productStateStationInfoListMap
							addToProductStateStationInfoListMap(productName,stnInfoObjList);
							
							productNameToTypeMap.put(productName,productType);
						}
						
					}
					

					//add each gp product list to gpToProductlistMap
					gpToProductlistMap.put(gpName, productLst);

					//also add each gp name to gp List
					dataTypeGpStrList.add(gpName);
				}
			}
		}
	}
	/*
	 * A generic SQL example is constructed like the following.
	 * "Select rawrecord, issuesite FROM awips.nctext WHERE issuesite='KNES' AND producttype='satest' AND issuetime>='2010-3-7 15:39:29' ORDER BY issuetime DESC"
	 * A warning type SQL example is constructed like the following.
	 * "Select rawrecord, issuesite FROM awips.nctext WHERE issuesite='KVCT' AND producttype='SVR' AND issuetime>='2010-3-7 15:45:51' ORDER BY issuetime DESC"
	 * A R type file SQL example is constructed like the following
	 * "Select rawrecord, issuesite FROM awips.nctext WHERE rawrecord LIKE '%RDU%' AND producttype='etagd' ORDER BY issuetime DESC"
	 */
	private String createProductDataQuery(String productName,NctextStationInfo sta,
			EReportTimeRange rptTimeRange){
		StringBuilder queryStr  = null;
		String productType;
		String fileType;
		if((productName!= null)&&(sta!= null)){
			productType = productNameToTypeMap.get(productName);  //product type is ingested text file extension 
			fileType = fileExtToFileTypeMap.get(productType);
			if((fileType != null) && (fileType.equals("R"))){
				//special case to handle "R" type GUIDENCE files, This type of text file, its record is saved with a gp stn id created by NCTEXT decorder
				//gpStn = stnToGpStnMap.get(sta.getStnid());
				//queryStr = new StringBuilder("Select rawrecord FROM "+NCTEXT_DATA_DB_TABLE+" WHERE issuesite='"+gpStn+"' AND producttype='"+productType+"'");
				queryStr = new StringBuilder("Select rawrecord, issuesite FROM "+NCTEXT_DATA_DB_TABLE+" WHERE rawrecord LIKE '%"+sta.getStnid()+"%' AND producttype='"+productType+"'");
			} else 	if((fileType != null) && (fileType.equals("RFTS"))){
				//special case to handle "R" type FTS files, This type of text file, its record is saved with stn id(s) embedded in record
				// plus some stn may be the main issue stn. So, if just search stn id will retunrs too many reports.
				//so, we will search "stnid xxxxxxZ" pattern. 
				queryStr = new StringBuilder("Select rawrecord, issuesite FROM "+NCTEXT_DATA_DB_TABLE+
						" WHERE rawrecord LIKE '%"+sta.getStnid()+"_______Z%' AND producttype='"+productType+"'");
			} else 	if((fileType != null) && (fileType.equals("WRECON"))){
				//special case to handle "W" type RECON files, This type of text file, its record should be 
				// selectd based on WMOID, as issue site are duplicated for many products 
				queryStr = new StringBuilder("Select rawrecord, issuesite FROM "+NCTEXT_DATA_DB_TABLE+" WHERE wmoid='"+sta.getProductid()+
						"' AND issuesite='"+sta.getStnid()+"' AND producttype='"+productType+"'");
			} else {
				queryStr = new StringBuilder("Select rawrecord, issuesite FROM "+NCTEXT_DATA_DB_TABLE+" WHERE issuesite='"+sta.getStnid()+"' AND producttype='"+productType+"'");
			}
			
		} else if(productName!= null){
			productType = productNameToTypeMap.get(productName);
			queryStr = new StringBuilder("Select rawrecord, issuesite FROM "+NCTEXT_DATA_DB_TABLE+" WHERE producttype='"+productType+"'");
		} else if(sta!= null){
			queryStr = new StringBuilder("Select rawrecord, issuesite FROM "+NCTEXT_DATA_DB_TABLE+" WHERE wmoid='" + sta.getProductid()+
					"' AND issuesite='"+sta.getStnid()+"'");
			
		}
		
		
		if(rptTimeRange.getTimeRange()>0) {
			
			Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
			cal.add(Calendar.HOUR_OF_DAY, -(rptTimeRange.getTimeRange()));
			String timeE = cal.get(Calendar.YEAR)+"-"+(cal.get(Calendar.MONTH)+1)+"-"+cal.get(Calendar.DAY_OF_MONTH)+" "+
					cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+cal.get(Calendar.SECOND);
			//System.out.println("Time after adjust "+ timeE);

			queryStr.append(" AND issuetime>='"+timeE+"'");
		}
		
		queryStr.append(" ORDER BY issuetime DESC");
		//System.out.println(queryStr.toString());		
		return queryStr.toString();
	}
	//singleton object
	private NctextDbQuery() {
		//System.out.println("NctextDbQuery constructed");
		initTables();
	}
	
	//create this singleton object
	private static NctextDbQuery nctextDbQuery = null; 

	public static NctextDbQuery getAccess() {
		if(nctextDbQuery == null){
			nctextDbQuery = new NctextDbQuery();
		}
		return nctextDbQuery;
	}
	public List<String> getDataTypeGpList() {
		return dataTypeGpStrList;
	}
	public List<String> getGpProductList(String dataTypeGpName ) {
		if(dataTypeGpName==null)
			return null;
		return (gpToProductlistMap.get(dataTypeGpName));
	}
	public List<NctextStationInfo> getProductStaList(String productName, EReportTimeRange timeCovered) {
		List<NctextStationInfo> staList= productAllStationInfoListMap.get(productName);
		
		String productType = productNameToTypeMap.get(productName);  //product type is ingested text file extension 
		String fileType = fileExtToFileTypeMap.get(productType);
		//System.out.println("product type "+ productType+ " file type "+ fileType );
		if((fileType != null) && (fileType.equals("W")||(fileType.equals("WRECON"))) ){
			//For Warning type file.
			//filter out stn which does not have report in DB at this time
			//to do this, we have to query DB first.
			StringBuilder queryStr  = null;
			List<Object[]> list = null;
			List<NctextStationInfo> staListFiltered = new ArrayList<NctextStationInfo> ();
			if(fileType.equals("WRECON")){
				//W type RECON file should use wmoid to get station
				queryStr = new StringBuilder("Select wmoid, issuesite FROM "+NCTEXT_DATA_DB_TABLE+" WHERE producttype='"+productType+"'");				
			} else {
				queryStr = new StringBuilder("Select issuesite FROM "+NCTEXT_DATA_DB_TABLE+" WHERE producttype='"+productType+"'");
			}
			//adjust time....
			if(timeCovered.getTimeRange() > 0){
				Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
				cal.add(Calendar.HOUR_OF_DAY, -(timeCovered.getTimeRange()));
				String timeE = cal.get(Calendar.YEAR)+"-"+(cal.get(Calendar.MONTH)+1)+"-"+cal.get(Calendar.DAY_OF_MONTH)+" "+
				cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+cal.get(Calendar.SECOND);
				//System.out.println("Time after adjust "+ timeE);
				queryStr.append(" AND issuetime>='"+timeE+"'");
			}
			
			
			//System.out.println(queryStr.toString());
			try {
				list = NcDirectDbQuery.executeQuery( queryStr.toString(), NCTEXT_DATA_DB_NAME, QueryLanguage.SQL);
				//list has stns which has report(s) in DB now, next, use each stnid as key to get stn info from  
				//main stn list and form a partial stn list
				for(Object[] objAr: list){
					for(NctextStationInfo stnInfo : staList){
						if(fileType.equals("WRECON")){
							if((stnInfo.getProductid().equals(objAr[0]))&& (stnInfo.getStnid().equals(objAr[1]) )){
								// this stn has report in DB now, add this stn info to return list
								staListFiltered.add(stnInfo);
							}
							
						} else {
							//currently on W type
							if(stnInfo.getStnid().equals(objAr[0])){
								// this stn has report in DB now, add this stn info to return list
								staListFiltered.add(stnInfo);
							}
						}
					}
				}

			}
			catch (Exception e ){
				// do nothing, this stn does not have report in DB	
				System.out.println("DB exception when filtering");
			}

			return staListFiltered;
		}
		else {
			//to make sure caller wont clear the return list. should create another list and return
			List<NctextStationInfo> rtnstaList = new ArrayList<NctextStationInfo> ();
			if(staList != null && staList.size()>0)
				rtnstaList.addAll(staList);
			return rtnstaList;
		}
	}
	
	public List<Object[]> getProductDataList(String productName,NctextStationInfo station,
			EReportTimeRange rptTimeRange, boolean isState, String outputFileName){
		List<NctextStationInfo> listOfStateStn;
		if(isState){
			// get state station list from map 
			//System.out.println("State "+ station.getState());
			listOfStateStn = productStateStationInfoListMap.get(productName+station.getState());
		}
		else {
			// create a state list with this single station
			listOfStateStn = new ArrayList<NctextStationInfo> ();
			listOfStateStn.add(station);
		}
		
		List<Object[]> list = null;
		List<Object[]> rtnList = new ArrayList<Object[]> ();
		for (NctextStationInfo sta: listOfStateStn){
			//System.out.println("Station "+ sta.getStnid() + " -  " + sta.getStnname());
			String queryStr = createProductDataQuery(productName,sta, rptTimeRange);

			try {
				list = NcDirectDbQuery.executeQuery( queryStr, NCTEXT_DATA_DB_NAME, QueryLanguage.SQL);
				//String ptype = (String)list.get(0)[0];
				//System.out.println("text data is \n"+ptype);
				rtnList.addAll(list);
			}
			catch (Exception e ){
				//System.out.println("-----DB exception at getProductDataList: "+e.getMessage());			
			}
		}
		return rtnList;
	}
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static Map<Object, Object> mapSortByComparator(Map<Object, Object> unsortMap) {
		  
		List list = new LinkedList(unsortMap.entrySet());
 
		//Chin: sort list based on comparator
		//Key value is date time.
		//sorted in descending order. I.e. latest time will be shown first.
		Collections.sort(list, new Comparator() {
			public int compare(Object o2, Object o1) {
				return ((Comparable) ((Map.Entry) (o1)).getKey())
                                       .compareTo(((Map.Entry) (o2)).getKey());
			}
		});
 
		// put sorted list into map again
                //LinkedHashMap make sure order in which keys were inserted
		Map sortedMap = new LinkedHashMap();
		for (Iterator it = list.iterator(); it.hasNext();) {
			Map.Entry entry = (Map.Entry) it.next();
			sortedMap.put(entry.getKey(), entry.getValue());
		}
		return sortedMap;
	}
	private List<List<Object[]>> getOBSSfcHrlyDataListList(List<NctextStationInfo> listOfStateStn,
			EReportTimeRange rptTimeRange, boolean isState) {
		
		List<Object[]> rtnList;
		List<List<Object[]>> rtnListList = new ArrayList<List<Object[]>> ();
		Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
		rcMap.put("pluginName", new RequestConstraint( OBS_SFC_HRLY_TABLE, ConstraintType.EQUALS ) );
		if(rptTimeRange.getTimeRange()>0) {
			Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
			SimpleDateFormat timeInSimple = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			Date date = new Date(cal.getTimeInMillis()-rptTimeRange.getTimeRange()* 3600000);
			String dateStr = timeInSimple.format(date);
			rcMap.put("dataTime.refTime", 
					new RequestConstraint(dateStr, ConstraintType.GREATER_THAN_EQUALS ) );
			//System.out.println("date time = "+dateStr);
		}

		for (NctextStationInfo sta: listOfStateStn){
			rtnList = new ArrayList<Object[]> ();
			//System.out.println("Station "+ sta.getStnid() + " -  " + sta.getStnname());
			
			String myStnId;
			//Chin Note; in nwx.sfstns table, those US stations' id started with K, its heading 'K' letter has been removed.
			// in "obs" table, US station id started with K is still coded with K at head.
			// so, we have add K in front of station id for query to 'obs' table.
			if(sta.stnid.length() < 4)
				myStnId = "K" + sta.stnid;
			else
				myStnId = sta.stnid;;
			rcMap.put("location.stationId", 
	        		new RequestConstraint(myStnId, ConstraintType.EQUALS ) );
			List<String> parameters = new ArrayList<String>();
            
	        parameters.add(OBS_SFC_HRLY_RAW_DATA_KEY);
	        parameters.add(OBS_SFC_HRLY_TIME_KEY);
			PointDataContainer pdc = requestObsPointData(parameters,rcMap,OBS_SFC_HRLY_TABLE);
			String report= new String();
			/*
			 * Chin Note: A typical report from "obs" DB table
			 * Header+Str1 (starting with station id)\r\r\n   +str2+optional \r\r\n....
			 * Example: KTPH as station id.
			 * A raw report 1
			 * "SAUS70 KWBC 250000
			 * METAR KTPH 251456Z AUTO 05007KT 10SM CLR M02/M10 A3009 RMK AO2 SLP197\r\r\n
			 *      T10221100 53003 FZRANO"
			 *      
			 * or report 2,
			 * "SAUS70 KWBC 250000
			 * METAR KTPH 252242Z AUTO 30012KT 10SM CLR 16/M13 A3000 RMK AO2 WSHFT 2222\r\r\n
			 *
             * $\r\r\n
             * 712 
             * SPCN31 KWBC 252246
             *
             * SPECI"
             * 
             * We will display them as,
             * "KTPH 251456Z AUTO 05007KT 10SM CLR M02/M10 A3009 RMK AO2 SLP197 T10221100 53003 FZRANO"
			 * and as,
			 * "KTPH 252242Z AUTO 30012KT 10SM CLR 16/M13 A3000 RMK AO2 WSHFT 2222 $"
			 */
			if(pdc !=null) {
				Map<Object, Object> timeDataMap = new HashMap<Object, Object>(); 
				for (int i = 0; i < pdc.getCurrentSz(); i++) {
					PointDataView pdv = pdc.readRandom(i);
					String oneReport;
					oneReport = pdv.getString(OBS_SFC_HRLY_RAW_DATA_KEY);
					int index = oneReport.indexOf(myStnId);
					if(index  == -1)
						continue;
					oneReport = oneReport.substring(index);
					index = oneReport.indexOf('\r');
					if(index  > 0) {
						String str2="";
						String str1 = oneReport.substring(0,index);
						
						String temStr = oneReport.substring(index);
						// find the head of str2, by get rid off \r, \n, Space in the front of temStr
						for(int j =0; j< temStr.length(); j++){
							if(temStr.charAt(j) > 33 && temStr.charAt(j)< 127) {
								str2= temStr.substring(j);
								// also get rid of tailing \r, \n etc..at end of str2 if there are existing
								index = str2.indexOf('\r');
								if(index  >0)
									str2 = str2.substring(0, index);
								break;
							}
						}
						oneReport = str1+ " "+ str2;
					}
					timeDataMap.put(pdv.getLong(OBS_SFC_HRLY_TIME_KEY), oneReport);
					//report = report + oneReport+"\n";
					System.out.println("raw data::"+i+ "::"+pdv.getString(OBS_SFC_HRLY_RAW_DATA_KEY));

				}
				timeDataMap = mapSortByComparator(timeDataMap);
				for (Map.Entry<Object, Object> entry : timeDataMap.entrySet()) {
					report = report + entry.getValue()+"\n";
				}
				Object[] rtnObj = new Object[2];
				rtnObj[0] =report;
				rtnObj[1]= myStnId;
				//System.out.println("final raw data::\n"+report);
				rtnList.add(rtnObj);
				rtnListList.add(rtnList);
			}
			rcMap.remove("location.stationId");
		}
		return rtnListList;
	
	}
	private List<List<Object[]>> getOBSSynDataListList(List<NctextStationInfo> listOfStateStn,
			EReportTimeRange rptTimeRange, boolean isState) {
		List<Object[]> rtnList;
		List<List<Object[]>> rtnListList = new ArrayList<List<Object[]>> ();
		Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
		rcMap.put("pluginName", new RequestConstraint( OBS_SYN_DATA_TABLE, ConstraintType.EQUALS ) );
		if(rptTimeRange.getTimeRange()>0) {
			Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
			SimpleDateFormat timeInSimple = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			Date date = new Date(cal.getTimeInMillis()-rptTimeRange.getTimeRange()* 3600000);
			String dateStr = timeInSimple.format(date);
			rcMap.put("dataTime.refTime", 
					new RequestConstraint(dateStr, ConstraintType.GREATER_THAN_EQUALS ) );
			//System.out.println("date time = "+dateStr);
		}
		Date date = new Date();
		@SuppressWarnings("deprecation")
		int timeoffset = date.getTimezoneOffset();
		for (NctextStationInfo sta: listOfStateStn){
			rtnList = new ArrayList<Object[]> ();
			//System.out.println("Station "+ sta.getStnid() + " -  " + sta.getStnname());
			
			String myProdId;
			/*
			 * Chin Note; 
			 * in nwx.lsfstns table, 'productId' was coded with 6 digits (characters)
			 * with the last digit always coded as '0'. This id is sent by GUI for data query.
			 * However, in OBS_SYN_DATA_TABLE (awips.sfcobs) DB table, its equivalent field is 
			 * 'stationid' and coded with only 5 digits (the first 5 chars of productId).
			 * Therefore, we will get rid of the last digit of 'productId' and used as stationId (as key) to search
			 * through OBS_SYN_DATA_TABLE.
			 * 
			 * TBD:::
			 * However, some productId in nwx.lsfstns table only have 5 digits. In such case, how do we handle it?? TBD Chin.
			 */
			
			myProdId = sta.productid.substring(0, 5);
			rcMap.put("location.stationId", 
	        		new RequestConstraint(myProdId, ConstraintType.EQUALS ) );
			List<String> parameters = new ArrayList<String>();
            
	        parameters.add(OBS_SYN_DATA_RAW_DATA_KEY);
	        parameters.add(OBS_SYN_DATA_TIME_KEY);
			PointDataContainer pdc = requestObsPointData(parameters,rcMap,OBS_SYN_DATA_TABLE);
			String report= new String();
			
			if(pdc !=null) {
				Map<Object, Object> timeDataMap = new HashMap<Object, Object>(); 
				for (int i = 0; i < pdc.getCurrentSz(); i++) {
					PointDataView pdv = pdc.readRandom(i);
					String oneReport;
					oneReport = pdv.getString(OBS_SYN_DATA_RAW_DATA_KEY);
					
					int index = oneReport.indexOf(myProdId);
					if(index  == -1)
						continue;
					oneReport = oneReport.substring(index);
					oneReport = oneReport.replace('\n', ' ');
					long obsTime = pdv.getLong(OBS_SYN_DATA_TIME_KEY);
					SimpleDateFormat timeInSimple = new SimpleDateFormat("ddHHmm");					
					date.setTime(obsTime+timeoffset*60000);
					String dateStr = timeInSimple.format(date);
					oneReport = dateStr +" "+oneReport;
										
					timeDataMap.put(pdv.getLong(OBS_SYN_DATA_TIME_KEY), oneReport);
					//report = report + oneReport+"\n";
					System.out.println("raw data::"+i+ "::"+pdv.getString(OBS_SYN_DATA_RAW_DATA_KEY));

				}
				timeDataMap = mapSortByComparator(timeDataMap);
				for (Map.Entry<Object, Object> entry : timeDataMap.entrySet()) {
					report = report + entry.getValue()+"\n";
				}
				report = sta.stnid+ " - "+sta.productid +"\n"+report;
				Object[] rtnObj = new Object[2];
				rtnObj[0] =report;
				rtnObj[1]= myProdId;
				//System.out.println("final raw data::\n"+report);
				rtnList.add(rtnObj);
				rtnListList.add(rtnList);
			}
			rcMap.remove("location.stationId");
		}
		return rtnListList;
	
	}
	@SuppressWarnings({ "unchecked", "rawtypes" })
	void addToSndPdvList(PointDataView pdv, List<PointDataView> pdvlst){
		boolean shouldAddToList = true;
		// First step, add the pdv to list.
		// If pdv's data type is Not found in any pdv in list, then just add it to list.
		// If pdv's data type is already found in one of the pdv in list. Then comparing its
		// "correction" string. The latest "corrected" one is used.
		for (Iterator it = pdvlst.iterator(); it.hasNext();) {
			PointDataView tpdv = (PointDataView) it.next();
			if(tpdv.getString(OBS_SND_DATA_TYPE_KEY).equals(pdv.getString(OBS_SND_DATA_TYPE_KEY))){
				if(tpdv.getString(OBS_SND_DATA_COR_KEY).compareTo((pdv.getString(OBS_SND_DATA_COR_KEY))) < 0){
					pdvlst.remove(tpdv);
				}
				else{
					shouldAddToList = false;
				}
				break;
			}
		}
		if(shouldAddToList){
			pdvlst.add(pdv);
		}
		// NWX report data in the order of "TTAA, PPAA, TTBB, PPBB, TTCC, ...etc. We will have to do the same way.
		// do sorting based on such rule.
		Collections.sort(pdvlst, new Comparator() {
			public int compare(Object o1, Object o2) {
				String first2of1= ((PointDataView) (o1)).getString(OBS_SND_DATA_TYPE_KEY).substring(0,2);
				String first2of2= ((PointDataView) (o2)).getString(OBS_SND_DATA_TYPE_KEY).substring(0,2);
				String last2of1 = ((PointDataView) (o1)).getString(OBS_SND_DATA_TYPE_KEY).substring(2);
				String last2of2 = ((PointDataView) (o2)).getString(OBS_SND_DATA_TYPE_KEY).substring(2);
				
					if(last2of1.compareTo(last2of2) > 0)
						return 1;
					else if(last2of1.compareTo(last2of2) < 0)
						return -1;
					else {
						if(first2of1.compareTo(first2of2) > 0)
							return -1;
						else if(first2of1.compareTo(first2of2) < 0)
							return 1;
						return 0;
					}
				
			}
		});
		return;
	}
	
	@SuppressWarnings("unchecked")
	private List<List<Object[]>> getOBSSndDataListList(List<NctextStationInfo> listOfStateStn,
			EReportTimeRange rptTimeRange, boolean isState) {

		List<Object[]> rtnList;
		List<List<Object[]>> rtnListList = new ArrayList<List<Object[]>> ();
		Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
		rcMap.put("pluginName", new RequestConstraint( OBS_SND_DATA_TABLE, ConstraintType.EQUALS ) );
		rcMap.put("nil", new RequestConstraint( "FALSE", ConstraintType.EQUALS ) ); 
		if(rptTimeRange.getTimeRange()>0) {
			Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
			SimpleDateFormat timeInSimple = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			Date date = new Date(cal.getTimeInMillis()-rptTimeRange.getTimeRange()* 3600000);
			String dateStr = timeInSimple.format(date);
			rcMap.put("dataTime.refTime", 
					new RequestConstraint(dateStr, ConstraintType.GREATER_THAN_EQUALS ) );
			System.out.println("date time = "+dateStr);
		}
		List<PointDataView> pdvList=null;
		Map<Object, Object> timeDataMap = new HashMap<Object, Object>(); 
		Date date = new Date();
		@SuppressWarnings("deprecation")
		int timeoffset = date.getTimezoneOffset();
		for (NctextStationInfo sta: listOfStateStn){
			rtnList = new ArrayList<Object[]> ();
			//System.out.println("Station "+ sta.getStnid() + " -  " + sta.getStnname());
			timeDataMap.clear();
			String myStnId = sta.stnid;
			String myProdId = sta.productid;
			//Chin Note; in nwx.snstns table, US stations' id are not completed coded.
			// Therefore, use its productId as searching key. ProductId is a miss leading name. It is actually
			// is the station number.
			// The equivalent filed of productId in OBS_SND_DATA_TABLE is 'stnum'.
			
			rcMap.put("stnum", 
	        		new RequestConstraint(myProdId, ConstraintType.EQUALS ) );
			List<String> parameters = new ArrayList<String>();
            
	        parameters.add(OBS_SND_DATA_RAW_DATA_KEY);
	        parameters.add(OBS_SND_DATA_TIME_KEY);
	        parameters.add(OBS_SND_DATA_COR_KEY);
	        parameters.add(OBS_SND_DATA_NIL_KEY);
	        parameters.add(OBS_SND_DATA_STATIONID_KEY);
	        parameters.add(OBS_SND_DATA_STNUM_KEY);
	        parameters.add(OBS_SND_DATA_TYPE_KEY);
			PointDataContainer pdc = requestObsPointData(parameters,rcMap,OBS_SND_DATA_TABLE);
			String oneRefTimeReport= new String();
			String finalReport= new String();
			
			if(pdc !=null) {
				PointDataView pdv=null;
				/*
				 * Chin Note:
				 * Each TTAA, TTBB...PPAA.. data are stored in its own record with "correction"
				 * We have to keep the latest corrected data and discard earlier "not correct" data
				 * We then store same obs time data in one pdvList and then put into
				 * timeDataMap<Obstime, pdvList> for later time sorting.
				 */
				for (int i = 0; i < pdc.getCurrentSz(); i++) {
					pdv = pdc.readRandom(i);
					long sndTime= pdv.getLong(OBS_SND_DATA_TIME_KEY);
					pdvList = (List<PointDataView>)timeDataMap.get(sndTime);
					if(pdvList==null){
						pdvList = new ArrayList<PointDataView>();
					}
					addToSndPdvList(pdv, pdvList);
					timeDataMap.put(sndTime, pdvList); // replace pdvList with latest one
					System.out.println("raw data::::"+i+"::"+pdv.getString(OBS_SND_DATA_RAW_DATA_KEY));
				}
				//We have to sort data based on observed time before creating final report.   
				timeDataMap = mapSortByComparator(timeDataMap);
				
				//now create report : use the same format as NWX
				for (Map.Entry<Object, Object> entry : timeDataMap.entrySet()) {
					pdvList = (List<PointDataView>) entry.getValue();
					long obsTime= (Long) entry.getKey();
					for (@SuppressWarnings("rawtypes")
					Iterator it = pdvList.iterator(); it.hasNext();) {
						pdv = (PointDataView) it.next();
						String oneRecord;
						oneRecord = pdv.getString(OBS_SND_DATA_RAW_DATA_KEY);
						int indexTT = oneRecord.indexOf("TT");
						int indexPP = oneRecord.indexOf("PP");
						int index;
						if(indexTT  == -1 && indexPP  == -1)
							continue;
						else if(indexTT  == -1)
							index = indexPP;
						else
							index = indexTT;
						
						oneRecord = oneRecord.substring(index);
						// beautify report. 
						//replace all Space (may be several Spaces), \n, etc.. with "one" Space
						oneRecord= oneRecord.replaceAll("\\s+", "S");		   
						// Every 12 digit strings displayed in one line, as NWX does.
						int spaceCount=0;
						char[] recordAry = oneRecord.toCharArray();
						for (int charindex=0; charindex <oneRecord.length(); ){
							String temStr = oneRecord.substring(charindex);
							int spaceIndex = temStr.indexOf("S");
							if(spaceIndex > 0){
								charindex = charindex + spaceIndex;
								spaceCount++;
								if(spaceCount%12 == 0){
									recordAry[charindex] = '\n';
								}
								else{
									recordAry[charindex] = ' ';
								}
								charindex++;
							}
							else
								break;
						}
						oneRefTimeReport = oneRefTimeReport + String.valueOf(recordAry)+"\n";
					}
					SimpleDateFormat timeInSimple = new SimpleDateFormat("yyMMdd/HHmm");//("yyyy-MM-dd HH:mm:ss");
					
					date.setTime(obsTime+timeoffset*60000);
					String dateStr = timeInSimple.format(date);
					oneRefTimeReport = pdv.getString(OBS_SND_DATA_STATIONID_KEY)+" - "+
						pdv.getString(OBS_SND_DATA_STNUM_KEY)+ " at "+ dateStr+"\n" +oneRefTimeReport;
					finalReport = finalReport + oneRefTimeReport+"\n";
					oneRefTimeReport="";
				}
				
				/*timeDataMap = mapSortByComparator(timeDataMap);
				for (Map.Entry<Object, Object> entry : timeDataMap.entrySet()) {
					report = report + entry.getValue()+"\n";
				}*/
				Object[] rtnObj = new Object[2];
				rtnObj[0] =finalReport;
				rtnObj[1]= myStnId;
				//System.out.println("final raw data::\n"+report);
				rtnList.add(rtnObj);
				rtnListList.add(rtnList);
			}
			rcMap.remove("stnum");
		}
		return rtnListList;
	
	}
	/*
	 * Return A List(A) which element is a List(B) with elements of Object[] type.
	 * Object[] - contain one DB query result. Object[0]= text rawrecord, Object[1] = text issuesite
	 * List(B) - List<Object[]> - contains one station's query result, its size = number of query hits fro this station
	 * List(A) - List<List<Object[]>> - contain one state's query result. its size = numer of Stations of this state having report
	 * In case of single station query, there will be only one station on List(A).
	 */
	public List<List<Object[]>> getProductDataListList(String groupName,String productName,NctextStationInfo station,
			EReportTimeRange rptTimeRange, boolean isState, String outputFileName){
		List<NctextStationInfo> listOfStateStn;
		if(isState){
			// get state station list from map 
			//System.out.println("State "+ station.getState());
			listOfStateStn = productStateStationInfoListMap.get(productName+station.getState());
		}
		else {
			// create a state list with this single station
			listOfStateStn = new ArrayList<NctextStationInfo> ();
			listOfStateStn.add(station);
		}
		if(groupName.equals(OBS_DATA_GROUP)){
			//Note: when products need query to its own DB table (NOT to "nctext" table)
			// It should do its own query and return from here.
			if(productName.equals(OBS_SFC_HRLY_PRODUCT_NAME)){
				return (getOBSSfcHrlyDataListList(listOfStateStn,
						rptTimeRange,  isState) );
			} else if(productName.equals(OBS_SND_DATA_PRODUCT_NAME)){
				return (getOBSSndDataListList(listOfStateStn,
						rptTimeRange,  isState) );
			} else if(productName.equals(OBS_SYN_DATA_PRODUCT_NAME)){
				return (getOBSSynDataListList( listOfStateStn,
						rptTimeRange,  isState) );
			}
			//Note: Agricurtural Obs (*.AGO), TAF(*.taf) and RADAT (*.FZL) data are decoded by Nctext decoder. 
		}
		// All other products, fall through here....
		List<Object[]> list = null;
		List<Object[]> rtnList;
		List<List<Object[]>> rtnListList = new ArrayList<List<Object[]>> ();
		for (NctextStationInfo sta: listOfStateStn){
			rtnList = new ArrayList<Object[]> ();
			//System.out.println("Station "+ sta.getStnid() + " -  " + sta.getStnname());
			// create DB query string and execute SQL query
			String queryStr = createProductDataQuery(productName,sta, rptTimeRange);

			try {
				list = NcDirectDbQuery.executeQuery( queryStr, NCTEXT_DATA_DB_NAME, QueryLanguage.SQL);
				if(list.size() > 0){					
					rtnList.addAll(list);
					rtnListList.add(rtnList);
				}
			}
			catch (Exception e ){
				//System.out.println("-----DB exception at getProductDataList: "+e.getMessage());			
			}
		}
		return rtnListList;
	}

	public Map<String, List<NctextStationInfo>> getProductStationInfoListMap() {
		return productAllStationInfoListMap;
	}

	public boolean isAutoUpdate() {
		return autoUpdate;
	}

	public void setAutoUpdate(boolean autoUpdate) {
		this.autoUpdate = autoUpdate;
	}
	
	public List<NctextStationInfo> getStateStationInfoList(String key){
		// key is product name + state name
		List<NctextStationInfo> listOfStateStn;
		listOfStateStn = productStateStationInfoListMap.get(key);
		return listOfStateStn;
	}
	private PointDataContainer requestObsPointData(List<String> parameters,
            Map<String, RequestConstraint> rcMap, String tableName)  {
       
        PointDataContainer pdc = null;
        try {
            pdc = DataCubeContainer.getPointData(tableName,
                    parameters.toArray(new String[parameters.size()]), null,rcMap);
        } catch (VizException e) {
        	System.out.println("requestObsPointData-DataCubeContainer:Error getting raw data from "+tableName+" ::" +
                    e);
        }
        if(pdc==null){
        	try {
        		pdc = PointDataRequest.requestPointDataAllLevels(
        				null, tableName, 
        				parameters.toArray( new String[parameters.size()] ),
        				null, rcMap );
        	} catch (VizException e) {
        		System.out.println("requestObsPointData-PointDataRequest: Error getting raw data from "+tableName+" ::" +
                    e);
        	}
        }
        if (pdc != null) {
            pdc.setCurrentSz(pdc.getAllocatedSz());
            return pdc;
        }
        else 
        	return null;
    }
}
