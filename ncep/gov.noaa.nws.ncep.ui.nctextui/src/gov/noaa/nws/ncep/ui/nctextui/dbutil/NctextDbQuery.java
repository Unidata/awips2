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
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nctextui.dbutil;

import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;


public class NctextDbQuery {

	private final String NCTEXT_DATA_DB_NAME = "metadata";
	private final String NCTEXT_STATIC_DB_NAME = "ncep";
	private final String NCTEXT_STATIC_GP_TABLE_NAME = "nwx.datatypegrouplist";	
	private final String NCTEXT_DATA_DB_TABLE = "awips.nctext";
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
	/*
	 * Return A List(A) which element is a List(B) with elements of Object[] type.
	 * Object[] - contain one DB query result. Object[0]= text rawrecord, Object[1] = text issuesite
	 * List(B) - List<Object[]> - contains one station's query result, its size = number of query hits fro this station
	 * List(A) - List<List<Object[]>> - contain one state's query result. its size = numer of Stations of this state having report
	 * In case of single station query, there will be only one station on List(A).
	 */
	public List<List<Object[]>> getProductDataListList(String productName,NctextStationInfo station,
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
}
