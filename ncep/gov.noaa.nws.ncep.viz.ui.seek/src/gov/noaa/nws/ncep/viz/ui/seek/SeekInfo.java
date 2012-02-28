/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * October 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */


package gov.noaa.nws.ncep.viz.ui.seek;

import java.util.*;
import java.io.File;
import java.io.IOException;

import javax.xml.parsers.*;

import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
//import gov.noaa.nws.ncep.viz.ui.locator.util.LoadEnvelopes;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The storage and utility class for the SEEK tool .
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * March 2009  	337        	G. Zhang   	Initial creation. 	 	
 * 07/11        #450        G. Hull     NcPathManager
 * 01/12        #561        G. Hull     make independent of Locator project. 
 * 
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 * 
 */

public class SeekInfo {
	
	/**
	 * data structure holding station types and database and table names
	 * LinkedHashMap for keeping the same order as in NMAP2
	 */
	private static final Map<String, String[]> TYPE_DB_TABLE_MAP = new LinkedHashMap<String, String[]>();
	
	
	// The first station type in Seek dialog Combo Widgets	 
//	private static final String FIRST_TYPE = "LATLON";

	//maximum factor nubmer
	private static final int MaxFactorNum = 6; 
	
	// Filter box factor
	private static final int boxFactor = 2;
	
//	private static int filterFactor = 1;
	
//	private static final double POINT_INQUIRY_TOLERANCE = 0.75;  
	
	// Default attribute name for point shapefile
	private String attribute = "NAME";		
	
	// Default attribute name for State ID
	private String stateIDAttr = "ST";	
	
	//maximum number of points for display.
	private static final int MAX_POINT_NUM = 25;
	
	//word separator
	public static final String WORD_SEPERATOR = ":::";
    
	//Map for query result with text field as key.
    private static Map<Integer, QueryResultRow[]>  QUERY_RESULTS_MAP = new HashMap<Integer, QueryResultRow[]>();
	
	/**
	 * initialize the data structure
	 */
	static{
		try{
			parseSeekStnsFile();
		}catch(Exception e){
			System.out.println("_______SeekInfo: static initialization block: Error: "+e.getMessage());
		}	
		
	}
	
    private static class LocatorPoitntDataComparator implements Comparator<SeekPointData> {

		public int compare(SeekPointData o1, SeekPointData o2) {
			return o1.getDistanceInMeter() - o2.getDistanceInMeter();
		}

	}
	
	/**
	 * Read the seekStns.xml file and store info in data structures
	 * the attribute names must match those in seekStns.xml.
	 * 
	 * The seek GUI is relatively stable thus the query fields;
	 * but the station types may change, a new station type in 
	 * the database may have different fields names; e.x. station_id
	 * vs. stid, latitude vs. lat. We use the xml file attributes to
	 * represent them so query can be write without change.   
	 */
	private static void parseSeekStnsFile(){
		
		Document doc = null;		
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();		

		try {			
			
			File file = NcPathManager.getInstance().getStaticFile( 
					NcPathConstants.SEEK_STN_TBL );
			DocumentBuilder builder = factory.newDocumentBuilder();
			doc = builder.parse( file.getAbsoluteFile() );
			
		} catch (Exception e) {  
			System.out.println("________SeekInfo: parseSeekStnsFile(): Error:"+e.getMessage());		
		} 
		
		NodeList nlist = doc.getElementsByTagNameNS("*", "*");		

		String type="", db="", table="", idfield="", namefield="", latfield="", lonfield="", statefield="";
		
		for(int i=0;  i<nlist.getLength(); i++){
			
			Node nElem = nlist.item(i);//each element: station			
					
			NamedNodeMap nnMap = nElem.getAttributes();		
					
			for(int j=0; j<nnMap.getLength(); j++){
				
				Node nAttr = nnMap.item(j);//each attribute of an element, like: type, dbtable, and db
					
				if("type".equalsIgnoreCase(nAttr.getNodeName().trim()))
					type = nAttr.getNodeValue();
				else if("db".equalsIgnoreCase(nAttr.getNodeName().trim()))
					db = nAttr.getNodeValue();
				else if("dbtable".equalsIgnoreCase(nAttr.getNodeName().trim()))
					table = nAttr.getNodeValue();		
				else if("idfield".equalsIgnoreCase(nAttr.getNodeName().trim()))
					idfield = nAttr.getNodeValue();
				else if("namefield".equalsIgnoreCase(nAttr.getNodeName().trim()))
					namefield = nAttr.getNodeValue();
				else if("latfield".equalsIgnoreCase(nAttr.getNodeName().trim()))
					latfield = nAttr.getNodeValue();
				else if("lonfield".equalsIgnoreCase(nAttr.getNodeName().trim()))
					lonfield = nAttr.getNodeValue();
				else if("statefield".equalsIgnoreCase(nAttr.getNodeName().trim()))
					statefield = nAttr.getNodeValue();
				
			
			}
			
			if( ! type.isEmpty())
				TYPE_DB_TABLE_MAP.put(type, new String[]{db, table, idfield, namefield, latfield, lonfield,statefield});
			
		}	
		
	}
	
	/**
	 * get the Station types in an array
	 * @return: the array containing station types
	 */
	
	public static String[] getStnTypes(){
		
		String[] a = null;		
		
		try{
			
			a = TYPE_DB_TABLE_MAP.keySet().toArray(new String[]{});

		}catch(Throwable t){
			System.out.println("_______ Error: "+t.getMessage());
		}
		
		return a;
	}
	
	/**
	 * get the station types' database and table names
	 * @param stnType: the station name
	 * @return:			the array containing database and table names: idx 0: db; idx 1: table
	 */
	public static String[] getStnDbTbl(String stnType){
		
		return TYPE_DB_TABLE_MAP.get(stnType);
		
	}
	
	/**
	 * Query method for getting data:
	 * Tables have different column names: station_id/stid, 
	 * latitude/lat, etc and stns.island table has no id at all.
	 * 
	 * Query Strings need to reflect this.
	 * @param coord:	the coordinate of the mouse click point;
	 * @param stnType:	the station type: vor, city, etc;
	 * @return:			the LocatorPointData array.
	 */
	public static SeekPointData[] getPtsData(Coordinate coord, String stnType){
		
//		LocatorDataSource citiesDataSource = 
//			LocatorDataSourceMngr.getInstance().getCitiesLocatorDataSource( );		
//		CitiesDataSource cpd = new CitiesDataSource();	
		
		java.util.List<SeekPointData> list = null;		
		
		try{
			
			list = nameIdQuery(coord.y, coord.x, getStnDbTbl(stnType)[1]);
			
		}catch (DataAccessLayerException e) {
			System.out.println("___________ SeekInfo: getPtsData(): Error: "+e.getMessage());
		}		

		return list.toArray(new SeekPointData[]{});
		
	}
	
	/**
	 * Query the database for a given point and station type.
	 * 
	 * @param lat:		latitude of the point;
	 * @param lon:		longitude of the point.
	 * @param stnType:  station type.
	 * @return:			a list of the LocatorPointData relative to the point and station.
	 * @throws DataAccessLayerException
	 */
	public static List<SeekPointData>  nameIdQuery(double lat,double lon, String stnType)
										throws DataAccessLayerException {
		
		/* island has only three field */
		boolean isIsland = "ISLAND".equalsIgnoreCase(stnType);
		
		QueryResultRow[] citiesResult = null;
		double corr = 0.2;
		List<SeekPointData> pointList = new ArrayList<SeekPointData>();

		double minLat= lat-corr;
		double maxLat =lat+corr;
		double minLon=lon-corr*5;
		double maxLon=lon+corr*5;
		
		String dbtbl = getStnDbTbl(stnType)[1];
		String id = getStnDbTbl(stnType)[2];
		String name = getStnDbTbl(stnType)[3];
		String la = getStnDbTbl(stnType)[4];
		String lo = getStnDbTbl(stnType)[5]; 
		
		StringBuilder query1 = new StringBuilder();
		query1.append("Select ");
		if(id!=null && ! id.isEmpty())	query1.append(id).append(",");
		query1.append(name).append(",");
		query1.append(la).append(",");
		query1.append(lo).append(" from ");
		query1.append(dbtbl);
		
		
		StringBuilder query2 = new StringBuilder();
		query2.append("Select ");
		if(id!=null && ! id.isEmpty())	query2.append(id).append(",");
		query2.append(name).append(",");
		query2.append(la).append(",");
		query2.append(lo).append(" from ");
		query2.append(dbtbl);		
		
		if( isWhereInQuery(dbtbl) ){
			
			query1.append(" where ").append(la).append(" between ").append(minLat).append(" and ").append(maxLat);
			query1.append(" and ").append(lo).append(" between ").append(minLon).append(" and ").append(maxLon);
			
			query2.append(" where ").append(la).append(" between ").append(minLat).append(" and ").append(maxLat);
		}
	
		if (citiesResult == null)  
			citiesResult = getcitiesList(query1.toString(),query2.toString());
		
		for(QueryResultRow rows:citiesResult){
			SeekPointData pointData = new SeekPointData();
			String n = (String)( isIsland ? ""+WORD_SEPERATOR+rows.getColumn(0) : rows.getColumn(0)+WORD_SEPERATOR+rows.getColumn(1) );
			pointData.setName(n);
			pointData.setLat((Double) rows.getColumn( isIsland ? 1 : 2));
			pointData.setLon((Double) rows.getColumn( isIsland ? 2 : 3));
			pointList.add(pointData);
		}
		
		return pointList;
		
	}
	
	/**
	 * Query the database using given query strings.
	 * @param query1:	query string within a smaller area;
	 * @param query2:	query string within a bigger area.
	 * @return:			QueryResultRow array.
	 */
	private  static QueryResultRow[]  getcitiesList(String query1,String query2){

		QueryResult citiesList;
		QueryResultRow[] citiesResult = null;

		try {
			
			citiesList = NcDirectDbQuery.executeMappedQuery(query1, "ncep", QueryLanguage.SQL);
			if (citiesList != null )	citiesResult= citiesList.getRows();
			
			if (citiesResult!=null && citiesResult.length >= MAX_POINT_NUM ){
			}
			else 
			{
				citiesList = NcDirectDbQuery.executeMappedQuery(query2, "ncep", QueryLanguage.SQL);
				citiesResult= citiesList.getRows();
			}
		}
		catch (Exception e ){
			System.out.println("___________SeekInfo: getcitiesList(): Error: "+e.getMessage());
		}

		return citiesResult ; 
	}
	
    /**
     * copied from PointDataResource.java to replace the getPtsData(,,)
     * Find the closest points from given point
     *  
     * @param ll:		Coordinate of the point;
     * @param stnType:	station type.
     * @return:			LocatorPointData array relative to the station and point.
     * @throws VizException
     * @throws RuntimeException
     * @throws IOException
     */
    public static SeekPointData[] getClosestPoints(Coordinate ll, String stnType) 
					throws VizException, RuntimeException, IOException  {
    	/*
    	 * Check for invalid Coordinate
    	 */
    	if (ll == null || ll.x > 180.0 || ll.x < -180.0 || ll.y > 90.0 || ll.y < -90.0)
    		return null;
	
    	/*
    	 * Retrieve all points within the envelope
    	 */
    	List<SeekPointData> list = new ArrayList<SeekPointData>();
    	    	
    	try{
    		list = nameIdQuery(ll.y, ll.x, stnType);
    	}catch(Exception e){
    		System.out.println("__________ SeekInfo: getClosestPoints(): Error: "+ e.getMessage());
    	}
	
    	/*
    	 * expand the envelope if no point inside
    	 */
    	int factorNum = 1;
    	boolean filter = true;
    	while (list.size() < MAX_POINT_NUM) {
//    		filterFactor = factorNum * boxFactor;
		
    		if (factorNum > MaxFactorNum) filter = false;
    		
    		list.clear();
    		try{
    			list = nameIdQuery(ll.y, ll.x, stnType);
    		}catch(Exception e){
    			
    		}
		
    		if (factorNum > MaxFactorNum || filter == false) break;
    		factorNum++;    		
    	}
	
    	if (list.size() < 1) return null;
	
    	/*
    	 * Compute distance (in meter) of the closest points
    	 */
    	SeekPointData[] ptDataOut = new SeekPointData[list.size()];
    	for (int ii = 0; ii < list.size(); ii++) {
		
    		GeodeticCalculator gc = new GeodeticCalculator(
    				DefaultEllipsoid.WGS84);
    		gc.setStartingGeographicPoint(ll.x, ll.y);
    		gc.setDestinationGeographicPoint(list.get(ii).getLon(), list.get(ii).getLat());            
    		double dist = -999.99;
    		dist = gc.getOrthodromicDistance();
    		//list.get(ii).setDistanceInMeter((int)dist);
        
    		double direction = -999.99;
    		if (dist > 0) {
    			direction = gc.getAzimuth();
    			if ( direction < 0 )
    				direction = 360 + direction;
    		}
    		//list.get(ii).setDir(direction);
    		
    		ptDataOut[ii] = new SeekPointData();
    		ptDataOut[ii].setName(list.get(ii).getName());
    		ptDataOut[ii].setDistanceInMeter((int)dist);
    		ptDataOut[ii].setDir(direction);
    		ptDataOut[ii].setLat(list.get(ii).getLat());
    		ptDataOut[ii].setLon(list.get(ii).getLon());
        
    	}
	
    	// Sort the list by distance
    	Arrays.sort(ptDataOut, new LocatorPoitntDataComparator());
    	
    	// Only export top 25 closest points
    	int number = (ptDataOut.length > MAX_POINT_NUM) ? MAX_POINT_NUM : ptDataOut.length;
    	SeekPointData[] out = new SeekPointData[number];
    	for (int i = 0; i < number; i++) {
    		out[i] = new SeekPointData();
    		out[i].setName(ptDataOut[i].getName());
    		out[i].setDistanceInMeter(ptDataOut[i].getDistanceInMeter());
    		out[i].setDir(ptDataOut[i].getDir());
    		out[i].setLat(ptDataOut[i].getLat());
    		out[i].setLon(ptDataOut[i].getLon());
    	}
    	
    	return out;
    }
    
    /**
     * Tell whether the database query string includes the where clause.
     * 
     * @param dbtbl:	the name of the database table;
     * @return:			true with where clause; false otherwise.
     */
    public static boolean isWhereInQuery(String dbtbl){
    	return "stns.cities".equalsIgnoreCase(dbtbl);//TODO: may have other station types
    }
    
    /**
     * Return station name or id according to station type, List type, and name.
     * 
     * @param name: 	String containing station_id/stid + WORD_SEPERATOR + name
     * @param stnType:	type of stations: vors, county, cities, etc
     * @param isList:	used in SWT List or SWT Text
     * @return
     */
    public static String getNameOrId(String name, String stnType, boolean isList){
    	if(name == null) return "";
    	
    	if( isList ){	
    		return name.split(SeekInfo.WORD_SEPERATOR)[1].toUpperCase();
    		
    	}else{
    		
    		if("VOLCANO".equalsIgnoreCase(stnType)
    				|| "COUNTY".equalsIgnoreCase(stnType)
    				|| "ISLAND".equalsIgnoreCase(stnType)
    				|| "WR_QPF".equalsIgnoreCase(stnType)
    				|| "TROPCY_BRKS".equalsIgnoreCase(stnType)
    				|| "CITIES".equalsIgnoreCase(stnType)/**/ )
    			return name.split(SeekInfo.WORD_SEPERATOR)[1].toUpperCase().trim();
    		else
    			return name.split(SeekInfo.WORD_SEPERATOR)[0].toUpperCase().trim();    		
    		
    	}
    }
    
    /**
     * NOT used anymore since Seek ONLY uses points NO areas.
     * 
     * Designed to be used for COUNTY in SeekResultsDialog's 
     * setClickPtText() for areas.
     * 
     * @param aLatLon
     * @param stnType
     * @return
     */
//    public static String getCntyLPD(Coordinate aLatLon, String stnType){   	
//    	
//    	HashMap<Envelope,List<Object[]>> savedEnvelopes = LoadEnvelopes.getbndEnv("COUNTIES");//stnType
//        
//    	List<Object[]> BoundsDataStore = null;
//		
//    	for ( Envelope env : savedEnvelopes.keySet() ) {
//			if ( env.contains(aLatLon) ) {
//	    		    BoundsDataStore = savedEnvelopes.get(env);
//				break;
//			}
//		}
//    			
//    	GeometryFactory gf = new GeometryFactory();
//	    Point llPoint = gf.createPoint(aLatLon);
//                
//	   	WKBReader wkbReader = new WKBReader();
//	    for ( Object[] bound : BoundsDataStore){
//	    	
//	    	byte[] wkb = (byte[]) bound[0];
//			MultiPolygon boundGeo = null;
//			
//			try {
//		    	boundGeo = (MultiPolygon) wkbReader.read(wkb);		    			
//		    } catch (ParseException e) {
//		    	e.printStackTrace();
//		    }
//		    
//		    if (boundGeo.contains(llPoint) ){		    	
//		    	return (String)bound[1];// name
//		    }
//	    }
//    	return "";
//
//    }
    
    private static class PointDataNameComparator implements Comparator<SeekPointData> {

		public int compare(SeekPointData o1, SeekPointData o2) {
			return o1.getName().compareTo(o2.getName());
		}

	}

    /**
     * Used for the Text fields typed-in searching in SeekResultsDialog.
     * Based on PointDataResource class' method with the same name.
     * 
     * TODO: assign SeekInfo.queryResults null for GC for memory saving.
     * 
     * @param prefix: 	letters typed-in;
     * @param stnType:	station type.
     * @return:			LocatorPointData array contains matched points info.
     * @throws VizException
     * @throws RuntimeException
     * @throws IOException
     */
    public static SeekPointData[] getMatchedPoints(String prefix, String stnType, int comboNo) 
									throws VizException, RuntimeException, IOException  {
    	
    	if(prefix==null || stnType==null)
    		return null;

		QueryResultRow[] citiesResult = SeekInfo.QUERY_RESULTS_MAP.get(comboNo);
		if(citiesResult == null)
			return null;	
		
		/* some has NO state field */
		String state = getStnDbTbl(stnType)[6];
		
		/* island has only three field */
		boolean isIsland = "ISLAND".equalsIgnoreCase(stnType);
		
		List<SeekPointData> list = new ArrayList<SeekPointData>();
		int len = prefix.length();
		
		for(int i=0; i<citiesResult.length; i++){
			
			SeekPointData point = new SeekPointData();
			
			/* some use col 0: id; others col 1: name */
			String name= (String)citiesResult[i].getColumn( isNameOrIdCol(stnType) );          
			if (name.length() < 2 || name.length() < len) continue;
						
			if (name.substring(0, len).equalsIgnoreCase(prefix)) {

				point.setName(name);
				point.setStateID( (state==null||state.isEmpty()) ? "" : (String)citiesResult[i].getColumn(4) );

				point.setLat((Double)citiesResult[i].getColumn( isIsland ? 1 : 2));
				point.setLon((Double)citiesResult[i].getColumn( isIsland ? 2 : 3));
		
				list.add(point);     
			}
		}		
			
		SeekPointData[] ptDataOut = new SeekPointData[list.size()];
		for (int i = 0; i < list.size(); i++) {
			ptDataOut[i] = new SeekPointData();
			ptDataOut[i].setName(list.get(i).getName());
			ptDataOut[i].setStateID(list.get(i).getStateID());
			ptDataOut[i].setLat(list.get(i).getLat());
			ptDataOut[i].setLon(list.get(i).getLon());
		}		
		
		// Sort the list by name
		Arrays.sort(ptDataOut, new PointDataNameComparator());
		
		return ptDataOut;
	}
    
    /**
     * Return the column index according to station type.
     * 
     * @param stnType: 	the type of stations
     * @return:			int 0: id of the station, 1: name.
     */
    public static int isNameOrIdCol(String stnType){
    	
    	if("VOLCANO".equalsIgnoreCase(stnType)
				|| "COUNTY".equalsIgnoreCase(stnType)
//				|| "ISLAND".equalsIgnoreCase(stnType)
				|| "WR_QPF".equalsIgnoreCase(stnType)
				|| "TROPCY_BRKS".equalsIgnoreCase(stnType)
				|| "CITIES".equalsIgnoreCase(stnType) )
			return 1;
		else
			return 0;
    	
    }
    
    /**
     * Query the database with a station type.
     * 
     * This method pre-query the database for better performance
     * in typed-in station seek.
     * 
     * @param stnType:	the station type;
     * @return:			a QueryResultRow array.
     */
    public static void doDBQuery(String stnType, int comboNo){
    	
    	if(stnType==null || stnType.isEmpty())
    		return;
    	
    	//get query fields
		String dbtbl = getStnDbTbl(stnType)[1];
		
		if(dbtbl==null || dbtbl.isEmpty())
			return;
		
		String id = getStnDbTbl(stnType)[2];
		String name1 = getStnDbTbl(stnType)[3];
		String la = getStnDbTbl(stnType)[4];
		String lo = getStnDbTbl(stnType)[5]; 
		String state = getStnDbTbl(stnType)[6];
		
		//build the query String
		StringBuilder query1 = new StringBuilder();
		query1.append("Select ");
		
		if(id!=null && ! id.isEmpty())	
			query1.append(id).append(",");
		
		query1.append("upper("+name1+")").append(",");		
		query1.append(la).append(",");
		query1.append(lo);
		
		if(state!=null && ! state.isEmpty()) 
			query1.append(",").append(state);
		
		query1.append(" from ").append(dbtbl);			
		
		//query the db
		try {
			
			QueryResult citiesList = NcDirectDbQuery.executeMappedQuery(query1.toString(), "ncep", QueryLanguage.SQL);
			if (citiesList != null ){	
				QUERY_RESULTS_MAP.put( comboNo, citiesList.getRows() );	//queryResults1 = citiesList.getRows();
			}
			
		}
		catch (Exception e ){
			System.out.println("___________ SeekInfo.doDbQuery() Error: "+e.getMessage());
		}
		
    }
    
    /**
     * Run doDBQuery asynchronously using VizApp class's runAsync().
     * @param stnType:	the station type.
     */
    public static void preQueryDB(final String stnType, final int comboNo){
    	
    	VizApp.runAsync(new Runnable(){
    		public void run(){
    			doDBQuery(stnType, comboNo);
    		}
    	});
    	
    }

}
