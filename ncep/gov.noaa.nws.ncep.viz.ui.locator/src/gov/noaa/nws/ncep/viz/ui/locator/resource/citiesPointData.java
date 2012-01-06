
/**
 * example for cities station table mapping
 * 
 * This java class defines the getters and setters for the 
 *      
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 01/2010		Uma Josyula	Initial creation	
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.ui.locator.resource;


import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

//import org.apache.commons.logging.Log;
//import org.apache.commons.logging.LogFactory;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;


import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;


public class citiesPointData  {//extends CoreDao {
	
	private String query1,query2;

	//private Log logger = LogFactory.getLog(getClass());
	private  List<LocatorPointData> pointList;
	double distance,latitude,longitude;
	LocatorPointData nearestPoint =new  LocatorPointData();
	
	public citiesPointData() {
		
	}

	private Locator locator; 
	
	public void setLocator(Locator l){ 
		locator = l;
	}
	public  void  nameIdQuery(Coordinate co){
		longitude = co.y;
		latitude = co.x;
		try {
			nameIdQuery(longitude,latitude);
		} catch (DataAccessLayerException e) {
			e.printStackTrace();
		}

	}

	//@SuppressWarnings("deprecation")
	public void  nameIdQuery(double lat,double lon)
	throws DataAccessLayerException {
		QueryResultRow[] citiesResult = null;
		double corr = 0.2;
		pointList = new ArrayList<LocatorPointData>();

		double minLat= lat-corr;
		double maxLat =lat+corr;
		double minLon=lon-corr*5;
		double maxLon=lon+corr*5;
//		query1="Select station_Id,name,latitude,longitude " +
//		"FROM stns.cities WHERE latitude BETWEEN "+
//		minLat+" AND "+maxLat+" AND longitude BETWEEN "+
//		minLon+" AND "+maxLon;;
//		query2="Select station_Id,name,latitude,longitude " +
//		"FROM stns.cities  WHERE latitude BETWEEN "+
//		minLat+" AND "+maxLat;
//query1=new gov.noaa.nws.ncep.viz.ui.locator.db.LocatorQueryBuilder(locator).getQueryWithBetweenLatLon(new Coordinate(lat,lon), 1.5, 5.0);
//query2=new gov.noaa.nws.ncep.viz.ui.locator.db.LocatorQueryBuilder(locator).getQueryWithBetweenLatOnly(new Coordinate(lat,lon), 1.5);
//if(locator.getLocatorName().contains("MARINE")) query1=new gov.noaa.nws.ncep.viz.ui.locator.db.LocatorQueryBuilder(locator).getQueryWithoutWhere();		
		gov.noaa.nws.ncep.viz.ui.locator.db.LocatorQueryBuilder lqb =new gov.noaa.nws.ncep.viz.ui.locator.db.LocatorQueryBuilder(locator,new Coordinate(lat,lon));
		query1 = lqb.getQuery(false); query2 = lqb.getQuery(true);
		if (citiesResult == null)  
			citiesResult = getcitiesList(query1,query2);
		for(QueryResultRow rows:citiesResult){
			LocatorPointData pointData = new LocatorPointData();
			pointData.setStateID((String)rows.getColumn(0));
			pointData.setName((String)rows.getColumn(1));
			pointData.setLat((Double) rows.getColumn(2));
			pointData.setLon((Double) rows.getColumn(3));
			pointList.add(pointData);
		}
		
	}

	
	private  QueryResultRow[]  getcitiesList(String query1,String query2){
		QueryResult citiesList;
		QueryResultRow[] citiesResult = null;
		try {
			
			citiesList = NcDirectDbQuery.executeMappedQuery(query1, "ncep", QueryLanguage.SQL);
			citiesResult= citiesList.getRows();
			//System.out.println("___________ query: "+query1);			
			if (citiesList != null &&  citiesResult!=null ){
			}
			else 
			{
				citiesList = NcDirectDbQuery.executeMappedQuery(query2, "ncep", QueryLanguage.SQL);
				citiesResult= citiesList.getRows();
			}
		}
		catch (Exception e ){
			System.out.println("___________ Error in getcitiesList() of citiesPointData: "+e.getMessage());
		}
		return citiesResult ; 
	} 
	
	public void retrieveNearestPoint()
	{

		nearestPoint.setLat(latitude);
		nearestPoint.setLon(longitude);



	}

	public String calculateNearestPoint(Coordinate co, DisplayOptions displayOption) 
	throws VizException, RuntimeException, IOException  {

		if (co.x > 180.0 || co.x < -180.0 || co.y > 90.0 || co.y < -90.0)
			return null;// Check for invalid Coordinate

		nameIdQuery(co);//Retrieve all points within the envelope
		if (pointList.size() < 1) return null;
		String output= computeDistance( displayOption);
		//the below line to be deleted
//		output=nearestPoint.getName();
		
		return output;

	}

	public String computeDistance(DisplayOptions displayOption){
		/*
		 * Compute distance (in meter) of the nearest point
		 */
		int index = -1;
		double distance = -999.99;

		for (int i = 0; i < pointList.size(); i++) {
			GeodeticCalculator gc = new GeodeticCalculator(
					DefaultEllipsoid.WGS84);
			gc.setStartingGeographicPoint(latitude, longitude);
			gc.setDestinationGeographicPoint(pointList.get(i).getLon(), pointList.get(i).getLat());            
			double dist = 0;
			dist = gc.getOrthodromicDistance();
			
			if (i == 0) {
				distance = dist;
				index = 0;
			} else {
				if (distance > dist ) {
					distance = dist;
					index = i;
				}
			}            
		}
		if(distance>0){
			computeDirection(index,displayOption);
			nearestPoint.setDistanceInMeter((int)distance);
			nearestPoint.setName(pointList.get(index).getName());
			
			int rounding = Integer.valueOf(LocatorTool.ROUNDING_OPTIONS[0]);
			if (displayOption.getRoundingToNearest() != null) {
				rounding = displayOption.getRoundingToNearest();
			}

			String distanceOuput = LocatorTool.distanceDisplay(distance, 
					rounding, displayOption.getDistanceUnit());

			String dirOutput = LocatorTool.directionDisplay(nearestPoint.getDir(),
					displayOption.getDirectionUnit());
			//System.out.println("######### citiesPointData: dis/dir: "+displayOption.getDistanceUnit()+"/"+displayOption.getDirectionUnit());
			return getPointDataText(distanceOuput,dirOutput, index);//distanceOuput + " " + dirOutput + " " + pointList.get(index).getName();
		}else {
			return getPointDataText("","", index);//pointList.get(index).getName();
		} 

	}
	
	private String getPointDataText(String dis, String dir, int index){
		
		StringBuilder sb = new StringBuilder();
		
		if(dis != null && ! dis.isEmpty())
			sb.append(dis).append(" ");
		
		if(dir != null && ! dir.isEmpty())
			sb.append(dir).append(" ");
		
		if(LocatorTool.STATIONDISPLAY_OPTIONS[0].equalsIgnoreCase( locator.getDisplayOptions().getDisplayAttribute()) )
			sb.append(pointList.get(index).getName());
		else
			sb.append(pointList.get(index).getStateID());
		
		return sb.toString();
	}

	/*
	 * Compute direction, and format output string
	 */
	public void computeDirection(int index,DisplayOptions dispOption){
		GeodeticCalculator gc = new GeodeticCalculator(
				DefaultEllipsoid.WGS84);
		gc.setStartingGeographicPoint(pointList.get(index).getLon(), 
				pointList.get(index).getLat());
		gc.setDestinationGeographicPoint(latitude, longitude);        
		double direction = gc.getAzimuth();
		if ( direction < 0 )
			direction = 360 + direction;
		nearestPoint.setDir(direction);

	} 

	/**
	 * getter for LocatorPointData List;
	 * used in Seek.
	 * @return
	 */
	public List<LocatorPointData> getPointList(){
		return pointList;
	}
	
	public void  nameIdQuery(double lat,double lon, String dbtbl)
	throws DataAccessLayerException {
		QueryResultRow[] citiesResult = null;
		double corr = 0.2;
		pointList = new ArrayList<LocatorPointData>();

		double minLat= lat-corr;
		double maxLat =lat+corr;
		double minLon=lon-corr*5;
		double maxLon=lon+corr*5;
		
		query1="Select stid,name,lat,lon " +
		"FROM "+dbtbl+" WHERE lat BETWEEN "+
		minLat+" AND "+maxLat+" AND lon BETWEEN "+
		minLon+" AND "+maxLon;;
		
		query2="Select stid,name,lat,lon " +
		"FROM "+dbtbl+"  WHERE lat BETWEEN "+
		minLat+" AND "+maxLat;
	
		if (citiesResult == null)  
			citiesResult = getcitiesList(query1,query2);
		
		for(QueryResultRow rows:citiesResult){
			LocatorPointData pointData = new LocatorPointData();

			pointData.setName((String)rows.getColumn(1));
			pointData.setLat((Double) rows.getColumn(2));
			pointData.setLon((Double) rows.getColumn(3));
			pointList.add(pointData);
		}
		
	}

//------------------------------------------2011-05-03: STRtree returns a List NOT a List<LocatorPointData>
	
	private List rawPointList;
	
	private Coordinate loc;
	
	private static final double DIST = 1.0;
	
	public void setLatLon(Coordinate co){		
		longitude = co.y;
		latitude = co.x;	
		
		loc = co;
	}	
	
	//ONLY called after setLocator() & setLatLon()
	public void setRawPointList(List l){	
		if(l == null){
			
			String name = locator.getLocatorName();
			com.vividsolutions.jts.geom.Envelope searchEnv = new com.vividsolutions.jts.geom.Envelope(loc.x-DIST, loc.x+DIST, loc.y-DIST, loc.y+DIST);
			
			rawPointList = gov.noaa.nws.ncep.viz.ui.locator.util.LocatorInfo.LOCATORNAME_STRTREE_MAP.get(name).query(searchEnv);
		
		}else{
			rawPointList = l;
		}
	}
	
	public String calculateNearestPoint2(Coordinate co, DisplayOptions displayOption) 
									throws VizException, RuntimeException, IOException  {

		if (co == null || co.x > 180.0 || co.x < -180.0 || co.y > 90.0 || co.y < -90.0)
			return null;// Check for invalid Coordinate

		String output= computeDistance2( displayOption);
		
		return output;
	}
	
	public String computeDistance2(DisplayOptions displayOption){
		/*
		 * Compute distance (in meter) of the nearest point
		 */
		int index = -1;
		double distance = -999.99;
		
				 

		for (int i = 0; i < rawPointList.size(); i++) {
			
			GeodeticCalculator gc = new GeodeticCalculator(	DefaultEllipsoid.WGS84);
			gc.setStartingGeographicPoint(latitude, longitude);
			gc.setDestinationGeographicPoint(
					((LocatorPointData)rawPointList.get(i)).getLon(), 
					((LocatorPointData)rawPointList.get(i)).getLat()); 
			
			double dist = 0;
			dist = gc.getOrthodromicDistance();
			
			if (i == 0) {
				distance = dist;
				index = 0;
			} else {
				if (distance > dist ) {
					distance = dist;
					index = i;
				}
			}            
		}
		
		if(distance>0){
			
			computeDirection2(index,displayOption);
			nearestPoint.setDistanceInMeter((int)distance);
			nearestPoint.setName(((LocatorPointData)rawPointList.get(index)).getName());
			
			int rounding = Integer.valueOf(LocatorTool.ROUNDING_OPTIONS[0]);
			if (displayOption.getRoundingToNearest() != null) {
				rounding = displayOption.getRoundingToNearest();
			}

			String distanceOuput = LocatorTool.distanceDisplay(distance, rounding, displayOption.getDistanceUnit());
			String dirOutput = LocatorTool.directionDisplay(nearestPoint.getDir(), displayOption.getDirectionUnit());

			return getPointDataText2(distanceOuput,dirOutput, index);
		}else {
			return getPointDataText2("","", index);
		} 

	}
	
	private String getPointDataText2(String dis, String dir, int index){
		
		StringBuilder sb = new StringBuilder();
		
		if(index < 0)//2011-05-03
			return sb.toString();
		
		if(dis != null && ! dis.isEmpty())
			sb.append(dis).append(" ");
		
		if(dir != null && ! dir.isEmpty())
			sb.append(dir).append(" ");
		
		if(LocatorTool.STATIONDISPLAY_OPTIONS[0].equalsIgnoreCase( locator.getDisplayOptions().getDisplayAttribute()) )
			sb.append(((LocatorPointData)rawPointList.get(index)).getName());
		else
			sb.append(((LocatorPointData)rawPointList.get(index)).getStateID());
		
		return sb.toString();
	}

	/*
	 * Compute direction, and format output string
	 */
	public void computeDirection2(int index,DisplayOptions dispOption){
		
		GeodeticCalculator gc = new GeodeticCalculator(	DefaultEllipsoid.WGS84);
		gc.setStartingGeographicPoint(
				((LocatorPointData)rawPointList.get(index)).getLon(), 
				((LocatorPointData)rawPointList.get(index)).getLat());
		gc.setDestinationGeographicPoint(latitude, longitude); 
		
		double direction = gc.getAzimuth();
		if ( direction < 0 )
			direction = 360 + direction;
		
		nearestPoint.setDir(direction);
	}

}
