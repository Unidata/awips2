/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.SurfaceStationPointData
 * 
 * This java class performs the surface station locator functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 11/08/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

public class SurfaceStationPointData {
	//private Log logger = LogFactory.getLog(getClass());
	public static final int DEFAULT_LATLON=999;
	private static List<PointData> pointList;
	private static double latitude,longitude;
	public SurfaceStationPointData() {
		// TODO Auto-generated constructor stub
	}

	public static class PointData {

		private String name = null;
		private double lat  = -999.99;
		private double lon  = -999.99;
		private double dir  = -999.99;
		private int distanceInMeter = -999;

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public PointData() {		
		}
		
		public PointData( PointData pd ) {
			name = pd.name;
			lat = pd.lat;
			lon = pd.lon;
			dir = pd.dir;
			distanceInMeter = pd.distanceInMeter;		
		}
		
		public double getLat() {
			return lat;
		}
		public void setLat(double lat) {
			this.lat = lat;
		}

		public double getLon() {
			return lon;
		}
		public void setLon(double lon) {
			this.lon = lon;
		}

		public int getDistanceInMeter() {
			return distanceInMeter;
		}
		public void setDistanceInMeter(int distanceInMeter) {
			this.distanceInMeter = distanceInMeter;
		}
		
		public double getDir() {
			return dir;
		}
		public void setDir(double dir) {
			this.dir = dir;
		}
	}
	
	private static List<PointData>  stationIdQuery(double lat,double lon)
	throws DataAccessLayerException {
		String query1,query2;
		QueryResultRow[] stnResults = null;
		double corr = 0.2;
		List<PointData> rtnPointList = new ArrayList<PointData>();

		double minLat= lat-5*corr;
		double maxLat =lat+ 5*corr;
		double minLon=lon-corr*5;
		double maxLon=lon+corr*5;
		query1="Select station_id,latitude,longitude " +
			"FROM stns.sfstns WHERE latitude BETWEEN "+
			minLat+" AND "+maxLat+" AND longitude BETWEEN "+
			minLon+" AND "+maxLon;
		query2="Select station_id,latitude,longitude " +
			"FROM stns.sfstns  WHERE latitude BETWEEN "+
			minLat+" AND "+maxLat;
	
		stnResults = getStnList(query1,query2);
		if(stnResults!= null){
			for(QueryResultRow rows:stnResults){
				PointData pointData = new PointData();

				pointData.setName((String)rows.getColumn(0));
				pointData.setLat((Double) rows.getColumn(1));
				pointData.setLon((Double) rows.getColumn(2));
				rtnPointList.add(pointData);
			}
		}
		return rtnPointList;
		
	}

	
	private  static QueryResultRow[]  getStnList(String query1,String query2){
		QueryResult stnList=null;
		QueryResultRow[] stnRowResults = null;
		try {
			
			stnList = NcDirectDbQuery.executeMappedQuery(query1, "ncep", QueryLanguage.SQL);
			if(stnList != null){
				stnRowResults= stnList.getRows();
			}
			else {
				
				stnList = NcDirectDbQuery.executeMappedQuery(query2, "ncep", QueryLanguage.SQL);
				if(stnList != null)
					stnRowResults= stnList.getRows();
			}
			
		}
		catch (Exception e ){
			System.out.println("db exception!");
		}
		return stnRowResults ; 
	} 
	

	
	private static int getNearestPointStnIndex(){
		/*
		 * Compute distance (in meter) of the nearest point
		 */
		int index = -1;
		double distance = -999.99;
		int i=0;
		for (PointData point:  pointList){
			GeodeticCalculator gc = new GeodeticCalculator(
					DefaultEllipsoid.WGS84);
			gc.setStartingGeographicPoint( longitude,latitude);
			gc.setDestinationGeographicPoint(point.getLon(), point.getLat());            
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
			i++;
		}

		return  index;

	}

	public static String calculateNearestPoint(Coordinate co)  {

		if (co.x > 180.0 || co.x < -180.0 || co.y > 90.0 || co.y < -90.0)
			return null;// Check for invalid Coordinate

		try {
			latitude = co.y;
			longitude =  co.x;
			pointList = stationIdQuery(latitude , longitude);
		} catch (DataAccessLayerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}//Retrieve all points within the envelope
		if (pointList == null || pointList.size() ==0 ) 
			return null;
		int index = getNearestPointStnIndex();

		String output = pointList.get(index).getName();
		//update input coordinate of picked stn so Nsharp can use it
		co.x = pointList.get(index).getLon();
		co.y = pointList.get(index).getLat();
		return output;

	}
	public static Coordinate getStnCoordinate(String stnName){
		Coordinate co = new Coordinate(DEFAULT_LATLON,DEFAULT_LATLON);
		//Chin: station id length in sfstns table is 8 chars. It fills whole 8 chars with space after real station id.
		// in order to make a successful query from user entered station id. We have to make sure using a 8 chars string
		// with space stuffed at end.
		String fixedLengthStnName = stnName + "        ";
		fixedLengthStnName = fixedLengthStnName.substring(0, 8);
		String query="Select latitude,longitude " +
			"FROM stns.sfstns WHERE station_id = '"+fixedLengthStnName+"'";
		
		
		List<Object[]> list = null;
		try {
			list = NcDirectDbQuery.executeQuery( query, "ncep", QueryLanguage.SQL);
			if(list != null && list.size()>0){
				//we only care for one and only first one station
				Object[] obj = list.get(0);
				co.y = (Double)obj[0];
				co.x = (Double)obj[1];
			}
		}
		catch (Exception e ){
			System.out.println("-----DB exception at getStnCoordinate: "+e.getMessage());			
		}
		
		
		return co;
	}
}
