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


import gov.noaa.nws.ncep.viz.common.LocatorUtil;
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
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.strtree.STRtree;


public class PointDataSource  {
	
	private STRtree pointDataSortTree = new STRtree();
	
	private LocatorDataSource locDataSrc; 

	public PointDataSource( LocatorDataSource lds ) { 
		locDataSrc = lds;
	}
	
	// TODO : we could add a flag to not load all the data in memory and instead
	// query the database each time.
	public void loadData( ) throws VizException {

		// Order is important here since it determines order in the
		// queryResults
		//
		String query = "select latitude,longitude," +
						locDataSrc.getDbFieldName() + " from " +
						locDataSrc.getDbTableName();
		
		QueryResult queryList;
		QueryResultRow[] queryResult = null;		

		//query the DB
		try {				
			queryList = NcDirectDbQuery.executeMappedQuery( query, "ncep", QueryLanguage.SQL);
			queryResult= queryList.getRows();				
		}
		catch (Exception e ){
			System.out.println("___ Error: populateTrees() of LoadPointData: "+e.getMessage());	return;
		}

		Object[] os = null;			

		// populate Tree/pointList
		for(QueryResultRow rows : queryResult){

			os = rows.getColumnValues();	
			//System.out.println( rows.getColumn(2).toString() );
			
			if(os == null || os.length == 0) 
				continue;

			PointData pointData = new PointData();

			if(  os[0] instanceof Double ) {
				pointData.setLat((Double)os[0]);
			}					
			if( os[1] instanceof Double ) { 					
				pointData.setLon((Double)os[1]);
			}
			if( os[2] instanceof String) {
				pointData.setName((String)os[2]);
			}

			Coordinate c = new Coordinate(pointData.getLon(), pointData.getLat());
			Envelope env = new Envelope(c.x-DIST, c.x+DIST, c.y-DIST, c.y+DIST);

			pointDataSortTree.insert(env, pointData);
		}
	}
	
	
	private static final double DIST = 1.0;
		
	public PointData calculateNearestPoint2(Coordinate loc ) // LocatorDisplayAttributes displayOption) 
									throws VizException {

		if (loc == null || loc.x > 180.0 || loc.x < -180.0 || loc.y > 90.0 || loc.y < -90.0) {
//			throw new VizException("Location out of range");
			return null;// Check for invalid Coordinate
		}

		List rawPointList=null;
		double searchRange = DIST;
		
		do {
			searchRange *= 2;

			if( searchRange > 130 ) {
				break;
			}
			
			Envelope searchEnv = new Envelope( 
					loc.x-searchRange, loc.x+searchRange, loc.y-searchRange, loc.y+searchRange);
			rawPointList = pointDataSortTree.query(searchEnv);
			
//			if( searchRange > 1 ) {
//				System.out.println("search range is "+ searchRange );
//			}
		} while( rawPointList.isEmpty() );

		/*
		 * Compute distance (in meter) of the nearest point
		 */
		PointData nearestPoint = null;

		for (int i = 0; i < rawPointList.size(); i++) {
			
			GeodeticCalculator gc = new GeodeticCalculator(	DefaultEllipsoid.WGS84);
			gc.setStartingGeographicPoint( 
					((PointData)rawPointList.get(i)).getLon(), 
					((PointData)rawPointList.get(i)).getLat() );
			gc.setDestinationGeographicPoint(loc.x, loc.y); 
			
			double dist = 0;
			dist = gc.getOrthodromicDistance();
			
			if( nearestPoint == null ||
				dist < nearestPoint.getDistanceInMeter() ) {
				
				nearestPoint = new PointData( (PointData)rawPointList.get(i) );
				nearestPoint.setDistanceInMeter( (int)(dist+.5) );
				
				double direction = gc.getAzimuth();
				if( direction < 0 ) {
					direction = 360 + direction;
				}
				nearestPoint.setDir( direction );
			}            
		}
		
		return nearestPoint;		
	}	
}
