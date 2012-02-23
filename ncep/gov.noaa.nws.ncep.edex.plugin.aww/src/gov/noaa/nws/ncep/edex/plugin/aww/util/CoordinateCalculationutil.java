package gov.noaa.nws.ncep.edex.plugin.aww.util;

import java.util.ArrayList;
import java.util.List;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.geom.Coordinate;

public class CoordinateCalculationutil {
	
	public static Coordinate getExtrapPoint(Coordinate startPointCoordinate, double direction, double distance) {
		System.out.println("===========within CoordinateCalculationutil.getExtrapPoint: "); 
		System.out.println("==============================  startPointCoordinate.x="+startPointCoordinate.x); 
		System.out.println("==============================  startPointCoordinate.y="+startPointCoordinate.y); 
		System.out.println("==============================  direction="+direction); 
		System.out.println("==============================  distance="+distance); 
		GeodeticCalculator gc = new GeodeticCalculator( DefaultEllipsoid.WGS84 );
		Coordinate calculatedCoordinate = null; 
		try {
			/*
			 * Find a point "distance" away at the "direction" of the given point
			 */
			gc.setStartingGeographicPoint(startPointCoordinate.x , startPointCoordinate.y);                   
			System.out.println("===========within CoordinateCalculationutil.getExtrapPoint:  line 1 "); 

			gc.setDirection(direction, distance);
			System.out.println("===========within CoordinateCalculationutil.getExtrapPoint:  line 2 "); 

			java.awt.geom.Point2D newPoint = gc.getDestinationGeographicPoint();      	            
			System.out.println("===========within CoordinateCalculationutil.getExtrapPoint:  line 3 "); 
			calculatedCoordinate = new Coordinate(newPoint.getX(), newPoint.getY());
		} catch(IllegalArgumentException iae) {
			System.out.println("====== inside CoordinateCalculationutil.getExtrapPoint(...), IllegalArgumentException is caught, error="+iae.getMessage()); 
		} catch(IllegalStateException ise) {
			System.out.println("====== inside CoordinateCalculationutil.getExtrapPoint(...), IllegalStateException is caught, error="+ise.getMessage()); 
		}
		
		System.out.println("===========within CoordinateCalculationutil.getExtrapPoint, BEFORE return: "); 
		System.out.println("==============================  calculatedCoordinate.x="+calculatedCoordinate.x); 
		System.out.println("==============================  calculatedCoordinate.y="+calculatedCoordinate.y); 
		return calculatedCoordinate;  
	}

	public static List<Coordinate> getExtrapPointList(List<Coordinate> startPointCoordinateList, double direction, double distance) {
		List<Coordinate> extrapCoordianteList = new ArrayList<Coordinate>(); 
		if(startPointCoordinateList != null) {
			for(Coordinate eachStartPointCoordinate : startPointCoordinateList) {
				Coordinate newPointCoordinate = getExtrapPoint(eachStartPointCoordinate, direction, distance); 
				extrapCoordianteList.add(newPointCoordinate); 
			}
		}
		return extrapCoordianteList; 
	}
}
