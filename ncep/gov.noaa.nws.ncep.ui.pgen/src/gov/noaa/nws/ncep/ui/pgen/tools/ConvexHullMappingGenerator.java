/*
 * DefaultPolyMappingGenerator
 * 
 * Date created 19 AUGUST 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PGenException;

import java.util.ArrayList;
import java.util.LinkedHashMap;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;

/**
  * Generates a mapping of points on one polygon to corresponding points on a second polygon.
 * This mapping is used by the LinearInterpolator to calculate points for intermediate polygons.
 * 
 * This algorithm assumes that the starting points for each polygon have been somehow matched up
 * ( e.g. both start with the right-most coordinate of the polygon), AND that the coordinates 
 * of each polygon traverse in the same direction (i.e. clockwise or counter clockwise).
 * 
 * At first, a convex hull is created from the two polygons. Each coordinate of the convex hull is
 * checked to determine which polygon it belongs to.  If the next coordinate belongs to the other
 * polygon, then those two points are mapped together.  This will create at least two mapped 
 * coordinate pairs, unless one polygon is fully contained within the other. If this happens, 
 * nothing can be done, and an empty set of mapping points is returned.
 * 
 * For each initial polygon segment created by the initial mapped points, any intermediate 
 * points are mapped together based on their fractional distance from the beginning of the
 * current mapped segment.  The two coordinates that have the minimal fractional distance 
 * difference are mapped together.  This, in effect, produces two segments from the original, 
 * and this distance differencing is used recursively on each segment and so on, until all the
 * coordinates on the polygon with the fewest number are mapped.  
 * 
 * Each polygon may still have some points unmapped.  For each
 * unmapped point in the source polygon, a new point is created on the other polygon at the same 
 * fractional distance in the corresponding segment, and then mapped with the source polygon point.
 * The mapping points are stored in a LinkedHashMap so that the order of the points 
 * is maintained from the beginning to the end of the polygons.
 * @author sgilbert
 *
 */
public class ConvexHullMappingGenerator extends AbstractPolyMappingGenerator{

	Coordinate[] fromPoly = null;
	Coordinate[] toPoly = null;
	
	/**
	 * Constructor accepting two polygons
	 * @param fromPoly
	 * @param toPoly
	 */
	public ConvexHullMappingGenerator(Coordinate[] fromPoly,
			Coordinate[] toPoly ) {
		
		this.fromPoly = fromPoly;
		this.toPoly = toPoly;
	}

	/**
	 * @return the fromPoly coordinates
	 */
	public Coordinate[] getFromPoly() {
		return fromPoly;
	}

	/**
	 * @param fromPoly the fromPoly coordinates to set
	 */
	public void setFromPoly(Coordinate[] fromPoly) {
		this.fromPoly = fromPoly;
	}

	/**
	 * @return the toPoly coordinates
	 */
	public Coordinate[] getToPoly() {
		return toPoly;
	}

	/**
	 * @param toPoly the toPoly coordinates to set
	 */
	public void setToPoly(Coordinate[] toPoly) {
		this.toPoly = toPoly;
	}

	/**
	 * Generate the set of mapped coordinate pairs
	 */
	public LinkedHashMap<Coordinate,Coordinate> generateMappingPoints() {
		
		LinkedHashMap<Coordinate,Coordinate> map = new LinkedHashMap<Coordinate,Coordinate>();
		GeometryFactory gf = new GeometryFactory();
		
		/*
		 * the polygons must not be null
		 */
		if ( fromPoly==null || toPoly==null ) 
			throw new IllegalStateException("All attributes must be non null");
		
		/*
		 * create a map with all the coordinates of the first polygon as the keys
		 */
		LinkedHashMap<Coordinate,Coordinate> fromMap = new LinkedHashMap<Coordinate,Coordinate>();
		for ( Coordinate pt : fromPoly ) fromMap.put(pt, null); 
		
		/*
		 * create a map with all the coordinates of the second polygon as the keys
		 */
		LinkedHashMap<Coordinate,Coordinate> toMap = new LinkedHashMap<Coordinate,Coordinate>();
		for ( Coordinate pt : toPoly ) toMap.put(pt, null); 
		
		/*
		 * create convex hull around the two polygons
		 */
		LinearRing[] rings = new LinearRing[] { gf.createLinearRing(fromPoly), gf.createLinearRing(toPoly) };
		GeometryCollection poly = new GeometryCollection(rings, gf);
		Geometry hull = poly.convexHull();

		/*
		 * traverse each coordinate in the convex hull, and map together any two consecutive
		 * points that belong to different polygons.  If they belong to the same polygon, ignore them.
		 */
		Coordinate[] chull = hull.getCoordinates();
		boolean prev = rings[0].contains( gf.createPoint(chull[0]) );
		for ( int i=1; i < chull.length; i++ ) {
			Point pt = gf.createPoint( chull[i] );
			boolean current = rings[0].contains(pt);
			if ( prev && !current ) {
				fromMap.put(chull[i-1], chull[i]);
				toMap.put(chull[i], chull[i-1]);
			}
			if ( !prev && current ) {
				fromMap.put(chull[i], chull[i-1]);
				toMap.put(chull[i-1], chull[i]);
			}
			prev = current;
		}
		
		/*
		 * Make polygon with most points primary
		 */
		LinkedHashMap<Coordinate,Coordinate> primary, secondary;
		if ( toPoly.length >  fromPoly.length ) {
			primary = toMap;
			secondary = fromMap;
		} else {
			primary = fromMap;
			secondary = toMap;
		}

		/*
		 * Get a list of primary polygon coordinates that have already been mapped to the
		 * secondary polygon.  If there are none, return an empty map
		 */
		ArrayList<Coordinate> clist = new ArrayList<Coordinate>();
		for ( Coordinate c : primary.keySet() ) {
			if ( primary.get(c) != null )  clist.add(c);
		}
		if ( clist.isEmpty() ) {
			map.clear();
			return map;           // return empty map
		}

		/*
		 * For each primary polygon section, map as many points as possible in them. 
		 */
		try {
			for ( Coordinate c : clist ) {
				mapSection(primary, secondary, c );
			}
			
			/*
			 *    Create new mapping points on the primary polygon, for any remaining umapped points 
			 *    in the secondary polygon.
			 */
			mapRemaining( secondary, primary);
			
			/*
			 *    Create new mapping points on the secondary polygon, for any remaining umapped points 
			 *    in the primary polygon.
			 */
			mapRemaining( primary, secondary);
		}
		catch (PGenException pe) {
			System.out.println(pe.getMessage());
			map.clear();
			return map;
		}
		
		if ( toPoly.length >  fromPoly.length ) {
			/*
			 * If the "to" polygon is larger, reverse the mapping coordinates in the primary hashmap
			 */
			for ( Coordinate coord : primary.keySet() ) {
				map.put(primary.get(coord), coord);
			}
		}
		else
			map=primary;
		
		return map;
	}
	
	

}
