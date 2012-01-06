/*
 * DefaultPolyMappingGenerator
 * 
 * Date created 19 AUGUST 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PGenException;

import java.util.LinkedHashMap;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Generates a mapping of points on one polygon to corresponding points on a second polygon.
 * This mapping is used by the LinearInterpolator to calculate points for intermediate polygons.
 * 
 * This algorithm assumes that the starting points for each polygon have been somehow matched up
 * ( e.g. both start with the right-most coordinate of the polygon), AND that the coordinates 
 * of each polygon traverse in the same direction (i.e. clockwise or counter clockwise).
 * 
 * The first and last points of each polygon are initially mapped together.  The rest of the 
 * points are mapped together based on their fractional distance from the beginning of the
 * current mapped segment.  The two coordinates that have the minimal fractional distance 
 * difference are mapped together.  This, in effect, produces two segments from the original, 
 * and this distance differencing is used recursively on each segment and so on, until all the
 * coordinates on the polygon with the fewest number are mapped.  
 * 
 * The polygon with the largest number of coordinates will still have some points unmapped.  For each
 * unmapped point in the source polygon, a new point is created on the other polygon at the same 
 * fractional distance in the corresponding segment, and then mapped with the source polygon point.
 *
 * The mapping points are stored in a LinkedHashMap so that the order of the points 
 * is maintained from the beginning to the end of the polygons.
 * @author sgilbert
 *
 */
public class DefaultPolyMappingGenerator extends AbstractPolyMappingGenerator{

	Coordinate[] fromPoly = null;
	Coordinate[] toPoly = null;
	
	/**
	 * Constructor accepting two Polygons
	 * @param fromPoly Coordinates defining the first polygon
	 * @param toPoly Coordinates defining the second polygon
	 */
	public DefaultPolyMappingGenerator(Coordinate[] fromPoly,
			Coordinate[] toPoly ) {
		
		this.fromPoly = fromPoly;
		this.toPoly = toPoly;
	}

	/**
	 * @return the fromPoly Coordinates
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
		
		/*
		 * the polygons must not be null
		 */
		if ( fromPoly==null || toPoly==null ) 
			throw new IllegalStateException("All attributes must be non null");

		LinkedHashMap<Coordinate,Coordinate> map= new LinkedHashMap<Coordinate,Coordinate>() ;
		
		/*
		 * create a map with all the coordinates of the first polygon as the keys
		 */
		LinkedHashMap<Coordinate,Coordinate> fromMap = new LinkedHashMap<Coordinate,Coordinate>();
		for ( Coordinate pt : fromPoly ) fromMap.put(pt, null); 
		
		/*
		 * create another map with all the coordinates of the second polygon as the keys
		 */
		LinkedHashMap<Coordinate,Coordinate> toMap = new LinkedHashMap<Coordinate,Coordinate>();
		for ( Coordinate pt : toPoly ) toMap.put(pt, null); 
		
		/*
		 * map first points of each polygon together to each other.
		 */
		fromMap.put(fromPoly[0], toPoly[0]);
		toMap.put(toPoly[0], fromPoly[0]);
		
		/*
		 * Set the polygon with the most points as the primary polygon
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
		 * get the first coordinate of the primary polygon
		 */
		Coordinate[] pCoords = primary.keySet().toArray(new Coordinate[primary.size()]);

		try {
			/*
			 * perform the initial mapping of this section of the polygon.  In this case,
			 * this section is the whole polygon.
			 */
			mapSection(primary, secondary, pCoords[0] );

			/*
			 * calculate corresponding coordinates on the secondary polygon for any
			 * points on the primary polygon that were left unmapped.
			 */
			mapRemaining( primary, secondary);
		}
		catch (PGenException pe ) {
			System.out.println(pe.getMessage());
			map.clear();
			return map;    // return empty map
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
