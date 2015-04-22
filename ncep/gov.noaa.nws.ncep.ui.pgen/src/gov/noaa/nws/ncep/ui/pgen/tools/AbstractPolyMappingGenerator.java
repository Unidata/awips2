/*
 * AbstractPolyMappingGenerator
 * 
 * Date created 19 AUGUST 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PGenException;

import java.util.HashMap;
import java.util.LinkedHashMap;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateArrays;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * Generates a mapping of points on one polygon to corresponding points on a second polygon.
 * This mapping is used by the LinearInterpolator to calculate points for intermediate polygons.
 * 
 * This abstract class provides two methods to help generate mapped coordinate pairs between
 * two polygons.  The mapSection method will try to map as many points as possible along a 
 * given mapped polygon segment.  The mapRemaining method creates new points on the second 
 * polygon for each unmapped point in the first polygon.
 * 
 * It is intended for subclasses to somehow create an initial set of mapping points for the two
 * polygons, thus defining mapped segments.  The mapSection should then be called for each segment.
 * Afterwards, the mapRemaining method should be called to ensure that all remaining unmapped 
 * points get mapped.
 * @author sgilbert
 *
 */
public abstract class AbstractPolyMappingGenerator implements IMappingGenerator{

	public abstract LinkedHashMap<Coordinate,Coordinate> generateMappingPoints();
	
	/*
	 * Points are mapped together based on their fractional distance from the beginning of the
	 * current mapped segment.  The two coordinates that have the minimal fractional distance 
	 * difference are mapped together.  This, in effect, produces two segments from the original, 
	 * and this distance differencing is used recursively on each segment and so on, until all the
	 * coordinates on the current segment with the fewest number are mapped.  
	 */
	protected void mapSection( LinkedHashMap<Coordinate,Coordinate> primary, 
			LinkedHashMap<Coordinate,Coordinate> secondary, Coordinate start) throws PGenException {
		
		/*
		 * Create Coordinate[] arrays from the keys of each HashMap
		 */
		Coordinate[] pCoords = primary.keySet().toArray(new Coordinate[primary.size()]);
		Coordinate[] sCoords = secondary.keySet().toArray(new Coordinate[secondary.size()]);
		
		/*
		 * make sure starting coordinate is mapped to another coordinate in the secondary polygon
		 */
		if ( primary.get(start) == null ) 
			throw new PGenException("mapSection: Starting coordinate is not mapped.");
		
		/*
		 * find the first coordinate after the start coordinate that is already mapped
		 * to a coordinate on the secondary polygon
		 */
		Coordinate end=null;
		Coordinate last = getPreviousCoordinate(start, pCoords);
		Coordinate current=start;
		do {
			current = getNextCoordinate(current, pCoords);
			if ( primary.get(current) !=null ) {
				end = current;
				break;
			}
		} while ( current != last );
		
		/*
		 * If no other mapped coordinate was found map the two last coordinates of
		 * each polygon together
		 */
		if ( end == null ) {
			end = last;
			Coordinate otherLast = getPreviousCoordinate(primary.get(start), sCoords);
			primary.put( end, otherLast);
			secondary.put (otherLast, end);
		}
		
		/*
		 * extract coordinates making up the current section of the primary and
		 * secondary polygon. 
		 */
		Coordinate[] section1 = extractCoordinates( pCoords, start, end);
		Coordinate[] section2 = extractCoordinates( sCoords, primary.get(start), primary.get(end));

		/*
		 * if either section has no intermediate points, then no additional mapping can be done
		 */
		if ( (section1.length<=2) || (section2.length<=2) ) return;
		
		/*
		 * if the number of intermediate points is the same for both the primary
		 * and secondary polygon, go ahead and map them one-to-one and return
		 */
		if ( section1.length == section2.length ) {
			for ( int j=1; j < section1.length-1; j++ ) {
				primary.put( section1[j], section2[j]);
				secondary.put( section2[j], section1[j]);
			}
			return;
		}
		else {
			/*
			 * if number of points in each section is unequal, find the pair of points
			 * that are closest to one another in terms of percent distance along the section,
			 * and map them together.  This mapping divides this section into two sections;
			 * call this method recursively again for the two new sections.
			 */
			GeometryFactory gf = new GeometryFactory();
			LineString ls1 = gf.createLineString(section1);
			LengthIndexedLine lil1 = new LengthIndexedLine(ls1);
			LineString ls2 = gf.createLineString(section2);
			LengthIndexedLine lil2 = new LengthIndexedLine(ls2);
			
			Coordinate new1=null, new2=null;
			double minDiff = Double.MAX_VALUE;
			for ( int j=1; j < section1.length-1; j++ ) {
				double dist1 = lil1.indexOf(section1[j]) / ls1.getLength();
				for ( int k=1; k < section2.length-1; k++ ) {
					double dist2 = lil2.indexOf(section2[k]) / ls2.getLength();
					double diff = Math.abs( dist1 - dist2);
					if ( diff < minDiff ) {
						minDiff = diff;
						new1 = section1[j];
						new2 = section2[k];
					}
				}
			}
			primary.put(new1, new2);
			secondary.put(new2, new1);
			
			mapSection(primary, secondary, start);
			
			mapSection(primary, secondary, new1);
			
		}
			
	}
	
	/*
	 * For each unmapped point in the source polygon, a new point is created on the other polygon 
	 * at the same fractional distance in the corresponding segment, and then mapped with 
	 * the source polygon point.
	 */
	protected void mapRemaining(LinkedHashMap<Coordinate, Coordinate> first,
			LinkedHashMap<Coordinate, Coordinate> second) throws PGenException {

		GeometryFactory gf = new GeometryFactory();
		LinkedHashMap<Coordinate,Coordinate> tempMap = new LinkedHashMap<Coordinate,Coordinate>();

//		if ( (start2==-1) || (end2==-1) ) {
//			throw new PGenException("mapRemaining: Could not find enough mapped points to start with");
//		}
		
		/*
		 * Create Coordinate[] arrays from the keys of each HashMap
		 */
		Coordinate[] fromCoords = first.keySet().toArray(new Coordinate[first.size()]);
		Coordinate[] toCoords = second.keySet().toArray(new Coordinate[second.size()]);
	
		/*
		 * For each coordinate in the first polygon...
		 */
		for ( int i=0; i < fromCoords.length; i++ ) {
			
			if ( first.get(fromCoords[i]) != null ) continue;   //  Coordinate already mapped to another.
			
			/*
			 * find first mapped coordinate before the current point
			 */
			Coordinate current = fromCoords[i];
			do {
				current = getPreviousCoordinate(current, fromCoords);
			} while ( first.get(current) == null );
			Coordinate start = current;
			
			/*
			 * find first mapped coordinate after the current point
			 */
			current = fromCoords[i];
			do {
				current = getNextCoordinate(current, fromCoords);
			} while ( first.get(current) == null );
			Coordinate end = current;
			
			/*
			 * extract coordinates making up the current section of the primary and
			 * secondary polygons. 
			 */
			Coordinate[] section1 = extractCoordinates( fromCoords, start, end);
			Coordinate[] section2 = extractCoordinates( toCoords, first.get(start), first.get(end));
			
			/*
			 * Create LengthIndexedLines for distance calculations
			 */
			LineString ls1 = gf.createLineString(section1);
			LengthIndexedLine lil1 = new LengthIndexedLine(ls1);
			LineString ls2 = gf.createLineString(section2);
			LengthIndexedLine lil2 = new LengthIndexedLine(ls2);
			
			/*
			 * Calculate fractional distance unmapped point is along source polygon section,
			 * and then create new point at the same fractional distance in the corresponding 
			 * segment of the second polygon.
			 */
			double dist1 = lil1.indexOf(fromCoords[i]) / ls1.getLength();
			Coordinate newone = lil2.extractPoint(dist1 * ls2.getLength() );
			tempMap.put(fromCoords[i], newone);    // save new coordinate point in a temporary map
			
		}
		
		/*
		 * Add any new coordinate pairs in the proper place in the coordinate pair maps.
		 */
		for ( Coordinate c : tempMap.keySet() ) {
			first.put(c, tempMap.get(c));
			Coordinate prev = getPreviousCoordinate(c, fromCoords);
			insertPoint(second, first.get(prev), tempMap.get(c));
			second.put(tempMap.get(c), c);    
		}
		
	}

	/*
	 * Returns the Coordinate in the position before start in the given array.  This method simulates
	 * a circular array, so that if the start coordinate is the first element, that last element
	 * of the array is returned.
	 */
	private Coordinate getPreviousCoordinate(Coordinate start, Coordinate[] coordList) {
		int length = coordList.length;
		int idx = CoordinateArrays.indexOf(start, coordList);
		idx = ( idx -1 + length ) % length;
		return coordList[idx];
	}

	/*
	 * Returns the Coordinate in the position ater start in the given array.  This method simulates
	 * a circular array, so that if the start coordinate is the last element, that first element
	 * of the array is returned.
	 */
	private Coordinate getNextCoordinate(Coordinate start, Coordinate[] coordList) {
		int length = coordList.length;
		int idx = CoordinateArrays.indexOf(start, coordList);
		idx = ( idx +1 + length ) % length;
		return coordList[idx];
	}

	/*
	 * Returns an array of coordinates containing all the coordinates between two given coordinates
	 * inclusively.  This method simulates a circular array (or polygon) in that if the requested
	 * coordinates reach the end of the array, it will wrap around to the beginning of the array until
	 * the end coordinate is reached.
	 */
	private Coordinate[] extractCoordinates(Coordinate[] coordList,
			Coordinate start, Coordinate end) {
		
		CoordinateList list = new CoordinateList();
		Coordinate current = start;
		list.add(current,true);
		do {
			current = getNextCoordinate(current, coordList);
			list.add(current,true);
		} while ( current != end );
		return list.toCoordinateArray();
	}

	/*
	 * inserts a new coordinate key into a LinkedHashMap directly after the specified 
	 * existing key.
	 */
	private void insertPoint(
			LinkedHashMap<Coordinate, Coordinate> map,
			Coordinate existing, Coordinate newpos) {
		
		//  create a new temporary map
		LinkedHashMap<Coordinate, Coordinate> newMap = new LinkedHashMap<Coordinate, Coordinate>();
		
		/*
		 * Copy each coordinate pair to the temporary map
		 */
		for ( Coordinate c : map.keySet() ) {
			newMap.put(c, map.get(c));
 		}

		// empty the original
		map.clear();
		
		/*
		 * Copy each coordinate pair back to the original map adding the
		 * new point after it finds the existing key.
		 */
		for ( Coordinate c : newMap.keySet() ) {
			map.put(c, newMap.get(c));
			if ( c.equals2D(existing) )  map.put( newpos, null);
 		}
		
	}

}
