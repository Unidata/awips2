/*
 * InterleaveMappingGenerator
 * 
 * Date created 19 AUGUST 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.util.LinkedHashMap;
import java.util.TreeMap;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * Generates a mapping of points on one line to corresponding points on a second line.
 * This mapping is used by the LinearInterpolator to calculate points for intermediate lines.
 * 
 * This algorithm maps the first point in each line together as well as the last point in each line.
 * For each initial intermediate point, it's fractional distance (percent) along the Line is calculated and the point
 * and mapped to a new point on the opposite Line that is at the same fractional distance.
 * The mapping points are stored in a LinkedHashMap so that the order of the points 
 * is maintained from the beginning to the end of the lines.
 * @author sgilbert
 *
 */
public class InterleaveMappingGenerator implements IMappingGenerator{

	Coordinate[] fromLine = null;
	Coordinate[] toLine = null;
	Coordinate[] initialFromPts = null;
	Coordinate[] initialToPts = null;
	
	/**
	 * Constructor accepting two lines and an initial set of mapping points for each.
	 * @param fromLine Coordinates defining the first line used for distance calculation al
	 * along the line.  Can possibly be smoothed points.
	 * @param toLine Coordinates defining the second line used for distance calculation al
	 * along the line.  Can possibly be smoothed points.
	 * @param initialFromPts Initial points defining the first line used for mapping points
	 * @param initialToPts Initial points defining the second line used for mapping points
	 */
	public InterleaveMappingGenerator(Coordinate[] fromLine,
			Coordinate[] toLine, Coordinate[] initialFromPts,
			Coordinate[] initialToPts) {
		
		this.fromLine = fromLine;
		this.toLine = toLine;
		this.initialFromPts = initialFromPts;
		this.initialToPts = initialToPts;
	}

	/**
	 * @return the fromLine Coordinates
	 */
	public Coordinate[] getFromLine() {
		return fromLine;
	}

	/**
	 * @param fromLine the fromLine Coordinates to set
	 */
	public void setFromLine(Coordinate[] fromLine) {
		this.fromLine = fromLine;
	}

	/**
	 * @return the toLine Coordinates
	 */
	public Coordinate[] getToLine() {
		return toLine;
	}

	/**
	 * @param toLine the toLine Coordinates to set
	 */
	public void setToLine(Coordinate[] toLine) {
		this.toLine = toLine;
	}

	/**
	 * @return the initialFromPts
	 */
	public Coordinate[] getInitialFromPts() {
		return initialFromPts;
	}

	/**
	 * @param initialFromPts the initialFromPts to set
	 */
	public void setInitialFromPts(Coordinate[] initialFromPts) {
		this.initialFromPts = initialFromPts;
	}

	/**
	 * @return the initialToPts
	 */
	public Coordinate[] getInitialToPts() {
		return initialToPts;
	}

	/**
	 * @param initialToPts the initialToPts to set
	 */
	public void setInitialToPts(Coordinate[] initialToPts) {
		this.initialToPts = initialToPts;
	}
	
	/**
	 * Generate the set of mapped coordinate pairs
	 */
	public LinkedHashMap<Coordinate,Coordinate> generateMappingPoints() {
		
		/*
		 * check to see if everything is set
		 */
		if ( fromLine==null || initialFromPts==null || toLine==null || initialToPts==null ) 
			throw new IllegalStateException("All attributes must be non null");
		
		LinkedHashMap<Coordinate,Coordinate> map = new LinkedHashMap<Coordinate,Coordinate>();
		
		/*
		 * create a LengthIndexedLine for both input lines
		 */
		GeometryFactory gf = new GeometryFactory();
		LineString ls = gf.createLineString( fromLine );
		LengthIndexedLine fromLil = new LengthIndexedLine( ls );
		ls = gf.createLineString( toLine );
		LengthIndexedLine toLil = new LengthIndexedLine( ls );
		
		/*
		 * create a map that will hold the first line's coordinates along with their 
		 * fractional distance from the beginning of the line.  Add both starting and 
		 * end point of the first line
		 */
		TreeMap<Double,Coordinate> fromMap = new TreeMap<Double,Coordinate>();
		fromMap.put(new Double(0.0), initialFromPts[0] );
		fromMap.put(new Double(1.0), initialFromPts[initialFromPts.length-1] );
		
		/*
		 * create a map that will hold the second line's coordinates along with their 
		 * fractional distance from the beginning of the line.  Add both starting and 
		 * end point of the second line
		 */
		TreeMap<Double,Coordinate> toMap = new TreeMap<Double,Coordinate>();
		toMap.put(new Double(0.0), initialToPts[0] );
		toMap.put(new Double(1.0), initialToPts[initialToPts.length-1] );

		/*  
		 * For each internal mapping point (not end point) on the FIRST line, 
		 * calculate its fractional distance from the beginning.  Then calculate
		 * the coordinate the same fractional distance along the SECOND line, and
		 * add both their respective distance,Coordinate maps
		 */
		for ( int j=1; j < initialFromPts.length-1; j++ ) {
			double pct = fromLil.indexOf(initialFromPts[j]) / fromLil.getEndIndex();
			fromMap.put(new Double(pct), initialFromPts[j]);
			double index = pct * toLil.getEndIndex();
			toMap.put(new Double(pct), toLil.extractPoint(index) );
		}
		
		/*  
		 * For each internal mapping point (not end point) on the SECOND line, 
		 * calculate its fractional distance from the beginning.  Then calculate
		 * the coordinate the same fractional distance along the FIRST line, and
		 * add both their respective distance,Coordinate maps
		 */
		for ( int j=1; j < initialToPts.length-1; j++ ) {
			double pct = toLil.indexOf(initialToPts[j]) / toLil.getEndIndex();
			toMap.put(new Double(pct), initialToPts[j]);
			double index = pct * fromLil.getEndIndex();
			fromMap.put(new Double(pct), fromLil.extractPoint(index) );
		}
		
		/*
		 * Each distance,Coordinate map should contain Coordinates for the same distances
		 * in order at this point.  Get corresponding coordinates for each distance and
		 * map them together as a pair.
		 */
		for ( Double key : fromMap.keySet() ) {
			Coordinate first = fromMap.get(key);
			Coordinate second = toMap.get(key);
			map.put( first, second );
		}
		
		return map;
	}
	
	
	
}
