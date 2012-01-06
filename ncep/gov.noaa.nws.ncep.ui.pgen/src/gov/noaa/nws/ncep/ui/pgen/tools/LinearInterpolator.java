/*
 * LinearInterpolator
 * 
 * Date created: 19 AUGUST 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.util.ArrayList;
import java.util.LinkedHashMap;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineSegment;

/**
 * This class is used as part of the PgenInterpolator.  It accepts an ordered map of Coordinate
 * pairs that map points on the first line/poly to points on the second line/poly. The interpolation 
 * is performed along every line segment defined by each of the Coordinate pairs.
 * That is, the number of Coordinates returned by the interpolator is the same as the number of
 * input Coordinate pairs.
 * @author sgilbert
 *
 */
public class LinearInterpolator {

	/*
	 * holds the ordered list of LineSegments
	 */
	private ArrayList<LineSegment> lines = null;

	/**
	 * Constructor accepting a mapping of Coordinate pairs
	 */
	public LinearInterpolator( LinkedHashMap<Coordinate,Coordinate> pointMap ) {
		
		lines = new ArrayList<LineSegment>();
		for ( Coordinate pt1 : pointMap.keySet() ) {
			lines.add( new LineSegment( pt1, pointMap.get(pt1) ) );
		}
		
	}

	/**
	 * Creates the list of coordinates that are at the specified fractional 
	 * distance from the first Coordinate to the second Coordinate in each 
	 * LineSegment defined by the Coordinate pair mapping.
	 * 
	 * @param fraction fractional distance from the first set of Coordinates to the second
	 * @return list of interpolated Coordinates
	 */
	public ArrayList<Coordinate> interpolate ( double fraction ) {
		
		ArrayList<Coordinate> newpts = new ArrayList<Coordinate>();
		for ( LineSegment ls : lines ) {
			newpts.add(  ls.pointAlong(fraction) );
		}
		
		return newpts;
	}
	
}
