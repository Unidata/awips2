/*
 * PgenInterpolator
 * 
 * Date created 19 AUGUST 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.CurveFitter;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.display.IMultiPoint;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateArrays;
import com.vividsolutions.jts.geom.CoordinateList;

/**
 * This class contains a public static method to generate new Drawable Elements between two given
 * elements using the specified InterpolationProperties.
 * @author sgilbert
 *
 */
public class PgenInterpolator {

	/**
	 * Generates new Drawable Elements between two given
	 * elements using the specified InterpolationProperties.
	 * @param from fist Drawable Element 
	 * @param to second Drawable Element
	 * @param props interpolation properties
	 * @param mapDescriptor used for converting lat/lon coordinates to/from pixel coordinates
	 * @return
	 */
	public static List<AbstractDrawableComponent> interpolate( DrawableElement from, DrawableElement to, 
			InterpolationProperties props, IMapDescriptor mapDescriptor ) {
		
		if ( from == null || to == null ) 
			throw new IllegalArgumentException("PgenInterpolator: Drawable elements must not be null.");
		if ( !(from instanceof ILine) || !(to instanceof ILine) ) 
			throw new IllegalArgumentException("PgenInterpolator: Drawable elements must be lines.");
		
		ArrayList<AbstractDrawableComponent> newElems = new ArrayList<AbstractDrawableComponent>();
		
		/*
		 * Convert from Lat/Lon coordinates to pixel coordinates for both elements
		 */
		double[][] fromPixel = PgenUtil.latlonToPixel(((IMultiPoint)from).getLinePoints(), mapDescriptor);
		double[][] toPixel = PgenUtil.latlonToPixel(((IMultiPoint)to).getLinePoints(), mapDescriptor);

		/*
		 * If, the elements are closed lines, reorder the points so that they are
		 * in a consistent direction.
		 */
		if ( ((ILine)from).isClosedLine() ) fromPixel = reorderPoints(fromPixel);
		if ( ((ILine)to).isClosedLine() ) toPixel = reorderPoints(toPixel);

		/*
		 * Convert original points from double[][] to Coordinate[]
		 */
		Coordinate[] fromPts = toCoordinates(fromPixel);
		Coordinate[] toPts = toCoordinates(toPixel);

		/*
		 * Apply any smoothing. if necessary 
		 */
		Coordinate[] fromLine = prepareLine( fromPixel, ((ILine)from).getSmoothFactor() );
		Coordinate[] toLine = prepareLine( toPixel, ((ILine)to).getSmoothFactor() );

		/*
		 * Generate a set of coordinate pairs that map points on the first line to 
		 * points on the second line.  These Mapping points will be used by the 
		 * LinearInterpolator to calculate the coordinates of intermediate lines.
		 */
		IMappingGenerator mapper;
		LinkedHashMap<Coordinate,Coordinate> mapping = null; 
		
		if ( !((ILine)from).isClosedLine() && !((ILine)to).isClosedLine() ) {
			
			// use the interleave mapping algorithm
			mapper = new InterleaveMappingGenerator(fromLine, toLine, fromPts, toPts);
			mapping = mapper.generateMappingPoints();
		}
		else if ( ((ILine)from).isClosedLine() && ((ILine)to).isClosedLine() ) {
			
			// try the convex hull mapping algorithm first
			mapper = new ConvexHullMappingGenerator(fromPts, toPts);
			mapping = mapper.generateMappingPoints();
			if ( mapping.isEmpty() ) {
				
				//  If convex hull algorithm not applicable ( like cases where one polygon is
				//  contained in another ), use default algorithm
				mapper = new DefaultPolyMappingGenerator(fromPts, toPts);
				mapping = mapper.generateMappingPoints();
			}
		}
		else
			return newElems;     //  return empty list 
		
		LinearInterpolator interp = new LinearInterpolator(mapping);
		
		/*
		 * calculate the coordinates for each new objects
		 * 
		 * Note - reverse the starting and ending time if starting time > ending time.
		 */
		int start = props.getStartingTime();
		int end = props.getEndingTime();
		int interval = props.getInterval();
		if ( start > end )  {
			start = -start;
			end = -end;
		}
		
		for ( int i=start+interval; i < end; i+=interval) {

			//  calculate the fractional distance of the new object from the original
			double fraction = Math.abs( (double)( i - start )  / (double)(end - start) ) ;
			
			/*
			 * calculate the interpolated coordinates at the specified fractional distance 
			 * from the original line, and then convert back to Lat/Lon
			 */
			ArrayList<Coordinate> interpPts = interp.interpolate(fraction);
			ArrayList<Coordinate> newPts = toWorld(interpPts,mapDescriptor);
			
			/*
			 * create a new Drawable Element with the new coordinates
			 */
			DrawableElement de = (DrawableElement) from.copy();
			de.setPoints(newPts);
			newElems.add(de);
		}
		
		/*
		 * Adjust for Gfa text box location and forecast hour
		 */
		if ( newElems.size() > 0 && newElems.get(0) instanceof Gfa ) {
			ArrayList<Coordinate>  txtLoc = interpolate( ((Gfa)from).getGfaTextCoordinate(), ((Gfa)to).getGfaTextCoordinate(), 
					props, mapDescriptor );
			
			int len = Math.min(newElems.size(), txtLoc.size());
			for ( int ii =start+interval, jj = 0; ii < end && jj<len; ii+=interval, jj++ ) {
				((Gfa)newElems.get( jj )).setGfaTextCoordinate( txtLoc.get(jj) );
				((Gfa)newElems.get( jj )).setGfaFcstHr( "" + Math.abs(ii) );
			}
		}
		
		return newElems;
	}
	
	/*
	 * Reorder points so that the first point is the one with the greatest X coordinate value
	 * and order clockwise from there.
	 */
	private static double[][] reorderPoints(double[][] pts) {
		
		//  Convert to Coordinate[]
		Coordinate[] coords = toCoordinates(pts);
		
		/*
		 * if first coordinate is same as last, remove last one
		 */
		if ( coords[0].equals2D( coords[coords.length-1]) )
			coords = CoordinateArrays.extract(coords, 0, coords.length-2);
		
		/*
		 * Find Coordinate w/ largest X component
		 */
		double maxX = coords[0].x;
		Coordinate start = coords[0];
		for ( Coordinate coord : coords ) {
			if ( coord.x > maxX ) {
				maxX = coord.x;
				start = coord;
			}
		}
		
		/*
		 * shift so that start is the first coordinate
		 */
		CoordinateArrays.scroll(coords, start);
		
		/* 
		 * Ensure first coordinate is added to list
		 */
		CoordinateList cl = new CoordinateList(coords);
		cl.closeRing();
		coords = cl.toCoordinateArray();
		
		/*
		 * re-orient coordinate so they are clockwise.
		 * (Note that this really produces counter clockwise since OPenGL 
		 * pixel coord (0,0) is top left.  Don't think it really matters as long as we are consistent.)
		 */
		//System.out.println("First Test: "+CGAlgorithms.isCCW(coords));
		if ( CGAlgorithms.isCCW(coords) )  CoordinateArrays.reverse(coords);
		//System.out.println("second Test: "+CGAlgorithms.isCCW(coords));
		
		return toDouble(coords);
	}
	
	/*
	 * Converts an array of coordinates from Coordinate[] to double[][]
	 */
	private static double[][] toDouble(Coordinate[] coords) {

		double[][] pts = new double[coords.length][3];
		
		for ( int i=0; i < coords.length; i++ ) {
			pts[i] = new double[] { coords[i].x, coords[i].y, 0.0 }; 
		}
		
		return pts;
	}

	/*
	 * Converts an array of coordinates from double[][] to Coordinate[]
	 */
	private static Coordinate[] toCoordinates(double[][] pts) {
		Coordinate[] coords = new Coordinate[pts.length];
		for ( int i=0; i<pts.length; i++ ) {
			coords[i] = new Coordinate(pts[i][0],pts[i][1]);
		}
		return coords;
	}

	/*
	 * Applies parametric smoothing to a given line, if requested
	 */
	private static Coordinate[] prepareLine(double[][] points,	int smoothFactor ) {

		float density, devScale = 50.0f;
		double[][] smoothpts;
		
		/*
		 *  Apply parametric smoothing on pixel coordinates, if required
		 */
		if ( smoothFactor > 0 ) {
  	    	if ( smoothFactor == 1 )  density = devScale / 1.0f;
  	    	else density = devScale / 5.0f;
  	    	smoothpts = CurveFitter.fitParametricCurve(points, density);
  	    }
		else {
			smoothpts = points;
		}
		
		Coordinate[] coords = toCoordinates(smoothpts);
		return coords;
	}

	
	/**
	 * Generates new points between two given points using the specified InterpolationProperties.
	 * @param from 	starting point 
	 * @param to 	end point
	 * @param props interpolation properties
	 * @param mapDescriptor used for converting lat/lon coordinates to/from pixel coordinates
	 * @return ArrayList<Coordinate>
	 */
	public static ArrayList<Coordinate> interpolate( Coordinate from, Coordinate to, 
			InterpolationProperties props, IMapDescriptor mapDescriptor ) {
		
		if ( from == null || to == null ) 
			throw new IllegalArgumentException("PgenInterpolator: Start/End point must not be null.");
		
		ArrayList<Coordinate> newPts = new ArrayList<Coordinate>();

		Coordinate[] pointsInMap = new Coordinate[2];
		pointsInMap[0] = from;
		pointsInMap[1] = to;

		/*
		 * Convert from Lat/Lon coordinates to pixel coordinates for from/to points
		 */
		double[][] ptsPixel = PgenUtil.latlonToPixel( pointsInMap, mapDescriptor);
		
		/*
		 * Convert points from double[][] to Coordinate[]
		 */
		Coordinate[] ptsCoord = toCoordinates( ptsPixel );
		
		/*
		 * calculate the coordinates
		 */		
		int start = props.getStartingTime();
		int end = props.getEndingTime();
		int interval = props.getInterval();
		if ( start > end )  {
			start = -start;
			end = -end;
		}
		
		for ( int i=start+interval; i < end; i+=interval) {

			//  calculate the fractional distance of the new object from the original
			double fraction = (double)( i - start )  / (double)(end - start) ;
			
	/*
			 * calculate the interpolated coordinate at the specified fractional distance
			 * from the start point. 
	 */
			double dx = ( ptsCoord[1].x - ptsCoord[0].x ) * fraction;
			double dy = ( ptsCoord[1].y - ptsCoord[0].y ) * fraction;

           	Coordinate pt = new Coordinate( ptsCoord[0].x + dx,  ptsCoord[0].y + dy);
            newPts.add( pt );
    					
		}
		
		//Convert back to Lat/Lon
		newPts = toWorld( newPts, mapDescriptor );
		
		return newPts;
	}
	

	private static ArrayList<Coordinate> toWorld( ArrayList<Coordinate> coords, IMapDescriptor mapDescriptor ) {
		
		ArrayList<Coordinate> latlons = new ArrayList<Coordinate>();
		
		for ( Coordinate coord : coords ) {
			double[] in, out;
			in = new double[] { coord.x, coord.y, 0.0 };
			out = mapDescriptor.pixelToWorld(in);
			latlons.add( new Coordinate(out[0],out[1]) );
		}
		
		return latlons;
	}
	
}
