/*
 * AbstractPatternApplicator
 * 
 * Date created:  17 November 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * This abstract class defines and implements some common functionality all Pattern
 * Applicator classes can use to "apply" a specific pattern to a specific segment of a line path.
 * <P>
 * The line path is represented by a JTS LengthIndexLine object, and the specific segment of that
 * path is defined by a starting and ending point defined by the distance of those points from
 * the beginning of the line path.
 * <P><P>
 * Subclasses should extend these constructors( this is, call the appropriate super method ), and 
 * not overwrite them.
 * 
 * @author sgilbert
 * 
 */
public abstract class AbstractPatternApplicator {

	/**
	 * JTS object representing the line path - contains a list of points defining the path.
	 */
	protected LengthIndexedLine line;
	
	/**
	 * Starting location of this segment defined by distance from the beginning of the line path
	 */
	protected double startLoc;
	private Coordinate startPoint;
	
	/**
	 * Ending location of this segment defined by distance from the beginning of the line path
	 */
	protected double endLoc;
	private Coordinate endPoint;

	/**
	 * Constructor used to set only the line path.  The start and end locations are initially set to specify 
	 * the entire line path.
	 * @param line JTS representation of the line path
	 */
	protected AbstractPatternApplicator(LengthIndexedLine line) {
		this(line, 0.0, line.getEndIndex());
	}

	/**
	 * Constructor used to set the specific segment along the given line path.
	 * @param line JTS representation of the line path
	 * @param startLoc Starting location of the segment defined by distance from beginning of
	 *  the line path.
	 * @param endLoc Ending location of the segment defined by distance from beginning of
	 *  the line path.
	 */
	protected AbstractPatternApplicator(LengthIndexedLine line, double startLoc,
			double endLoc) {
		this.line = line;
		this.startLoc = startLoc;
		this.startPoint = line.extractPoint(startLoc);
		this.endLoc = endLoc;
		this.endPoint = line.extractPoint(endLoc);
	}

	/**
	 * @return The starting location of the current segment (distance from beginning of line)
	 */
	public double getStartLoc() {
		return startLoc;
	}

	/**
	 * @param startLoc The starting location of the current segment
	 */
	protected void setStartLoc(double startLoc) {
		this.startLoc = startLoc;
		this.startPoint = line.extractPoint(startLoc);
	}

	/**
	 * @return The ending location of the current segment (distance from beginning of line)
	 */
	public double getEndLoc() {
		return endLoc;
	}

	/**
	 * @param endLoc The ending location of the current segment
	 */
	protected void setEndLoc(double endLoc) {
		this.endLoc = endLoc;
		this.endPoint = line.extractPoint(endLoc);
	}
	
	/**
	 * Gets the Coordinate along the line path at the current starting location
	 * @return The starting coordinate
	 */
	protected Coordinate getStartPoint() {
		//return line.extractPoint(startLoc);
		return startPoint;
	}

	/**
	 * Gets the Coordinate along the line path at the current ending location
	 * @return The ending coordinate
	 */
	protected Coordinate getEndPoint() {
		//return line.extractPoint(endLoc);
		return endPoint;
	}
	
	/**
	 * Calculates the midpoint of the line segment defined by the current start and end points
	 * @return The midpoint
	 */
	protected Coordinate getMidpoint() {
		Coordinate c1 = getStartPoint();
		Coordinate c2 = getEndPoint();
		return new Coordinate((c1.x+c2.x)/2.0, (c1.y+c2.y)/2.0);
	}
	
	/**
	 * Calculates the linear distance of the line segment defined by the current start and end points
	 * @return The linear distance
	 */
	protected double getDistance() {
		Coordinate c1 = getStartPoint();
		Coordinate c2 = getEndPoint();
		
		return c1.distance(c2);
	}
	
	/**
	 * Calculates the angle relative to x-axis of the segment defined by the current start and end points.
	 * @return The angle in degrees.
	 */
	protected double getSegmentAngle() {
		Coordinate c1 = getStartPoint();
		Coordinate c2 = getEndPoint();
		
		return Math.toDegrees(Math.atan2( (c2.y-c1.y), (c2.x-c1.x) ));
	}
	
	/**
	 * Gets a list of all points along the path between the current start and end points 
	 * @return The path segment
	 */
	protected Coordinate[] getSegmentPath() {
		return line.extractLine(startLoc, endLoc).getCoordinates();
	}
	
	/**
	 * Converts a list of points from a Coordinate[] array to a two dimensional double[][] array.
	 * @return Array of points
	 */
	protected double[][] getSegmentPts() {
		Coordinate[] tmp = getSegmentPath();
		double[][] newpts = new double[tmp.length][3];
		for (int k=0; k<tmp.length; k++ ) {
			newpts[k][0] = tmp[k].x;
			newpts[k][1] = tmp[k].y;
		}
		return newpts;
	}
	
	/**
	 * Implemented by the subclass to calculate any filled areas of the specific pattern being 
	 * applied to the current path segment.
	 * @return List of Coordinates defining the filled area.
	 */
	abstract public Coordinate[] calculateFillArea();
	
	/**
	 * Implemented by the subclass to calculate any line segments of the specific pattern being 
	 * applied to the current path segment.
	 * @return list of points defining line segments of pattern
	 */
	abstract public double[][] calculateLines();
	
}
