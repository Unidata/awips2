/*
 * ArcPatternApplicator
 * 
 * Date created: 20 NOVEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * This class can be used to apply several different patterns involving points
 * along the circumference of a cirle or arc to a specific segment of a line path.
 * <P>
 * The midpoint of the line connecting the endpoints of the segment is used as the
 * center of the arc/circle and the radius used is half the distance between the two
 * endpoints.  
 * <P>
 * This applicator can be used for pattern segments such as circle, half circle, 
 * carets, ticks, etc...  Users can specify the starting and ending angles along
 * the arc as well as the number of segments to be calculated in between.
 * @author sgilbert
 *
 */
public class ArcPatternApplicator extends AbstractPatternApplicator {

	/**
	 * Angle defining the starting point of the arc
	 */
	double startAngle = 0.0;

	/**
	 * Angle defining the ending point of the arc
	 */
	double endAngle = 0.0;
	
	/**
	 * number of segments along the arc to calculate
	 */
	int    numsegs = 0;
	
	/**
	 * Indicator to add the segment path to the calculated arc points 
	 */
	boolean addSegment = false;
	
	/**
  	 * Constructor used to set the line path as well as the specific segment of the
	 * line where the arc pattern will be applied.
	 * @param line - Set of points Defining a line path.
	 * @param startLoc - Starting location of the segment defined by distance from the 
	 * beginning of the line path.  Negative value specifies distance from the end of line.
	 * A distance greater than the line length is set to the end of the line path.
	 * @param endLoc Ending location of the segment defined by distance from the 
	 * beginning of the line path.  Negative value specifies distance from the end of line.
	 * A distance greater than the line length is set to the end of the line path.
	 */
	public ArcPatternApplicator(LengthIndexedLine line, double startLoc,
			double endLoc) {
		super(line, startLoc, endLoc);
	}

	/**
	 * Constructor used to set the line path. The segment of the path is set to the
	 * entire line.
	 * @param line - Set of points defining a line path.
	 */
	public ArcPatternApplicator(LengthIndexedLine line) {
		super(line);
	}

	/**
	 * Sets flag whether to add the segment path to the calculated arc points
	 * @param add True to add segment path.  Default is false
	 */
	public void addSegmentToFill(boolean add) {
		this.addSegment = add;
	}
	
	/**
	 * Sets the attributes of the arc points to calculate.  The Start and End angles are 
	 * relative to the slope of the line connecting the segment endpoints (not relative to x-axis).
	 * @param start Angle defining stating point of arc
	 * @param end Angle defining ending point of arc
	 * @param num Number of segments to calculate along arc
	 */
	public void setArcAttributes(double start, double end, int num) {
		this.startAngle = start;
		this.endAngle = end;
		this.numsegs = num;
		
	}

	/**
	 * Calculates the points along the arc applied to the current line segment.
	 * @return The arc points of the pattern.  
	 * @see gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#calculateFillArea()
	 */
	public Coordinate[] calculateFillArea() {
		
		int index;
		double[][] arc;
		Coordinate[] cfill;

		/*
		 * calculate arc points
		 */
		arc = calculateLines();
		int arcnum = arc.length;
		
		/*
		 * If addSegment flag is true, add segment path to cfill.
		 * Otherwise, initialize cfill for arc
		 */
		if ( addSegment ) {
			Coordinate[] path = getSegmentPath();
			cfill = new Coordinate[path.length+arcnum+1];
			for (int j=0; j<path.length; j++) {
				   cfill[j] = path[j];
				   //System.out.println("SAGPATH="+cfill[j].toString());
				}
			index = path.length;
		} else {
			cfill = new Coordinate[arcnum+1];
			index = 0;
		}
		
		/*
  		 * convert Points on the arc from Coordinate[] to double[][]
		 */
		for (int i=0; i<arcnum; i++) {
			cfill[index++] = new Coordinate( arc[i][0], arc[i][1]);
			   //System.out.println("SAGARC="+cfill[index-1].toString());
		}
		cfill[index] = cfill[0];
		
		return cfill;
		/*
		Coordinate mid = getMidpoint();
		double angle = getSegmentAngle();
		double radius = getDistance() / 2.0;
		double interval = (endAngle - startAngle) / (double)numsegs;
		Coordinate[] path = getSegmentPath();
		
		//System.out.println("SAGARC="+angle+":"+radius+":"+interval);
		//
		//  grab current path along segment
		// 
		Coordinate[] fill = new Coordinate[path.length+numsegs+2];
		for (int j=0; j<path.length; j++) {
		   fill[j] = path[j];
		}
		
		//
		//  calculate coordinates along arc
		//
		int index = path.length;
		double theta = angle + startAngle;
		for (int i=0; i < numsegs+1; i++) {
			double xxxx = radius * Math.cos(Math.toRadians(theta));
			double yyyy = radius * Math.sin(Math.toRadians(theta));
			fill[index++] = new Coordinate( mid.x + xxxx, mid.y + yyyy);
			theta += interval;
		}
		fill[index] = fill[0];
		
		//System.out.println("SAGFILL"+startLoc+":"+endLoc+":"+fill.length);
		return fill;
		*/
	}
	
	/**
	 * Calculates the points along the arc applied to the current line segment.
	 * @return The arc points of the pattern.  
	 * @see gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#calculateLines()
	 */
	public double[][] calculateLines() {
		
		Coordinate mid = getMidpoint();
		double angle = getSegmentAngle();
		double radius = getDistance() / 2.0;
		double interval = (endAngle - startAngle) / (double)numsegs;
		double theta = angle + startAngle;
		double[][] lines = new double[numsegs+1][3];

		/*
		 * Calculate coordinates along arc
		 */
		for (int i=0; i < numsegs+1; i++) {
			lines[i][0] = (radius * Math.cos(Math.toRadians(theta))) + mid.x;
			lines[i][1] = (radius * Math.sin(Math.toRadians(theta))) + mid.y;
			theta += interval;
		}

		return lines;
	}
	
}
