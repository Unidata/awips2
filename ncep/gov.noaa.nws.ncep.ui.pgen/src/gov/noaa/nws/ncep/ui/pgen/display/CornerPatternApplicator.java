/*
 * CornerPatternApplicator
 * 
 * Date created: 20 NOVEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * This class can be used to apply several different patterns involving four corner points 
 * to a specific segment of a line path.  The four corner points can be used to form a box,
 * "X", "Z", or double line pattern segment.
 * <P>
 * Users can specify the pattern distance left and right of the center of the line segment.
 * @author sgilbert
 */
public class CornerPatternApplicator extends AbstractPatternApplicator {
	
	/**
	 * The available patterns that can be created with CornerPatternApplicator class.
	 * @author sgilbert
	 *
	 */
	public static enum CornerPattern { BOX, X_PATTERN, Z_PATTERN, DOUBLE_LINE, TICK };

	/**
	 * Size of pattern on the left and right side of center line.
	 */
	private double height=1.0;
	private CornerPattern patternType = CornerPattern.BOX;
	
	/**
	 * Constructor used to set the line path. The segment of the path is set to the
	 * entire line.
	 * @param line - Set of points defining a line path.
	 */
	public CornerPatternApplicator(LengthIndexedLine line) {
		super(line);
	}

	/**
	 * Constructor used to set the line path as well as the specific segment of the
	 * line where the corner pattern will be applied.
	 * @param line - Set of points Defining a line path.
	 * @param startLoc - Starting location of the segment defined by distance from the 
	 * beginning of the line path.  Negative value specifies distance from the end of line.
	 * A distance greater than the line length is set to the end of the line path.
	 * @param endLoc Ending location of the segment defined by distance from the 
	 * beginning of the line path.  Negative value specifies distance from the end of line.
	 * A distance greater than the line length is set to the end of the line path.
	 */
	public CornerPatternApplicator(LengthIndexedLine line, double startLoc,
			double endLoc) {
		super(line, startLoc, endLoc);
	}

	/**
	 * Sets the size, in the normal direction of the path, for the resulting corner pattern.
	 * @param height Distance left and right of the center line.  Default is 1.0
	 */
	public void setHeight(double height) {
		this.height = height;
	}
	
	
	/**
	 * Set the specific pattern to calculate.
	 * @param type the type of corner pattern.
	 */
	public void setPatternType(CornerPattern type) {
		this.patternType = type;
	}

	/**
	 * Calculates the corner points applied to the current line segment.
	 * @return The corner points of the pattern.  
	 * <P> If patternType set to BOX, the first point is repeated at the end of the 
	 * array of points so that line segments connecting the point will produce a closed box.
	 * <P> If patternType set to DOUBLE_LINE, Note the first two points represent the top line,
	 * and the last two points define the bottom line segment.
	 * <P> If patternType set to Z_PATTERN, the segments defined by the points look like a "Z". 
	 * <P> If patternType set to X_PATTERN, Note the first two points represent one slash
	 * of the "X", and the last two points define the other slash segment.
	 * @see gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#calculateFillArea()
	 */
	@Override
	public Coordinate[] calculateFillArea() {
		int cnum=5;
		double[][] corner = new double[cnum][3];
		Coordinate[] cfill = new Coordinate[cnum];

		// calculate corner points
		corner = calculateLines();
		
		// TODO - Add check for type BOX only?
		// convert from Coordinate[] to double[][]
		for (int i=0; i<cnum; i++) {
			cfill[i] = new Coordinate( corner[i][0], corner[i][1]);
		}
		
		return cfill;
	}

	/**
	 * Calculates the corner points applied to the current line segment.
	 * @return The corner points of the pattern.  
	 * <P> If patternType set to BOX, the first point is repeated at the end of the 
	 * array of points so that line segments connecting the point will produce a closed box.
	 * <P> If patternType set to DOUBLE_LINE, Note the first two points represent the top line,
	 * and the last two points define the bottom line segment.
	 * <P> If patternType set to Z_PATTERN, the segments defined by the points look like a "Z". 
	 * <P> If patternType set to X_PATTERN, Note the first two points represent one slash
	 * of the "X", and the last two points define the other slash segment.
	 * @see gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#calculateLines()
	 */
	@Override
	public double[][] calculateLines() {
		int cnum;
		double[][] corners;

		//
		//  get line segment angle and calculate the x and y coordinate 
		//  offsets at right angle to the segment
		//
		double theta = getSegmentAngle();
		//System.out.println("SAG THETA = "+theta);
		double xOffset = height * Math.cos( Math.toRadians(90.0+theta));
		double yOffset = height * Math.sin( Math.toRadians(90.0+theta));
		
		switch ( patternType ) {
			case BOX:
				cnum=5;
				corners = new double[cnum][3];
				// left side start corner
				corners[0][0] = getStartPoint().x + xOffset;
				corners[0][1] = getStartPoint().y + yOffset;
				// right side start corner
				corners[1][0] = getStartPoint().x - xOffset;
				corners[1][1] = getStartPoint().y - yOffset;
				// right side end corner
				corners[2][0] = getEndPoint().x - xOffset;
				corners[2][1] = getEndPoint().y - yOffset;
				// left side end corner
				corners[3][0] = getEndPoint().x + xOffset;
				corners[3][1] = getEndPoint().y + yOffset;
				// repeat first corner point
				corners[4] = corners[0];
				break;
			
			case DOUBLE_LINE:
				cnum=4;
				corners = new double[cnum][3];
				// left side start corner
				corners[0][0] = getStartPoint().x + xOffset;
				corners[0][1] = getStartPoint().y + yOffset;
				// left side end corner
				corners[1][0] = getEndPoint().x + xOffset;
				corners[1][1] = getEndPoint().y + yOffset;
				// right side start corner
				corners[2][0] = getStartPoint().x - xOffset;
				corners[2][1] = getStartPoint().y - yOffset;
				// right side end corner
				corners[3][0] = getEndPoint().x - xOffset;
				corners[3][1] = getEndPoint().y - yOffset;
				break;

			case X_PATTERN:
				cnum=4;
				corners = new double[cnum][3];
				// left side start corner
				corners[0][0] = getStartPoint().x + xOffset;
				corners[0][1] = getStartPoint().y + yOffset;
				// right side end corner
				corners[1][0] = getEndPoint().x - xOffset;
				corners[1][1] = getEndPoint().y - yOffset;
				// right side start corner
				corners[2][0] = getStartPoint().x - xOffset;
				corners[2][1] = getStartPoint().y - yOffset;
				// left side end corner
				corners[3][0] = getEndPoint().x + xOffset;
				corners[3][1] = getEndPoint().y + yOffset;
				break;

			case Z_PATTERN:
				cnum=4;
				corners = new double[cnum][3];
				// left side start corner
				corners[0][0] = getStartPoint().x + xOffset;
				corners[0][1] = getStartPoint().y + yOffset;
				// left side end corner
				corners[1][0] = getEndPoint().x + xOffset;
				corners[1][1] = getEndPoint().y + yOffset;
				// right side start corner
				corners[2][0] = getStartPoint().x - xOffset;
				corners[2][1] = getStartPoint().y - yOffset;
				// right side end corner
				corners[3][0] = getEndPoint().x - xOffset;
				corners[3][1] = getEndPoint().y - yOffset;
				break;
				
			case TICK:
				cnum=2;
				corners = new double[cnum][3];
				// end point
				corners[0][0] = getEndPoint().x;
				corners[0][1] = getEndPoint().y;
				// right side end corner
				corners[1][0] = getEndPoint().x - xOffset;
				corners[1][1] = getEndPoint().y - yOffset;
				break;
				
			default:
			    // TODO - Message? -OR- Exception?
				corners = null;
				System.out.println("Corner type is not set.");
				break;
		}
		
		return corners;
	}

}
