/*
 * IArc
 * 
 * Date created: 27 JANUARY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface used to get specific attributes of a PGEN drawable object that 
 * represents a circle, ellipse, or an arc segment of either 
 * @author sgilbert
 *
 */
public interface IArc extends IAttribute{

	/**
	 * Gets the lat/lon coordinate of the center of the circle/ellipse
	 * @return center lat/lon
	 */
	public Coordinate getCenterPoint();
	
	/**
	 * Gets the lat/lon of a circumference point.  This point, along with the center point, 
	 * defines the major axis
	 * @return lat/lon on the circumference
	 */
	public Coordinate getCircumferencePoint();
	
	/**
	 * Gets the axis ratio = length of minor axis / length of major axis
	 * @return axis ratio
	 */
	public double getAxisRatio();
	
	/**
	 * Gets the start angle used to find the starting point of the arc. This angle is relative to 
	 * the major axis 
	 * @return angle in degrees
	 */
	public double getStartAngle();
	
	/**
	 * Gets the end angle used to find the ending point of the arc. This angle is relative to 
	 * the major axis
	 * @return angle in degrees
	 */
	public double getEndAngle();
	
}
