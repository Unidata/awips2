/*
 * IKink
 * 
 * Date created: 10 DECEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import gov.noaa.nws.ncep.ui.pgen.display.ArrowHead.ArrowHeadType;

import java.awt.Color;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface used to get specific attributes of a PGEN Geographic two-point object
 * that represents a line with a "kink" in it.
 * @author sgilbert
 *
 */
public interface IKink extends IAttribute{

	/**
	 * Gets the starting coordinate of the line segment
	 * @return starting Coordinate of the segment
	 */
	public Coordinate getStartPoint();
	
	/**
	 * Gets the ending coordinate of the line segment
	 * @return Ending Coordinate of the segment
	 */
	public Coordinate getEndPoint();
	
	/**
	 * Gets the color of this line
	 * @return the color
	 */
	public Color getColor();
	
	/**
	 * Gets the location of the kink along the line.  Should be in 
	 * the range 0.25 to 0.75
	 * @return location as fraction of the way along the line.
	 */
	public double getKinkPosition();

	/**
	 * Gets the arrow head type. open or closed.
	 * @return Arrow Head type. 
	 */
	public ArrowHeadType getArrowHeadType();
	
}
