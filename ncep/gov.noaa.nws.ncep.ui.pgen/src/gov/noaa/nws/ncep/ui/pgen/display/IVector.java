/*
 * IVector
 * 
 * Date created: 26 JANUARY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import java.awt.Color;

/**
 * Interface used to get specific attributes of a PGEN Geographic Vector object
 * such as wind barbs, arrows, and hash marks.  
 * @author sgilbert
 *
 */
public interface IVector extends ISinglePoint{

	/**
	 * Different Vector Types representing wind direction (and maybe speed)
	 * @author sgilbert
	 *
	 */
    public static enum VectorType {
        ARROW, WIND_BARB, HASH_MARK
    }
    
	/**
	 * Gets vector object type
	 * @return type of object
	 */
	public VectorType getVectorType();
	
	/**
	 * Gets color associated with the object
	 * @return Color
	 */
	public Color getColor();
	
	/**
	 * Checks whether the background of the object should be cleared.
	 * @return true, if background should be cleared
	 */
	public Boolean hasBackgroundMask();

	/**
	 * Gets the wind speed 
	 * @return wind spped
	 */
	public double getSpeed();
	
	/**
	 * Gets the wind direction
	 * @return direction from which the wind is blowing. North is considered 0 degrees 
	 * and direction increases clockwise.
	 */
	public double getDirection();
	
	/**
	 * Gets the size scale for the arrow head.
	 * @return returns arrow head size
	 */
	public double getArrowHeadSize();
	
	/**
	 * Gets boolean flag indication whether arrow has an associated speed,
	 * or indicates direction only
	 * @return direction only flag
	 */
	public boolean hasDirectionOnly();

}
