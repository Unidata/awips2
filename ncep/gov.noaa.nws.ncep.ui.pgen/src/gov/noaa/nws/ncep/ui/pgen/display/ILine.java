/*
 * ILine
 * 
 * Date created: 20 April 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;


/**
 * Interface for all Line and its subclasses 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/11					B. Yin   	Initial Creation.
 * </pre>
 * 
 * @author	B. Yin
 */

public interface ILine extends IMultiPoint {
	
	/**
	 * Gets the pattern that should be applied to the line.
	 * @return The line pattern
	 */
	public String getPatternName();
	
	/**
	 * Gets the smooth factor used to create line path
	 * @return Line smoothing factor
	 */
	public int getSmoothFactor();
	
	/**
	 * Checks whether the line path is closed.
	 * @return true, if line path is closed.
	 */
	public Boolean isClosedLine();
	
	/**
	 * Checks whether the object should be filled
	 * @return true, if a fill pattern applies
	 */
	public Boolean isFilled();
	
	/**
	 * Specifes the Fill Pattern to use, if isFilled returns true.
	 * @return The Fill Pattern associated with the object
	 */
	public FillPattern getFillPattern();
	
}
