/*
 * ISymbol
 * 
 * Date created: 20 April 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

/**
 * Interface for Symbol and its subclasses.
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

public interface ISymbol extends ISinglePoint {
	/**
	 * Gets the name of the symbol pattern to use for this element
	 * @return name of symbol pattern
	 */
	public String getPatternName();
	
}
