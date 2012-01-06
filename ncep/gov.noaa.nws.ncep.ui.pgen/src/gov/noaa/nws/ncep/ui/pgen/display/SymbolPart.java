/*
 * SymbolPart
 * 
 * Date created: 10 DECEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This the base class defining a single part of a symbol pattern. It basically defines methods to get
 * the line path defining the symbol part and a flag indicating whether the area defined by the path 
 * should be filled.
 * The coordinates used for the pattern assume that the center of the symbol is at coordinate (0,0).
 * @author sgilbert
 *
 */
public abstract class SymbolPart {

	public SymbolPart() {
		
	}
	
	/**
	 * Gets the coordinates defining the line path
	 * @return the line path
	 */
	public abstract Coordinate[] getPath();

	/**
	 * Gets whether area defined by line path should be filled
	 * @return the filled flag
	 */
	public abstract boolean isFilled();

	
}
