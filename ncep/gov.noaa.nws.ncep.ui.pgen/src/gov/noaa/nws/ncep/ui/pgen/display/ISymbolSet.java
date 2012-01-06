/*
 * ISymbolSet
 * 
 * Date created: 20 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;

/**
 * Interface used to get specific attributes of a Symbol Set, where a symbol is displayed
 * at one or more locations.
 * @author sgilbert
 *
 */
public interface ISymbolSet {

	/**
	 * Gets the symbol/marker to be displayed.
	 * @return a symbol object
	 */
	public Symbol getSymbol();
	
	/**
	 * gets the list of lat/lon Coordinates at which the symbol should be displayed
	 * @return coordinate array
	 */
	public Coordinate[] getLocations();
	
}
