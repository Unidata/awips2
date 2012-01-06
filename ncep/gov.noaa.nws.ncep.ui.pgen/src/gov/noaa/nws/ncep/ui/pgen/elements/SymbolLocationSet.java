/*
 * SymbolLocationSet
 * 
 * Date created: 20 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import java.awt.Color;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.display.ISymbolSet;

/**
 * Class to represent one symbol and a list of one or more lat/lon coordinate locations
 * associated with that symbol.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/09			77		S. Gilbert   	Initial Creation.
 * 05/09        #42         S. Gilbert  Added pgenType and pgenCategory to constructors
 *
 * </pre>
 * 
 * @author	
 * @version	0.0.1
 */
public class SymbolLocationSet implements ISymbolSet {
    
	/**
	 * Drawable element symbol/marker
	 */
	private Symbol	symbol;

	/**
	 * Array of lat/lon coordinate locations
	 */
	private Coordinate[] locations;

	
	/**
	 * Constructor used to set symbol and list of locations.
	 * @param symbol 
	 * @param locations Array of lat/lon coordinate locations
	 */
	public SymbolLocationSet(Symbol symbol, Coordinate[] locations) {
		this.symbol = symbol;
		this.locations = locations;
	}

	/**
	 * Constructor used to specify all attributes of a symbol along with a list of
	 * lat/lon coordinate locations.
	 * @param range  TBD
	 * @param colors Symbol color
	 * @param lineWidth Line width used to create symbol
	 * @param sizeScale size scale of the symbol
	 * @param hasMask indicates whether symbol has background mask. 
	 * @param location array of lat/lon coordinates
	 * @param type Identifies symbol/marker pattern.
	 */
	public SymbolLocationSet(Coordinate[] range, Color[] colors,
			float lineWidth, double sizeScale, Boolean hasMask,
			Coordinate[] locations, String pgenCategory, String pgenType) {

		this.symbol = new Symbol( range, colors, lineWidth, sizeScale, hasMask, locations[0], pgenCategory, pgenType );
		this.locations = locations;
	}

	/**
	 * Gets list of lat/lon locations
	 */
	@Override
	public Coordinate[] getLocations() {
		return locations;
	}

	/**
	 * Gets symbol
	 */
	@Override
	public Symbol getSymbol() {
		return symbol;
	}	

}
