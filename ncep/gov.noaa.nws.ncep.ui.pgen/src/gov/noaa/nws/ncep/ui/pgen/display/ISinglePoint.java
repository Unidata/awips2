/*
 * ISinglePoint
 * 
 * Date created: 20 NOVEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface used to get specific attributes of a PGEN Geographic single-point object
 * such as Markers and Symbols.
 * @author sgilbert
 *
 */
public interface ISinglePoint extends IAttribute{

	/**
	 * Gets the Lat/lon location of the object
	 * @return Lat/lon coordinate
	 */
	public Coordinate getLocation();

	/**
	 * Checks whether the background of the object should be cleared.
	 * @return true, if background should be cleared
	 */
	public Boolean isClear();

}
