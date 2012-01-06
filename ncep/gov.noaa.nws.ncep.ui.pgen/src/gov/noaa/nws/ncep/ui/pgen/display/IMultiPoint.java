/*
 * IMultiPoint
 * 
 * Date created: 20 NOVEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface used to get specific attributes of a PGEN Geographic multi-point object.
 * @author sgilbert
 *
 */
public interface IMultiPoint extends IAttribute{

	/**
	 * Gets an array of coordinates defining path of the object
	 * @return Coordinate array
	 */
	public Coordinate[] getLinePoints();

}
