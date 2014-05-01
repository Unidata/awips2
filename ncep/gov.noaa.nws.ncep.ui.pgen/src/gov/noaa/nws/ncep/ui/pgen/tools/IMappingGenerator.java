/*
 * IMappingGenerator
 * 
 * Date created: 26 AUGUST 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.util.LinkedHashMap;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Classes that implement this interface generate a set of Coordinate pairs that map Coordinates of
 *  one object to another.  This Coordinate mapping can later be used by the LinearInterpolator to
 *  calculate the coordinates for  new objects.
 * @author sgilbert
 *
 */
public interface IMappingGenerator {

	/**
	 * The algorithm used to generate a set of mapping coordinates
	 * @return A map of Coordinate pairs
	 */
	public LinkedHashMap<Coordinate,Coordinate> generateMappingPoints();
	
}
