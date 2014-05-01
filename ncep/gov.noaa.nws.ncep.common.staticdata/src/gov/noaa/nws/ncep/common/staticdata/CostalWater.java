/*
 * gov.noaa.nws.ncep.common.staticData.CostalWater
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.staticdata;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Class for NCEP costal waters.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/12		#?			B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class CostalWater extends AbstractBounds {


	/**
	 * Class to hold Costal Water information
	 * 
	 * <pre>
	 * SOFTWARE HISTORY
	 * Date       	Ticket#		Engineer	Description
	 * ------------	----------	-----------	--------------------------
	 * 04/12		?		B. Yin   	Initial Creation.
	 *
	 * </pre>
	 * 
	 * @author	B. Yin
	 */



	public CostalWater(){

	}

	public CostalWater(String bid, String name,Coordinate centriod,
			int numBlocks, String id, Geometry geometry) {
		super();
		this.bid = bid;
		this.name = name;
		this.id = id;
		this.numBlocks = numBlocks;
		this.centriod = centriod;
		this.geometry = geometry;
	}


}
