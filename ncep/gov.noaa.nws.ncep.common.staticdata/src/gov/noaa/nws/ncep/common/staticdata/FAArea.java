/*
 * gov.noaa.nws.ncep.common.staticData.FAArea
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.staticdata;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Class to hold FA area information
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?		B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class FAArea extends AbstractBounds{
	
	public FAArea(){
		
	}
	
	public FAArea(String bid, String area, String name, String statesInArea,
			int numBlocks, Coordinate centriod, Geometry geometry) {
		super();
		this.bid = bid;
		this.area = area;
		this.name = name;
		this.statesInArea = statesInArea;
		this.numBlocks = numBlocks;
		this.centriod = centriod;
		this.geometry = geometry;
	}

}
