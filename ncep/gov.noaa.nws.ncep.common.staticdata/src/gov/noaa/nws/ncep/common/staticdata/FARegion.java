/*
 * gov.noaa.nws.ncep.common.staticData.FARegion
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.staticdata;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
/**
 * Class to hold FA region information
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
public class FARegion extends AbstractBounds {
	
	private String region;
	
	public FARegion(){
		
	}
	
	public FARegion(String bid, String region,
			int numBlocks, Coordinate centriod, Geometry geometry) {
		super();
		this.bid = bid;
		this.setRegion(region);
		this.numBlocks = numBlocks;
		this.centriod = centriod;
		this.geometry = geometry;
	}

	/**
	 * @param region the region to set
	 */
	public void setRegion(String region) {
		this.region = region;
	}

	/**
	 * @return the region
	 */
	public String getRegion() {
		return region;
	}
}
