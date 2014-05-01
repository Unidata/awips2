/*
 * gov.noaa.nws.ncep.common.staticData.GreatLake
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.staticdata;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Class to hold great lake information
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

public class GreatLake extends AbstractBounds {

		public GreatLake(){
			
		}
		
		public GreatLake(String bid, String area,Coordinate centriod,
				int numBlocks, String id, Geometry geometry) {
			super();
			this.bid = bid;
			this.area = area;
			this.id = id;
			this.numBlocks = numBlocks;
			this.centriod = centriod;
			this.geometry = geometry;
		}

}
