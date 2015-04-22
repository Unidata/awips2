/*
 * gov.noaa.nws.ncep.common.staticData
 * 
 * 24 June 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.staticdata;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Class to hold CWA information
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?		    B. Yin   	Initial Creation.
 * 06/12        734         J. Zeng     move from PGEN
 * </pre>
 * 
 * @author	B. Yin
 */

public class Cwa {
		
	private String cwaName; 	
	private String wfoName;		
	private Coordinate centriod;
	private Geometry shape;
	
	//constructor
	public Cwa(){
		
	}
	
	//constructor
	public Cwa( String cwa,				
				   String wfo,				
				   Coordinate centroid,
				   Geometry shape ){
				   
		this.setCwaName(cwa);
		this.setWfoName(wfo);
		this.setCentriod(centroid);
		this.setShape(shape);
	}
	
	/**
	 * Set Centriod
	 * @param centriod
	 */
	public void setCentriod(Coordinate centriod) {
		this.centriod = centriod;
	}

	/**
	 * Get Centriod
	 * @return
	 */
	public Coordinate getCentriod() {
		return centriod;
	}

	/**
	 * set shape
	 * @param shape
	 */
	public void setShape(Geometry shape) {
		this.shape = shape;
	}

	/**
	 * get shape
	 * @return
	 */
	public Geometry getShape() {
		return shape;
	}
	
	/**
	 * find the 
	 * @param geo
	 * @return
	 */
	public boolean intersectGeometry(Geometry geo){
		if ( shape != null ){
			return shape.intersects(geo);
		}
		else return false;
	}


	/**
	 * set cwa name
	 * @param cwa
	 */
	public void setCwaName(String cwa) {
		this.cwaName = cwa;
	}

	/**
	 * get cwa name
	 * @return
	 */
	public String getCwaName() {
		return cwaName;
	}

	/**
	 * set wfo name
	 * @param wfo
	 */
	public void setWfoName(String wfo) {
		this.wfoName = wfo;
	}

	/**
	 * get wfo name
	 * @return
	 */
	public String getWfoName() {
		return wfoName;
	}
}