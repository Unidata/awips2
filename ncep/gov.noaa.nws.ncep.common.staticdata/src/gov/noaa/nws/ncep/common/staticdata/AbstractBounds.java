/*
 * gov.noaa.nws.ncep.common.staticData.AbstractBounds
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.staticdata;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Class to hold the common attributes for NCEP bounds.
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

public abstract class AbstractBounds {
	
	protected String bid;
	protected String area;
	protected String name;
	protected String id;
	protected String statesInArea;
	protected int numBlocks;
	protected Coordinate centriod;
	protected Geometry geometry;
	
	/**
	 * @return the bid
	 */
	public String getBid() {
		return bid;
	}
	/**
	 * @param bid the bid to set
	 */
	public void setBid(String bid) {
		this.bid = bid;
	}
	/**
	 * @return the area
	 */
	public String getArea() {
		return area;
	}
	/**
	 * @param area the area to set
	 */
	public void setArea(String area) {
		this.area = area;
	}
	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}
	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}
	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}
	/**
	 * @param id the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}
	/**
	 * @return the statesInArea
	 */
	public String getStatesInArea() {
		return statesInArea;
	}
	/**
	 * @param statesInArea the statesInArea to set
	 */
	public void setStatesInArea(String statesInArea) {
		this.statesInArea = statesInArea;
	}
	/**
	 * @return the numBlocks
	 */
	public int getNumBlocks() {
		return numBlocks;
	}
	/**
	 * @param numBlocks the numBlocks to set
	 */
	public void setNumBlocks(int numBlocks) {
		this.numBlocks = numBlocks;
	}
	/**
	 * @return the centriod
	 */
	public Coordinate getCentriod() {
		return centriod;
	}
	/**
	 * @param centriod the centriod to set
	 */
	public void setCentriod(Coordinate centriod) {
		this.centriod = centriod;
	}
	/**
	 * @return the geometry
	 */
	public Geometry getGeometry() {
		return geometry;
	}
	/**
	 * @param geometry the geometry to set
	 */
	public void setGeometry(Geometry geometry) {
		this.geometry = geometry;
	}

}
