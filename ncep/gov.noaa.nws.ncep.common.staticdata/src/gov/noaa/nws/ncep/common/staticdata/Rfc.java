/*
 * gov.noaa.nws.ncep.common.staticData
 * 
 * 24 June 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.staticdata;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Class to hold RFC information
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?		    B. Yin   	Initial Creation.
 * 05/12        734         J. Zeng     move from PGEN 
 * </pre>
 * 
 * @author	B. Yin
 */

public class Rfc {

	private String siteId; 	
	private String state;
	private String rfcName;
	private String rfcCity;
	private String basinId;
	private Geometry shape;
	
	//constructor
	public Rfc(){
		
	}
	
	//constructor
	public Rfc( String rfcName,				
				   String rfcCity,				
				   String state,
				   String siteId,
				   String basinId,
				   Geometry shape ){
				   
		this.setRfcName(rfcName);
		this.setRfcCity(rfcCity);
		this.setBasinId(basinId);
		this.setSiteId(siteId);
		this.setState(state);
		this.setShape(shape);
	}

	/**
	 * Set the site ID
	 * @param siteId
	 */
	public void setSiteId(String siteId) {
		this.siteId = siteId;
	}

	/**
	 * Get the site ID
	 * @return
	 */
	public String getSiteId() {
		return siteId;
	}

	/**
	 * Set the state
	 * @param state
	 */
	public void setState(String state) {
		this.state = state;
	}

	/**
	 * Get the State
	 * @return
	 */
	public String getState() {
		return state;
	}

	/**
	 * Set RFC name
	 * @param rfcName
	 */
	public void setRfcName(String rfcName) {
		this.rfcName = rfcName;
	}

	/**
	 * get RFC name
	 * @return
	 */
	public String getRfcName() {
		return rfcName;
	}

	/**
	 * Set RFC city
	 * @param rfcCity
	 */
	public void setRfcCity(String rfcCity) {
		this.rfcCity = rfcCity;
	}

	/**
	 * Get RFC city
	 * @return
	 */
	public String getRfcCity() {
		return rfcCity;
	}

	/**
	 * Set basin ID
	 * @param basinId
	 */
	public void setBasinId(String basinId) {
		this.basinId = basinId;
	}

	/**
	 * Get basin ID
	 * @return
	 */
	public String getBasinId() {
		return basinId;
	}

	/**
	 * Get the shape geometry
	 * @return
	 */
	public Geometry getShape() {
		return shape;
	}

	/**
	 * Set the shape geometry
	 * @param shape
	 */
	public void setShape(Geometry shape) {
		this.shape = shape;
	}
	
	/**
	 * Check if the RFC shape intersect with the input geometry.
	 * @param geo
	 * @return
	 */
	public boolean intersectGeometry(Geometry geo){
		if ( shape != null ){
			return shape.intersects(geo);
		}
		else return false;
	}
	
}
