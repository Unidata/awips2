/*
 * gov.noaa.nws.ncep.common.staticData.SPCCounty
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.staticdata;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Class to hold SPC county/marine zone information
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/10		#159		B. Yin   	Initial Creation.
 * 04/11		?			B. Yin		Read from Raytheon's tables
 * 01/12		?			B. Yin		Read county maps with lowest resolution.
 * 										Fix invalid geometry problems. 
 * 03/12					B. Yin		Moved from elements package
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class SPCCounty {
	
	
	//county name
	private String name;
	private String fips;
	private String wfo;
	private String ugcId;
	private String state;
	private String country;
	
	//for marine zone
	private String zoneName;
	private boolean marineZone;
	
	private Coordinate centriod;
	private Geometry shape;
	
	//constructor
	public SPCCounty(){
		
	}
	
	//constructor
	public SPCCounty( String fips,
				   String name,
				   String wfo,
				   String ugcId,
				   String state,
				   String country,
				   String zoneName,
				   Coordinate centroid,
				   Geometry shape,
				   boolean marineZone ){
		this.setFips(fips);
		this.setName(name);
		this.setWfo(wfo);
		this.setUgcId(ugcId);
		this.setState(state);
		this.setCountry(country);
		this.setZoneName(zoneName);
		this.setCentriod(centroid);
		this.setShape(shape);
		this.setMarineZone(marineZone);
		
	}

	public void setFips(String fips) {
		this.fips = fips;
	}

	public String getFips() {
		return fips;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setWfo(String wfo) {
		this.wfo = wfo;
	}

	public String getWfo() {
		return wfo;
	}

	public void setUgcId(String ugcId) {
		this.ugcId = ugcId;
	}

	public String getUgcId() {
		return ugcId;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getState() {
		return state;
	}

	public void setCountry(String country) {
		this.country = country;
	}

	public String getCountry() {
		return country;
	}

	public void setZoneName(String zoneName) {
		this.zoneName = zoneName;
	}

	public String getZoneName() {
		return zoneName;
	}

	public void setMarineZone(boolean marineZone) {
		this.marineZone = marineZone;
	}

	public boolean isMarineZone() {
		return marineZone;
	}

	public void setCentriod(Coordinate centriod) {
		this.centriod = centriod;
	}

	public Coordinate getCentriod() {
		return centriod;
	}

	public void setShape(Geometry shape) {
		this.shape = shape;
	}

	public Geometry getShape() {
		return shape;
	}

}
