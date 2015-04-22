/*
 * gov.noaa.nws.ncep.common.staticData
 * 
 * 23 February 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */


package gov.noaa.nws.ncep.common.staticdata;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Class to hold US state information
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

public class USState {
	
	private String stateAbrv; 	//abbreviation
	private String name;		//full name
	private String fips;
	private Coordinate centriod;
	private Geometry shape;
	
	//constructor
	public USState(){
		
	}
	
	//constructor
	public USState( String stateAbrv,				
				   String name,				
				   String fips,
				   Coordinate centroid,
				   Geometry shape ){
				   
		this.setFips(fips);
		this.setName(name);
		this.setStateAbrv(stateAbrv);
		this.setCentriod(centroid);
		this.setShape(shape);
	}

	public void setStateAbrv(String state) {
		this.stateAbrv = state;
	}

	public String getStateAbrv() {
		return stateAbrv;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setFips(String fips) {
		this.fips = fips;
	}

	public String getFips() {
		return fips;
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
	
	public boolean intersectGeometry(Geometry geo){
		if ( shape != null ){
			return shape.intersects(geo);
		}
		else return false;
	}

}
