/*
 * Breakpoint
 * 
 * Date created 23 OCTOBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.CoordAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Breakpoints are geographic defining points specified in tropical cyclone
 * watches and warnings.  (See NWSI 10-605 @ http://www.nws.noaa.gov/directives
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class Breakpoint implements ISerializableObject {

	/*
	 * breakpoint name
	 */
	@XmlAttribute
	private String name;
	
	@XmlAttribute
	private String state;
	
	@XmlAttribute
	private String country;
	
	@XmlAttribute
    @XmlJavaTypeAdapter(value = CoordAdapter.class)
	private Coordinate location;

	/*
	 * indicates whether this breakpoint is considered official
	 */
	@XmlAttribute
	private boolean official;
	
	
	/**
	 * default constructor
	 */
	public Breakpoint() {
	}

	/**
	 * @param name  Breakpoint name 
	 * @param state
	 * @param country
	 * @param location lat/lon coordinate
	 * @param official boolean
	 */
	public Breakpoint(String name, String state, String country, Coordinate location, boolean official) {
		this.name = name;
		this.state = state;
		this.country = country;
		this.location = location;
		this.official = official;
	}

	/**
	 * @return the name of the breakpoint
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name the breakpoint name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the location
	 */
	public Coordinate getLocation() {
		return location;
	}

	/**
	 * @param location the location to set
	 */
	public void setLocation(Coordinate location) {
		this.location = location;
	}

	/**
	 * @return the state
	 */
	public String getState() {
		return state;
	}

	/**
	 * @param state the state to set
	 */
	public void setState(String state) {
		this.state = state;
	}

	/**
	 * @return the country
	 */
	public String getCountry() {
		return country;
	}

	/**
	 * @param country the country to set
	 */
	public void setCountry(String country) {
		this.country = country;
	}

	/**
	 * @return the official
	 */
	public boolean isOfficial() {
		return official;
	}

	/**
	 * @param official the official to set
	 */
	public void setOfficial(boolean official) {
		this.official = official;
	}

	/**
	 * Two breakpoints are considered "equal" if they have the same Name.
	 */
	@Override
	public boolean equals(Object obj) {

		boolean retval = false;
		
		if ( obj == null ) return false;
		
		if ( obj instanceof Breakpoint ) {
			Breakpoint tmp = (Breakpoint)obj;
			if ( this.getName().equals(tmp.getName()) ) retval = true;
		}
		
		return retval;
		
	}
	
}
