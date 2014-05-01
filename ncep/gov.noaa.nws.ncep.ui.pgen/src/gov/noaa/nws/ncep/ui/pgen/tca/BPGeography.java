/*
 * BPGeography
 * 
 * Date created 23 OCTOBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

import gov.noaa.nws.ncep.ui.pgen.display.CoordinateArrayAdapter;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.vividsolutions.jts.geom.Coordinate;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Base class for all the representations of tropical cyclone breakpoints.
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class BPGeography implements ISerializableObject {

	/**
	 * List of lat/lon coordinate pairs that are used to draw/display the breakpoint
	 * area or segment
	 */
	@XmlElement(name="path")
    @XmlJavaTypeAdapter(value = CoordinateArrayAdapter.class)
	private List<Coordinate[]> paths;

	/**
	 * A list of UGC LandZone codes identified with this breakpoint
	 */
	@XmlElement(name="landZones")
	@XmlList
	private List<String> zones;
	
	/**
	 * Default constructor - creates Lists - should be extended by subclasses.
	 */
	public BPGeography() {
		paths = new ArrayList<Coordinate[]>();
		zones = new ArrayList<String>();
	}

	/**
	 * @return the lat/lon coordinate paths
	 */
	public List<Coordinate[]> getPaths() {
		return paths;
	}

	/**
	 * @param paths the lat/lon coordinate paths to set
	 */
	public void setPaths(List<Coordinate[]> paths) {
		this.paths = paths;
	}
	
	/**
	 * Gets a list of breakpoints associated with this geography
	 * @return list of breakpoints
	 */
	public abstract List<Breakpoint> getBreakpoints();
	
	/**
	 * Add a path to the list of paths
	 * @param coords lat/lon coordinate pairs defining a path
	 */
	public void addPath(Coordinate[] coords) {
		paths.add(coords);
	}

	/**
	 * @return the list of land zone codes
	 */
	public List<String> getZones() {
		return zones;
	}

	/**
	 * @param zones the list of land zone codes to set
	 */
	public void setZones(List<String> zones) {
		this.zones = zones;
	}
	
	/**
	 * @param zones the list of land zones to add to the current list
	 */
	public void addZones(List<String> zones) {
		this.zones.addAll( zones );
	}
	
}
