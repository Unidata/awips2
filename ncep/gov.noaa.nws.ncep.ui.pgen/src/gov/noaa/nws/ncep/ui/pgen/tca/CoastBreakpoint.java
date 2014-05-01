/*
 * CoastBreakpoint
 * 
 * Date created 27 OCTOBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tca;

import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * This class is used to hold a list of Breakpoint Geography segments, where each breakpoint
 * contains the geography which is assumed to be valid up until the next breakpoint in
 * the list. The list of breakpoints has an inherent geographical order proceeding along a specific 
 * coast line thus defining the coast line.
 * Coasts can be identified as an island, when applicable, indicating that the last breakpoint is 
 * assumed to be just before the first breakpoint geographically, and therefore can be treated as a
 * circular list of breakpoints.
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class CoastBreakpoint implements ISerializableObject {

	@XmlAttribute
	private String name;
	
	@XmlAttribute
	private boolean island;
	
	@XmlElements({@XmlElement(name="segment")})
	private List<BreakpointSegment> segments;

	/**
	 * 
	 */
	public CoastBreakpoint() {
		super();
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
	 * @return the breakpoint segments
	 */
	public List<BreakpointSegment> getSegments() {
		return segments;
	}

	/**
	 * @param breakpoints the breakpoints to set
	 */
	public void setSegments(List<BreakpointSegment> segments) {
		this.segments = segments;
	}

	/**
	 * @return the island
	 */
	public boolean isIsland() {
		return island;
	}

	/**
	 * @param island the island to set
	 */
	public void setIsland(boolean island) {
		this.island = island;
	}

	
	
}
