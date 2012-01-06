/*
 * TropicalCycloneAdvisory
 * 
 * Date created 03 NOVEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tca;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Defines the intensity and geographical area for a specific Tropical cyclone
 * advisory.  
 * @author sgilbert
 *
 */
@XmlType(name = "", propOrder = {
	    "severity",
	    "advisoryType",
	    "geographyType",
	    "segment"	})
@XmlAccessorType(XmlAccessType.NONE)
public class TropicalCycloneAdvisory implements ISerializableObject {

	//  Typically, this indicates Tropical Storm or Hurricane
	@XmlElement
	private String severity;
	
	// advisory type can be a watch or warning
	@XmlElement
	private String advisoryType;
	
	@XmlElement
	private String geographyType;

	@XmlElements({
        @XmlElement(name = "coast", type = BreakpointPair.class),
        @XmlElement(name = "island", type = IslandBreakpoint.class),
       @XmlElement(name = "waterway", type = WaterBreakpoint.class) })
//	@XmlMixed
	private BPGeography segment;
//	private ArrayList<BPGeography> breakpoints;

	public TropicalCycloneAdvisory() {
		//no-arg
	}
	
	/**
	 * @param severity
	 * @param advisoryType
	 * @param geographyType
	 * @param breakpoints
	public TropicalCycloneAdvisory(String severity, String advisoryType,
			String geographyType, ArrayList<BPGeography> breakpoints) {
		super();
		this.severity = severity;
		this.advisoryType = advisoryType;
		this.geographyType = geographyType;
		this.breakpoints = breakpoints;
	}
	 */

	/**
	 * @return the severity
	 */
	public String getSeverity() {
		return severity;
	}

	/**
	 * @param severity
	 * @param advisoryType
	 * @param geographyType
	 * @param segment
	 */
	public TropicalCycloneAdvisory(String severity, String advisoryType,
			String geographyType, BPGeography segment) {
		super();
		this.severity = severity;
		this.advisoryType = advisoryType;
		this.geographyType = geographyType;
		this.segment = segment;
	}

	/**
	 * @param severity the severity to set
	 */
	public void setSeverity(String severity) {
		this.severity = severity;
	}

	/**
	 * @return the advisoryType
	 */
	public String getAdvisoryType() {
		return advisoryType;
	}

	/**
	 * @param advisoryType the advisoryType to set
	 */
	public void setAdvisoryType(String advisoryType) {
		this.advisoryType = advisoryType;
	}

	/**
	 * @return the geographyType
	 */
	public String getGeographyType() {
		return geographyType;
	}

	/**
	 * @param geographyType the geographyType to set
	 */
	public void setGeographyType(String geographyType) {
		this.geographyType = geographyType;
	}

	/**
	 * @return the segment
	 */
	public BPGeography getSegment() {
		return this.segment;
	}

	/**
	 * @param segment the segment to set
	 */
	public void setSegment(BPGeography segment) {
		this.segment = segment;
	}

	
	/**
	 * @return the breakpoints
	public ArrayList<BPGeography> getBreakpoints() {
		return breakpoints;
	}
	 */

	/**
	 * @param breakpoints the breakpoints to set
	public void setBreakpoints(ArrayList<BPGeography> breakpoints) {
		this.breakpoints = breakpoints;
	}
	 */
	
	/**
	 * Create a copy of this object
	 */
	public TropicalCycloneAdvisory copy() {
		String sevType = new String(this.getSeverity());
		String advType = new String(this.getAdvisoryType());
		String geogType = new String(this.getGeographyType());
		return new TropicalCycloneAdvisory( sevType, advType, geogType, this.getSegment());
	}

	/* (non-Javadoc)
	 * Determines whether this advisory has same attributes and breakpoints as another.
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {

		if ( obj == null ) return false;
		
		if ( obj instanceof TropicalCycloneAdvisory ) {
			TropicalCycloneAdvisory tca = (TropicalCycloneAdvisory)obj;
			
			if ( ! this.geographyType.equals( tca.getGeographyType() ) ) return false;
			
			if ( ! this.advisoryType.equals( tca.getAdvisoryType() ) ) return false;
			
			if ( ! this.severity.equals( tca.getSeverity() ) ) return false;
			
			List<Breakpoint> thislist = this.segment.getBreakpoints();
			List<Breakpoint> thatlist = tca.getSegment().getBreakpoints();
			if ( thislist.size() != thatlist.size() ) return false;
			
			for ( int j=0; j<thislist.size(); j++ ) {
				if ( ! thislist.get(j).equals( thatlist.get(j) ) ) return false;
			}
		}
		else
			return false;
		
		return true;
	}
	
	/**
	 * Determines whether the watch/warning segment of this advisory defined by the 
	 * pair of breakpoints overlaps the watch/warning segment of the given advisory
	 * @param tca
	 * @return
	 */
	public boolean overlaps( TropicalCycloneAdvisory tca ) {
		
		BreakpointManager bm = BreakpointManager.getInstance();
		
		if ( ! this.advisoryType.equals( tca.getAdvisoryType() ) ) return false;
		
		if ( ! this.severity.equals( tca.getSeverity() ) ) return false;
		
		if ( ! (this.segment instanceof BreakpointPair) ||
			 ! (tca.getSegment() instanceof BreakpointPair) )  return false;

		return bm.pairsOverlap( (BreakpointPair)this.segment, (BreakpointPair)tca.getSegment() );
	}
	
}
