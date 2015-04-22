/*
 * BreakpointPair
 * 
 * Date created 27 OCTOBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tca;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlElement;

/**
 * This class is used to identify a Breakpoint Geography that is associated with 
 * a pair of breakpoints that define the two endpoints of the segment along a coast line..
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class BreakpointPair extends BPGeography {
	
	/*
	 * The two breakpoints associated with this geography
	 */
	@XmlElement
	private List<Breakpoint> breakpoints;
	
	/**
	 * 
	 */
	public BreakpointPair() {
		super();
		breakpoints = new ArrayList<Breakpoint>();
	}


	/**
	 * @param breakpoints
	 */
	public BreakpointPair(List<Breakpoint> breakpoints) {
		super();
		this.breakpoints = breakpoints;
	}


	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.tca.BPGeography#getBreakpoints()
	 */
	@Override
	public List<Breakpoint> getBreakpoints() {
		return breakpoints;
	}
	
	public void addBreakpoint( Breakpoint bkpt) {
		breakpoints.add(bkpt);
	}

}
