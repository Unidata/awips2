/*
 * BreakpointList
 * 
 * Date created 23 OCTOBER 2009
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
 * a list of (typically more than two) breakpoints.  
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class BreakpointList extends BPGeography {
	
	/*
	 * breakpoints associated with this geography
	 */
	@XmlElement
	private List<Breakpoint> breakpoints;
	
	/**
	 * 
	 */
	public BreakpointList() {
		super();
		breakpoints = new ArrayList<Breakpoint>();
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
