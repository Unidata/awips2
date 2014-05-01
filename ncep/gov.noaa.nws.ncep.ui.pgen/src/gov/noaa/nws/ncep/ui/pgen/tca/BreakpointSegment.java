/*
 * BreakpointSegment
 * 
 * Date created 27 OCTOBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tca;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * This class is used to identify a Breakpoint Geography segment where only one breakpoint
 * is specified, and the geography is assumed to be valid up until the next breakpoint in
 * a master coastal breakpoint list with in inherent geographical order proceeding along a
 * specific coast line.
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class BreakpointSegment extends BPGeography implements ISerializableObject {

	/*
	 * The breakpoint defining the beginning of the segment.
	 * The end of the segment is assumed to be the next breakpoint in the list.
	 */
	@XmlElement
	private Breakpoint breakpoint;

	/**
	 * 
	 */
	public BreakpointSegment() {
		super();
	}

	/**
	 * @return the breakpoint
	 */
	public Breakpoint getBreakpoint() {
		return breakpoint;
	}

	/**
	 * @param breakpoint the breakpoint to set
	 */
	public void setBreakpoint(Breakpoint breakpoint) {
		this.breakpoint = breakpoint;
	}

	@Override
	public List<Breakpoint> getBreakpoints() {
		List<Breakpoint> list = new ArrayList<Breakpoint>();
		list.add(breakpoint);
		return list;
	}
	
	
}
