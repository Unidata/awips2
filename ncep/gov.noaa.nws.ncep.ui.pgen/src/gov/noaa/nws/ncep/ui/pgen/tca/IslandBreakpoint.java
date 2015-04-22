/*
 * IslandBreakpoint
 * 
 * Date created 03 NOVEMBER 2009
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
 * This class identifies an entire island as a breakpoint.  The geography (drawing path and
 * land zones) are assumed to be valid for the entire Island defined by the breakpoint.
 * @author sgilbert
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class IslandBreakpoint extends BPGeography implements ISerializableObject {

	@XmlElement
	private Breakpoint breakpoint;

	/**
	 * 
	 */
	public IslandBreakpoint() {
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
