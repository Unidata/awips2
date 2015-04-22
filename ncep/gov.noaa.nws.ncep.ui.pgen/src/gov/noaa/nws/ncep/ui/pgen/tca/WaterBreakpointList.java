/*
 * WaterBreakpointList
 * 
 * Date created 03 NOVEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tca;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * This class contains a list of many waterway breakpoints
 * @author sgilbert
 *
 */
@XmlRootElement(name="waterBreakpoints")
@XmlAccessorType(XmlAccessType.NONE)
public class WaterBreakpointList implements ISerializableObject {


	@XmlElement(name="waterway")
	private List<WaterBreakpoint> breakpoints;

	/**
	 * @return the waterway breakpoints
	 */
	public List<WaterBreakpoint> getWaterways() {
		return breakpoints;
	}

	/**
	 * @param breakpoints the list of waterway breakpoints
	 */
	public void setWaterways(List<WaterBreakpoint> breakpoints) {
		this.breakpoints = breakpoints;
	}

	
}
