/*
 * CoastBreakpointList
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
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Contains a list of CoastBreakpoint objects defining multiple coast lines.
 * @author sgilbert
 *
 */
@XmlRootElement(name="coastBreakpoints")
@XmlAccessorType(XmlAccessType.NONE)
public class CoastBreakpointList implements ISerializableObject {


	@XmlElements({@XmlElement(name="coast")})
	private List<CoastBreakpoint> coasts;

	/**
	 * @return the coasts
	 */
	public List<CoastBreakpoint> getCoasts() {
		return coasts;
	}

	/**
	 * @param coasts the coasts to set
	 */
	public void setCoasts(List<CoastBreakpoint> coasts) {
		this.coasts = coasts;
	}

	public List<BreakpointSegment> getCoast(String name) {
		
		for ( CoastBreakpoint coast : coasts ) {
			if ( coast.getName().equals(name) ) {
				return coast.getSegments();
			}
		}
		
		return null;
	}
	
}
