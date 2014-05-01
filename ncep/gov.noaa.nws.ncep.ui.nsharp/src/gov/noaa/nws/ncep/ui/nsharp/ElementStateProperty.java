/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/23/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants.State;

public class ElementStateProperty {
	String elementDescription;
	State elementState;
	NsharpStationInfo stnInfo;
	public NsharpStationInfo getStnInfo() {
		return stnInfo;
	}
	public void setStnInfo(NsharpStationInfo stnInfo) {
		this.stnInfo = stnInfo;
	}
	public String getElementDescription() {
		return elementDescription;
	}
	public State getElementState() {
		return elementState;
	}
	public void setElementDescription(String elementDescription) {
		this.elementDescription = elementDescription;
	}
	public void setElementState(State elementState) {
		this.elementState = elementState;
	}
	
}
