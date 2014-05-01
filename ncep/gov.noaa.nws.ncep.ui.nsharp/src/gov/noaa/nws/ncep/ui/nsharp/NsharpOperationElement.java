package gov.noaa.nws.ncep.ui.nsharp;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants.ActState;


/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpSoundingStateProperty
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 06/26/2013    			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */


public class NsharpOperationElement {
	public String elementDescription;
	public NsharpConstants.ActState actionState; //possible values are ACTIVE, INACTIVE, CURRENT. set by User. Default is Active
	public NsharpOperationElement(String elementDescription,
			ActState actionState) {
		super();
		this.elementDescription = elementDescription;
		this.actionState = actionState;
	}
	public String getElementDescription() {
		return elementDescription;
	}
	public void setElementDescription(String elementDescription) {
		this.elementDescription = elementDescription;
	}
	public NsharpConstants.ActState getActionState() {
		return actionState;
	}
	public void setActionState(NsharpConstants.ActState actionState) {
		this.actionState = actionState;
	}

}
