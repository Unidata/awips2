package gov.noaa.nws.ncep.ui.nsharp.skewt.rsc;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpTimeLineStateProperty
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

import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource.State;

public class NsharpTimeLineStateProperty {
	String timeDescription;
	State timeState;//possible values are ACTIVE, INACTIVE. set by User. Default is Active
	public NsharpTimeLineStateProperty(String timeDescription, State timeState) {
		super();
		this.timeDescription = timeDescription;
		this.timeState = timeState;
	}
	public String getTimeDescription() {
		return timeDescription;
	}
	public void setTimeDescription(String timeDescription) {
		this.timeDescription = timeDescription;
	}
	public State getTimeState() {
		return timeState;
	}
	public void setTimeState(State timeState) {
		this.timeState = timeState;
	}

}
