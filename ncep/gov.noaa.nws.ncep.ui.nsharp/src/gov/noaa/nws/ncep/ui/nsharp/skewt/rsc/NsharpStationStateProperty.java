package gov.noaa.nws.ncep.ui.nsharp.skewt.rsc;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpStationStateProperty
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

import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource.State;

public class NsharpStationStateProperty {
	String stnDescription;
	State stnState; //possible values are ACTIVE, INACTIVE. set by User. Default is Active
	NsharpStationInfo stnInfo;
	public NsharpStationStateProperty(String stnDescription,
			State stnState, NsharpStationInfo stnInfo) {
		super();
		this.stnDescription = stnDescription;
		this.stnState = stnState;
		this.stnInfo = stnInfo;
	}
	public String getStnDescription() {
		return stnDescription;
	}
	public void setStnDescription(String stnDescription) {
		this.stnDescription = stnDescription;
	}
	public State getStnState() {
		return stnState;
	}
	public void setStnState(State stnState) {
		this.stnState = stnState;
	}
	public NsharpStationInfo getStnInfo() {
		return stnInfo;
	}
	public void setStnInfo(NsharpStationInfo stnInfo) {
		this.stnInfo = stnInfo;
	}

}
