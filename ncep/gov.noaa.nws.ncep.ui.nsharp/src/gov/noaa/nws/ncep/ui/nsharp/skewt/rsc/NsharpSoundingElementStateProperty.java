package gov.noaa.nws.ncep.ui.nsharp.skewt.rsc;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSoundingElementStateProperty
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

public class NsharpSoundingElementStateProperty {
	String elementDescription;
	State elementState; //  possible values are AVAIL,NOTAVAIL
						// NOTAVAIL is set when there is no sounding data loaded for this stn at this time line.
	String stnDescription;
	String timeDescription;
	NsharpStationInfo stnInfo;
	public NsharpSoundingElementStateProperty(String elementDescription,
			State elementState, String stnDescription, 
			String timeDescription,  NsharpStationInfo stnInfo) {
		super();
		this.elementDescription = elementDescription;
		this.elementState = elementState;
		this.stnDescription = stnDescription;
		this.timeDescription = timeDescription;
		this.stnInfo = stnInfo;
	}
	
	public NsharpSoundingElementStateProperty() {
		super();
		// TODO Auto-generated constructor stub
	}

	public String getElementDescription() {
		return elementDescription;
	}
	public void setElementDescription(String elementDescription) {
		this.elementDescription = elementDescription;
	}
	public State getElementState() {
		return elementState;
	}
	public void setElementState(State elementState) {
		this.elementState = elementState;
	}
	public String getStnDescription() {
		return stnDescription;
	}
	public void setStnDescription(String stnDescription) {
		this.stnDescription = stnDescription;
	}
	
	public String getTimeDescription() {
		return timeDescription;
	}
	public void setTimeDescription(String timeDescription) {
		this.timeDescription = timeDescription;
	}
	
	public NsharpStationInfo getStnInfo() {
		return stnInfo;
	}
	public void setStnInfo(NsharpStationInfo stnInfo) {
		this.stnInfo = stnInfo;
	}

	public void copy(NsharpSoundingElementStateProperty target){
		elementDescription = target.getElementDescription();
		elementState= target.getElementState();
		stnDescription=target.getStnDescription();
		stnInfo = target.getStnInfo();
		timeDescription=target.getTimeDescription();
	}
	
}