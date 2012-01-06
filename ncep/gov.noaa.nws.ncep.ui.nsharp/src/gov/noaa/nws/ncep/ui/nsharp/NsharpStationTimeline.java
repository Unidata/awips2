package gov.noaa.nws.ncep.ui.nsharp;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpStationTimeline
 * 
 * This java class defines NSHARP NsharpStationInfo functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/12/2011	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
public class NsharpStationTimeline {
	private String timeline;
	private String stnName;
	public String getTimeline() {
		return timeline;
	}
	public void setTimeline(String timeline) {
		this.timeline = timeline;
	}
	public String getStnName() {
		return stnName;
	}
	public void setStnName(String stnName) {
		this.stnName = stnName;
	}
	
}
