/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo
 * 
 * This java class defines NSHARP NsharpStationInfo functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/26/2010	229			Chin Chen	Initial coding
 * 12/16/2010   362         Chin Chen   add support of BUFRUA observed sounding and PFC (NAM and GFS) model sounding data
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

public class NsharpStationInfo {
	public class timeLineSpecific {
		Timestamp tiemLine;
		String displayInfo;
		public Timestamp getTiemLine() {
			return tiemLine;
		}
		public void setTiemLine(Timestamp tiemLine) {
			this.tiemLine = tiemLine;
		}
		public String getDisplayInfo() {
			return displayInfo;
		}
		public void setDisplayInfo(String displayInfo) {
			this.displayInfo = displayInfo;
		}
		
	}
	protected List<timeLineSpecific> timeLineSpList = new ArrayList<timeLineSpecific>(); 
	//Chin PER protected float latitude;
	//protected float longitude;
	protected double latitude;
	protected double longitude;
	protected Timestamp reftime;  //uair (same as Synoptictime in uair)
	protected Timestamp rangestarttime; //rangestart used by model sounding e.g. PFC sounding, or uair (same as Synoptictime in uair) 
	protected String stnDisplayInfo;
	protected String sndType;
	protected String stnId;
	
	
	
	public String getStnId() {
		return stnId;
	}
	public void setStnId(String stnId) {
		this.stnId = stnId;
	}
	public List<timeLineSpecific> getTimeLineSpList() {
		return timeLineSpList;
	}
	public void setTimeLineSpList(List<timeLineSpecific> timeLineSpList) {
		this.timeLineSpList = timeLineSpList;
	}
	public void addToTimeLineSpList(timeLineSpecific timeLineSpInfo,int index ) {
		this.timeLineSpList.add(index,timeLineSpInfo);
	}
	public void addToTimeLineSpList(timeLineSpecific timeLineSpInfo) {
		this.timeLineSpList.add(timeLineSpInfo);
	}
	public String getSndType() {
		return sndType;
	}
	public void setSndType(String sndType) {
		this.sndType = sndType;
	}
	public Timestamp getRangestarttime() {
		return rangestarttime;
	}
	public void setRangestarttime(Timestamp rangestarttime) {
		this.rangestarttime = rangestarttime;
	}
	public Timestamp getReftime() {
		return reftime;
	}
	public void setReftime(Timestamp reftime) {
		this.reftime = reftime;
	}
	public String getStnDisplayInfo() {
		return stnDisplayInfo;
	}
	public void setStnDisplayInfo(String stnDisplayInfo) {
		this.stnDisplayInfo = stnDisplayInfo;
	}
	public double getLatitude() {
		return latitude;
	}
	public void setLatitude(double latitude) {
		this.latitude = latitude;
	}
	public double getLongitude() {
		return longitude;
	}
	public void setLongitude(double longitude) {
		this.longitude = longitude;
	}
	
}
