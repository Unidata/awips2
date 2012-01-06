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

public class NsharpStationInfo {
	protected float latitude;
	protected float longitude;
	//protected float elevation;
	protected Timestamp reftime;
	protected Timestamp rangestarttime; //rangestart used by model sounding e.g. PFC sounding, or uair (same as Synoptictime in uair) 
	//protected List<Integer> dbId = new ArrayList<Integer>(); // used by observed data (e.g uair) only 
	protected String stnDisplayInfo;
	//protected String datauri;
	protected String sndType;
	
	
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
	//public String getDatauri() {
	//	return datauri;
	//}
	//public void setDatauri(String datauri) {
	////	this.datauri = datauri;
	//}
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
	//public List<Integer> getDbId() {
	//	return dbId;
	//}
	//public void setDbId(List<Integer> dbId) {
	//	this.dbId = dbId;
	//}
	public float getLatitude() {
		return latitude;
	}
	public void setLatitude(float latitude) {
		this.latitude = latitude;
	}
	public float getLongitude() {
		return longitude;
	}
	public void setLongitude(float longitude) {
		this.longitude = longitude;
	}
	//public float getElevation() {
	//	return elevation;
	//}
	//public void setElevation(float elevation) {
	//	this.elevation = elevation;
	//}

}
