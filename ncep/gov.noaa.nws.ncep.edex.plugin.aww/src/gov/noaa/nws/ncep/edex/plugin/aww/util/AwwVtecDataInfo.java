package gov.noaa.nws.ncep.edex.plugin.aww.util;

import java.util.Calendar;

public class AwwVtecDataInfo {
	private Calendar eventStartTime, eventEndTime, awwRecordIssueTime; 
	private String action; 
	private Integer vtecRecordId; 
	
	public Calendar getAwwRecordIssueTime() {
		return awwRecordIssueTime;
	}
	public void setAwwRecordIssueTime(Calendar awwRecordIssueTime) {
		this.awwRecordIssueTime = awwRecordIssueTime;
	}
	
	public String getAction() {
		return action;
	}
	public void setAction(String action) {
		this.action = action;
	}
	
	public Integer getVtecRecordId() {
		return vtecRecordId;
	}
	public void setVtecRecordId(Integer vtecRecordId) {
		this.vtecRecordId = vtecRecordId;
	}
	
	public Calendar getEventStartTime() {
		return eventStartTime;
	}
	public void setEventStartTime(Calendar eventStartTime) {
		this.eventStartTime = eventStartTime;
	}

	public Calendar getEventEndTime() {
		return eventEndTime;
	}
	public void setEventEndTime(Calendar eventEndTime) {
		this.eventEndTime = eventEndTime;
	}

}
