package gov.noaa.nws.ncep.ui.nctextui.dbutil;

public enum EReportTimeRange {

	NONE(0), ONE_HOUR (1), THREE_HOURS(3), SIX_HOURS(6), TWELVE_HOURS(12), TWENTYFOUR_HOURS(24), FORTYEIGHT_HOURS(48);
	private int timeRangeVal;

	// Constructor 
	EReportTimeRange(int p) { timeRangeVal = p; } 

	// Overloaded constructor 
	EReportTimeRange() { timeRangeVal = -1; } 

	public int getTimeRange() { return timeRangeVal; } 
}
