package gov.noaa.nws.ncep.ui.nctextui.dbutil;

import java.util.List;

public class NctextStationInfo {
	protected String productid; //WMO id
	protected String stnid;
	protected String stnname;
	protected String state;
	protected String country;
	protected double latitude;
	protected double longitude;
	protected Integer elevation;
	
	
	public String getProductid() {
		return productid;
	}
	public void setProductid(String productid) {
		this.productid = productid;
	}
	public String getStnid() {
		return stnid;
	}
	public void setStnid(String stnid) {
		this.stnid = stnid;
	}
	public String getStnname() {
		return stnname;
	}
	public void setStnname(String stnname) {
		this.stnname = stnname;
	}
	public String getState() {
		return state;
	}
	public void setState(String state) {
		this.state = state;
	}
	public String getCountry() {
		return country;
	}
	public void setCountry(String country) {
		this.country = country;
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
	public Integer getElevation() {
		return elevation;
	}
	public void setElevation(Integer elevation) {
		this.elevation = elevation;
	}

}
