package gov.noaa.nws.ncep.viz.customprojection;

public class GareaValues {
	private String abbreviationAreaName; 
	private double lowLeftLat; 
	private double lowLeftLon; 
	private double upperRightLat; 
	private double upperRightLon; 
	
	public String getAbbreviationAreaName() {
		return abbreviationAreaName;
	}
	public void setAbbreviationAreaName(String abbreviationAreaName) {
		this.abbreviationAreaName = abbreviationAreaName;
	}

	public double getLowLeftLat() {
		return lowLeftLat;
	}
	public void setLowLeftLat(double lowLeftLat) {
		this.lowLeftLat = lowLeftLat;
	}
	
	public double getLowLeftLon() {
		return lowLeftLon;
	}
	public void setLowLeftLon(double lowLeftLon) {
		this.lowLeftLon = lowLeftLon;
	}
	
	public double getUpperRightLat() {
		return upperRightLat;
	}
	public void setUpperRightLat(double upperRightLat) {
		this.upperRightLat = upperRightLat;
	}
	
	public double getUpperRightLon() {
		return upperRightLon;
	}
	public void setUpperRightLon(double upperRightLon) {
		this.upperRightLon = upperRightLon;
	}
}
