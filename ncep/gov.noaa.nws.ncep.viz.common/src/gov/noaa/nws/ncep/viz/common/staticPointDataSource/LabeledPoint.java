package gov.noaa.nws.ncep.viz.common.staticPointDataSource;

import java.util.HashMap;
import java.util.Map;

public class LabeledPoint {

	private String  name = null; // set if only one label or create a name from the set of labels
	
	// allow for multiple labels. ex. for a StationsTable these would be the 
	// values in IStationField
	private Map<String,String> labels=null; // ex. stationId="xxxx", icao="MCO"

	private double latitude  = Double.NaN;

	private double longitude  = Double.NaN;
	
	public LabeledPoint( String n, double lat, double lon ) {
		name = n;
		latitude = lat;
		longitude = lon;
	}

	public void addLabel( String lblName, String lblStr ) {
	    if( labels == null ) {
	    	labels = new HashMap<String,String>();
	    }
	    labels.put( lblName, lblStr );
	}
	
	public Map<String,String> getLabels() {
		return labels;
	}
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
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
