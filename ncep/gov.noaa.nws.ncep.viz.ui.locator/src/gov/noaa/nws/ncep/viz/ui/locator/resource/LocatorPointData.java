package gov.noaa.nws.ncep.viz.ui.locator.resource;

/**
 * 
 * Class defines locator point data
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/14/2009    #48        M. Li     Initial version
 * 
 * 
 * * @author mli
 * * @version 1.0
 * 
 */
public class LocatorPointData {

	private String name = null;
	
	private String StateID = null;
	
	private double lat  = -999.99;
	
	private double lon  = -999.99;
	
	private double dir  = -999.99;
	
	private int distanceInMeter = -999;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getStateID() {
		return StateID;
	}

	public void setStateID(String stateID) {
		StateID = stateID;
	}

	public double getLat() {
		return lat;
	}

	public void setLat(double lat) {
		this.lat = lat;
	}

	public double getLon() {
		return lon;
	}

	public void setLon(double lon) {
		this.lon = lon;
	}

	public int getDistanceInMeter() {
		return distanceInMeter;
	}

	public void setDistanceInMeter(int distanceInMeter) {
		this.distanceInMeter = distanceInMeter;
	}
	
	public double getDir() {
		return dir;
	}

	public void setDir(double dir) {
		this.dir = dir;
	}
}
