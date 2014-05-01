package gov.noaa.nws.ncep.viz.ui.seek;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Class defines click point data in Seek.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * March  2009    #86        M. Li     Initial version
 * 
 * 
 * * @author mli
 * * @version 1.0
 * 
 */

public class ClickPointData {

	private String LocatorName = null;
	
	private Coordinate coord = null;
	
	private double direction = -999.99;
	
	private String text = null;
	
	boolean isActivated = false;
	
	public String getLocatorName() {
		return LocatorName;
	}

	public void setLocatorName(String locatorName) {
		LocatorName = locatorName;
	}

	public Coordinate getCoord() {
		return coord;
	}

	public void setCoord(Coordinate coord) {
		this.coord = coord;
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	public boolean isActivated() {
		return isActivated;
	}

	public void setActivated(boolean isActivated) {
		this.isActivated = isActivated;
	}

	public double getDirection() {
		return direction;
	}

	public void setDirection(double direction) {
		this.direction = direction;
	}

}
