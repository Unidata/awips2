package gov.noaa.nws.ncep.viz.ui.locator.resource;

public class LocatorDisplayAttributes {
	
	private String locatorSource;

	private Integer roundToNearest=1;

	private String distanceUnit;

	private String directionUnit;

	public LocatorDisplayAttributes( ) {
		locatorSource = null;
		roundToNearest = 1;
		distanceUnit = null;
		directionUnit = null;
	}

	public LocatorDisplayAttributes( String src, Integer round, String distUnit, String dirUnit ) {
		locatorSource = src;
		roundToNearest = round;
		distanceUnit = distUnit;
		directionUnit = dirUnit;
	}

	public String getLocatorSource() {
		return locatorSource;
	}

	public void setLocatorSource(String locatorSource) {
		this.locatorSource = locatorSource;
	}

	public Integer getRoundToNearest() {
		return roundToNearest;
	}

	public void setRoundToNearest(Integer roundToNearest) {
		this.roundToNearest = roundToNearest;
	}

	public String getDistanceUnit() {
		return distanceUnit;
	}

	public void setDistanceUnit(String distanceUnit) {
		this.distanceUnit = distanceUnit;
	}

	public String getDirectionUnit() {
		return directionUnit;
	}

	public void setDirectionUnit(String directionUnit) {
		this.directionUnit = directionUnit;
	}


}
