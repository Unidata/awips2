/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.stormtrack.rsc;

/**
 * @author sgilbert
 *
 */
public class StormLocation implements Comparable<StormLocation> {

	private double latitude;
	
	private double longitude;
	 
	private int forecastHour;
	
	private double mslp;
	
	private double windMax;

	public StormLocation(double latitude, double longitude, int forecastHour,
			double mslp, double windMax) {
		this.latitude = latitude;
		this.longitude = longitude;
		this.forecastHour = forecastHour;
		this.mslp = mslp;
		this.windMax = windMax;
	}

	/**
	 * @return the latitude
	 */
	public double getLatitude() {
		return latitude;
	}

	/**
	 * @param latitude the latitude to set
	 */
	public void setLatitude(double latitude) {
		this.latitude = latitude;
	}

	/**
	 * @return the longitude
	 */
	public double getLongitude() {
		return longitude;
	}

	/**
	 * @param longitude the longitude to set
	 */
	public void setLongitude(double longitude) {
		this.longitude = longitude;
	}

	/**
	 * @return the forecastHour
	 */
	public int getForecastHour() {
		return forecastHour;
	}

	/**
	 * @param forecastHour the forecastHour to set
	 */
	public void setForecastHour(int forecastHour) {
		this.forecastHour = forecastHour;
	}

	/**
	 * @return the mslp
	 */
	public double getMslp() {
		return mslp;
	}

	/**
	 * @param mslp the mslp to set
	 */
	public void setMslp(double mslp) {
		this.mslp = mslp;
	}

	/**
	 * @return the windMax
	 */
	public double getWindMax() {
		return windMax;
	}

	/**
	 * @param windMax the windMax to set
	 */
	public void setWindMax(double windMax) {
		this.windMax = windMax;
	}

	@Override
	public int compareTo(StormLocation o) {
		return (this.forecastHour < o.forecastHour ? -1 : (this.forecastHour == o.forecastHour ? 0 : 1));
	}
	
}
