/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.stormtrack.rsc;

/**
 * @author sgilbert
 *
 */
public class StormLocation implements Comparable<StormLocation> {

	private float latitude;
	
	private float longitude;
	 
	private int forecastHour;
	
	private float mslp;
	
	private float windMax;

	public StormLocation(float latitude, float longitude, int forecastHour,
			float mslp, float windMax) {
		this.latitude = latitude;
		this.longitude = longitude;
		this.forecastHour = forecastHour;
		this.mslp = mslp;
		this.windMax = windMax;
	}

	/**
	 * @return the latitude
	 */
	public float getLatitude() {
		return latitude;
	}

	/**
	 * @param latitude the latitude to set
	 */
	public void setLatitude(float latitude) {
		this.latitude = latitude;
	}

	/**
	 * @return the longitude
	 */
	public float getLongitude() {
		return longitude;
	}

	/**
	 * @param longitude the longitude to set
	 */
	public void setLongitude(float longitude) {
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
	public float getMslp() {
		return mslp;
	}

	/**
	 * @param mslp the mslp to set
	 */
	public void setMslp(float mslp) {
		this.mslp = mslp;
	}

	/**
	 * @return the windMax
	 */
	public float getWindMax() {
		return windMax;
	}

	/**
	 * @param windMax the windMax to set
	 */
	public void setWindMax(float windMax) {
		this.windMax = windMax;
	}

	@Override
	public int compareTo(StormLocation o) {
		return (this.forecastHour < o.forecastHour ? -1 : (this.forecastHour == o.forecastHour ? 0 : 1));
	}
	
}
