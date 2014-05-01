/*
 * InterpolationProperties
 * 
 * Date created: 19 AUGUST 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

/**
 * Properties used with the PgenInterpolator
 * @author sgilbert
 *
 */
public class InterpolationProperties {

	/*
	 * start time associated with the first selected element
	 */
	private int startingTime;
	
	/*
	 * end time associated with the second selected element
	 */
	private int endingTime;
	
	/*
	 * time interval between all interpolated elements
	 */
	private int interval;
	
	/**
	 * Constructor
	 * @param startingTime start time associated with the first selected element
	 * @param endingTime end time associated with the second selected element
	 * @param interval time interval between all interpolated elements
	 */
	public InterpolationProperties(int startingTime, int endingTime,
			int interval) {
		super();
		this.startingTime = startingTime;
		this.endingTime = endingTime;
		this.interval = interval;
	}

	/**
	 * Gets the start time
	 * @return
	 */
	public int getStartingTime() {
		return startingTime;
	}

	/**
	 * Sets the start time
	 * @param startingTime
	 */
	public void setStartingTime(int startingTime) {
		this.startingTime = startingTime;
	}

	/**
	 * Gets the end time
	 * @return
	 */
	public int getEndingTime() {
		return endingTime;
	}

	/**
	 * Sets the end time
	 * @param endingTime
	 */
	public void setEndingTime(int endingTime) {
		this.endingTime = endingTime;
	}

	/**
	 * Gets the time interval
	 * @return
	 */
	public int getInterval() {
		return interval;
	}

	/**
	 * Sets the time interval
	 * @param interval
	 */
	public void setInterval(int interval) {
		this.interval = interval;
	}
	
	
	
}
