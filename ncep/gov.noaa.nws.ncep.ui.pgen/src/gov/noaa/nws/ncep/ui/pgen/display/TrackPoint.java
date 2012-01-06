/*
 * TrackPoint
 * 
 * Date created: 06 April 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import gov.noaa.nws.ncep.ui.pgen.PGenRuntimeException;

import java.util.Calendar;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * The Track point class is used to storm the lat/lon location of a storm with its
 * actual date/time.
 * @author sgilbert
 *
 */
public class TrackPoint {

	/**
	 *  Lat, lon coordinate of the storm
	 */
	private Coordinate location;
	
	/**
	 * date/time
	 */
	private Calendar time;

	/**
	 * Constructor to set both location and date/time
	 * @param location
	 * @param time
	 */
	public TrackPoint(Coordinate location, Calendar time) {
		this.location = location;
		this.time = time;
	}

	/**
	 * Gets the lat/lon coordinate of the storm
	 * @return the storm location
	 */
	public Coordinate getLocation() {
		return location;
	}

	/**
	 * Sets the lat/lon coordinates of the storm
	 * @param storm location to set
	 */
	public void setLocation(Coordinate location) {
		this.location = location;
	}

	/**
	 * Gets the date/time at that location
	 * @return the date/time
	 */
	public Calendar getTime() {
		return time;
	}

	/**
	 * Sets the storm location date/time
	 * @param time the time to set
	 */
	public void setTime(Calendar time) {
		this.time = time;
	}
	
	public static TrackPoint clone(Coordinate location, Calendar time) {
		if(location == null)
			throw new PGenRuntimeException("Class: TrackPoint, invalid input paremeter, "+
					" Coordinate location is NULL"); 
		Coordinate newCoordinate = new Coordinate(); 
		newCoordinate.x = location.x; 
		newCoordinate.y = location.y; 

		Calendar newCalendar = null; 
		if(time != null) {
			newCalendar = Calendar.getInstance(); 
			newCalendar.setTimeInMillis(time.getTimeInMillis());
		}
		return new TrackPoint(newCoordinate, newCalendar); 
	}
}
