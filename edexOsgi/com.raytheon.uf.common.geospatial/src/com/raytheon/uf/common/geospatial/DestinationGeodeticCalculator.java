package com.raytheon.uf.common.geospatial;

import java.awt.geom.Point2D;

import org.geotools.referencing.GeodeticCalculator;

/**
 * 
 * Used ONLY for finding the destination points that have distances beyond the
 * maxOrthodromicDistance of a standard GeodeticCalculator
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class DestinationGeodeticCalculator {
	private static final double MAX_DIST = 20000000;

	private double azimuth;

	private double distance;

	private double startLongitude;

	private double startLatitude;

	private double destLongitude;

	private double destLatitude;

	private boolean validDistance;

	private boolean validDestination;

	private GeodeticCalculator gc = new GeodeticCalculator();

	/**
	 * Set the azimuth and the distance.
	 * 
	 * @param azimuth
	 *            The azimuth in decimal degrees from -180° to 180°.
	 * @param distance
	 *            The orthodromic distance in the same units as the ellipsoid
	 *            axis.
	 * 
	 */
	public void setDirection(double azimuth, double distance)
			throws IllegalArgumentException {
		this.azimuth = checkAzimuth(azimuth);
		this.distance = distance;
		validDistance = true;
		validDestination = false;
	}

	/**
	 * Returns the orthodromic distance
	 */
	public double getOrthodromicDistance() {
		if (!validDistance) {
			GeodeticCalculator gc = new GeodeticCalculator();
			gc.setStartingGeographicPoint(startLongitude, startLatitude);
			gc.setDestinationGeographicPoint(destLongitude, destLatitude);
			distance = gc.getOrthodromicDistance();
		}
		return distance;
	}

	/**
	 * Sets starting point
	 */
	public void setStartingGeographicPoint(double lon, double lat)
			throws IllegalArgumentException {
		this.startLongitude = checkLongitude(lon);
		this.startLatitude = checkLatitude(lat);
	}

	/**
	 * Sets starting point
	 */
	public void setStartingGeographicPoint(Point2D point)
			throws IllegalArgumentException {
		setStartingGeographicPoint(point.getX(), point.getY());
	}

	/**
	 * Sets destination point
	 */
	public void setDestinationGeographicPoint(double lon, double lat)
			throws IllegalArgumentException {
		this.destLongitude = checkLongitude(lon);
		this.destLatitude = checkLatitude(lat);
		validDestination = true;
		validDistance = false;
	}

	/**
	 * Sets destination point
	 */
	public void setDestinationGeographicPoint(Point2D point)
			throws IllegalArgumentException {
		setStartingGeographicPoint(point.getX(), point.getY());
	}

	/**
	 * Returns the destination point.
	 */
	public Point2D getDestinationGeographicPoint() {
		gc.setStartingGeographicPoint(startLongitude, startLatitude);

		if (validDestination) {
			gc.setDestinationGeographicPoint(destLongitude, destLatitude);
			return gc.getDestinationGeographicPoint();
		}

		Point2D referenceGeographicPoint = gc.getStartingGeographicPoint();
		double dist = distance;
		while (dist > MAX_DIST) {
			gc.setStartingGeographicPoint(referenceGeographicPoint);
			gc.setDirection(azimuth, MAX_DIST);
			referenceGeographicPoint = gc.getDestinationGeographicPoint();
			dist -= MAX_DIST;
		}

		gc.setStartingGeographicPoint(referenceGeographicPoint);
		gc.setDirection(azimuth, dist);

		return gc.getDestinationGeographicPoint();
	}

	private static double checkLongitude(final double longitude)
			throws IllegalArgumentException {
		if (longitude >= -180 && longitude <= 180) {
			return longitude;
		}
		throw new IllegalArgumentException("Longitude out of range: "
				+ longitude + ". Must be between -180 and 180");
	}

	private static double checkLatitude(final double latitude)
			throws IllegalArgumentException {
		if (latitude >= -90 && latitude <= 90) {
			return latitude;
		}
		throw new IllegalArgumentException("Latitude out of range: " + latitude
				+ ". Must be between -90 and 90");

	}

	private static double checkAzimuth(final double azimuth)
			throws IllegalArgumentException {
		if (azimuth >= -180.0 && azimuth <= 180.0) {
			return azimuth;
		}
		throw new IllegalArgumentException("Azimuth out of range: " + azimuth
				+ ". Must be between -180 and 180");
	}
}
