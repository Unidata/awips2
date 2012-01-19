/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.decodertools.core;

/**
 * BasePoint provides a simple 4D (latitude, longitude, elevation, and time)
 * base class for observation data. Sub-classes of BasePoint are responsible for
 * adding additional checking or using specific units as required. An example
 * would be adding a accessors for a Calendar to expose the date information.
 * Most attributes of this class do not have units.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27 July 2007        411 jkorman     Initial Development
 * 20070911            379 jkorman     Added comments.
 * 20070912            379 jkorman     Code review cleanup.
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public class BasePoint {
    private double latitude;

    private double longitude;

    private double elevation;

    private int year;

    private int month;

    private int day;

    private int hour;

    private int minute;

    private int second;

    private int millis;

    /**
     * Create an empty basepoint instance.
     */
    public BasePoint() {
    }

    /**
     * Copy constructor for BasePoint.
     * 
     * @param point
     *            A basepoint to copy.
     */
    public BasePoint(BasePoint point) {
        latitude = point.latitude;
        longitude = point.longitude;
        elevation = point.elevation;
        year = point.year;
        month = point.month;
        day = point.day;
        hour = point.hour;
        minute = point.minute;
        second = point.second;
        millis = point.millis;
    }

    /**
     * Create a base point at a given location.
     * 
     * @param latitude
     * @param longitude
     */
    public BasePoint(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * Get the latitude of this point.
     * 
     * @return The latitude.
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * Set the latitude for this point.
     * 
     * @param latitude
     *            The latitude.
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Get the longitude of this point.
     * 
     * @return The longitude.
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * Set the longitude for this point.
     * 
     * @param longitude
     *            The longitude.
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Get the elevation of this point. No units are specified.
     * 
     * @return The elevation of this point.
     */
    public double getElevation() {
        return elevation;
    }

    /**
     * Set the elevation of this point. No units are specified.
     * 
     * @param elevation
     *            The elevation of this point.
     */
    public void setElevation(double elevation) {
        this.elevation = elevation;
    }

    /**
     * Get the year part of the date.
     * 
     * @return The year.
     */
    public int getYear() {
        return year;
    }

    /**
     * Set the year part of the date.
     * 
     * @param year
     *            Year.
     */
    public void setYear(int year) {
        this.year = year;
    }

    /**
     * Get the month part of the date.
     * 
     * @return The month of the year.
     */
    public int getMonth() {
        return month;
    }

    /**
     * Set the day of the month part of the date.
     * 
     * @param day
     *            Day of month [1..12].
     */
    public void setMonth(int month) {
        this.month = month;
    }

    /**
     * Get the day part of the month.
     * 
     * @return The day of the month.
     */
    public int getDay() {
        return day;
    }

    /**
     * Set the day of the month part of the date.
     * 
     * @param day
     *            Day of month [1..12].
     */
    public void setDay(int day) {
        this.day = day;
    }

    /**
     * Get the hours part of the day.
     * 
     * @return The hours part of the day.
     */
    public int getHour() {
        return hour;
    }

    /**
     * Set the hour part of the time.
     * 
     * @param hour
     *            Hour of the day [0..23].
     */
    public void setHour(int hour) {
        this.hour = hour;
    }

    /**
     * Get the minutes part of the hour.
     * 
     * @return The minutes part of the hour.
     */
    public int getMinute() {
        return minute;
    }

    /**
     * Set the minute of the hour.
     * 
     * @param minute
     *            The minute of the hour [0..59]
     */
    public void setMinute(int minute) {
        this.minute = minute;
    }

    /**
     * Get the seconds part of the minute.
     * 
     * @return The seconds part of the minute.
     */
    public int getSecond() {
        return second;
    }

    /**
     * Set the second of the minute.
     * 
     * @param second
     *            The second of the minute [0..59]
     */
    public void setSecond(int second) {
        this.second = second;
    }

    /**
     * Get the milliseconds part of the time.
     * 
     * @return The milliseconds part of the time.
     */
    public int getMillis() {
        return millis;
    }

    /**
     * Set the milliseconds part of the time.
     * 
     * @param millis
     *            Milliseconds [0..999].
     */
    public void setMillis(int millis) {
        this.millis = millis;
    }
}
