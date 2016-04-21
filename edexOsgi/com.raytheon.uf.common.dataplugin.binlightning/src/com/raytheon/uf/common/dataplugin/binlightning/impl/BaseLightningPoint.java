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
package com.raytheon.uf.common.dataplugin.binlightning.impl;

import java.util.Calendar;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Lightning information common to strikes and pulses
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2014  3226      bclement     Initial creation
 * May 8, 2015  DR17252   MPorricelli  Use HOUR_OF_DAY for setHour
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BaseLightningPoint {

    private final Coordinate point;

    private Calendar time;

    // Lightning strike strength and polarity
    private double strikeStrength;

    private int sensorCount;

    /**
     * 
     */
    public BaseLightningPoint() {
        this(TimeUtil.newGmtCalendar());
    }

    /**
     * @param time
     */
    public BaseLightningPoint(Calendar time) {
        this(new Coordinate(), time);
    }

    /**
     * @param point
     */
    public BaseLightningPoint(Coordinate point, Calendar time) {
        this.point = point;
        this.time = time;
    }

    /**
     * @param latitude
     * @param longitude
     */
    public BaseLightningPoint(double latitude, double longitude, Calendar time) {
        this(new Coordinate(longitude, latitude), time);
    }

    /**
     * Set the year part of the date.
     * 
     * @param year
     *            Year.
     */
    public void setYear(int year) {
        this.time.set(Calendar.YEAR, year);
    }

    /**
     * Set the day of the month part of the date.
     * 
     * @param day
     *            Day of month [1..12].
     */
    public void setMonth(int month) {
        this.time.set(Calendar.MONTH, month - 1);
    }

    /**
     * Set the day of the month part of the date.
     * 
     * @param day
     *            Day of month [1..12].
     */
    public void setDay(int day) {
        this.time.set(Calendar.DAY_OF_MONTH, day);
    }

    /**
     * Set the hour part of the time.
     * 
     * @param hour
     *            Hour of the day [0..23].
     */
    public void setHour(int hour) {
        this.time.set(Calendar.HOUR_OF_DAY, hour);
    }

    /**
     * Set the minute of the hour.
     * 
     * @param minute
     *            The minute of the hour [0..59]
     */
    public void setMinute(int minute) {
        this.time.set(Calendar.MINUTE, minute);
    }

    /**
     * Set the second of the minute.
     * 
     * @param second
     *            The second of the minute [0..59]
     */
    public void setSecond(int second) {
        this.time.set(Calendar.SECOND, second);
    }

    /**
     * Set the milliseconds part of the time.
     * 
     * @param millis
     *            Milliseconds [0..999].
     */
    public void setMillis(int millis) {
        this.time.set(Calendar.MILLISECOND, millis);
    }

    /**
     * @return the time
     */
    public Calendar getTime() {
        return time;
    }

    /**
     * @param time
     *            the time to set
     */
    public void setTime(Calendar time) {
        this.time = time;
    }

    /**
     * Get the strike strength.
     * 
     * @return The strike strength.
     */
    public double getStrikeStrength() {
        return strikeStrength;
    }

    /**
     * Set the strike strength.
     * 
     * @return The strike strength.
     */
    public void setStrikeStrength(double strikeStrength) {
        this.strikeStrength = strikeStrength;
    }

    /**
     * @return the sensorCount
     */
    public int getSensorCount() {
        return sensorCount;
    }

    /**
     * @param sensorCount
     *            the sensorCount to set
     */
    public void setSensorCount(int sensorCount) {
        this.sensorCount = sensorCount;
    }

    /**
     * Get the latitude of this point.
     * 
     * @return The latitude.
     */
    public double getLatitude() {
        return point.y;
    }

    /**
     * Set the latitude for this point.
     * 
     * @param latitude
     *            The latitude.
     */
    public void setLatitude(double latitude) {
        this.point.y = latitude;
    }

    /**
     * Get the longitude of this point.
     * 
     * @return The longitude.
     */
    public double getLongitude() {
        return point.x;
    }

    /**
     * Set the longitude for this point.
     * 
     * @param longitude
     *            The longitude.
     */
    public void setLongitude(double longitude) {
        this.point.x = longitude;
    }

    /**
     * Get the elevation of this point. No units are specified.
     * 
     * @return The elevation of this point.
     */
    public double getElevation() {
        return this.point.z;
    }

    /**
     * Set the elevation of this point. No units are specified.
     * 
     * @param elevation
     *            The elevation of this point.
     */
    public void setElevation(double elevation) {
        this.point.z = elevation;
    }

}
