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
package com.raytheon.uf.viz.monitor.data;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.time.SimulatedTime;

/**
 * This class models a SAFESEAS report (derived from a METAR, a buoy report, a
 * C_MAN report, or a ship report). An instance of this class contains one
 * report. An example of an instance of this class would be one METAR, or one
 * C-MAN report, or one buoy report, or one ship report.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2009 1999       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class ObReportWorstTime {

    // Observing platform identifier
    private Date platformId;

    // Indicator of whether observing platform is stationary
    private Date isStationary;

    // Actual time of the observation
    private Date observationTime;

    // Latitude in degrees of the observing platform
    private Date latitude;

    // Longitude in degrees of the observing platform
    private Date longitude;

    // Observed wind speed in knots
    private Date windSpeed;

    // Observed maximum wind speed in knots
    private Date maxWindSpeed;

    // Observed wind direction in degrees
    private Date windDir;

    // Observed high resolution wave height
    private Date highResWaveHeight;

    // Observed wave steepness in seconds
    private Date waveSteepness;

    // Observed visibility in sixteenths of a nautical mile
    private Date visibility;

    // Observed temperature in degrees Kelvin
    private Date temperature;

    // Observed wave period in seconds
    private Date wavePeriod;

    // Wind gust
    private Date windGust;

    // Primary swell height in feet
    private Date pSwellHeight;

    // Primary swell period in seconds
    private Date pSwellPeriod;

    // Primary swell direction in degrees
    private Date pSwellDir;

    // Secondary swell height in feet
    private Date sSwellHeight;

    // Secondary swell period in seconds
    private Date sSwellPeriod;

    // Secondary swell direction in degrees
    private Date sSwellDir;

    // Pressure in inches of mercury
    private Date pressure;

    // Three-hour pressure change in thousandths of an inch of mercury
    private Date pressureChange;

    // Pressure change character
    private Date pressChangeChar;

    // Observed dewpoint
    private Date dewpoint;

    // Observed sea surface temperature in degrees Kelvin
    private Date seaSurfaceTemp;

    // Observed hourly precipitation
    private Date hourlyPrecip;

    // Observed snow depth
    private Date snowDepth;

    // Observed snow increasing rapidly, hourly total
    private Date snincrHourly;

    // Observed snow increasing rapidly, total
    private Date snincrTotal;

    // Observed windchill
    private Date windChill;

    // Observed frostbite
    private Date frostbiteTime;

    // Observed present weather conditions (unitless)
    private Date presentWx;

    // Observed relative humidity in percent
    private Date relativeHumidity;

    // Observed ceiling in feet above ground level
    private Date ceiling;

    // Observed dewpoint depression
    private Date dewpointDepr;

    // Raw message
    private Date rawMessage;

    // Public constructor
    public ObReportWorstTime() {

    }

    // Initializer of report
    public ObReportWorstTime init() {

        Date now = SimulatedTime.getSystemTime().getTime();
        Calendar deltaTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        deltaTime.setTime(now);
        deltaTime.add(Calendar.HOUR, -(ObConst.THREAT_INTERVAL_HOURS));
        Date initTime = deltaTime.getTime();

        // Populate the report with the worst case times
        populateReport(initTime, initTime, initTime, initTime, initTime,
                initTime, initTime, initTime, initTime, initTime, initTime,
                initTime, initTime, initTime, initTime, initTime, initTime,
                initTime, initTime, initTime, initTime, initTime, initTime,
                initTime, initTime, initTime, initTime, initTime, initTime,
                initTime, initTime, initTime, initTime, initTime, initTime,
                initTime);

        return ObReportWorstTime.this;

    }

    // Populate report
    private void populateReport(Date platformId, Date isStationary,
            Date observationTime, Date latitude, Date longitude,
            Date windSpeed, Date maxWindSpeed, Date windDir,
            Date highResWaveHeight, Date waveSteepness, Date visibility,
            Date temperature, Date wavePeriod, Date windGust,
            Date pSwellHeight, Date pSwellPeriod, Date pSwellDir,
            Date sSwellHeight, Date sSwellPeriod, Date sSwellDir,
            Date pressure, Date pressureChange, Date pressChangeChar,
            Date dewpoint, Date seaSurfaceTemp, Date hourlyPrecip,
            Date snowDepth, Date snincrHourly, Date snincrTotal,
            Date windChill, Date frostbiteTime, Date presentWx,
            Date relativeHumidity, Date ceiling, Date dewpointDepr,
            Date rawMessage) {

        this.platformId = platformId;
        this.isStationary = isStationary;
        this.observationTime = observationTime;
        this.latitude = latitude;
        this.longitude = longitude;
        this.windSpeed = windSpeed;
        this.maxWindSpeed = maxWindSpeed;
        this.windDir = windDir;
        this.highResWaveHeight = highResWaveHeight;
        this.waveSteepness = waveSteepness;
        this.visibility = visibility;
        this.temperature = temperature;
        this.wavePeriod = wavePeriod;
        this.windGust = windGust;
        this.pSwellHeight = pSwellHeight;
        this.pSwellPeriod = pSwellPeriod;
        this.pSwellDir = pSwellDir;
        this.sSwellHeight = sSwellHeight;
        this.sSwellPeriod = sSwellPeriod;
        this.sSwellDir = sSwellDir;
        this.pressure = pressure;
        this.pressureChange = pressureChange;
        this.pressChangeChar = pressChangeChar;
        this.dewpoint = dewpoint;
        this.seaSurfaceTemp = seaSurfaceTemp;
        this.hourlyPrecip = hourlyPrecip;
        this.snowDepth = snowDepth;
        this.snincrHourly = snincrHourly;
        this.snincrTotal = snincrTotal;
        this.windChill = windChill;
        this.frostbiteTime = frostbiteTime;
        this.presentWx = presentWx;
        this.relativeHumidity = relativeHumidity;
        this.ceiling = ceiling;
        this.dewpointDepr = dewpointDepr;
        this.rawMessage = rawMessage;

    }

    public Date getPlatformId() {
        return platformId;
    }

    public void setPlatformId(Date platformId) {
        this.platformId = platformId;
    }

    public Date getIsStationary() {
        return isStationary;
    }

    public void setIsStationary(Date isStationary) {
        this.isStationary = isStationary;
    }

    public Date getObservationTime() {
        return observationTime;
    }

    public void setObservationTime(Date observationTime) {
        this.observationTime = observationTime;
    }

    public Date getLatitude() {
        return latitude;
    }

    public void setLatitude(Date latitude) {
        this.latitude = latitude;
    }

    public Date getLongitude() {
        return longitude;
    }

    public void setLongitude(Date longitude) {
        this.longitude = longitude;
    }

    public Date getWindSpeed() {
        return windSpeed;
    }

    public void setWindSpeed(Date windSpeed) {
        this.windSpeed = windSpeed;
    }

    public Date getMaxWindSpeed() {
        return maxWindSpeed;
    }

    public void setMaxWindSpeed(Date maxWindSpeed) {
        this.maxWindSpeed = maxWindSpeed;
    }

    public Date getWindDir() {
        return windDir;
    }

    public void setWindDir(Date windDir) {
        this.windDir = windDir;
    }

    public Date getHighResWaveHeight() {
        return highResWaveHeight;
    }

    public void setHighResWaveHeight(Date highResWaveHeight) {
        this.highResWaveHeight = highResWaveHeight;
    }

    public Date getWaveSteepness() {
        return waveSteepness;
    }

    public void setWaveSteepness(Date waveSteepness) {
        this.waveSteepness = waveSteepness;
    }

    public Date getVisibility() {
        return visibility;
    }

    public void setVisibility(Date visibility) {
        this.visibility = visibility;
    }

    public Date getTemperature() {
        return temperature;
    }

    public void setTemperature(Date temperature) {
        this.temperature = temperature;
    }

    public Date getWavePeriod() {
        return wavePeriod;
    }

    public void setWavePeriod(Date wavePeriod) {
        this.wavePeriod = wavePeriod;
    }

    public Date getWindGust() {
        return windGust;
    }

    public void setWindGust(Date windGust) {
        this.windGust = windGust;
    }

    public Date getPSwellHeight() {
        return pSwellHeight;
    }

    public void setPSwellHeight(Date swellHeight) {
        pSwellHeight = swellHeight;
    }

    public Date getPSwellPeriod() {
        return pSwellPeriod;
    }

    public void setPSwellPeriod(Date swellPeriod) {
        pSwellPeriod = swellPeriod;
    }

    public Date getPSwellDir() {
        return pSwellDir;
    }

    public void setPSwellDir(Date swellDir) {
        pSwellDir = swellDir;
    }

    public Date getSSwellHeight() {
        return sSwellHeight;
    }

    public void setSSwellHeight(Date swellHeight) {
        sSwellHeight = swellHeight;
    }

    public Date getSSwellPeriod() {
        return sSwellPeriod;
    }

    public void setSSwellPeriod(Date swellPeriod) {
        sSwellPeriod = swellPeriod;
    }

    public Date getSSwellDir() {
        return sSwellDir;
    }

    public void setSSwellDir(Date swellDir) {
        sSwellDir = swellDir;
    }

    public Date getPressure() {
        return pressure;
    }

    public void setPressure(Date pressure) {
        this.pressure = pressure;
    }

    public Date getPressureChange() {
        return pressureChange;
    }

    public void setPressureChange(Date pressureChange) {
        this.pressureChange = pressureChange;
    }

    public Date getPressChangeChar() {
        return pressChangeChar;
    }

    public void setPressChangeChar(Date pressChangeChar) {
        this.pressChangeChar = pressChangeChar;
    }

    public Date getDewpoint() {
        return dewpoint;
    }

    public void setDewpoint(Date dewpoint) {
        this.dewpoint = dewpoint;
    }

    public Date getSeaSurfaceTemp() {
        return seaSurfaceTemp;
    }

    public void setSeaSurfaceTemp(Date seaSurfaceTemp) {
        this.seaSurfaceTemp = seaSurfaceTemp;
    }

    public Date getHourlyPrecip() {
        return hourlyPrecip;
    }

    public void setHourlyPrecip(Date hourlyPrecip) {
        this.hourlyPrecip = hourlyPrecip;
    }

    public Date getSnowDepth() {
        return snowDepth;
    }

    public void setSnowDepth(Date snowDepth) {
        this.snowDepth = snowDepth;
    }

    public Date getSnincrHourly() {
        return snincrHourly;
    }

    public void setSnincrHourly(Date snincrHourly) {
        this.snincrHourly = snincrHourly;
    }

    public Date getSnincrTotal() {
        return snincrTotal;
    }

    public void setSnincrTotal(Date snincrTotal) {
        this.snincrTotal = snincrTotal;
    }

    public Date getWindChill() {
        return windChill;
    }

    public void setWindChill(Date windChill) {
        this.windChill = windChill;
    }

    public Date getFrostbiteTime() {
        return frostbiteTime;
    }

    public void setFrostbiteTime(Date frostbiteTime) {
        this.frostbiteTime = frostbiteTime;
    }

    public Date getPresentWx() {
        return presentWx;
    }

    public void setPresentWx(Date presentWx) {
        this.presentWx = presentWx;
    }

    public Date getRelativeHumidity() {
        return relativeHumidity;
    }

    public void setRelativeHumidity(Date relativeHumidity) {
        this.relativeHumidity = relativeHumidity;
    }

    public Date getCeiling() {
        return ceiling;
    }

    public void setCeiling(Date ceiling) {
        this.ceiling = ceiling;
    }

    public Date getDewpointDepr() {
        return dewpointDepr;
    }

    public void setDewpointDepr(Date dewpointDepr) {
        this.dewpointDepr = dewpointDepr;
    }

    public Date getRawMessage() {
        return rawMessage;
    }

    public void setRawMessage(Date rawMessage) {
        this.rawMessage = rawMessage;
    }

}
