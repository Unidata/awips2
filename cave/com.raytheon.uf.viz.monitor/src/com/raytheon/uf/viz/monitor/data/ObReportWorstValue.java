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

public class ObReportWorstValue {

    // Observing platform identifier
    private String platformId;

    // Indicator of whether observing platform is stationary
    private boolean isStationary;

    // Actual time of the observation
    private Date observationTime;

    // Latitude in degrees of the observing platform
    private float latitude;

    // Longitude in degrees of the observing platform
    private float longitude;

    // Observed wind speed in knots
    private float windSpeed;

    // Observed maximum wind speed in knots
    private float maxWindSpeed;

    // Observed wind direction in degrees
    private float windDir;

    // Observed high resolution wave height
    private float highResWaveHeight;

    // Observed wave steepness in seconds
    private float waveSteepness;

    // Observed visibility in sixteenths of a nautical mile
    private float visibility;

    // Observed temperature in degrees Kelvin
    private float temperature;

    // Observed wave period in seconds
    private float wavePeriod;

    // Wind gust
    private float windGust;

    // Primary swell height in feet
    private float pSwellHeight;

    // Primary swell period in seconds
    private float pSwellPeriod;

    // Primary swell direction in degrees
    private float pSwellDir;

    // Secondary swell height in feet
    private float sSwellHeight;

    // Secondary swell period in seconds
    private float sSwellPeriod;

    // Secondary swell direction in degrees
    private float sSwellDir;

    // Pressure in inches of mercury
    private float pressure;

    // Three-hour pressure change in thousandths of an inch of mercury
    private float pressureChange;

    // Pressure change character
    private short pressChangeChar;

    // Observed dewpoint
    private float dewpoint;

    // Observed sea surface temperature in degrees Kelvin
    private float seaSurfaceTemp;

    // Observed hourly precipitation
    private float hourlyPrecip;

    // Observed snow depth
    private float snowDepth;

    // Observed snow increasing rapidly, hourly total
    private float snincrHourly;

    // Observed snow increasing rapidly, total
    private float snincrTotal;

    // Observed windchill
    private float windChill;

    // Observed frostbite
    private float frostbiteTime;

    // Observed present weather conditions (unitless)
    private String presentWx;

    // Observed relative humidity in percent
    private float relativeHumidity;

    // Observed ceiling in feet above ground level
    private float ceiling;

    // Observed dewpoint depression
    private float dewpointDepr;

    // Raw message
    private String rawMessage;

    // Public constructor
    public ObReportWorstValue() {

    }

    // Initializer of report
    public ObReportWorstValue init() {

        Date now = SimulatedTime.getSystemTime().getTime();
        Calendar deltaTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        deltaTime.setTime(now);
        deltaTime.add(Calendar.HOUR, -(ObConst.THREAT_INTERVAL_HOURS));
        Date initTime = deltaTime.getTime();

        // Populate the report with the worst case values
        populateReport("", true, initTime, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, 1e30f, 1e30f,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, (short) 0, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, 1e30f, ObConst.MISSING, "", ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, "");

        return ObReportWorstValue.this;

    }

    // Populate report
    private void populateReport(String platformId, boolean isStationary,
            Date observationTime, float latitude, float longitude,
            float windSpeed, float maxWindSpeed, float windDir,
            float highResWaveHeight, float waveSteepness, float visibility,
            float temperature, float wavePeriod, float windGust,
            float pSwellHeight, float pSwellPeriod, float pSwellDir,
            float sSwellHeight, float sSwellPeriod, float sSwellDir,
            float pressure, float pressureChange, short pressChangeChar,
            float dewpoint, float seaSurfaceTemp, float hourlyPrecip,
            float snowDepth, float snincrHourly, float snincrTotal,
            float windChill, float frostbiteTime, String presentWx,
            float relativeHumidity, float ceiling, float dewpointDepr,
            String rawMessage) {

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

    public String getPlatformId() {
        return platformId;
    }

    public void setPlatformId(String platformId) {
        this.platformId = platformId;
    }

    public boolean isStationary() {
        return isStationary;
    }

    public void setStationary(boolean isStationary) {
        this.isStationary = isStationary;
    }

    public Date getObservationTime() {
        return observationTime;
    }

    public void setObservationTime(Date observationTime) {
        this.observationTime = observationTime;
    }

    public float getLatitude() {
        return latitude;
    }

    public void setLatitude(float latitude) {
        this.latitude = latitude;
    }

    public float getLongitude() {
        return longitude;
    }

    public void setLongitude(float longitude) {
        this.longitude = longitude;
    }

    public float getWindSpeed() {
        return windSpeed;
    }

    public void setWindSpeed(float windSpeed) {
        this.windSpeed = windSpeed;
    }

    public float getMaxWindSpeed() {
        return maxWindSpeed;
    }

    public void setMaxWindSpeed(float maxWindSpeed) {
        this.maxWindSpeed = maxWindSpeed;
    }

    public float getWindDir() {
        return windDir;
    }

    public void setWindDir(float windDir) {
        this.windDir = windDir;
    }

    public float getHighResWaveHeight() {
        return highResWaveHeight;
    }

    public void setHighResWaveHeight(float highResWaveHeight) {
        this.highResWaveHeight = highResWaveHeight;
    }

    public float getWaveSteepness() {
        return waveSteepness;
    }

    public void setWaveSteepness(float waveSteepness) {
        this.waveSteepness = waveSteepness;
    }

    public float getVisibility() {
        return visibility;
    }

    public void setVisibility(float visibility) {
        this.visibility = visibility;
    }

    public float getTemperature() {
        return temperature;
    }

    public void setTemperature(float temperature) {
        this.temperature = temperature;
    }

    public float getWavePeriod() {
        return wavePeriod;
    }

    public void setWavePeriod(float wavePeriod) {
        this.wavePeriod = wavePeriod;
    }

    public float getWindGust() {
        return windGust;
    }

    public void setWindGust(float windGust) {
        this.windGust = windGust;
    }

    public float getPSwellHeight() {
        return pSwellHeight;
    }

    public void setPSwellHeight(float swellHeight) {
        pSwellHeight = swellHeight;
    }

    public float getPSwellPeriod() {
        return pSwellPeriod;
    }

    public void setPSwellPeriod(float swellPeriod) {
        pSwellPeriod = swellPeriod;
    }

    public float getPSwellDir() {
        return pSwellDir;
    }

    public void setPSwellDir(float swellDir) {
        pSwellDir = swellDir;
    }

    public float getSSwellHeight() {
        return sSwellHeight;
    }

    public void setSSwellHeight(float swellHeight) {
        sSwellHeight = swellHeight;
    }

    public float getSSwellPeriod() {
        return sSwellPeriod;
    }

    public void setSSwellPeriod(float swellPeriod) {
        sSwellPeriod = swellPeriod;
    }

    public float getSSwellDir() {
        return sSwellDir;
    }

    public void setSSwellDir(float swellDir) {
        sSwellDir = swellDir;
    }

    public float getPressure() {
        return pressure;
    }

    public void setPressure(float pressure) {
        this.pressure = pressure;
    }

    public float getPressureChange() {
        return pressureChange;
    }

    public void setPressureChange(float pressureChange) {
        this.pressureChange = pressureChange;
    }

    public short getPressChangeChar() {
        return pressChangeChar;
    }

    public void setPressChangeChar(short pressChangeChar) {
        this.pressChangeChar = pressChangeChar;
    }

    public float getDewpoint() {
        return dewpoint;
    }

    public void setDewpoint(float dewpoint) {
        this.dewpoint = dewpoint;
    }

    public float getSeaSurfaceTemp() {
        return seaSurfaceTemp;
    }

    public void setSeaSurfaceTemp(float seaSurfaceTemp) {
        this.seaSurfaceTemp = seaSurfaceTemp;
    }

    public float getHourlyPrecip() {
        return hourlyPrecip;
    }

    public void setHourlyPrecip(float hourlyPrecip) {
        this.hourlyPrecip = hourlyPrecip;
    }

    public float getSnowDepth() {
        return snowDepth;
    }

    public void setSnowDepth(float snowDepth) {
        this.snowDepth = snowDepth;
    }

    public float getSnincrHourly() {
        return snincrHourly;
    }

    public void setSnincrHourly(float snincrHourly) {
        this.snincrHourly = snincrHourly;
    }

    public float getSnincrTotal() {
        return snincrTotal;
    }

    public void setSnincrTotal(float snincrTotal) {
        this.snincrTotal = snincrTotal;
    }

    public float getWindChill() {
        return windChill;
    }

    public void setWindChill(float windChill) {
        this.windChill = windChill;
    }

    public float getFrostbiteTime() {
        return frostbiteTime;
    }

    public void setFrostbiteTime(float frostbiteTime) {
        this.frostbiteTime = frostbiteTime;
    }

    public String getPresentWx() {
        return presentWx;
    }

    public void setPresentWx(String presentWx) {
        this.presentWx = presentWx;
    }

    public float getRelativeHumidity() {
        return relativeHumidity;
    }

    public void setRelativeHumidity(float relativeHumidity) {
        this.relativeHumidity = relativeHumidity;
    }

    public float getCeiling() {
        return ceiling;
    }

    public void setCeiling(float ceiling) {
        this.ceiling = ceiling;
    }

    public float getDewpointDepr() {
        return dewpointDepr;
    }

    public void setDewpointDepr(float dewpointDepr) {
        this.dewpointDepr = dewpointDepr;
    }

    public String getRawMessage() {
        return rawMessage;
    }

    public void setRawMessage(String rawMessage) {
        this.rawMessage = rawMessage;
    }

}
