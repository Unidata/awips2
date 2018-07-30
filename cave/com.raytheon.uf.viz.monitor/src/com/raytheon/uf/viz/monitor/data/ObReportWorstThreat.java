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

import com.raytheon.uf.common.monitor.data.ObConst.ThreatLevel;

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

public class ObReportWorstThreat {

    // Observing platform identifier
    private ThreatLevel platformId;

    // Indicator of whether observing platform is stationary
    private ThreatLevel isStationary;

    // Actual time of the observation
    private ThreatLevel observationTime;

    // Latitude in degrees of the observing platform
    private ThreatLevel latitude;

    // Longitude in degrees of the observing platform
    private ThreatLevel longitude;

    // Observed wind speed in knots
    private ThreatLevel windSpeed;

    // Observed maximum wind speed in knots
    private ThreatLevel maxWindSpeed;

    // Observed wind direction in degrees
    private ThreatLevel windDir;

    // Observed high resolution wave height
    private ThreatLevel highResWaveHeight;

    // Observed wave steepness in seconds
    private ThreatLevel waveSteepness;

    // Observed visibility in sixteenths of a nautical mile
    private ThreatLevel visibility;

    // Observed temperature in degrees Kelvin
    private ThreatLevel temperature;

    // Observed wave period in seconds
    private ThreatLevel wavePeriod;

    // Wind gust
    private ThreatLevel windGust;

    // Primary swell height in feet
    private ThreatLevel pSwellHeight;

    // Primary swell period in seconds
    private ThreatLevel pSwellPeriod;

    // Primary swell direction in degrees
    private ThreatLevel pSwellDir;

    // Secondary swell height in feet
    private ThreatLevel sSwellHeight;

    // Secondary swell period in seconds
    private ThreatLevel sSwellPeriod;

    // Secondary swell direction in degrees
    private ThreatLevel sSwellDir;

    // Pressure in inches of mercury
    private ThreatLevel pressure;

    // Three-hour pressure change in thousandths of an inch of mercury
    private ThreatLevel pressureChange;

    // Pressure change character
    private ThreatLevel pressChangeChar;

    // Observed dewpoint
    private ThreatLevel dewpoint;

    // Observed sea surface temperature in degrees Kelvin
    private ThreatLevel seaSurfaceTemp;

    // Observed hourly precipitation
    private ThreatLevel hourlyPrecip;

    // Observed snow depth
    private ThreatLevel snowDepth;

    // Observed snow increasing rapidly, hourly total
    private ThreatLevel snincrHourly;

    // Observed snow increasing rapidly, total
    private ThreatLevel snincrTotal;

    // Observed windchill
    private ThreatLevel windChill;

    // Observed frostbite
    private ThreatLevel frostbiteTime;

    // Observed present weather conditions (unitless)
    private ThreatLevel presentWx;

    // Observed relative humidity in percent
    private ThreatLevel relativeHumidity;

    // Observed ceiling in feet above ground level
    private ThreatLevel ceiling;

    // Observed dewpoint depression
    private ThreatLevel dewpointDepr;

    // Raw message
    private ThreatLevel rawMessage;

    // Public constructor
    public ObReportWorstThreat() {

    }

    // Initializer of report
    public ObReportWorstThreat init() {

        // Populate the report with the worst case threats
        populateReport(ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY,
                ThreatLevel.GRAY, ThreatLevel.GRAY, ThreatLevel.GRAY);

        return ObReportWorstThreat.this;

    }

    // Populate report
    private void populateReport(ThreatLevel platformId,
            ThreatLevel isStationary, ThreatLevel observationTime,
            ThreatLevel latitude, ThreatLevel longitude, ThreatLevel windSpeed,
            ThreatLevel maxWindSpeed, ThreatLevel windDir,
            ThreatLevel highResWaveHeight, ThreatLevel waveSteepness,
            ThreatLevel visibility, ThreatLevel temperature,
            ThreatLevel wavePeriod, ThreatLevel windGust,
            ThreatLevel pSwellHeight, ThreatLevel pSwellPeriod,
            ThreatLevel pSwellDir, ThreatLevel sSwellHeight,
            ThreatLevel sSwellPeriod, ThreatLevel sSwellDir,
            ThreatLevel pressure, ThreatLevel pressureChange,
            ThreatLevel pressChangeChar, ThreatLevel dewpoint,
            ThreatLevel seaSurfaceTemp, ThreatLevel hourlyPrecip,
            ThreatLevel snowDepth, ThreatLevel snincrHourly,
            ThreatLevel snincrTotal, ThreatLevel windChill,
            ThreatLevel frostbiteTime, ThreatLevel presentWx,
            ThreatLevel relativeHumidity, ThreatLevel ceiling,
            ThreatLevel dewpointDepr, ThreatLevel rawMessage) {

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

    public ThreatLevel getPlatformId() {
        return platformId;
    }

    public void setPlatformId(ThreatLevel platformId) {
        this.platformId = platformId;
    }

    public ThreatLevel getIsStationary() {
        return isStationary;
    }

    public void setIsStationary(ThreatLevel isStationary) {
        this.isStationary = isStationary;
    }

    public ThreatLevel getObservationTime() {
        return observationTime;
    }

    public void setObservationTime(ThreatLevel observationTime) {
        this.observationTime = observationTime;
    }

    public ThreatLevel getLatitude() {
        return latitude;
    }

    public void setLatitude(ThreatLevel latitude) {
        this.latitude = latitude;
    }

    public ThreatLevel getLongitude() {
        return longitude;
    }

    public void setLongitude(ThreatLevel longitude) {
        this.longitude = longitude;
    }

    public ThreatLevel getWindSpeed() {
        return windSpeed;
    }

    public void setWindSpeed(ThreatLevel windSpeed) {
        this.windSpeed = windSpeed;
    }

    public ThreatLevel getMaxWindSpeed() {
        return maxWindSpeed;
    }

    public void setMaxWindSpeed(ThreatLevel maxWindSpeed) {
        this.maxWindSpeed = maxWindSpeed;
    }

    public ThreatLevel getWindDir() {
        return windDir;
    }

    public void setWindDir(ThreatLevel windDir) {
        this.windDir = windDir;
    }

    public ThreatLevel getHighResWaveHeight() {
        return highResWaveHeight;
    }

    public void setHighResWaveHeight(ThreatLevel highResWaveHeight) {
        this.highResWaveHeight = highResWaveHeight;
    }

    public ThreatLevel getWaveSteepness() {
        return waveSteepness;
    }

    public void setWaveSteepness(ThreatLevel waveSteepness) {
        this.waveSteepness = waveSteepness;
    }

    public ThreatLevel getVisibility() {
        return visibility;
    }

    public void setVisibility(ThreatLevel visibility) {
        this.visibility = visibility;
    }

    public ThreatLevel getTemperature() {
        return temperature;
    }

    public void setTemperature(ThreatLevel temperature) {
        this.temperature = temperature;
    }

    public ThreatLevel getWavePeriod() {
        return wavePeriod;
    }

    public void setWavePeriod(ThreatLevel wavePeriod) {
        this.wavePeriod = wavePeriod;
    }

    public ThreatLevel getWindGust() {
        return windGust;
    }

    public void setWindGust(ThreatLevel windGust) {
        this.windGust = windGust;
    }

    public ThreatLevel getPSwellHeight() {
        return pSwellHeight;
    }

    public void setPSwellHeight(ThreatLevel swellHeight) {
        pSwellHeight = swellHeight;
    }

    public ThreatLevel getPSwellPeriod() {
        return pSwellPeriod;
    }

    public void setPSwellPeriod(ThreatLevel swellPeriod) {
        pSwellPeriod = swellPeriod;
    }

    public ThreatLevel getPSwellDir() {
        return pSwellDir;
    }

    public void setPSwellDir(ThreatLevel swellDir) {
        pSwellDir = swellDir;
    }

    public ThreatLevel getSSwellHeight() {
        return sSwellHeight;
    }

    public void setSSwellHeight(ThreatLevel swellHeight) {
        sSwellHeight = swellHeight;
    }

    public ThreatLevel getSSwellPeriod() {
        return sSwellPeriod;
    }

    public void setSSwellPeriod(ThreatLevel swellPeriod) {
        sSwellPeriod = swellPeriod;
    }

    public ThreatLevel getSSwellDir() {
        return sSwellDir;
    }

    public void setSSwellDir(ThreatLevel swellDir) {
        sSwellDir = swellDir;
    }

    public ThreatLevel getPressure() {
        return pressure;
    }

    public void setPressure(ThreatLevel pressure) {
        this.pressure = pressure;
    }

    public ThreatLevel getPressureChange() {
        return pressureChange;
    }

    public void setPressureChange(ThreatLevel pressureChange) {
        this.pressureChange = pressureChange;
    }

    public ThreatLevel getPressChangeChar() {
        return pressChangeChar;
    }

    public void setPressChangeChar(ThreatLevel pressChangeChar) {
        this.pressChangeChar = pressChangeChar;
    }

    public ThreatLevel getDewpoint() {
        return dewpoint;
    }

    public void setDewpoint(ThreatLevel dewpoint) {
        this.dewpoint = dewpoint;
    }

    public ThreatLevel getSeaSurfaceTemp() {
        return seaSurfaceTemp;
    }

    public void setSeaSurfaceTemp(ThreatLevel seaSurfaceTemp) {
        this.seaSurfaceTemp = seaSurfaceTemp;
    }

    public ThreatLevel getHourlyPrecip() {
        return hourlyPrecip;
    }

    public void setHourlyPrecip(ThreatLevel hourlyPrecip) {
        this.hourlyPrecip = hourlyPrecip;
    }

    public ThreatLevel getSnowDepth() {
        return snowDepth;
    }

    public void setSnowDepth(ThreatLevel snowDepth) {
        this.snowDepth = snowDepth;
    }

    public ThreatLevel getSnincrHourly() {
        return snincrHourly;
    }

    public void setSnincrHourly(ThreatLevel snincrHourly) {
        this.snincrHourly = snincrHourly;
    }

    public ThreatLevel getSnincrTotal() {
        return snincrTotal;
    }

    public void setSnincrTotal(ThreatLevel snincrTotal) {
        this.snincrTotal = snincrTotal;
    }

    public ThreatLevel getWindChill() {
        return windChill;
    }

    public void setWindChill(ThreatLevel windChill) {
        this.windChill = windChill;
    }

    public ThreatLevel getFrostbiteTime() {
        return frostbiteTime;
    }

    public void setFrostbiteTime(ThreatLevel frostbiteTime) {
        this.frostbiteTime = frostbiteTime;
    }

    public ThreatLevel getPresentWx() {
        return presentWx;
    }

    public void setPresentWx(ThreatLevel presentWx) {
        this.presentWx = presentWx;
    }

    public ThreatLevel getRelativeHumidity() {
        return relativeHumidity;
    }

    public void setRelativeHumidity(ThreatLevel relativeHumidity) {
        this.relativeHumidity = relativeHumidity;
    }

    public ThreatLevel getCeiling() {
        return ceiling;
    }

    public void setCeiling(ThreatLevel ceiling) {
        this.ceiling = ceiling;
    }

    public ThreatLevel getDewpointDepr() {
        return dewpointDepr;
    }

    public void setDewpointDepr(ThreatLevel dewpointDepr) {
        this.dewpointDepr = dewpointDepr;
    }

    public ThreatLevel getRawMessage() {
        return rawMessage;
    }

    public void setRawMessage(ThreatLevel rawMessage) {
        this.rawMessage = rawMessage;
    }

}
