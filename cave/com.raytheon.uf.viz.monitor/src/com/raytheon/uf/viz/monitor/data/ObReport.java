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
import com.raytheon.uf.common.monitor.data.ObConst.ReportType;
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
 * Dec  9, 2009 3424       zhao        Added member waveHeight and method get(varName)
 * Jan 22, 2010 3888       zhao        Removed member waveHeight, which is the same as highResWaveHeight
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class ObReport {

    // Observing platform identifier
    private String platformId;

    // Indicator of whether observing platform is stationary
    private boolean isStationary;

    // Actual time of the observation
    private Date observationTime;

    // Reference time (closest hour)
    private Date refHour;

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

    // Observed wave height in feet
    // private float waveHeight; // same as highResWaveHeight

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

    // Report Type
    private ReportType reportType;

    // Zone ID
    private String zoneId;

    // Sea Level Press
    private float seaLevelPress;

    // Public constructor
    public ObReport() {
    	init();
    }

    // Initializer of report
    private void init() {

        Date now = SimulatedTime.getSystemTime().getTime();
        Calendar deltaTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        deltaTime.setTime(now);
        deltaTime.add(Calendar.HOUR, -(ObConst.THREAT_INTERVAL_HOURS));
        Date initTime = deltaTime.getTime();
        deltaTime.set(Calendar.MINUTE, 0);
        Date initDate = deltaTime.getTime();

        // Populate the report with the default values
        populateReport("", true, initTime, initDate, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, (short) 0, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, "", ObConst.MISSING, ObConst.MISSING,
                ObConst.MISSING, "", ReportType.UNSPECIFIED, "",
                ObConst.MISSING);

    }

    // Populate report
    private void populateReport(String platformId, boolean isStationary,
            Date observationTime, Date refHour, float latitude,
            float longitude,
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
            String rawMessage, ReportType reportType, String zoneId,
            float seaLevelPress) {

        this.platformId = platformId;
        this.isStationary = isStationary;
        this.observationTime = observationTime;
        this.refHour = refHour;
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
        this.reportType = reportType;
        this.zoneId = zoneId;
        this.seaLevelPress = seaLevelPress;
    }

    /**
     * Returns the value of an observation variable of type "float" for
     * caller-specified variable name
     * 
     * @param var
     * @return
     */
    public float get(ObConst.VarName var) {
        if (var == ObConst.VarName.CEILING) {
            return ceiling;
        }
        if (var == ObConst.VarName.DEWPOINT) {
            return dewpoint;
        }
        if (var == ObConst.VarName.DEWPOINT_DEPR) {
            return dewpointDepr;
        }
        if (var == ObConst.VarName.FROSTBITE_TIME) {
            return frostbiteTime;
        }
        if (var == ObConst.VarName.GUST_SPEED) {
            return windGust;
        }
        if (var == ObConst.VarName.HOURLY_PRECIP) {
            return hourlyPrecip;
        }
        if (var == ObConst.VarName.LATITUDE) {
            return latitude;
        }
        if (var == ObConst.VarName.LONGITUDE) {
            return longitude;
        }
        if (var == ObConst.VarName.MAX_WIND_SPEED) {
            return maxWindSpeed;
        }
        if (var == ObConst.VarName.PRESSURE) {
            return pressure;
        }
        if (var == ObConst.VarName.PRIM_SWELL_DIR) {
            return pSwellDir;
        }
        if (var == ObConst.VarName.PRIM_SWELL_HT) {
            return pSwellHeight;
        }
        if (var == ObConst.VarName.PRIM_SWELL_PD) {
            return pSwellPeriod;
        }
        if (var == ObConst.VarName.RELATIVE_HUMIDITY) {
            return relativeHumidity;
        }
        if (var == ObConst.VarName.SEA_SURFACE_TEMPERATURE) {
            return seaSurfaceTemp;
        }
        if (var == ObConst.VarName.SEC_SWELL_DIR) {
            return sSwellDir;
        }
        if (var == ObConst.VarName.SEC_SWELL_HT) {
            return sSwellHeight;
        }
        if (var == ObConst.VarName.SEC_SWELL_PD) {
            return sSwellPeriod;
        }
        if (var == ObConst.VarName.SNINCR_HOURLY) {
            return snincrHourly;
        }
        if (var == ObConst.VarName.SNINCR_TOTAL) {
            return snincrTotal;
        }
        if (var == ObConst.VarName.SNOW_DEPTH) {
            return snowDepth;
        }
        if (var == ObConst.VarName.TEMPERATURE) {
            return temperature;
        }
        if (var == ObConst.VarName.VISIBILITY) {
            return visibility;
        }
        if (var == ObConst.VarName.WAVE_HEIGHT) {
            return highResWaveHeight;
        }
        if (var == ObConst.VarName.WAVE_PERIOD) {
            return wavePeriod;
        }
        if (var == ObConst.VarName.WAVE_STEEPNESS) {
            return waveSteepness;
        }
        if (var == ObConst.VarName.WIND_CHILL) {
            return windChill;
        }
        if (var == ObConst.VarName.WIND_DIR) {
            return windDir;
        }
        if (var == ObConst.VarName.WIND_SPEED) {
            return windSpeed;
        }
        if (var == ObConst.VarName.SEA_LEVEL_PRESS) {
            return seaLevelPress;
        }

        return ObConst.MISSING;
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

    public Date getRefHour() {
        return refHour;
    }

    public void setRefHour(Date refHour) {
        this.refHour = refHour;
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

    // public float getWaveHeight() {
    // return waveHeight;
    // }

    // public void setWaveHeight(float waveHeight) {
    // this.waveHeight = waveHeight;
    // }

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

    public ReportType getReportType() {
        return reportType;
    }

    public void setReportType(ReportType reportType) {
        this.reportType = reportType;
    }

    public String getZoneId() {
        return zoneId;
    }

    public void setZoneId(String zoneId) {
        this.zoneId = zoneId;
    }

    public float getSeaLevelPress() {
        return seaLevelPress;
    }

    public void setSeaLevelPress(float seaLevelPress) {
        this.seaLevelPress = seaLevelPress;
    }

}
