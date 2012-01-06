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

/**
 * The threshold data class (f/k/a struct) for Decision Assistance Tools.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2009 2047       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class SafeSeasThresholdStruct {

    // Small Craft Advisory Wind Speed
    int scaWindSpeed;

    // Small Craft Advisory Gust Speed
    int scaGustSpeed;

    // Small Craft Advisory Maximum Wind Speed
    int scaMaxWindSpeed;

    // Small Craft Advisory Wave Height
    int scaWaveHeight;

    // Gale Wind Speed
    int galeWindSpeed;

    // Gale Gust Speed
    int galeGustSpeed;

    // Gale Maximum Wind Speed
    int galeMaxWindSpeed;

    // Storm Wind Speed
    int stormWindSpeed;

    // Storm Gust Speed
    int stormGustSpeed;

    // Storm Maximum Wind Speed
    int stormMaxWindSpeed;

    // Hurricane Wind Speed
    int hurricaneWindSpeed;

    // Hurricane Gust Speed
    int hurricaneGustSpeed;

    // Hurricane Maximum Wind Speed
    int hurricaneMaxWindSpeed;

    // Wind Direction
    int windDir1;

    // Wind Direction
    int windDir2;

    // Wind Speed
    int windSpeed;

    // Maximum Wind Speed
    int maxWindSpeed;

    // Gust Speed
    int gustSpeed;

    // Wave Height
    int waveHeight;

    // Wave Steepness
    float waveSteepness;

    // Visibility
    float visibility;

    // Temperature
    int temperature;

    // Dewpoint
    int dewpoint;

    // Pressure
    int pressure;

    // Sea Surface Temperature
    int seaSurfaceTemp;

    // Primary Swell Height
    int primSwellHt;

    // Primary Swell Period
    int primSwellPd;

    // Primary Swell Direction
    int primSwellDir1;

    // Primary Swell Direction
    int primSwellDir2;

    // Secondary Swell Height
    int secSwellHt;

    // Secondary Swell Period
    int secSwellPd;

    // Secondary Swell Direction
    int secSwellDir1;

    // Secondary Swell Direction
    int secSwellDir2;

    // Blizzard Wind Speed
    int blizzardWindSpeed;

    // Blizzard Gust Speed
    int blizzardGustSpeed;

    // Blizzard Maximum Wind Speed
    int blizzardMaxWindSpeed;

    // Blizzard Visibility
    int blizzardVisibility;

    // Frozen Hourly Precipitation
    float frzHourlyPrecip;

    // Frozen Temperature
    int frzTemperature;

    // HSW Snow Depth
    int hswSnowDepth;

    // HSW Snow Increasing Hourly
    int hswSnincrHourly;

    // HSW Snow Increasing Total
    int hswSnincrTotal;

    // Hourly Precipitation
    float hourlyPrecip;

    // Windchill
    int windchill;

    // Frostbite Time
    int frostbiteTime;

    // Snow Depth
    int snowDepth;

    // Snow Increasing Hourly
    int snincrHourly;

    // Snow Increasing Total
    int snincrTotal;

    // Relative Humidity
    int relativeHumidity;

    // Ceiling
    int ceiling;

    // Dewpoint Depression
    int dewpointDepr;

    public int getScaWindSpeed() {
        return scaWindSpeed;
    }

    public void setScaWindSpeed(int scaWindSpeed) {
        this.scaWindSpeed = scaWindSpeed;
    }

    public int getScaGustSpeed() {
        return scaGustSpeed;
    }

    public void setScaGustSpeed(int scaGustSpeed) {
        this.scaGustSpeed = scaGustSpeed;
    }

    public int getScaMaxWindSpeed() {
        return scaMaxWindSpeed;
    }

    public void setScaMaxWindSpeed(int scaMaxWindSpeed) {
        this.scaMaxWindSpeed = scaMaxWindSpeed;
    }

    public int getScaWaveHeight() {
        return scaWaveHeight;
    }

    public void setScaWaveHeight(int scaWaveHeight) {
        this.scaWaveHeight = scaWaveHeight;
    }

    public int getGaleWindSpeed() {
        return galeWindSpeed;
    }

    public void setGaleWindSpeed(int galeWindSpeed) {
        this.galeWindSpeed = galeWindSpeed;
    }

    public int getGaleGustSpeed() {
        return galeGustSpeed;
    }

    public void setGaleGustSpeed(int galeGustSpeed) {
        this.galeGustSpeed = galeGustSpeed;
    }

    public int getGaleMaxWindSpeed() {
        return galeMaxWindSpeed;
    }

    public void setGaleMaxWindSpeed(int galeMaxWindSpeed) {
        this.galeMaxWindSpeed = galeMaxWindSpeed;
    }

    public int getStormWindSpeed() {
        return stormWindSpeed;
    }

    public void setStormWindSpeed(int stormWindSpeed) {
        this.stormWindSpeed = stormWindSpeed;
    }

    public int getStormGustSpeed() {
        return stormGustSpeed;
    }

    public void setStormGustSpeed(int stormGustSpeed) {
        this.stormGustSpeed = stormGustSpeed;
    }

    public int getStormMaxWindSpeed() {
        return stormMaxWindSpeed;
    }

    public void setStormMaxWindSpeed(int stormMaxWindSpeed) {
        this.stormMaxWindSpeed = stormMaxWindSpeed;
    }

    public int getHurricaneWindSpeed() {
        return hurricaneWindSpeed;
    }

    public void setHurricaneWindSpeed(int hurricaneWindSpeed) {
        this.hurricaneWindSpeed = hurricaneWindSpeed;
    }

    public int getHurricaneGustSpeed() {
        return hurricaneGustSpeed;
    }

    public void setHurricaneGustSpeed(int hurricaneGustSpeed) {
        this.hurricaneGustSpeed = hurricaneGustSpeed;
    }

    public int getHurricaneMaxWindSpeed() {
        return hurricaneMaxWindSpeed;
    }

    public void setHurricaneMaxWindSpeed(int hurricaneMaxWindSpeed) {
        this.hurricaneMaxWindSpeed = hurricaneMaxWindSpeed;
    }

    public int getWindDir1() {
        return windDir1;
    }

    public void setWindDir1(int windDir1) {
        this.windDir1 = windDir1;
    }

    public int getWindDir2() {
        return windDir2;
    }

    public void setWindDir2(int windDir2) {
        this.windDir2 = windDir2;
    }

    public int getWindSpeed() {
        return windSpeed;
    }

    public void setWindSpeed(int windSpeed) {
        this.windSpeed = windSpeed;
    }

    public int getMaxWindSpeed() {
        return maxWindSpeed;
    }

    public void setMaxWindSpeed(int maxWindSpeed) {
        this.maxWindSpeed = maxWindSpeed;
    }

    public int getGustSpeed() {
        return gustSpeed;
    }

    public void setGustSpeed(int gustSpeed) {
        this.gustSpeed = gustSpeed;
    }

    public int getWaveHeight() {
        return waveHeight;
    }

    public void setWaveHeight(int waveHeight) {
        this.waveHeight = waveHeight;
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

    public int getTemperature() {
        return temperature;
    }

    public void setTemperature(int temperature) {
        this.temperature = temperature;
    }

    public int getDewpoint() {
        return dewpoint;
    }

    public void setDewpoint(int dewpoint) {
        this.dewpoint = dewpoint;
    }

    public int getPressure() {
        return pressure;
    }

    public void setPressure(int pressure) {
        this.pressure = pressure;
    }

    public int getSeaSurfaceTemp() {
        return seaSurfaceTemp;
    }

    public void setSeaSurfaceTemp(int seaSurfaceTemp) {
        this.seaSurfaceTemp = seaSurfaceTemp;
    }

    public int getPrimSwellHt() {
        return primSwellHt;
    }

    public void setPrimSwellHt(int primSwellHt) {
        this.primSwellHt = primSwellHt;
    }

    public int getPrimSwellPd() {
        return primSwellPd;
    }

    public void setPrimSwellPd(int primSwellPd) {
        this.primSwellPd = primSwellPd;
    }

    public int getPrimSwellDir1() {
        return primSwellDir1;
    }

    public void setPrimSwellDir1(int primSwellDir1) {
        this.primSwellDir1 = primSwellDir1;
    }

    public int getPrimSwellDir2() {
        return primSwellDir2;
    }

    public void setPrimSwellDir2(int primSwellDir2) {
        this.primSwellDir2 = primSwellDir2;
    }

    public int getSecSwellHt() {
        return secSwellHt;
    }

    public void setSecSwellHt(int secSwellHt) {
        this.secSwellHt = secSwellHt;
    }

    public int getSecSwellPd() {
        return secSwellPd;
    }

    public void setSecSwellPd(int secSwellPd) {
        this.secSwellPd = secSwellPd;
    }

    public int getSecSwellDir1() {
        return secSwellDir1;
    }

    public void setSecSwellDir1(int secSwellDir1) {
        this.secSwellDir1 = secSwellDir1;
    }

    public int getSecSwellDir2() {
        return secSwellDir2;
    }

    public void setSecSwellDir2(int secSwellDir2) {
        this.secSwellDir2 = secSwellDir2;
    }

    public int getBlizzardWindSpeed() {
        return blizzardWindSpeed;
    }

    public void setBlizzardWindSpeed(int blizzardWindSpeed) {
        this.blizzardWindSpeed = blizzardWindSpeed;
    }

    public int getBlizzardGustSpeed() {
        return blizzardGustSpeed;
    }

    public void setBlizzardGustSpeed(int blizzardGustSpeed) {
        this.blizzardGustSpeed = blizzardGustSpeed;
    }

    public int getBlizzardMaxWindSpeed() {
        return blizzardMaxWindSpeed;
    }

    public void setBlizzardMaxWindSpeed(int blizzardMaxWindSpeed) {
        this.blizzardMaxWindSpeed = blizzardMaxWindSpeed;
    }

    public int getBlizzardVisibility() {
        return blizzardVisibility;
    }

    public void setBlizzardVisibility(int blizzardVisibility) {
        this.blizzardVisibility = blizzardVisibility;
    }

    public float getFrzHourlyPrecip() {
        return frzHourlyPrecip;
    }

    public void setFrzHourlyPrecip(float frzHourlyPrecip) {
        this.frzHourlyPrecip = frzHourlyPrecip;
    }

    public int getFrzTemperature() {
        return frzTemperature;
    }

    public void setFrzTemperature(int frzTemperature) {
        this.frzTemperature = frzTemperature;
    }

    public int getHswSnowDepth() {
        return hswSnowDepth;
    }

    public void setHswSnowDepth(int hswSnowDepth) {
        this.hswSnowDepth = hswSnowDepth;
    }

    public int getHswSnincrHourly() {
        return hswSnincrHourly;
    }

    public void setHswSnincrHourly(int hswSnincrHourly) {
        this.hswSnincrHourly = hswSnincrHourly;
    }

    public int getHswSnincrTotal() {
        return hswSnincrTotal;
    }

    public void setHswSnincrTotal(int hswSnincrTotal) {
        this.hswSnincrTotal = hswSnincrTotal;
    }

    public float getHourlyPrecip() {
        return hourlyPrecip;
    }

    public void setHourlyPrecip(float hourlyPrecip) {
        this.hourlyPrecip = hourlyPrecip;
    }

    public int getWindchill() {
        return windchill;
    }

    public void setWindchill(int windchill) {
        this.windchill = windchill;
    }

    public int getFrostbiteTime() {
        return frostbiteTime;
    }

    public void setFrostbiteTime(int frostbiteTime) {
        this.frostbiteTime = frostbiteTime;
    }

    public int getSnowDepth() {
        return snowDepth;
    }

    public void setSnowDepth(int snowDepth) {
        this.snowDepth = snowDepth;
    }

    public int getSnincrHourly() {
        return snincrHourly;
    }

    public void setSnincrHourly(int snincrHourly) {
        this.snincrHourly = snincrHourly;
    }

    public int getSnincrTotal() {
        return snincrTotal;
    }

    public void setSnincrTotal(int snincrTotal) {
        this.snincrTotal = snincrTotal;
    }

    public int getRelativeHumidity() {
        return relativeHumidity;
    }

    public void setRelativeHumidity(int relativeHumidity) {
        this.relativeHumidity = relativeHumidity;
    }

    public int getCeiling() {
        return ceiling;
    }

    public void setCeiling(int ceiling) {
        this.ceiling = ceiling;
    }

    public int getDewpointDepr() {
        return dewpointDepr;
    }

    public void setDewpointDepr(int dewpointDepr) {
        this.dewpointDepr = dewpointDepr;
    }

}
