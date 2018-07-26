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

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.DisplayVarName;
import com.raytheon.uf.common.monitor.data.ObConst.ThreatLevel;

/**
 * This class holds a collection of SAFESEAS' display thresholds. Two methods
 * are provided to retrieve the specified display threshold, one for
 * single-valued thresholds and another for dual-valued thresholds.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2009 2047       grichard    Initial creation.
 * Jan 26, 2010 4268       skorolev    Corrected SWELL DisplayVarNames
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class DisplayThresholdsSet {

    // The display threshold map of maps for single-valued monitor thresholds
    private Map<ObConst.DisplayVarName, Map<ObConst.ThreatLevel, Float>> dispThreshMap = new HashMap<ObConst.DisplayVarName, Map<ObConst.ThreatLevel, Float>>();

    // The display threshold map of maps for dual-valued monitor thresholds
    private Map<ObConst.DisplayVarName, Map<ObConst.ThreatLevel, Float[]>> dispDualThreshMap = new HashMap<ObConst.DisplayVarName, Map<ObConst.ThreatLevel, Float[]>>();

    // The display threshold for single-valued display thresholds
    private Map<ObConst.ThreatLevel, Float> dispThresh;

    // The display threshold for dual-valued display thresholds
    private Map<ObConst.ThreatLevel, Float[]> dispDualThresh;

    // Constructor -- takes "red" and "yellow" threshold data classes as
    // arguments
    public DisplayThresholdsSet(final SafeSeasThresholdStruct redThresh,
            final SafeSeasThresholdStruct yellowThresh) {

        // SCA Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.scaWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.scaWindSpeed));
        dispThreshMap.put(DisplayVarName.SCA_WIND_SPEED, dispThresh);

        // SCA Gust Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.scaGustSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.scaGustSpeed));
        dispThreshMap.put(DisplayVarName.SCA_GUST_SPEED, dispThresh);

        // SCA Max Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.scaMaxWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.scaMaxWindSpeed));
        dispThreshMap.put(DisplayVarName.SCA_MAX_WIND_SPEED, dispThresh);

        // SCA Wave Height
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.scaWaveHeight));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.scaWaveHeight));
        dispThreshMap.put(DisplayVarName.SCA_WAVE_HEIGHT, dispThresh);

        // Gale Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.galeWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.galeGustSpeed));
        dispThreshMap.put(DisplayVarName.GALE_WIND_SPEED, dispThresh);

        // Gale Gust Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.galeGustSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.galeGustSpeed));
        dispThreshMap.put(DisplayVarName.GALE_GUST_SPEED, dispThresh);

        // Gale Max Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.galeMaxWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.galeMaxWindSpeed));
        dispThreshMap.put(DisplayVarName.GALE_MAX_WIND_SPEED, dispThresh);

        // Storm Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh
                .put(ThreatLevel.RED, Float.valueOf(redThresh.stormWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.stormWindSpeed));
        dispThreshMap.put(DisplayVarName.STORM_WIND_SPEED, dispThresh);

        // Storm Gust Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh
                .put(ThreatLevel.RED, Float.valueOf(redThresh.stormGustSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.stormGustSpeed));
        dispThreshMap.put(DisplayVarName.STORM_GUST_SPEED, dispThresh);

        // Storm Max Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.stormMaxWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.stormMaxWindSpeed));
        dispThreshMap.put(DisplayVarName.STORM_MAX_WIND_SPEED, dispThresh);

        // Hurricane Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.hurricaneWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.hurricaneWindSpeed));
        dispThreshMap.put(DisplayVarName.HURRICANE_WIND_SPEED, dispThresh);

        // Hurricane Gust Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.hurricaneGustSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.hurricaneGustSpeed));
        dispThreshMap.put(DisplayVarName.HURRICANE_GUST_SPEED, dispThresh);

        // Hurricane Max Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.hurricaneMaxWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.hurricaneMaxWindSpeed));
        dispThreshMap.put(DisplayVarName.HURRICANE_MAX_WIND_SPEED, dispThresh);

        // Var Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.windSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.windSpeed));
        dispThreshMap.put(DisplayVarName.VAR_WIND_SPEED, dispThresh);

        // Var Max Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.maxWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.maxWindSpeed));
        dispThreshMap.put(DisplayVarName.VAR_MAX_WIND_SPEED, dispThresh);

        // Var Gust Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.gustSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.gustSpeed));
        dispThreshMap.put(DisplayVarName.VAR_GUST_SPEED, dispThresh);

        // Var Wave Height
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.waveHeight));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.waveHeight));
        dispThreshMap.put(DisplayVarName.VAR_WAVE_HEIGHT, dispThresh);

        // Var Wave Steepness
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.waveSteepness));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.waveSteepness));
        dispThreshMap.put(DisplayVarName.VAR_WAVE_STEEPNESS, dispThresh);

        // Var visibility in statute miles
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.visibility));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.visibility));
        dispThreshMap.put(DisplayVarName.VAR_VISIBILITY, dispThresh);

        // Var Temperature
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.temperature));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.temperature));
        dispThreshMap.put(DisplayVarName.VAR_TEMPERATURE, dispThresh);

        // Var Dewpoint
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.dewpoint));
        dispThresh
                .put(ThreatLevel.YELLOW, Float.valueOf(yellowThresh.dewpoint));
        dispThreshMap.put(DisplayVarName.VAR_DEWPOINT, dispThresh);

        // Var Dewpoint Depression
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.dewpointDepr));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.dewpointDepr));
        dispThreshMap.put(DisplayVarName.VAR_DEWPOINT_DEPR, dispThresh);

        // Var Pressure
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.pressure));
        dispThresh
                .put(ThreatLevel.YELLOW, Float.valueOf(yellowThresh.pressure));
        dispThreshMap.put(DisplayVarName.VAR_PRESSURE, dispThresh);

        // Var Sea Surface Temperature
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh
                .put(ThreatLevel.RED, Float.valueOf(redThresh.seaSurfaceTemp));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.seaSurfaceTemp));
        dispThreshMap.put(DisplayVarName.VAR_SEA_SURFACE_TEMPERATURE,
                dispThresh);

        // Var primary swell height
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.primSwellHt));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.primSwellHt));
        dispThreshMap.put(DisplayVarName.VAR_PRIM_SWELL_HT, dispThresh);

        // Var primary swell period
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.primSwellPd));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.primSwellPd));
        dispThreshMap.put(DisplayVarName.VAR_PRIM_SWELL_PD, dispThresh);

        // Var secondary swell height
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.secSwellHt));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.secSwellHt));
        dispThreshMap.put(DisplayVarName.VAR_SEC_SWELL_HT, dispThresh);

        // Var secondary swell period
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.secSwellPd));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.secSwellPd));
        dispThreshMap.put(DisplayVarName.VAR_SEC_SWELL_PD, dispThresh);

        // Blizzard Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.blizzardWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.blizzardWindSpeed));
        dispThreshMap.put(DisplayVarName.BLIZ_WIND_SPEED, dispThresh);

        // Blizzard Gust Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.blizzardGustSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.blizzardGustSpeed));
        dispThreshMap.put(DisplayVarName.BLIZ_GUST_SPEED, dispThresh);

        // Blizzard Max Wind Speed
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.blizzardMaxWindSpeed));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.blizzardMaxWindSpeed));
        dispThreshMap.put(DisplayVarName.BLIZ_MAX_WIND_SPEED, dispThresh);

        // Blizzard visibility in statute miles
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.blizzardVisibility));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.blizzardVisibility));
        dispThreshMap.put(DisplayVarName.BLIZ_VISIBILITY, dispThresh);

        // Frozen hourly precipitation
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.frzHourlyPrecip));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.frzHourlyPrecip));
        dispThreshMap.put(DisplayVarName.FRZ_HOURLY_PRECIP, dispThresh);

        // Frozen temperature
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh
                .put(ThreatLevel.RED, Float.valueOf(redThresh.frzTemperature));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.frzTemperature));
        dispThreshMap.put(DisplayVarName.FRZ_TEMPERATURE, dispThresh);

        // HSW snow depth
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.hswSnowDepth));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.hswSnowDepth));
        dispThreshMap.put(DisplayVarName.HSW_SNOW_DEPTH, dispThresh);

        // HSW snow increasing hourly
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.hswSnincrHourly));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.hswSnincrHourly));
        dispThreshMap.put(DisplayVarName.HSW_SNINCR_HOURLY, dispThresh);

        // HSW snow increasing total
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh
                .put(ThreatLevel.RED, Float.valueOf(redThresh.hswSnincrTotal));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.hswSnincrTotal));
        dispThreshMap.put(DisplayVarName.HSW_SNINCR_TOTAL, dispThresh);

        // Var snow increasing hourly
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.snincrHourly));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.snincrHourly));
        dispThreshMap.put(DisplayVarName.VAR_SNINCR_HOURLY, dispThresh);

        // Var snow increasing total
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.snincrTotal));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.snincrTotal));
        dispThreshMap.put(DisplayVarName.VAR_SNINCR_TOTAL, dispThresh);

        // Var wind chill
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.windchill));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.windchill));
        dispThreshMap.put(DisplayVarName.VAR_WIND_CHILL, dispThresh);

        // Var snow depth
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.snowDepth));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.snowDepth));
        dispThreshMap.put(DisplayVarName.VAR_SNOW_DEPTH, dispThresh);

        // Var hourly precipitation
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.hourlyPrecip));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.hourlyPrecip));
        dispThreshMap.put(DisplayVarName.VAR_HOURLY_PRECIP, dispThresh);

        // Var frostbite time
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.frostbiteTime));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.frostbiteTime));
        dispThreshMap.put(DisplayVarName.VAR_FROSTBITE_TIME, dispThresh);

        // Var relative humidity
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float
                .valueOf(redThresh.relativeHumidity));
        dispThresh.put(ThreatLevel.YELLOW, Float
                .valueOf(yellowThresh.relativeHumidity));
        dispThreshMap.put(DisplayVarName.VAR_RELATIVE_HUMIDITY, dispThresh);

        // Var ceiling
        dispThresh = new HashMap<ObConst.ThreatLevel, Float>();
        dispThresh.put(ThreatLevel.RED, Float.valueOf(redThresh.ceiling));
        dispThresh.put(ThreatLevel.YELLOW, Float.valueOf(yellowThresh.ceiling));
        dispThreshMap.put(DisplayVarName.VAR_CEILING, dispThresh);

        // Var primary swell direction
        dispDualThresh = new HashMap<ObConst.ThreatLevel, Float[]>();
        Float[] prf = { Float.valueOf(redThresh.primSwellDir1),
                Float.valueOf(redThresh.primSwellDir2) };
        dispDualThresh.put(ThreatLevel.RED, prf);
        Float[] pyf = { Float.valueOf(yellowThresh.primSwellDir1),
                Float.valueOf(yellowThresh.primSwellDir2) };
        dispDualThresh.put(ThreatLevel.YELLOW, pyf);
        dispDualThreshMap.put(DisplayVarName.VAR_PRIM_SWELL_DIR, dispDualThresh);

        // Var secondary swell direction
        dispDualThresh = new HashMap<ObConst.ThreatLevel, Float[]>();
        Float[] srf = { Float.valueOf(redThresh.secSwellDir1),
                Float.valueOf(redThresh.secSwellDir2) };
        dispDualThresh.put(ThreatLevel.RED, srf);
        Float[] syf = { Float.valueOf(yellowThresh.secSwellDir1),
                Float.valueOf(yellowThresh.secSwellDir2) };
        dispDualThresh.put(ThreatLevel.YELLOW, syf);
        dispDualThreshMap.put(DisplayVarName.VAR_SEC_SWELL_DIR, dispDualThresh);

        // Var wind direction
        dispDualThresh = new HashMap<ObConst.ThreatLevel, Float[]>();
        Float[] rf = { Float.valueOf(redThresh.windDir1),
                Float.valueOf(redThresh.windDir2) };
        dispDualThresh.put(ThreatLevel.RED, rf);
        Float[] yf = { Float.valueOf(yellowThresh.windDir1),
                Float.valueOf(yellowThresh.windDir2) };
        dispDualThresh.put(ThreatLevel.YELLOW, yf);
        dispDualThreshMap.put(DisplayVarName.VAR_WIND_DIR, dispDualThresh);

    }

    /**
     * Method that obtains the display threshold
     * 
     * @param varName
     *            -- the variable name
     * @param threatLevel
     *            -- the threat level
     * @return the display threshold value
     */
    public float getThreshold(ObConst.DisplayVarName varName,
            ObConst.ThreatLevel threatLevel) {
        Map<ObConst.ThreatLevel, Float> dispThresh;
        dispThresh = dispThreshMap.get(varName);
        return dispThresh.get(threatLevel);
    }

    /**
     * Method that obtains the display threshold
     * 
     * @param varName
     *            -- the variable name
     * @param threatLevel
     *            -- the threat level
     * @return the display threshold values
     */
    public Float[] getThresholds(ObConst.DisplayVarName varName,
            ObConst.ThreatLevel threatLevel) {
        Map<ObConst.ThreatLevel, Float[]> dispDualThresh;
        dispDualThresh = dispDualThreshMap.get(varName);
        return dispDualThresh.get(threatLevel);
    }

}
