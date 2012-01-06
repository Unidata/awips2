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
package com.raytheon.uf.viz.monitor;

import java.util.HashMap;

import com.raytheon.uf.common.monitor.data.ObConst.ChosenAppKey;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.data.ObConst.ThreatLevel;
import com.raytheon.uf.viz.monitor.data.SafeSeasThresholdStruct;
import com.raytheon.uf.viz.monitor.util.MonitorThresholdConfiguration;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.FogDisplay;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.FogMonitor;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SafeSeasDisplay;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SafeSeasMonitor;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SnowDisplay;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SnowMonitor;

/**
 * The MonitorThresholdDataFactory class is a "Simple Factory Pattern" that
 * creates a monitor threshold data entity that uses the same data class layout
 * as the legacy SSthresholdStruct.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2009 2047       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public final class MonitorThresholdDataFactory {

    // Private constructor.
    private MonitorThresholdDataFactory() {
    }

    /**
     * createMonitorThresholdData is a static method in a simple factory pattern
     * that news-up a HashMap of MonitorThresholdStruct objects requested.
     * 
     * @param app
     *            -- the kind of application in decision assistance tool suite
     * @param usage
     *            -- the usage of the threshold data, either monitor or display
     * @param level
     *            -- the threat level of the threshold data, e.g., red or yellow
     * @param areaId
     *            -- array of area IDs
     * @param config
     *            -- MonitorThresholdConfiguration object that holds the
     *            configuration data for the app and usage.
     * @return -- HashMap of MonitorThresholdStruct objects keyed by the areaId.
     */
    public static HashMap<String, SafeSeasThresholdStruct> createMonitorThresholdData(
            ChosenAppKey app, DataUsageKey usage, ThreatLevel level,
            String[] areaId, MonitorThresholdConfiguration config) {
        HashMap<String, SafeSeasThresholdStruct> data = new HashMap<String, SafeSeasThresholdStruct>();

        for (int i = 0; i < areaId.length; i++) {
            data.put(areaId[i], createMonitorThresholdData(app, usage, level,
                    areaId[i], config));
        }

        return data;
    }

    /**
     * createMonitorThresholdData is a static method in a simple factory pattern
     * that news-up the MonitorThresholdStruct object requested.
     * 
     * @param app
     *            -- the kind of application in decision assistance tool suite
     * @param usage
     *            -- the usage of the threshold data, either monitor or display
     * @param level
     *            -- the threat level of the threshold data, e.g., red or yellow
     * @param areaID
     *            -- the area ID
     * @param config
     *            -- MonitorThresholdConfiguration object that holds the
     *            configuration data for the app and usage.
     * @return the MonitorThresholdStruct object
     */
    public static SafeSeasThresholdStruct createMonitorThresholdData(
            ChosenAppKey app, DataUsageKey usage, ThreatLevel level,
            String areaId, MonitorThresholdConfiguration config) {
        SafeSeasThresholdStruct data = new SafeSeasThresholdStruct();

        if (app == ChosenAppKey.SAFESEAS) {
            if (usage == DataUsageKey.DISPLAY) {
                // SAFESEAS Display Product Small Craft Advisory
                data.setScaWindSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_SCA_WIND_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setScaGustSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_SCA_GUST_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setScaMaxWindSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_SCA_PEAK_WIND.getXmlKey())
                        .getValueInt(level));
                data.setScaWaveHeight(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_SCA_WAVE_HT.getXmlKey())
                        .getValueInt(level));

                // SAFESEAS Display Product Gale Warning
                data.setGaleWindSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_GALE_WIND_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setGaleGustSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_GALE_GUST_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setGaleMaxWindSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_GALE_PEAK_WIND.getXmlKey())
                        .getValueInt(level));

                // SAFESEAS Display Product Storm Warning
                data.setStormWindSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_STORM_WIND_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setStormGustSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_STORM_GUST_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setStormMaxWindSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_STORM_PEAK_WIND.getXmlKey())
                        .getValueInt(level));

                // SAFESEAS Display Product Hurricane Warning
                data.setHurricaneWindSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_HFWW_WIND_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setHurricaneGustSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_HFWW_GUST_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setHurricaneMaxWindSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_PROD_HFWW_PEAK_WIND.getXmlKey())
                        .getValueInt(level));

                // SAFESEAS Display Wind
                data.setWindSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_WIND_WIND_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setMaxWindSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_WIND_PEAK_WIND.getXmlKey())
                        .getValueInt(level));
                data.setGustSpeed(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_WIND_GUST_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setWindDir1(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_WIND_DIR_FROM.getXmlKey()).getValueInt(
                        level));
                data.setWindDir2(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_WIND_DIR_TO.getXmlKey()).getValueInt(
                        level));

                // SAFESEAS Display Meteo
                data.setVisibility(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_METEO_VIS.getXmlKey()).getValueInt(
                        level));
                data.setTemperature(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_METEO_TEMP.getXmlKey()).getValueInt(
                        level));
                data.setDewpoint(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_METEO_DEWPT.getXmlKey())
                        .getValueInt(level));
                data.setPressure(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_METEO_SLP.getXmlKey()).getValueInt(
                        level));
                data.setSeaSurfaceTemp(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_METEO_SST.getXmlKey()).getValueInt(
                        level));
                data.setWaveHeight(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_METEO_WAVE_HT.getXmlKey())
                        .getValueInt(level));
                data.setWaveSteepness(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_METEO_WAVE_STEEP.getXmlKey())
                        .getValueInt(level));

                // SAFESEAS Display Swell primary
                data.setPrimSwellHt(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT.getXmlKey())
                        .getValueInt(level));
                data.setPrimSwellPd(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD.getXmlKey())
                        .getValueInt(level));
                data.setPrimSwellDir1(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_FROM.getXmlKey())
                        .getValueInt(level));
                data.setPrimSwellDir2(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_TO.getXmlKey())
                        .getValueInt(level));

                // SAFESEAS Display Swell Secondary
                data.setSecSwellHt(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_SWELL_SEC_HT.getXmlKey())
                        .getValueInt(level));
                data.setSecSwellPd(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_SWELL_SEC_PD.getXmlKey())
                        .getValueInt(level));
                data.setSecSwellDir1(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM.getXmlKey())
                        .getValueInt(level));
                data.setSecSwellDir2(config.getValue(areaId,
                        SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_TO.getXmlKey())
                        .getValueInt(level));
            } else if (usage == DataUsageKey.MONITOR) {
                // SAFESEAS Monitor Meteo
                data.setWindSpeed(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_METEO_WIND_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setMaxWindSpeed(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_METEO_PEAK_WIND.getXmlKey())
                        .getValueInt(level));
                data.setGustSpeed(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_METEO_GUST_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setWaveHeight(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_METEO_WAVE_HT.getXmlKey())
                        .getValueInt(level));
                data.setVisibility(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_METEO_VIS.getXmlKey()).getValueInt(
                        level));

                // SAFESEAS Monitor Swell Primary
                data.setPrimSwellHt(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_SWELL_PRIM_HT.getXmlKey())
                        .getValueInt(level));
                data.setPrimSwellPd(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_SWELL_PRIM_PD.getXmlKey())
                        .getValueInt(level));
                data.setPrimSwellDir1(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_SWELL_PRIM_DIR_FROM.getXmlKey())
                        .getValueInt(level));
                data.setPrimSwellDir2(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_SWELL_PRIM_DIR_TO.getXmlKey())
                        .getValueInt(level));

                // SAFESEAS Monitor Swell Secondary
                data.setSecSwellHt(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_SWELL_SEC_HT.getXmlKey())
                        .getValueInt(level));
                data.setSecSwellPd(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_SWELL_SEC_PD.getXmlKey())
                        .getValueInt(level));
                data.setSecSwellDir1(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_SWELL_SEC_DIR_FROM.getXmlKey())
                        .getValueInt(level));
                data.setSecSwellDir2(config.getValue(areaId,
                        SafeSeasMonitor.SS_MON_SWELL_SEC_DIR_TO.getXmlKey())
                        .getValueInt(level));
            }
        } else if (app == ChosenAppKey.SNOW) {
            if (usage == DataUsageKey.DISPLAY) {
                // SNOW Display Product Blizzard
                data.setBlizzardVisibility(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_PROD_BLIZZ_VIS.getXmlKey())
                        .getValueInt(level));
                data.setBlizzardWindSpeed(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_PROD_BLIZZ_WIND_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setBlizzardGustSpeed(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_PROD_BLIZZ_GUST_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setBlizzardMaxWindSpeed(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_PROD_BLIZZ_PEAK_WIND.getXmlKey())
                        .getValueInt(level));

                // SNOW Display Product Freezing Precipitation
                data.setFrzTemperature(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_PROD_FRZ_TEMP.getXmlKey())
                        .getValueInt(level));
                data.setFrzHourlyPrecip(config.getValue(areaId,
                                        SnowDisplay.SNOW_DISP_PROD_FRZ_HOURLY_PRECIP.getXmlKey())
                                .getValueInt(level));
                data.setHswSnincrHourly(config.getValue(areaId,
                                        SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_HOURLY.getXmlKey())
                                .getValueInt(level));
                data.setHswSnincrTotal(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_TOTAL.getXmlKey())
                        .getValueInt(level));
                data.setHswSnowDepth(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_PROD_HSW_SNOW_DEPTH.getXmlKey())
                        .getValueInt(level));

                // SNOW Display Product Wind
                data.setWindSpeed(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_WIND_WIND_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setMaxWindSpeed(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_WIND_PEAK_WIND.getXmlKey())
                        .getValueInt(level));
                data.setGustSpeed(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_WIND_GUST_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setWindDir1(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_WIND_DIR_FROM.getXmlKey())
                        .getValueInt(level));
                data.setWindDir2(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_WIND_DIR_TO.getXmlKey())
                        .getValueInt(level));

                // SNOW Display Meteo
                data.setTemperature(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_METEO_TEMP.getXmlKey())
                        .getValueInt(level));
                data.setDewpoint(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_METEO_DEWPT.getXmlKey())
                        .getValueInt(level));
                data.setVisibility(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_METEO_VIS.getXmlKey())
                        .getValueInt(level));
                data.setPressure(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_METEO_SLP.getXmlKey())
                        .getValueInt(level));
                data.setHourlyPrecip(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP.getXmlKey())
                        .getValueInt(level));
                data.setWindchill(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_METEO_WIND_CHILL.getXmlKey())
                        .getValueInt(level));
                data.setFrostbiteTime(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_METEO_FROSTBITE.getXmlKey())
                        .getValueInt(level));
                data.setSnowDepth(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_METEO_SNOW_DEPTH.getXmlKey())
                        .getValueInt(level));
                data.setSnincrHourly(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_METEO_SNINCR_HOURLY.getXmlKey())
                        .getValueInt(level));
                data.setSnincrTotal(config.getValue(areaId,
                        SnowDisplay.SNOW_DISP_METEO_SNINCR_TOTAL.getXmlKey())
                        .getValueInt(level));
            } else if (usage == DataUsageKey.MONITOR) {
                // SNOW Monitor Meteo
                data.setWindSpeed(config.getValue(areaId,
                        SnowMonitor.SNOW_MON_METEO_WIND_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setMaxWindSpeed(config.getValue(areaId,
                        SnowMonitor.SNOW_MON_METEO_PEAK_WIND.getXmlKey())
                        .getValueInt(level));
                data.setGustSpeed(config.getValue(areaId,
                        SnowMonitor.SNOW_MON_METEO_GUST_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setTemperature(config.getValue(areaId,
                        SnowMonitor.SNOW_MON_METEO_TEMP.getXmlKey())
                        .getValueInt(level));
                data.setWindchill(config.getValue(areaId,
                        SnowMonitor.SNOW_MON_METEO_WIND_CHILL.getXmlKey())
                        .getValueInt(level));
                data.setVisibility(config.getValue(areaId,
                        SnowMonitor.SNOW_MON_METEO_VIS.getXmlKey()).getValueInt(
                        level));
                data.setSnowDepth(config.getValue(areaId,
                        SnowMonitor.SNOW_MON_METEO_SNOW_DEPTH.getXmlKey())
                        .getValueInt(level));
            }
        } else if (app == ChosenAppKey.FOG) {
            if (usage == DataUsageKey.DISPLAY) {
                // Fog Display Meteo
                data.setVisibility(config.getValue(areaId,
                        FogDisplay.FOG_DISP_METEO_VIS.getXmlKey()).getValueInt(
                        level));
                data.setCeiling(config.getValue(areaId,
                        FogDisplay.FOG_DISP_METEO_CEILING.getXmlKey())
                        .getValueInt(level));
                data.setTemperature(config.getValue(areaId,
                        FogDisplay.FOG_DISP_METEO_TEMP.getXmlKey())
                        .getValueInt(level));
                data.setDewpointDepr(config.getValue(areaId,
                        FogDisplay.FOG_DISP_METEO_T_TD.getXmlKey())
                        .getValueInt(level));
                data.setRelativeHumidity(config.getValue(areaId,
                        FogDisplay.FOG_DISP_METEO_REL_HUMIDITY.getXmlKey())
                        .getValueInt(level));

                // Fog Display Wind
                data.setWindSpeed(config.getValue(areaId,
                        FogDisplay.FOG_DISP_WIND_WIND_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setMaxWindSpeed(config.getValue(areaId,
                        FogDisplay.FOG_DISP_WIND_PEAK_WIND.getXmlKey())
                        .getValueInt(level));
                data.setGustSpeed(config.getValue(areaId,
                        FogDisplay.FOG_DISP_WIND_GUST_SPEED.getXmlKey())
                        .getValueInt(level));
                data.setWindDir1(config.getValue(areaId,
                        FogDisplay.FOG_DISP_WIND_DIR_FROM.getXmlKey())
                        .getValueInt(level));
                data.setWindDir2(config.getValue(areaId,
                        FogDisplay.FOG_DISP_WIND_DIR_TO.getXmlKey())
                        .getValueInt(level));
            } else if (usage == DataUsageKey.MONITOR) {
                // Fog Monitor Meteo
                data.setVisibility(config.getValue(areaId,
                        FogMonitor.FOG_MONITOR_METEO_VIS.getXmlKey())
                        .getValueInt(level));
            }
        }

        return data;
    }
}
