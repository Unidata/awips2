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
package com.raytheon.uf.common.monitor.data;

import java.util.HashMap;

import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;

/**
 * Monitor Configuration Constants.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2009 2076       avarani     Initial creation
 * Sep 18, 2015 3873       skorolev    Corrected appName.
 * Jan 04, 2016 5115       skorolev    Moved from com.raytheon.uf.viz.monitor.util. Corrected misspelling.
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */
public class MonitorConfigConstants {
    private static HashMap<String, Boolean> ryHigherMapSS;

    private static HashMap<String, Boolean> ryHigherMapSnow;

    private static HashMap<String, Boolean> ryHigherMapFog;

    private static boolean rankSwellPeriodHigh = false;

    /**
     * Monitoring parameters
     */
    public final static String MON_METEO_WIND_SPEED = "Monitor.Meteo.wind_speed";

    public final static String MON_METEO_GUST_SPEED = "Monitor.Meteo.gust_speed";

    public final static String MON_METEO_PEAK_WIND = "Monitor.Meteo.peak_wind_speed";

    public final static String MON_METEO_WAVE_HT = "Monitor.Meteo.wave_height";

    public final static String MON_METEO_VIS = "Monitor.Meteo.visibility";

    public final static String MON_SWELL_PRIM_HT = "Monitor.Swell.Primary.height";

    public final static String MON_SWELL_PRIM_PD = "Monitor.Swell.Primary.period";

    public final static String MON_SWELL_PRIM_DIR_FROM = "Monitor.Swell.Primary.dirFrom";

    public final static String MON_SWELL_PRIM_DIR_TO = "Monitor.Swell.Primary.dirTo";

    public final static String MON_SWELL_SEC_HT = "Monitor.Swell.Secondary.height";

    public final static String MON_SWELL_SEC_PD = "Monitor.Swell.Secondary.period";

    public final static String MON_SWELL_SEC_DIR_FROM = "Monitor.Swell.Secondary.dirFrom";

    public final static String MON_SWELL_SEC_DIR_TO = "Monitor.Swell.Secondary.dirTo";

    public final static String MON_METEO_TEMP = "Monitor.Meteo.temperature";

    public final static String MON_METEO_WIND_CHILL = "Monitor.Meteo.wind_chill";

    public final static String MON_METEO_SNOW_DEPTH = "Monitor.Meteo.snow_depth";

    /*
     * SAFESEAS
     */
    public static enum SafeSeasDisplay {
        SS_DISP_PROD_SCA_WIND_SPEED("Display.Product.SCA.wind_speed"), SS_DISP_PROD_SCA_GUST_SPEED(
                "Display.Product.SCA.gust_speed"), SS_DISP_PROD_SCA_PEAK_WIND(
                "Display.Product.SCA.peak_wind_speed"), SS_DISP_PROD_SCA_WAVE_HT(
                "Display.Product.SCA.wave_height"), SS_DISP_PROD_GALE_WIND_SPEED(
                "Display.Product.Gale.wind_speed"), SS_DISP_PROD_GALE_GUST_SPEED(
                "Display.Product.Gale.gust_speed"), SS_DISP_PROD_GALE_PEAK_WIND(
                "Display.Product.Gale.peak_wind_speed"), SS_DISP_PROD_STORM_WIND_SPEED(
                "Display.Product.Storm.wind_speed"), SS_DISP_PROD_STORM_GUST_SPEED(
                "Display.Product.Storm.gust_speed"), SS_DISP_PROD_STORM_PEAK_WIND(
                "Display.Product.Storm.peak_wind_speed"), SS_DISP_PROD_HFWW_WIND_SPEED(
                "Display.Product.Hurricane.wind_speed"), SS_DISP_PROD_HFWW_GUST_SPEED(
                "Display.Product.Hurricane.gust_speed"), SS_DISP_PROD_HFWW_PEAK_WIND(
                "Display.Product.Hurricane.peak_wind_speed"), SS_DISP_WIND_WIND_SPEED(
                "Display.Wind.wind_speed"), SS_DISP_WIND_PEAK_WIND(
                "Display.Wind.peak_wind_speed"), SS_DISP_WIND_GUST_SPEED(
                "Display.Wind.gust_speed"), SS_DISP_WIND_DIR_FROM(
                "Display.Wind.dirFrom"), SS_DISP_WIND_DIR_TO(
                "Display.Wind.dirTo"), SS_DISP_METEO_VIS(
                "Display.Meteo.visibility"), SS_DISP_METEO_TEMP(
                "Display.Meteo.temperature"), SS_DISP_METEO_DEWPT(
                "Display.Meteo.dewpoint"), SS_DISP_METEO_SLP(
                "Display.Meteo.slp"), SS_DISP_METEO_SST(
                "Display.Meteo.sea_surface_temp"), SS_DISP_METEO_WAVE_HT(
                "Display.Meteo.wave_height"), SS_DISP_METEO_WAVE_STEEP(
                "Display.Meteo.wave_steepness"), SS_DISP_SWELL_PRIM_HT(
                "Display.Swell.Primary.height"), SS_DISP_SWELL_PRIM_PD(
                "Display.Swell.Primary.period"), SS_DISP_SWELL_PRIM_DIR_FROM(
                "Display.Swell.Primary.dirFrom"), SS_DISP_SWELL_PRIM_DIR_TO(
                "Display.Swell.Primary.dirTo"), SS_DISP_SWELL_SEC_HT(
                "Display.Swell.Secondary.height"), SS_DISP_SWELL_SEC_PD(
                "Display.Swell.Secondary.period"), SS_DISP_SWELL_SEC_DIR_FROM(
                "Display.Swell.Secondary.dirFrom"), SS_DISP_SWELL_SEC_DIR_TO(
                "Display.Swell.Secondary.dirTo");

        String XMLKey;

        SafeSeasDisplay(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }

    public static enum SafeSeasMonitor {
        SS_MON_METEO_WIND_SPEED(MON_METEO_WIND_SPEED), SS_MON_METEO_PEAK_WIND(
                MON_METEO_PEAK_WIND), SS_MON_METEO_GUST_SPEED(
                MON_METEO_GUST_SPEED), SS_MON_METEO_WAVE_HT(MON_METEO_WAVE_HT), SS_MON_METEO_VIS(
                MON_METEO_VIS), SS_MON_SWELL_PRIM_HT(MON_SWELL_PRIM_HT), SS_MON_SWELL_PRIM_PD(
                MON_SWELL_PRIM_PD), SS_MON_SWELL_PRIM_DIR_FROM(
                MON_SWELL_PRIM_DIR_FROM), SS_MON_SWELL_PRIM_DIR_TO(
                MON_SWELL_PRIM_DIR_TO), SS_MON_SWELL_SEC_HT(MON_SWELL_SEC_HT), SS_MON_SWELL_SEC_PD(
                MON_SWELL_SEC_PD), SS_MON_SWELL_SEC_DIR_FROM(
                MON_SWELL_SEC_DIR_FROM), SS_MON_SWELL_SEC_DIR_TO(
                MON_SWELL_SEC_DIR_TO);

        String XMLKey;

        SafeSeasMonitor(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }

    /*
     * SNOW
     */
    public static enum SnowDisplay {
        SNOW_DISP_PROD_BLIZZ_VIS("Display.Product.Blizzard.visibility"), SNOW_DISP_PROD_BLIZZ_WIND_SPEED(
                "Display.Product.Blizzard.wind_speed"), SNOW_DISP_PROD_BLIZZ_GUST_SPEED(
                "Display.Product.Blizzard.gust_speed"), SNOW_DISP_PROD_BLIZZ_PEAK_WIND(
                "Display.Product.Blizzard.peak_wind_speed"), SNOW_DISP_PROD_FRZ_TEMP(
                "Display.Product.Freezing_Precip.temperature"), SNOW_DISP_PROD_FRZ_HOURLY_PRECIP(
                "Display.Product.Freezing_Precip.hourly_precip"), SNOW_DISP_PROD_HSW_SNINCR_HOURLY(
                "Display.Product.Heavy_Snow_Warning.snincr_hourly"), SNOW_DISP_PROD_HSW_SNINCR_TOTAL(
                "Display.Product.Heavy_Snow_Warning.snincr_total"), SNOW_DISP_PROD_HSW_SNOW_DEPTH(
                "Display.Product.Heavy_Snow_Warning.snow_depth"), SNOW_DISP_WIND_WIND_SPEED(
                "Display.Product.Wind.wind_speed"), SNOW_DISP_WIND_PEAK_WIND(
                "Display.Product.Wind.peak_wind_speed"), SNOW_DISP_WIND_GUST_SPEED(
                "Display.Product.Wind.gust_speed"), SNOW_DISP_WIND_DIR_FROM(
                "Display.Product.Wind.dirFrom"), SNOW_DISP_WIND_DIR_TO(
                "Display.Product.Wind.dirTo"), SNOW_DISP_METEO_TEMP(
                "Display.Meteo.temperature"), SNOW_DISP_METEO_DEWPT(
                "Display.Meteo.dewpoint"), SNOW_DISP_METEO_VIS(
                "Display.Meteo.visibility"), SNOW_DISP_METEO_SLP(
                "Display.Meteo.slp"), SNOW_DISP_METEO_HOURLY_PRECIP(
                "Display.Meteo.hourly_precip"), SNOW_DISP_METEO_WIND_CHILL(
                "Display.Meteo.wind_chill"), SNOW_DISP_METEO_FROSTBITE(
                "Display.Meteo.frostbite_time"), SNOW_DISP_METEO_SNOW_DEPTH(
                "Display.Meteo.snow_depth"), SNOW_DISP_METEO_SNINCR_HOURLY(
                "Display.Meteo.snincr_hourly"), SNOW_DISP_METEO_SNINCR_TOTAL(
                "Display.Meteo.snincr_total");

        String XMLKey;

        SnowDisplay(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }

    public static enum SnowMonitor {
        SNOW_MON_METEO_WIND_SPEED(MON_METEO_WIND_SPEED), SNOW_MON_METEO_PEAK_WIND(
                MON_METEO_PEAK_WIND), SNOW_MON_METEO_GUST_SPEED(
                MON_METEO_GUST_SPEED), SNOW_MON_METEO_TEMP(MON_METEO_TEMP), SNOW_MON_METEO_WIND_CHILL(
                MON_METEO_WIND_CHILL), SNOW_MON_METEO_VIS(MON_METEO_VIS), SNOW_MON_METEO_SNOW_DEPTH(
                MON_METEO_SNOW_DEPTH);

        String XMLKey;

        SnowMonitor(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }

    // FOG
    public static enum FogDisplay {
        FOG_DISP_METEO_VIS("Display.Meteo.visibility"), FOG_DISP_METEO_CEILING(
                "Display.Meteo.ceiling"), FOG_DISP_METEO_TEMP(
                "Display.Meteo.temperature"), FOG_DISP_METEO_DEWPT(
                "Display.Meteo.dewpoint"), FOG_DISP_METEO_T_TD(
                "Display.Meteo.t_td"), FOG_DISP_METEO_REL_HUMIDITY(
                "Display.Meteo.relative_humidity"), FOG_DISP_WIND_WIND_SPEED(
                "Display.Wind.wind_speed"), FOG_DISP_WIND_PEAK_WIND(
                "Display.Wind.peak_wind_speed"), FOG_DISP_WIND_GUST_SPEED(
                "Display.Wind.gust_speed"), FOG_DISP_WIND_DIR_FROM(
                "Display.Wind.dirFrom"), FOG_DISP_WIND_DIR_TO(
                "Display.Wind.dirTo");

        String XMLKey;

        FogDisplay(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }

    public static enum FogMonitor {
        FOG_MONITOR_METEO_VIS(MON_METEO_VIS);

        String XMLKey;

        FogMonitor(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }

    public static Boolean rValueIsHigher(String key, AppName appName) {
        if (appName.equals(AppName.SAFESEAS)) {
            if (ryHigherMapSS == null) {
                createRYHigherMapSS();
            }

            if (ryHigherMapSS.containsKey(key) == true) {
                return ryHigherMapSS.get(key);
            }
        } else if (appName.equals(AppName.SNOW)) {
            if (ryHigherMapSnow == null) {
                createRYHigherMapSnow();
            }

            if (ryHigherMapSnow.containsKey(key) == true) {
                return ryHigherMapSnow.get(key);
            }
        } else if (appName.equals(AppName.FOG)) {
            if (ryHigherMapFog == null) {
                createRYHigherMapFog();
            }

            if (ryHigherMapFog.containsKey(key) == true) {
                return ryHigherMapFog.get(key);
            }
        } else {
            // do nothing here
        }

        return false;
    }

    private static void createRYHigherMapSS() {
        ryHigherMapSS = new HashMap<String, Boolean>();

        /*
         * SafeSeas Display
         */
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_PROD_SCA_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_PROD_SCA_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_PROD_SCA_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_SCA_WAVE_HT.getXmlKey(),
                true);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_PROD_GALE_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_PROD_GALE_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_PROD_GALE_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSS
                .put(SafeSeasDisplay.SS_DISP_PROD_STORM_WIND_SPEED.getXmlKey(),
                        true);
        ryHigherMapSS
                .put(SafeSeasDisplay.SS_DISP_PROD_STORM_GUST_SPEED.getXmlKey(),
                        true);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_PROD_STORM_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_PROD_HFWW_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_PROD_HFWW_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_PROD_HFWW_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_WIND_WIND_SPEED.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_WIND_PEAK_WIND.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_WIND_GUST_SPEED.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_WIND_DIR_FROM.getXmlKey(),
                null);
        ryHigherMapSS
                .put(SafeSeasDisplay.SS_DISP_WIND_DIR_TO.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_VIS.getXmlKey(), false);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_TEMP.getXmlKey(), true);
        ryHigherMapSS
                .put(SafeSeasDisplay.SS_DISP_METEO_DEWPT.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_SLP.getXmlKey(), false);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_SST.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_WAVE_HT.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_WAVE_STEEP.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD.getXmlKey(),
                rankSwellPeriodHigh);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_FROM.getXmlKey(), null);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_TO.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_SEC_HT.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_SEC_PD.getXmlKey(),
                rankSwellPeriodHigh);
        ryHigherMapSS.put(
                SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_TO.getXmlKey(),
                null);

        /*
         * SafeSeas Monitor
         */
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_METEO_WIND_SPEED.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_METEO_PEAK_WIND.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_METEO_GUST_SPEED.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_METEO_WAVE_HT.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_METEO_VIS.getXmlKey(), false);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_PRIM_HT.getXmlKey(),
                true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_PRIM_PD.getXmlKey(),
                false);
        ryHigherMapSS.put(
                SafeSeasMonitor.SS_MON_SWELL_PRIM_DIR_FROM.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_PRIM_DIR_TO.getXmlKey(),
                null);
        ryHigherMapSS
                .put(SafeSeasMonitor.SS_MON_SWELL_SEC_HT.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_SEC_PD.getXmlKey(),
                false);
        ryHigherMapSS.put(
                SafeSeasMonitor.SS_MON_SWELL_SEC_DIR_FROM.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_SEC_DIR_TO.getXmlKey(),
                null);
    }

    private static void createRYHigherMapSnow() {
        ryHigherMapSnow = new HashMap<String, Boolean>();

        /*
         * Snow Display
         */
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_BLIZZ_VIS.getXmlKey(),
                false);
        ryHigherMapSnow.put(
                SnowDisplay.SNOW_DISP_PROD_BLIZZ_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSnow.put(
                SnowDisplay.SNOW_DISP_PROD_BLIZZ_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSnow.put(
                SnowDisplay.SNOW_DISP_PROD_BLIZZ_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_FRZ_TEMP.getXmlKey(),
                true);
        ryHigherMapSnow.put(
                SnowDisplay.SNOW_DISP_PROD_FRZ_HOURLY_PRECIP.getXmlKey(), true);
        ryHigherMapSnow.put(
                SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_HOURLY.getXmlKey(), true);
        ryHigherMapSnow.put(
                SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_TOTAL.getXmlKey(), true);
        ryHigherMapSnow.put(
                SnowDisplay.SNOW_DISP_PROD_HSW_SNOW_DEPTH.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_WIND_WIND_SPEED.getXmlKey(),
                true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_WIND_PEAK_WIND.getXmlKey(),
                true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_WIND_GUST_SPEED.getXmlKey(),
                true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_WIND_DIR_FROM.getXmlKey(),
                null);
        ryHigherMapSnow
                .put(SnowDisplay.SNOW_DISP_WIND_DIR_TO.getXmlKey(), null);
        ryHigherMapSnow
                .put(SnowDisplay.SNOW_DISP_METEO_TEMP.getXmlKey(), false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_DEWPT.getXmlKey(),
                false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_VIS.getXmlKey(), false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_SLP.getXmlKey(), false);
        ryHigherMapSnow.put(
                SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_WIND_CHILL.getXmlKey(),
                false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_FROSTBITE.getXmlKey(),
                false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_SNOW_DEPTH.getXmlKey(),
                true);
        ryHigherMapSnow.put(
                SnowDisplay.SNOW_DISP_METEO_SNINCR_HOURLY.getXmlKey(), true);
        ryHigherMapSnow.put(
                SnowDisplay.SNOW_DISP_METEO_SNINCR_TOTAL.getXmlKey(), true);

        /*
         * Snow Monitor
         */
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_WIND_SPEED.getXmlKey(),
                true);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_PEAK_WIND.getXmlKey(),
                true);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_GUST_SPEED.getXmlKey(),
                true);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_TEMP.getXmlKey(), false);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_WIND_CHILL.getXmlKey(),
                false);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_VIS.getXmlKey(), false);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_SNOW_DEPTH.getXmlKey(),
                true);
    }

    private static void createRYHigherMapFog() {
        ryHigherMapFog = new HashMap<String, Boolean>();
        /*
         * Fog Display
         */
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_VIS.getXmlKey(), false);
        ryHigherMapFog
                .put(FogDisplay.FOG_DISP_METEO_CEILING.getXmlKey(), false);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_TEMP.getXmlKey(), true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_DEWPT.getXmlKey(), true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_T_TD.getXmlKey(), false);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_REL_HUMIDITY.getXmlKey(),
                true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_WIND_WIND_SPEED.getXmlKey(),
                true);
        ryHigherMapFog
                .put(FogDisplay.FOG_DISP_WIND_PEAK_WIND.getXmlKey(), true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_WIND_GUST_SPEED.getXmlKey(),
                true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_WIND_DIR_FROM.getXmlKey(), null);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_WIND_DIR_TO.getXmlKey(), null);

        /*
         * Fog Monitor
         */
        ryHigherMapFog.put(FogMonitor.FOG_MONITOR_METEO_VIS.getXmlKey(), false);
    }

    public static final String[] TABLE_FIELDS = { "low", "mid", "upp", "rank",
            "trend", "mClr", "clucon", "inTbl", "min", "intrvl", "range",
            "alarm", "aAlarm", "units" };

    public static final String[] FFMP_TABLE = { "ffmpTable", "name", "rate",
            "qpe", "guidance", "ratio", "diff" };

    public static enum tableFields {
        LOW(0), MID(1), UPP(2), RANK(3), TREND(4), MCLR(5), CLUCON(6), INTBL(7), MIN(
                8), INTRVL(9), RANGE(10), ALARM(11), AALARM(12), UNITS(13);

        int index;

        tableFields(int i) {
            index = i;
        }

        @Override
        public String toString() {
            return TABLE_FIELDS[index];
        }
    }

    public static enum ffmpTable {
        NAME(1), RATE(2), QPE(3), QPF(4), GUIDANCE(5), RATIO(6), DIFF(7);

        int index;

        ffmpTable(int i) {
            index = i;
        }

        @Override
        public String toString() {
            return FFMP_TABLE[index];
        }

        public String toString(tableFields field) {
            return FFMP_TABLE[0] + "." + FFMP_TABLE[index] + "."
                    + field.toString();
        }
    }

    // Other Constants
    public static final String AREA_IDS_KEY = "AreaIDs.";

    public static final String ALL_ZONES_KEY = "AllZones";

    public static final String ACTIVE_ZONES_KEY = "ActiveZones";

    public static final String STATION_LIST_KEY = "StationList";

    public static final String STATIONS_KEY = "Stations";

    public static final String COLUMN_LIST_KEY = "ColumnList";

    public static final String SCAN_RADAR_ICAO = "radarIcao";

    public static final String SCAN_WMO_NUMBER = "wmoNumber";

    public static final String SCAN_START_INTERVAL = "startInterval";

    public static final String SCAN_CELL_TILT = "cellTilt";

    public static final String SCAN_DMD_TILT = "dmdTilt";

    private MonitorConfigConstants() {
        // no instantiation
    }

    public static void setRankSwellPeriodHigh(boolean rankSwellPeriodHigh) {
        MonitorConfigConstants.rankSwellPeriodHigh = rankSwellPeriodHigh;
        if (ryHigherMapSS != null) {
            ryHigherMapSS.put(
                    SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD.getXmlKey(),
                    rankSwellPeriodHigh);
            ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_SEC_PD.getXmlKey(),
                    rankSwellPeriodHigh);
        }
    }

    public static boolean isRankSwellPeriodHigh() {
        return rankSwellPeriodHigh;
    }
}
