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
package com.raytheon.uf.viz.monitor.util;

import java.util.HashMap;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2009 2076       avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */
public class MonitorConfigConstants
{   
    private static HashMap<String, Boolean> ryHigherMapSS;
    private static HashMap<String, Boolean> ryHigherMapSnow;
    private static HashMap<String, Boolean> ryHigherMapFog;

    private static boolean rankSwellPeriodHigh = false;
    /*
     *  SAFESEAS
     */    
    public static enum SafeSeasDisplay
    {        
        SS_DISP_PROD_SCA_WIND_SPEED("Display.Product.SCA.wind_speed"),
        SS_DISP_PROD_SCA_GUST_SPEED("Display.Product.SCA.gust_speed"),
        SS_DISP_PROD_SCA_PEAK_WIND("Display.Product.SCA.peak_wind_speed"),
        SS_DISP_PROD_SCA_WAVE_HT("Display.Product.SCA.wave_height"),
        SS_DISP_PROD_GALE_WIND_SPEED("Display.Product.Gale.wind_speed"),
        SS_DISP_PROD_GALE_GUST_SPEED("Display.Product.Gale.gust_speed"),
        SS_DISP_PROD_GALE_PEAK_WIND("Display.Product.Gale.peak_wind_speed"),
        SS_DISP_PROD_STORM_WIND_SPEED("Display.Product.Storm.wind_speed"),
        SS_DISP_PROD_STORM_GUST_SPEED("Display.Product.Storm.gust_speed"),
        SS_DISP_PROD_STORM_PEAK_WIND("Display.Product.Storm.peak_wind_speed"),
        SS_DISP_PROD_HFWW_WIND_SPEED("Display.Product.Hurricane.wind_speed"),
        SS_DISP_PROD_HFWW_GUST_SPEED("Display.Product.Hurricane.gust_speed"),
        SS_DISP_PROD_HFWW_PEAK_WIND("Display.Product.Hurricane.peak_wind_speed"),
        SS_DISP_WIND_WIND_SPEED("Display.Wind.wind_speed"),
        SS_DISP_WIND_PEAK_WIND("Display.Wind.peak_wind_speed"),
        SS_DISP_WIND_GUST_SPEED("Display.Wind.gust_speed"),
        SS_DISP_WIND_DIR_FROM("Display.Wind.dirFrom"),
        SS_DISP_WIND_DIR_TO("Display.Wind.dirTo"),
        SS_DISP_METEO_VIS("Display.Meteo.visibility"),
        SS_DISP_METEO_TEMP("Display.Meteo.temperature"),
        SS_DISP_METEO_DEWPT("Display.Meteo.dewpoint"),
        SS_DISP_METEO_SLP("Display.Meteo.slp"),
        SS_DISP_METEO_SST("Display.Meteo.sea_surface_temp"),
        SS_DISP_METEO_WAVE_HT("Display.Meteo.wave_height"),
        SS_DISP_METEO_WAVE_STEEP("Display.Meteo.wave_steepness"),
        SS_DISP_SWELL_PRIM_HT("Display.Swell.Primary.height"),
        SS_DISP_SWELL_PRIM_PD("Display.Swell.Primary.period"),
        SS_DISP_SWELL_PRIM_DIR_FROM("Display.Swell.Primary.dirFrom"),
        SS_DISP_SWELL_PRIM_DIR_TO("Display.Swell.Primary.dirTo"),
        SS_DISP_SWELL_SEC_HT("Display.Swell.Secondary.height"),
        SS_DISP_SWELL_SEC_PD("Display.Swell.Secondary.period"),
        SS_DISP_SWELL_SEC_DIR_FROM("Display.Swell.Secondary.dirFrom"),
        SS_DISP_SWELL_SEC_DIR_TO("Display.Swell.Secondary.dirTo");

        String XMLKey;

        SafeSeasDisplay(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }
    
    public static enum SafeSeasMonitor
    {
        SS_MON_METEO_WIND_SPEED("Monitor.Meteo.wind_speed"),
        SS_MON_METEO_PEAK_WIND("Monitor.Meteo.peak_wind_speed"),
        SS_MON_METEO_GUST_SPEED("Monitor.Meteo.gust_speed"),
        SS_MON_METEO_WAVE_HT("Monitor.Meteo.wave_height"),
        SS_MON_METEO_VIS("Monitor.Meteo.visibility"),
        SS_MON_SWELL_PRIM_HT("Monitor.Swell.Primary.height"),
        SS_MON_SWELL_PRIM_PD("Monitor.Swell.Primary.period"),
        SS_MON_SWELL_PRIM_DIR_FROM("Monitor.Swell.Primary.dirFrom"),
        SS_MON_SWELL_PRIM_DIR_TO("Monitor.Swell.Primary.dirTo"),
        SS_MON_SWELL_SEC_HT("Monitor.Swell.Secondary.height"),
        SS_MON_SWELL_SEC_PD("Monitor.Swell.Secondary.period"),
        SS_MON_SWELL_SEC_DIR_FROM("Monitor.Swell.Secondary.dirFrom"),
        SS_MON_SWELL_SEC_DIR_TO("Monitor.Swell.Secondary.dirTo");

        String XMLKey;

        SafeSeasMonitor(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }

    /*
     *  SNOW
     */
    public static enum SnowDisplay
    {
        SNOW_DISP_PROD_BLIZZ_VIS("Display.Product.Blizzard.visibility"),
        SNOW_DISP_PROD_BLIZZ_WIND_SPEED("Display.Product.Blizzard.wind_speed"),
        SNOW_DISP_PROD_BLIZZ_GUST_SPEED("Display.Product.Blizzard.gust_speed"),
        SNOW_DISP_PROD_BLIZZ_PEAK_WIND("Display.Product.Blizzard.peak_wind_speed"),
        SNOW_DISP_PROD_FRZ_TEMP("Display.Product.Freezing_Precip.temperature"),
        SNOW_DISP_PROD_FRZ_HOURLY_PRECIP("Display.Product.Freezing_Precip.hourly_precip"),
        SNOW_DISP_PROD_HSW_SNINCR_HOURLY("Display.Product.Heavy_Snow_Warning.snincr_hourly"),
        SNOW_DISP_PROD_HSW_SNINCR_TOTAL("Display.Product.Heavy_Snow_Warning.snincr_total"),
        SNOW_DISP_PROD_HSW_SNOW_DEPTH("Display.Product.Heavy_Snow_Warning.snow_depth"),
        SNOW_DISP_WIND_WIND_SPEED("Display.Product.Wind.wind_speed"),
        SNOW_DISP_WIND_PEAK_WIND("Display.Product.Wind.peak_wind_speed"),
        SNOW_DISP_WIND_GUST_SPEED("Display.Product.Wind.gust_speed"),
        SNOW_DISP_WIND_DIR_FROM("Display.Product.Wind.dirFrom"),
        SNOW_DISP_WIND_DIR_TO("Display.Product.Wind.dirTo"),
        SNOW_DISP_METEO_TEMP("Display.Meteo.temperature"), 
        SNOW_DISP_METEO_DEWPT("Display.Meteo.dewpoint"),
        SNOW_DISP_METEO_VIS("Display.Meteo.visibility"),
        SNOW_DISP_METEO_SLP("Display.Meteo.slp"),
        SNOW_DISP_METEO_HOURLY_PRECIP("Display.Meteo.hourly_precip"),
        SNOW_DISP_METEO_WIND_CHILL("Display.Meteo.wind_chill"),
        SNOW_DISP_METEO_FROSTBITE("Display.Meteo.frostbite_time"),
        SNOW_DISP_METEO_SNOW_DEPTH("Display.Meteo.snow_depth"),
        SNOW_DISP_METEO_SNINCR_HOURLY("Display.Meteo.snincr_hourly"),
        SNOW_DISP_METEO_SNINCR_TOTAL("Display.Meteo.snincr_total");

        String XMLKey;

        SnowDisplay(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }
    
    public static enum SnowMonitor
    {
        SNOW_MON_METEO_WIND_SPEED("Monitor.Meteo.wind_speed"),
        SNOW_MON_METEO_PEAK_WIND("Monitor.Meteo.peak_wind_speed"),
        SNOW_MON_METEO_GUST_SPEED("Monitor.Meteo.gust_speed"),
        SNOW_MON_METEO_TEMP("Monitor.Meteo.temperature"),
        SNOW_MON_METEO_WIND_CHILL("Monitor.Meteo.wind_chill"),
        SNOW_MON_METEO_VIS("Monitor.Meteo.visibility"),
        SNOW_MON_METEO_SNOW_DEPTH("Monitor.Meteo.snow_depth");

        String XMLKey;

        SnowMonitor(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }

    // FOG
    public static enum FogDisplay
    {
        FOG_DISP_METEO_VIS("Display.Meteo.visibility"),
        FOG_DISP_METEO_CEILING("Display.Meteo.ceiling"),
        FOG_DISP_METEO_TEMP("Display.Meteo.temperature"),
        FOG_DISP_METEO_DEWPT("Display.Meteo.dewpoint"),
        FOG_DISP_METEO_T_TD("Display.Meteo.t_td"),
        FOG_DISP_METEO_REL_HUMIDITY("Display.Meteo.relative_humidity"),
        FOG_DISP_WIND_WIND_SPEED("Display.Wind.wind_speed"),
        FOG_DISP_WIND_PEAK_WIND("Display.Wind.peak_wind_speed"),
        FOG_DISP_WIND_GUST_SPEED("Display.Wind.gust_speed"),
        FOG_DISP_WIND_DIR_FROM("Display.Wind.dirFrom"),
        FOG_DISP_WIND_DIR_TO("Display.Wind.dirTo");

        String XMLKey;

        FogDisplay(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }
    
    public static enum FogMonitor
    {
        FOG_MONITOR_METEO_VIS("Monitor.Meteo.visiblity");
        
        String XMLKey;

        FogMonitor(String s) {
            XMLKey = s;
        }

        public String getXmlKey() {
            return XMLKey;
        }
    }
    
    public static Boolean rValueIsHigher(String key, String appName)
    {
    	if ( appName.equals("safeseas") ) {
    		if ( ryHigherMapSS == null ) {
    			createRYHigherMapSS();
    		}
        
    		if ( ryHigherMapSS.containsKey(key) == true ) {
    			return ryHigherMapSS.get(key);
    		}
    	} else if ( appName.equals("snow") ) {
    		if ( ryHigherMapSnow == null ) {
    			createRYHigherMapSnow();
    		}
        
    		if ( ryHigherMapSnow.containsKey(key) == true ) {
    			return ryHigherMapSnow.get(key);
    		}
    	} else if ( appName.equals("fog") ) {
    		if ( ryHigherMapFog == null ) {
    			createRYHigherMapFog();
    		}
        
    		if ( ryHigherMapFog.containsKey(key) == true ) {
    			return ryHigherMapFog.get(key);
    		}
    	} else {
    		// do nothing here
    	}
        
        return false;
    }
    
    private static void createRYHigherMapSS()
    {
        ryHigherMapSS = new HashMap<String, Boolean>();
        
        /*
         * SafeSeas Display
         */
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_SCA_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_SCA_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_SCA_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_SCA_WAVE_HT.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_GALE_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_GALE_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_GALE_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_STORM_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_STORM_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_STORM_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_HFWW_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_HFWW_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_PROD_HFWW_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_WIND_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_WIND_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_WIND_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_WIND_DIR_FROM.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_WIND_DIR_TO.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_VIS.getXmlKey(), false);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_TEMP.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_DEWPT.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_SLP.getXmlKey(), false);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_SST.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_WAVE_HT.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_METEO_WAVE_STEEP.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD.getXmlKey(), rankSwellPeriodHigh);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_FROM.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_TO.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_SEC_HT.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_SEC_PD.getXmlKey(), rankSwellPeriodHigh);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_TO.getXmlKey(), null);
        
        /*
         * SafeSeas Monitor // TODO
         */
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_METEO_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_METEO_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_METEO_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_METEO_WAVE_HT.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_METEO_VIS.getXmlKey(), false);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_PRIM_HT.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_PRIM_PD.getXmlKey(), false);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_PRIM_DIR_FROM.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_PRIM_DIR_TO.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_SEC_HT.getXmlKey(), true);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_SEC_PD.getXmlKey(), false);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_SEC_DIR_FROM.getXmlKey(), null);
        ryHigherMapSS.put(SafeSeasMonitor.SS_MON_SWELL_SEC_DIR_TO.getXmlKey(), null);
    }
    
    private static void createRYHigherMapSnow()
    {
        ryHigherMapSnow = new HashMap<String, Boolean>();        
        
        /*
         * Snow Display
         */
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_BLIZZ_VIS.getXmlKey(), false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_BLIZZ_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_BLIZZ_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_BLIZZ_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_FRZ_TEMP.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_FRZ_HOURLY_PRECIP.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_HOURLY.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_TOTAL.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_PROD_HSW_SNOW_DEPTH.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_WIND_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_WIND_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_WIND_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_WIND_DIR_FROM.getXmlKey(), null);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_WIND_DIR_TO.getXmlKey(), null);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_TEMP.getXmlKey(), false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_DEWPT.getXmlKey(), false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_VIS.getXmlKey(), false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_SLP.getXmlKey(), false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_WIND_CHILL.getXmlKey(), false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_FROSTBITE.getXmlKey(), false);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_SNOW_DEPTH.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_SNINCR_HOURLY.getXmlKey(), true);
        ryHigherMapSnow.put(SnowDisplay.SNOW_DISP_METEO_SNINCR_TOTAL.getXmlKey(), true);
        
        /*
         * Snow Monitor
         */
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_WIND_SPEED.getXmlKey(), true);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_PEAK_WIND.getXmlKey(), true);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_GUST_SPEED.getXmlKey(), true);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_TEMP.getXmlKey(), false);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_WIND_CHILL.getXmlKey(), false);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_VIS.getXmlKey(), false);
        ryHigherMapSnow.put(SnowMonitor.SNOW_MON_METEO_SNOW_DEPTH.getXmlKey(), true);
    }
        
    private static void createRYHigherMapFog()
    {
        ryHigherMapFog = new HashMap<String, Boolean>();        
        /*
         * Fog Display
         */
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_VIS.getXmlKey(), false);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_CEILING.getXmlKey(), false);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_TEMP.getXmlKey(), true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_DEWPT.getXmlKey(), true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_T_TD.getXmlKey(), false);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_METEO_REL_HUMIDITY.getXmlKey(), true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_WIND_WIND_SPEED.getXmlKey(), true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_WIND_PEAK_WIND.getXmlKey(), true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_WIND_GUST_SPEED.getXmlKey(), true);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_WIND_DIR_FROM.getXmlKey(), null);
        ryHigherMapFog.put(FogDisplay.FOG_DISP_WIND_DIR_TO.getXmlKey(), null);
        
        /*
         * Fog Monitor
         */
        ryHigherMapFog.put(FogMonitor.FOG_MONITOR_METEO_VIS.getXmlKey(), false);
    }

    // FOG Parameters
//    public static final String FOG_IR_MIN = "fog.ir.min";
//    public static final String FOG_IR_DELTA_YELLOW_LOW = "fog.ir.delta.yellow.low";
//    public static final String FOG_IR_DELTA_YELLOW_HIGH = "fog.ir.delta.yellow.high";
//    public static final String FOG_IR_DELTA_RED_LOW = "fog.ir.delta.red.low";
//    public static final String FOG_IR_DELTA_RED_HIGH = "fog.ir.delta.red.high";
//    public static final String FOG_IR_MAX = "fog.ir.max";
//    public static final String FOG_IR_ICE_SNOW = "fog.ir.ice_snow_threshold";
//    public static final String FOG_IR_CLOUD = "fog.ir.cloud_threshold";
//    public static final String FOG_IR_CLOUD_FREE = "fog.ir.cloud_free_threshold";
//    public static final String FOG_VIS_MIN = "fog.vis.min";
//    public static final String FOG_VIS_YELLOW_LOW = "fog.vis.yellow.low";
//    public static final String FOG_VIS_YELLOW_HIGH = "fog.vis.yellow.high";
//    public static final String FOG_VIS_RED_LOW = "fog.vis.red.low";
//    public static final String FOG_VIS_RED_HIGH = "fog.vis.red.high";
//    public static final String FOG_VIS_MAX = "fog.vis.max";
//    public static final String FOG_VIS_SMOOTHNESS = "fog.vis.smoothness_threshold";
//    public static final String FOG_VIS_TWILIGHT_ANG = "fog.vis.twilight_threshold";
//    public static final String FOG_VIS_FRACTAL_DIM = "fog.vis.fractal_dim_threshold";
//    public static final String FOG_SCALE_STATE = "fog.scale_state";
//    public static final String FOG_ICE_SNOW = "fog.ice_snow";
//    public static final String FOG_CLOUD = "fog.cloud";
//    public static final String FOG_CLOUD_FREE = "fog.cloud_free";
//    public static final String FOG_SMOOTHNESS = "fog.smoothness";
//    public static final String FOG_ADJACENCY = "fog.adjacency";
//    public static final String FOG_ADJACENCY_THRESH = "fog.adjacency_threshold";
//    public static final String FOG_TWILIGHT_ANG = "fog.twilight_ang";
//    public static final String FOG_FRACTAL_DIM = "fog.fractal_dim";
//
//    public static enum FogParameters {
//        FOG_IR_MIN(MonitorConfigConstants.FOG_IR_MIN),
//        FOG_IR_DELTA_YELLOW_LOW(MonitorConfigConstants.FOG_IR_DELTA_YELLOW_LOW),
//        FOG_IR_DELTA_YELLOW_HIGH(MonitorConfigConstants.FOG_IR_DELTA_YELLOW_HIGH),
//        FOG_IR_DELTA_RED_LOW(MonitorConfigConstants.FOG_IR_DELTA_RED_LOW),
//        FOG_IR_DELTA_RED_HIGH(MonitorConfigConstants.FOG_IR_DELTA_RED_HIGH),
//        FOG_IR_MAX(MonitorConfigConstants.FOG_IR_MAX),
//        FOG_IR_ICE_SNOW(MonitorConfigConstants.FOG_IR_ICE_SNOW),
//        FOG_IR_CLOUD(MonitorConfigConstants.FOG_IR_CLOUD),
//        FOG_IR_CLOUD_FREE(MonitorConfigConstants.FOG_IR_CLOUD_FREE),
//        FOG_VIS_MIN(MonitorConfigConstants.FOG_VIS_MIN),
//        FOG_VIS_YELLOW_LOW(MonitorConfigConstants.FOG_VIS_YELLOW_LOW),
//        FOG_VIS_YELLOW_HIGH(MonitorConfigConstants.FOG_VIS_YELLOW_HIGH),
//        FOG_VIS_RED_LOW(MonitorConfigConstants.FOG_VIS_RED_LOW),
//        FOG_VIS_RED_HIGH(MonitorConfigConstants.FOG_VIS_RED_HIGH),
//        FOG_VIS_MAX(MonitorConfigConstants.FOG_VIS_MAX),
//        FOG_VIS_SMOOTHNESS(MonitorConfigConstants.FOG_VIS_SMOOTHNESS),
//        FOG_VIS_TWILIGHT_ANG(MonitorConfigConstants.FOG_VIS_TWILIGHT_ANG),
//        FOG_VIS_FRACTAL_DIM(MonitorConfigConstants.FOG_VIS_FRACTAL_DIM),
//        FOG_SCALE_STATE(MonitorConfigConstants.FOG_SCALE_STATE),
//        FOG_ICE_SNOW(MonitorConfigConstants.FOG_ICE_SNOW),
//        FOG_CLOUD(MonitorConfigConstants.FOG_CLOUD),
//        FOG_CLOUD_FREE(MonitorConfigConstants.FOG_CLOUD_FREE),
//        FOG_SMOOTHNESS(MonitorConfigConstants.FOG_SMOOTHNESS),
//        FOG_ADJACENCY(MonitorConfigConstants.FOG_ADJACENCY),
//        FOG_ADJACENCY_THRESH(MonitorConfigConstants.FOG_ADJACENCY_THRESH),
//        FOG_TWILIGHT_ANG(MonitorConfigConstants.FOG_TWILIGHT_ANG),
//        FOG_FRACTAL_DIM(MonitorConfigConstants.FOG_FRACTAL_DIM);
//
//        String XMLKey;
//
//        FogParameters(String s) {
//            XMLKey = s;
//        }
//
//        @Override
//        public String toString() {
//            return XMLKey;
//        }
//    }

    // SCAN
//    public static final String SCAN_PLUGIN_NAME = "pluginName";
//    public static final String SCAN_STATION_NAME = "stationName";
//    public static final String SCAN_CELL_TABLE_ATTRIB = "cellTableAttributes";
//    public static final String SCAN_MESO_TABLE_ATTRIB = "mesoTableAttributes";
//    public static final String SCAN_TVS_TABLE_ATTRIB = "tvsTableAttributes";
//
//    public static final String[] CELL_TABLE = { "cellTable", "time", "radar",
//        "ident", "azm", "rng", "rank", "tvs", "mdaSR", "posh", "poh", "hsize",
//        "vil", "dbz", "dbzHt", "top", "dir", "spd", "azm15", "rng15", "azm30",
//        "rng30", "azm45", "rng45", "azm60", "rng60", "mvtErr", "mvtMn", "lat",
//        "lon", "polh", "svrwx", "hvyPr", "pPos", "cgRate", "vcp", "cape",
//        "sreh", "county" };
//
//    public static final String[] MESO_TABLE = { "mesoTable", "time", "radar",
//       "strmID","ident", "azm", "rng", "mdaSR", "class", "llVr", "llgtg",
//       "base", "depth", "relDep", "maxVr", "htMxVr", "tvs", "dir", "spd",
//       "msi","lat", "lon", "county" };
//
//    public static final String[] TVS_TABLE = { "tvsTable", "time", "radar",
//      "strmID", "ident", "type", "azm", "rng", "avgDv", "llDv", "maxDv",
//      "mxDvHt", "base", "depth", "top", "shear", "shrHt", "lat", "lon", "county" };
//
    public static final String[] TABLE_FIELDS = { "low", "mid", "upp", "rank",
        "trend", "mClr", "clucon", "inTbl", "min", "intrvl", "range", "alarm",
        "aAlarm", "units" };
    
    public static final String[] FFMP_TABLE = { "ffmpTable", "name", "rate",
        "qpe", "guidance", "ratio", "diff" };
//
//    public static enum ScanTables {
//        CELL, MESO, TVS;
//    }
//
//    public static enum cellTable {
//        TIME(1), RADAR(2), IDENT(3), AZM(4), RNG(5), RANK(6), TVS(7), MDASR(8),
//        POSH(9), POH(10), HSIZE(11), VIL(12), DBZ(13), DBZHT(14), TOP(15), DIR(16),
//        SPD(17), AZM15(18), RNG15(19), AZM30(20), RNG30(21), AZM45(22), RNG45(23),
//        AZM60(24), RNG60(25), MVTERR(26), MVTMN(27), LAT(28), LON(29), POLH(30),
//        SVRWX(31), HVYPR(32), PPOS(33), CGRATE(34), VCP(35), CAPE(36), SREH(37),
//        COUNTY(38);
//
//        int index;
//
//        cellTable(int i) {
//            index = i;
//        }
//
//        @Override
//        public String toString() {
//            return CELL_TABLE[index];
//        }
//
//        public String toString(tableFields field) {
//            return CELL_TABLE[0] + "." + CELL_TABLE[index] + "." + field.toString();
//        }
//    }
//
//    public static enum mesoTable {
//        TIME(1), RADAR(2), STRMID(3), IDENT(4), AZM(5), RNG(6), MDASR(7), CLASS(8),
//        LLVR(9), LLGTG(10), BASE(11), DEPTH(12), RELDEP(13), MAXVR(14), HTMXVR(15),
//        TVS(16), DIR(18), SPD(19), MSI(20), LAT(21), LON(22), COUNTY(23);
//
//        int index;
//
//        mesoTable(int i) {
//            index = i;
//        }
//
//        @Override
//        public String toString() {
//            return MESO_TABLE[index];
//        }
//
//        public String toString(tableFields t) {
//            return MESO_TABLE[0] + "." + MESO_TABLE[index] + "." + t.toString();
//        }
//    }
//
//    public static enum tvsTable {
//        TIME(1), RADAR(2), STRMID(3), IDENT(4), TYPE(5), AM(6), RNG(7), AVGDV(8),
//        LLDV(9), MAXDV(10), MXDVHT(11), BASE(12), DEPTH(13), TOP(14), SHEAR(15),
//        SHRHT(16), LAT(17), LON(18), COUNTY(19);
//
//        int index;
//
//        tvsTable(int i) {
//            index = i;
//        }
//
//        @Override
//        public String toString() {
//            return TVS_TABLE[index];
//        }
//
//        public String toString(tableFields t) {
//            return TVS_TABLE[0] + "." + TVS_TABLE[index] + "." + t.toString();
//        }
//    }
//
    public static enum tableFields {
        LOW(0), MID(1), UPP(2), RANK(3), TREND(4), MCLR(5), CLUCON(6), INTBL(7), 
        MIN(8), INTRVL(9), RANGE(10), ALARM(11), AALARM(12), UNITS(13);

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
            return FFMP_TABLE[0] + "." + FFMP_TABLE[index] + "." + field.toString();
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
		if ( ryHigherMapSS != null ) {
			ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD.getXmlKey(), rankSwellPeriodHigh);
			ryHigherMapSS.put(SafeSeasDisplay.SS_DISP_SWELL_SEC_PD.getXmlKey(), rankSwellPeriodHigh);
		}
	}

	public static boolean isRankSwellPeriodHigh() {
		return rankSwellPeriodHigh;
	}
}
