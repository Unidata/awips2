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

import java.text.SimpleDateFormat;
import java.util.TimeZone;

/**
 * Constants for Observations within the Decision Assistance Tool Suite that
 * hark back to the SAFESEAS.H C++ header file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2009 1999       grichard    Initial creation.
 * Jan 26, 2010 4268       skorolev    Corrected DisplayVarName according to VarName for SWELL
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public final class ObConst {

    // Private constructor -- all contents must be public static
    private ObConst() {
    }

    // Date Format
    public static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm");

    public static SimpleDateFormat obTimeFormat = DATE_FORMAT;

    static {
        obTimeFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    // Time Window in units of hours
    public static final int THREAT_INTERVAL_HOURS = 2;

    // Missing value for ObReport class attributes
    public static final float MISSING = -9999.0f;

    // The constant representing the sky condition for sky clear
    public static final int SKC_SKY_CONDITION = 9999999;

    // The constant representing the sky condition for clear sky
    public static final int CLR_SKY_CONDITION = 8888888;

    /**
     * Somehow ceiling = 88888.88f is used to represent Clear_Sky_condition in
     * obs report The following three constants are created for the use of
     * representing "clear sky condition" in Zone/Station/History tables [DR
     * #5576, 05/27/2010, zhao]
     */
    public static final float CLR_SKY_FLOAT = 8888888.0f;

    public static final int CLR_SKY_INTEGER = 88888;

    public static final String CLR_SKY_STRING = "CLR";

    public static final float SKC_SKY_FLOAT = 9999999.0f;

    public static final int SKC_SKY_INTEGER = 99999;

    public static final String SKC_SKY_STRING = "SKC";

    // Image type used for Fog Monitor
    public enum ImageType {
        VIS, IR3_9, IR10_7, Undefined_ImageType
    }

    // Intensity Level
    public enum IntensityLevel {
        LIGHT, MODERATE, HEAVY
    };

    // Monitor Area
    // public enum MonitorArea {
    // SS, FM, SS_FM
    // }

    // Chosen Application Key
    public enum ChosenAppKey {
        SNOW, FOG, SAFESEAS
    }

    // Usage of threshold data
    public enum DataUsageKey {
        MONITOR, DISPLAY
    }

    // Threat Level
    public enum ThreatLevel {
        BLACK, GRAY, GREEN, YELLOW, RED, YELLOW_LO, YELLOW_HI, RED_LO, RED_HI, UNDEFINED_THREATLEVEL
    }

    // Threat Level Strings
    public static final String[] THREAT_LEVEL_STRINGS = { "black", "gray",
            "green", "yellow", "red", "yellow_lo", "yellow_hi", "red_lo",
            "red_hi" };

    // Report Type
    public enum ReportType {
        SYNOPTIC_FIXED_LAND,

        SYNOPTIC_MOBILE_LAND,

        SYNOPTIC_SHIP,

        SYNOPTIC_CMAN,

        SYNOPTIC_MOORED_BUOY,

        DRIFTING_BUOY,

        SYNOPTIC_MAROB,

        MARINE,

        METAR,

        SPECI,

        MESONET,

        UNSPECIFIED
    }

    // Product Name. Note: SCA is small craft advisory;
    // HFWW is hurricane force wind warning.
    public enum ProductName {
        SCA, GALE_WARNING, STORM_WARNING, HFWW, BLIZZARD_WARNING, ICE_WARNING, HEAVY_SNOW_WARNING, UNDEFINED_PRODUCT
    }

    // Variable names within Report
    public enum VarName {
        VISIBILITY, WIND_SPEED, MAX_WIND_SPEED, GUST_SPEED, TEMPERATURE, WIND_CHILL, SNOW_DEPTH, PRIM_SWELL_HT, PRIM_SWELL_PD, PRIM_SWELL_DIR, SEC_SWELL_HT, SEC_SWELL_PD, SEC_SWELL_DIR, STATIONARY, WAVE_HEIGHT, WAVE_PERIOD, WAVE_STEEPNESS, LATITUDE, LONGITUDE, NOMINAL_DATETIME, OBSERVATION_DATETIME, PLATFORM_ID, PRESSURE, PRES_CHANGE, WIND_DIR, FOG, ZONE_ID, DEWPOINT, PRES_WX, SEA_SURFACE_TEMPERATURE, HOURLY_PRECIP, SNINCR_HOURLY, SNINCR_TOTAL, FROSTBITE_TIME, RELATIVE_HUMIDITY, CEILING, DEWPOINT_DEPR, SEA_LEVEL_PRESS, UNDEFINED_VARIABLE
    }

    // Variable Name Strings
    public static final String[] VAR_NAME_TEXT = { "GUST_SPEED", "LATITUDE",
            "LONGITUDE", "NOMINAL_DATETIME", "OBSERVATION_DATETIME",
            "PLATFORM_ID", "PRESSURE", "PRES_CHANGE", "PRIM_SWELL_DIR",
            "PRIM_SWELL_HT", "PRIM_SWELL_PD", "SEC_SWELL_DIR", "SEC_SWELL_HT",
            "SEC_SWELL_PD", "STATIONARY", "TEMPERATURE", "VISIBILITY",
            "WAVE_HEIGHT", "WAVE_PERIOD", "WAVE_STEEPNESS", "WIND_DIR",
            "WIND_SPEED", "MAX_WIND_SPEED", "FOG", "ZONE_ID", "DEWPOINT",
            "PRES_WX", "SEA_SURFACE_TEMPERATURE", "HOURLY_PRECIP",
            "SNOW_DEPTH", "SNINCR_HOURLY", "SNINCR_TOTAL", "WIND_CHILL",
            "FROSTBITE_TIME", "RELATIVE_HUMIDITY", "CEILING", "DEWPOINT_DEPR",
            "SEA_LEVEL_PRESS", "UNDEFINED_VARIABLE" };

    // Display Variable Names
    public enum DisplayVarName {
        SCA_WIND_SPEED, SCA_GUST_SPEED, SCA_MAX_WIND_SPEED, SCA_WAVE_HEIGHT, GALE_WIND_SPEED, GALE_GUST_SPEED, GALE_MAX_WIND_SPEED, STORM_WIND_SPEED, STORM_GUST_SPEED, STORM_MAX_WIND_SPEED, HURRICANE_WIND_SPEED, HURRICANE_GUST_SPEED, HURRICANE_MAX_WIND_SPEED, VAR_WIND_DIR, VAR_WIND_SPEED, VAR_MAX_WIND_SPEED, VAR_GUST_SPEED, VAR_VISIBILITY, VAR_TEMPERATURE, VAR_DEWPOINT, VAR_SEA_SURFACE_TEMPERATURE, VAR_WAVE_HEIGHT, VAR_WAVE_STEEPNESS, VAR_TIME, VAR_LATITUDE, VAR_LONGITUDE, VAR_PRESSURE, VAR_PRES_CHANGE, VAR_SNOW_DEPTH, VAR_SNINCR_HOURLY, VAR_SNINCR_TOTAL, VAR_WIND_CHILL, VAR_FROSTBITE_TIME, VAR_HOURLY_PRECIP, VAR_PRIM_SWELL_HT, VAR_PRIM_SWELL_PD, VAR_PRIM_SWELL_DIR, VAR_SEC_SWELL_HT, VAR_SEC_SWELL_PD, VAR_SEC_SWELL_DIR, VAR_FOG, BLIZ_WIND_SPEED, BLIZ_GUST_SPEED, BLIZ_MAX_WIND_SPEED, BLIZ_VISIBILITY, FRZ_HOURLY_PRECIP, FRZ_TEMPERATURE, HSW_SNOW_DEPTH, HSW_SNINCR_HOURLY, HSW_SNINCR_TOTAL, VAR_RELATIVE_HUMIDITY, VAR_CEILING, VAR_DEWPOINT_DEPR, VAR_SEA_LEVEL_PRESS
    }

    // Display Variable Name Strings
    public static final String[] DISPLAY_VAR_NAME_TEXT = { "Wind Speed",
            "Wind Gust", "Peak Wind", "Wave Height", "Wind Speed", "Wind Gust",
            "Peak Wind", "Wind Speed", "Wind Gust", "Peak Wind", "Wind Speed",
            "Wind Gust", "Peak Wind", "Wind Direc", "Wind Speed", "Peak Wind",
            "Wind Gust", "Visibility", "Temperature", "Dewpoint",
            "Sea Sfc Temp", "Wave Height", "Wave Steep", "UTC", "Lat", "Lon",
            "Pressure", "Pressure Change", "Snow Depth", "SNINCR Hourly",
            "SNINCR Total", "Wind Chill", "Frostbite Time", "Hourly Precip",
            "Prim Swell Ht", "Prim Swell Pd", "Prim Swell Dir", "Sec Swell Ht",
            "Sec Swell Pd", "Sec Swell Dir", "Fog", "Wind Speed", "Wind Gust",
            "Peak Wind", "Visibility", "Hourly Precip", "Temperature",
            "Snow Depth", "SNINCR Hourly", "SNINCR Total", "Relative Humidity",
            "Ceiling", "T-Td", "Sea Level Pressure" };

    // Display Variable Unit Strings
    public static final String[] DISPLAY_VAR_UNITS_TEXT = { "kts", "kts",
            "kts", "ft", "kts", "kts", "kts", "kts", "kts", "kts", "kts",
            "kts", "kts", "deg", "kts", "kts", "kts", "nm", "F", "F", "F",
            "ft", "x1000", "", "deg", "deg", "mb", "mb", "in", "in", "in", "F",
            "min", "in", "ft", "sec", "deg", "ft", "sec", "deg", "", "kts",
            "kts", "kts", "nm", "in", "F", "in", "in", "in", "%", "ftx100",
            "F", "mb" };

    // Remarks section delimiter in METAR
    public static final String REMARK_EXPR = "RMK";

    // Snow increasing rapidly remark expression in a METAR's remarks section
    public static final String SNINCR_EXPR = "(SNINCR )(\\d+)/(\\d+)";

    // Snow depth remark expression in a METAR's remarks section
    public static final String SNOW_DEPTH_EXPR = "4/\\(d{3})";

    // Metar plugin name
    public static final String METAR_PLUGIN_NAME = "obs";

    // Marine plugin name
    public static final String MARINE_PLUGIN_NAME = "scfobs";

    // Meso plugin name
    public static final String MESO_PLUGIN_NAME = "mesowest";

    // Default station identifier
    public static final String DEFAULT_STATION_NAME = "KOMA";

}
