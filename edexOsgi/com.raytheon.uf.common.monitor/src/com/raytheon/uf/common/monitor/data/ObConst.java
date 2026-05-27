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

/**
 * Constants for Observations within the Decision Assistance Tool Suite that
 * hark back to the SAFESEAS.H C++ header file.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 10, 2009  1999     grichard  Initial creation.
 * Jan 26, 2010  4268     skorolev  Corrected DisplayVarName according to
 *                                  VarName for SWELL
 * Nov 06, 2013  2493     bsteffen  Remove unused constants.
 * Sep 18, 2015  3873     skorolev  Added constants for Maritime data
 * May 21, 2019  7689     randerso  Code cleanup.
 *
 * </pre>
 *
 * @author grichard
 */

public final class ObConst {

    // Private constructor -- all contents must be public static
    private ObConst() {
    }

    /**
     * The maximum number of most recent hours within which observation reports
     * are to be archived.
     */
    public static final int MAX_FRAMES = 64;

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

    public static final String SYNOPTIC_SHIP = "1003";

    public static final String SYNOPTIC_CMAN = "1004";

    public static final String SYNOPTIC_MOORED_BUOY = "1005";

    public static final String DRIFTING_BUOY = "1006";

    public static final String SYNOPTIC_MAROB = "1007";

    public static final String METAR = "METAR";

    public static final String SPECI = "SPECI";

    public static final String MARITIME = "MARITIME";

    public static final String MESONET = "MESONET";

    // Chosen Application Key
    public enum ChosenAppKey {
        SNOW,
        FOG,
        SAFESEAS
    }

    // Usage of threshold data
    public enum DataUsageKey {
        MONITOR,
        DISPLAY
    }

    // Threat Level
    public enum ThreatLevel {
        BLACK,
        GRAY,
        GREEN,
        YELLOW,
        RED,
        YELLOW_LO,
        YELLOW_HI,
        RED_LO,
        RED_HI,
        UNDEFINED_THREATLEVEL
    }

    // Report Type
    public enum ReportType {
        SYNOPTIC_FIXED_LAND,

        SYNOPTIC_MOBILE_LAND,

        SYNOPTIC_SHIP,

        SYNOPTIC_CMAN,

        SYNOPTIC_MOORED_BUOY,

        DRIFTING_BUOY,

        SYNOPTIC_MAROB,

        MARITIME,

        METAR,

        SPECI,

        MESONET,

        UNSPECIFIED
    }

    // Product Name. Note: SCA is small craft advisory;
    // HFWW is hurricane force wind warning.
    public enum ProductName {
        SCA,
        GALE_WARNING,
        STORM_WARNING,
        HFWW,
        BLIZZARD_WARNING,
        ICE_WARNING,
        HEAVY_SNOW_WARNING,
        UNDEFINED_PRODUCT
    }

    // Variable names within Report
    public enum VarName {
        VISIBILITY,
        WIND_SPEED,
        MAX_WIND_SPEED,
        GUST_SPEED,
        TEMPERATURE,
        WIND_CHILL,
        SNOW_DEPTH,
        PRIM_SWELL_HT,
        PRIM_SWELL_PD,
        PRIM_SWELL_DIR,
        SEC_SWELL_HT,
        SEC_SWELL_PD,
        SEC_SWELL_DIR,
        STATIONARY,
        WAVE_HEIGHT,
        WAVE_PERIOD,
        WAVE_STEEPNESS,
        LATITUDE,
        LONGITUDE,
        NOMINAL_DATETIME,
        OBSERVATION_DATETIME,
        PLATFORM_ID,
        PRESSURE,
        PRES_CHANGE,
        WIND_DIR,
        FOG,
        ZONE_ID,
        DEWPOINT,
        PRES_WX,
        SEA_SURFACE_TEMPERATURE,
        HOURLY_PRECIP,
        SNINCR_HOURLY,
        SNINCR_TOTAL,
        FROSTBITE_TIME,
        RELATIVE_HUMIDITY,
        CEILING,
        DEWPOINT_DEPR,
        SEA_LEVEL_PRESS,
        UNDEFINED_VARIABLE
    }

    // Display Variable Names
    public enum DisplayVarName {
        SCA_WIND_SPEED(ProductName.SCA, "Wind Speed", "kts"),
        SCA_GUST_SPEED(ProductName.SCA, "Wind Gust", "kts"),
        SCA_MAX_WIND_SPEED(ProductName.SCA, "Peak Wind", "kts"),
        SCA_WAVE_HEIGHT(ProductName.SCA, "Wave Height", "ft"),
        GALE_WIND_SPEED(ProductName.GALE_WARNING, "Wind Speed", "kts"),
        GALE_GUST_SPEED(ProductName.GALE_WARNING, "Wind Gust", "kts"),
        GALE_MAX_WIND_SPEED(ProductName.GALE_WARNING, "Peak Wind", "kts"),
        STORM_WIND_SPEED(ProductName.STORM_WARNING, "Wind Speed", "kts"),
        STORM_GUST_SPEED(ProductName.STORM_WARNING, "Wind Gust", "kts"),
        STORM_MAX_WIND_SPEED(ProductName.STORM_WARNING, "Peak Wind", "kts"),
        HURRICANE_WIND_SPEED(ProductName.HFWW, "Wind Speed", "kts"),
        HURRICANE_GUST_SPEED(ProductName.HFWW, "Wind Gust", "kts"),
        HURRICANE_MAX_WIND_SPEED(ProductName.HFWW, "Peak Wind", "kts"),
        VAR_WIND_DIR(ProductName.UNDEFINED_PRODUCT, "Wind Direc", "deg"),
        VAR_WIND_SPEED(ProductName.UNDEFINED_PRODUCT, "Wind Speed", "kts"),
        VAR_MAX_WIND_SPEED(ProductName.UNDEFINED_PRODUCT, "Peak Wind", "kts"),
        VAR_GUST_SPEED(ProductName.UNDEFINED_PRODUCT, "Wind Gust", "kts"),
        VAR_VISIBILITY(ProductName.UNDEFINED_PRODUCT, "Visibility", "nm"),
        VAR_TEMPERATURE(ProductName.UNDEFINED_PRODUCT, "Temperature", "F"),
        VAR_DEWPOINT(ProductName.UNDEFINED_PRODUCT, "Dewpoint", "F"),
        VAR_SEA_SURFACE_TEMPERATURE(
                ProductName.UNDEFINED_PRODUCT,
                "Sea Sfc Temp",
                "F"),
        VAR_WAVE_HEIGHT(ProductName.UNDEFINED_PRODUCT, "Wave Height", "ft"),
        VAR_WAVE_STEEPNESS(
                ProductName.UNDEFINED_PRODUCT,
                "Wave Steep",
                "x1000"),
        VAR_TIME(ProductName.UNDEFINED_PRODUCT, "UTC", ""),
        VAR_LATITUDE(ProductName.UNDEFINED_PRODUCT, "Lat", "deg"),
        VAR_LONGITUDE(ProductName.UNDEFINED_PRODUCT, "Lon", "deg"),
        VAR_PRESSURE(ProductName.UNDEFINED_PRODUCT, "Pressure", "mb"),
        VAR_PRES_CHANGE(ProductName.UNDEFINED_PRODUCT, "Pressure Change", "mb"),
        VAR_SNOW_DEPTH(ProductName.UNDEFINED_PRODUCT, "Snow Depth", "in"),
        VAR_SNINCR_HOURLY(ProductName.UNDEFINED_PRODUCT, "SNINCR Hourly", "in"),
        VAR_SNINCR_TOTAL(ProductName.UNDEFINED_PRODUCT, "SNINCR Total", "in"),
        VAR_WIND_CHILL(ProductName.UNDEFINED_PRODUCT, "Wind Chill", "F"),
        VAR_FROSTBITE_TIME(
                ProductName.UNDEFINED_PRODUCT,
                "Frostbite Time",
                "min"),
        VAR_HOURLY_PRECIP(ProductName.UNDEFINED_PRODUCT, "Hourly Precip", "in"),
        VAR_PRIM_SWELL_HT(ProductName.UNDEFINED_PRODUCT, "Prim Swell Ht", "ft"),
        VAR_PRIM_SWELL_PD(
                ProductName.UNDEFINED_PRODUCT,
                "Prim Swell Pd",
                "sec"),
        VAR_PRIM_SWELL_DIR(
                ProductName.UNDEFINED_PRODUCT,
                "Prim Swell Dir",
                "deg"),
        VAR_SEC_SWELL_HT(ProductName.UNDEFINED_PRODUCT, "Sec Swell Ht", "ft"),
        VAR_SEC_SWELL_PD(ProductName.UNDEFINED_PRODUCT, "Sec Swell Pd", "sec"),
        VAR_SEC_SWELL_DIR(
                ProductName.UNDEFINED_PRODUCT,
                "Sec Swell Dir",
                "deg"),
        VAR_FOG(ProductName.UNDEFINED_PRODUCT, "Fog", ""),
        BLIZ_WIND_SPEED(ProductName.BLIZZARD_WARNING, "Wind Speed", "kts"),
        BLIZ_GUST_SPEED(ProductName.BLIZZARD_WARNING, "Wind Gust", "kts"),
        BLIZ_MAX_WIND_SPEED(ProductName.BLIZZARD_WARNING, "Peak Wind", "kts"),
        BLIZ_VISIBILITY(ProductName.BLIZZARD_WARNING, "Visibility", "nm"),
        FRZ_HOURLY_PRECIP(ProductName.ICE_WARNING, "Hourly Precip", "in"),
        FRZ_TEMPERATURE(ProductName.ICE_WARNING, "Temperature", "F"),
        HSW_SNOW_DEPTH(ProductName.HEAVY_SNOW_WARNING, "Snow Depth", "in"),
        HSW_SNINCR_HOURLY(
                ProductName.HEAVY_SNOW_WARNING,
                "SNINCR Hourly",
                "in"),
        HSW_SNINCR_TOTAL(ProductName.HEAVY_SNOW_WARNING, "SNINCR Total", "in"),
        VAR_RELATIVE_HUMIDITY(
                ProductName.UNDEFINED_PRODUCT,
                "Relative Humidity",
                "%"),
        VAR_CEILING(ProductName.UNDEFINED_PRODUCT, "Ceiling", "ftx100"),
        VAR_DEWPOINT_DEPR(ProductName.UNDEFINED_PRODUCT, "T-Td", "F"),
        VAR_SEA_LEVEL_PRESS(
                ProductName.UNDEFINED_PRODUCT,
                "Sea Level Pressure",
                "mb");

        private ProductName productName;

        private String displayName;

        private String units;

        private DisplayVarName(ProductName productName, String displayName,
                String units) {
            this.productName = productName;
            this.displayName = displayName;
            this.units = units;
        }

        /**
         * @return the productName
         */
        public ProductName getProductName() {
            return productName;
        }

        /**
         * @return the displayName
         */
        public String getDisplayName() {
            return displayName;
        }

        /**
         * @return the units
         */
        public String getUnits() {
            return units;
        }
    }

    // Default station identifier
    public static final String DEFAULT_STATION_NAME = "KOMA";

}
