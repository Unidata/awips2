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
package com.raytheon.uf.common.dataplugin.shef.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Provides methods to map human readable descriptions to shef parameter codes.
 * <p>
 * This class handles the mapping of the parameter code portion (also known as
 * PEDTSEP) portion of the shef data structure.
 * <p>
 * The parameter code string, when fully specified, contains six keys for
 * database identification. In order to reduce manual entry and communications
 * requirements, standard defaults for each key (except PE) reduce
 * identification of most hydrometeorological data to a minimum key of two
 * characters.
 * <p>
 * To obtain the constant of any given code use the getEnum method. If a code
 * has no associated constant getEnum will return the constant: UNKNOWN
 * <p>
 * To obtain the code for a constant use the getCode method. If a constant has
 * no associated code, getCode will return null. To obtain any value associated
 * with a constant use the getValue method.
 * <p>
 * Portions of the documentation have been taken from the NWS SHEF Manual
 * 10-944, Jan 2, 2008
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2008            jelkins     Initial creation
 * Apr 29, 2014   3088     mpduff      Clean up/optimization.
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class ParameterCode {

    /**
     * Main category of physical element codes.
     */
    public enum PhysicalElementCategory {

        /** Agricultural Data */
        AGRICULTURE("A"),

        /** Unknown Category, possibly agriculture data */
        B("B"),

        /** Unknown Category, possibly agriculture data */
        C("C"),

        EVAPORATION("E"),

        /** Fish count data */
        FISH_COUNT("F"),

        /** Ground frost and ground state */
        GROUND("G"),

        /** Height, depth, and stage */
        HEIGHT("H"),

        ICE("I"),

        LAKE("L"),

        /** Moisture and Fire/Fuel Parameters */
        MOISTURE("M"),

        /** Gate and Dam data */
        DAM("N"),

        /** Pressure and Precipitation */
        PRECIPITATION("P"),

        DISCHARGE("Q"),

        RADIATION("R"),

        SNOW("S"),

        TEMPERATURE("T"),

        WIND("W"),

        /** Generation and generator data */
        GENERATION("V"),

        /** Water Quality */
        WATER("W"),

        /** Weather Codes */
        WEATHER("X"),

        /** Unique, station specific type codes */
        STATION("Y"),

        UNKNOWN;

        private static Map<String, PhysicalElementCategory> map;

        static {
            map = Collections.unmodifiableMap(createMap());
        }

        private String code;

        PhysicalElementCategory() {
        }

        PhysicalElementCategory(String code) {
            this.code = code;
        }

        public String getCode() {
            return code;
        }

        public static PhysicalElementCategory getEnum(String code) {
            PhysicalElementCategory p = map.get(code);
            if (p != null) {
                return p;
            }
            return UNKNOWN;
        }

        private static Map<String, PhysicalElementCategory> createMap() {
            Map<String, PhysicalElementCategory> map = new HashMap<String, PhysicalElementCategory>(
                    PhysicalElementCategory.values().length);
            for (PhysicalElementCategory p : PhysicalElementCategory.values()) {
                map.put(p.getCode(), p);
            }

            return map;
        }
    }

    /**
     * Defines the category and detail of the data.
     * <p>
     * The first character of the code usually defines the basic category of the
     * data, while the second character provides additional detail. Certain
     * characters within the table have special meaning. No physical element
     * codes begin with “D” since they are reserved for date/data-quality
     * elements
     * <P>
     * Some routinely transmitted hydrometeorological data require up to the
     * full seven parameter code characters to identify the data. To facilitate
     * manual entry for these data, several two-character send codes have been
     * developed. The two-character send codes are used only in the transmission
     * of data and represent the seven-character expanded parameter codes for
     * storage in a database. The send codes deal mainly with maxima and minima
     * since the extremum “E” is the sixth character in the parameter code
     * string “PEDTSEP” and would prove somewhat awkward to code routinely.
     * <p>
     * The physical element is always specified by a two-character code. Typical
     * physical elements are discharge (QR), gage height (HG), reservoir forebay
     * elevation (HF), or precipitation increment (PP).
     * <P>
     * The units of each physical element are provided in the accompanying
     * documentation for each enumeration constant in parenthesis.
     */
    public static enum PhysicalElement {

        AGRICULTURAL_RESERVED("AD", null),

        /**
         * Frost Intensity:
         * <dl>
         * <dt>0
         * <dd>No Frost
         * <dt>1
         * <dd>Light frost: surface objects, vegetation, etc., covered with a
         * thin deposit of frost, which may be more or less patchy
         * <dt>2
         * <dd>Moderate frost: surface objects, vegetation, etc., covered with a
         * thicker but patchy deposit of frost
         * <dt>3
         * <dd>Heavy frost: surface objects, vegetation, etc., covered with
         * copious deposit of frost
         * </dl>
         */
        AGRICULTURAL_SURFACE_FROST("AF", null),

        /** percent of green vegetation (%) */
        AGRICULTURAL_GREEN_VEGETATION("AG", null),

        /**
         * Surface Dew Intensity:
         * <dl>
         * <dt>0
         * <dd>None
         * <dt>1
         * <dd>Light dew: dew droplets not connected, no dew under trees or
         * sheltered areas
         * <dt>2
         * <dd>Moderate dew: dew droplets mostly connected but surfaces not
         * saturated and no dripping occurring
         * <dt>3
         * <dd>Heavy dew: nearly saturated surfaces and dripping, some moisture
         * under trees and sheltered areas
         * </dl>
         */
        AGRICULTURAL_SURFACE_DEW("AM", null),

        /** Time below critical temperature, 25 DF or -3.9 DC (HRS and MIN) */
        AGRICULTURAL_TIME_BELOW_25F("AT", null),

        /** Time below critical temperature, 32 DF or 0 DC (HRS and MIN) */
        AGRICULTURAL_TIME_BELOW_32F("AU", null),

        /** Leaf wetness (HRS and MIN) */
        AGRICULTURAL_LEAF_WETNESS("AW", null),

        // TODO Figure out what B means

        /** Solid portion of water equivalent (in, mm) */
        B_WATER_EQUIVALENT_SOLID("BA", null),

        /** (in, mm) */
        B_HEAT_DEFICIT("BB", null),

        /** Liquid water storage (in, mm) */
        B_LIQUID_WATER_STORAGE("BC", null),

        /** (DF, DC) */
        B_TEMPERATURE_INDEX("BD", null),

        /** Maximum water equivalent since snow began to accumulate (in, mm) */
        B_WATER_EQUIVALENT_MAX("BE", null),

        /** Areal water equivalent just prior to the new snowfall (in, mm) */
        B_WATER_EQUIVALENT_PRE_SNOW("BF", null),

        /**
         * Areal extent of snow cover from the areal depletion curve just prior
         * to the new snowfall (%)
         */
        B_SNOW_COVER("BG", null),

        /**
         * Amount of water equivalent above which 100 % areal snow cover
         * temporarily exists (in, mm)
         */
        B_WATER_EQUIVALENT_ABOVE_SNOW_COVER("BH", null),

        /** Excess liquid water in storage (in, mm) */
        B_LIQUID_WATER_STORAGE_EXCESS("BI", null),

        /** Areal extent of snow cover adjustment (in, mm) */
        B_SNOW_COVER_ADJUSTMENT("BJ", null),

        /** Lagged excess liquid water for interval 1 (in, mm) */
        B_LIQUID_WATER_EXCESS_1("BK", null),

        /** Lagged excess liquid water for interval 2 (in, mm) */
        B_LIQUID_WATER_EXCESS_2("BL", null),

        /** Lagged excess liquid water for interval 3 (in, mm) */
        B_LIQUID_WATER_EXCESS_3("BM", null),

        /** Lagged excess liquid water for interval 4 (in, mm) */
        B_LIQUID_WATER_EXCESS_4("BN", null),

        /** Lagged excess liquid water for interval 5 (in, mm) */
        B_LIQUID_WATER_EXCESS_5("BO", null),

        /** Lagged excess liquid water for interval 6 (in, mm) */
        B_LIQUID_WATER_EXCESS_6("BP", null),

        /** Lagged excess liquid water for interval 7 (in, mm) */
        B_LIQUID_WATER_EXCESS_7("BQ", null),

        // TODO Figure out what C means

        /** Upper zone tension water contents (in, mm) */
        C_UPPER_ZONE_TENSION_WATER("CA", null),

        /** Upper zone free water contents (in, mm) */
        C_UPPER_ZONE_FREE_WATER("CB", null),

        /** Lower zone tension water contents (in, mm) */
        C_LOWER_ZONE_TENSION_WATER("CC", null),

        /** Lower zone free water supplementary storage contents (in, mm) */
        C_LOWER_ZONE_FREE_WATER_SUPPLEMENTARY_STORAGE_CONTENTS("CD", null),

        /** Lower zone free water primary storage contents (in, mm) */
        C_LOWER_ZONE_FREE_WATER_PRIMARY_STORAGE_CONTENTS("CE", null),

        /** Additional impervious area contents (in, mm) */
        C_ADDITIONAL_IMPERVIOUS_AREA_CONTENTS("CF", null),

        /** Antecedent precipitation index (in, mm) */
        C_ANTECEDENT_PRECIPITATION_INDEX("CG", null),

        /** Soil moisture index deficit (in, mm) */
        C_SOIL_MOISTER_INDEX_DEFICIT("CH", null),
        /** Base flow storage contents (in, mm) */
        C_BASE_FLOW_STORAGE_CONENTS("CI", null),
        /** Base flow index (in, mm) */
        C_BASE_FLOW_INDEX("CJ", null),
        /** First quadrant index Antecedent Evaporation Index (AEI) (in, mm) */
        C_FIRST_QUADRANT_AEI("CK", null),
        /** First quadrant index Antecedent Temperature Index (ATI) (DF, DC) */
        C_FIRST_QUADRANT_ATI("CL", null),
        /** Frost index (DF, DC) */
        C_FROST_INDEX("CM", null),
        /** Frost efficiency index (%) */
        C_FROST_EFFICIENCY_INDEX("CN", null),
        /** Indicator of first quadrant index (AEI or ATI) */
        C_FIRST_QUADRANT_INDICATOR("CO", null),
        /** Storm total rainfall (in, mm) */
        C_STORM_TOTAL_RAINFAL("CP", null),
        /** Storm total runoff (in, mm) */
        C_STORM_TOTAL_RUNOFF("CQ", null),
        /** Storm antecedent index (in, mm) */
        C_STORM_ANTECEDENT_INDEX("CR", null),
        /** Current antecedent index (in, mm) */
        C_CURRENT_ANTECEDENT_INDEX("CS", null),
        /** Storm period counter (integer) */
        C_STORM_PERIOD_COUNTER("CT", null),
        /** Average air temperature (DF, DC) */
        C_AVERAGE_AIR_TEMPERATURE("CU", null),
        /** Current corrected synthetic temperature (DF, DC) */
        C_CURRENT_CORRECTED_SYNTHETIC_TEMPERATURE("CV", null),
        /** Storm antecedent evaporation index, AEI (in, mm) */
        C_STORM_AEI("CW", null),
        /** Current AEI (in, mm) */
        C_CURRENT_AEI("CX", null),
        /** Current API (in, mm) */
        C_CURRENT_API("CY", null),
        /** Climate Index */
        C_CLIMATE_INDEX("CZ", null),
        /** Evapotranspiration potential amount (IN, MM) */
        EVAPORATION_POTENTIAL_AMOUNT("EA", null),
        /** Evaporation, pan depth (IN, MM) */
        EVAPORATION_PAN_DEPTH("ED", null),
        /** Evapotranspiration amount (IN, MM) */
        EVAPORATION_AMOUNT("EM", null),
        /** Evaporation, pan increment (IN, MM) */
        EVAPORATION_PAN_INCREMENT("EP", null),
        /** Evaporation rate (IN/day, MM/day) */
        EVAPORATION_RATE("ER", null),
        /** Evapotranspiration total (IN, MM) */
        EVAPORATION_TOTAL("ET", null),
        /** Evaporation, lake computed (IN, MM) */
        EVAPORATION_LAKE_COMPUTED("EV", null),
        /** Condition, road surface (coded, see Table 1) */
        GROUND_CONDITION("GC", null),
        /** Frost depth, depth of frost penetration, non permafrost (IN, CM) */
        GROUND_FROST_DEPTH("GD", null),
        /** Salt content on a surface (e.g., road) (%) */
        GROUND_SALT_CONTENT("GL", null),
        /** Frost, depth of pavement surface (IN, CM) */
        GROUND_FROST_DEPTH_PAVEMENT("GP", null),
        /**
         * Frost report, structure:
         * <dl>
         * <dt>1
         * <dd>Concrete
         * <dt>2
         * <dd>Granular
         * <dt>3
         * <dd>Honeycomb
         * <dt>4
         * <dd>Stalactite
         * </dl>
         */
        GROUND_FROST_REPORT("GR", null),
        /**
         * Ground state:
         * 
         * <ul>
         * <li>0-9 Without snow or measurable ice cover
         * <li>10-20 With snow or measurable ice cover
         * <ul>
         * <dl>
         * <dt>0
         * <dd>Surface of ground dry (without cracks and no appreciable amount
         * of loose sand)
         * <dt>1
         * <dd>Surface of ground moist
         * <dt>2
         * <dd>Surface of ground wet (standing water in small or large pools on
         * surface)
         * <dt>3
         * <dd>Flooded
         * <dt>4
         * <dd>Surface of ground frozen
         * <dt>5
         * <dd>Glaze on ground
         * <dt>6
         * <dd>Loose dry dust or sand not covering ground completely
         * <dt>7
         * <dd>Thin cover of loose dry dust or sand covering ground completely
         * <dt>8
         * <dd>Moderate or thick cover of loose, dry dust or sand covering
         * ground completely
         * <dt>9
         * <dd>Extremely dry with cracks
         * <dt>10
         * <dd>Ground predominantly covered by ice
         * <dt>11
         * <dd>Compact or wet snow (with or without ice) covering less than
         * one-half of the ground
         * <dt>12
         * <dd>Compact or wet snow (with or without ice) covering at least
         * one-half of the ground but ground not completely covered
         * <dt>13
         * <dd>Even layer of compact or wet snow covering ground completely
         * <dt>14
         * <dd>Uneven layer of compact or wet snow covering ground completely
         * <dt>15
         * <dd>Loose dry snow covering less than one-half of the ground
         * <dt>16
         * <dd>Loose dry snow covering at least one-half of the ground (but not
         * completely)
         * <dt>17
         * <dd>Even layer of loose, dry snow covering ground completely
         * <dt>18
         * <dd>Uneven layer of loose, dry snow covering ground completely
         * <dt>19
         * <dd>Snow covering ground completely deep drifts
         * <dt>20
         * <dd>Sleet or hail covering the ground completely
         * </dl>
         */
        GROUND_STATE("GS", null),
        /** Frost, depth of surface frost thawed (IN, CM) */
        GROUND_FROST_DEPTH_THAWED("GT", null),
        /** Frost, depth of pavement surface frost thawed (IN, CM) */
        GROUND_FROST_DEPTH_THAWED_PAVEMENT("GW", null),
        /** Height of reading, altitude above surface (FT, M) */
        HEIGHT_READING_ABOVE_SURFACE("HA", null),
        /** Depth of reading below surface (FT, M) */
        DEPTH_READING_BELOW_SURFACE("HB", null),
        /** Height, ceiling (FT, M) */
        HEIGHT_CEILING("HC", null),
        /** Height, head (FT, M) */
        HEIGHT_HEAD("HD", null),
        /** Height, regulating gate (FT, M) */
        HEIGHT_REGULATING_GATE("HE", null),
        /** Elevation, project powerhouse forebay (FT, M) */
        // TODO : Are these duplicates correct!?
        RESERVOIR_FOREBAY_ELEVATION("HF", null),
        /** Elevation, project powerhouse forebay (FT, M) */
        ELEVATION_POWERHOUSE_FOREBAY("HF", null),
        /** Height, river stage (FT, M) */
        HEIGHT_RIVER_STAGE("HG", null),
        /** Height of reading, elevation in MSL (FT, M) */
        HEIGHT_READING_MSL("HH", null),
        /**
         * Stage trend indicator:
         * 
         * <ul>
         * <li>Code figures 0, 1 or 2 as appropriate only when stage is below
         * flood stage.
         * <li>Code figures 4, 5, or 6 as appropriate only when stage is at or
         * above flood stage.
         * </ul>
         * 
         * <dl>
         * <dt>0
         * <dd>Stationary
         * <dt>1
         * <dd>Rising
         * <dt>2
         * <dd>Falling
         * <dt>3
         * <dd>Unknown
         * <dt>4
         * <dd>Stationary
         * <dt>5
         * <dd>Rising
         * <dt>6
         * <dd>Falling
         * <dt>7
         * <dd>Frozen
         * </dl>
         */
        STAGE_TREND_INDICATOR("HI", null),
        /** Height, spillway gate (FT, M) */
        HEIGHT_SPILLWAY_GATE("HJ", null),
        /** Height, lake above a specified datum (FT, M) */
        HEIGHT_LAKE_ABOVE_DATUM("HK", null),
        /** Elevation, natural lake (FT, M) */
        ELEVATION_NATURAL_LAKE("HL", null),
        /** Height of tide, MLLW (FT, M) */
        HEIGHT_TIDE("HM", null),
        /**
         * (S) Height, river stage, daily minimum, translates to HGIRZNZ (FT, M)
         */
        HEIGHT_RIVER_STAGE_DAILY_MINIMUM("HN", "HGIRZNZ"),
        /** Height, flood stage (FT, M) */
        HEIGHT_FLOOD_STAGE("HO", null),
        /** Elevation, pool (FT, M) */
        ELEVATION_POOL("HP", null),
        /**
         * Distance from a ground reference point to the river's edge used to
         * estimate stage (coded, see Chapter 7.4.6)
         */
        STAGE_ESTIMATE("HQ", null),
        /** Elevation, lake or reservoir rule curve (FT, M) */
        ELEVATION_RULE_CURVE("HR", null),
        /** Elevation, spillway forebay (FT, M) */
        ELEVATION_SPILLWAY("HS", null),
        /** Elevation, project tail water stage (FT, M) */
        ELEVATION_PROJECT_TAIL("HT", null),
        /** Height, cautionary stage (FT, M) */
        HEIGHT_CAUTIONARY_STAGE("HU", null),
        /** Depth of water on a surface (e.g., road) (IN, MM) */
        DEPTH_SURFACE_WATER("HV", null),
        /** Height, spillway tail water (FT, M) */
        HEIGHT_SPILLWAY_TAIL_WATER("HW", null),
        /**
         * (S) Height, river stage, daily maximum, translates to HGIRZXZ (FT, M)
         */
        HEIGHT_RIVER_STAGE_DAILY_MAXIMUM("HX", "HGIRZXZ"),
        /**
         * (S) Height, river stage at 7 a.m. local just prior to date-time
         * stamp, translates to HGIRZZZ at 7 a.m. local time (FT, M)
         */
        HEIGHT_RIVER_STAGE_7AM("HY", "HGIRZZZ"),
        /** Elevation, freezing level (KFT, KM) */
        ELEVATION_FREEZING_LEVEL("HZ", null),
        /** Ice cover, river (%) */
        ICE_COVER("IC", null),
        /**
         * Extent of ice from reporting area, upstream “+,” downstream - (MI,
         * KM)
         */
        ICE_EXTENT("IE", null),
        /**
         * Extent of open water from reporting area, downstream “+,” upstream -
         * (FT, M)
         */
        ICE_OPEN_WATER_EXTENT("IO", null),
        /**
         * Ice report type, structure, and cover:
         * 
         * <pre>
         * The ice report consists of a three-digit code:
         *           IR x1x2x3
         *           IR x2x3
         *           IR x3
         *           x1 = Ice type
         *           x2 = Ice structure
         *           x3 = Ice cover
         *                       
         *         ICE TYPE              CODE       ICE STRUCTURE CODE
         *         No report             0          No report     0
         *         Running ice           1          Breaking ice  1
         *         Stationary ice        2          Honeycombed   2
         *         Stopped ice           3          Rotten        3
         *         Jammed ice            4          Layered       4
         *         Formed locally        5          Clear         5
         *         Shore ice             6          Hanging       6
         *         Anchor ice            7          Frazil        7
         *         Cake ice              8          Slush         8
         *         Shell ice             9          Sheet         9
         * 
         *         ICE COVER             CODE
         *         No ice                0
         *         1/10 Cover            1
         *         2/10 “                2
         *         3/10 “                3
         *         4/10 “                4
         *         5/10 “                5
         *         6/10 “                6
         *         7/10 “                7
         *         8/10 - 9/10           8
         *         Fully covered         9
         * </pre>
         */
        ICE_REPORT_TYPE("IR", null),
        /** Ice thickness (IN, CM) */
        ICE_THICKNESS("IT", null),
        /** Lake surface area (KAC,KM2) */
        LAKE_SURFACE_AREA("LA", null),
        /**
         * Lake storage volume change (KAF,MCM) Thousands of acre-feet,Millions
         * of cubic meters
         */
        LAKE_STORAGE_VOLUME_CHANGE("LC", null),
        /** Lake storage volume (KAF,MCM) */
        LAKE_STORAGE_VOLUME("LS", null),
        /**
         * Dielectric Constant at depth, paired value vector (coded, see Chapter
         * 7.4.6 for format)
         */
        DIELECTRIC_CONSTANT("MD", null),
        /** Moisture, soil index or API (IN, CM) */
        MOISTURE_SOIL_INDEX("MI", null),
        /** Moisture, lower zone storage (IN, CM) */
        MOISTURE_LOWER_ZONE_STORAGE("ML", null),
        /** Fuel moisture, wood (%) */
        FUEL_MOISTURE("MM", null),
        /**
         * Soil Salinity at depth, paired value vector (coded, see Chapter 7.4.6
         * for format)
         */
        SOIL_SALINITY("MN", null),
        /** Soil Moisture amount at depth (coded, see Chapter 7.4.6) */
        SOIL_MOISTURE("MS", null),
        /** Fuel temperature, wood probe (DF, DC) */
        FUEL_TEMPERATURE("MT", null),
        /** Moisture, upper zone storage (IN, CM) */
        MOISTURE_UPPER_ZONE_STORAGE("MU", null),
        /**
         * Water Volume at Depth, paired value vector (coded, see Chapter 7.4.6
         * for format)
         */
        WATER_VOLUME("MV", null),
        /** Moisture, soil, percent by weight (%) */
        MOISTURE_SOIL("MW", null),
        /**
         * River control switch (0=manual river control, 1=open river
         * uncontrolled)
         */
        DAM_RIVER_CONTROL("NC", null),
        /** Total of gate openings (FT, M) */
        DAM_GATE_OPENINGS("NG", null),
        /** Number of large flash boards down (whole number) */
        DAM_LARGE_FLASH_BOARDS_DOWN("NL", null),
        /** Number of the spillway gate reported (used with HP, QS) */
        DAM_SPILLWAY_GATE_REPORTED("NN", null),
        /** Gate opening for a specific gate (coded, see Chapter 7.4.6) */
        DAM_GATE_OPENING("NO", null),
        /** Number of small flash boards down (whole number) */
        DAM_SMALL_FLASH_BOARDS_DOWN("NS", null),
        /** Discharge, adjusted for storage at project only (KCFS, CMS) */
        DISCHARGE_ADJUSTED("QA", null),
        /** Runoff depth (IN, MM) */
        DISCHARGE_RUNOFF_DEPTH("QB", null),
        /** Runoff volume (KAF, MCM) */
        DISCHARGE_RUNOFF_VOLUME("QC", null),
        /** Discharge, canal diversion (KCFS, CMS) */
        DISCHARGE_CANAL_DIVERSION("QD", null),
        /** Discharge, percent of flow diverted from channel (%) */
        DISCHARGE_CHANNEL_FLOW_DIVERSION("QE", null),
        /** Discharge velocity (MPH, KPH) */
        DISCHARGE_VELOCITY("QF", null),
        /** Discharge from power generation (KCFS, CMS) */
        DISCHARGE_POWER_GENERATION("QG", null),
        /** Discharge, inflow (KCFS, CMS) */
        DISCHARGE_INFLOW("QI", null),
        /** Discharge, rule curve (KCFS, CMS) */
        DISCHARGE_RULE_CURVE("QL", null),
        /** Discharge, preproject conditions in basin (KCFS, CMS) */
        DISCHARGE_PREPROJECT_CONDITION("QM", null),
        /** (S) Discharge, minimum flow, translates to QRIRZNZ (KCFS, CMS) */
        DISCHARGE_MINIMUM_FLOW("QN", "QRIRZNZ"),
        /** Discharge, pumping (KCFS, CMS) */
        DISCHARGE_PUMPING("QP", null),
        /** Discharge, river (KCFS, CMS) */
        DISCHARGE_RIVER("QR", null),
        /** Discharge, spillway (KCFS, CMS) */
        DISCHARGE_SPILLWAY("QS", null),
        /** Discharge, computed total project outflow (KCFS, CMS) */
        DISCHARGE_TOTAL_PROJECT_OUTFLOW("QT", null),
        /** Discharge, controlled by regulating outlet (KCFS, CMS) */
        DISCHARGE_REGULATING_OUTLET_CONTROLLED("QU", null),
        /** Cumulative volume increment (KAF, MCM) */
        DISCHARGE_CUMULATIVE_VOLUME_INCREMENT("QV", null),
        /** (S) Discharge, maximum flow, translates to QRIRZXZ (KCFS, CMS) */
        DISCHARGE_MAXIMUM_FLOW("QX", "QRIRZXZ"),
        /**
         * (S) Discharge, river at 7 a.m. local just prior to date-time stamp
         * translates to QRIRZZZ at 7 a.m. local time (KCFS, CMS)
         */
        DISCHARGE_RIVER_7AM("QY", "QRIRZZZ"),
        /** Reserved */
        DISCHARGE_RESERVED("QZ", null),
        /** Radiation, albedo (%) */
        RADIATION_ALBEDO("RA", null),
        /**
         * Radiation, accumulated incoming solar over specified duration in
         * langleys (LY)
         */
        RADIATION_ACCUMULATED_SOLAR("RI", null),
        /** Radiation, net radiometers (watts/meter squared) */
        RADIATION_NET_RADIOMETERS("RN", null),
        /** Radiation, sunshine percent of possible (%) */
        RADIATION_SUNSHINE_PERCENT("RP", null),
        /** Radiation, sunshine hours (HRS) */
        RADIATION_SUNSHINE_HOURS("RT", null),
        /** Radiation, total incoming solar radiation (watts/meter squared) */
        RADIATION_TOTAL_SOLAR("RW", null),
        /** Snow, areal extent of basin snow cover (%) */

        SNOW_AREAL_EXTENT("SA", null),
        /** Snow, Blowing Snow Sublimation (IN) */
        SNOW_BLOWING_SNOW("SB", null),
        /** Snow, depth (IN, CM) */
        SNOW_DEPTH("SD", null),
        /** Snow, Average Snowpack Temperature (DF) */
        SNOW_SNOWPACK_TEMPERATURE("SE", null),
        /** Snow, depth, new snowfall (IN, CM) */
        SNOW_NEW_SNOWFALL("SF", "SFDRZZZ"),
        /** Snow, depth on top of river or lake ice (IN, CM) */
        SNOW_DEPTH_ON_ICE("SI", null),
        /** Snow, elevation of snow line (KFT, M) */
        SNOW_LINE_ELEVATION("SL", null),
        /** Snow, Melt (IN) */
        SNOW_MELT("SM", null),
        /** Snowmelt plus rain (IN) */
        SNOW_PLUS_RAIN("SP", null),
        /**
         * Snow report, structure, type, surface, and bottom:
         * 
         * <pre>
         * The snow report consists of a four-digit code:
         *              SR x1x2x3x4
         *              SR x2x3x4
         *              SR x3x4
         *              SR x4
         *              x1 = Snow structure
         *              x2 = Base of snow cover
         *              x3 = Surface of snow cover
         *              x4 = Area description
         *                                    
         *         SNOW STRUCTURE                     BASE OF SNOWCOVER
         *         No report      0                   No report   0
         *         Loosely packed 1                   Wet snow    1
         *         Densely packed 2                   Dry snow    2
         *                                            Ice layer   3
         *                                            
         *         SURFACE OF SNOWCOVER               AREA DESCRIPTION
         *         No report    0                     No report     0
         *         Snow crust   1                     Uniform       1
         *         Loose        2                     Some drifts   2
         *         Ice          3                     Drifted       3
         * </pre>
         */
        SNOW_REPORT("SR", null),
        /** Snow density (IN SWE/IN snow, CM SWE/CM snow) */
        SNOW_DENSITY("SS", null),
        /**
         * Snow temperature at depth measured from ground (See Chapter 7.4.6 for
         * format)
         */
        SNOW_TEMPERATURE("ST", null),
        /** Snow, Surface Sublimation (IN) */
        SNOW_SURFACE_SUBLIMATION("SU", null),
        /** Snow, water equivalent (IN, MM) */
        SNOW_WATER_EQUIVALENT("SW", null),
        /** Temperature, air, dry bulb (DF,DC) */
        TEMPERATURE_AIR_DRY("TA", null),
        /**
         * Temperature in bare soil at depth (coded, see Chapter 7.4.6 for
         * format)
         */
        TEMPERATURE_BARE_SOIL_DEPTH("TB", null),
        /** Temperature, degree days of cooling, above 65 DF or 18.3 DC (DF,DC) */
        TEMPERATURE_COOLING("TC", null),
        /** Temperature, dew point (DF,DC) */
        TEMPERATURE_DEW("TD", null),
        /**
         * Temperature, air temperature at elevation above MSL (See Chapter
         * 7.4.6 for format)
         */
        TEMPERATURE_ELEVATION_ABOVE_MSL("TE", null),
        /** Temperature, degree days of freezing, below 32 DF or 0 DC (DF,DC) */
        TEMPERATURE_FREEZING("TF", null),
        /** Temperature, degree days of heating, below 65 DF or 18.3 DC (DF,DC) */
        TEMPERATURE_HEATING("TH", null),
        /** Temperature, departure from normal (DF, DC) */
        TEMPERATURE_NORMAL_DEPARTURE("TJ", null),
        /** Temperature, air, wet bulb (DF,DC) */
        TEMPERATURE_AIR_WET("TM", null),
        /** (S) Temperature, air minimum, translates to TAIRZNZ (DF,DC) */
        TEMPERATURE_AIR_MINIMUM("TN", "TAIRZNZ"),
        /** Temperature, pan water (DF,DC) */
        TEMPERATURE_PAN_WATER("TP", null),
        /** Temperature, road surface (DF,DC) */
        TEMPERATURE_ROAD_SURFACE("TR", null),
        /** Temperature, bare soil at the surface (DF,DC) */
        TEMPERATURE_BARE_SOIL_SURFACE("TS", null),
        /**
         * Temperature in vegetated soil at depth (coded, see Chapter 7.4.6 for
         * format)
         */
        TEMPERATURE_VEGETAGED_SOIL_DEPTH("TV", null),
        /** Temperature, water (DF,DC) */
        TEMPERATURE_WATER("TW", null),
        /** (S) Temperature, air maximum, translates to TAIRZXZ (DF,DC) */
        TEMPERATURE_AIR_MAXIMUM("TX", "TAIRZXZ"),
        /** Temperature, Freezing, road surface (DF,DC) */
        TEMPERATURE_FREEZING_SURFACE("TZ", null),
        /** Wind, accumulated wind travel (MI,KM) */
        WIND_ACCUMULATED_TRAVEL("UC", null),
        /** Wind, direction (whole degrees) */
        WIND_DIRECTION("UD", null),
        /** Wind, standard deviation (Degrees) */
        WIND_STANDARD_DEVIATION("UE", null),
        /** Wind, gust at observation time (MPH,M/SEC) */
        WIND_GUST("UG", null),
        /** Wind, travel length accumulated over specified (MI,KM) */
        WIND_TRAVEL_LENGTH("UL", null),
        /** Peak wind speed (MPH) */
        WIND_PEAK("UP", null),
        /**
         * Wind direction and speed combined (SSS.SDDD), a value of 23.0275
         * would indicate a wind of 23.0 MPH from 275 degrees
         */
        WIND_DIRECTION_SPEED("UQ", null),
        /**
         * Peak wind direction associated with peak wind speed (in tens of
         * degrees)
         */
        WIND_PEEK_DIRECTION_SPEED("UR", null),
        /** Wind, speed (MPH,M/SEC) */
        WIND_SPEED("US", null),
        /** Voltage - battery (volt) */
        GENERATION_BATTERY_VOLTAGE("VB", null),
        /** Generation, surplus capacity of units on line (megawatts) */
        GENERATION_SURPLUS_CAPACITY("VC", null),
        /** Generation, energy total (megawatt hours) */
        GENERATION_ENERGY_TOTAL("VE", null),
        /** Generation, pumped water, power produced (megawatts) */
        GENERATION_PUMPED_WATER_POWER_PRODUCED("VG", null),
        /** Generation, time (HRS) */
        GENERATION_TIME("VH", null),
        /** Generation, energy produced from pumped water (megawatt hours) */
        GENERATION_PUMPED_WATER_ENERGY_PRODUCED("VJ", null),
        /** Generation, energy stored in reservoir only (megawatt * “duration”) */
        GENERATION_ENERGY_STORED_RESERVOIR("VK", null),
        /** Generation, storage due to natural flow only (megawatt * “duration”) */
        GENERATION_ENERGY_STORED_NATURAL_FLOW("VL", null),
        /**
         * Generation, losses due to spill and other water losses (megawatt *
         * “duration”)
         */
        GENERATION_ENERGY_LOSSES("VM", null),
        /** Generation, pumping use, power used (megawatts) */
        GENERATION_PUMPING_POWER_USED("VP", null),
        /** Generation, pumping use, total energy used (megawatt hours) */
        GENERATION_PUMPING_ENERGY_USED("VQ", null),
        /**
         * Generation, stored in reservoir plus natural flow, energy potential
         * (megawatt * “duration”)
         */
        GENERATION_ENERGY_POTENTIAL("VR", null),
        /** Generation, station load, energy used (megawatt hours) */
        GENERATION_STATION_LOAD_ENERGY_USED("VS", null),
        /** Generation, power total (megawatts) */
        GENERATION_POWER_TOTAL("VT", null),
        /** Generator, status (encoded) */
        GENERATION_GENERATOR_STATUS("VU", null),
        /** Generation station load, power used (megawatts) */
        GENERATION_STATION_LOAD_POWER_USED("VW", null),
        /** Water, dissolved nitrogen & argon (PPM, MG/L) */
        WATER_DISSOLVED_NITROGEN_ARGON("WA", null),
        /** Water, conductance (uMHOS/CM) */
        WATER_CONDUCTANCE("WC", null),
        /** Water, piezometer water depth (IN, CM) */
        WATER_DEPTH("WD", null),
        /** Water, dissolved total gases, pressure (IN-HG, MM-HG) */
        WATER_DISSOLVED_GASES("WG", null),
        /** Water, dissolved hydrogen sulfide (PPM, MG/L) */
        WATER_DISSOLVED_HYDROGEN_SULFIDE("WH", null),
        /** Water, suspended sediment (PPM, MG/L) */
        WATER_SUSPENDED_SEDIMENT("WL", null),
        /** Water, dissolved oxygen (PPM, MG/L) */
        WATER_DISSOLVED_OXYGEN("WO", null),
        /** Water, ph (PH value) */
        WATER_PH("WP", null),
        /** Water, salinity (parts per thousand, PPT) */
        WATER_SALINITY("WS", null),
        /** Water, turbidity (JTU) */
        WATER_TURBIDITY("WT", null),
        /** Water, velocity (FT/SEC, M/SEC) */
        WATER_VELOCITY("WV", null),
        /** Water, Oxygen Saturation (%) */
        WATER_OXYGEN_SATURATION("WX", null),
        /** Water, Chlorophyll (ppb (parts/billion), ug/L (micrograms/L)) */
        WATER_CHLOROPHYLL("WY", null),
        /** Total sky cover (tenths) */
        WEATHER_SKY_COVER("XC", null),
        /** Lightning, number of strikes per grid box (whole number) */
        WEATHER_LIGHTENING_GRID("XG", null),
        /**
         * Lightning, point strike, assumed one strike at transmitted latitude
         * and longitude (whole number)
         */
        WEATHER_LIGHTENING_POINT_STRIKE("XL", null),
        /** Weather, past NWS synoptic code (see Appendix D) */
        WEATHER_SYNOPTIC_CODE_PAST("XP", null),
        /** Humidity, relative (%) */
        WEATHER_HUMIDITY_RELATIVE("XR", null),
        /** Humidity, absolute (grams/FT3,grams/M3) */
        WEATHER_HUMIDITY_ABSOLUTE("XU", null),
        /** Weather, visibility (MI, KM) */
        WEATHER_VISIBILITY("XV", null),
        /** Weather, present NWS synoptic code (see Appendix C) */
        WEATHER_SYNOPTIC_CODE_PRESENT("XW", null),
        /**
         * Number of 15-minute periods a river has been above a specified
         * critical level (whole number)
         */
        STATION_RIVER_ABOVE_CRITICAL("YA", null),
        /** Random report sequence number (whole number) */
        STATION_RANDOM_SEQUENCE("YC", null),
        /**
         * Forward power, a measurement of the DCP, antenna, and coaxial cable
         * (watts)
         */
        STATION_FORWARD_POWER("YF", null),
        /** SERFC unique */
        STATION_SERFC("YI", null),
        /** Reserved Code */
        STATION_RESERVED("YP", null),
        /**
         * Reflected power, a measurement of the DCP, antenna, and coaxial cable
         * (watts)
         */
        STATION_REFLECTED_POWER("YR", null),
        /**
         * Sequence number of the number of times the DCP has transmitted (whole
         * number)
         */
        STATION_TRANSMISSION_SEQUENCE("YS", null),
        /**
         * Number of 15-minute periods since a random report was generated due
         * to an increase of 0.4 inch of precipitation (whole number)
         */
        STATION_RANDOM_PRECIPITATION_REPORT("YT", null),
        /** GENOR raingage status level 1 - NERON observing sites (YUIRG) */
        STATION_GENOR_STATUS1("YU", null),
        /** A Second Battery Voltage (NERON sites ONLY), voltage 0 (YVIRG) */
        STATION_SECOND_BATTERY_VOLTAGE("YV", null),
        /** GENOR raingage status level 2 - NERON observing sites (YWIRG) */
        // STATION_GENOR_STATUS2("YW",null),
        /** GENOR raingage status level 3 - NERON observing sites (YYIRG) */
        STATION_GENOR_STATUS3("YY", null),
        /**
         * Time of Observation – Minutes of the calendar day, minutes 0 - NERON
         * observing sites (YZIRG)
         */
        // STATION_OBSERVATION_TIME("YZ",null),

        FISH_SHAD("FA", null),

        FISH_SOCKEYE("FB", null),

        FISH_CHINOOK("FC", null),

        FISH_CHUM("FE", null),

        FISH_COHO("FK", null),

        /** 1=left, 2=right, 3=total */
        FISH_LADDER("FL", null),

        FISH_PINK("FP", null),

        FISH_STEELHEAD("FS", null),

        /** 1=adult, 2=jacks, 3=fingerlings */
        FISH_TYPE("FT", null),

        /** Count of all types combined */
        FISH_ALL("FZ", null),

        PRESSURE_ATMOSPHERIC("PA", null),

        /** Atmospheric net change during past 3 hours */
        PRESSURE_ATMOSPHERIC_3HR("PD", null),

        PRESSURE_SEA_LEVEL("PL", null),

        PRESSURE_CHARACTERISTIC("PE", null),

        /**
         * Precipitation, flash flood guidance, precipitation to initiate
         * flooding, translates to PPTCF for 3-hour intervals
         */
        PRECIPITATION_FLASH_FLOOD_GUIDANCE("PF", "PPTCF"),

        /** Departure from normal */
        PRECIPITATION_NORMAL_DEPARTURE("PJ", null),

        PRECIPITATION_ACCUMULATOR("PC", null),

        /** Probability of measurable precipitation (dimensionless) */
        PRECIPITATION_MEASURABLE_PROBABILITY("PM", null),

        PRECIPITATION_NORMAL("PN", null),

        PRECIPITATION_INCREMENT("PP", null),

        PRECIPITATION_RATE("PR", null),

        PRECIPITATION_TYPE("PT", null),

        /**
         * (S) Precipitation, increment ending at 7 a.m. local just prior to
         * date-time stamp, translates to PPDRZZZ at 7 a.m. local time (IN,MM)
         */
        PRECIPITATION_INCREMENT_DAILY("PY", "PPDRZZZ"),

        UNKNOWN(null, null);

        private String code;

        private final String translatedCode;

        private static Map<String, PhysicalElement> map;

        static {
            map = Collections.unmodifiableMap(createMap());
        }

        private PhysicalElement(String code, String translation) {
            this.code = code;
            translatedCode = translation;
        }

        public String getCode() {
            return this.code;
        }

        /**
         * Get the PE translation, if defined.
         * 
         * @return The PE translation if defined. Null reference otherwise.
         */
        public String translate() {
            return translatedCode;
        }

        public PhysicalElementCategory getCategory() {
            return PhysicalElementCategory.getEnum(this.code.substring(0, 1));
        }

        public static PhysicalElement getEnum(String code) {
            PhysicalElement p = map.get(code);
            if (p != null) {
                return p;
            }
            return UNKNOWN;
        }

        private static Map<String, PhysicalElement> createMap() {
            Map<String, PhysicalElement> map = new HashMap<String, PhysicalElement>(
                    PhysicalElement.values().length);
            for (PhysicalElement pe : PhysicalElement.values()) {
                map.put(pe.getCode(), pe);
            }

            return map;
        }
    }

    private static final HashMap<PhysicalElement, PhysicalElement> TRACE_CODES = new HashMap<PhysicalElement, PhysicalElement>();
    static {
        TRACE_CODES.put(PhysicalElement.PRECIPITATION_INCREMENT,
                PhysicalElement.PRECIPITATION_INCREMENT);
        TRACE_CODES.put(PhysicalElement.PRECIPITATION_ACCUMULATOR,
                PhysicalElement.PRECIPITATION_ACCUMULATOR);
        TRACE_CODES.put(PhysicalElement.PRECIPITATION_INCREMENT_DAILY,
                PhysicalElement.PRECIPITATION_INCREMENT_DAILY);
        TRACE_CODES.put(PhysicalElement.SNOW_NEW_SNOWFALL,
                PhysicalElement.SNOW_NEW_SNOWFALL);
    }

    /**
     * 
     * @param element
     * @return
     */
    public static final boolean usesTrace(PhysicalElement element) {
        return (TRACE_CODES.get(element) != null);
    }

    /**
     * The duration code describes the period to which an observed or computed
     * increment applies, such as mean discharge or precipitation increment. If
     * the physical element described by the duration code is a continuous
     * function, such as river discharge, the physical element value can be
     * assumed to be an average. If the physical element is noncontinuous, such
     * as precipitation increment, the physical element value can be assumed to
     * be a summation over the duration specified.
     * <p>
     * Legacy fortran code parsed raw shef data into a binary shefout file where
     * the duration was formatted as follows:
     * <ul>
     * <li>7XXX seconds
     * <li>OXXX minutes
     * <li>1XXX hours
     * <li>2XXX days
     * <li>3XXX months
     * <li>4XXX years
     * <li>5000 specified as default
     * <li>5001 seasonal
     * <li>5002 entire period of record
     * <li>5003 variable period - duration specified separately (see Table 11a
     * and Table 11b)
     * <li>5004 time period beginning at 7AM local time prior to the observation
     * and ending at the observation time
     * <li>5005 unknown
     * <li>6XXX months - end of month
     * </ul>
     * The above values can be obtained through the getValue method for each
     * constant.
     * 
     * 
     */
    public static enum Duration {

        /* leading _'s were used since variables cannot begin with a number */

        INSTANTENOUS("I", 0),

        _1_MINUTE("U", 1),

        _5_MINUTES("E", 5),

        _10_MINUTES("G", 10),

        _15_MINUTES("C", 15),

        _30_MINUTES("J", 30),

        _1_HOUR("H", 1001),

        /**
         * 60 minutes is equivalent to the _1_HOUR code, but has a different
         * value
         */
        _60_MINUTES("-", 60),

        _2_HOUR("B", 1002),

        _3_HOUR("T", 1003),

        _4_HOUR("F", 1004),

        _6_HOUR("Q", 1006),

        _8_HOUR("A", 1008),

        _12_HOUR("K", 1012),

        _18_HOUR("L", 1018),

        _1_DAY("D", 2001),

        _1_WEEK("W", 2007),

        /**
         * Mid month, duration for the period from the 1st day of the month to
         * and ending on the 15th day of the same month
         */
        MID_MONTH("N", 2015),

        _1_MONTH("M", 3001),

        _1_YEAR("Y", 4001),

        /**
         * Duration for a period beginning at previous 7 a.m. local and ending
         * at time of observation
         */
        _1_PERIOD("P", 5004),

        /**
         * Variable period, duration defined separately
         */
        VARIABLE_PERIOD("V", 5003),

        /**
         * Period of seasonal duration (normally used to designate a partial
         * period, for example, 1 January to current date)
         */
        SEASONAL_PERIOD("S", 5001),

        /**
         * Entire period of record
         */
        RECORD_PERIOD("R", 5002),

        UNKNOWN("X", 5005),

        DEFAULT("Z", 5000);

        /** used in the raw shef data */
        private String code;

        /** used in legacy shef processing code */
        private int value;

        private static Map<String, Duration> map;

        static {
            map = Collections.unmodifiableMap(createMap());
        }

        Duration(String code) {
            this.code = code;
        }

        Duration(String code, int value) {
            this.code = code;
            this.value = value;
        }

        /**
         * The shef character code
         * 
         * @return SHEF character code
         */
        public String getCode() {
            return code;
        }

        /**
         * Obtain the associated value
         * <p>
         * 
         * @return the associated value or null if non exists
         */
        public int getValue() {
            return value;
        }

        /**
         * Obtain a Duration enumeration of the given shef code
         * <p>
         * 
         * @param code
         *            the shef code
         * @return a Duration enumeration matching the shef code or
         *         Duration.UNKNOWN if no match is found.
         */
        public static Duration getEnum(String code) {
            Duration d = map.get(code);
            if (d != null) {
                return d;
            }

            return UNKNOWN;
        }

        public static Duration getDefault(PhysicalElement pe) {
            Duration d = DEFAULT_DURATIONS.get(pe);
            if (d == null) {
                d = INSTANTENOUS;
            }
            return d;
        }

        private static Map<String, Duration> createMap() {
            Map<String, Duration> map = new HashMap<String, Duration>();
            for (Duration d : Duration.values()) {
                map.put(d.getCode(), d);
            }

            return map;
        }

    }

    private static final HashMap<PhysicalElement, Duration> DEFAULT_DURATIONS = new HashMap<PhysicalElement, Duration>();
    static {
        DEFAULT_DURATIONS.put(PhysicalElement.AGRICULTURAL_RESERVED,
                Duration.DEFAULT);
        DEFAULT_DURATIONS.put(PhysicalElement.AGRICULTURAL_TIME_BELOW_25F,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.AGRICULTURAL_TIME_BELOW_32F,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.AGRICULTURAL_LEAF_WETNESS,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.EVAPORATION_POTENTIAL_AMOUNT,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.EVAPORATION_AMOUNT,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.EVAPORATION_PAN_INCREMENT,
                Duration._1_DAY);
        DEFAULT_DURATIONS
                .put(PhysicalElement.EVAPORATION_RATE, Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.EVAPORATION_TOTAL,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.EVAPORATION_LAKE_COMPUTED,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.LAKE_STORAGE_VOLUME_CHANGE,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.PRECIPITATION_INCREMENT,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.PRECIPITATION_RATE,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.DISCHARGE_RUNOFF_VOLUME,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(
                PhysicalElement.DISCHARGE_CUMULATIVE_VOLUME_INCREMENT,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.RADIATION_ACCUMULATED_SOLAR,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.RADIATION_SUNSHINE_PERCENT,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.RADIATION_SUNSHINE_HOURS,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.SNOW_NEW_SNOWFALL,
                Duration._1_DAY);

        DEFAULT_DURATIONS.put(PhysicalElement.TEMPERATURE_COOLING,
                Duration.SEASONAL_PERIOD);
        DEFAULT_DURATIONS.put(PhysicalElement.TEMPERATURE_FREEZING,
                Duration.SEASONAL_PERIOD);
        DEFAULT_DURATIONS.put(PhysicalElement.TEMPERATURE_HEATING,
                Duration.SEASONAL_PERIOD);

        DEFAULT_DURATIONS.put(PhysicalElement.WIND_ACCUMULATED_TRAVEL,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.WIND_TRAVEL_LENGTH,
                Duration._1_DAY);
        DEFAULT_DURATIONS.put(PhysicalElement.WEATHER_LIGHTENING_GRID,
                Duration._30_MINUTES);
        DEFAULT_DURATIONS.put(PhysicalElement.WEATHER_SYNOPTIC_CODE_PAST,
                Duration._6_HOUR);
    }

    /**
     * Note that these are defined "meta" types. TODO Add Description
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Mar 9, 2011            jkorman     Initial creation
     * 
     * </pre>
     * 
     * @author jkorman
     * @version 1.0
     */
    public enum DataType {
        READING, CONTINGENCY, FORECAST, AREAL_FORECAST, PROCESSED, AREAL_PROCESSED, UNKNOWN;

        /**
         * Get the metatype based on the Type Source code and flag.
         * 
         * @param ts
         * @param procObs
         * @return
         */
        public static DataType getDataType(TypeSource ts, boolean procObs) {
            DataType type = null;

            // Don't use the TypeSource directly because there are some cases
            // where the "type" defaults. (See the last else clause)
            String dType = ts.getCode().substring(0, 1);
            String dSrc = ts.getCode().substring(1, 2);
            if ("R".equals(dType)) {
                type = READING;
            } else if ("F".equals(dType)) {
                if ("L".equals(dSrc)) {
                    type = AREAL_FORECAST;
                } else {
                    type = FORECAST;
                }
            } else if ("C".equals(dType)) {
                type = CONTINGENCY;
            } else {
                if (TypeSource.PROCESSED_MEAN_AREAL_DATA.equals(ts)) {
                    type = AREAL_PROCESSED;
                } else {
                    if (procObs) {
                        type = READING;
                    } else {
                        type = PROCESSED;
                    }
                }
            }
            return type;
        }
    }

    /**
     * Describes the source of the data.
     * <p>
     * The type character is used to describe the basic category of the
     * hydrometeoro- logical data being transmitted. These types include
     * contingency, forecast, historical, observed, and processed. Observed and
     * forecast type codes are relatively self- explanatory. The processed type
     * code is intended for nonforecast-derived values from the observed
     * database, such as mean areal precipitation or temperature. In most
     * applications, the processed parameters would not be used for external
     * transmission of data. Contingency types deal with possible results gained
     * for various hydrometeorological input scenarios. Historical values (such
     * as peaks of record) are not normally sent real-time via external
     * transmission, but the code is designed to accommodate consistent
     * descriptions of real-time and historical information in user databases.
     * <p>
     * The source character is a further refinement of the type code which may
     * indicate how the hydrometeorological data was created or transmitted.
     * 
     * 
     */
    public static enum TypeSource {

        /** Contingency 1 */
        CONTINGENCY_1("C1"),
        /** Contingency 2 */
        CONTINGENCY_2("C2"),
        /** Contingency 3 */
        CONTINGENCY_3("C3"),
        /** Contingency 4 */
        CONTINGENCY_4("C4"),
        /** Contingency 5 */
        CONTINGENCY_5("C5"),
        /** Contingency 6 */
        CONTINGENCY_6("C6"),
        /** Contingency 7 */
        CONTINGENCY_7("C7"),
        /** Contingency 8 */
        CONTINGENCY_8("C8"),
        /** Contingency 9 */
        CONTINGENCY_9("C9"),
        /** Contingency A */
        CONTINGENCY_A("CA"),
        /** Contingency B */
        CONTINGENCY_B("CB"),
        /** Contingency C */
        CONTINGENCY_C("CC"),
        /** Contingency D */
        CONTINGENCY_D("CD"),
        /** Contingency E */
        CONTINGENCY_E("CE"),
        /** Contingency for flash flood guidance */
        CONTINGENCY_F("CF"),
        /** Contingency G */
        CONTINGENCY_G("CG"),
        /** Contingency H */
        CONTINGENCY_H("CH"),
        /** Contingency I */
        CONTINGENCY_I("CI"),
        /** Contingency J */
        CONTINGENCY_J("CJ"),
        /** Contingency K */
        CONTINGENCY_K("CK"),
        /** Contingency L */
        CONTINGENCY_L("CL"),
        /** Contingency M */
        CONTINGENCY_M("CM"),
        /** Contingency N */
        CONTINGENCY_N("CN"),
        /** Contingency O */
        CONTINGENCY_O("CO"),
        /** Contingency P */
        CONTINGENCY_P("CP"),
        /** Contingency Q */
        CONTINGENCY_Q("CQ"),
        /** Contingency R */
        CONTINGENCY_R("CR"),
        /** Contingency S */
        CONTINGENCY_S("CS"),
        /** Contingency T */
        CONTINGENCY_T("CT"),
        /** Contingency U */
        CONTINGENCY_U("CU"),
        /** Contingency V */
        CONTINGENCY_V("CV"),
        /** Contingency W */
        CONTINGENCY_W("CW"),
        /** Contingency X */
        CONTINGENCY_X("CX"),
        /** Contingency Y */
        CONTINGENCY_Y("CY"),
        /** Nonspecific contingency (default for this type category) */
        CONTINGENCY_NONSPECIFIC("CZ"),
        /** Adjusted model 1 */
        FORECAST_ADJUSTED_MODEL1("FA"),
        /** Adjusted model 2 */
        FORECAST_ADJUSTED_MODEL2("FB"),
        /** Adjusted model 3 */
        FORECAST_ADJUSTED_MODEL3("FC"),
        /** Adjusted model 4 */
        FORECAST_ADJUSTED_MODEL4("FD"),
        /** Public version, external */
        FORECAST_PUBLIC_VERSION_EXTERNAL("FE"),
        /** Forecast includes QPF */
        FORECAST_INCLUDES_QPF("FF"),
        /** Reservoir release forecast */
        FORECAST_RESERVIOR_RELEASE("FG"),
        /** Forecast Mean Areal Data */
        FORECAST_MEAN_AREAL_DATA("FL"),
        /** Manual method number 1 */
        FORECAST_METHOD_NUMBER1("FM"),
        /** Manual method number 2 */
        FORECAST_METHOD_NUMBER2("FN"),
        /** Manual method number 3 */
        FORECAST_METHOD_NUMBER3("FP"),
        /** Manual method number 4 */
        FORECAST_METHOD_NUMBER4("FQ"),
        /** Persistence forecasts */
        FORECAST_PERSISTENCE("FR"),
        /** Unadjusted model 1 */
        FORECAST_UNADJUSTED_MODEL1("FU"),
        /** Unadjusted model 2 */
        FORECAST_UNADJUSTED_MODEL2("FV"),
        /** Unadjusted model 3 */
        FORECAST_UNADJUSTED_MODEL3("FW"),
        /** Unadjusted model 4 */
        FORECAST_UNADJUSTED_MODEL4("FX"),
        /** Nonspecific forecast data (default for this type category) */
        FORECAST_NONSPECIFIC("FZ"),
        // ***********************
        // Reserved for historical use
        HISTORIC_RESERVED_A("HA"), HISTORIC_RESERVED_B("HB"), HISTORIC_RESERVED_C(
                "HC"), HISTORIC_RESERVED_D("HD"), HISTORIC_RESERVED_E("HE"), HISTORIC_RESERVED_F(
                "HF"), HISTORIC_RESERVED_G("HG"), HISTORIC_RESERVED_H("HH"), HISTORIC_RESERVED_I(
                "HI"), HISTORIC_RESERVED_J("HJ"), HISTORIC_RESERVED_K("HK"), HISTORIC_RESERVED_L(
                "HL"), HISTORIC_RESERVED_M("HM"), HISTORIC_RESERVED_N("HN"), HISTORIC_RESERVED_O(
                "HO"), HISTORIC_RESERVED_P("HP"), HISTORIC_RESERVED_Q("HQ"), HISTORIC_RESERVED_R(
                "HR"), HISTORIC_RESERVED_S("HS"), HISTORIC_RESERVED_T("HT"), HISTORIC_RESERVED_U(
                "HU"), HISTORIC_RESERVED_V("HV"), HISTORIC_RESERVED_W("HW"), HISTORIC_RESERVED_X(
                "HX"), HISTORIC_RESERVED_Y("HY"), HISTORIC_RESERVED_Z("HZ"),

        /** Sacramento Soil Moisture Accounting Model */
        MODEL_SACRAMENTO_SOIL_MOISTURE_ACCOUNTING("MS"),
        /** Continuous Antecedent Precipitation Index (API) Model */
        MODEL_CONTINUOUS_API("MA"),
        /** Kansas City (MBRFC) Event API Model */
        MODEL_KANSAS_CITY_EVENT_API("MK"),
        /** Cincinnati (OHRFC) Event API Model */
        MODEL_CINCINNATI_EVENT_API("MC"),
        /** Harrisburg (MARFC) Event API Model */
        MODEL_HARRISBURG_EVENT_API("MH"),
        /** Hartford (NERFC) Event API Model */
        MODEL_HARTFORD_EVENT_API("MT"),
        /** SNOW-17 Snow Accumulation and Ablation Model */
        MODEL_SNOW17_ACCUMULATION_ABLATION("MW"),
        /** Process #1 */
        PROCESSED_PROCESS1("PA"),
        /** Process #2 */
        PROCESSED_PROCESS2("PB"),
        /** Process #3 */
        PROCESSED_PROCESS3("PC"),
        /** Process #4 */
        PROCESSED_PROCESS4("PD"),
        /** Processed Mean Areal Data */
        PROCESSED_MEAN_AREAL_DATA("PM"),
        /** Process #5 */
        PROCESSED_PROCESS5("PE"),
        /** Process #6 */
        PROCESSED_PROCESS6("PF"),
        /** Process #7 */
        PROCESSED_PROCESS7("PG"),
        /** Process #8 */
        PROCESSED_PROCESS8("PH"),
        /** Process #9 */
        PROCESSED_PROCESS9("PI"),
        /** Process #10 */
        PROCESSED_PROCESS10("PJ"),
        /** Process #11 */
        PROCESSED_PROCESS11("PK"),
        /** Process #12 */
        PROCESSED_PROCESS12("PL"),
        /** Process #13 */
        PROCESSED_PROCESS13("PN"),
        /** Process #14 */
        PROCESSED_PROCESS14("PO"),
        /** Process #15 */
        PROCESSED_PROCESS15("PP"),
        /** Process #16 */
        PROCESSED_PROCESS16("PQ"),
        /** Process #17 */
        PROCESSED_PROCESS17("PR"),
        /** Process #18 */
        PROCESSED_PROCESS18("PS"),
        /** Process #19 */
        PROCESSED_PROCESS19("PT"),
        /** Process #20 */
        PROCESSED_PROCESS20("PU"),
        /** Process #21 */
        PROCESSED_PROCESS21("PV"),
        /** Process #22 */
        PROCESSED_PROCESS22("PW"),
        /** Process #23 */
        PROCESSED_PROCESS23("PX"),
        /** Process #24 */
        PROCESSED_PROCESS24("PY"),
        /** Process level 1, Best Quality */
        PROCESSED_LEVEL1_GRADE_A("1A"),
        /** Process level 1, 2nd Best */
        PROCESSED_LEVEL1_GRADE_B("1B"),
        /** Process level 1, 3rd Best */
        PROCESSED_LEVEL1_GRADE_C("1C"),
        /** Process level 1, 4th Best */
        PROCESSED_LEVEL1_GRADE_D("1D"),
        /** Process level 1, 2nd Sensor */
        PROCESSED_LEVEL1_SENSOR2("12"),
        /** Process level 1, 3rd Sensor */
        PROCESSED_LEVEL1_SENSOR3("13"),
        /** Process level 1, 4th Sensor */
        PROCESSED_LEVEL1_SENSOR4("14"),
        /** Process level 1, 5th Sensor */
        PROCESSED_LEVEL1_SENSOR5("15"),
        /** Process level 1, 6th Sensor */
        PROCESSED_LEVEL1_SENSOR6("16"),
        /** Process level 1, 7th Sensor */
        PROCESSED_LEVEL1_SENSOR7("17"),
        /** Process level 1, 8th Sensor */
        PROCESSED_LEVEL1_SENSOR8("18"),
        /** Process level 1, 9th Sensor */
        PROCESSED_LEVEL1_SENSOR9("19"),
        /** Process level 1, Airborne */
        PROCESSED_LEVEL1_AIRBORNE("1F"),
        /** Process level 1, GOES */
        PROCESSED_LEVEL1_GOES("1G"),
        /** Process level 1, Meteor burst */
        PROCESSED_LEVEL1_METEOR_BURST("1M"),
        /** Process level 1, Phone (DARDC/LARC) */
        PROCESSED_LEVEL1_PHONE("1P"),
        /** Process level 1, Radio #1 */
        PROCESSED_LEVEL1_RADIO1("1R"),
        /** Process level 1, Radio #2 */
        PROCESSED_LEVEL1_RADIO2("1S"),
        /** Process level 1, Telemark/BDT (phone audio) */
        PROCESSED_LEVEL1_TELEMARK("1T"),
        /** Process level 1, Visual/manual #1 */
        PROCESSED_LEVEL1_VISUAL1("1V"),
        /** Process level 1, Visual/manual #2 */
        PROCESSED_LEVEL1_VISUAL2("1W"),
        /** Process level 1, Visual/manual #3 */
        PROCESSED_LEVEL1_VISUAL3("1X"),
        /** Process level 1, Nonspecific observed reading (default) */
        PROCESSED_LEVEL1_NONSPECIFIC("1Z"),
        /** Process level 2, Best Quality */
        PROCESSED_LEVEL2_GRADE_A("2A"),
        /** Process level 2, 2nd Best */
        PROCESSED_LEVEL2_GRADE_B("2B"),
        /** Process level 2, 3rd Best */
        PROCESSED_LEVEL2_GRADE_C("2C"),
        /** Process level 2, 4th Best */
        PROCESSED_LEVEL2_GRADE_D("2D"),
        /** Process level 2, 2nd Sensor */
        PROCESSED_LEVEL2_SENSOR2("22"),
        /** Process level 2, 3rd Sensor */
        PROCESSED_LEVEL2_SENSOR3("23"),
        /** Process level 2, 4th Sensor */
        PROCESSED_LEVEL2_SENSOR4("24"),
        /** Process level 2, 5th Sensor */
        PROCESSED_LEVEL2_SENSOR5("25"),
        /** Process level 2, 6th Sensor */
        PROCESSED_LEVEL2_SENSOR6("26"),
        /** Process level 2, 7th Sensor */
        PROCESSED_LEVEL2_SENSOR7("27"),
        /** Process level 2, 8th Sensor */
        PROCESSED_LEVEL2_SENSOR8("28"),
        /** Process level 2, 9th Sensor */
        PROCESSED_LEVEL2_SENSOR9("29"),
        /** Process level 2, Airborne */
        PROCESSED_LEVEL2_AIRBORNE("2F"),
        /** Process level 2, GOES */
        PROCESSED_LEVEL2_GOES("2G"),
        /** Process level 2, Meteor burst */
        PROCESSED_LEVEL2_METEOR_BURST("2M"),
        /** Process level 2, Phone (DARDC/LARC) */
        PROCESSED_LEVEL2_PHONE("2P"),
        /** Process level 2, Radio #1 */
        PROCESSED_LEVEL2_RADIO1("2R"),
        /** Process level 2, Radio #2 */
        PROCESSED_LEVEL2_RADIO2("2S"),
        /** Process level 2, Telemark/BDT (phone audio) */
        PROCESSED_LEVEL2_TELEMARK("2T"),
        /** Process level 2, Visual/manual #1 */
        PROCESSED_LEVEL2_VISUAL1("2V"),
        /** Process level 2, Visual/manual #2 */
        PROCESSED_LEVEL2_VISUAL2("2W"),
        /** Process level 2, Visual/manual #3 */
        PROCESSED_LEVEL2_VISUAL3("2X"),
        /** Process level 2, Nonspecific observed reading (default) */
        PROCESSED_LEVEL2_NONSPECIFIC("2Z"),
        /** Process level 3, Best Quality */
        PROCESSED_LEVEL3_GRADE_A("3A"),
        /** Process level 3, 2nd Best */
        PROCESSED_LEVEL3_GRADE_B("3B"),
        /** Process level 3, 3rd Best */
        PROCESSED_LEVEL3_GRADE_C("3C"),
        /** Process level 3, 4th Best */
        PROCESSED_LEVEL3_GRADE_D("3D"),
        /** Process level 3, 2nd Sensor */
        PROCESSED_LEVEL3_SENSOR2("32"),
        /** Process level 3, 3rd Sensor */
        PROCESSED_LEVEL3_SENSOR3("33"),
        /** Process level 3, 4th Sensor */
        PROCESSED_LEVEL3_SENSOR4("34"),
        /** Process level 3, 5th Sensor */
        PROCESSED_LEVEL3_SENSOR5("35"),
        /** Process level 3, 6th Sensor */
        PROCESSED_LEVEL3_SENSOR6("36"),
        /** Process level 3, 7th Sensor */
        PROCESSED_LEVEL3_SENSOR7("37"),
        /** Process level 3, 8th Sensor */
        PROCESSED_LEVEL3_SENSOR8("38"),
        /** Process level 3, 9th Sensor */
        PROCESSED_LEVEL3_SENSOR9("39"),
        /** Process level 3, Airborne */
        PROCESSED_LEVEL3_AIRBORNE("3F"),
        /** Process level 3, GOES */
        PROCESSED_LEVEL3_GOES("3G"),
        /** Process level 3, Meteor burst */
        PROCESSED_LEVEL3_METEOR_BURST("3M"),
        /** Process level 3, Phone (DARDC/LARC) */
        PROCESSED_LEVEL3_PHONE("3P"),
        /** Process level 3, Radio #1 */
        PROCESSED_LEVEL3_RADIO1("3R"),
        /** Process level 3, Radio #2 */
        PROCESSED_LEVEL3_RADIO2("3S"),
        /** Process level 3, Telemark/BDT (phone audio) */
        PROCESSED_LEVEL3_TELEMARK("3T"),
        /** Process level 3, Visual/manual #1 */
        PROCESSED_LEVEL3_VISUAL1("3V"),
        /** Process level 3, Visual/manual #2 */
        PROCESSED_LEVEL3_VISUAL2("3W"),
        /** Process level 3, Visual/manual #3 */
        PROCESSED_LEVEL3_VISUAL3("3X"),
        /** Process level 3, Nonspecific observed reading (default) */
        PROCESSED_LEVEL3_NONSPECIFIC("3Z"),
        /** Process level 4, Best Quality */
        PROCESSED_LEVEL4_GRADE_A("4A"),
        /** Process level 4, 2nd Best */
        PROCESSED_LEVEL4_GRADE_B("4B"),
        /** Process level 4, 3rd Best */
        PROCESSED_LEVEL4_GRADE_C("4C"),
        /** Process level 4, 4th Best */
        PROCESSED_LEVEL4_GRADE_D("4D"),
        /** Process level 4, 2nd Sensor */
        PROCESSED_LEVEL4_SENSOR2("42"),
        /** Process level 4, 3rd Sensor */
        PROCESSED_LEVEL4_SENSOR3("43"),
        /** Process level 4, 4th Sensor */
        PROCESSED_LEVEL4_SENSOR4("44"),
        /** Process level 4, 5th Sensor */
        PROCESSED_LEVEL4_SENSOR5("45"),
        /** Process level 4, 6th Sensor */
        PROCESSED_LEVEL4_SENSOR6("46"),
        /** Process level 4, 7th Sensor */
        PROCESSED_LEVEL4_SENSOR7("47"),
        /** Process level 4, 8th Sensor */
        PROCESSED_LEVEL4_SENSOR8("48"),
        /** Process level 4, 9th Sensor */
        PROCESSED_LEVEL4_SENSOR9("49"),
        /** Process level 4, Airborne */
        PROCESSED_LEVEL4_AIRBORNE("4F"),
        /** Process level 4, GOES */
        PROCESSED_LEVEL4_GOES("4G"),
        /** Process level 4, Meteor burst */
        PROCESSED_LEVEL4_METEOR_BURST("4M"),
        /** Process level 4, Phone (DARDC/LARC) */
        PROCESSED_LEVEL4_PHONE("4P"),
        /** Process level 4, Radio #1 */
        PROCESSED_LEVEL4_RADIO1("4R"),
        /** Process level 4, Radio #2 */
        PROCESSED_LEVEL4_RADIO2("4S"),
        /** Process level 4, Telemark/BDT (phone audio) */
        PROCESSED_LEVEL4_TELEMARK("4T"),
        /** Process level 4, Visual/manual #1 */
        PROCESSED_LEVEL4_VISUAL1("4V"),
        /** Process level 4, Visual/manual #2 */
        PROCESSED_LEVEL4_VISUAL2("4W"),
        /** Process level 4, Visual/manual #3 */
        PROCESSED_LEVEL4_VISUAL3("4X"),
        /** Process level 4, Nonspecific observed reading (default) */
        PROCESSED_LEVEL4_NONSPECIFIC("4Z"),
        /** Process level 5, Best Quality */
        PROCESSED_LEVEL5_GRADE_A("5A"),
        /** Process level 5, 2nd Best */
        PROCESSED_LEVEL5_GRADE_B("5B"),
        /** Process level 5, 3rd Best */
        PROCESSED_LEVEL5_GRADE_C("5C"),
        /** Process level 5, 4th Best */
        PROCESSED_LEVEL5_GRADE_D("5D"),
        /** Process level 5, 2nd Sensor */
        PROCESSED_LEVEL5_SENSOR2("52"),
        /** Process level 5, 3rd Sensor */
        PROCESSED_LEVEL5_SENSOR3("53"),
        /** Process level 5, 4th Sensor */
        PROCESSED_LEVEL5_SENSOR4("54"),
        /** Process level 5, 5th Sensor */
        PROCESSED_LEVEL5_SENSOR5("55"),
        /** Process level 5, 6th Sensor */
        PROCESSED_LEVEL5_SENSOR6("56"),
        /** Process level 5, 7th Sensor */
        PROCESSED_LEVEL5_SENSOR7("57"),
        /** Process level 5, 8th Sensor */
        PROCESSED_LEVEL5_SENSOR8("58"),
        /** Process level 5, 9th Sensor */
        PROCESSED_LEVEL5_SENSOR9("59"),
        /** Process level 5, Airborne */
        PROCESSED_LEVEL5_AIRBORNE("5F"),
        /** Process level 5, GOES */
        PROCESSED_LEVEL5_GOES("5G"),
        /** Process level 5, Meteor burst */
        PROCESSED_LEVEL5_METEOR_BURST("5M"),
        /** Process level 5, Phone (DARDC/LARC) */
        PROCESSED_LEVEL5_PHONE("5P"),
        /** Process level 5, Radio #1 */
        PROCESSED_LEVEL5_RADIO1("5R"),
        /** Process level 5, Radio #2 */
        PROCESSED_LEVEL5_RADIO2("5S"),
        /** Process level 5, Telemark/BDT (phone audio) */
        PROCESSED_LEVEL5_TELEMARK("5T"),
        /** Process level 5, Visual/manual #1 */
        PROCESSED_LEVEL5_VISUAL1("5V"),
        /** Process level 5, Visual/manual #2 */
        PROCESSED_LEVEL5_VISUAL2("5W"),
        /** Process level 5, Visual/manual #3 */
        PROCESSED_LEVEL5_VISUAL3("5X"),
        /** Process level 5, Nonspecific observed reading (default) */
        PROCESSED_LEVEL5_NONSPECIFIC("5Z"),
        /** Process level 6, Best Quality */
        PROCESSED_LEVEL6_GRADE_A("6A"),
        /** Process level 6, 2nd Best */
        PROCESSED_LEVEL6_GRADE_B("6B"),
        /** Process level 6, 3rd Best */
        PROCESSED_LEVEL6_GRADE_C("6C"),
        /** Process level 6, 4th Best */
        PROCESSED_LEVEL6_GRADE_D("6D"),
        /** Process level 6, 2nd Sensor */
        PROCESSED_LEVEL6_SENSOR2("62"),
        /** Process level 6, 3rd Sensor */
        PROCESSED_LEVEL6_SENSOR3("63"),
        /** Process level 6, 4th Sensor */
        PROCESSED_LEVEL6_SENSOR4("64"),
        /** Process level 6, 5th Sensor */
        PROCESSED_LEVEL6_SENSOR5("65"),
        /** Process level 6, 6th Sensor */
        PROCESSED_LEVEL6_SENSOR6("66"),
        /** Process level 6, 7th Sensor */
        PROCESSED_LEVEL6_SENSOR7("67"),
        /** Process level 6, 8th Sensor */
        PROCESSED_LEVEL6_SENSOR8("68"),
        /** Process level 6, 9th Sensor */
        PROCESSED_LEVEL6_SENSOR9("69"),
        /** Process level 6, Airborne */
        PROCESSED_LEVEL6_AIRBORNE("6F"),
        /** Process level 6, GOES */
        PROCESSED_LEVEL6_GOES("6G"),
        /** Process level 6, Meteor burst */
        PROCESSED_LEVEL6_METEOR_BURST("6M"),
        /** Process level 6, Phone (DARDC/LARC) */
        PROCESSED_LEVEL6_PHONE("6P"),
        /** Process level 6, Radio #1 */
        PROCESSED_LEVEL6_RADIO1("6R"),
        /** Process level 6, Radio #2 */
        PROCESSED_LEVEL6_RADIO2("6S"),
        /** Process level 6, Telemark/BDT (phone audio) */
        PROCESSED_LEVEL6_TELEMARK("6T"),
        /** Process level 6, Visual/manual #1 */
        PROCESSED_LEVEL6_VISUAL1("6V"),
        /** Process level 6, Visual/manual #2 */
        PROCESSED_LEVEL6_VISUAL2("6W"),
        /** Process level 6, Visual/manual #3 */
        PROCESSED_LEVEL6_VISUAL3("6X"),
        /** Process level 6, Nonspecific observed reading (default) */
        PROCESSED_LEVEL6_NONSPECIFIC("6Z"),
        /** Process level 7, Best Quality */
        PROCESSED_LEVEL7_GRADE_A("7A"),
        /** Process level 7, 2nd Best */
        PROCESSED_LEVEL7_GRADE_B("7B"),
        /** Process level 7, 3rd Best */
        PROCESSED_LEVEL7_GRADE_C("7C"),
        /** Process level 7, 4th Best */
        PROCESSED_LEVEL7_GRADE_D("7D"),
        /** Process level 7, 2nd Sensor */
        PROCESSED_LEVEL7_SENSOR2("72"),
        /** Process level 7, 3rd Sensor */
        PROCESSED_LEVEL7_SENSOR3("73"),
        /** Process level 7, 4th Sensor */
        PROCESSED_LEVEL7_SENSOR4("74"),
        /** Process level 7, 5th Sensor */
        PROCESSED_LEVEL7_SENSOR5("75"),
        /** Process level 7, 6th Sensor */
        PROCESSED_LEVEL7_SENSOR6("76"),
        /** Process level 7, 7th Sensor */
        PROCESSED_LEVEL7_SENSOR7("77"),
        /** Process level 7, 8th Sensor */
        PROCESSED_LEVEL7_SENSOR8("78"),
        /** Process level 7, 9th Sensor */
        PROCESSED_LEVEL7_SENSOR9("79"),
        /** Process level 7, Airborne */
        PROCESSED_LEVEL7_AIRBORNE("7F"),
        /** Process level 7, GOES */
        PROCESSED_LEVEL7_GOES("7G"),
        /** Process level 7, Meteor burst */
        PROCESSED_LEVEL7_METEOR_BURST("7M"),
        /** Process level 7, Phone (DARDC/LARC) */
        PROCESSED_LEVEL7_PHONE("7P"),
        /** Process level 7, Radio #1 */
        PROCESSED_LEVEL7_RADIO1("7R"),
        /** Process level 7, Radio #2 */
        PROCESSED_LEVEL7_RADIO2("7S"),
        /** Process level 7, Telemark/BDT (phone audio) */
        PROCESSED_LEVEL7_TELEMARK("7T"),
        /** Process level 7, Visual/manual #1 */
        PROCESSED_LEVEL7_VISUAL1("7V"),
        /** Process level 7, Visual/manual #2 */
        PROCESSED_LEVEL7_VISUAL2("7W"),
        /** Process level 7, Visual/manual #3 */
        PROCESSED_LEVEL7_VISUAL3("7X"),
        /** Process level 7, Nonspecific observed reading (default) */
        PROCESSED_LEVEL7_NONSPECIFIC("7Z"),
        /** Process level 8, Best Quality */
        PROCESSED_LEVEL8_GRADE_A("8A"),
        /** Process level 8, 2nd Best */
        PROCESSED_LEVEL8_GRADE_B("8B"),
        /** Process level 8, 3rd Best */
        PROCESSED_LEVEL8_GRADE_C("8C"),
        /** Process level 8, 4th Best */
        PROCESSED_LEVEL8_GRADE_D("8D"),
        /** Process level 8, 2nd Sensor */
        PROCESSED_LEVEL8_SENSOR2("82"),
        /** Process level 8, 3rd Sensor */
        PROCESSED_LEVEL8_SENSOR3("83"),
        /** Process level 8, 4th Sensor */
        PROCESSED_LEVEL8_SENSOR4("84"),
        /** Process level 8, 5th Sensor */
        PROCESSED_LEVEL8_SENSOR5("85"),
        /** Process level 8, 6th Sensor */
        PROCESSED_LEVEL8_SENSOR6("86"),
        /** Process level 8, 7th Sensor */
        PROCESSED_LEVEL8_SENSOR7("87"),
        /** Process level 8, 8th Sensor */
        PROCESSED_LEVEL8_SENSOR8("88"),
        /** Process level 8, 9th Sensor */
        PROCESSED_LEVEL8_SENSOR9("89"),
        /** Process level 8, Airborne */
        PROCESSED_LEVEL8_AIRBORNE("8F"),
        /** Process level 8, GOES */
        PROCESSED_LEVEL8_GOES("8G"),
        /** Process level 8, Meteor burst */
        PROCESSED_LEVEL8_METEOR_BURST("8M"),
        /** Process level 8, Phone (DARDC/LARC) */
        PROCESSED_LEVEL8_PHONE("8P"),
        /** Process level 8, Radio #1 */
        PROCESSED_LEVEL8_RADIO1("8R"),
        /** Process level 8, Radio #2 */
        PROCESSED_LEVEL8_RADIO2("8S"),
        /** Process level 8, Telemark/BDT (phone audio) */
        PROCESSED_LEVEL8_TELEMARK("8T"),
        /** Process level 8, Visual/manual #1 */
        PROCESSED_LEVEL8_VISUAL1("8V"),
        /** Process level 8, Visual/manual #2 */
        PROCESSED_LEVEL8_VISUAL2("8W"),
        /** Process level 8, Visual/manual #3 */
        PROCESSED_LEVEL8_VISUAL3("8X"),
        /** Process level 8, Nonspecific observed reading (default) */
        PROCESSED_LEVEL8_NONSPECIFIC("8Z"),
        /** Process level 9, Best Quality */
        PROCESSED_LEVEL9_GRADE_A("9A"),
        /** Process level 9, 2nd Best */
        PROCESSED_LEVEL9_GRADE_B("9B"),
        /** Process level 9, 3rd Best */
        PROCESSED_LEVEL9_GRADE_C("9C"),
        /** Process level 9, 4th Best */
        PROCESSED_LEVEL9_GRADE_D("9D"),
        /** Process level 9, 2nd Sensor */
        PROCESSED_LEVEL9_SENSOR2("92"),
        /** Process level 9, 3rd Sensor */
        PROCESSED_LEVEL9_SENSOR3("93"),
        /** Process level 9, 4th Sensor */
        PROCESSED_LEVEL9_SENSOR4("94"),
        /** Process level 9, 5th Sensor */
        PROCESSED_LEVEL9_SENSOR5("95"),
        /** Process level 9, 6th Sensor */
        PROCESSED_LEVEL9_SENSOR6("96"),
        /** Process level 9, 7th Sensor */
        PROCESSED_LEVEL9_SENSOR7("97"),
        /** Process level 9, 8th Sensor */
        PROCESSED_LEVEL9_SENSOR8("98"),
        /** Process level 9, 9th Sensor */
        PROCESSED_LEVEL9_SENSOR9("99"),
        /** Process level 9, Airborne */
        PROCESSED_LEVEL9_AIRBORNE("9F"),
        /** Process level 9, GOES */
        PROCESSED_LEVEL9_GOES("9G"),
        /** Process level 9, Meteor burst */
        PROCESSED_LEVEL9_METEOR_BURST("9M"),
        /** Process level 9, Phone (DARDC/LARC) */
        PROCESSED_LEVEL9_PHONE("9P"),
        /** Process level 9, Radio #1 */
        PROCESSED_LEVEL9_RADIO1("9R"),
        /** Process level 9, Radio #2 */
        PROCESSED_LEVEL9_RADIO2("9S"),
        /** Process level 9, Telemark/BDT (phone audio) */
        PROCESSED_LEVEL9_TELEMARK("9T"),
        /** Process level 9, Visual/manual #1 */
        PROCESSED_LEVEL9_VISUAL1("9V"),
        /** Process level 9, Visual/manual #2 */
        PROCESSED_LEVEL9_VISUAL2("9W"),
        /** Process level 9, Visual/manual #3 */
        PROCESSED_LEVEL9_VISUAL3("9X"),
        /** Process level 9, Nonspecific observed reading (default) */
        PROCESSED_LEVEL9_NONSPECIFIC("9Z"),

        /** Nonspecific processed data (default for this type category) */
        PROCESSED_NONSPECIFIC("PZ"),
        /**
         * For secondary, tertiary, etc. sensors of the same data type at the
         * same station location following the primary sensor of type RF, RG,
         * RM, RP, RR, RS, RT, RV, RW, RX, and RZ. For example, the primary,
         * secondary, and tertiary sensors of the same data type at a GOES
         * station would be encoded with SHEF Type and Source Codes of RG, R2,
         * and R3 respectively.
         */
        READING_SENSOR2("R2"),
        /**
         * For secondary, tertiary, etc. sensors of the same data type at the
         * same station location following the primary sensor of type RF, RG,
         * RM, RP, RR, RS, RT, RV, RW, RX, and RZ. For example, the primary,
         * secondary, and tertiary sensors of the same data type at a GOES
         * station would be encoded with SHEF Type and Source Codes of RG, R2,
         * and R3 respectively.
         */
        READING_SENSOR3("R3"),
        /**
         * For secondary, tertiary, etc. sensors of the same data type at the
         * same station location following the primary sensor of type RF, RG,
         * RM, RP, RR, RS, RT, RV, RW, RX, and RZ. For example, the primary,
         * secondary, and tertiary sensors of the same data type at a GOES
         * station would be encoded with SHEF Type and Source Codes of RG, R2,
         * and R3 respectively.
         */
        READING_SENSOR4("R4"),
        /**
         * For secondary, tertiary, etc. sensors of the same data type at the
         * same station location following the primary sensor of type RF, RG,
         * RM, RP, RR, RS, RT, RV, RW, RX, and RZ. For example, the primary,
         * secondary, and tertiary sensors of the same data type at a GOES
         * station would be encoded with SHEF Type and Source Codes of RG, R2,
         * and R3 respectively.
         */
        READING_SENSOR5("R5"),
        /**
         * For secondary, tertiary, etc. sensors of the same data type at the
         * same station location following the primary sensor of type RF, RG,
         * RM, RP, RR, RS, RT, RV, RW, RX, and RZ. For example, the primary,
         * secondary, and tertiary sensors of the same data type at a GOES
         * station would be encoded with SHEF Type and Source Codes of RG, R2,
         * and R3 respectively.
         */
        READING_SENSOR6("R6"),
        /**
         * For secondary, tertiary, etc. sensors of the same data type at the
         * same station location following the primary sensor of type RF, RG,
         * RM, RP, RR, RS, RT, RV, RW, RX, and RZ. For example, the primary,
         * secondary, and tertiary sensors of the same data type at a GOES
         * station would be encoded with SHEF Type and Source Codes of RG, R2,
         * and R3 respectively.
         */
        READING_SENSOR7("R7"),
        /**
         * For secondary, tertiary, etc. sensors of the same data type at the
         * same station location following the primary sensor of type RF, RG,
         * RM, RP, RR, RS, RT, RV, RW, RX, and RZ. For example, the primary,
         * secondary, and tertiary sensors of the same data type at a GOES
         * station would be encoded with SHEF Type and Source Codes of RG, R2,
         * and R3 respectively.
         */
        READING_SENSOR8("R8"),
        /**
         * For secondary, tertiary, etc. sensors of the same data type at the
         * same station location following the primary sensor of type RF, RG,
         * RM, RP, RR, RS, RT, RV, RW, RX, and RZ. For example, the primary,
         * secondary, and tertiary sensors of the same data type at a GOES
         * station would be encoded with SHEF Type and Source Codes of RG, R2,
         * and R3 respectively.
         */
        READING_SENSOR9("R9"),
        /** Best quality (retrieve code, not for transmission) */
        READING_GRADE_A("RA"),
        /** 2nd best (retrieve code, not for transmission) */
        READING_GRADE_B("RB"),
        /** 3rd best (retrieve code, not for transmission) */
        READING_GRADE_C("RC"),
        /** 4th best (retrieve code, not for transmission) */
        READING_GRADE_D("RD"),
        /** Airborne */
        READING_AIRBORNE("RF"),
        /** GOES */
        READING_GOES("RG"),
        /** Meteor burst */
        READING_METEOR_BURST("RM"),
        /** Phone ASCII (DARDC/LARC) */
        READING_PHONE("RP"),
        /** Radio #1 */
        READING_RADIO1("RR"),
        /** Radio #2 */
        READING_RADIO2("RS"),
        /** Telemark/BDT (phone audio) */
        READING_TELEMARK("RT"),
        /** Visual/manual #1 */
        READING_VISUAL1("RV"),
        /** Visual/manual #2 */
        READING_VISUAL2("RW"),
        /** Visual/manual #3 */
        READING_VISUAL3("RX"),
        /**
         * Nonspecific observed reading (default for this category and universal
         * default for type/source)
         */
        READING_NONSPECIFIC("RZ"),

        UNKNOWN;

        private String code;

        private static Map<String, TypeSource> map;

        TypeSource() {

        }

        TypeSource(String code) {
            this.code = code;
        }

        public String getCode() {
            return this.code;
        }

        public DataType getType() {
            return DataType.getDataType(this, false);
        }

        public String getSource() {
            return this.code.substring(1, 2);
        }

        public static TypeSource getEnum(String code) {
            if (code.length() == 2) {
                if (code.charAt(0) == 'Z') {
                    code = "R" + code.charAt(1);
                }
            }

            if (map == null) {
                createMap();
            }
            TypeSource ts = map.get(code);
            if (ts != null) {
                return ts;
            }

            return UNKNOWN;
        }

        private static void createMap() {
            map = new HashMap<String, TypeSource>();
            for (TypeSource ts : TypeSource.values()) {
                map.put(ts.getCode(), ts);
            }
        }
    }

    /**
     * This single-character descriptor allows identification of maximum and
     * minimum values, such as crest or minimum stages.
     * 
     */
    public static enum Extremum {

        MIN_RECORD("J"),

        MIN_YEAR("K"),

        MIN_MONTH("L"),

        MIN_WEEK("M"),

        MIN_DAY("N"),

        MIN_1_HOUR("F"),

        MIN_3_HOURS("G"),

        MIN_6_HOURS("H"),

        MIN_12_HOURS("P"),

        MIN_18_HOURS("I"),

        MAX_RECORD("T"),

        MAX_YEAR("U"),

        MAX_MONTH("V"),

        MAX_WEEK("W"),

        MAX_DAY("X"),

        MAX_1_HOUR("D"),

        MAX_3_HOURS("E"),

        MAX_6_HOURS("R"),

        MAX_12_HOURS("Y"),

        MAX_18_HOURS("S"),

        NULL("Z"),

        UNKNOWN;

        private static Map<String, Extremum> map;

        static {
            map = Collections.unmodifiableMap(createMap());
        }

        private String code;

        Extremum() {
        }

        Extremum(String code) {
            this.code = code;
        }

        public String getCode() {
            return code;
        }

        public static Extremum getEnum(String code) {
            Extremum e = map.get(code);
            if (e != null) {
                return e;
            }
            return UNKNOWN;
        }

        private static Map<String, Extremum> createMap() {
            Map<String, Extremum> map = new HashMap<String, Extremum>();
            for (Extremum e : Extremum.values()) {
                map.put(e.getCode(), e);
            }
            return map;
        }
    }

    /**
     * The probability character descriptor is a key element in identifying both
     * input and products of forecast procedures.
     * <p>
     * Underscores are used in place of '.'
     * 
     */
    public static enum Probability {

        /** Chance value is at or below the specified value */
        _002("A", 0.002),

        /** Chance value is at or below the specified value */
        _004("B", 0.004),

        /** Chance value is at or below the specified value */
        _01("C", 0.01),

        /** Chance value is at or below the specified value */
        _02("D", 0.02),

        /** Chance value is at or below the specified value */
        _04("E", 0.04),

        /** Chance value is at or below the specified value */
        _05("F", 0.05),

        /** Chance value is at or below the specified value */
        _1("1", 0.1),

        /** Chance value is at or below the specified value */
        _2("2", 0.2),

        /** Chance value is at or below the specified value */
        _25("G", 0.25),

        /** Chance value is at or below the specified value */
        _3("3", 0.3),

        /** Chance value is at or below the specified value */
        _4("4", 0.4),

        /** Chance value is at or below the specified value */
        _5("5", 0.5),

        /** Chance value is at or below the specified value */
        _6("6", 0.6),

        /** Chance value is at or below the specified value */
        _7("7", 0.7),

        /** Chance value is at or below the specified value */
        _75("H", 0.75),

        /** Chance value is at or below the specified value */
        _8("8", 0.8),

        /** Chance value is at or below the specified value */
        _9("9", 0.9),

        /** Chance value is at or below the specified value */
        _95("T", 0.95),

        /** Chance value is at or below the specified value */
        _96("U", 0.96),

        /** Chance value is at or below the specified value */
        _98("V", 0.98),

        /** Chance value is at or below the specified value */
        _99("W", 0.99),

        /** Chance value is at or below the specified value */
        _996("X", 0.996),

        /** Chance value is at or below the specified value */
        _998("Y", 0.998),

        /** Chance value below specified: -3 standard deviations */
        _0013("J", 0.0013),

        /** Chance value below specified: -2 standard deviations */
        _0228("K", 0.0228),

        /** Chance value below specified: -1 standard deviations */
        _1587("L", 0.1587),

        /** Expected value */
        MEAN("M", -0.5),

        /** Chance value below specified: +1 standard deviations */
        _8413("N", 0.8413),

        /** Chance value below specified: +2 standard deviations */
        _9772("P", 0.9772),

        /** Chance value below specified: +3 standard deviations */
        _9987("Q", 0.9987),

        NULL("Z", -1.0),

        UNKNOWN;

        private static Map<String, Probability> map;
        static {
            map = Collections.unmodifiableMap(createMap());
        }

        private String code;

        private double value;

        Probability() {

        }

        Probability(String code, double value) {
            this.code = code;
            this.value = value;
        }

        public String getCode() {
            return code;
        }

        public double getValue() {
            return value;
        }

        public static Probability getEnum(String code) {
            Probability p = map.get(code);
            if (p != null) {
                return p;
            }
            return UNKNOWN;
        }

        private static Map<String, Probability> createMap() {
            Map<String, Probability> map = new HashMap<String, Probability>();
            for (Probability p : Probability.values()) {
                map.put(p.getCode(), p);
            }
            return map;
        }
    }
}
