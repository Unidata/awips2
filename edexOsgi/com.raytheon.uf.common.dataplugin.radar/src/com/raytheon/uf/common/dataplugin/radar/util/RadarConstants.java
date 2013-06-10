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
package com.raytheon.uf.common.dataplugin.radar.util;

import java.util.regex.Pattern;

/**
 * Class holding all the constants for radar, including all the tabular block
 * regular expressions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2009            mnash     Initial creation
 * 03/04/2013   DCS51      zwang     Add a map type for GFM product
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarConstants {

    /*
     * The available products in the tabular block in the map
     */
    public enum MapValues {
        DMD_TYPE, RCM_TYPE, STI_TYPE, MESO_TYPE, LEG_MESO_TYPE, TVS_TYPE, VAD_TYPE, HAIL_TYPE, STORM_ID, DMD_ELEV_TIMES(
                "elev_time"), DMD_ELEV_ANGLES("elev_angle"), DMD_LAST_ELEV(
                "last_elev_flag"), DMD_AVG_SPD("avg_spd"), DMD_AVG_DIR(
                "avg_dir"), HI_MAX_HAIL_SIZE, STI_AVG_SPEED, STI_AVG_DIRECTION, HI_POSH, HI_POH, STI_AZIMUTH_DIRECTION, STI_AZIMUTH_RANGE, STI_MOVEMENT_DIRECTION, STI_MOVEMENT_SPEED, STI_FORECAST_15_DIRECTION, STI_FORECAST_15_RANGE, STI_FORECAST_30_DIRECTION, STI_FORECAST_30_RANGE, STI_FORECAST_45_DIRECTION, STI_FORECAST_45_RANGE, STI_FORECAST_60_DIRECTION, STI_FORECAST_60_RANGE, STI_ERROR_FCST, STI_ERROR_MEAN, STI_DEF_DIR, STI_DEF_SPD, STI_MAX_TIME, STI_NUM_PAST_VOLS, STI_CORR_SPD, STI_THRESH_MIN_SPD, STI_ALLOW_ERROR, STI_FORECAST_INT, STI_NUM_INT, STI_ERROR_INT, MESO_STORM_ID, MESO_FEATURE_TYPE, MESO_BASE_KFT, MESO_TOP_KFT, MESO_AZIMUTH_DIRECTION, MESO_AZIMUTH_RANGE, MESO_HGT, MESO_DIAM_RAD, MESO_DIAM_AZ, MESO_SHEAR, VAD_U, VAD_W, VAD_V, VAD_DIR, VAD_SPD, VAD_RMS, VAD_DIV, VAD_SRNG, VAD_ELEV, VAD_ANALYSIS_SLANT_RNG, VAD_BEGIN_AZIMUTH_ANGLE, VAD_END_AZIMUTH_ANGLE, VAD_NUM_PASSES, VAD_RMS_THRESHOLD, TVS_FEATURE_TYPE, TVS_AZIMUTH, TVS_RANGE, TVS_AVGDV, TVS_LLDV, TVS_MXDV, TVS_DVHGT, TVS_DEPTH, TVS_BASE, TVS_TOP, TVS_MXSHR, TVS_SHRHGT, TVS_MIN_REFLECT, TVS_CIRC_RADIUS_1, TVS_CIRC_RADIUS_2, TVS_VECTOR_VEL_DIFF, TVS_MAX_PATTERN_VEC_RANGE, TVS_CIRC_RADIUS_RANGE, TVS_MAX_PATTER_VECTOR_HEIGHT, TVS_MAX_2D_FEATURES, TVS_MAX_NUM_PATTERN_VECTORS, TVS_MIN_2D_FEATURES, TVS_DIFF_VEL_1, TVS_DIFF_VEL_2, TVS_DIFF_VEL_3, TVS_DIFF_VEL_4, TVS_DIFF_VEL_5, TVS_DIFF_VEL_6, TVS_MIN_3D_FEATURE_DEPTH, TVS_MIN_3D_FEATURE_LOW_LVL_DELTA, TVS_MIN_TVS_DELTA_VEL, TVS_MAX_3D_FEATURES, TVS_MAX_TVS, TVS_MAX_ELEVATED_TVS, TVS_MIN_VECTORS, TVS_MIN_TVS_BASE_HEIGHT, TVS_2D_VECTOR_RADIAL_DISTANCE, TVS_MIN_TVS_ELEVATION, TVS_2D_VECTOR_AZIMUTHAL_DIST, TVS_MIN_AVG_DELTA_VEL_HEIGHT, TVS_2D_FEATURE_ASPECT_RATIO, TVS_MAX_STORM_ASSOCIATION_DIST, MESO_CIRC_ID, MESO_SR, MESO_RV, MESO_DV, MESO_BASE, MESO_DEPTH, MESO_DEPTH_PERCENT, MESO_MAX_RV, MESO_MAX_RV_SPD, MESO_TVS_TYPE, MESO_MOTION_DIR, MESO_MOTION_SPD, MESO_MSI, RCM_RDA_SITE, RCM_TIME, RCM_PROD_CAT, RCM_OPER_MODE, RCM_SCAN_STRAT, RCM_TOT_INTENS, RCM_REFL_STRING, RCM_MT_HGT, RCM_MT_LOC, RCM_NCEN, RCM_CENTROIDS, VAD_WINDS, RCM_NUM_TVS, RCM_NUM_MESO, RCM_NUM_CENTROIDS, RCM_CENT, RCM_MESO, RCM_TVS, GFM_TYPE;

        private String name;

        private MapValues() {
            this.name = "";
        }

        private MapValues(String name) {
            this.name = name;
        }

        /**
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * @param name
         *            the name to set
         */
        public void setName(String name) {
            this.name = name;
        }

    }

    public enum GraphicBlockValues {
        AZIMUTH, RANGE, TVS, MDA, POSH, POH, MXHAILSIZE, VIL, DBZM, HT, TOP, FCSTDIR, FCSTRAN;
    }

    public enum DHRValues {
        BEAMWIDTH, BLOCKAGETHRESHOLD, CLUTTERTHRESHOLD, WEIGHTTHRESHOLD, FULLHYBRIDSCANTHRESH, LOWREFLTHRESHOLD, RAINDETREFLTHRESHOLD, RAINDETAREATHRESHOLD, RAINDETTIMETHRESHOLD, ZRMULTCOEFF, ZRPOWERCOEFF, MINREFLTORATE, MAXREFLTORATE, NUMEXCLZONE, RANGECUTOFF, RANGEEFFCOEFF1, RANGEEFFCOEFF2, RANGEEFFCOEFF3, MINPRECIPRATEINCL, MAXPRECIPRATEALLOW, THRESHELAPSEDTIME, MAXTIMEFORINTERP, MINTIMEHOURLYPERIOD, THRESHOLDHROUTLIER, ENDTIMEGAGEACCUM, MAXPERIODACCUMVAL, MAXHOURLYACCUMVAL, TIMEBIASEST, THRESHNOGAGERADAR, RESETBIASVALUE, LONGESTALLOWLAG, BIASAPPLIEDFLAG, PRECIPCAT, BIAS, FLAGZEROHYBRID, HAVE_PRECIP, BIAS_TO_USE;
    }

    public static final String PLUGIN_NAME = "radar";

    public static final String PLUGIN_ID = "com.raytheon.uf.common.dataplugin.radar";

    /*
     * For breaking the STI header out and being able to use the regex below
     */
    public static final String STI_HEADER = "(NM)";

    /*
     * For breaking the STI header into two parts and matching the correct
     * things for storage in the radar record
     */
    public static final String STI_VALUES = "AVG SPEED\\W+(.\\d+)\\W+\\w+\\W+AVG DIRECTION\\W+(.\\d+)\\W+\\w+";

    /*
     * For parsing the tabular block for the sti product
     */
    public static final String STI_REGEX = "\\s+(.\\w+)\\s+(.{7})\\s+(.{7})\\s+(.{7})\\s+(.{7})\\s+(.{7})\\s+(.{7})\\s+(.{8})\\s+(.{8})";

    /*
     * For breaking the HI header out and being able to use the regex below
     */
    public static final String HAIL_HEADER = "(IN)";

    /*
     * For parsing the tabular block for the hail product
     */
    public static final String HAIL_REGEX = "\\s+(.\\w+)\\s+(.\\w+)\\s+(.\\w+)\\s+(.{7})";

    /*
     * For breaking the legacy MESO header out and being able to use the regex
     * below
     */
    public static final String LEGACY_MESO_HEADER = "(E-3/S)";

    /*
     * For parsing the tabular block for the legacy meso product
     */
    public static final String LEGACY_MESO_REGEX = "(.{1})\\s+-\\s+(.{2})\\s+(.{7})\\s+(.{4})\\s+(.{4})\\s+(.{3})/(.{3})\\s+(.{4})\\s+(.{3})\\s+(.{3})\\s+(.{2})";

    /*
     * For breaking the MESO header out and being able to use the regex below
     */
    public static final String MESO_HEADER = "deg/kts";

    /*
     * For parsing the tabular block for the meso product
     */
    public static final String MESO_REGEX = "\\s*(.\\w+)\\s+(.{7})\\s+(.\\w+)\\s+(.{2})\\s+(.{2})\\s+(.{3})\\s+(.{3})\\s+(.{3})\\s+(.\\w+)\\s+(.\\w+)\\s+(.\\w+)\\s+(.\\w+)\\s\\s(.{7})\\s+(.{5})";

    /*
     * For breaking the MESO header out and being able to use the regex below
     */
    public static final String VAD_HEADER = "nm\\s+deg";

    /*
     * For parsing the tabular block for the vad product
     */
    public static final String VAD_REGEX = "([-+]?[0-9]*\\.?[0-9]+|NA)\\s+([-+]?[0-9]*\\.?[0-9]+|NA)\\s+([-+]?[0-9]*\\.?[0-9]+|NA)\\s+([-+]?[0-9]*\\.?[0-9]+|NA)\\s+([-+]?[0-9]*\\.?[0-9]+|NA)\\s+([-+]?[0-9]*\\.?[0-9]+|NA)\\s+([-+]?[0-9]*\\.?[0-9]+|NA)\\s+([-+]?[0-9]*\\.?[0-9]+|NA)\\s+([-+]?[0-9]*\\.?[0-9]+|NA)\\s+([-+]?[0-9]*\\.?[0-9]+|NA)";

    /*
     * For breaking the TVS header out and being able to use the regex below
     */
    public static final String TORNADO_HEADER = "(E-3/s,kft)";

    public static final String TORNADO_REGEX = "\\s+(.\\w+)\\s+(.{2})\\s\\s\\s(.{3})/(.{3})\\s+(.\\w+)\\s+(.\\w+)\\s\\s\\s(.{3})/(.{4})\\s\\s\\s(.{5})\\s\\s(.{5})/(.{5})\\s\\s\\s(.{3})/(.{4})";

    public static final String RCM_REGEX = "1234\\s+(.\\w+)\\s+(.\\w+)\\s+/NEXRAA\\s+(.\\w+)\\s+(.\\w+)\\s+UNEDITED\\s+/MD(.\\w+)*\\s+/SC(.\\w+)*\\s+/NI(.{4})*:\\s+(.*)/MT(.{3}):(.{3})\\s+/NCEN(.{2}):\\s+(.*)\\s*/ENDAA\\s+/NEXRBB\\s+(.\\w+)\\s+(.\\w+)\\s+(.*)/ENDBB\\s+/NEXRCC\\s+(.\\w+)\\s+(.\\w+)\\s+/NTVS(.{2}):\\s+(.*)\\s*/NMES(.{2}):\\s+(.*)\\s*/NCEN(.{2}):(.*)\\s*";

    public static final String PLACEHOLDER = "((.*)\\s+)*";

    private static final String GRAPHIC_BLOCK = "\\s+(.{2})\\s\\s(.{3})/(.{3})\\s(.{4})\\s(.{4})\\s\\s(.{13})\\s+(.\\w+)\\s+(.\\w+)\\s+(.\\S+)\\s+(.\\S+)\\s\\s(.{7})\\s+";

    public static final Pattern graphic_block_pattern = Pattern.compile(
            GRAPHIC_BLOCK, Pattern.CASE_INSENSITIVE);

    public static final Pattern hail_headerPattern = Pattern.compile(
            HAIL_HEADER, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern hail_productValues = Pattern.compile(
            HAIL_REGEX, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern sti_headerValues = Pattern.compile(STI_VALUES,
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern sti_productValues = Pattern.compile(STI_REGEX,
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern sti_headerPattern = Pattern.compile(STI_HEADER,
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern legacy_meso_headerPattern = Pattern.compile(
            LEGACY_MESO_HEADER, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern meso_headerPattern = Pattern.compile(
            MESO_HEADER, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern legacy_meso_productValues = Pattern.compile(
            LEGACY_MESO_REGEX, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern meso_productValues = Pattern.compile(
            MESO_REGEX, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern vad_headerPattern = Pattern.compile(VAD_HEADER,
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern vad_productValues = Pattern.compile(VAD_REGEX,
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern tvs_headerPattern = Pattern.compile(
            TORNADO_HEADER, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern tvs_productValues = Pattern.compile(
            TORNADO_REGEX, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern rcm_productValues = Pattern.compile(RCM_REGEX,
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final String[] rdaOpStatusStr = {
            "Automatic Calibration Disabled", "Online",
            "Maintenance Action Required", "Maintenance Action Mandatory",
            "Command Shutdown", "Inoperable", null, "Wideband Disconnect",
            "Indeterminate", "Indeterminate", "Indeterminate", "Indeterminate",
            "Indeterminate", "Indeterminate", "Indeterminate", "Indeterminate" };

    public static final String[] rdaStatusStr = { null, "Startup", "Standby",
            "Restart", "Operate", null, "Off-line Operate", null, null, null,
            null, null, null, null, null, null };

    public static final String[] rdaAlarmStr = { "Indeterminate",
            "Tower/Utilities", "Pedestal", "Transmitter",
            "Receiver/Signal Processor", "RDA Control", "RDA Communications" };

    public static final String[] dteStr = { null, "None", "Reflectivity",
            "Velocity", "Spectrum Width" };

    public static final String[] rpgOpStr = { "Loadshed", "Online",
            "Maintenance Action Required", "Maintenance Action Mandatory",
            "Commanded Shutdown", "Indeterminate", "Indeterminate",
            "Indeterminate", "Indeterminate", "Indeterminate", "Indeterminate",
            "Indeterminate", "Indeterminate", "Indeterminate", "Indeterminate",
            "Indeterminate", };

    public static final String[] rpgAlarmStr = { "No Alarms",
            "Node Connectivity", null, "RPG Control Task Failure",
            "Data Base Failure", null, "RPG Input Buffer Loadshed (Wideband)",
            null, "Product Storage Loadshed", null, null, null,
            "RPG/RPG Intercomputer Link Failure", "Redundant Channel Error",
            "Task Failure", "Media Failure" };

    public static final String[] rpgStatusStr = { "Restart", "Operate",
            "Standby", null, "Test Mode", null, null, null, null, null, null,
            null, null, null, null };

    public static final String[] productAvailStr = { "Product Availability",
            "Degraded Availability", "Not Available" };

    public static final String[] requestReponseErrorCode = {
            "No Such Message Code", "No Such Product Code",
            "Product Not Generated (Not Available in Data Base",
            "One-Time Request Generation Process Faulted",
            "Narrowband Loadshed", "Illegal Request", "RPG Memory Loadshed",
            "RPG CPU Loadshed",
            "Unavailability of Slots (Real-Time, Replay, or Customized)",
            "Failure (Task Failed)",
            "Unavailable (Task Not Loaded Upon Startup)",
            "Available Next Volume Scan", "Moment Disabled",
            "Aborted Volume Scan", "Invalid Product Parameters",
            "Product Not Generated (Data Sequence Error)",
            "Task Failure (Self-Terminated)" };

    public static final String[] rpgNarrowbandStatus = {
            "Commanded Disconnect", "Narrowband Loadshed" };
}
