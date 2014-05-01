
package gov.noaa.nws.ncep.edex.plugin.mosaic.util;

import java.util.regex.Pattern;


/**
 * Class holding all the constants for mosaic.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial coding
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

public class MosaicConstants {

    /*
     * The available products in the tabular block in the map
     */
    public enum MapValues {
        STI_TYPE, MESO_TYPE, MESO_VALS_TYPE, TVS_TYPE, TVS_VALS_TYPE, STI_VALS_TYPE, VAD_TYPE, VAD_VALS_TYPE, HAIL_TYPE, STORM_ID, HI_MAX_HAIL_SIZE, STI_AVG_SPEED, STI_AVG_DIRECTION, HI_POSH, HI_POH, STI_AZIMUTH_DIRECTION, STI_AZIMUTH_RANGE, STI_MOVEMENT_DIRECTION, STI_MOVEMENT_SPEED, STI_FORECAST_15_DIRECTION, STI_FORECAST_15_RANGE, STI_FORECAST_30_DIRECTION, STI_FORECAST_30_RANGE, STI_FORECAST_45_DIRECTION, STI_FORECAST_45_RANGE, STI_FORECAST_60_DIRECTION, STI_FORECAST_60_RANGE, STI_ERROR_FCST, STI_ERROR_MEAN, STI_DEF_DIR, STI_DEF_SPD, STI_MAX_TIME, STI_NUM_PAST_VOLS, STI_CORR_SPD, STI_THRESH_MIN_SPD, STI_ALLOW_ERROR, STI_FORECAST_INT, STI_NUM_INT, STI_ERROR_INT, MESO_STORM_ID, MESO_FEATURE_TYPE, MESO_BASE_KFT, MESO_TOP_KFT, MESO_AZIMUTH_DIRECTION, MESO_AZIMUTH_RANGE, MESO_HGT, MESO_DIAM_RAD, MESO_DIAM_AZ, MESO_SHEAR, VAD_U, VAD_W, VAD_V, VAD_DIR, VAD_SPD, VAD_RMS, VAD_DIV, VAD_SRNG, VAD_ELEV, VAD_ANALYSIS_SLANT_RNG, VAD_BEGIN_AZIMUTH_ANGLE, VAD_END_AZIMUTH_ANGLE, VAD_NUM_PASSES, VAD_RMS_THRESHOLD, TVS_FEATURE_TYPE, TVS_AZIMUTH, TVS_RANGE, TVS_AVGDV, TVS_LLDV, TVS_MXDV, TVS_DVHGT, TVS_DEPTH, TVS_BASE, TVS_TOP, TVS_MXSHR, TVS_SHRHGT, TVS_MIN_REFLECT, TVS_CIRC_RADIUS_1, TVS_CIRC_RADIUS_2, TVS_VECTOR_VEL_DIFF, TVS_MAX_PATTERN_VEC_RANGE, TVS_CIRC_RADIUS_RANGE, TVS_MAX_PATTER_VECTOR_HEIGHT, TVS_MAX_2D_FEATURES, TVS_MAX_NUM_PATTERN_VECTORS, TVS_MIN_2D_FEATURES, TVS_DIFF_VEL_1, TVS_DIFF_VEL_2, TVS_DIFF_VEL_3, TVS_DIFF_VEL_4, TVS_DIFF_VEL_5, TVS_DIFF_VEL_6, TVS_MIN_3D_FEATURE_DEPTH, TVS_MIN_3D_FEATURE_LOW_LVL_DELTA, TVS_MIN_TVS_DELTA_VEL, TVS_MAX_3D_FEATURES, TVS_MAX_TVS, TVS_MAX_ELEVATED_TVS, TVS_MIN_VECTORS, TVS_MIN_TVS_BASE_HEIGHT, TVS_2D_VECTOR_RADIAL_DISTANCE, TVS_MIN_TVS_ELEVATION, TVS_2D_VECTOR_AZIMUTHAL_DIST, TVS_MIN_AVG_DELTA_VEL_HEIGHT, TVS_2D_FEATURE_ASPECT_RATIO, TVS_MAX_STORM_ASSOCIATION_DIST
    }

    /*
     * For breaking the STI header out and being able to use the regex below
     */
    public static final String STI_HEADER = "(NM)";

    /*
     * For breaking the STI header into two parts and matching the correct
     * things for storage in the mosaic record
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
    public static final String HAIL_REGEX = "\\s+(.\\w+)\\s+(.\\w+)\\s+(.\\w+)\\s+(.\\w.+)\\s+";

    /*
     * For breaking the MESO header out and being able to use the regex below
     */
    public static final String MESO_HEADER = "(E-3/S)";

    /*
     * For parsing the tabular block for the meso product
     */
    public static final String MESO_REGEX = "(.{1})\\s+-\\s+(.{2})\\s+(.{7})\\s+(.{4})\\s+(.{4})\\s+(.{3})/(.{3})\\s+(.{4})\\s+(.{3})\\s+(.{3})\\s+(.{2})";

    /*
     * For breaking the MESO header out and being able to use the regex below
     */
    public static final String VAD_HEADER = "nm\\s+deg";

    /*
     * For parsing the tabular block for the vad product
     */
    public static final String VAD_REGEX = "\\s+(.{3})\\s+(.{4})\\s+(.{4})\\s+(.{3})\\s+(.{3})\\s+(.{3})\\s+(.{3})\\s\\s\\s(.{7})\\s\\s\\s(.{5})\\s\\s\\s\\s(.{4})";

    /*
     * For breaking the TVS header out and being able to use the regex below
     */
    public static final String TORNADO_HEADER = "(E-3/s,kft)";

    // TODO needs work
    public static final String TORNADO_REGEX = "\\s+(.\\w+)\\s+(.{2})\\s\\s\\s(.{3})/(.{3})\\s+(.\\w+)\\s+(.\\w+)\\s\\s\\s(.{3})/(.{4})\\s\\s\\s(.{5})\\s\\s(.{5})/(.{5})\\s\\s\\s(.{3})/(.{4})";

    // public static final String

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

    public static final Pattern meso_headerPattern = Pattern.compile(
            MESO_HEADER, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

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

}
