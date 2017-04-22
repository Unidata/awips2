package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Declaration of constants that will be utilized by {@link DqcPreProcessing}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2016 4623       skorolev    Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 */
public class PreProcConstants extends CommonHydroConstants {

    public static final int MAX_TOKEN_NUMBER = 50;

    public static final String DEFAULT_DAY = "10";

    public static final int MAXIMUM_DAY = 100;

    public static final double PRECIP_MISSING = -99.0;

    public static final double TEMPERATURE_MISSING = -9999.0;

    public static final int TEMPERATURE_WINDOW_DEFAULT = 60;

    public static final int MAX_DIFF_TIME = TimeUtil.SECONDS_PER_DAY;

    public static final int SECONDS_PER_HOUR = TimeUtil.SECONDS_PER_HOUR;

    public static final double MISSING_MIN_TEMPERATURE = 999.0;

    public static final double MISSING_MAX_TEMPERATURE = -999.0;

    public static final int MAXMIN_TEMPERATURE_HOUR_WINDOW = 2;

    public static final int SECONDS_PER_3_HOURS = 3 * SECONDS_PER_HOUR;

    public static final int SECONDS_PER_6_HOURS = 6 * SECONDS_PER_HOUR;

    public static final int SECONDS_PER_9_HOURS = 9 * SECONDS_PER_HOUR;

    public static final int SECONDS_PER_12_HOURS = 12 * SECONDS_PER_HOUR;

    public static final int SECONDS_PER_15_HOURS = 15 * SECONDS_PER_HOUR;

    public static final int SECONDS_PER_18_HOURS = 18 * SECONDS_PER_HOUR;

    public static final int SECONDS_PER_21_HOURS = 21 * SECONDS_PER_HOUR;

    public static final int SECONDS_PER_24_HOURS = TimeUtil.SECONDS_PER_DAY;

    /*
     * The default ending obs time for temperature and freezing level data.
     */
    public static final int DEFAULT_ENDING_6HOUR_OBS_TIME = 6;

    public static final boolean DQC_PREPROCESSOR_ENDING_OBSTIME_06Z = false;

    public static final boolean DQC_PREPROCESSOR_ENDING_OBSTIME_12Z = true;

    public static final String INPUT_DATE_FORMAT = "%Y%m%d";

    public static final String WRITE_DATE_FORMAT = "%04d%02d%02d";

    public static final String MPE_AREA_NAMES = "mpe_area_names";

    public static final String MPE_SITE_ID = "mpe_site_id";

    public static final String DB_NAME = "db_name";

    public static final String MPE_STATION_LIST_DIR = "mpe_station_list_dir";

    public static final String MPE_POINT_TEMPERATURE_DIR = "mpe_point_temperature_dir";

    public static final String MPE_POINT_PRECIP_DIR = "mpe_point_precip_dir";

    public static final String MPE_LOAD_HOURLY_PC = "mpe_load_hourlypc";

    public static final String EXCLUD_TS = "!ts";

    public static final String MPE_TEMPERATURE_WINDOW = "mpe_temperature_window";

    public static final String MPE_MAXMINT_HOUR_WINDOW = "mpe_maxminT_hour_window";

    public static final String DQC_ENDING_6HOUR_OBSTIME = "dqc_ending_6hour_obstime";

    public static final float MISSING_PRECIP = -9999f;

    /* Token for accumulated precipitation totals */
    public static final String SUM_PC_REPORTS = "sum_pc_reports";

    /**
     * Default value for summation of accumulated precipitation reports
     */
    public static final int DEFAULT_SUM_PC_REPORTS_VALUE = 0;
}