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
package com.raytheon.viz.hydrocommon;

import java.text.SimpleDateFormat;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;

/**
 * Hydrology constants.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jun 17, 2008 1194        M. Duff     Initial creation.
 * 11/18/2008   1662        grichard    Added physical element enum type.
 * 
 * </pre>
 * 
 * @author M. Duff
 * @version 1.0
 */

public class HydroConstants {
    private static SimpleDateFormat sdf = null;
    private static SimpleDateFormat sds = null;
    private static SimpleDateFormat qpe = null;
    private static SimpleDateFormat display = null;
    private static SimpleDateFormat fr = null;
    private static SimpleDateFormat gridDataFormat = null;
    
    static {
        sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        sds = new SimpleDateFormat("yyyyMMddHH");
        sds.setTimeZone(TimeZone.getTimeZone("GMT"));
        qpe = new SimpleDateFormat("MMddyyyyHH");
        qpe.setTimeZone(TimeZone.getTimeZone("GMT"));
        display = new SimpleDateFormat("MM dd yyyy HH");
        display.setTimeZone(TimeZone.getTimeZone("GMT"));
        fr = new SimpleDateFormat("MM/dd/yyyy HH:mm");
        fr.setTimeZone(TimeZone.getTimeZone("GMT"));
        gridDataFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.S");
        gridDataFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }
    
    /** Hydro debug token name. */
    public static final String DEBUG_HYDRO_DB_TOKEN = "DEBUG_HYDRO_DB";
    
    /** MPE debug token name. */
    public static final String DEBUG_MPE_TOKEN = "DEBUG_MPE";
    
    /** pghost token */
    public static final String PGHOST = "pghost";
    
    /** pgport token */
    public static final String PGPORT = "pgport";
    
    /** db_name token */
    public static final String DB_NAME = "db_name";
    
    /** MISSING STRING **/
    public static final String MISSING_STRING = "MSG";
    
    /** default year */
    public static final int DEFAULT_YEAR = 1900;

    /** maximum number of days for the time series */
    public static final int DAYS_MAX = 90;

    /** Number of milliseconds per second */
    public static final int MILLIS_PER_SECOND = 1000;

    /** Number of milliseconds per minute */
    public static final int MILLIS_PER_MINUTE = 1000 * 60;

    /** Number of milliseconds per hour */
    public static final int MILLIS_PER_HOUR = 1000 * 60 * 60;

    /** Number of milliseconds per day */
    public static final long MILLIS_PER_DAY = 1000 * 60 * 60 * 24;

    /** Number of minutes per day */
    public static final int MINUTES_PER_DAY = 60 * 24;

    /** Number of seconds per hour */
    public static final int SECONDS_PER_HOUR = 60 * 60;

    /** Number of seconds per day */
    public static final int SECONDS_PER_DAY = 60 * 60 * 24;
    
    /** Number of seconds per minute */
    public static final int SECONDS_PER_MINUTE = 60;

    /** Max number of traces per time series graph */
    public static final int MAX_TRACES = 30;

    /** Max number of points per time series graph */
    public static final int MAX_POINTS = 5000;

    /** String constant lid */
    public static final String LID = "lid";

    /** String constant pe */
    public static final String PE = "pe";

    /** String constant ts */
    public static final String TS = "ts";

    /** String constant value */
    public static final String VALUE = "value";

    /** String constant ihfs */
    public static final String IHFS = "ihfs";

    /** String constant lat */
    public static final String LAT = "lat";

    /** String constant lon */
    public static final String LON = "lon";

    /** Major Flooding Color */
    public static final RGB SQUARE_COLOR = new RGB(255, 0, 0);

    /** Major Flooding Color */
    public static final RGB MAJOR_FLOODING_COLOR = new RGB(204, 51, 255);

    /** Moderate Flooding Color */
    public static final RGB MODERATE_FLOODING_COLOR = new RGB(255, 0, 0);

    /** Minor Flooding Color */
    public static final RGB MINOR_FLOODING_COLOR = new RGB(255, 153, 0);

    /** Near Flood Stage Color */
    public static final RGB NEAR_FLOOD_STAGE_COLOR = new RGB(255, 255, 0);

    /** No Flooding Color */
    public static final RGB NO_FLOODING_COLOR = new RGB(0, 255, 0);

    /** Old Obs Color */
    public static final RGB OLD_OBS_COLOR = new RGB(189, 194, 187);

    /** No Data Color */
    public static final RGB NO_DATA_COLOR = new RGB(102, 102, 102);

    /** Rating Conversion failure value */
    public static final int RATING_CONVERT_FAILED = -9999;

    /** Missing Data Value */
    public static final int MISSING_VALUE = -9999;
    
    /** Unassigned value */
    public static final int UNASSIGNED = -1;

    /** IHFS Date Format yyyy-MM-dd HH:mm:ss */
    public static final SimpleDateFormat DATE_FORMAT = sdf;

    /** Metadata Grib Date Format yyyy-MM-dd HH:mm:ss.S */
    public static final SimpleDateFormat GRIB_DATE_FORMAT = gridDataFormat;

    /** Hydro File Date Format yyyyMMddHH */
    public static final SimpleDateFormat FILE_DATE_FORMAT = sds;

    /** QPE File Date Format MMddyyyyHH */
    public static final SimpleDateFormat QPE_DATE_FORMAT = qpe;
    
    /** Display Date Format MM dd yyyy HH */
    public static final SimpleDateFormat DISPLAY_DATE_FORMAT = display;
    
    /** Date Format MM/dd/yyyy HH:mm */
    public static final SimpleDateFormat FR_DATE_FORMAT = fr;

    /** Minor Flood Stage Column */
    public static final String MINOR_STAGE = "minor_stage";

    /** Moderate Flood Stage Column */
    public static final String MODERATE_STAGE = "moderate_stage";

    /** Major Flood Stage Column */
    public static final String MAJOR_STAGE = "major_stage";

    /** Minor Flow Stage Column */
    public static final String MINOR_FLOW = "minor_flow";

    /** Moderate Flow Stage Column */
    public static final String MODERATE_FLOW = "moderate_flow";

    /** Major Flow Stage Column */
    public static final String MAJOR_FLOW = "major_flow";

    /** The Group Definition Config file */
    public static final String GROUP_DEFINITION = "/hydro/group_definition.cfg";
    
    /** XDat groups file */
    public static final String XDAT_GROUPS = "/hydro/groups";
    
    /** XDat PE map file */
    public static final String XDAT_PE_MAP = "/hydro/pe_map";
    
    /** XDat towns nc file */
    public static final String XDAT_TOWNS_NC = "/hydro/towns_nc";
    
    /** XDat tokens file */
    public static final String XDAT_TOKENS = "/hydro/xdatTokens.txt";

    /** The TimeSeries Line Width Token Name */
    public static final String TS_LINEWIDTH = "timeseries_linewidth";

    /** The IHFS Persistent object package */
    public static final String IHFS_DB_PACKAGE = "com.raytheon.edex.plugin.shef.objects.";

    /** The PC as PP interpolate flag */
    public static final String INTERPOLATE = "interpolate";

    /** the PC as PP assign flag */
    public static final String ASSIGN = "assign";

    /** the PC as PP off flag */
    public static final String OFF = "off";

    /** The interval in seconds */
    public static final int INTERVAL_SECONDS = 3600;

    /** The PP physical element */
    public static final String PP = "PP";

    /** The PC physical element */
    public static final String PC = "PC";

    /** The local 7am search window token */
    public static final String PPP_PPD_LOCAL_7AM_WINDOW = "ppp_ppd_local_7am_window";

    /** The local 7am window default value in hours */
    public static final int LOCAL_5004_7AM_WINDOW = 3;

    public static final int EXACT_ENDINGTIME_MATCH = -1;

    public static final int CLOSEST_ENDINGTIME_MATCH = -2;

    public static final int LATEST_ENDINGTIME_MATCH = -3;
    
    public static final String XMRG_DIR_TOKEN = "rfcwide_xmrg_dir";
    
    public static final String RFCMOSAIC_DIR_TOKEN = "gaq_xmrg_1hr_dir";

    /**
     * Enumeration used for selection of physical element.
     */
    public static enum PhysicalElement {
        PC("Accumulated Precipitation"), PP("Actual Increment");

        private String stringValue;

        private PhysicalElement(String value) {
            stringValue = value;
        }

        @Override
        public String toString() {
            return stringValue;
        }
    }

    /**
     * The Time Step Data Element Sub Types.
     */
    public static enum TimeStepDataElement {
        // RIVER     
        STAGE_POOL_TSDE(0),
        FLOW_STORAGE_TSDE(1),
        DEPTH_ABOVE_FS_TSDE(2),
        PERCENT_FLOOD_FLOW_TSDE(3),
        
        // RAIN  
        INSTANTANEOUS_PRECIP_TSDE(4),
        HOURLY_PRECIP_TSDE(5),
        THREE_HOUR_PRECIP_TSDE(6),
        SIX_HOUR_PRECIP_TSDE(7),
        DAILY_PRECIP_TSDE(8),
        
        // SNOW 
        SNOW_WATER_EQUIV_TSDE(9),
        SWE_24_HOUR_CHANGE_TSDE(10),
        
        // TEMPERATURE
        TEMPERATURE_TSDE(11),
        TEMP_24_HOUR_CHANGE_TSDE(12),
        TEMP_MAX_TSDE(13),
        TEMP_MIN_TSDE(14),
        
        // HUMIDITY 
        DEWPOINT_TSDE(15),
        DEWPOINT_24_HOUR_CHANGE_TSDE(16),
        RELATIVE_HUMIDITY_TSDE(17),
        
        // WIND
        WIND_SPEED_TSDE(18),
        WIND_DIRECTION_TSDE(19),
        
        TIME_STEP_DATA_ELEMENT_COUNT(20);     
        
        private final int elementType;
        
        TimeStepDataElement(int value) {
            elementType = value;
        }
        
        public int getElementType() {
            return elementType;
        }
    
    }

    /**
     * 
     */
    public static enum AdHocDataElementType {
        RIVER_AD_HOC_TYPE(0),
        RAIN_AD_HOC_TYPE(1),
        SNOW_AD_HOC_TYPE(2),
        TEMP_AD_HOC_TYPE(3),
        OTHER_AD_HOC_TYPE(4);
        
        private int elementType;
        
        AdHocDataElementType(int value) {
            elementType = value;
        }
        
        public int getAdHocDataElementType() {
            return elementType;
        }
    }

    /**
     * Time Step Element Type.
     *   left cbo box
     */
    public static enum TimeStepDataElementType {
        RIVER_TIME_STEP_TYPE(0),
        RAIN_TIME_STEP_TYPE(1),
        SNOW_TIME_STEP_TYPE(2),
        TEMPERATURE_TIME_STEP_TYPE(3),
        HUMIDITY_TIME_STEP_TYPE(4),
        WIND_TIME_STEP_TYPE(5);
        
        private final int dataElementType;
        
        TimeStepDataElementType(int value) {
            dataElementType = value;
        }
        
        public int getTimeStepDataElementType() {
            return dataElementType;
        }
     }

    public static enum InstPrecipSelection {
        PRECIP_TIME_30_MINUTES(0),
        PRECIP_TIME_1_HOUR(1),
        PRECIP_TIME_2_HOURS(2),
        PRECIP_TIME_3_HOURS(3),
        PRECIP_TIME_4_HOURS(4),
        PRECIP_TIME_6_HOURS(5),
        PRECIP_TIME_12_HOURS(6),
        PRECIP_TIME_18_HOURS(7),
        PRECIP_TIME_24_HOURS(8),
        PRECIP_TIME_COUNT(9);
        
        private final int instPrecipSelection;
        
        InstPrecipSelection(int value) {
            instPrecipSelection = value;
        }
        
        public int getInstPrecipSelection() {
            return instPrecipSelection;
        }        
    }

    /** Undefined text */
    public static final String UNDEFINED = "UNDEFINED";

    public static final int QC_MANUAL_PASSED = 121;

    public static final int QC_MANUAL_QUEST = 122;

    public static final int QC_MANUAL_FAILED = 123;

    public static final int DEFAULT_QC_VALUE = 1879048191;
    
    public static final double FLOOD_REPORT_MSG = -999;
    
    public static enum ArealTypeSelection {
        ZONES,    
        COUNTIES,
        BASINS,
        RESERVOIRS;
    };
    
    public static final String[] GEOAREA_DATANAMES = {
        "ZONE", "COUNTY", "BASIN", "RESRVR"
    };
    
    public static final int MAX_HRAP_ROWS = 5000;
}