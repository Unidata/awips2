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
package com.raytheon.viz.hydro.pointdatacontrol;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.viz.hydrocommon.HydroConstants;


/**
 * Point Data Control Constants.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2008            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class PDCConstants extends HydroConstants {
    public static enum RiverStationFilter {
        ALL_STATIONS_RSF(0),    
        STREAM_STATIONS_RSF(1),
        RESERVOIR_STATIONS_RSF(2);
        
        private final int riverStationFilterType;
        
        RiverStationFilter(int value) {
            riverStationFilterType = value;
        }
        
        public int getRiverStationFilter() {
            return riverStationFilterType;
        }
    }
    
    public static enum PrecipPeFilter {
        PC_AND_PP_PPF(0),   
        PC_ONLY_PPF(1),
        PP_ONLY_PPF(2);
        
        private final int filterValue;
        
        PrecipPeFilter(int value) {
            filterValue = value;
        }
        
        public int getFilterType() {
            return filterValue;
        }
    }
    
    public static enum TimeModeType {
        LATEST(0),
        SETTIME(1),
        MINSELECT(2),
        MAXSELECT(3),
        VALUE_CHANGE(4);
        
        private final int timeMode;
        
        TimeModeType(int value) {
            timeMode = value; 
        }
        
        public int getTimeMode() {
            return timeMode;
        }
    }
    
    public static enum ValueFilterOperation {
        SHOW_ALL ("Any Value"),
        SHOW_EQUAL ("Value ="),
        SHOW_NOT_EQUAL ("Value Not ="),
        SHOW_GREATER_EQUAL ("Value >="),
        SHOW_LESS_EQUAL ("Value <="),
        SHOW_GREATER ("Value >"),
        SHOW_LESS ("Value <");
        
        private final String filterType;
        
        ValueFilterOperation(String value) {
            filterType = value;
        }
        
        public String getFilterType() {
            return filterType;
        }
    }
    
    public static enum ElevationFilterOperation {
        SHOW_ALL ("Any Elev"),
        SHOW_EQUAL ("Elev ="),
        SHOW_NOT_EQUAL ("Elev Not ="),
        SHOW_GREATER_EQUAL ("Elev >="),
        SHOW_LESS_EQUAL ("Elev <="),
        SHOW_GREATER ("Elev >"),
        SHOW_LESS ("Elev <");
        
        private final String filterType;
        
        ElevationFilterOperation(String value) {
            filterType = value;
        }
        
        public String getFilterType() {
            return filterType;
        }
    }
    
    public static enum QueryMode {
        AD_HOC_MODE(0),
        TIME_STEP_MODE(1);
        
        private final int queryMode;
        
        QueryMode(int value) {
            queryMode = value;
        }
        
        public int getQueryMode() {
            return queryMode;
        }

    }
    
    /** The element type */
    public static enum ElementType {
        AD_HOC,
        TIME_STEP
    }

    public static String[] pcOptionArray = new String[] {
        "at" ,
        "ct" , "da" , "de" , "dh" ,
        "ds" , "dt" , "eo" , "ev" ,
        "fl" , "fo" , "ic" , "id" , 
        "nm" , "ns" , "pe" , "pp" ,
        "pr" , "ps" , "qm" , "rs" ,
        "sb" , "se",  "sf" ,
        "sm" , "sp" ,
        "sr" , "st" , "sv" , "sz" , 
        "ti" , "tm" , "tp",
        "ts" , "vl", 
        "vo" , "vt" , "vv" , "wf" ,
        "xs"
    };
    
//    InstantaneousPrecipAccumTime,  
//    SelectedTypeSources /*was CurTs */ , DataType , DeriveStageFlow , DurHours , 
//    FilterByDataSource , DateTime , ElevFilterOperation, ElevFilterValue ,
//    FloodLevel , FcstOnly , ShowIcon , ShowId ,
//    ShowName , NumSources , SelectedAdHocPE /*was CurPE  */ , PCandPP ,
//    Primary , ProcessSelected , SelectedQueryMode , ShowRiverStatus ,
//    StageBasis , ShowElevation, StreamStationFilter,
//    SuppressMissing , ShowParamCode ,
//    SourceList ,  SelectedTimeStepElement, ServiceBackup , SuppressZero ,
//    ShowTime , TimeMode , TimeStepPrecipPeFilter,
//    FilterByTypeSource , ShowValue ,
//    ValueFilterOperation , ValueType , ValueFilterValue , WFOlist ,
//    PESelectedIndex } ;
    
    public static enum ValueType {
        TYPE_VALUE(0),
        TYPE_DEPART(1);
        
        private final int valueType;
        
        ValueType(int value) {
            valueType = value;
        }
        
        public int getValueType() {
            return valueType;
        }
    }
    
    /**
     * BASIS_OBS = 0
     * BASIS_FCST = 1
     * BASIS_MOFO = 2
     */
    public static enum StageBasis {
        BASIS_OBS("Observed Value"),
        BASIS_FCST("Forecast"),
        BASIS_MOFO("Max (Obs, Fcst)");

        private final String stageBasis;
        
        StageBasis(String value) {
            stageBasis = value;
        }
        
        public String getStageBasis() {
            return stageBasis;
        }
    }
     
    public static enum RiverValueDisplayMode {
        RAW_VALUE_ONLY(0),
        RAW_VALUE_FLOOD_LEVEL(1), 
        RAW_VALUE_STAGE_FLOW(2),
        FLOOD_DEPARTURE(3), 
        FLOOD_DEPARTURE_FLOOD_LEVEL(4);

        private final int displayMode;
        
        RiverValueDisplayMode(int value) {
            displayMode = value;
        }
        
        public int getIntValue() {
            return displayMode;
        }
    }
    
    public static enum ProcessLineMode {
        PROCESS_NEW_STATION(0),
        PROCESS_DATA_SOURCES(1),
        PROCESS_VALUES(2);
        
        private final int processMode;
        
        ProcessLineMode(int value) {
            processMode = value;
        }
        
        public int getProcessMode() {
            return processMode;
        }
    }
     
    /* possible values for process present */
    public static final int PROC_AS_OBS = 1;
    public static final int PROC_AS_PROC = 0;
    
    /* Option Sting constants */
    public static final int INSTANTANEOUS_PRECIP_ACCUM_TIME = 0;
    public static final int SELECTED_TYPE_SOURCES = 1;
    public static final int DATA_TYPE = 2;
    public static final int DERIVE_STAGE_FLOW = 3;
    public static final int DUR_HOURS = 4; 
    public static final int FILTER_BY_DATA_SOURCE = 5;
    public static final int DATETIME = 6;
    public static final int ELEV_FILTER_OPERATION = 7;
    public static final int ELEV_FILTER_VALUE = 8;
    public static final int FLOOD_LEVEL = 9;
    public static final int FCST_ONLY = 10;
    public static final int SHOW_ICON = 11;
    public static final int SHOW_ID = 12;
    public static final int SHOW_NAME = 13;
    public static final int NUM_SOURCES = 14;
    public static final int SELECTED_ADHOC_PE = 15;
    public static final int PC_AND_PP = 16;
    public static final int PRIMARY = 17;
    public static final int PROCESS_SELECTED = 18;
    public static final int SELECTED_QUERY_MODE = 19;
    public static final int SHOW_RIVER_STATUS = 20;
    public static final int STAGE_BASIS = 21;
    public static final int SHOW_ELEVATION = 22;
    public static final int STREAM_STATION_FILTER = 23;
    public static final int SUPPRESS_MISSING = 24;
    public static final int SHOW_PARAM_CODE = 25;
    public static final int SOURCE_LIST = 26;
    public static final int SELECTED_TIME_STEP_ELEMENT = 27;
    public static final int SERVICE_BACKUP = 28;
    public static final int SUPPRESS_ZERO = 29;
    public static final int SHOW_TIME = 30;
    public static final int TIME_MODE = 31;
    public static final int TIME_STEP_PRECIP_PE_FILTER = 32;
    public static final int FILTER_BY_TYPE_SOURCE = 33;
    public static final int SHOW_VALUE = 34;
    public static final int VALUE_FILTER_OPERATION = 35;
    public static final int VALUE_TYPE = 36;
    public static final int VALUE_FILTER_VALUE = 37;
    public static final int WFO_LIST = 38;
    public static final int PE_SELECTION_INDEX = 39;
    
    /**
     * Number of minutes between refresh, default.
     */
    public static final int REFRESH_MINUTES = 15;
    
    public static final String HV_REFRESH_MINUTES = "hv_refresh_minutes";
    
    public static final String HV_HOURS_IN_WINDOW = "hv_hours_in_window";
    
    public static final int DEFAULT_HOURS_IN_WINDOW = 4;
    
    public static final double MINMAX_DUR_MULTIPLIER = 1.5;

    public static final String THREAT_MISSING_DATA = "Z";
    public static final String THREAT_MISSING_STAGE = "M";
    public static final String THREAT_NONE = "G";
    public static final String THREAT_ACTION = "Y";
    public static final String THREAT_FLOOD = "R";
    
    public static final String OBSERVER_STRING = "Observer";
    public static final String DCP_STRING = "DCP";
    public static final String UNDEFINED_STRING = "Undefined";
    
    public static final String FCSTPT_STNCLASS = "F";
    
    public static final String ALL_AREAS = "A-L-L_A-R-E-A-S";
    
    public static final String[] TIME_STEP_FILE_NAME_ARRAY = {
        "Height.dat", 
        "FlowStorage.dat",
        "HeightAboveFloodStage.dat", 
        "PercentFloodFlow.dat",
        "PrecipInstant.dat",
        "Precip1Hour.dat",
        "Precip3Hour.dat",
        "Precip6Hour.dat",
        "Precip24Hour.dat",
        "SnowWaterEquivalent.dat",
        "SnowWaterEquivalentDelta.dat",
        "Temperature.dat",
        "TemperatureDelta.dat",
        "TemperatureMax.dat",
        "TemperatureMin.dat",
        "DewPoint.dat",
        "DewPointDelta.dat",
        "RelativeHumidity.dat",
        "WindSpeed.dat",
        "WindDirection.dat"    
    };
    
    public static final String[] TIME_STEP_STRINGS = {
        "Stage/Pool",
        "Flow/Storage",
        "Depth Above Flood Stage",
        "Percent Flood Flow",
        "Instantaneous Precip",
        "Hourly Precip Total",
        "3-Hour Precip Total",
        "6-Hour Precip Total",
        "24-Hour Precip Total",         
        "Snow-Water Equiv",
        "SWE 24-Hour Change",
        "Temperature (F)",
        "Temp. 24-Hour Change",
        "Temp. 24-Hour Max",
        "Temp. 24-Hour Min",
        "Dewpoint (F)",
        "Dewpoint Change (F)",
        "Relative Humidity",
        "Wind Speed",
        "Wind Direction"
    };
    
    public static final String[] INST_PRECIP_STRINGS = {
        "30 Min.",
        "1 Hour",
        "2 Hours",
        "3 Hours",
        "4 Hours",
        "6 Hours",
        "12 Hours",
        "18 Hours",
        "24 Hours"
    };

    public static final RGB DEFAULT_COLOR = new RGB(30, 144, 255);
}
