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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.SimpleTimeZone;
import java.util.regex.Pattern;

/**
 * Constants used in the SHEF decoder plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/19/08     387         M. Duff     Initial creation.  
 * 10/16/2008   1548        jelkins     Removed unneeded constants
 * 02/02/2009   1943        jsanchez    Added shef_load_maxfcst.
 * 06/03/2009   2410        jsanchez    Changed kk to HH.
 * 04/29/2014   3088        mpduff      Added MILLLIS_PER_SECOND;
 * 
 * </pre>
 */
public class ShefConstants {
    public static final String IHFS = "ihfs";

    public static final String TYPE_A = ".A";

    public static final String TYPE_B = ".B";

    public static final String TYPE_E = ".E";

    public static final int MILLIS_PER_SECOND = 1000;

    public static final int MILLIS_PER_MINUTE = MILLIS_PER_SECOND * 60;

    public static final int MILLIS_PER_HOUR = MILLIS_PER_SECOND * 60 * 60;

    public static final long MILLIS_PER_DAY = 1000 * 60 * 60 * 24;

    public static final long HALF_YEAR = 365L * 24L * 3600L * 1000L / 2L;

    public static final String SHEF_SKIPPED = "-9998";

    public static final String SHEF_MISSING = "-9999";

    public static final String SHEF_MISSING_DEC = "-9999.0";

    public static final int SHEF_MISSING_INT = -9999;

    public static final String SHEF_TRACE = "0.001";

    public static final int SHEF_NOT_SERIES = 0;

    public static final Pattern SHEF_TYPE_PATTERN = Pattern.compile("\\.[ABE]");

    public static final String EMPTYSTRING = "";

    public static final String SINGLE_QUOTE = "'";

    public static final String DOUBLE_QUOTE = "\"";

    public static final String EOL = "\n";

    public static final String SLASH = "/";

    public static final int LOWER_LID_LIMIT = 2;

    public static final int UPPER_LID_LIMIT = 9;

    /* Precipitation index constants */
    public static final int NOT_PRECIP = 0;

    public static final int RAWPC = 1;

    public static final int RAWPP = 2;

    public static final int RAWPOTHER = 3;

    /** Greenwich Mean Time */
    public static final String GMT = "GMT";

    public static final String Z = "Z";

    public static final SimpleTimeZone GMT_ZONE = new SimpleTimeZone(0, GMT);

    /* simple date formats */
    public static final SimpleDateFormat MMDD_FORMAT = new SimpleDateFormat(
            "MMdd");

    public static final SimpleDateFormat MMDDHH_FORMAT = new SimpleDateFormat(
            "MMddHH");

    public static final SimpleDateFormat MMDDHHMM_FORMAT = new SimpleDateFormat(
            "MMddHHmm");

    public static final SimpleDateFormat MMDDHHMMSS_FORMAT = new SimpleDateFormat(
            "MMddHHmmss");

    public static final SimpleDateFormat YYMM_FORMAT = new SimpleDateFormat(
            "yyMM");

    public static final SimpleDateFormat YYMMDD_FORMAT = new SimpleDateFormat(
            "yyMMdd");

    public static final SimpleDateFormat YYMMDDHH_FORMAT = new SimpleDateFormat(
            "yyMMddHH");

    public static final SimpleDateFormat YYMMDDHHMM_FORMAT = new SimpleDateFormat(
            "yyMMddHHmm");

    public static final SimpleDateFormat YYMMDDHHMMSS_FORMAT = new SimpleDateFormat(
            "yyMMddHHmmss");

    public static final SimpleDateFormat YYYYMMDD_FORMAT = new SimpleDateFormat(
            "yyyyMMdd");

    public static final SimpleDateFormat YYYYMMDDHH_FORMAT = new SimpleDateFormat(
            "yyyyMMddHH");

    public static final SimpleDateFormat YYYYMMDDHHMM_FORMAT = new SimpleDateFormat(
            "yyyyMMddHHmm");

    public static final SimpleDateFormat YYYYMMDDHHMMSS_FORMAT = new SimpleDateFormat(
            "yyyyMMddHHmmss");

    public static final SimpleDateFormat YYYYMMJJJ_FORMAT = new SimpleDateFormat(
            "yyyyMMDDD");

    public static final SimpleDateFormat YYYYMMJJJHH_FORMAT = new SimpleDateFormat(
            "yyyyMMDDDHH");

    public static final SimpleDateFormat YYYYMMJJJHHMM_FORMAT = new SimpleDateFormat(
            "yyyyMMDDHHmm");

    public static final SimpleDateFormat YYYYMMJJJHHMMSS_FORMAT = new SimpleDateFormat(
            "yyyyMMDDHHmmss");

    public static final SimpleDateFormat YYMMJJJ_FORMAT = new SimpleDateFormat(
            "yyMMDDD");

    public static final SimpleDateFormat YYMMJJJHH_FORMAT = new SimpleDateFormat(
            "yyMMDDDHH");

    public static final SimpleDateFormat YYMMJJJHHMM_FORMAT = new SimpleDateFormat(
            "yyMMDDHHmm");

    public static final String POSTGRES_DATE_STRING = "yyyy-MM-dd HH:mm:ss";

    public static final SimpleDateFormat POSTGRES_DATE_FORMAT = new SimpleDateFormat(
            POSTGRES_DATE_STRING);

    /*
     * Date/Data Type Element Definition DS Second of minute (ss) DN Minute of
     * hour (nn) (nnss) DH Hour of day (hh) (hhnn) (hhnnss) DD Day of month (dd)
     * (ddhh) (ddhhnn) (ddhhnnss) DM Month of year (mm) (mmdd) (mmddhh)
     * (mmddhhnn) mmddhhnnss) DJ Julian date (ddd) (yyddd) (ccyyddd) DY Year
     * (yy) (yymm) (yymmdd) (yymmddhh) (yymmddhhnn) DI Time interval specifier
     * for .E format (see Table 12) DQ Data qualifier for rest of line (see
     * Table 10) DR Date relative increment (see Table 13) DU Unit type change
     * (E=English, S=Standard International) DV Duration variable code (see
     * Table 11a & 11b) DT Century (cc) (ccyy) (ccyymmdd) (ccyymmddhh)
     * (ccyymmddhhnn)
     */
    public static final String DS = "DS";

    public static final String DN = "DN";

    public static final String DH = "DH";

    public static final String DD = "DD";

    public static final String DM = "DM";

    public static final String DJ = "DJ";

    public static final String DY = "DY";

    public static final String DI = "DI";

    public static final String DQ = "DQ";

    public static final String DR = "DR";

    public static final String DU = "DU";

    public static final String DV = "DV";

    public static final String DT = "DT";

    public static final String D = "D";

    public static final String DE = "DE";

    public static final String DC = "DC";

    public static final String VALID_UNITS = "ES";

    public static final String DATE_INC_CODES = "SNHDMEY";

    public static final int[] DATE_INC_VALS = new int[] { Calendar.SECOND, // S
            Calendar.MINUTE, // N
            Calendar.HOUR_OF_DAY, // H
            Calendar.DAY_OF_MONTH, // D
            Calendar.MONTH, // M
            -1, // E, -1 signifies special handling
            Calendar.YEAR, // Y
    };

    public static final String DURATION_CODES = "SNHDMY";

    public static final short[] DURATION_VALS = new short[] { 7000, // "S"
                                                                    // Seconds
            0, // "N" Minutes
            1000, // "H" Hours
            2000, // "D" Days
            3000, // "M" Months
            4000, // "Y" Years
    };

    public static final String QUALIFER_CODES = "BDEFGLMNPQRSTVWZ";

    /*
     * these requests are for checking a value. they are valid for building a
     * where clause or for checking the qc code
     */
    public static final long QC_PASSED = 101;

    public static final long QC_QUESTIONABLE = 102;

    public static final long QC_FAILED = 103;

    public static final long QC_NOT_PASSED = 104;

    public static final long QC_NOT_FAILED = 105;

    public static final long QC_DEFAULT = 110;

    /*
     * these requests are for setting the qc code, which is done by setting a
     * specific bit and its associated summary bit
     */
    public static final long QC_GROSSRANGE_FAILED = 120;

    public static final long QC_MANUAL_PASSED = 121;

    public static final long QC_MANUAL_QUEST = 122;

    public static final long QC_MANUAL_FAILED = 123;

    public static final long QC_MANUAL_NEW = 124;

    public static final long QC_EXTERN_QUEST = 125;

    public static final long QC_EXTERN_FAILED = 126;

    public static final long QC_REASONRANGE_FAILED = 127;

    public static final long QC_ROC_PASSED = 128;

    public static final long QC_ROC_FAILED = 129;

    public static final long QC_OUTLIER_PASSED = 130;

    public static final long QC_OUTLIER_FAILED = 131;

    /***************************************************************************
     * The remainder of this file contains definitions for internal use.
     * Definition of symbolic constants used as bit descriptors
     **************************************************************************/
    public static final long SIGN_QC = 31;

    public static final long CERTAINTY_QC = 30;

    public static final long NOTQUEST_QC = 29;

    public static final long TEST_RUN_QC = 28;

    public static final long BIT27 = 27;

    public static final long BIT26 = 26;

    public static final long BIT25 = 25;

    public static final long BIT24 = 24;

    public static final long EXTERN_QC = 23;

    public static final long MANUAL_QC = 22;

    public static final long GROSSRANGE_QC = 21;

    public static final long EXTERN_NOTQUEST_QC = 20;

    public static final long REASONRANGE_QC = 19;

    public static final long ROC_QC = 18;

    public static final long OUTLIER_QC = 17;

    public static final long BIT16 = 16;

    public static final long BIT15 = 15;

    public static final long BIT14 = 14;

    public static final long BIT13 = 13;

    public static final long BIT12 = 12;

    public static final long BIT11 = 11;

    public static final long BIT10 = 10;

    public static final long BIT9 = 9;

    public static final long BIT8 = 8;

    public static final long BIT7 = 7;

    public static final long BIT6 = 6;

    public static final long BIT5 = 5;

    public static final long BIT4 = 4;

    public static final long BIT3 = 3;

    public static final long BIT2 = 2;

    public static final long BIT1 = 1;

    public static final long BIT0 = 0;

    /***************************************************************************
     * Bit pattern 0110111111111111 1111111111111111 yields 1,879,048,191.
     **************************************************************************/
    public static final long DEFAULT_QC_VALUE = 1879048191;

    /***************************************************************************
     * Bit pattern 0110000000000000 0000000000000000 yields 1,610,612,736.
     **************************************************************************/
    public static final long GOOD_QUESTIONABLE_THRESHOLD = 1610612736;

    /***************************************************************************
     * Bit pattern 0100000000000000 0000000000000000 yields 1,073,741,824.
     **************************************************************************/
    public static final long QUESTIONABLE_BAD_THRESHOLD = 1073741824;

    /***************************************************************************
     * Bit pattern 0111111111111111 1111111111111111 yields 2,147,483,647. =
     * 2**31 - 1
     **************************************************************************/
    public static final long ALL_ONES = 2147483647;

    /* Spatial Consistency Check */
    public static final long SCC_QC = 16;

    /* Multisensor Consistency Check */
    public static final long MSC_QC = 15;

    public static final long INVALID_QC_REQUEST = -1;

    public static final long VALID_QC_REQUEST = 1;

    public static enum IngestSwitch {
        POST_PE_ONLY, POST_PE_OFF, POST_PE_AND_HOURLY
    };

    public static final String ALWAYS_OVERWRITE = "ALWAYS_OVERWRITE";

    public static final String USE_REVCODE = "USE_REVCODE";

    public static final String IF_DIFFERENT = "IF_DIFFERENT";

    public static final String IF_DIFFERENT_OR_REVCODE = "IF_DIFFERENT_OR_REVCODE";

    public static final String IF_DIFFERENT_AND_REVCODE = "IF_DIFFERENT_AND_REVCODE";

    public static final int DONT_UPDATE_ACTION = 0;

    public static final int UPDATE_ACTION = 1;

    public static final int IF_DIFFERENT_UPDATE_ACTION = 2;

    public static final String UPPER_CHECKSTR = "upper";

    public static final String LOWER_CHECKSTR = "lower";

    public static final String DIFF_CHECKSTR = "diff";

    public static final String ROC_CHECKSTR = "roc";

    public static final String ALERT_CATEGSTR = "alert";

    public static final String ALARM_CATEGSTR = "alarm";

    public static final int NO_ALERTALARM = 200;

    public static final int MAXFCST_INFO = 200;

    public static final int ALERT_UPPER_DETECTED = 201;

    public static final int ALARM_UPPER_DETECTED = 202;

    public static final int ALERT_LOWER_DETECTED = 203;

    public static final int ALARM_LOWER_DETECTED = 204;

    public static final String NONE = "NONE";

    public static final String VALID_ONLY = "VALID_ONLY";

    public static final String VALID_OR_MISSING = "VALID_OR_MISSING";

    /* Token constants */
    public static final String DUP_MESSAGE = "dupmess";

    public static final String SHEF_PROCOBS = "shef_procobs";

    public static final String SHEF_POST_UNKNOWN = "shef_post_unk";

    public static final String LOCMESS = "locmess";

    public static final String SHEF_WINPAST = "shef_winpast";

    public static final String SHEF_WINFUTURE = "shef_winfuture";

    public static final String SHEF_POST_LINK = "shef_post_link";

    public static final String SHEF_POST_LATEST = "shef_post_latest";

    public static final String SHEF_LOAD_MAXFCST = "shef_load_maxfcst";

    public static final String BASIS_HOURS_FILTER = "basis_hours_filter";

    public static final String SHEF_DUPLICATE = "shef_duplicate";

    public static final String SHEF_POST_BADDATA = "shef_post_baddata";

    public static final String SHEF_ALERTALARM = "shef_alertalarm";

    public static final String SHEF_STORETEXT = "shef_storetext";

    public static final String ELGMESS = "elgmess";

    public static final String SHEF_LOAD_INGEST = "shef_load_ingest";

    public static final String INGEST_MESS = "ingest_mess";

    public static final String SHEF_DATA_LOG = "shef_data_log";

    public static final String SHEF_PERFLOG = "shef_perflog";

    public static final String SHEF_EMIT_SKIPPED = "shef_emit_skipped";

    /* IHFS Table Names */
    public static final String REJECTED_DATA = "rejecteddata";

    public static final String AREAL_OBS = "arealobs";

    public static final String AREAL_FCST = "arealfcst";

    public static final String COMMENT_VALUE = "commentvalue";

    public static final String CONTINGENCY_VALUE = "contingencyvalue";

    public static final String PROC_VALUE = "procvalue";

    public static final String ALERTALARM_VALUE = "alertalarmval";

    public static final String UNKNOWN_STATION = "unkstn";

    public static final String UNKNOWN_STATION_VALUE = "unkstnvalue";

}
