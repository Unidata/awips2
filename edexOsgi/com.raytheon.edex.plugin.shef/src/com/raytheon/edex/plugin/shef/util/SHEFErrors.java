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
package com.raytheon.edex.plugin.shef.util;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 9, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class SHEFErrors {
    
    public enum HANDLERS {
        DEBUG, WARNING, ERROR;
    }
    
    private static final String [] LOG_MSGS = {
        "00 Reserved",
        "Intentionally left blank",
        "Two digits are required in date or time group",
        "An expected parameter code is missing",
        "File read error while accessing data file",
        "No dot in column 1 when looking for new message",
        "Dot found but not in column 1 of new message",
        "Unknown message type, looking for .A, .B, or .E",
        "Bad char in message type format (or missing blank delimiter)",
        "Last message format was different from this continuation messg",
        // 10
        "Last message was NOT a revision unlike this continuation messg",
        "Last message had an error so cannot continue",
        "No positional data or no blank before it",
        "Bad character in station id",
        "Station id has more than 8 characters",
        "Bad number in positional data date group",
        "Incorrect number in date group",
        "Incorrect number in time group",
        "Missing blank char in positional data",
        "Bad creation date",
        // 20
        "Bad date code letter after the character \"D\"",               // 20
        "Unknown data qualifier, data value is lost",                   // 21
        "Unknown data units code (need S or E)",                        // 22
        "Unknown duration code (need S,N,H,D,M,Y)",                     // 23
        "Bad 2-digit number following duration code",                   // 24
        "Unknown time interval code (need Y,M,D,H,N,S,E)",              // 25
        "Bad 2-digit number following time interval code",              // 26
        "Bad character after \"DR\" (relative date code)",              // 27
        "Bad 1- or 2-digit number in relative date code",               // 28
        "Bad character in parameter code",                              // 29
        // 30
        "Bad parameter code calls for send code",
        "Trace for code other than PP, PC, PY, SD, SF, SW",
        "Variable duration not defined",
        "Bad character where delimiter is expected",
        "Non-existent value for given type and source parameter code",
        "ZULU, DR, or DI has send code QY, PY, or HY",
        "Forecast data given without creation date",
        "No value given after parameter code and before slash or eol",
        "Explicit date for codes DRE or DIE is not the end-of-month",
        "Year not in good range (1753-2199)",
        // 40
        "Exceeded limit of data items",
        "Too many data items for given .B format",
        "Not enough data items for given .B format",
        "Cannot adjust forecast date to Zulu time",
        "Time between 0200 & 0259 on day changing from standard to daylight",
        "No time increment specified (use DI code)",
        "No \".END\" message for previous “.B” format",
        "ID requires 3 to 8 characters",
        "For daylight savings time, check Apr or Oct for 1976 thru 2040 only",
        "Bad character in the message",
        // 50
        "Missing parameter code",
        "Bad value chars (or missing delimiter), data may be lost",
        "Bad character in data field",
        "\"?\" not accepted for missing, use \"M\" or \"+\"",
        "Parameter code is too long or too short",
        "Missing delimiter between data type fields",
        "Missing delimiter after data type field",
        "Should use \"/\" after date, time, or other D-code; before data",
        "Parm codes PP and PC require decimal value",
        "Abort, cannot read \"shefparm\" file correctly",
        // 60
        "Non-existent value for given duration parameter code",
        "Non-existent value for given extremum parameter code",
        "Non-existent value for given conversion factor parameter code",
        "Non-existent value for given probability parameter code",
        "Parameter code too short or field misinterpreted as parameter code",
        "Comma not allowed in data field, data value is lost",
        "Date check for yr-mo-da shows bad date",
        "No data on line identified with a message type format",
        "An unexpected \".END\" message was encountered",
        "BUMMER!!! Maximum number of errors reached, abort message",
        // 70
        "Cannot output to binary shefpars file",
        "Cannot access “PE conversion factors” from the \"shefparm\" file",
        "Cannot access “send codes” from the \"shefparm\" file",
        "Cannot access “duration codes” from the \"shefparm\" file",
        "Cannot access “type/source codes” from the \"shefparm\" file",
        "Cannot access “extremum codes” from the \"shefparm\" file",
        "Cannot access “probability codes” from the \"shefparm\" file",
        "Cannot read \"SHEFPARM\" file!!!!!",
        "Bad character in data value, data value is lost",
        "Julian day should be written with 3 digits",
        // 80
        "Too many digits in date group!",
        "Too many characters in quotes",
        "Data line found before completing .B format line(s)",
        "Missing slash delimiter or bad time zone code",
        "Too many chars in qualifier code, data value is lost",
        "Bad data qualifier, rest of format is lost",
        "Retained comment found without a data value, comment is lost",
        "Unexpected slash found after parameter code, before data value",
        "Cannot access “qualifier codes” from the \"shefparm\" file",
        "Intentionally left blank",
        // 90
        "Unknown error number given",
        "91 Reserved",
        "92 Reserved",
        "93 Reserved",
        "94 Reserved",
        "95 Reserved",
        "96 Reserved",
        "97 Reserved",
        "98 Reserved",
        "99 Reserved",
        // 100
        "Observation time expected but not found", // 100
        "PEDTSEP redeclared in E record data", // 101
        "Too many digits in DI code", // 102
        "Too many digits in DR code", // 103
        "Too many digits in DV code", // 104
    };

//    public static final int LOG_000 =    0;
//    public static final int LOG_001 =    1;
//    public static final int LOG_002 =    2;
//    public static final int LOG_003 =    3;
//    public static final int LOG_004 =    4;
//    public static final int LOG_005 =    5;
//    public static final int LOG_006 =    6;
//    public static final int LOG_007 =    7;
//    public static final int LOG_008 =    8;
//    public static final int LOG_009 =    9;
//
//    public static final int LOG_010 =   10;
//    public static final int LOG_011 =   11;
//    public static final int LOG_012 =   12;
//    public static final int LOG_013 =   13;
//    public static final int LOG_014 =   14;
//    public static final int LOG_015 =   15;
//    public static final int LOG_016 =   16;
//    public static final int LOG_017 =   17;
//    public static final int LOG_018 =   18;
//    public static final int LOG_019 =   19;
//    
//    public static final int LOG_020 =   20;
//    public static final int LOG_021 =   21;
//    public static final int LOG_022 =   22;
//    public static final int LOG_023 =   23;
//    public static final int LOG_024 =   24;
//    public static final int LOG_025 =   25;
//    public static final int LOG_026 =   26;
//    public static final int LOG_027 =   27;
//    public static final int LOG_028 =   28;
//    public static final int LOG_029 =   29;
//
//    public static final int LOG_030 =   30;
//    public static final int LOG_031 =   31;
//    public static final int LOG_032 =   32;
//    public static final int LOG_033 =   33;
//    public static final int LOG_034 =   34;
//    public static final int LOG_035 =   35;
//    public static final int LOG_036 =   36;
//    public static final int LOG_037 =   37;
//    public static final int LOG_038 =   38;
//    public static final int LOG_039 =   39;
//
//    public static final int LOG_040 =   40;
//    public static final int LOG_041 =   41;
//    public static final int LOG_042 =   42;
//    public static final int LOG_043 =   43;
//    public static final int LOG_044 =   44;
//    public static final int LOG_045 =   45;
//    public static final int LOG_046 =   46;
//    public static final int LOG_047 =   47;
//    public static final int LOG_048 =   48;
//    public static final int LOG_049 =   49;
//
//    public static final int LOG_050 =   50;
//    public static final int LOG_051 =   51;
//    public static final int LOG_052 =   52;
//    public static final int LOG_053 =   53;
//    public static final int LOG_054 =   54;
//    public static final int LOG_055 =   55;
//    public static final int LOG_056 =   56;
//    public static final int LOG_057 =   57;
//    public static final int LOG_058 =   58;
//    public static final int LOG_059 =   59;
//
//    public static final int LOG_060 =   60;
//    public static final int LOG_061 =   61;
//    public static final int LOG_062 =   62;
//    public static final int LOG_063 =   63;
//    public static final int LOG_064 =   64;
//    public static final int LOG_065 =   65;
//    public static final int LOG_066 =   66;
//    public static final int LOG_067 =   67;
//    public static final int LOG_068 =   68;
//    public static final int LOG_069 =   69;
//
//    public static final int LOG_070 =   70;
//    public static final int LOG_071 =   71;
//    public static final int LOG_072 =   72;
//    public static final int LOG_073 =   73;
//    public static final int LOG_074 =   74;
//    public static final int LOG_075 =   75;
//    public static final int LOG_076 =   76;
//    public static final int LOG_077 =   77;
//    public static final int LOG_078 =   78;
//    public static final int LOG_079 =   79;
//
//    public static final int LOG_080 =   80;
//    public static final int LOG_081 =   81;
//    public static final int LOG_082 =   82;
//    public static final int LOG_083 =   83;
//    public static final int LOG_084 =   84;
//    public static final int LOG_085 =   85;
//    public static final int LOG_086 =   86;
//    public static final int LOG_087 =   87;
//    public static final int LOG_088 =   88;
//    public static final int LOG_089 =   89;
//
//    public static final int LOG_090 =   90;
//    public static final int LOG_091 =   91;
//    public static final int LOG_092 =   92;
//    public static final int LOG_093 =   93;
//    public static final int LOG_094 =   94;
//    public static final int LOG_095 =   95;
//    public static final int LOG_096 =   96;
//    public static final int LOG_097 =   97;
//    public static final int LOG_098 =   98;
//    public static final int LOG_099 =   99;
//
//    public static final int LOG_100 =  100;
//    public static final int LOG_101 =  101;
//    public static final int LOG_102 =  102;
//    public static final int LOG_103 =  103;
//    public static final int LOG_104 =  104;
    

    public static final boolean LOGOUT = false;
    public static final boolean STDOUT = true;

    private static final String ERR_FMT   = "ERROR %20s %03d %s";
    private static final String ERROR_FMT = "ERROR %03d %s";

    private static final String WRN_FMT   = "WARNG %20s %03d %s";
    private static final String WARN_FMT  = "WARNG %03d %s";

    private static final String DBG_FMT   = "DEBUG %20s %03d %s";
    private static final String DEBUG_FMT  = "DEBUG %03d %s";
    
    
    private static SHEFErrors instance;

    private Log logger = LogFactory.getLog(getClass());
    
    private Map<Class<?>,Log> loggers;
    
    private boolean stdOut = LOGOUT;

    /**
     * 
     */
    private SHEFErrors() {
        loggers = new HashMap<Class<?>,Log>();
    }

    /**
     * 
     */
    private static void ensureMap() {
        if(instance == null) {
            instance = new SHEFErrors();
        }
    }
    
    /**
     * 
     * @param clazz Class that is requesting a logger registration.
     */
    public static SHEFErrors registerLogger(Class<?> clazz) {
        ensureMap();
        if(clazz != null) {
            instance.loggers.put(clazz,LogFactory.getLog(clazz));
        }
        return instance;
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     */
    public void error(Class<?> clazz, int errorCode) {
        error(clazz,errorCode,(Throwable) null);
    }
    
    /**
     * 
     * @param clazz
     * @param errorCode
     * @param t
     */
    public void error(Class<?> clazz, int errorCode, Throwable t) {
        Log log = loggers.get(clazz);
        if(log == null) {
            setOutMode(STDOUT);
        }
        if((errorCode >= LOG_MSGS.length) || (errorCode < 0)) {
            if(stdOut) {
                System.out.println("Invalid SHEF errorCode [" + errorCode + "]");
            } else {
                logger.error("Invalid SHEF errorCode [" + errorCode + "]");
            }
        } else {
            if(stdOut) {
                System.out.println(String.format(ERR_FMT,new Date().toString(),errorCode,LOG_MSGS[errorCode]));
            } else {
                String err = String.format(ERROR_FMT,errorCode, LOG_MSGS[errorCode]);
                if(t == null) {
                    log.error(err);
                } else {
                    log.error(err, t);
                }
            }
        }
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     * @param t
     */
    public void error(Class<?> clazz, String message) {
        error(clazz,message,(Throwable) null);
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     * @param t
     */
    public void error(Class<?> clazz, String message, Throwable t) {
        Log log = loggers.get(clazz);
        if (log == null) {
            setOutMode(STDOUT);
        }
        if (stdOut) {
            System.out.println("ERROR " + new Date().toString() + " " + message);
        } else {
            if (t == null) {
                log.error(message);
            } else {
                log.error(message, t);
            }
        }
    }

    
    /**
     * 
     * @param clazz
     * @param errorCode
     */
    public void warning(Class<?> clazz, int errorCode) {
        warning(clazz,errorCode,(Throwable) null);
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     * @param t
     */
    public void warning(Class<?> clazz, int errorCode, Throwable t) {
        Log log = loggers.get(clazz);
        if(log == null) {
            setOutMode(STDOUT);
        }
        if((errorCode >= LOG_MSGS.length) || (errorCode < 0)) {
            if(stdOut) {
                System.out.println("Invalid SHEF errorCode [" + errorCode + "]");
            } else {
                logger.error("Invalid SHEF errorCode [" + errorCode + "]");
            }
        } else {
            if(stdOut) {
                System.out.println(String.format(WRN_FMT,new Date().toString(),errorCode,LOG_MSGS[errorCode]));
            } else {
                String err = String.format(WARN_FMT,errorCode, LOG_MSGS[errorCode]);
                if(t == null) {
                    log.info(err);
                } else {
                    log.info(err, t);
                }
            }
        }
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     * @param t
     */
    public void warning(Class<?> clazz, String message) {
        warning(clazz,message,(Throwable) null);
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     * @param t
     */
    public void warning(Class<?> clazz, String message, Throwable t) {
        Log log = loggers.get(clazz);
        if (log == null) {
            setOutMode(STDOUT);
        }
        if (stdOut) {
            System.out.println("WARNG " + new Date().toString() + " " + message);
        } else {
            if (t == null) {
                log.info(message);
            } else {
                log.info(message, t);
            }
        }
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     * @param t
     */
    public void debug(Class<?> clazz, String data) {
        Log log = loggers.get(clazz);
        if (log == null) {
            setOutMode(STDOUT);
        }
        if (stdOut) {
            System.out.println(String.format("DEBUG %20s %s", new Date().toString(),data));
        } else {
            log.debug(String.format("DEBUG %s", data));
        }
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     * @param t
     */
    public void debug(Class<?> clazz, String message, Throwable t) {
        Log log = loggers.get(clazz);
        if (log == null) {
            setOutMode(STDOUT);
        }
        if (stdOut) {
            System.out.println("DEBUG " + new Date().toString() + " " + message);
        } else {
            if (t == null) {
                log.info(message);
            } else {
                log.info(message, t);
            }
        }
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     * @param t
     */
    public void debug(Class<?> clazz, Throwable t) {
        Log log = loggers.get(clazz);
        if (log == null) {
            setOutMode(STDOUT);
        }
        if (stdOut) {
            System.out.println("DEBUG " + new Date().toString() + " " + t.getMessage());
        } else {
            if (t == null) {
                log.info(t);
            } else {
                log.info(t);
            }
        }
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     */
    public void debug(Class<?> clazz, int errorCode) {
        debug(clazz,errorCode,(Throwable) null);
    }

    /**
     * 
     * @param clazz
     * @param errorCode
     * @param t
     */
    public void debug(Class<?> clazz, int errorCode, Throwable t) {
        Log log = loggers.get(clazz);
        if(log == null) {
            setOutMode(STDOUT);
        }
        if((errorCode >= LOG_MSGS.length) || (errorCode < 0)) {
            if(stdOut) {
                System.out.println("Invalid SHEF errorCode [" + errorCode + "]");
            } else {
                logger.error("Invalid SHEF errorCode [" + errorCode + "]");
            }
        } else {
            if(stdOut) {
                System.out.println(String.format(DBG_FMT,new Date().toString(),errorCode,LOG_MSGS[errorCode]));
            } else {
                String err = String.format(DEBUG_FMT,errorCode, LOG_MSGS[errorCode]);
                if(t == null) {
                    log.info(err);
                } else {
                    log.info(err, t);
                }
            }
        }
    }
    
    public void setOutMode(boolean mode) {
        stdOut = mode;
    }
    
}
