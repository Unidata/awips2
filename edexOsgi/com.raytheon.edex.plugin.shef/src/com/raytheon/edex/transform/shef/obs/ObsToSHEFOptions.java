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
package com.raytheon.edex.transform.shef.obs;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.COMMON_STATIC;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Reads options available from the metar2shef command line string, as well as
 * the metar.cfg.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2010            jkorman     Initial creation
 * ======================================
 * AWIPS2 DR Work
 * 20120918           1185 jkorman     Added save to archive capability.     
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ObsToSHEFOptions {
    // 1 minute update delta.
    private static final long UPDATE_DELTA = 60L * 1000L;

    private static final String METAR_CFG = "metar.cfg";

    private static final String ERROR_1_FMT = "Could not create {%s} context for file \"%s\"";

    private static final String ERROR_2_FMT = "File %s does not exist";

    public static final String CONFIG_DIR = "shef";

    private static final String BASE_CONTEXT = "BASE";

    private static final String SITE_CONTEXT = "SITE";

    private static final Pattern CFG_ITEMS = Pattern
            .compile("^([+-])(SAOOUT|ERRORFILE|SHEFPASS)$");

    private static final Pattern CMD_DIRECTIVES = Pattern
            .compile("^\\.((?:begin|end)_(?:names|sm_alias|pc_reset))$");

    private static final String SAO_OUT = "SAOOUT";

    private static final String ERROR_FILE = "ERRORFILE";

    private static final String SHEF_PASS = "SHEFPASS";

    private static final String BEGIN_ALIAS = "begin_sm_alias";

    private static final String END_ALIAS = "end_sm_alias";

    private static final String BEGIN_NAMES = "begin_names";

    private static final String END_NAMES = "end_names";

    private static final String BEGIN_PC_RESET = "begin_pc_reset";

    private static final String END_PC_RESET = "end_pc_reset";

    public static final String OPT_NONE = "none";

    public static final String OPT_STRIPICAO = "optStripICAO";

    public static final String OPT_COLLECTIVES = "optCollectives";

    public static final String OPT_CONTINOUS = OPT_NONE;

    public static final String OPT_DEBUG = "optDebug";

    public static final String OPT_NOCONVERT = "optNoConvert";

    public static final String OPT_CONFIG_PATH = "optConfigPath";

    public static final String OPT_CONFIG_METAR_IDS = "optConfigMetarIds";

    public static final String OPT_HOWOLD = "optHowOld";

    public static final String OPT_INPUT_FILE = "optInputFile";

    public static final String OPT_OB_DECODE = OPT_NONE;

    public static final String OPT_OB = OPT_NONE;

    public static final String OPT_DECODE = OPT_NONE;

    public static final String OPT_KNOTS = "optWindInKnots";

    public static final String OPT_ASOS_TS = "optASOS_TS";

    public static final String OPT_LOG_CURRENT = "optLogCurrent";

    public static final String OPT_METRIC = "optMetric";

    public static final String OPT_SPECI = "optSpeci";

    public static final String OPT_ZA_1HOUR_PRECIP = "optZeroAuto1HourPrecip";

    public static final String OPT_ZA_6HOUR_PRECIP = "optZeroAuto6HourPrecip";

    public static final String OPT_ZA_24HOUR_PRECIP = "optZeroAuto24HourPrecip";

    public static final String OPT_Z_6HOUR_PRECIP = "optZero6HourPrecip";

    public static final String OPT_Z_24HOUR_PRECIP = "optZero24HourPrecip";

    public static final String OPT_DECODE_12Z_PRECIP = "optDecodePrecipAt12Z";

    public static final String OPT_PEDTSEP = "optPEDTSEP";

    public static final String OPT_PCT = "optPCT";

    public static final String OPT_WIND_HUNDREDS = "optWindInHundreds";

    public static final String OPT_ROUND_OBS_TIME = "optRoundObsTime";

    public static final String OPT_CHECK_ALIAS = "optCheckAliasId";

    public static final String OPT_TYPE_SRC_V = "optTypeSrcV";

    public static final String OPT_STRIP = "optStrip";

    public static final String OPT_TEST = "optTest";

    public static final String OPT_VERBOSE = "optVerbose";

    public static final String OPT_WMO_ENVELOPE = OPT_NONE;

    public static final String OPT_DATE_OVERRIDE = "optDateOverRide";

    public static final String OPT_CENTURY = "optCentury";

    public static final String OPT_NO_HR_TRACE = "optNoHourTrace";
    
    private String cfgFileName=null;
    // private static class PCReset {
    //
    // private final String stationId;
    //
    // private final Integer resetTime;
    //
    // public PCReset(String id, Integer time) {
    // stationId = id;
    // resetTime = time;
    // }
    //
    // /**
    // * @return the stationId
    // */
    // public String getStationId() {
    // return stationId;
    // }
    //
    // /**
    // * @return the resetTime
    // */
    // public Integer getResetTime() {
    // return resetTime;
    // }
    // }

    private static class CmdLineData {

        final String key;

        final String option;

        final Integer numOptions;

        final Class<?> clazz;

        public CmdLineData(String key, String option, Integer numOptions,
                Class<?> clazz) {
            this.key = key;
            this.option = option;
            this.numOptions = numOptions;
            this.clazz = clazz;
        }

        public String toString() {
            return key + ":" + option + ":" + numOptions;
        }
    }

    private static final Map<String, CmdLineData> CMDS = new HashMap<String, CmdLineData>();
    static {
        // -a
        // strip off the first character in the id of a metar ob
        CMDS.put("-a", new CmdLineData("-a", OPT_STRIPICAO, 0, Boolean.class));
        // -b
        // turn on decoder to accept collectives
        CMDS.put("-b", new CmdLineData("-b", OPT_COLLECTIVES, 0, Boolean.class));
        // -c
        // -----
        CMDS.put("-c", new CmdLineData("-c", OPT_CONTINOUS, 0, Boolean.class));
        // -d
        // turn on debug option
        CMDS.put("-d", new CmdLineData("-d", OPT_DEBUG, 0, Boolean.class));
        // -e
        // do not convert metric temps to english
        CMDS.put("-e", new CmdLineData("-e", OPT_NOCONVERT, 0, Boolean.class));
        // -fcfg filename
        // input configuration path/filename from command line
        CMDS.put("-fcfg", new CmdLineData("-fcfg", OPT_CONFIG_PATH, 1,
                String.class));
        // -g
        // read list of metar ids to process from the cfg file
        CMDS.put("-g", new CmdLineData("-g", OPT_CONFIG_METAR_IDS, 0,
                Boolean.class));
        // -howold #
        // how old a file needs to be in seconds before processing
        CMDS.put("-howold", new CmdLineData("-howold", OPT_HOWOLD, 1,
                Integer.class));
        // -i filename
        // input filename from command line
        CMDS.put("-i", new CmdLineData("-i", OPT_INPUT_FILE, 1, String.class));
        // -j1
        // output each metar in it's own file - j1=ob + decode
        CMDS.put("-j1",
                new CmdLineData("-j1", OPT_OB_DECODE, -1, Boolean.class));
        // -j2
        // output each metar in it's own file j2=ob
        CMDS.put("-j2", new CmdLineData("-j2", OPT_OB, -1, Boolean.class));
        // -j3
        // output each metar in it's own file j3=decode - no shef
        CMDS.put("-j3", new CmdLineData("-j3", OPT_DECODE, -1, Boolean.class));
        // -kt
        // output wind speeds in units of knots (default mph)
        CMDS.put("-kt", new CmdLineData("-kt", OPT_KNOTS, 0, Boolean.class));
        // -l
        // TS for ASOS stations = RO, TS for all other sites = RV\n\n
        CMDS.put("-l", new CmdLineData("-l", OPT_ASOS_TS, 0, Boolean.class));
        // -log
        // turn on log of current product and ob being processed
        CMDS.put("-log", new CmdLineData("-log", OPT_LOG_CURRENT, 0,
                Boolean.class));
        // -m
        // assume SAO obs are in metric units - translate into english units
        CMDS.put("-m", new CmdLineData("-m", OPT_METRIC, 0, Boolean.class));
        // -nospeci
        // do not decode special observations
        CMDS.put("-nospeci", new CmdLineData("-nospeci", OPT_SPECI, 0,
                Boolean.class));
        // -p12z
        // only decode 24 precipitation in the 12Z window
        CMDS.put("-p12z", new CmdLineData("-p12z", OPT_DECODE_12Z_PRECIP, 0,
                Boolean.class));
        // -p1
        // generate a 0 value for automated stations if no precip value is
        // present
        CMDS.put("-p1", new CmdLineData("-p1", OPT_ZA_1HOUR_PRECIP, 0,
                Boolean.class));
        // -p6
        // generate a 0 value for PPQ for 6 hour periods if 6$$$/ group is
        // missing
        // for automatic stations
        CMDS.put("-p6", new CmdLineData("-p6", OPT_ZA_6HOUR_PRECIP, 0,
                Boolean.class));
        // -p24
        // decode only 24 precip amounts for times 1140-1230Z
        CMDS.put("-p24", new CmdLineData("-p24", OPT_ZA_24HOUR_PRECIP, 0,
                Boolean.class));
        // -pall6
        // generate a 0 value for PPQ for auto 6 hour periods if 6$$$/ group is
        // missing
        // for all stations
        CMDS.put("-pall6", new CmdLineData("-pall6", OPT_Z_6HOUR_PRECIP, 0,
                Boolean.class));
        // -pall24
        // decode only 24 auto precip amounts for times 1140-1230Z
        CMDS.put("-pall24", new CmdLineData("-pall24", OPT_Z_24HOUR_PRECIP, 0,
                Boolean.class));
        // -pedtsed
        //
        CMDS.put("-pedtsep", new CmdLineData("-pedtsed", OPT_PEDTSEP, 0,
                Boolean.class));
        // -pct #
        // -----tolerance in minutes for pc reset times (default = 2)
        CMDS.put("-pct", new CmdLineData("-pct", OPT_PCT, 1, Integer.class));
        // -q1
        // output wind direction in hundreds & not tens
        CMDS.put("-q1", new CmdLineData("-q1", OPT_WIND_HUNDREDS, 0,
                Boolean.class));
        // -round
        // round observation time to the whole hour for non special obs
        CMDS.put("-round", new CmdLineData("-round", OPT_ROUND_OBS_TIME, 0,
                Boolean.class));
        // -salias
        // check alias id table for sm ids
        CMDS.put("-salias", new CmdLineData("-salias", OPT_CHECK_ALIAS, 0,
                Boolean.class));
        // -sw
        // switch source of PEDTSEP from 'Z' to 'V' for testing METAR
        CMDS.put("-sw", new CmdLineData("-sw", OPT_TYPE_SRC_V, 0,
                Boolean.class));
        // -strip
        // convert bad ascii values to blanks
        CMDS.put("-strip", new CmdLineData("-strip", OPT_STRIP, 0,
                Boolean.class));
        // -t
        // turn on test option
        CMDS.put("-t", new CmdLineData("-t", OPT_TEST, 0, Boolean.class));
        // -v
        // turn on verbose option
        CMDS.put("-v", new CmdLineData("-v", OPT_VERBOSE, 0, Boolean.class));
        // -w
        // list wmo and ZCZC line in output
        CMDS.put("-w", new CmdLineData("-w", OPT_WMO_ENVELOPE, -1,
                Boolean.class));
        // -x #
        // override system ccyymmdyhrmn with value = #
        CMDS.put("-x", new CmdLineData("-x", OPT_DATE_OVERRIDE, 1,
                Integer.class));
        // -y2k
        // output century in SHEF output
        CMDS.put("-y2k", new CmdLineData("-y2k", OPT_CENTURY, 0, Boolean.class));
        
        // -notrace
        // turn off trace precip for 1 hour data.
        CMDS.put("-notrace", new CmdLineData("-notrace", OPT_NO_HR_TRACE, 0, Boolean.class));
    }

    private Log logger = LogFactory.getLog(getClass());

    private Map<String, Object> options = new HashMap<String, Object>();

    private String optConfigContext = SITE_CONTEXT;

    private String metarInFile = null; // /tmp/queue/metar/in

    private String metarOutFile = null; // /tmp/queue/metar/out

    private String metarErrFile = null; // /tmp/queue/metar/err

    private boolean saoOut = false;

    private boolean errorFile = false;

    private boolean shefPass = false;

    private Set<String> optNames = new HashSet<String>();

    private Properties optAlias = new Properties();

    private Properties optPCReset = new Properties();

    private Set<String> optPE = new HashSet<String>();

    private Properties optGenProps = new Properties();

    private boolean loaded = false;

    private final boolean localized;

    private long updateTime = 0;

    /**
     * 
     * @param cmdLine
     */
    public ObsToSHEFOptions(String cmdLine, boolean useLocalized) {
        initOptions();
        parseCommandLine(cmdLine);
        if (useLocalized) {
        	if (cfgFileName==null){
        		readConfig(METAR_CFG, optConfigContext);
        	} else {
        		readConfig(cfgFileName, optConfigContext);
        	}
            updateTime = System.currentTimeMillis();
        }
        localized = useLocalized;
    }

    private void initOptions() {

        Iterator<String> entries = CMDS.keySet().iterator();
        while (entries.hasNext()) {
            String key = entries.next();
            CmdLineData cmd = CMDS.get(key);
            if (cmd != null) {
                if (OPT_NONE.equals(cmd.option)) {

                } else {
                    if (cmd.numOptions > -1) {
                        Object o = null;
                        if (Boolean.class.equals(cmd.clazz)) {
                            o = Boolean.FALSE;
                        } else if (Integer.class.equals(cmd.clazz)) {
                        } else if (String.class.equals(cmd.clazz)) {
                        }
                        options.put(cmd.option, o);
                    }
                }
            }
        }
    }

    /**
     * 
     * 
     * @param cmdLine
     */
    private void parseCommandLine(String cmdLine) {

        if (cmdLine != null) {
            String[] cmds = cmdLine.trim().split("-");
            if (cmds != null) {
                for (String s : cmds) {
                    String[] parts = s.split(" ");
                    String key = null;
                    if (parts.length > 0) {
                        key = "-" + parts[0];
                    }
                    if (CMDS.containsKey(key)) {
                        CmdLineData cmd = CMDS.get(key);
                        if (cmd.numOptions > -1) {
                            if (Boolean.class.equals(cmd.clazz)) {
                                options.put(cmd.option, Boolean.TRUE);
                            } else if (Integer.class.equals(cmd.clazz)) {
                                try {
                                    Integer n = new Integer(parts[1]);
                                    options.put(cmd.option, n);
                                } catch (NumberFormatException nfe) {
                                    System.out
                                            .println("Could not parse value ["
                                                    + parts[1]
                                                    + "] for option [" + key
                                                    + "]");
                                }
                            } else if (String.class.equals(cmd.clazz)) {
                                options.put(cmd.option, parts[1]);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * -a strip off the first character in the id of a metar ob
     * 
     * @return the optStripICAO
     */
    public boolean isOptStripICAO() {
        return (Boolean) options.get(OPT_STRIPICAO);
    }

    /**
     * -b turn on decoder to accept collectives"
     * 
     * @return the optCollectives
     */
    public boolean isOptCollectives() {
        return (Boolean) options.get(OPT_COLLECTIVES);
    }

    // -c seconds run decoder in continuous mode every # of seconds"

    /**
     * -d turn on debug information"
     * 
     * @return the optDebug
     */
    public boolean isOptDebug() {
        return (Boolean) options.get(OPT_DEBUG);
    }

    /**
     * -fcfg filename input configuration path/filename from command line
     * 
     * @return the optConfigPath
     */
    public String getOptConfigPath() {
        return (String) options.get(OPT_CONFIG_PATH);
    }

    /**
     * -i filename provide filename from command line
     * 
     * @return the optInputFile
     */
    public String getOptInputFile() {
        return (String) options.get(OPT_INPUT_FILE);
    }

    /**
     * -kt output wind speeds in units of knots (default mph)"
     * 
     * @return the optWindInKnots
     */
    public boolean isOptWindInKnots() {
        return (Boolean) options.get(OPT_KNOTS);
    }

    /**
     * -l SHEF TS for ASOS stations = RO, TS for all other sites = RV
     * @return
     */
    public boolean isOptASOS_TS() {
        return (Boolean) options.get(OPT_ASOS_TS);
    }
    
    /**
     * -log turn on log for current product and observation
     * 
     * @return the optLogCurrent
     */
    public boolean isOptLogCurrent() {
        return (Boolean) options.get(OPT_LOG_CURRENT);
    }

    /**
     * -m assume SAO (Canadian) obs are in metric and convert to english"
     * 
     * @return the optMetric
     */
    public boolean isOptMetric() {
        return (Boolean) options.get(OPT_METRIC);
    }

    /**
     * -nospeci do not decode special (SPECI) obs"
     * 
     * @return the optSpeci
     */
    public boolean isOptSpeci() {
        return (Boolean) options.get(OPT_SPECI);
    }

    /**
     * -p1 generate a 0 value for PPH for 1 hour period if precip group is
     * missing for automatic stations only"
     * 
     * @return the optZero1HourPrecip
     */
    public boolean isOptZeroAuto1HourPrecip() {
        return (Boolean) options.get(OPT_ZA_1HOUR_PRECIP);
    }

    /**
     * -p6 generate a 0 value for PPQ for 6 hour periods if precip group is
     * missing for automatic stations only"
     * 
     * @return the optZero6HourPrecip
     */
    public boolean isOptZeroAuto6HourPrecip() {
        return (Boolean) options.get(OPT_ZA_6HOUR_PRECIP);
    }

    /**
     * -pall6 generate a 0 value for PPQ for 6 hour periods if precip group is
     * missing for all stations"
     * 
     * @return the optZero6HourPrecip
     */
    public boolean isOptZero6HourPrecip() {
        return (Boolean) options.get(OPT_Z_6HOUR_PRECIP);
    }

    /**
     * -p24 generate a 0 value for PPD for 24 hour period if precip group is
     * missing for automatic stations only in window 1140-1230Z"
     * 
     * @return the optZero24HourPrecip
     */
    public boolean isOptZeroAuto24HourPrecip() {
        return (Boolean) options.get(OPT_ZA_24HOUR_PRECIP);
    }

    /**
     * -pall24 generate a 0 value for PPD for 24 hour periods if precip group is
     * missing for all stations in window 1140-1230Z"
     * 
     * @return the optZero24HourPrecip
     */
    public boolean isOptZero24HourPrecip() {
        return (Boolean) options.get(OPT_Z_24HOUR_PRECIP);
    }

    /**
     * -p12z decode 24 precipitation amounts in the window 1140-1230 default is
     * to decode 24 amounts for all ob times
     * 
     * @return the optDecodePrecipAt12Z
     */
    public boolean isOptDecodePrecipAt12Z() {
        return (Boolean) options.get(OPT_DECODE_12Z_PRECIP);
    }
    
    /**
     * -sw
     * @return
     */
    public boolean isOptTypeSrcV() {
        return (Boolean) options.get(OPT_TYPE_SRC_V);
    }
    

    /**
     * -pct 
     * 
     * @return
     */
    public Integer getOptPCT() {
        return (Integer) options.get(OPT_PCT);
    }
    
    /**
     * -q1 output wind direction in hundreds & not tens
     * 
     * @return the optWindInHundreths
     */
    public boolean isOptWindInHundreds() {
        return (Boolean) options.get(OPT_WIND_HUNDREDS);
    }

    /**
     * -round round time to nearest whole hour for non-special obs"
     * 
     * @return the optRoundObsTime
     */
    public boolean isOptRoundObsTime() {
        return (Boolean) options.get(OPT_ROUND_OBS_TIME);
    }

    /**
     * -salias turn on check of alias id table for sm observations
     * 
     * @return the optCheckAliasId
     */
    public boolean isOptCheckAliasId() {
        return (Boolean) options.get(OPT_CHECK_ALIAS);
    }

    /**
     * -t execute test version"
     * 
     * @return the optTest
     */
    public boolean isOptTest() {
        return (Boolean) options.get(OPT_TEST);
    }

    /**
     * -v turn on verbose option"
     * 
     * @return the optVerbose
     */
    public boolean isOptVerbose() {
        return (Boolean) options.get(OPT_VERBOSE);
    }

    // -w output wmo line and ZCZC CCCNNNXXX with output

    /**
     * -x # override system ccyymmdyhrmn to the value = #
     * 
     * @return the optDateOverRide
     */
    public String getOptDateOverRide() {
        return (String) options.get(OPT_DATE_OVERRIDE);
    }

    /**
     * -y2k output century in SHEF product
     * 
     * @return the optCentury
     */
    public boolean isOptCentury() {
        return (Boolean) options.get(OPT_CENTURY);
    }

    /**
     * -notrace 
     * 
     * @return Should trace precip not be reported for hourly metars.
     */
    public boolean isOptNoTrace() {
        return (Boolean) options.get(OPT_NO_HR_TRACE);
    }
    
    // *****************************************************

    /**
     * Check a name to see if it exists in option names. The check only occurs
     * if the -g option is set, otherwise true is returned.
     * 
     * @param name
     * @return
     */
    public boolean checkName(String name) {
        boolean result = true;
        if ((Boolean) options.get(OPT_CONFIG_METAR_IDS)) {
            result = optNames.contains(name);
        }
        return result;
    }

    /**
     * 
     * @param name
     * @return
     */
    public String checkAlias(String name) {
        return optAlias.getProperty(name, null);
    }

    /**
     * Retrieve the reset time for a given station id if it is in the PCReset
     * properties.
     * 
     * @param name
     *            A target station namel
     * @return The PC Reset time if it exists, returns a null reference if not
     *         found.
     */
    public Integer getPCReset(String name) {
        Integer resetTime = null;
        String s = optPCReset.getProperty(name);
        if (s != null) {
            try {
                resetTime = Integer.parseInt(s.trim());
            } catch (NumberFormatException nfe) {
                resetTime = null;

            }
        }
        return resetTime;
    }

    /**
     * Set an ad hoc property key, value pair.
     * 
     * @param key
     * @param value
     */
    public void setGeneralProperty(String key, String value) {
        optGenProps.setProperty(key, value);
    }

    /**
     * Get an ad hoc property value.
     * 
     * @param key
     * @param value
     * @return The value of the property given by key.
     */
    public String getGeneralProperty(String key) {
        return optGenProps.getProperty(key);
    }

    /**
     * Get an ad hoc property value. If not found the supplied default value is
     * returned.
     * 
     * @param key
     * @param defaultValue
     *            String value to be returned if the specified property is not
     *            found.
     * @return The value of the property given by key.
     */
    public String getGeneralProperty(String key, String defaultValue) {
        String value = optGenProps.getProperty(key);
        if (value == null) {
            value = defaultValue;
        }
        return value;
    }

    /**
     * 
     * @param pe
     * @return
     */
    public boolean checkPE(String pe) {
        boolean check = false;
        if (pe != null) {
            StringTokenizer st = new StringTokenizer(pe, "|");
            while (st.hasMoreTokens()) {
                check |= optPE.contains(st.nextToken());
            }
        }
        return check;
    }

    /**
     * 
     */
    public void updateCommandLine(String commandLine) {
        options.clear();
        initOptions();
        parseCommandLine(commandLine);
    }

    /**
     * Check if the metar.cfg needs to be reread.
     */
    public void updateOptions() {
        long cTime = System.currentTimeMillis() - updateTime;
        if (cTime > UPDATE_DELTA) {
            if (loaded && localized) {
            	if (cfgFileName==null) {
            		readConfig(METAR_CFG, optConfigContext);
            	} else {
            		readConfig(cfgFileName, optConfigContext);
            	}
                updateTime = System.currentTimeMillis();
            }
        }
    }

    private void readConfig(String configFile, String localContext) {

        try {
            File configDir = null;

            IPathManager manager = PathManagerFactory.getPathManager();
            if (manager != null) {
                LocalizationContext context = manager.getContext(COMMON_STATIC,
                        LocalizationLevel.valueOf(localContext));
                if (context != null) {
                    configDir = manager.getFile(context, CONFIG_DIR);
                    if (configDir.exists()) {
                        File srcFile = new File(configDir, configFile);

                        BufferedReader reader = null;
                        try {
                            reader = new BufferedReader(new FileReader(srcFile));

                            readConfig(reader);
                        } catch (FileNotFoundException fnf) {
                            // If we were trying to get site context and failed,
                            // try base.
                            if (SITE_CONTEXT.equals(optConfigContext)) {
                                // Retry from a base context.
                                optConfigContext = BASE_CONTEXT;
                                if (cfgFileName==null) {
                                	readConfig(METAR_CFG, optConfigContext);
                                }else{
                                	readConfig(cfgFileName, optConfigContext);
                                }
                            }
                        }
                    } else {
                        logger.error(String.format(ERROR_2_FMT,
                                configDir.getPath()));
                    }
                } else {
                    logger.error(String.format(ERROR_1_FMT, localContext,
                            configFile));
                }
            } else {
                logger.error("Could not create PathManagerFactory");
            }
        } catch (Exception e) {
            logger.error("Exception setting METAR2SHEF options", e);
        }
    }

    /**
     * 
     * @param reader
     * @throws IOException
     */
    public void readConfig(BufferedReader reader) throws IOException {
        String line = null;
        if ((line = reader.readLine()) != null) {
            metarInFile = line;
        }
        if ((line = reader.readLine()) != null) {
            metarOutFile = line;
        }
        if ((line = reader.readLine()) != null) {
            metarErrFile = line;
        }
        optAlias.clear();
        boolean readAlias = false;
        optNames.clear();
        boolean readNames = false;
        optPCReset.clear();
        boolean readReset = false;
        int lineCount = 3;
        while ((line = reader.readLine()) != null) {
            lineCount++;
            Matcher m = CFG_ITEMS.matcher(line.trim());
            if (m.matches()) {
                boolean val = ("+".equals(m.group(1)));
                if (SAO_OUT.equals(m.group(2)) && lineCount == 4) {
                    saoOut = val;
                } else if (ERROR_FILE.equals(m.group(2)) && lineCount == 5) {
                    errorFile = val;
                } else if (SHEF_PASS.equals(m.group(2)) && lineCount == 6) {
                    shefPass = val;
                }
            } else if (lineCount == 7) {
                // Check for PE List
                // TA UP SD UD US TX TN PPT PPQ PPH PPD PA TAIRZR TAIRZH TAIRZP
                // TAIRZY
                StringTokenizer st = new StringTokenizer(line, " ");
                optPE.clear();
                while (st.hasMoreTokens()) {
                    optPE.add(st.nextToken());
                }
            } else {
                m = CMD_DIRECTIVES.matcher(line.trim());
                if (m.matches()) {
                    String s = m.group(1);
                    if (BEGIN_ALIAS.equals(s)) {
                        readAlias = true;
                    } else if (END_ALIAS.equals(s)) {
                        readAlias = false;
                    } else if (BEGIN_NAMES.equals(s)) {
                        readNames = true;
                    } else if (END_NAMES.equals(s)) {
                        readNames = false;
                    } else if (BEGIN_PC_RESET.equals(s)) {
                        readReset = true;
                    } else if (END_PC_RESET.equals(s)) {
                        readReset = false;
                    }
                } else {
                    if (readNames) {
                        optNames.add(line.trim());
                    } else if (readAlias) {
                        String[] parts = line.trim().split("\\s+");
                        optAlias.setProperty(parts[0], parts[1]);
                    } else if (readReset) {
                        String[] parts = line.trim().split("\\s+");
                        optPCReset.setProperty(parts[0], parts[1]);
                    }
                }
            }
        }
        if ((line = reader.readLine()) != null) {
            metarErrFile = line;
        }
        loaded = true;
    }

    public void setCfgFileName(String fileName) {
    	cfgFileName=fileName;
    }
    
    public String toString() {

        StringBuilder sb = new StringBuilder();

        Iterator<String> keys = options.keySet().iterator();
        while (keys.hasNext()) {
            String key = keys.next();
            sb.append(String.format("%25s [%s] = %s", key, "--",
                    options.get(key)));
            if (keys.hasNext()) {
                sb.append('\n');
            }
        }
        return sb.toString();
    }
}
