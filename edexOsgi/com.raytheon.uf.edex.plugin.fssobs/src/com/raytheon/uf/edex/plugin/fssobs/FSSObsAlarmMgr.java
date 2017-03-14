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
package com.raytheon.uf.edex.plugin.fssobs;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.ThresholdMgr;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.FogMonitor;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.SafeSeasMonitor;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.SnowMonitor;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Creates AlarmViz messages for Fog, SAFESEAS and SNOW monitor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2015 5115       skorolev    Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */

public class FSSObsAlarmMgr {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FSSObsAlarmMgr.class);

    private final static String FOG_THRESHOLDS = "DefaultFogMonitorThresholds.xml";

    private final static String SS_THRESHOLDS = "DefaultSSMonitorThresholds.xml";

    private final static String SNOW_THRESHOLDS = "DefaultSnowMonitorThresholds.xml";

    private FSSObsMonitorConfigurationManager areaConfig = null;

    private static FSSObsRecord fssRec = null;

    protected ThresholdMgr fogThreshMgr = null;

    protected ThresholdMgr ssThreshMgr = null;

    protected ThresholdMgr snowThreshMgr = null;

    /** Full path of Monitor threat configuration file */
    private String fullThresholdFileName = null;

    /** Name of plug-in */
    private static String pluginName = null;

    /** Map for current configuration managers. */
    private final static Map<AppName, FSSObsAlarmMgr> instanceMap = new HashMap<>();

    /** Monitoring parameters **/
    private static List<String> threshKeys = new ArrayList<String>();

    /** Monitoring areas **/
    private static List<String> areaIDs;

    /** List of threat priorities **/
    private final static List<Priority> priorities = new ArrayList<Priority>();

    /** Threat calculated last time **/
    private static Map<String, Integer> lastThreat = new HashMap<String, Integer>();

    /** Last observation time in milliseconds **/
    private static long lastObsTime = 0;

    /** Temporary place for red direction angle threat **/
    private static float redFrom = 0;

    /** Temporary place for yellow direction angle threat **/
    private static float yellowFrom = 0;

    /**
     * Private constructor
     */
    public FSSObsAlarmMgr(AppName monitor) {
        setAreaConfig(FSSObsMonitorConfigurationManager.getInstance(monitor));
        setPluginName(monitor.name());
        switch (monitor) {
        case FOG:
            threshKeys.clear();
            for (FogMonitor fogMon : FogMonitor.values()) {
                threshKeys.add(fogMon.getXmlKey());
            }
            this.fogThreshMgr = setThreasholdMgr(FOG_THRESHOLDS);
            break;
        case SAFESEAS:
            threshKeys.clear();
            for (SafeSeasMonitor ssMon : SafeSeasMonitor.values()) {
                threshKeys.add(ssMon.getXmlKey());
            }
            this.ssThreshMgr = setThreasholdMgr(SS_THRESHOLDS);
            break;
        case SNOW:
            threshKeys.clear();
            for (SnowMonitor snowMon : SnowMonitor.values()) {
                threshKeys.add(snowMon.getXmlKey());
            }
            this.snowThreshMgr = setThreasholdMgr(SNOW_THRESHOLDS);
            break;
        default:
            statusHandler.handle(Priority.WARN,
                    "Unknown monitor" + monitor.name());
            break;
        }
        lastThreat.put(getPluginName(), 0);
        initThreatPriorities();
    }

    /**
     * Instance.
     * 
     * @param monitor
     * @return
     */
    public final static synchronized FSSObsAlarmMgr getInstance(AppName monitor) {
        FSSObsAlarmMgr instance = instanceMap.get(monitor);
        if (instance == null) {
            instance = new FSSObsAlarmMgr(monitor);
            instanceMap.put(monitor, instance);
        }
        return instance;
    }

    /**
     * Sets Threshold Manager for monitor.
     * 
     * @param defaultThresholdsFilename
     * @return
     */
    private ThresholdMgr setThreasholdMgr(String defaultThresholdsFilename) {
        fullThresholdFileName = getDefaultThresholdFilePath()
                + defaultThresholdsFilename;
        ThresholdMgr mgr = new ThresholdMgr(fullThresholdFileName);
        if (fileNameValid(fullThresholdFileName) == true) {
            mgr.readThresholdXml();
        } else {
            String defaultFile = getMonitorDefaultThresholdFilePath()
                    + defaultThresholdsFilename;
            areaIDs = getAreaConfig().getAreaList();
            mgr.createMonitorConfigFromDefaults(defaultFile, areaIDs,
                    getThreshKeys());
            mgr.saveThresholdXml();
        }
        return mgr;
    }

    /**
     * File validation.
     * 
     * @param thresholdFileName
     * @return
     */
    private boolean fileNameValid(String thresholdFileName) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile locFile = pm.getLocalizationFile(context,
                thresholdFileName);
        return locFile.getFile().exists();
    }

    /**
     * Gets the path where the monitor thresholds XML file is contained.
     * 
     * @param plugin
     *            Name of plug-in
     * @return the defaultThresholdFilePath
     */
    public String getDefaultThresholdFilePath() {
        // fog/threshold/monitor/
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();
        sb.append(pluginName.toLowerCase()).append(fs);
        sb.append("threshold").append(fs);
        sb.append("monitor").append(fs);
        return sb.toString();
    }

    /**
     * Gets the path where the default monitor thresholds XML file is contained.
     * 
     * @return File path.
     */
    public String getMonitorDefaultThresholdFilePath() {
        // monitoring/fog/
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder("monitoring");
        sb.append(fs);
        sb.append(pluginName.toLowerCase()).append(fs);
        return sb.toString();
    }

    /**
     * Initiates Priorities for monitor's alarm images.
     */
    private void initThreatPriorities() {
        // EVENTB - Suggested reading but non-required
        // EVENTA - An important message, but non-crucial
        // PROBLEM - A problem occurred
        // SIGNIFICANT - May not be an "emergency" but still very important
        priorities.add(0, Priority.EVENTB);
        priorities.add(1, Priority.EVENTA);
        priorities.add(2, Priority.PROBLEM);
        priorities.add(3, Priority.SIGNIFICANT);

    }

    /**
     * Sends AlertViz Message.
     * 
     * @param thMgr
     *            ThresholdMgr
     * @param fssObsRec
     * @param winTime
     *            TimeWindow from area config.
     * @param areaList
     *            List of areas from area config.
     */
    public void sendAlertVizMsg(ThresholdMgr thMgr, FSSObsRecord fssObsRec,
            double winTime, List<String> areaList) {
        // Hours to millis
        long tw = (long) (TimeUtil.MILLIS_PER_HOUR * winTime);
        setFssRec(fssObsRec);
        String sourceKey = getPluginName();
        String message = "";
        Priority priority = calculateThreat(thMgr, fssObsRec, tw, areaList);
        if (priority != null) {
            // Threat has changed
            switch (priority) {
            case SIGNIFICANT:
                message = "Data within hazardous levels";
                break;
            case PROBLEM:
                message = "Data approaching hazardous levels";
                break;
            case EVENTA:
                message = "Data below hazardous levels";
                break;
            case EVENTB:
                message = "No data available";
                break;
            default:
                statusHandler.warn("Get unused priority: " + priority.name());
            }
            EDEXUtil.sendMessageAlertViz(priority,
                    "com.raytheon.uf.edex.plugin.fssobs", sourceKey, "MONITOR",
                    message, null, null);
            statusHandler.debug(sourceKey + ": " + message);
        }
    }

    /**
     * Calculates priority threat for monitor.
     * 
     * @param thMgr
     *            monitor threshold manager
     * @param fssObsRec
     *            FSSObs record
     * @param period
     *            TimeWindow from area config in millis
     * @param areaList
     *            List of areas from area config.
     * @return Priority
     */
    private static Priority calculateThreat(ThresholdMgr thMgr,
            FSSObsRecord fssObsRec, long period, List<String> areaList) {
        Priority retVal = null;
        // Max threat for all zones
        int finalThreat = 0;
        for (String areaId : areaList) {
            // Max threat for all monitor parameters
            int areaThreat = 0;
            List<String> params = thMgr.getMonitorParameters();
            for (String param : params) {
                double red = thMgr.getRedValue(areaId, param);
                double yellow = thMgr.getYellowValue(areaId, param);
                int pThreat = getParamThreat(param, red, yellow);
                // skip not used threats
                if (param.contains(".dirFrom")) {
                    continue;
                }
                areaThreat = Math.max(areaThreat, pThreat);
            }
            finalThreat = Math.max(finalThreat, areaThreat);
        }

        if (lastThreat.get(getPluginName()) != finalThreat) {
            // current observation time in milliseconds
            long refTime = getFssRec().getTimeObs().getTimeInMillis();
            if ((refTime - period) < getLastObsTime()) {
                // got data inside time window
                lastThreat.put(getPluginName(),
                        Math.max(lastThreat.get(getPluginName()), finalThreat));
            } else {
                // start a new time window
                lastThreat.put(getPluginName(), finalThreat);
            }
            setLastObsTime(refTime);
            // 0 => EVENTB
            // 1 => EVENTA
            // 2 => PROBLEM
            // 3 => SIGNIFICANT
            retVal = priorities.get(lastThreat.get(getPluginName()));
        }
        return retVal;
    }

    /**
     * Gets Parameter's Threat.
     * 
     * @param param
     * @param red
     * @param yellow
     * @return
     */
    private static int getParamThreat(String param, double red, double yellow) {
        int threat = 0;
        float fred = (float) red;
        float fyellow = (float) yellow;
        FSSObsRecord rec = getFssRec();
        switch (param) {
        case MonitorConfigConstants.MON_METEO_WIND_SPEED:
            threat = moreThan(rec.getWindSpeed(), fred, fyellow);
            break;
        case MonitorConfigConstants.MON_METEO_PEAK_WIND:
            threat = moreThan(rec.getMaxWindSpeed(), fred, fyellow);
            break;
        case MonitorConfigConstants.MON_METEO_GUST_SPEED:
            threat = moreThan(rec.getWindGust(), fred, fyellow);
            break;
        case MonitorConfigConstants.MON_METEO_WAVE_HT:
            threat = moreThan(rec.getWaveHeight().floatValue(), fred, fyellow);
            break;
        case MonitorConfigConstants.MON_METEO_VIS:
            // Visibility value in units of "nautical miles/16". Used to compare
            // thresholds.
            threat = lessThan(rec.getVisibility() * 16f, fred, fyellow);
            break;
        case MonitorConfigConstants.MON_SWELL_PRIM_HT:
            threat = moreThan(rec.getPrimarySwellWaveHeight().floatValue(),
                    fred, fyellow);
            break;
        case MonitorConfigConstants.MON_SWELL_PRIM_PD:
            threat = moreThan(rec.getPrimarySwellWavePeriod(), fred, fyellow);
            break;
        case MonitorConfigConstants.MON_SWELL_PRIM_DIR_FROM:
            // in Azimuth degrees
            setRedFrom(fred);
            setYellowFrom(fyellow);
        case MonitorConfigConstants.MON_SWELL_PRIM_DIR_TO:
            threat = dirThreat(rec.getPrimarySwellWaveDir().floatValue(),
                    redFrom, yellowFrom, fred, fyellow);
            break;
        case MonitorConfigConstants.MON_SWELL_SEC_HT:
            threat = moreThan(rec.getSecondarySwellWaveHeight().floatValue(),
                    fred, fyellow);
            break;
        case MonitorConfigConstants.MON_SWELL_SEC_PD:
            threat = moreThan(rec.getSecondarySwellWavePeriod(), fred, fyellow);
            break;
        case MonitorConfigConstants.MON_SWELL_SEC_DIR_FROM:
            // in Azimuth degrees
            setRedFrom(fred);
            setYellowFrom(fyellow);
        case MonitorConfigConstants.MON_SWELL_SEC_DIR_TO:
            threat = dirThreat(rec.getSecondarySwellWaveDir().floatValue(),
                    redFrom, yellowFrom, fred, fyellow);
            break;
        case MonitorConfigConstants.MON_METEO_TEMP:
            threat = lessThan(rec.getTemperature(), fred, fyellow);
            break;
        case MonitorConfigConstants.MON_METEO_WIND_CHILL:
            threat = moreThan(rec.getWindChill(), fred, fyellow);
            break;
        case MonitorConfigConstants.MON_METEO_SNOW_DEPTH:
            threat = moreThan(rec.getSnowDepth(), fred, fyellow);
            break;
        default:
            statusHandler.handle(Priority.WARN,
                    "Unknown threat parameter detected: " + param);
            break;
        }
        return threat;
    }

    /**
     * Gets threat for max values
     * 
     * @param paramVal
     * @param red
     * @param yellow
     * @return threat value
     */
    private static int moreThan(float paramVal, float red, float yellow) {
        if (paramVal != ObConst.MISSING) {
            if (paramVal >= red) {
                return 3;
            } else if (paramVal >= yellow) {
                return 2;
            } else {
                return 1;
            }
        }
        return 0;
    }

    /**
     * Gets threat for min values
     * 
     * @param paramVal
     * @param red
     * @param yellow
     * @return threat value
     */
    private static int lessThan(float paramVal, float red, float yellow) {
        if (paramVal != ObConst.MISSING) {
            if (paramVal <= red) {
                return 3;
            } else if (paramVal <= yellow) {
                return 2;
            } else {
                return 1;
            }
        }
        return 0;
    }

    /**
     * Gets threat for directions.
     * 
     * @param paramVal
     * @param redFrom
     *            saved value
     * @param yellowFrom
     *            saved value
     * @param redTo
     * @param yellowTo
     * @return threat value
     */
    private static int dirThreat(float paramVal, float redFrom,
            float yellowFrom, float redTo, float yellowTo) {
        if (paramVal != ObConst.MISSING) {
            if (redFrom < redTo) {
                if (paramVal > redFrom && paramVal < redTo) {
                    return 3;
                }
            }
            if (redFrom > redTo) {
                if (paramVal > redFrom || paramVal < redTo) {
                    return 3;
                }
            }
            if (yellowFrom < yellowTo) {
                if (paramVal > yellowFrom && paramVal < yellowTo) {
                    return 2;
                }
            }
            if (yellowFrom > yellowTo) {
                if (paramVal > yellowFrom || paramVal < yellowTo) {
                    return 2;
                }
            }
            return 1;
        }
        return 0;
    }

    public ThresholdMgr getFogThreshMgr() {
        return fogThreshMgr;
    }

    public ThresholdMgr getSsThreshMgr() {
        return ssThreshMgr;
    }

    public ThresholdMgr getSnowThreshMgr() {
        return snowThreshMgr;
    }

    public static FSSObsRecord getFssRec() {
        return fssRec;
    }

    public FSSObsMonitorConfigurationManager getAreaConfig() {
        return areaConfig;
    }

    public void setAreaConfig(FSSObsMonitorConfigurationManager areaConfig) {
        this.areaConfig = areaConfig;
    }

    public void setFssRec(FSSObsRecord fssRec) {
        FSSObsAlarmMgr.fssRec = fssRec;
    }

    public static String getPluginName() {
        return pluginName;
    }

    public static void setPluginName(String pluginName) {
        FSSObsAlarmMgr.pluginName = pluginName;
    }

    /**
     * @return the threshKeys
     */
    public static List<String> getThreshKeys() {
        return threshKeys;
    }

    public static long getLastObsTime() {
        return lastObsTime;
    }

    public static void setLastObsTime(long lastObsTime) {
        FSSObsAlarmMgr.lastObsTime = lastObsTime;
    }

    /**
     * @return the areaIDs
     */
    public static List<String> getAreaIDs() {
        return areaIDs;
    }

    public static float getRedFrom() {
        return redFrom;
    }

    public static float getYellowFrom() {
        return yellowFrom;
    }

    public static void setRedFrom(float redFrom) {
        FSSObsAlarmMgr.redFrom = redFrom;
    }

    public static void setYellowFrom(float yellowFrom) {
        FSSObsAlarmMgr.yellowFrom = yellowFrom;
    }
}