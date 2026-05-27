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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.UnitConverter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecordTransform;
import com.raytheon.uf.common.dataquery.db.OrderField.ResultOrder;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.ThresholdMgr;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.FogMonitor;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.SafeSeasMonitor;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.SnowMonitor;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler;
import com.raytheon.uf.edex.pointdata.PointDataQuery;

import systems.uom.common.USCustomary;

/**
 * Creates AlertViz messages for Fog, SAFESEAS and SNOW monitor.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 20, 2015  5115     skorolev  Initial creation
 * Aug 13, 2018  6670     randerso  Corrected checks for windChill and
 *                                  visibility. Store last threat info in
 *                                  cluster lock. Significant code cleanup.
 * May 21, 2019  7689     randerso  Reprocess recent FSSObsRecords at startup or
 *                                  when thesholds are changed.
 * May 24, 2019  7689     randerso  Convert visibility to nautical miles for
 *                                  safeseas.
 * Jul 09, 2020  7689     randerso  Ensure latest status is sent to AlertViz
 *                                  when AlertViz is started.
 * Sep 01, 2020  7689     randerso  Fix issue with clean database startup
 *
 * </pre>
 *
 * @author skorolev
 */

public class FSSObsAlarmMgr implements MonitorConfigListener {
    private static final Logger logger = LoggerFactory
            .getLogger(FSSObsAlarmMgr.class);

    private static final String PLUGIN_NAME = "com.raytheon.uf.edex.plugin.fssobs";

    private static final String FSS_OBS_MONITOR = "FSSObsMonitor";

    private static final String FOG_THRESHOLDS = "DefaultFogMonitorThresholds.xml";

    private static final String SS_THRESHOLDS = "DefaultSSMonitorThresholds.xml";

    private static final String SNOW_THRESHOLDS = "DefaultSnowMonitorThresholds.xml";

    private static final UnitConverter statuteToNauticalMiles = USCustomary.MILE
            .getConverterTo(USCustomary.NAUTICAL_MILE);

    /** Map for current configuration managers. */
    private static final Map<AppName, FSSObsAlarmMgr> instanceMap = new HashMap<>();

    private enum FSSThreat {
        NO_DATA(Priority.EVENTB, "No data available"),
        BELOW(Priority.EVENTA, "Data below hazardous levels"),
        APPROACHING(Priority.PROBLEM, "Data approaching hazardous levels"),
        WITHIN(Priority.SIGNIFICANT, "Data within hazardous levels");

        private Priority priority;

        private String message;

        FSSThreat(Priority priority, String message) {
            this.priority = priority;
            this.message = message;
        }

        /**
         * @return the priority
         */
        public Priority getPriority() {
            return priority;
        }

        /**
         * @return the message
         */
        public String getMessage() {
            return message;
        }
    }

    /**
     * Specialized lock hander with following behaviors:
     *
     * <pre>
     * 1. Does not set lastExecution when locked
     * 2. Sets extraInfo at unlock
     * 3. Allows lastExecution to be set to desired value at unlock
     * </pre>
     */
    private static class FSSClusterLockHandler
            extends CurrentTimeClusterLockHandler {
        private long lastExecution;

        /**
         * Construct a lock handle with specified time out that sets extraInfo
         * at unlock
         *
         * @param timeOutOverride
         */
        public FSSClusterLockHandler(long timeOutOverride) {
            super(timeOutOverride, false);
        }

        /**
         * @param lastExecution
         *            the new lastExecution time to be set at unlock
         */
        public void setLastExecution(long lastExecution) {
            this.lastExecution = lastExecution;
        }

        @Override
        public boolean updateLock(ClusterTask ct) {
            // don't update lastExecution on lock
            ct.setRunning(true);
            return true;
        }

        @Override
        public void unlock(ClusterTask ct, boolean clearTime) {
            super.unlock(ct, clearTime);

            // set lastExecution to desired value on unlock
            ct.setLastExecution(lastExecution);
        }
    }

    private FSSObsMonitorConfigurationManager areaConfig = null;

    protected ThresholdMgr threshMgr = null;

    private final AppName appName;

    private boolean initializing;

    /** Temporary place for red direction angle threat **/
    private static float redFrom = 0;

    /** Temporary place for yellow direction angle threat **/
    private static float yellowFrom = 0;

    private FSSThreat lastThreat;

    /**
     * Get the appropriate instance for the particular monitor
     *
     * @param monitor
     * @return the instance
     */
    public static final synchronized FSSObsAlarmMgr getInstance(
            AppName monitor) {
        FSSObsAlarmMgr instance = instanceMap.get(monitor);
        if (instance == null) {
            instance = new FSSObsAlarmMgr(monitor);
            instanceMap.put(monitor, instance);
        }
        return instance;
    }

    /**
     * Send latest monitor status to alertviz
     */
    public static void sendLatestMonitorStatus() {
        for (AppName appName : AppName.values()) {
            // getInstance to ensure FSSObsAlarmMgr has been initialized
            getInstance(appName);

            // Get latest status from cluster task table and send it to alertviz
            ClusterTask ct = ClusterLockUtils.lookupLock(FSS_OBS_MONITOR,
                    appName.name());

            FSSThreat threat = FSSThreat.NO_DATA;
            try {
                if (ct.getExtraInfo() != null) {
                    threat = FSSThreat.valueOf(ct.getExtraInfo());
                }
            } catch (Exception e) {
                logger.warn(String.format(
                        "Unrecognized FSSThreat value \"%s\" found in cluster lock for %s.",
                        ct.getExtraInfo(), appName.name()), e);
            }
            sendToAlertViz(appName, threat, null);
        }
    }

    private static void sendToAlertViz(AppName appName, FSSThreat threat,
            String details) {
        EDEXUtil.sendMessageAlertViz(threat.getPriority(), PLUGIN_NAME,
                appName.name(), "MONITOR", threat.getMessage(), details, null);
    }

    /**
     * Private constructor
     */
    private FSSObsAlarmMgr(AppName appName) {
        this.appName = appName;

        initialize();
    }

    private void initialize() {
        this.initializing = true;

        this.areaConfig = FSSObsMonitorConfigurationManager
                .getInstance(appName);
        this.areaConfig.addListener(this);

        List<String> threshKeys = new ArrayList<>();
        switch (appName) {
        case FOG:
            for (FogMonitor fogMon : FogMonitor.values()) {
                threshKeys.add(fogMon.getXmlKey());
            }
            setThresholdMgr(FOG_THRESHOLDS);
            break;
        case SAFESEAS:
            for (SafeSeasMonitor ssMon : SafeSeasMonitor.values()) {
                threshKeys.add(ssMon.getXmlKey());
            }
            setThresholdMgr(SS_THRESHOLDS);
            break;
        case SNOW:
            for (SnowMonitor snowMon : SnowMonitor.values()) {
                threshKeys.add(snowMon.getXmlKey());
            }
            setThresholdMgr(SNOW_THRESHOLDS);
            break;
        default:
            throw new IllegalArgumentException("Unknown AppName: " + appName);
        }

        double timeWindow = this.areaConfig.getTimeWindow();
        Date oldestTime = new Date(System.currentTimeMillis()
                - (long) (timeWindow * TimeUtil.MILLIS_PER_HOUR));
        try {

            PointDataQuery pdq = new PointDataQuery(FSSObsRecord.PLUGIN_NAME);
            pdq.setParameters(FSSObsRecordTransform.FSSOBS_PARAMS_LIST);
            pdq.getQuery().addQueryParam(FSSObsRecord.REFTIME_ID, oldestTime,
                    QueryOperand.GREATERTHANEQUALS);
            pdq.getQuery().addOrder(FSSObsRecord.REFTIME_ID, ResultOrder.ASC);
            pdq.requestAllLevels();
            PointDataContainer pdc = pdq.execute();
            FSSObsRecord[] records = FSSObsRecordTransform.toFSSObsRecords(pdc);

            lastThreat = FSSThreat.NO_DATA;
            for (FSSObsRecord rec : records) {
                calculateThreat(rec);
            }
            sendToAlertViz(appName, lastThreat, null);
        } catch (Exception e) {
            logger.error("Error instantiating FSSObsDAO", e);
        }
    }

    /**
     * Re-initialize this instance when configuration has changed
     */
    public void reInitialize() {
        this.areaConfig.removeListener(this);
        this.threshMgr.removeListener(this);
        this.initialize();
    }

    /**
     * Sets Threshold Manager for monitor.
     *
     * @param fileName
     */
    private void setThresholdMgr(String fileName) {
        try {
            if (this.threshMgr != null) {
                this.threshMgr.dispose();
            }

            this.threshMgr = new ThresholdMgr(this.appName,
                    DataUsageKey.MONITOR, fileName);
            this.threshMgr.addListener(this);
        } catch (Exception e) {
            logger.error(String.format(
                    "Error loading monitor thresholds for %s", appName), e);
        }
    }

    /**
     * Sends AlertViz Message.
     *
     * @param fssObsRec
     * @param areaList
     *            List of areas from area config.
     */
    public void sendAlertVizMsg(FSSObsRecord fssObsRec) {
        FSSThreat threat = calculateThreat(fssObsRec);
        if (threat != null) {
            sendToAlertViz(appName, threat, fssObsRec.getDataURI());
        }
    }

    /**
     * Calculates priority threat for monitor.
     *
     * @param fssObsRec
     *            FSSObs record
     * @return Priority
     */
    private FSSThreat calculateThreat(FSSObsRecord fssObsRec) {

        String stnName = fssObsRec.getStationId();
        List<String> areaList = areaConfig.getAreaByStationId(stnName);
        if (areaList.isEmpty()) {
            return null;
        }

        // Max threat for all zones
        FSSThreat retVal = null;
        FSSThreat finalThreat = FSSThreat.NO_DATA;
        for (String areaId : areaList) {
            // Max threat for all monitor parameters
            FSSThreat areaThreat = FSSThreat.NO_DATA;
            List<String> params = threshMgr.getMonitorParameters();
            for (String param : params) {
                double red = threshMgr.getRedValue(areaId, param);
                double yellow = threshMgr.getYellowValue(areaId, param);
                FSSThreat pThreat = getParamThreat(fssObsRec, param, red,
                        yellow);
                // skip not used threats
                if (param.contains(".dirFrom")) {
                    continue;
                }
                if (pThreat.compareTo(areaThreat) > 0) {
                    areaThreat = pThreat;
                }
            }

            if (areaThreat.compareTo(finalThreat) > 0) {
                finalThreat = areaThreat;
            }
        }

        lastThreat = FSSThreat.NO_DATA;
        long lastObsTime = 0;
        FSSClusterLockHandler lockHandler = new FSSClusterLockHandler(
                TimeUtil.MILLIS_PER_SECOND);
        ClusterTask ct = ClusterLockUtils.lock(FSS_OBS_MONITOR, appName.name(),
                lockHandler, true);
        try {
            if (ct.getLockState() != LockState.FAILED) {
                if (ct.getExtraInfo() != null) {
                    if (!initializing) {
                        lastObsTime = ct.getLastExecution();
                        lastThreat = FSSThreat.valueOf(ct.getExtraInfo());
                    }
                }
            } else {
                logger.error("Failed to get last threat info for " + appName);
            }

            long refTime = fssObsRec.getTimeObs().getTimeInMillis();
            long windowTime = lastObsTime + (long) (areaConfig.getTimeWindow()
                    * TimeUtil.MILLIS_PER_HOUR);

            // compare finalThreat to lastThreat
            int compare = finalThreat.compareTo(lastThreat);
            if (compare > 0) {
                // threat increased

                // update last threat and time
                lastObsTime = refTime;
                lastThreat = finalThreat;

                // return updated threat to cause message to be sent
                retVal = lastThreat;

            } else if (compare == 0) {
                // threat unchanged

                // update last time
                lastObsTime = refTime;

            } else {
                // threat decreased

                // if reftime outside window of last threat
                if (refTime > windowTime) {

                    // update last threat and time
                    lastObsTime = refTime;
                    lastThreat = finalThreat;

                    // return updated threat to cause message to be sent
                    retVal = lastThreat;
                }
            }

        } finally {
            // update the last threat info and time
            lockHandler.setExtraInfo(lastThreat.name());
            lockHandler.setLastExecution(lastObsTime);
            if (!ClusterLockUtils.unlock(ct, false)) {
                logger.error(
                        "Unable to update last threat info for " + appName);
            } else {
                initializing = false;
            }
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
    private FSSThreat getParamThreat(FSSObsRecord rec, String param, double red,
            double yellow) {
        FSSThreat threat = FSSThreat.NO_DATA;
        float fred = (float) red;
        float fyellow = (float) yellow;
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
            // Visibility value in 16ths
            float visValue = rec.getVisibility() * 16;

            // SafeSeas thresholds are in nautical miles
            if (appName.equals(AppName.SAFESEAS)) {
                visValue = (float) statuteToNauticalMiles.convert(visValue);
            }
            threat = lessThan(visValue, fred, fyellow);
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
            redFrom = fred;
            yellowFrom = fyellow;
            // intentional fall through to next case
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
            redFrom = fred;
            yellowFrom = fyellow;
            // intentional fall through to next case
        case MonitorConfigConstants.MON_SWELL_SEC_DIR_TO:
            threat = dirThreat(rec.getSecondarySwellWaveDir().floatValue(),
                    redFrom, yellowFrom, fred, fyellow);
            break;
        case MonitorConfigConstants.MON_METEO_TEMP:
            threat = lessThan(rec.getTemperature(), fred, fyellow);
            break;
        case MonitorConfigConstants.MON_METEO_WIND_CHILL:
            threat = lessThan(rec.getWindChill(), fred, fyellow);
            break;
        case MonitorConfigConstants.MON_METEO_SNOW_DEPTH:
            threat = moreThan(rec.getSnowDepth(), fred, fyellow);
            break;
        default:
            logger.warn("Unknown threat parameter detected: " + param);
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
     * @return threat level
     */
    private FSSThreat moreThan(float paramVal, float red, float yellow) {
        if (paramVal != ObConst.MISSING) {
            if (paramVal >= red) {
                return FSSThreat.WITHIN;
            } else if (paramVal >= yellow) {
                return FSSThreat.APPROACHING;
            } else {
                return FSSThreat.BELOW;
            }
        }
        return FSSThreat.NO_DATA;
    }

    /**
     * Gets threat for min values
     *
     * @param paramVal
     * @param red
     * @param yellow
     * @return threat value
     */
    private FSSThreat lessThan(float paramVal, float red, float yellow) {
        if (paramVal != ObConst.MISSING) {
            if (paramVal <= red) {
                return FSSThreat.WITHIN;
            } else if (paramVal <= yellow) {
                return FSSThreat.APPROACHING;
            } else {
                return FSSThreat.BELOW;
            }
        }
        return FSSThreat.NO_DATA;
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
    private FSSThreat dirThreat(float paramVal, float redFrom, float yellowFrom,
            float redTo, float yellowTo) {
        if (paramVal != ObConst.MISSING) {
            if (redFrom < redTo) {
                if (paramVal > redFrom && paramVal < redTo) {
                    return FSSThreat.WITHIN;
                }
            }
            if (redFrom > redTo) {
                if (paramVal > redFrom || paramVal < redTo) {
                    return FSSThreat.WITHIN;
                }
            }
            if (yellowFrom < yellowTo) {
                if (paramVal > yellowFrom && paramVal < yellowTo) {
                    return FSSThreat.APPROACHING;
                }
            }
            if (yellowFrom > yellowTo) {
                if (paramVal > yellowFrom || paramVal < yellowTo) {
                    return FSSThreat.APPROACHING;
                }
            }
            return FSSThreat.BELOW;
        }
        return FSSThreat.NO_DATA;
    }

    /**
     * @return the ThresholdMgr
     */
    public ThresholdMgr getThreshMgr() {
        return threshMgr;
    }

    @Override
    public void configChanged(MonitorConfigEvent fce) {
        this.reInitialize();
    }

}