package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * This function initializes the station data arrays.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2016 4623       skorolev    Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 */
public class DqcPreProcInit {

    private final static Logger logger = LoggerFactory
            .getLogger(DqcPreProcInit.class);

    private static DqcInitStruct initStruct = DqcInitStruct.getInstance();

    public static List<PrecipInfo> precipInfoList = new ArrayList<>();

    public static List<TemperatureInfo> tempInfoList = new ArrayList<>();

    private static final char Z = 'Z';

    /**
     * Initiates tokens
     * 
     * @param startTime
     * @param numDays
     */
    public static void initTokens(Date startTime, int numDays) {

        String windowToken = AppsDefaults.getInstance().getToken(
                PreProcConstants.MPE_TEMPERATURE_WINDOW);
        if (windowToken == null) {
            logger.warn(
                    "WARNING: Token \"{}\" is not defined. Application will use \"default value -- {} hours.",
                    PreProcConstants.MPE_TEMPERATURE_WINDOW,
                    initStruct.getSearchWindow());
        } else {
            logger.info(
                    "  STATUS: Set the temperature window value to {} minutes.",
                    initStruct.getSearchWindow());
        }

        String maxTempToken = AppsDefaults.getInstance().getToken(
                PreProcConstants.MPE_MAXMINT_HOUR_WINDOW);
        if (maxTempToken == null) {
            logger.warn(
                    "WARNING: Token \"{}\" is not defined. Application will use default value -- {} hours.",
                    PreProcConstants.MPE_MAXMINT_HOUR_WINDOW,
                    initStruct.getTemperatureHourWindow());
        } else {
            logger.info(
                    "  STATUS: Set the max/min temperature hour window value to {} hours.",
                    initStruct.getTemperatureHourWindow());
        }

        /*
         * get the token get_ending_6hour_obstime, the default value is 06Z
         * which means to generate temperature point level1 data at 12Z, 18Z,
         * 00Z and 06Z. If the token is set as 12Z, then generate temperature
         * point level1 data at 18Z, 00Z, 06Z and 12Z.
         */
        int endingObsTime = PreProcConstants.DEFAULT_ENDING_6HOUR_OBS_TIME;
        String tokenValue = AppsDefaults.getInstance().getToken(
                PreProcConstants.DQC_ENDING_6HOUR_OBSTIME);
        if (tokenValue != null && tokenValue.length() > 0) {
            endingObsTime = Integer.parseInt(tokenValue.trim());
        }
        if (endingObsTime == 6) {
            initStruct
                    .setDqcEnding6hourObstimeFlag(PreProcConstants.DQC_PREPROCESSOR_ENDING_OBSTIME_06Z);
            initStruct.ending6HourObsTime = endingObsTime;
        } else {
            /* The ending obstime is 12z. */
            initStruct
                    .setDqcEnding6hourObstimeFlag(PreProcConstants.DQC_PREPROCESSOR_ENDING_OBSTIME_12Z);
            initStruct.ending6HourObsTime = endingObsTime;
        }
        logger.info(
                "  STATUS: The token dqc_ending_6hour_obstime is set as {}.",
                endingObsTime);
        /*
         * Initialize the arrays containing the six hour slot indexes for the
         * two possible 6 hour ending observation times.
         */
        /* 6 Hour Ending Obstime of 06Z. */
        int dpeo6 = 0;
        int dpeo12 = 1;
        if (!PreProcConstants.DQC_PREPROCESSOR_ENDING_OBSTIME_06Z) {
            dpeo6 = 1;
        }
        if (PreProcConstants.DQC_PREPROCESSOR_ENDING_OBSTIME_12Z) {
            dpeo12 = 0;
        }

        initStruct.hourSlotMap[dpeo6][12] = 0;
        initStruct.hourSlotMap[dpeo6][18] = 1;
        initStruct.hourSlotMap[dpeo6][0] = 2;
        initStruct.hourSlotMap[dpeo6][6] = 3;

        /* 6 Hour Ending Obstime of 12z. */
        initStruct.hourSlotMap[dpeo12][18] = 0;
        initStruct.hourSlotMap[dpeo12][0] = 1;
        initStruct.hourSlotMap[dpeo12][6] = 2;
        initStruct.hourSlotMap[dpeo12][12] = 3;
    }

    /**
     * Initiate precipInfoMap
     * 
     * @param stationSize
     * @param numDays
     */
    public static void initPrecipInfoArray(int stationSize, int numDays) {
        precipInfoList = new ArrayList<>(stationSize);
        for (int i = 0; i < stationSize; i++) {
            PrecipInfo pi = new PrecipInfo();
            for (int j = 0; j < numDays; j++) {
                pi.pPPD.put(j, PreProcConstants.PRECIP_MISSING);
                List<Double> pppqList = new ArrayList<>(4);
                for (int k = 0; k < 4; k++) {
                    pppqList.add(k, PreProcConstants.PRECIP_MISSING);
                }
                pi.pPPQ.put(j, pppqList);
                pi.setSource(Z);
                pi.pPPQPE.add(j, PreProcConstants.PP);
            }
            precipInfoList.add(i, pi);
        }
        setPrecipInfoList(precipInfoList);
    }

    /**
     * Initiate tempInfoMap.
     * 
     * @param stationSize
     * @param numDays
     */
    public static void initTempInfoArray(int stationSize, int numDays) {
        tempInfoList = new ArrayList<>(stationSize);
        for (int i = 0; i < stationSize; i++) {
            TemperatureInfo ti = new TemperatureInfo();
            for (int j = 0; j < numDays; j++) {
                List<Double> valList = new ArrayList<>(8);
                for (int k = 0; k < 4; k++) {
                    valList.add(k, PreProcConstants.TEMPERATURE_MISSING);
                }
                valList.add(4, PreProcConstants.MISSING_MAX_TEMPERATURE);
                valList.add(5, PreProcConstants.MISSING_MIN_TEMPERATURE);
                valList.add(6, PreProcConstants.MISSING_MAX_TEMPERATURE);
                valList.add(7, PreProcConstants.MISSING_MIN_TEMPERATURE);
                ti.value.put(j, valList);
                List<Integer> dt = new ArrayList<>(6);
                for (int k = 0; k < 6; k++) {
                    dt.add(k, PreProcConstants.MAX_DIFF_TIME);
                }
                ti.diffTime.put(j, dt);
            }
            tempInfoList.add(i, ti);
        }
        setTempInfoList(tempInfoList);
    }

    /**
     * Release PrecipInfo Array
     */
    public static void releasePrecipArray() {
        if (!getPrecipInfoList().isEmpty()) {
            setPrecipInfoList(new ArrayList<PrecipInfo>());
        }
    }

    /**
     * Release TemperatureInfo Array
     */
    public static void releaseTempArray() {
        if (getTempInfoList() != null && !getTempInfoList().isEmpty()) {
            setTempInfoList(new ArrayList<TemperatureInfo>());
        }
    }

    public static List<PrecipInfo> getPrecipInfoList() {
        return precipInfoList;
    }

    public static void setPrecipInfoList(List<PrecipInfo> precipInfoList) {
        DqcPreProcInit.precipInfoList = precipInfoList;
    }

    public static List<TemperatureInfo> getTempInfoList() {
        return tempInfoList;
    }

    public static void setTempInfoList(List<TemperatureInfo> tempInfoList) {
        DqcPreProcInit.tempInfoList = tempInfoList;
    }

}
