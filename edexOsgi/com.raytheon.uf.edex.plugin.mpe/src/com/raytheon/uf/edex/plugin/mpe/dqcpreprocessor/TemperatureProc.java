package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang.time.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Locdatalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Temperature;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.CommonMPEUtils;
import com.raytheon.uf.edex.plugin.mpe.MpeException;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DatalimitsDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.LocdatalimitsDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.TemperatureDqcDao;

/**
 * Set of DQC temperature processing functions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2016 4623       skorolev    Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 */

public class TemperatureProc {

    private final static Logger logger = LoggerFactory
            .getLogger(TemperatureProc.class);

    private static TemperatureInfo tempInfo;

    private static int tempCount;

    private static double maxValue = PreProcConstants.MISSING_MAX_TEMPERATURE;

    private static double minValue = PreProcConstants.MISSING_MIN_TEMPERATURE;

    /**
     * Time difference in seconds
     */
    private static int diffTime = PreProcConstants.MAX_DIFF_TIME;

    private static int closest6HourSynopticTime = -1;

    private static List<Date> dateList;

    /**
     * This function fills in the temperature array with the data loaded from
     * Temperature table. Including four time periods 00z, 06z, 12z, 18z, and
     * max & min values.
     * 
     * @param startTime
     * @param numDays
     * @return
     * @throws MpeException
     * @throws Exception
     */
    public static void processTemperature(Date startTime, int numDays)
            throws MpeException {

        tempInfo = null;
        TemperatureInfo stationInfo = null;
        dateList = PreProcUtils.getDateArray(startTime, numDays);

        // Load the mpe_temperature_window token value.
        DqcPreProcInit.initTokens(startTime, numDays);

        // Load the temperature data from the IHFS Temperature table.
        List<TemperatureInfo> tempInfoList = DqcPreProcInit.getTempInfoList();

        int tw = DqcInitStruct.getInstance().getSearchWindow() * 60;
        Calendar startCal = TimeUtil.newGmtCalendar(startTime);
        Calendar endCal = TimeUtil.newCalendar(startCal);

        /*
         * Adjust the start time to include the extra look back time for
         * temperature reports.
         */
        startCal.add(Calendar.SECOND, -tw);
        Date strtTime = startCal.getTime();

        /*
         * Compute the ending time of the interval to retrieve temperature data
         * for. Add extra look ahead time.
         */
        endCal.add(Calendar.DAY_OF_YEAR, numDays);
        endCal.add(Calendar.SECOND, tw);
        Date endTime = endCal.getTime();

        /*
         * Adjust the start time to include the extra look back time for
         * temperature reports.
         */
        TemperatureDqcDao tempDao = new TemperatureDqcDao();

        List<Temperature> tempRecList;
        try {
            tempRecList = tempDao.getRecordList(strtTime, endTime);

            Iterator<Temperature> titr = tempRecList.iterator();
            // Loop through db records
            while (titr.hasNext()) {
                Temperature tempRec = titr.next();
                String strCompare = tempRec.getId().getLid();
                char charTsCompare = tempRec.getId().getTs().charAt(1);
                int id = -1;
                for (int i = 0; i < getTempCount(); i++) {
                    TemperatureInfo tempInfo = tempInfoList.get(i);
                    // Compare lid and ts with values from db record
                    if (strCompare.equals(tempInfo.getLid())
                            && charTsCompare == tempInfo.getSource()) {
                        stationInfo = tempInfo;
                        id = i;
                        break;
                    }
                }
                if (stationInfo != null && id >= 0) {
                    Date obsDate = tempRec.getId().getObstime();

                    // Determine the synoptic 6-hour the obstime is closest to.
                    getClosestSynopticHour(obsDate);

                    /*
                     * Retrieve the DQC hydrologic day to compute max/min
                     * temperatures for.
                     */
                    int indexOfDQCDayToComputeMaxMinFor = getDayForMaxMinT(
                            obsDate, numDays);
                    /*
                     * Retrieve the DQC hydrologic day to compute hourly
                     * temperatures for.
                     */
                    int indexOfDQCDayToComputeHourlyFor = getDayForHourlyT(
                            obsDate, numDays);

                    // Retrieve the DQC hydrologic day to store the 12z obs in.
                    int indexOfDQCDayToStore12HrObs = getDayOf12HrObs(obsDate,
                            numDays);

                    // Mismatch pTemperatureNode->obstime value, skip it.
                    if ((indexOfDQCDayToComputeMaxMinFor >= 0)
                            && (indexOfDQCDayToStore12HrObs >= 0)) {

                        processTemperatureRecord(obsDate, tempRec,
                                indexOfDQCDayToComputeMaxMinFor,
                                indexOfDQCDayToComputeHourlyFor,
                                indexOfDQCDayToStore12HrObs, stationInfo);

                    }
                    // Replace default
                    tempInfoList.set(id, stationInfo);
                }
            }
        } catch (Exception e) {
            throw new MpeException("Error getting records from Temperature. ",
                    e);
        }
    }

    /**
     * Process Temperature Record
     * 
     * @param date
     * @param tempRec
     * @param indexOfDQCDayToComputeMaxMinFor
     * @param indexOfDQCDayToComputeHourlyFor
     * @param indexOfDQCDayToStore12HrObs
     * @param stationInfo
     * @throws MpeException
     */
    private static void processTemperatureRecord(Date date,
            Temperature tempRec, int indexOfDQCDayToComputeMaxMinFor,
            int indexOfDQCDayToComputeHourlyFor,
            int indexOfDQCDayToStore12HrObs, TemperatureInfo stationInfo)
            throws MpeException {

        DqcInitStruct initStruct = DqcInitStruct.getInstance();
        // hour of the day
        int obsHour = TimeUtil.newGmtCalendar(date).get(Calendar.HOUR_OF_DAY);
        readTemperatureLimit(stationInfo.getLid(), date);
        if (tempRec.getValue() != PreProcConstants.TEMPERATURE_MISSING
                && tempRec.getValue() >= getMinValue()
                && tempRec.getValue() <= getMaxValue()) {
            /* Process and store this temperature observation. */
            if (tempRec.getId().getExtremum().charAt(0) != 'X'
                    && tempRec.getId().getExtremum().charAt(0) != 'N') {
                // extremum char = Z or P
                /*
                 * Process the hourly max/min values. Max/Min temperature values
                 * are always computed for a hydrologic day which spans 12z-12z.
                 */
                if (stationInfo.getValue().get(indexOfDQCDayToComputeMaxMinFor)
                        .get(6) < tempRec.getValue()) {
                    // record temperature more then value[6]
                    stationInfo.getValue().get(indexOfDQCDayToComputeMaxMinFor)
                            .set(6, tempRec.getValue());
                    if (stationInfo.getValue()
                            .get(indexOfDQCDayToComputeMaxMinFor).get(6) > stationInfo
                            .getValue().get(indexOfDQCDayToComputeMaxMinFor)
                            .get(4)) {
                        // if value[6] more than value[4] move to value[4]
                        stationInfo
                                .getValue()
                                .get(indexOfDQCDayToComputeMaxMinFor)
                                .set(4,
                                        stationInfo
                                                .getValue()
                                                .get(indexOfDQCDayToComputeMaxMinFor)
                                                .get(6));
                    }
                }
                // Process the hourly min
                if (stationInfo.getValue().get(indexOfDQCDayToComputeMaxMinFor)
                        .get(7) > tempRec.getValue()) {
                    stationInfo.getValue().get(indexOfDQCDayToComputeMaxMinFor)
                            .set(7, tempRec.getValue());
                    if (stationInfo.getValue()
                            .get(indexOfDQCDayToComputeMaxMinFor).get(7) < stationInfo
                            .getValue().get(indexOfDQCDayToComputeMaxMinFor)
                            .get(5)) {
                        // if value[7] less than value[5] move to value[5]
                        stationInfo
                                .getValue()
                                .get(indexOfDQCDayToComputeMaxMinFor)
                                .set(5,
                                        stationInfo
                                                .getValue()
                                                .get(indexOfDQCDayToComputeMaxMinFor)
                                                .get(7));
                    }
                }
                /*
                 * store the 00z, 06z, 12z, 18z temperature value Take into
                 * consideration that the 6 hour observed values for a 12z-12z
                 * hydrologic day may be 12z, 18z, 00z, 06z or it may be 18z,
                 * 00z, 06z, 12z.
                 */
                if (getDiffTime() <= initStruct.getSearchWindow() * 60) {
                    /*
                     * The index of the element in the diffTime and value arrays
                     * of the temperature_info structure representing the
                     * synoptic hour being processed.
                     */
                    int hourIndex = initStruct.getHourSlotMap()[initStruct
                            .getHourSlotX()][getClosest6HourSynopticTime()];
                    stationInfo.setSource(tempRec.getId().getTs().charAt(1));

                    /*
                     * Check if the closest6HourSynopticTime is 12. If it is,
                     * then check which DQC day receives this 12hour
                     * observation. This depends on what the specified last 6
                     * hour obstime is.
                     */
                    if (getClosest6HourSynopticTime() == 12) {

                        /*
                         * Use the temperature report which is closest to the
                         * 12Z synoptic hour.
                         */
                        if (getDiffTime() < stationInfo.getDiffTime()
                                .get(indexOfDQCDayToStore12HrObs)
                                .get(hourIndex)) {
                            stationInfo.getValue()
                                    .get(indexOfDQCDayToStore12HrObs)
                                    .set(hourIndex, tempRec.getValue());
                            stationInfo.getDiffTime()
                                    .get(indexOfDQCDayToStore12HrObs)
                                    .set(hourIndex, getDiffTime());
                        }
                    } else {
                        /*
                         * Use the temperature report which is closest to the
                         * 00Z, 06Z, or 18Z synoptic hour.
                         */
                        if (getDiffTime() < stationInfo.getDiffTime()
                                .get(indexOfDQCDayToComputeHourlyFor)
                                .get(hourIndex)) {
                            stationInfo.getValue()
                                    .get(indexOfDQCDayToComputeHourlyFor)
                                    .set(hourIndex, tempRec.getValue());
                            stationInfo.getDiffTime()
                                    .get(indexOfDQCDayToComputeHourlyFor)
                                    .set(hourIndex, getDiffTime());
                        }
                    }
                }
            } else {
                // Process the max and min temperatures. Extremum char = X or N.
                if (obsHour > initStruct.getLocalMidnightInZ()) {
                    if (tempRec.getId().getExtremum().charAt(0) == 'X') {
                        if (stationInfo.getValue()
                                .get(indexOfDQCDayToComputeMaxMinFor).get(4) < tempRec
                                .getValue()) {
                            stationInfo.getValue()
                                    .get(indexOfDQCDayToComputeMaxMinFor)
                                    .set(4, tempRec.getValue());
                        }
                    } else {
                        // process the min temperature value.
                        if (stationInfo.getValue()
                                .get(indexOfDQCDayToComputeMaxMinFor).get(5) > tempRec
                                .getValue()) {
                            stationInfo.getValue()
                                    .get(indexOfDQCDayToComputeMaxMinFor)
                                    .set(5, tempRec.getValue());
                        }
                    }
                }
            }
        }
    }

    /**
     * Check to make sure the temperature value is within acceptable limits.
     * 
     * @param gageId
     * @param date
     * @throws MpeException
     */
    private static void readTemperatureLimit(String gageId, Date date)
            throws MpeException {
        List<Locdatalimits> locDataLimitsList = null;
        List<Datalimits> dataLimitsList = null;

        // Select value from Locdatalimits table first.
        locDataLimitsList = new LocdatalimitsDao().getPeFromDatalimitRecords();

        if (locDataLimitsList != null) {
            Iterator<Locdatalimits> ldlItr = locDataLimitsList.iterator();
            while (ldlItr.hasNext()) {
                Locdatalimits ldlRecord = ldlItr.next();
                if (!ldlRecord.getId().getLid().equals(gageId)) {
                    try {
                        if (CommonMPEUtils.withinDataLimitTimeRange(date,
                                ldlRecord.getId().getMonthdaystart(),
                                ldlRecord.getMonthdayend())) {
                            setMaxValue(ldlRecord.getGrossRangeMax());
                            setMinValue(ldlRecord.getGrossRangeMin());
                            return;
                        }
                    } catch (Exception e) {
                        throw new MpeException(
                                "Error getting records from Locdatalimits: ", e);
                    }
                }
            }
        }
        /*
         * There is no valid data set from Locdatalimits table. select value
         * from Datalimits table.
         */
        dataLimitsList = new DatalimitsDao().getPeFromDatalimitRecords();

        if (dataLimitsList != null) {
            Iterator<Datalimits> dlItr = dataLimitsList.iterator();
            while (dlItr.hasNext()) {
                Datalimits dlRecord = dlItr.next();
                try {
                    if (CommonMPEUtils.withinDataLimitTimeRange(date, dlRecord
                            .getId().getMonthdaystart(), dlRecord
                            .getMonthdayend())) {
                        setMaxValue(dlRecord.getGrossRangeMax());
                        setMinValue(dlRecord.getGrossRangeMin());
                        return;
                    }
                } catch (Exception e) {
                    throw new MpeException(
                            "Error getting records from Datalimits: ", e);
                }
            }
        }
        return;
    }

    /**
     * This function examines the hour of the temperature report and returns the
     * synoptic hour (00,06,12,18) it falls closest to. It also returns the
     * difference in seconds between the observed time and the synoptic hour and
     * the dailyQC day whose 24 hour max/min temperature value this temperature
     * observation will apply to.
     * 
     * @param currDate
     */
    private static void getClosestSynopticHour(Date obsDate) {
        // seconds from day start
        int secTime = getSecFromDayStart(obsDate);

        /* Compute the observation time of the current report being examined. */
        int sixHourTimeDiff = Math.abs(PreProcConstants.SECONDS_PER_6_HOURS
                - secTime);
        int twelveHourTimeDiff = Math.abs(PreProcConstants.SECONDS_PER_12_HOURS
                - secTime);
        int eighteenHourTimeDiff = Math
                .abs(PreProcConstants.SECONDS_PER_18_HOURS - secTime);
        int twentyfourHourTimeDiff = Math
                .abs(PreProcConstants.SECONDS_PER_24_HOURS - secTime);

        /**
         * determine the candidate hour index value and the diff time, and
         * adjust the date index if necessary.
         **/

        /* Determine the synoptic hour the obstime is closest to. */

        if (secTime <= PreProcConstants.SECONDS_PER_3_HOURS) {
            setClosest6HourSynopticTime(0);
            setDiffTime(secTime);
        } else if ((secTime > PreProcConstants.SECONDS_PER_3_HOURS)
                && (secTime <= PreProcConstants.SECONDS_PER_9_HOURS)) {
            setClosest6HourSynopticTime(6);
            setDiffTime(sixHourTimeDiff);
        } else if ((secTime > PreProcConstants.SECONDS_PER_9_HOURS)
                && (secTime <= PreProcConstants.SECONDS_PER_15_HOURS)) {
            setClosest6HourSynopticTime(12);
            setDiffTime(twelveHourTimeDiff);
        } else if ((secTime > PreProcConstants.SECONDS_PER_15_HOURS)
                && (secTime <= PreProcConstants.SECONDS_PER_21_HOURS)) {
            setClosest6HourSynopticTime(18);
            setDiffTime(eighteenHourTimeDiff);
        } else /* Time between 21z and 00z. */
        {
            setClosest6HourSynopticTime(0);
            setDiffTime(twentyfourHourTimeDiff);
        }
    }

    /**
     * Determine the date for max/min temperature record (extremum code as X/N).
     * The hydrologic day is determined as follows: 0 < obstime <=
     * 12+mpe_maxminT_hour_window : Applies to current DQC day.
     * 12+mpe_maxminT_hour_window < obstime <= 24 : Applies to next DQC day.
     * 
     * @return
     */
    private static int getDayForMaxMinT(Date currDate, int numDays) {
        // determine the candidate date index value
        int dateIndex = -1;
        for (int i = 0; i < numDays; i++) {
            /* Compare the YYYY-MM-DD portion of the dates. */
            if (DateUtils.isSameDay(currDate, dateList.get(i))) {
                dateIndex = i;
                break;
            }
        }
        if (dateIndex != -1) {
            int obsTime = getSecFromDayStart(currDate);
            if (obsTime > PreProcConstants.SECONDS_PER_12_HOURS
                    + DqcInitStruct.getInstance().getTemperatureHourWindow()
                    * PreProcConstants.SECONDS_PER_HOUR) {
                dateIndex++;

                if (dateIndex == numDays) {
                    dateIndex = -1;
                }
            }
        }
        return dateIndex;
    }

    /**
     * Determine the date for hourly temperature record (extremum code as Z).
     * 
     * The hydrologic day is determined as follows: 0 < obstime <= 12 : Applies
     * to current DQC day. 12 < obstime <= 24 : Applies to next DQC day.
     * 
     * @param date
     * @param numDays
     * @return
     */
    private static int getDayForHourlyT(Date date, int numDays) {
        int dateIndex = -1;
        for (int i = 0; i < numDays; i++) {
            if (DateUtils.isSameDay(date, dateList.get(i))) {
                dateIndex = i;
                break;
            }
        }
        if (dateIndex != -1) {
            /*
             * Compute the observation time of the current report being
             * examined.
             */
            int secNum = getSecFromDayStart(date);
            if (secNum > PreProcConstants.SECONDS_PER_12_HOURS) {
                dateIndex++;

                if (dateIndex == numDays) {
                    dateIndex = -1;
                }
            }
        }
        return dateIndex;
    }

    /**
     * Using the specified times of the 6 hour observed temperatures (12z, 18z,
     * 00z, 06z or 18z, 00z, 06z, 12z) this function returns the index of the
     * DQC day the 12 hour observation belongs to.
     * 
     * @param date
     * @param numDays
     * @return
     */
    private static int getDayOf12HrObs(Date date, int numDays) {
        int dateIndex = -1;
        for (int i = 0; i < numDays; i++) {
            if (DateUtils.isSameDay(date, dateList.get(i))) {
                dateIndex = i;
                break;
            }
        }
        if (dateIndex != -1) {
            if (DqcInitStruct.getInstance().getDqcEnding6hourObstimeFlag() == PreProcConstants.DQC_PREPROCESSOR_ENDING_OBSTIME_06Z) {
                dateIndex++;
                if (dateIndex == numDays) {
                    dateIndex = -1;
                }
            }
        }
        return dateIndex;
    }

    /**
     * Gets amount of seconds from start of the day.
     * 
     * @param date
     * @return sec
     */
    private static int getSecFromDayStart(Date date) {
        Calendar hCal = TimeUtil.newGmtCalendar(date);
        TimeUtil.minCalendarFields(hCal, Calendar.HOUR_OF_DAY, Calendar.MINUTE,
                Calendar.SECOND, Calendar.MILLISECOND);
        long difTime = date.getTime() - TimeUtil.calendarToGMT(hCal).getTime();
        return (int) TimeUnit.MILLISECONDS.toSeconds(difTime);
    }

    public static TemperatureInfo getTempInfo() {
        return tempInfo;
    }

    public static void setTempInfo(TemperatureInfo tempInfo) {
        TemperatureProc.tempInfo = tempInfo;
    }

    public static int getTempCount() {
        return tempCount;
    }

    public static void setTempCount(int tempCount) {
        TemperatureProc.tempCount = tempCount;
    }

    public static double getMaxValue() {
        return maxValue;
    }

    public static void setMaxValue(double maxValue) {
        TemperatureProc.maxValue = maxValue;
    }

    public static double getMinValue() {
        return minValue;
    }

    public static void setMinValue(double minValue) {
        TemperatureProc.minValue = minValue;
    }

    /**
     * @return time difference in seconds
     */
    public static int getDiffTime() {
        return diffTime;
    }

    public static void setDiffTime(int diffTime) {
        TemperatureProc.diffTime = diffTime;
    }

    public static int getClosest6HourSynopticTime() {
        return closest6HourSynopticTime;
    }

    public static void setClosest6HourSynopticTime(int closest6HourSynopticTime) {
        TemperatureProc.closest6HourSynopticTime = closest6HourSynopticTime;
    }
}
