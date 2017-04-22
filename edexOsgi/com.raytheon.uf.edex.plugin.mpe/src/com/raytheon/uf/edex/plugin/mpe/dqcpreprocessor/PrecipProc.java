package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.time.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.tables.Dailypp;
import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.Locdatalimits;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.common.mpe.util.PrecipUtil;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.CommonMPEUtils;
import com.raytheon.uf.edex.plugin.mpe.MpeException;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DailyPPDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DatalimitsDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.HourlyPCDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.HourlyPPDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.IngestFilterDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.LocdatalimitsDao;

/**
 * Set of DQC precipitation processing functions.
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
public class PrecipProc {
    private final static Logger logger = LoggerFactory
            .getLogger(PrecipProc.class);

    private static int precipCount;

    private static PrecipTotal totalPrecip;

    /**
     * This function fills into the precip array with loaded the PPD data from
     * DailyPP table.
     * 
     * @param startTime
     * @param numDays
     * @throws MpeException
     */
    public static void processDailyPP(Date startTime, int numDays)
            throws MpeException {
        PrecipInfo stationInfo = null;
        List<PrecipInfo> precipInfoList = DqcPreProcInit.getPrecipInfoList();
        List<Date> dateArray = PreProcUtils.getDateArray(startTime, numDays);
        Calendar startCal = TimeUtil.newGmtCalendar(startTime);
        startCal.add(Calendar.DAY_OF_YEAR, numDays);
        Date endTime = startCal.getTime();

        // Get Dailypp records
        DailyPPDao dailyPPDao = new DailyPPDao();
        try {
            List<Dailypp> dppList = dailyPPDao
                    .getRecordList(startTime, endTime);
            Iterator<Dailypp> pitr = dppList.iterator();

            // Loop through db records
            while (pitr.hasNext()) {
                Dailypp dailyPP = pitr.next();
                int index = -1;
                for (int i = 0; i < numDays; i++) {
                    // test matching date
                    if (DateUtils.isSameDay(dailyPP.getId().getObstime(),
                            dateArray.get(i))) {
                        // day index
                        index = i;
                        break;
                    }
                }

                // Mismatch obstime value, skip it.
                if (index < 0) {
                    continue;
                }
                /*
                 * get the info for the matching identifier, if there is a
                 * match.
                 */
                String strCompare = dailyPP.getId().getLid();
                char charTsCompare = dailyPP.getId().getTs().charAt(1);

                // Select record from PrecipInfoList
                int id = -1;
                for (int i = 0; i < precipInfoList.size(); i++) {
                    PrecipInfo precipInfo = precipInfoList.get(i);
                    // Compare lid and ts with values from db record
                    if (strCompare.equals(precipInfo.getLid())
                            && charTsCompare == precipInfo.getSource()) {
                        stationInfo = precipInfo;
                        // info index
                        id = i;
                        break;
                    }
                }
                if (stationInfo != null && id >= 0) {
                    Double val = dailyPP.getValue();
                    // fill pPPD and source in the precipInfo
                    if (val != null && val != PreProcConstants.PRECIP_MISSING
                            && val >= 0.0) {
                        stationInfo.pPPD.put(index, val);
                        stationInfo
                                .setSource(dailyPP.getId().getTs().charAt(1));
                        // replace default
                        precipInfoList.set(id, stationInfo);
                    }
                }
            }
        } catch (Exception e) {
            throw new MpeException("Error getting Dailypp records. ", e);
        }
    }

    /**
     * This function fills into the precip array with the observed/calculated
     * PPQ data from HourlyPP/HourlyPC tables.
     * 
     * @param startTime
     * @param numDays
     * @throws MpeException
     */
    public static void processHourlyPPPC(Date startTime, int numDays)
            throws MpeException {

        boolean blnLoadHourlyPC = false;
        List<Date> dateArray = PreProcUtils.getDateArray(startTime, numDays);
        List<PrecipInfo> precipInfoList = DqcPreProcInit.getPrecipInfoList();
        PrecipUtil pu = PrecipUtil.getInstance();
        List<String> ts = new ArrayList<>();
        PrecipInfo stationInfo = null;
        double minPercent = 0.0;
        double maxPrecip = 0.0;
        int precipSettings = CommonHydroConstants.PRECIP_TS_SINGLE
                | CommonHydroConstants.PRECIP_PP;
        boolean advance = false;
        List<Hourlypc> hourlyPCList = new ArrayList<>();
        List<Hourlypp> hourlyPPList = new ArrayList<>();
        int[] pHourlyPCIdx = new int[] { 0 };
        int[] pHourlyPPIdx = new int[] { 0 };
        int[] pcRecordCnt = new int[] { 0 };
        int[] ppRecordCnt = new int[] { 0 };
        IngestFilterDao ingestDao = new IngestFilterDao();

        // Compute the ending data retrieval time.
        Calendar startCal = TimeUtil.newGmtCalendar(startTime);
        startCal.add(Calendar.DAY_OF_YEAR, numDays);
        Date endTime = startCal.getTime();

        // Read token if need load hourlyPC data only first time.
        String token = AppsDefaults.getInstance().getToken(
                PreProcConstants.MPE_LOAD_HOURLY_PC, "ON");

        logger.info("      STATUS: Token "
                + PreProcConstants.MPE_LOAD_HOURLY_PC + " is set to " + token);

        if (token.equalsIgnoreCase("ON")) {
            blnLoadHourlyPC = true;
        }

        /*
         * Load the PC and PP hourly data
         */
        if (hourlyPCList.isEmpty()) {
            hourlyPCList = loadPcHourly(startTime, endTime, "", ts);
        }
        if (hourlyPPList.isEmpty()) {
            hourlyPPList = loadPpHourly(startTime, endTime, "", ts);
        }

        // Load the observed PPQ data.
        if (!hourlyPPList.isEmpty()) {
            Iterator<Hourlypp> itrPP = hourlyPPList.iterator();
            // Fill in the observed PPQ data.
            while (itrPP.hasNext()) {
                Hourlypp hourlyPP = itrPP.next();
                int index = -1;
                for (int i = 0; i < numDays; i++) {
                    if (DateUtils.isSameDay(hourlyPP.getId().getObsdate(),
                            dateArray.get(i))) {
                        // day index
                        index = i;
                        break;
                    }
                }
                /*
                 * Mismatch pHourlyPP->obsdate value, skip it. Such case should
                 * not occur.
                 */
                if (index < 0) {
                    continue;
                }
                /*
                 * get the info for the matching identifier, if there is a
                 * match.
                 */
                if (!precipInfoList.isEmpty()) {
                    String strCompare = hourlyPP.getId().getLid();
                    char charTsCompare = hourlyPP.getId().getTs().charAt(1);
                    // Select record from PrecipInfoList
                    int id = -1;
                    for (int i = 0; i < precipInfoList.size(); i++) {
                        PrecipInfo precipInfo = precipInfoList.get(i);
                        // Compare lid and ts with values from db record
                        if (strCompare.equals(precipInfo.getLid())
                                && charTsCompare == precipInfo.getSource()) {
                            stationInfo = precipInfo;
                            // info index
                            id = i;
                            break;
                        }
                    }
                    // sixhr18 / sixhr24 / sixhr06 / sixhr12
                    if (stationInfo != null && id >= 0) {
                        if (hourlyPP.getSixhr18() != null
                                && hourlyPP.getSixhr18() != PreProcConstants.PRECIP_MISSING
                                && hourlyPP.getSixhr18() >= 0.0) {
                            if ((index + 1) < numDays
                                    && stationInfo.getpPPQ().get(index + 1)
                                            .get(0) == PreProcConstants.PRECIP_MISSING) {
                                stationInfo
                                        .getpPPQ()
                                        .get(index + 1)
                                        .set(0,
                                                (double) hourlyPP.getSixhr18() / 100.0);
                                stationInfo.getpPPQPE().set(index + 1,
                                        PreProcConstants.PP);
                                if (stationInfo.getSource() == 'Z') {
                                    stationInfo.setSource(hourlyPP.getId()
                                            .getTs().charAt(1));
                                }
                            }
                        }
                        if (hourlyPP.getSixhr24() != null
                                && hourlyPP.getSixhr24() != PreProcConstants.PRECIP_MISSING
                                && hourlyPP.getSixhr24() >= 0.0) {
                            if ((index + 1) < numDays
                                    && stationInfo.pPPQ.get(index + 1).get(1) == PreProcConstants.PRECIP_MISSING) {
                                stationInfo
                                        .getpPPQ()
                                        .get(index + 1)
                                        .set(1,
                                                (double) hourlyPP.getSixhr24() / 100.0);
                                stationInfo.getpPPQPE().set(index + 1,
                                        PreProcConstants.PP);
                                if (stationInfo.getSource() == 'Z') {
                                    stationInfo.setSource(hourlyPP.getId()
                                            .getTs().charAt(1));
                                }
                            }
                        }
                        if (hourlyPP.getSixhr06() != null
                                && hourlyPP.getSixhr06() != PreProcConstants.PRECIP_MISSING
                                && hourlyPP.getSixhr06() >= 0.0
                                && stationInfo.getpPPQ().get(index).get(2) == PreProcConstants.PRECIP_MISSING) {
                            stationInfo
                                    .getpPPQ()
                                    .get(index)
                                    .set(2,
                                            (double) hourlyPP.getSixhr06() / 100.0);
                            stationInfo.getpPPQPE().set(index + 1,
                                    PreProcConstants.PP);
                            if (stationInfo.getSource() == 'Z') {
                                stationInfo.setSource(hourlyPP.getId().getTs()
                                        .charAt(1));
                            }
                        }
                        if (hourlyPP.getSixhr12() != null
                                && hourlyPP.getSixhr12() != PreProcConstants.PRECIP_MISSING
                                && hourlyPP.getSixhr12() >= 0.0
                                && stationInfo.getpPPQ().get(index).get(3) == PreProcConstants.PRECIP_MISSING) {
                            stationInfo
                                    .getpPPQ()
                                    .get(index)
                                    .set(3,
                                            (double) hourlyPP.getSixhr12() / 100.0);
                            stationInfo.getpPPQPE().set(index + 1,
                                    PreProcConstants.PP);
                            if (stationInfo.getSource() == 'Z') {
                                stationInfo.setSource(hourlyPP.getId().getTs()
                                        .charAt(1));
                            }
                        }
                        // Replace default
                        precipInfoList.set(id, stationInfo);
                    }
                }
            }
        }

        /*
         * Compute the total hourly precipitation for PPQ and PPD. Use PC in the
         * computation of 6 hour totals and 24 hour totals. Do not use PP. Only
         * do this if the mpe_load_hourlypc token is set to 'ON'.
         */
        pu.setIngestList(ingestDao.getIngestInfo());
        /* initialize duration array */
        int[] duration = { 6, 24 };
        int[] numPeriods = { 4, 1 };

        if (blnLoadHourlyPC) {
            for (int j = 0; j < 2; j++) {
                Calendar strtCal = TimeUtil.newGmtCalendar(startTime);
                strtCal.add(Calendar.HOUR, duration[j]);
                for (int index = 0; index < numDays; index++) {
                    for (int i = 0; i < numPeriods[j]; i++) {
                        // Compute the ending data retrieval time.
                        endTime = strtCal.getTime();
                        while (pHourlyPCIdx[0] < hourlyPCList.size()) {
                            /*
                             * Get the total precip for the station. Calculated
                             * by HourlyPP and HourlyPC, HourlyPP will be used
                             * if available, otherwise use hourlyPC.
                             */
                            totalPrecip = pu.getTotalHourlyPrecip(hourlyPCList,
                                    pHourlyPCIdx, null, pHourlyPPIdx, endTime,
                                    duration[j], (float) minPercent,
                                    precipSettings, advance, pcRecordCnt,
                                    ppRecordCnt);
                            /*
                             * For each station with a precipitation total
                             * greater than or equal to 0.0, process the gage's
                             * value.
                             */
                            if (totalPrecip.value >= 0.0
                                    && totalPrecip.value != PreProcConstants.PRECIP_MISSING) {
                                /*
                                 * Apply a range check to the totals derived
                                 * from PC data. Load the max hourly precip data
                                 * for range check.
                                 */
                                maxPrecip = readPrecipLimit(totalPrecip.lid,
                                        endTime, duration[j]);
                                if (maxPrecip != PreProcConstants.PRECIP_MISSING
                                        && totalPrecip.value > maxPrecip) {
                                    continue;
                                }
                                /*
                                 * get the info for the matching identifier, if
                                 * there is a match.
                                 */
                                int id = -1;
                                if (precipInfoList.size() > 0) {
                                    String strCompare = totalPrecip.lid;
                                    char charTsCompare = totalPrecip.getTs()
                                            .charAt(1);
                                    for (int k = 0; k < precipInfoList.size(); k++) {
                                        if (strCompare.equals(precipInfoList
                                                .get(k).getLid())
                                                && charTsCompare == precipInfoList
                                                        .get(k).getSource()) {
                                            stationInfo = precipInfoList.get(k);
                                            // info index
                                            id = k;
                                            break;
                                        }
                                    }
                                }
                                if (stationInfo != null && id >= 0) {
                                    // correct value, PE and source
                                    if (j == 0) {
                                        // 4 periods
                                        if (stationInfo.getpPPQ().get(index)
                                                .get(i) == PreProcConstants.PRECIP_MISSING) {
                                            stationInfo
                                                    .getpPPQ()
                                                    .get(index)
                                                    .set(i,
                                                            (double) totalPrecip.value);
                                            stationInfo.getpPPQPE().set(index,
                                                    totalPrecip.getPe());
                                            if (stationInfo.getSource() == 'Z') {
                                                stationInfo
                                                        .setSource(totalPrecip
                                                                .getTs()
                                                                .charAt(1));
                                            }
                                        }
                                    } else {
                                        // 1 period
                                        if (stationInfo.getpPPD().get(index) == PreProcConstants.PRECIP_MISSING) {
                                            stationInfo.getpPPD().put(index,
                                                    (double) totalPrecip.value);
                                            stationInfo.setSource(totalPrecip
                                                    .getTs().charAt(1));
                                        }
                                    }
                                    // Replace default
                                    precipInfoList.set(id, stationInfo);
                                }
                            }
                            pHourlyPCIdx[0]++;
                        }
                        // next period
                        strtCal.add(Calendar.HOUR, duration[j]);
                        pHourlyPCIdx[0] = 0;
                    }// end loop for periods
                }// end loop for days
            }
        }

        // clear data
        hourlyPPList.clear();
        hourlyPPList = null;

        hourlyPCList.clear();
        hourlyPCList = null;
    }

    /**
     * Reads PrecipLimit from {@link Locdatalimits} and {@link Datalimits}.
     * 
     * @param gageId
     * @param date
     * @param runHours
     * @return
     * @throws MpeException
     */
    private static double readPrecipLimit(String gageId, Date date, int runHours)
            throws MpeException {
        List<Locdatalimits> locDataLimitsList = null;
        List<Datalimits> dataLimitsList = null;

        // Select value from Locdatalimits table first.
        locDataLimitsList = new LocdatalimitsDao()
                .getPeAndDurInLocdatalimitRecords();

        if (locDataLimitsList != null) {
            Iterator<Locdatalimits> ldlItr = locDataLimitsList.iterator();
            while (ldlItr.hasNext()) {
                Locdatalimits ldlRecord = ldlItr.next();
                if (!ldlRecord.getId().getLid().equals(gageId)) {
                    try {
                        if (CommonMPEUtils.withinDataLimitTimeRange(date,
                                ldlRecord.getId().getMonthdaystart(),
                                ldlRecord.getMonthdayend())) {
                            if (ldlRecord.getGrossRangeMax() != null) {
                                if (ldlRecord.getId().getDur() == 1006
                                        && runHours == 6) {
                                    return ldlRecord.getGrossRangeMax();
                                } else if (ldlRecord.getId().getDur() == 2001
                                        && runHours == 24) {
                                    return ldlRecord.getGrossRangeMax();
                                } else if (ldlRecord.getId().getDur() == 5004
                                        && runHours == 24) {
                                    return ldlRecord.getGrossRangeMax();
                                }
                            }
                        }
                    } catch (Exception e) {
                        throw new MpeException(
                                "Error getting records from Locdatalimits: ", e);
                    }
                }

            }
        }
        /*
         * There is no complete valid data set from LocDataLimits table. select
         * value from Datalimits table.
         */
        dataLimitsList = new DatalimitsDao().getPeAndDurFromDatalimitsRecords();
        if (dataLimitsList != null) {
            Iterator<Datalimits> dlItr = dataLimitsList.iterator();
            while (dlItr.hasNext()) {
                Datalimits dlRecord = dlItr.next();
                try {
                    if (CommonMPEUtils.withinDataLimitTimeRange(date, dlRecord
                            .getId().getMonthdaystart(), dlRecord
                            .getMonthdayend())) {
                        if (dlRecord.getId().getDur() == 1006 && runHours == 6) {
                            return dlRecord.getGrossRangeMax();
                        } else if (dlRecord.getId().getDur() == 2001
                                && runHours == 24) {
                            return dlRecord.getGrossRangeMax();
                        } else if (dlRecord.getId().getDur() == 5004
                                && runHours == 24) {
                            return dlRecord.getGrossRangeMax();
                        }
                    }
                } catch (Exception e) {
                    throw new MpeException(
                            "Error getting records from Datalimits: ", e);
                }
            }
        }
        return PreProcConstants.PRECIP_MISSING;
    }

    /**
     * Loads PcHourly records
     * 
     * @param queryBeginTime
     * @param queryEndTime
     * @param lid
     * @param ts
     * @param numTs
     * @return
     */
    private static List<Hourlypc> loadPcHourly(Date queryBeginTime,
            Date queryEndTime, String lid, List<String> ts) {

        List<Hourlypc> hourlyPCList = new ArrayList<>();
        int numTs = ts.size();
        boolean notExclude = true;
        /*
         * Need special logic to account for accumulation intervals which start
         * at 00Z. This is because the 00Z PC value is actually placed in the 24
         * hour slot of the previous day.
         */
        Calendar begCal = TimeUtil.newGmtCalendar(queryBeginTime);
        if (begCal.get(Calendar.HOUR_OF_DAY) == 0) {
            begCal.add(Calendar.DAY_OF_YEAR, -1);
            queryBeginTime = begCal.getTime();
        }
        /* consider according to whether type-source specified. */
        /* load data which is not missing value (-9999.0) */
        HourlyPCDao pcDao = new HourlyPCDao();
        if (!ts.isEmpty()) {
            if (lid.isEmpty()) {
                if (notExclude) {
                    if (numTs == 1) {
                        hourlyPCList = pcDao.getHourlyPC_for_Ts_SglEQ_Obstime(
                                ts.get(0), queryBeginTime, queryEndTime);
                    } else {
                        hourlyPCList = pcDao
                                .getHourlyPC_for_Ts_MultiEQ_Obstime(ts,
                                        queryBeginTime, queryEndTime);
                    }
                } else {
                    if (numTs == 1) {
                        hourlyPCList = pcDao.getHourlyPC_for_Ts_SglNOT_Obstime(
                                ts.get(0), queryBeginTime, queryEndTime);
                    } else {
                        hourlyPCList = pcDao
                                .getHourlyPC_for_Ts_MultiNOT_Obstime(ts,
                                        queryBeginTime, queryEndTime);
                    }
                }
            } else {
                if (notExclude) {
                    if (numTs == 1) {
                        hourlyPCList = pcDao
                                .getHourlyPC_for_Ts_SglEQ_Lid_Obstime(lid,
                                        ts.get(0), queryBeginTime, queryEndTime);
                    } else {
                        hourlyPCList = pcDao
                                .getHourlyPC_for_Ts_MultiEQ_Lid_Obstime(lid,
                                        ts, queryBeginTime, queryEndTime);
                    }
                } else {
                    if (numTs == 1) {
                        hourlyPCList = pcDao
                                .getHourlyPC_for_Ts_SglNOT_Lid_Obstime(lid,
                                        ts.get(0), queryBeginTime, queryEndTime);
                    } else {
                        hourlyPCList = pcDao
                                .getHourlyPC_for_Ts_MultiNOT_Lid_Obstime(lid,
                                        ts, queryBeginTime, queryEndTime);
                    }
                }
            }
        } else {
            if (lid.isEmpty()) {
                hourlyPCList = pcDao.getHourlyPC_for_Obstime(queryBeginTime,
                        queryEndTime);
            } else {
                hourlyPCList = pcDao.getHourlyPC_for_Lid_Obstime(lid,
                        queryBeginTime, queryEndTime);
            }
        }
        return hourlyPCList;
    }

    /**
     * Load PpHourly
     * 
     * @param queryBeginTime
     * @param queryEndTime
     * @param lid
     * @param ts
     * @param exclude
     * @param numTs
     * @return
     */
    private static List<Hourlypp> loadPpHourly(Date queryBeginTime,
            Date queryEndTime, String lid, List<String> ts) {

        List<Hourlypp> hourlyPPList = new ArrayList<>();
        int numTs = ts.size();
        boolean notExclude = true;
        /*
         * Need special logic to account for accumulation intervals which start
         * at 00Z. This is because the 00Z PC value is actually placed in the 24
         * hour slot of the previous day.
         */
        Calendar begCal = TimeUtil.newGmtCalendar(queryBeginTime);
        if (begCal.get(Calendar.HOUR_OF_DAY) == 0) {
            begCal.add(Calendar.DAY_OF_YEAR, -1);
            queryBeginTime = begCal.getTime();
        }

        /* consider according to whether type-source specified. */
        /* load data which is not missing value (-9999.0) */
        HourlyPPDao ppDao = new HourlyPPDao();
        if (!ts.isEmpty()) {
            if (lid.isEmpty()) {
                if (notExclude) {
                    if (numTs == 1) {
                        hourlyPPList = ppDao.getHourlyPP_for_Ts_SglEQ_Obstime(
                                ts.get(0), queryBeginTime, queryEndTime);
                    } else {
                        hourlyPPList = ppDao
                                .getHourlyPP_for_Ts_MultiEQ_Obstime(ts,
                                        queryBeginTime, queryEndTime);
                    }
                } else {
                    if (numTs == 1) {
                        hourlyPPList = ppDao.getHourlyPP_for_Ts_SglNOT_Obstime(
                                ts.get(0), queryBeginTime, queryEndTime);
                    } else {
                        hourlyPPList = ppDao
                                .getHourlyPP_for_Ts_MultiNOT_Obstime(ts,
                                        queryBeginTime, queryEndTime);
                    }
                }
            } else {
                if (notExclude) {
                    if (numTs == 1) {
                        hourlyPPList = ppDao
                                .getHourlyPP_for_Ts_SglEQ_Lid_Obstime(lid,
                                        ts.get(0), queryBeginTime, queryEndTime);
                    } else {
                        hourlyPPList = ppDao
                                .getHourlyPP_for_Ts_MultiEQ_Lid_Obstime(lid,
                                        ts, queryBeginTime, queryEndTime);
                    }
                } else {
                    if (numTs == 1) {
                        hourlyPPList = ppDao
                                .getHourlyPP_for_Ts_SglNOT_Lid_Obstime(lid,
                                        ts.get(0), queryBeginTime, queryEndTime);
                    } else {
                        hourlyPPList = ppDao
                                .getHourlyPP_for_Ts_MultiNOT_Lid_Obstime(lid,
                                        ts, queryBeginTime, queryEndTime);
                    }
                }
            }
        } else {
            if (lid.isEmpty()) {
                hourlyPPList = ppDao.getHourlyPP_for_Obstime(queryBeginTime,
                        queryEndTime);
            } else {
                hourlyPPList = ppDao.getHourlyPP_for_Lid_Obstime(lid,
                        queryBeginTime, queryEndTime);
            }
        }
        return hourlyPPList;
    }

    public static int getPrecipCount() {
        return precipCount;
    }

    public static void setPrecipCount(int precipCount) {
        PrecipProc.precipCount = precipCount;
    }
}
