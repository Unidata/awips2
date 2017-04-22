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
package com.raytheon.viz.hydro.pointprecipitation;

import static com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.MISSING_PRECIP;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Rawpc;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.common.hydro.data.PrecipTotal.DataErr;
import com.raytheon.uf.common.hydro.util.DurationUtils;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.RawPrecipData;

/**
 * Precipitation accumulation calculations are done here.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2009 2257       mpduff     Initial creation
 * Sep 29, 2010 4384       lbousaidi  Fixed bugs related to TS Lookup for PP and PC
 * May 26, 2016 5571       skorolev   {@link  DurationUtils} relocated to common. Cleanup.
 * Jul 25, 2016 4623       skorolev   Replaced TotalPrecip with PrecipTotal.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class PrecipAccumulation {
    private static final String MIN_DUR_TOKEN = "hv_min_dur_filled";

    private static final long MISSING_COVERAGE = -999;

    private double adjustedStartTimeHrs = HydroConstants.MISSING_VALUE;

    private RawPrecipData rawData = null;

    private PointPrecipData data = null;

    private double minPercent = HydroConstants.MISSING_VALUE;

    private PointPrecipOptions paOptions;

    private Calendar beginTime = null;

    private Calendar endTime = null;

    private Date endDate = null;

    private int sumPcReports = HydroConstants.MISSING_VALUE;

    private double ppTotal;

    /** Best total match time */
    private long bestMatchTime = 0;

    private boolean noAccumFlag;

    private String bestPpTs = null;

    private String bestPcTs = null;

    /** Best individual coverage */
    private long pcCoverage = MISSING_COVERAGE;

    /** Best PC coverage */
    private long bestPcCoverage = MISSING_COVERAGE;

    /** Best PP coverage */
    private long bestPpCoverage = MISSING_COVERAGE;

    /** Best total coverage */
    private long bestCovered = MISSING_COVERAGE;

    /** Best individual amount */
    private double pcAmount;

    /** Best PC amount */
    private double bestPcAmount;

    /** Best PP amount */
    private double bestPpAmount;

    private boolean summedFlag;

    private long ppMatchTime;

    /**
     * Precipitation Accumulation
     * 
     * @param data
     * @param paOptions
     */
    public PrecipAccumulation(RawPrecipData data, PointPrecipOptions paOptions) {
        rawData = data;
        this.paOptions = paOptions;
        endDate = paOptions.getEndDate();
    }

    /**
     * Sums up the precipitation data for the Point Precipitation Accumulation
     * Dialog.
     * 
     * This method in AWIPS 1 was in a precip library. We don't currently have
     * the precip library in place. The summation code for the point precip
     * accum is completely encapsulated in this class. If changes are needed in
     * the future see the legacy AWIPS code in function get_total_raw_precip in
     * whfs_lib/src/PrecipUtil/TEXT/get_total_precip.c for original algorithms
     * 
     * Not using the settings as in the original code because for this dialog
     * the values are always PrecipPEBest and PrecipTSSingle
     */
    private void sumItUp() {
        PrecipTotal totalPrecip = null;
        data = new PointPrecipData();

        if (minPercent == HydroConstants.MISSING_VALUE) {
            String percent = AppsDefaults.getInstance().getToken(MIN_DUR_TOKEN);
            if ((percent != null) && (percent.length() > 0)) {
                minPercent = Double.parseDouble(percent);
                if ((minPercent < 0.0) || (minPercent > 1.0)) {
                    minPercent = 0.0;
                }
            }
        }

        int[] durations = paOptions.getDurations();

        /*
         * initialize the temporary structure to hold the data for all the
         * durations for this lid-pe-ts
         */
        data = new PointPrecipData();
        if (rawData.getRpc().size() > 0) {
            data.setLid(rawData.getRpc().get(0).getLid());
            data.setPe(rawData.getRpc().get(0).getPe());
        } else {
            data.setLid(rawData.getRpp().get(0).getLid());
            data.setPe(rawData.getRpp().get(0).getPe());
        }

        /*
         * Loop on the number of durations requested.
         */
        for (int i = 0; i < durations.length; i++) {
            /*
             * for the current duration, determine the begin and end time
             */
            setAccumBeginEnd(durations[i]);

            /*
             * use the PC function if processing PC data. PC data is always
             * derived, so fix the indicator flag to 1
             */
            totalPrecip = new PrecipTotal();

            /* Get the algorithm to use for totaling PC precipitation amounts. */
            int sumPcReports = checkSumPcReports();

            if (rawData.getRpc().size() > 0) {
                List<Rawpc> list = null;
                String ts = rawData.getPcTsLookup().get(0);
                data.setTs(ts);

                list = rawData.getPcList(ts);
                if ((list == null) || (list.size() < 0)) {
                    pcAmount = MISSING_PRECIP;
                }

                if ((list != null) && (list.size() > 0)) {
                    if (sumPcReports == 1) {
                        getTotalBySummingRawPC(list);
                    } else {
                        getTotalBySubtractingRawPC(list);
                    }
                } else {
                    pcAmount = MISSING_PRECIP;
                }

                // Since a single data set, they are the best
                bestPcAmount = pcAmount;
                bestPcCoverage = pcCoverage;
            }

            if (rawData.getRpp().size() > 0) {
                List<Rawpp> list = null;
                String ts = rawData.getPpTsLookup().get(0);

                data.setTs(ts);
                list = rawData.getPpList(ts);

                if ((list != null) && (list.size() > 0)) {
                    getTotalRawPP(list);
                } else {
                    bestPpAmount = MISSING_PRECIP;
                }
            }

            if ((rawData.getRpc().size() > 0) && (rawData.getRpp().size() > 0)) {
                if ((bestPcAmount != MISSING_PRECIP)
                        || (bestPpAmount != MISSING_PRECIP)) {
                    /*
                     * Select the PE or PC estimate which provides the best
                     * precipitation amount estimate.
                     */
                    if (bestPcCoverage > bestPpCoverage) {
                        totalPrecip.setValue((float) bestPcAmount);
                        totalPrecip.setPe(HydroConstants.PC);
                        totalPrecip.setTs(bestPcTs);
                        bestCovered = bestPcCoverage;

                        /*
                         * The summed flag is not applicable to PC data.
                         */
                        totalPrecip.setSummedFlag(false);
                    } else {
                        totalPrecip.setValue((float) bestPpAmount);
                        totalPrecip.setPe(HydroConstants.PP);
                        totalPrecip.setTs(bestPpTs);
                        totalPrecip.setSummedFlag(summedFlag);
                        Calendar c = TimeUtil.newGmtCalendar();
                        c.setTimeInMillis(bestMatchTime);
                        totalPrecip.setMatchTime(c.getTime());
                        bestCovered = bestPpCoverage;
                    }
                }
            } else if (rawData.getPpTsLookup().size() > 0) {
                totalPrecip.setValue((float) bestPpAmount);
                totalPrecip.setPe(HydroConstants.PP);
                totalPrecip.setTs(bestPpTs);
                totalPrecip.setSummedFlag(summedFlag);
                Calendar c = TimeUtil.newGmtCalendar();
                c.setTimeInMillis(bestMatchTime);
                totalPrecip.setMatchTime(c.getTime());
                bestCovered = bestPpCoverage;
            } else {
                totalPrecip.setValue((float) bestPcAmount);
                totalPrecip.setPe(HydroConstants.PC);
                totalPrecip.setTs(bestPcTs);
                totalPrecip.setSummedFlag(false);
                bestCovered = bestPcCoverage;
            }

            if (totalPrecip.getValue() != MISSING_PRECIP) {
                totalPrecip.setHoursCovered(bestCovered
                        / HydroConstants.MILLIS_PER_HOUR);
                totalPrecip.setPercentFilled(bestCovered
                        / (endTime.getTimeInMillis() - beginTime
                                .getTimeInMillis())
                        / HydroConstants.MILLIS_PER_HOUR);
            }

            /* Do no allow for a percent filled of greater than 100%. */
            if (totalPrecip.getPercentFilled() > 1.0) {
                totalPrecip.setPercentFilled(1.0f);
            }

            totalPrecip.setValueIndicator(PointPrecipConstants.OK_CHAR);

            /* Set the QC and error flags. */
            if (totalPrecip.getPercentFilled() < minPercent) {
                totalPrecip.setValue(MISSING_PRECIP);
                totalPrecip
                        .setValueIndicator(PointPrecipConstants.REJECTED_CHAR);
            }

            if ((totalPrecip.getValue() < 0)
                    && (totalPrecip.getValue() != MISSING_PRECIP)) {
                totalPrecip.setValue(MISSING_PRECIP);
                DataErr err = new DataErr();
                err.negdiff = true;
                totalPrecip.setErr(err);
                totalPrecip
                        .setValueIndicator(PointPrecipConstants.MISSING_CHAR);
            }

            // Set the data back into data
            double[] amount = data.getAmount();
            double[] hrfill = data.getHrfill();
            int[] summedFlag = data.getSummedFlag();

            amount[i] = totalPrecip.getValue();
            hrfill[i] = totalPrecip.getHoursCovered();
            if (totalPrecip.isSummedFlag()) {
                summedFlag[i] = 1;
            } else {
                summedFlag[i] = 0;
            }

            data.setAmount(amount);
            data.setHrfill(hrfill);
            data.setSummedFlag(summedFlag);

            /* find the maximum value considering all selected durations */
            data.setMaxValue(MISSING_PRECIP);
            for (int j = 0; j < data.getAmount().length; j++) {
                if (data.getMaxValue() < data.getAmount()[j]) {
                    data.setMaxValue(data.getAmount()[j]);
                }
            }
        }
    }

    /**
     * Gets Total RawPP
     * 
     * @param ppGroup
     */
    private void getTotalRawPP(List<Rawpp> ppGroup) {
        double total = MISSING_PRECIP;
        int numMinutes;
        int[] pMinutes;
        boolean durationMatch = false;
        long milliSecondsCovered = 0;
        long dataDur;
        boolean endIsWithin;
        boolean startIsWithin;
        long startmin;
        long endmin;
        boolean alreadyUsed;
        noAccumFlag = paOptions.isPpAccumSwitch();

        /* Check to make sure that there are data to process. */
        if ((ppGroup == null) || (ppGroup.size() < 1)) {
            bestPpAmount = MISSING_PRECIP;
            return;
        }

        /*
         * Create an array which has a number of elements equal to the number of
         * minutes between the starting and the ending times.
         */
        numMinutes = (int) ((endTime.getTimeInMillis() - beginTime
                .getTimeInMillis()) / HydroConstants.MILLIS_PER_MINUTE);

        pMinutes = new int[(int) numMinutes];

        /* Initialize all the elements in the minutes array to 0. */
        Arrays.fill(pMinutes, 0);

        durationMatch = findDurationMatch(ppGroup);

        if (durationMatch) {
            bestPpAmount = ppTotal;
            return;
        }

        total = ppTotal;

        /*
         * A duration match was not found. Check if the user will allow an
         * accumulation.
         */
        if (noAccumFlag) {
            for (Rawpp rpp : ppGroup) {
                /* Only consider the PP value if it is valid */
                if ((rpp.getValue() != MISSING_PRECIP)
                        && !rpp.getShefQualCode().startsWith("B")
                        && !rpp.getShefQualCode().startsWith("R")) {
                    /*
                     * Convert the SHEF duration code to an interval in seconds.
                     * Subtract this interval from the report's obstime to get
                     * the start time.
                     */
                    dataDur = DurationUtils.durationToMilliseconds(
                            rpp.getDur(), rpp.getObstime().getTime());

                    /*
                     * Test whether the report's obstime is within the interval.
                     * If it is, then set the start flag to true.
                     */
                    if ((rpp.getObstime().getTime() > beginTime
                            .getTimeInMillis())
                            && (rpp.getObstime().getTime() <= endTime
                                    .getTimeInMillis())) {
                        endIsWithin = true;
                    } else {
                        endIsWithin = false;
                    }

                    long reportStart = rpp.getObstime().getTime() - dataDur;

                    /*
                     * Test whether the starting time of the report's
                     * observation is within the interval. If it is, then set
                     * the end flag to true.
                     */
                    if ((reportStart >= beginTime.getTimeInMillis())
                            && (reportStart < endTime.getTimeInMillis())) {
                        startIsWithin = true;
                    } else {
                        startIsWithin = false;
                    }

                    /*
                     * Calculate the indexes in the minutes array corresponding
                     * to the starting and ending times.
                     */
                    startmin = (reportStart - beginTime.getTimeInMillis())
                            / HydroConstants.MILLIS_PER_MINUTE;
                    endmin = (rpp.getObstime().getTime() - beginTime
                            .getTimeInMillis())
                            / HydroConstants.MILLIS_PER_MINUTE;

                    /*
                     * Check that the report's time period is completetly within
                     * the time period being considered.
                     */
                    if (rpp.getValue() >= 0.0) {
                        bestPpTs = rpp.getTs();

                        if (startIsWithin && endIsWithin) {
                            /*
                             * Check to determine if the portion of the
                             * accumulation interval covered by the report's
                             * duration has already been covered by a previous
                             * report.
                             */
                            alreadyUsed = false;
                            for (int j = (int) startmin; j < endmin; j++) {
                                if (pMinutes[j] == 1) {
                                    alreadyUsed = true;
                                    break;
                                }
                            }
                            /*
                             * The data being considered has a duration that
                             * fits in the duration being considered, and the
                             * duration 'slot' is available, so apply the data
                             * value to the total and set the array to indicate
                             * the slot is now not available.
                             */
                            if (alreadyUsed == false) {
                                if (total == MISSING_PRECIP) {
                                    total = rpp.getValue();
                                } else {
                                    total += rpp.getValue();
                                }
                                for (int j = (int) startmin; j < endmin; ++j) {
                                    pMinutes[j] = 1;
                                }

                            }
                        } else if (rpp.getValue() == 0.0) {
                            if (startIsWithin && endIsWithin) {
                                /*
                                 * initialize the value as necessary and set
                                 * what time period the zero value covers.
                                 */
                                if (total == MISSING_PRECIP) {
                                    total = rpp.getValue();
                                }

                                if (startmin < 0) {
                                    startmin = 0;
                                }

                                if (endmin > numMinutes) {
                                    endmin = numMinutes;
                                }

                                for (int j = (int) startmin; j < endmin; j++) {
                                    pMinutes[j] = 1;
                                }
                            }
                        }
                    }
                }
            }
        }

        if (durationMatch == false) {
            int j = 0;
            for (int i = 0; i < numMinutes; i++) {
                if (pMinutes[i] == 1) {
                    j++;
                }
            }

            milliSecondsCovered = j * HydroConstants.MILLIS_PER_MINUTE;

            if (total >= 0.0) {
                summedFlag = true;
            } else {
                summedFlag = false;
            }
        } else {
            milliSecondsCovered = endTime.getTimeInMillis()
                    - beginTime.getTimeInMillis();
            summedFlag = false;
        }

        bestPpCoverage = milliSecondsCovered;
        bestPpAmount = total;
    }

    /**
     * Finds Duration Match
     * 
     * @param ppGroup
     * @return
     */
    private boolean findDurationMatch(List<Rawpp> ppGroup) {
        boolean match = false;
        boolean isEndtimeNear7am = false;
        boolean isObstimeNear7am = false;
        long dataDur;
        long diff = -1;
        long prevDiff = Long.MAX_VALUE;

        long duration = endTime.getTimeInMillis() - beginTime.getTimeInMillis();

        for (Rawpp rpp : ppGroup) {
            double value = rpp.getValue();

            if ((value != MISSING_PRECIP)
                    && !rpp.getShefQualCode().startsWith("B")
                    && !rpp.getShefQualCode().startsWith("R")) {
                /*
                 * Convert the SHEF duration code to an interval in seconds.
                 * Subtract this interval from the report's obstime to get the
                 * start time.
                 */
                dataDur = DurationUtils.durationToMilliseconds(rpp.getDur(),
                        rpp.getObstime().getTime());

                if (dataDur == duration) {
                    /*
                     * When accumulating for 24 hours and processing a 5004
                     * report with a duration of 24 hours and the ending time of
                     * the accumulation interval is within +/- 3 hours of 7 AM
                     * local time, find the 5004 report whose obstime is closest
                     * to the ending time of the accumulation interval. Treat it
                     * like an exact match.
                     */
                    if (((rpp.getDur() == 5004) || (rpp.getDur() == 2001))
                            && (duration == HydroConstants.MILLIS_PER_DAY)) {
                        isEndtimeNear7am = DurationUtils.isNear7am(endTime
                                .getTimeInMillis(), rpp.getObstime().getTime());

                        if (isEndtimeNear7am) {
                            /*
                             * If this is a 2001 report, make sure that its
                             * obstime is within the specified number of hours
                             * of 7am local. For a 5004 report we already know
                             * that the obstime is close enough to 7am local
                             * because it has a 24 hour duration.
                             */
                            isObstimeNear7am = true;

                            if (rpp.getDur() == 2001) {
                                isObstimeNear7am = DurationUtils.isNear7am(rpp
                                        .getObstime().getTime(), rpp
                                        .getObstime().getTime());
                            }

                            if (isObstimeNear7am) {
                                match = true;
                                diff = Math.abs(rpp.getObstime().getTime()
                                        - endTime.getTimeInMillis());

                                if (prevDiff >= diff) {
                                    prevDiff = diff;
                                    ppTotal = rpp.getValue();
                                    ppMatchTime = rpp.getObstime().getTime();
                                    bestPpTs = rpp.getTs();
                                }
                            }
                        }
                    } else {
                        /*
                         * Does the obstime of the report match the ending time
                         * of the accumulation interval?
                         */
                        if (rpp.getObstime().getTime() == endTime
                                .getTimeInMillis()) {
                            ppTotal = rpp.getValue();
                            ppMatchTime = rpp.getObstime().getTime();
                            prevDiff = diff;
                        }
                    }
                }
            }
        }
        bestMatchTime = ppMatchTime;

        return match;
    }

    /**
     * Set the beginning and ending times for the query.
     * 
     * @param durTime
     *            The duration of the query.
     */
    private void setAccumBeginEnd(int durTime) {
        endTime = TimeUtil.newGmtCalendar();
        endTime.setTime(endDate);

        /* the start time is ALWAYS the endtime minus the duration */
        beginTime = TimeUtil.newGmtCalendar();
        beginTime.setTimeInMillis(endDate.getTime());
        beginTime.add(Calendar.HOUR_OF_DAY, durTime * -1);
    }

    /**
     * Checks if the sum_pc_reports token is set to "YES" or "NO". YES means to
     * compute PC-based precipitation totals by adding the PC reports within the
     * accumulation interval. NO means to subtract the value of the PC report
     * closest to the starting time from the value of the PC report closest to
     * the ending time.
     * 
     * @return 1 if the sum_pc_reports token is "YES", 0 if "NO"
     */
    private int checkSumPcReports() {

        /*
         * Check the sum_pc_reports token. This will determine if PC-based
         * precipitation totals should be arrived at by a) adding all PC reports
         * over the user-specified interval or b) by subtracting the value of
         * the PC report closest to the starting time of the accumulation
         * interval from the value of the PC report closest to the ending time
         * of the accumulation interval.
         */
        if (sumPcReports == HydroConstants.MISSING_VALUE) {
            sumPcReports = 0;
            String value = AppsDefaults.getInstance()
                    .getToken("sum_pc_reports");

            if (value.startsWith("Y") || value.startsWith("y")) {
                sumPcReports = 1;
            }
        }

        return sumPcReports;
    }

    /**
     * Gets Total By Summing RawPC
     * 
     * @param dataList
     */
    private void getTotalBySummingRawPC(List<Rawpc> dataList) {
        /*
         * find the first record (the first record is the PC on the point which
         * is closest to the start_time within the precipitation duration plus
         * the hour window); make sure the type-source also matches. the records
         * should be ordered by ts, then by time. they are ordered earliest to
         * latest. Assign the value of the first record within the time window
         * (not the missing value) to the begin_value
         */
        double total = MISSING_PRECIP;
        long absoluteTimeDiff;
        long milliSecondsCovered = 0;
        long endTimeDiff = Long.MAX_VALUE;
        long beginTimeDiff = Long.MAX_VALUE;
        boolean endValid = false;
        long windowEndTime = 0;
        long windowBeginTime = 0;
        double endValue = MISSING_PRECIP;
        double beginValue = MISSING_PRECIP;

        if ((dataList == null) || (dataList.size() < 1)) {
            bestPcAmount = MISSING_PRECIP;
            return;
        }
        List<Rawpc> validDataList = new ArrayList<>();
        for (Rawpc rpc : dataList) {
            long obstime = rpc.getObstime().getTime();
            if (obstime > beginTime.getTimeInMillis()) {
                double value = rpc.getValue();

                if ((value >= 0.0) && !rpc.getShefQualCode().startsWith("B")
                        && !rpc.getShefQualCode().startsWith("R")) {
                    absoluteTimeDiff = Math.abs(obstime
                            - endTime.getTimeInMillis());

                    if (absoluteTimeDiff < endTimeDiff) {
                        endTimeDiff = absoluteTimeDiff;
                        endValue = value;
                        windowEndTime = obstime;
                        endValid = true;
                        validDataList.add(rpc);
                    } else {
                        /* The best ending value has been found. */
                        break;
                    }
                }
            } else {
                break;
            }
        }

        /*
         * if valid ending PC report cannot be found, then return with a missing
         * value
         */
        if (endValid == false) {
            bestPcAmount = MISSING_PRECIP;
            return;
        }

        /*
         * starting from the first record found above walk through each PC
         * report. If the PC report is valid, then compute the precipitation
         * total.
         */
        for (Rawpc rpc : validDataList) {
            long obstime = rpc.getObstime().getTime();
            if (obstime >= (beginTime.getTimeInMillis() - 12 * 3600 * 1000)) {
                double value = rpc.getValue();
                if ((value >= 0.0) && (value <= endValue)
                        && !rpc.getShefQualCode().startsWith("B")
                        && !rpc.getShefQualCode().startsWith("R")) {
                    absoluteTimeDiff = Math.abs(obstime
                            - beginTime.getTimeInMillis());

                    if (absoluteTimeDiff < beginTimeDiff) {
                        beginTimeDiff = absoluteTimeDiff;
                        beginValue = value;
                        windowBeginTime = obstime;

                        if (total != MISSING_PRECIP) {
                            total += (endValue - beginValue);
                        } else {
                            total = endValue - beginValue;
                        }

                        /* Figure out the coverage. */
                        if (milliSecondsCovered > 0) {
                            milliSecondsCovered += (windowEndTime - windowBeginTime);
                        } else {
                            milliSecondsCovered = windowEndTime
                                    - windowBeginTime;
                        }
                    } else {
                        /* The best starting value has been found. */
                        break;
                    }
                }
                /* Set the end values to the current values. */
                endValue = rpc.getValue();
                windowEndTime = obstime;
            } else {
                break;
            }
        }

        pcCoverage = milliSecondsCovered;

        pcAmount = total;
    }

    /**
     * Gets Total By Subtracting RawPC
     * 
     * @param dataList
     */
    private void getTotalBySubtractingRawPC(List<Rawpc> dataList) {
        PointPrecipDataManager dman = PointPrecipDataManager.getInstance();
        double total = MISSING_PRECIP;
        long absoluteTimeDiff;
        long endTimeDiff = Long.MAX_VALUE;
        long beginTimeDiff = Long.MAX_VALUE;
        long windowEndTime = 0;
        long windowBeginTime = 0;
        double endValue = MISSING_PRECIP;
        double beginValue = MISSING_PRECIP;
        boolean alertStationFlag = false;
        long milliSecondsCovered = 0;

        if ((dataList == null) || (dataList.size() < 1)) {
            bestPcAmount = MISSING_PRECIP;
            return;
        }

        if (adjustedStartTimeHrs == HydroConstants.MISSING_VALUE) {
            String value = AppsDefaults.getInstance().getToken(
                    "adjust_PC_startingtime");
            if ((value != null) && (value.length() > 0)) {
                adjustedStartTimeHrs = Double.parseDouble(value);
                if (adjustedStartTimeHrs <= 0) {
                    adjustedStartTimeHrs = PointPrecipConstants.DEFAULT_ADJUSTED_STARTTIME_HRS;
                }
            } else {
                adjustedStartTimeHrs = PointPrecipConstants.DEFAULT_ADJUSTED_STARTTIME_HRS;
            }
        }

        /* apply the start time adjustment */
        long adjustedStartTime = (long) (beginTime.getTimeInMillis() - (adjustedStartTimeHrs * HydroConstants.MILLIS_PER_HOUR));

        /*
         * find the closest PC value to the ending time, but still later than
         * the starting time.
         */
        for (int i = 0; i < dataList.size(); i++) {
            Rawpc rpc = dataList.get(i);
            long obstime = rpc.getObstime().getTime();

            Calendar c = TimeUtil.newGmtCalendar();
            c.setTimeInMillis(obstime);

            if (obstime > beginTime.getTimeInMillis()) {
                double value = rpc.getValue();
                if (value >= 0.0) {
                    absoluteTimeDiff = Math.abs(obstime
                            - endTime.getTimeInMillis());

                    if (absoluteTimeDiff < endTimeDiff) {
                        endTimeDiff = absoluteTimeDiff;
                        endValue = value;
                        windowEndTime = obstime;
                    } else {
                        /* The best ending value has been found. */
                        break;
                    }
                }
            } else {
                break;
            }
        }

        /*
         * if this is an ALERT station, then if no end_value was found, there
         * was no data before the starting_time, search for a begin value. if
         * this is not an ALERT station, then return with no value if no
         * end_value was found.
         */
        if (!dman.isAlertStation(dataList.get(0).getLid())) {
            if (endValue == MISSING_PRECIP) {
                pcAmount = MISSING_PRECIP;
                return;
            }
        } else {
            alertStationFlag = true;
        }

        /*
         * starting from the first record found above to look for the last
         * record which has value <= the end_value (not missing value) in the
         * precipitation duration plus the hour window
         */
        for (Rawpc rpc : dataList) {
            long obstime = rpc.getObstime().getTime();

            Calendar c = TimeUtil.newGmtCalendar();
            c.setTimeInMillis(obstime);

            double value = rpc.getValue();
            if (obstime >= adjustedStartTime) {
                if (((value >= 0.0) && (value <= endValue))
                        || (endValue == MISSING_PRECIP)) {
                    absoluteTimeDiff = Math.abs(obstime
                            - beginTime.getTimeInMillis());
                    if (absoluteTimeDiff < beginTimeDiff) {
                        beginTimeDiff = absoluteTimeDiff;
                        beginValue = value;
                        windowBeginTime = obstime;
                    } else {
                        /* The best starting value has been found. */
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        /*
         * If there is only one report, this report may be before, within, or
         * after the requested time period. Treat these cases the same as
         * follows. If the station is an alert station than we assume that the
         * one report means that it is still reporting data, so the value is not
         * missing. Furthermore, we assume that the value is 0.0. To ensure that
         * the value is used, we set the period covered to be the full period,
         * even though that is not truly the case.
         */
        if (((beginValue != MISSING_PRECIP) && (endValue == MISSING_PRECIP))
                || ((beginValue == MISSING_PRECIP) && (endValue != MISSING_PRECIP))) {
            if (alertStationFlag) {
                milliSecondsCovered = windowEndTime - windowBeginTime;
                total = 0.0;
            }
        } else if ((windowBeginTime >= beginTime.getTimeInMillis())
                && (windowEndTime <= endTime.getTimeInMillis())) {
            /*
             * The two times are within the desired time period. This is the
             * ideal case. The resulting value is safe to use whether it is zero
             * or not. Allow more coverage tolerance for ALERT gages
             */
            if (alertStationFlag) {
                milliSecondsCovered = windowEndTime
                        - beginTime.getTimeInMillis();
            } else {
                milliSecondsCovered = windowEndTime - windowBeginTime;
            }

            total = endValue - beginValue;
        } else if ((windowBeginTime < beginTime.getTimeInMillis())
                && (windowEndTime > endTime.getTimeInMillis())) {
            /*
             * The two times are both outside (before and after) the desired
             * time period, i.e. no reports were found within the time period.
             * This value can only be used if it is zero since any non-zero
             * precip may have occurred after the time period, and we must not
             * allow double counting of precip to occur.
             */
            if ((endValue >= beginValue) && ((endValue - beginValue) < 0.0001)) {
                milliSecondsCovered = endTime.getTimeInMillis()
                        - beginTime.getTimeInMillis();
                total = 0.0;
            }
        } else if ((windowBeginTime < beginTime.getTimeInMillis())
                && (windowEndTime <= endTime.getTimeInMillis())) {
            /*
             * The two times straddle the requested beginning time. This value
             * may be used even if it is non-zero since we are assumming that
             * the precip fell instantaneously at the end time and the end time
             * is within the time period.
             */
            milliSecondsCovered = windowEndTime - beginTime.getTimeInMillis();

            total = endValue - beginValue;
        } else if ((windowBeginTime >= beginTime.getTimeInMillis())
                && (windowEndTime > endTime.getTimeInMillis())) {
            /*
             * The two times straddle the requested ending time. This value can
             * only be used if the value is 0.0 since the end time is outside
             * the time period, and we are assuming the rain falls
             * instantaneously at the ending report time.
             */
            if (((endValue >= beginValue) && ((endValue - beginValue) < 0.0001))) {
                if (alertStationFlag) {
                    milliSecondsCovered = endTime.getTimeInMillis()
                            - beginTime.getTimeInMillis();
                } else {
                    milliSecondsCovered = endTime.getTimeInMillis()
                            - windowBeginTime;
                    total = 0.0;
                }
            }
        }
        pcCoverage = milliSecondsCovered;

        pcAmount = total;
    }

    public PointPrecipData getPointPrecipData() {
        sumItUp();

        return data;
    }
}
