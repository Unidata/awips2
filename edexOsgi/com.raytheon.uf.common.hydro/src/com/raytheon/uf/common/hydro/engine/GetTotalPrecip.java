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
package com.raytheon.uf.common.hydro.engine;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.dataquery.requests.QlServerRequest;
import com.raytheon.uf.common.dataquery.requests.QlServerRequest.QueryLanguage;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.data.PrecipRecord;
import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.common.hydro.util.DurationUtils;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Get the total precip for PC and PP reports from the rawPC, rawPP, curPC, and
 * curPP tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2010 4564       mpduff      Initial creation
 * Mar 09, 2015 13998      lbousaidi   Initialize minuteList
 * May 26, 2016 5571       skorolev    Relocated to a common plugin 
 *                                     for use in both EDEX/CAVE. Cleanup.
 * Jun 08, 2016 5571       njensen     Split into GetTotalPrecip and VizGetTotalPrecip
 * Oct 13, 2016 5857       skorolev    Corrected condition in computeRawPpTotal.
 * Nov 16, 2016 ?          bkowal      Corrected {@link QlServerRequest} response handling.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class GetTotalPrecip {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private Set<String> alertStations;

    /**
     * Constructor
     */
    public GetTotalPrecip() {

    }

    /**
     * Get total raw PC data
     * 
     * @param pcDataList
     *            PrecipRecord list
     * @param startingTime
     *            starting time of period
     * @param endingTime
     *            ending time of period
     * @param sumPcReports
     *            sum the reports or not
     * @return PrecipTotal object
     */
    protected PrecipTotal getTotalRawPc(List<PrecipRecord> pcDataList,
            Date startingTime, Date endingTime, boolean sumPcReports) {
        PrecipTotal precipTotal = null;

        /*
         * Determine which algorithm to use in deriving PC precip totals.
         */
        if (sumPcReports) {
            precipTotal = sumRawPc(pcDataList, startingTime, endingTime);
        } else {
            precipTotal = subtractRawPc(pcDataList, startingTime, endingTime);
        }

        return precipTotal;
    }

    /**
     * Get toal raw PP data
     * 
     * @param ppDataList
     *            PrecipRecord list
     * @param startingTime
     *            starting time of period
     * @param endingTime
     *            ending time of period
     * @param noAccumFlag
     *            accumulate the data or not
     * @param endingTimeMatch
     *            match the ending time or not
     * @return PrecipTotal object
     */
    protected PrecipTotal getTotalRawPp(List<PrecipRecord> ppDataList,
            Date startingTime, Date endingTime, int noAccumFlag,
            int endingTimeMatch) {
        PrecipTotal precipTotal = new PrecipTotal();
        int numMinutes;
        int secondsCovered = 0;

        /* Check to make sure that there are data to process. */
        if ((ppDataList == null) || (ppDataList.size() == 0)) {
            precipTotal.setSecondsCovered(secondsCovered);
            precipTotal.setTotal(CommonHydroConstants.MISSING_VALUE);

            return precipTotal;
        }

        numMinutes = (int) ((endingTime.getTime() - startingTime.getTime())
                / TimeUtil.MILLIS_PER_MINUTE);
        List<Integer> minuteList = new ArrayList<>(numMinutes);
        for (int i = 0; i < numMinutes; i++) {
            minuteList.add(0);
        }

        precipTotal = computeRawPpTotal(ppDataList, startingTime, endingTime,
                noAccumFlag, endingTimeMatch, minuteList);

        /* Return the value and seconds covered to the user. */
        if (precipTotal.isDurationMatchFound() == false) {
            int j = 0;

            for (int i = 0; i < minuteList.size(); i++) {
                if (minuteList.get(i) == 1) {
                    j++;
                }
            }

            secondsCovered = j * TimeUtil.SECONDS_PER_MINUTE;

        } else {
            secondsCovered = (int) ((endingTime.getTime()
                    - startingTime.getTime()) / TimeUtil.MILLIS_PER_SECOND);
        }

        precipTotal.setSecondsCovered(secondsCovered);

        return precipTotal;
    }

    /**
     * Compute the Raw PP Total preciptation.
     * 
     * @param ppDataList
     *            PP PrecipRecord list
     * @param startingTime
     *            Starting time of period
     * @param endingTime
     *            Ending time of period
     * @param noAccumFlag
     *            Accumulate or not
     * @param endingTimeMatch
     *            match the ending time or not
     * @param minuteList
     *            list of minutes in the time period
     * @return PrecipTotal object
     */
    private PrecipTotal computeRawPpTotal(List<PrecipRecord> ppDataList,
            Date startingTime, Date endingTime, int noAccumFlag,
            int endingTimeMatch, List<Integer> minuteList) {
        PrecipTotal precipTotal = new PrecipTotal();
        double total;
        boolean startIsWithin = false;
        boolean endIsWithin = false;
        boolean alreadyUsed = false;
        long reportStart;

        if ((ppDataList == null) || (minuteList == null)
                || (minuteList.size() == 0)) {
            precipTotal.setDurationMatchFound(false);
            return precipTotal;
        }

        /* Check for an exact match first. */
        precipTotal = findDurationMatch(ppDataList, startingTime, endingTime,
                noAccumFlag, endingTimeMatch);

        if (precipTotal.isDurationMatchFound()) {
            return precipTotal;
        }

        total = precipTotal.getTotal();

        /*
         * A duration match was not found. Check if the user will allow an
         * accumulation.
         */
        if (noAccumFlag == 0) {
            for (int i = 0; i < ppDataList.size(); i++) {
                PrecipRecord pRec = ppDataList.get(i);
                Date obstime = pRec.getDate();
                /* Only consider the PP value if it is valid. */
                if ((pRec.getValue() != CommonHydroConstants.MISSING_VALUE)
                        && !pRec.getShefQualCode().startsWith("B")
                        && !pRec.getShefQualCode().startsWith("R")) {
                    /*
                     * Convert the SHEF duration code to an interval in seconds.
                     * Subtract this interval from the report's obstime to get
                     * the start time.
                     */
                    long durMillis = DurationUtils.durationToMilliseconds(
                            pRec.getDuration(), obstime.getTime());
                    /*
                     * Test whether the report's obstime is within the interval.
                     * If it is, then set the start flag to true.
                     */
                    if ((obstime.getTime() > startingTime.getTime())
                            && (obstime.getTime() <= endingTime.getTime())) {
                        endIsWithin = true;
                    } else {
                        endIsWithin = false;
                    }

                    reportStart = obstime.getTime() - durMillis;

                    /*
                     * Test whether the starting time of the report's
                     * observation is within the interval. If it is, then set
                     * the end flag to true.
                     */
                    if ((reportStart >= startingTime.getTime())
                            && (reportStart < endingTime.getTime())) {
                        startIsWithin = true;
                    } else {
                        startIsWithin = false;
                    }

                    /*
                     * Calculate the indexes in the minutes array corresponding
                     * to the starting and ending times.
                     */
                    int startmin = (int) ((reportStart - startingTime.getTime())
                            / TimeUtil.MILLIS_PER_MINUTE);
                    int endmin = (int) ((obstime.getTime()
                            - startingTime.getTime())
                            / TimeUtil.MILLIS_PER_MINUTE);

                    /*
                     * Check that the report's time period is completetly within
                     * the time period being considered.
                     */
                    if (pRec.getValue() >= 0.0) {
                        if (startIsWithin && endIsWithin) {
                            /*
                             * Check to determine if the portion of the
                             * accumulation interval covered by the report's
                             * duration has already been covered by a previous
                             * report.
                             */
                            alreadyUsed = false;

                            for (int j = startmin; j < endmin; j++) {
                                if (minuteList.get(j) == 1) {
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
                            if (!alreadyUsed) {
                                if (total == CommonHydroConstants.MISSING_VALUE) {
                                    total = pRec.getValue();
                                } else {
                                    total += pRec.getValue();
                                }

                                for (int j = startmin; j < endmin; j++) {
                                    minuteList.add(j, 1);
                                }
                            }
                        } else if (pRec.getValue() == 0.0) {
                            if (startIsWithin || endIsWithin) {
                                /*
                                 * initialize the value as necessary and set
                                 * what time period the zero value covers.
                                 */
                                if (total == CommonHydroConstants.MISSING_VALUE) {
                                    total = pRec.getValue();
                                }

                                if (startmin < 0) {
                                    startmin = 0;
                                }

                                if (endmin > minuteList.size()) {
                                    endmin = minuteList.size();
                                }

                                for (int j = startmin; j < endmin; j++) {
                                    minuteList.add(j, 1);
                                }
                            }
                        }
                    }
                }
            }
        }

        precipTotal.setTotal(total);
        precipTotal.setDurationMatchFound(false);

        return precipTotal;
    }

    /**
     * Find a duration match.
     * 
     * @param ppDataList
     *            PrecipRecord list
     * @param startingTime
     *            starting time of the period
     * @param endingTime
     *            ending time of the period
     * @param noAccumFlag
     *            accumulate the data or not
     * @param endingTimeMatch
     *            match the ending time or not
     * @return PrecipTotal object
     */
    private PrecipTotal findDurationMatch(List<PrecipRecord> ppDataList,
            Date startingTime, Date endingTime, int noAccumFlag,
            int endingTimeMatch) {
        PrecipTotal precipTotal = new PrecipTotal();
        int duration;
        boolean isEndtimeNear7am = false;
        boolean isObstimeNear7am = false;
        long diff;
        long prevDiff = Integer.MAX_VALUE;

        duration = (int) ((endingTime.getTime() - startingTime.getTime())
                / TimeUtil.MILLIS_PER_SECOND);

        for (int i = 0; i < ppDataList.size(); i++) {
            PrecipRecord pRec = ppDataList.get(i);

            if ((pRec.getValue() != CommonHydroConstants.MISSING_VALUE)
                    && (!pRec.getShefQualCode().startsWith("B"))
                    && (!pRec.getShefQualCode().startsWith("R"))) {
                /*
                 * Convert the SHEF duration code to an interval in seconds.
                 * Subtract this interval from the report's obstime to get the
                 * start time.
                 */
                long durMillis = DurationUtils.durationToMilliseconds(
                        pRec.getDuration(), pRec.getDate().getTime());

                if (((int) (durMillis / 1000)) == duration) {
                    switch (endingTimeMatch) {
                    case CommonHydroConstants.LATEST_ENDINGTIME_MATCH:
                        /*
                         * All done. The duration match with the most recent
                         * ending time has been found.
                         */
                        precipTotal.setTotal(pRec.getValue());
                        precipTotal.setMatchTime(pRec.getDate());
                        precipTotal.setDurationMatchFound(true);

                        return precipTotal;

                    case CommonHydroConstants.EXACT_ENDINGTIME_MATCH:
                        /*
                         * When accumulating for 24 hours and processing a 5004
                         * report with a duration of 24 hours and the ending
                         * time of the accumulation interval is within +/- 3
                         * hours of 7 AM local time, find the 5004 report whose
                         * obstime is closest to the ending time of the
                         * accumulation interval. Treat it like an exact match.
                         */
                        if (((pRec.getDuration() == 5004)
                                || (pRec.getDuration() == 2001))
                                && (duration == TimeUtil.SECONDS_PER_DAY)) {
                            isEndtimeNear7am = isNear7amLocal(endingTime,
                                    pRec.getDate());

                            if (isEndtimeNear7am == true) {
                                /*
                                 * If this is a 2001 report, make sure that its
                                 * obstime is within the specified number of
                                 * hours of 7am local. For a 5004 report we
                                 * already know that the obstime is close enough
                                 * to 7am local because it has a 24 hour
                                 * duration.
                                 */
                                isObstimeNear7am = true;

                                if (pRec.getDuration() == 2001) {
                                    isObstimeNear7am = isNear7amLocal(
                                            pRec.getDate(), pRec.getDate());
                                }

                                if (isObstimeNear7am) {
                                    precipTotal.setDurationMatchFound(true);
                                    diff = Math.abs(pRec.getDate().getTime()
                                            - endingTime.getTime());

                                    if (prevDiff >= diff) {
                                        precipTotal.setTotal(pRec.getValue());
                                        precipTotal
                                                .setMatchTime(pRec.getDate());
                                        prevDiff = diff;
                                    }
                                }
                            }
                        } else {
                            /*
                             * Does the obstime of the report match the ending
                             * time of the accumulation interval?
                             */
                            if (pRec.getDate().getTime() == endingTime
                                    .getTime()) {
                                precipTotal.setTotal(pRec.getValue());
                                precipTotal.setMatchTime(pRec.getDate());
                                precipTotal.setDurationMatchFound(true);

                                return precipTotal;
                            }
                        }

                        break;

                    case CommonHydroConstants.CLOSEST_ENDINGTIME_MATCH:
                        /*
                         * Find a report with a matching duration whose obstime
                         * is closest to the ending time of the accumulation
                         * interval.
                         */
                        diff = Math.abs(pRec.getDate().getTime()
                                - endingTime.getTime());

                        if (prevDiff >= diff) {
                            precipTotal.setTotal(pRec.getValue());
                            precipTotal.setMatchTime(pRec.getDate());
                            prevDiff = diff;
                        } else {
                            precipTotal.setDurationMatchFound(true);

                            return precipTotal;
                        }

                    default:
                        /*
                         * Find a report with a matching duration whose obstime
                         * is closest to the ending time of the accumulation
                         * interval and within the user specified hour window.
                         */
                        diff = Math.abs(pRec.getDate().getTime()
                                - endingTime.getTime());
                        if ((prevDiff >= diff) && (diff <= (endingTimeMatch
                                * TimeUtil.SECONDS_PER_HOUR))) {
                            precipTotal.setDurationMatchFound(true);
                            precipTotal.setTotal(pRec.getValue());
                            precipTotal.setMatchTime(pRec.getDate());
                            prevDiff = diff;
                        }
                    }
                }
            }
        }

        return precipTotal;
    }

    /**
     * Subtract the values for the given time period.
     * 
     * @param pcDataList
     * @param startingTime
     * @param endingTime
     * @return
     */
    private PrecipTotal subtractRawPc(List<PrecipRecord> pcDataList,
            Date startingTime, Date endingTime) {
        PrecipTotal precipTotal = new PrecipTotal();
        int adjStartHrs = CommonHydroConstants.DEFAULT_ADJUSTED_STARTTIME_HRS;
        int secondsCovered = 0;
        long adjustedStartTime = 0;
        long absoluteTimeDiff = 0;
        long endTimeDiff = Integer.MAX_VALUE;
        long beginTimeDiff = Integer.MAX_VALUE;
        long endMillis = 0; // last good data time
        long startMillis = 0;// first good data time
        double startValue = CommonHydroConstants.MISSING_VALUE;
        double endValue = CommonHydroConstants.MISSING_VALUE;
        boolean alertStationFlag = false;
        double total = 0;
        int i = 0;

        /*
         * Define an adjusted starting time to improve chances of getting a
         * meaningful start value.
         */
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String pcAdjustedStartTime = appsDefaults
                .getToken("adjust_PC_startingtime");
        if (pcAdjustedStartTime != null) {
            adjStartHrs = Integer.parseInt(pcAdjustedStartTime);
        }

        /* apply the start time adjustment */
        adjustedStartTime = startingTime.getTime()
                - (adjStartHrs * TimeUtil.MILLIS_PER_HOUR);

        /*
         * find the closest PC value to the ending time, but still later than
         * the starting time.
         */
        if (pcDataList != null && pcDataList.size() > 0) {
            for (i = 0; i < pcDataList.size(); i++) {
                PrecipRecord pRec = pcDataList.get(i);
                Date obsTime = pRec.getDate();

                if (startingTime.before(obsTime)) {
                    if (pRec.getValue() >= 0.0) {
                        absoluteTimeDiff = Math
                                .abs(obsTime.getTime() - endingTime.getTime());

                        if (absoluteTimeDiff < endTimeDiff) {
                            endTimeDiff = absoluteTimeDiff;
                            endValue = pRec.getValue();
                            endMillis = obsTime.getTime();
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
             * if this is an ALERT station, then if no end_value was found,
             * there was no data before the starting_time, search for a begin
             * value. if this is not an ALERT station, then return with no value
             * if no end_value was found.
             */

            if ((pcDataList != null)
                    && !isAlertStation(pcDataList.get(0).getLid())) {
                alertStationFlag = false;
                if (endValue == CommonHydroConstants.MISSING_VALUE) {
                    precipTotal.setTotal(CommonHydroConstants.MISSING_VALUE);
                    precipTotal.setSecondsCovered(secondsCovered);

                    return precipTotal;
                } else {
                    alertStationFlag = true;
                }

                /*
                 * starting from the first record found above to look for the
                 * last record which has value <= the end_value (not missing
                 * value) in the precipitation duration plus the hour window
                 */
                for (; i < pcDataList.size(); i++) {
                    PrecipRecord pRec = pcDataList.get(i);
                    Date obsTime = pRec.getDate();

                    if (obsTime.getTime() >= adjustedStartTime) {
                        if ((pRec.getValue() >= 0.0) && ((pRec
                                .getValue() <= endValue)
                                || (endValue == CommonHydroConstants.MISSING_VALUE))) {
                            absoluteTimeDiff = Math.abs(
                                    obsTime.getTime() - startingTime.getTime());

                            if (absoluteTimeDiff < beginTimeDiff) {
                                beginTimeDiff = absoluteTimeDiff;
                                startValue = pRec.getValue();
                                startMillis = obsTime.getTime();
                            } else {
                                /* The best starting value has been found. */
                                break;
                            }
                        }
                    } else {
                        break;
                    }
                }

                /*
                 * If there is only one report, this report may be before,
                 * within, or after the requested time period. Treat these cases
                 * the same as follows. If the station is an alert station than
                 * we assume that the one report means that it is still
                 * reporting data, so the value is not missing. Furthermore, we
                 * assume that the value is 0.0. To ensure that the value is
                 * used, we set the period covered to be the full period, even
                 * though that is not truly the case.
                 */
                if (((startValue != CommonHydroConstants.MISSING_VALUE)
                        && (endValue == CommonHydroConstants.MISSING_VALUE))
                        || ((startValue == CommonHydroConstants.MISSING_VALUE)
                                && (endValue != CommonHydroConstants.MISSING_VALUE))) {
                    if (alertStationFlag) {
                        secondsCovered = (int) ((endMillis - startMillis)
                                / TimeUtil.MILLIS_PER_SECOND);
                        total = 0.0;
                    }
                } else if ((startMillis >= startingTime.getTime())
                        && (endMillis <= endingTime.getTime())) {
                    /*
                     * The two times are within the desired time period. This is
                     * the ideal case. The resulting value is safe to use
                     * whether it is zero or not. Allow more coverage tolerance
                     * for ALERT gages
                     */
                    if (alertStationFlag) {
                        secondsCovered = (int) ((endingTime.getTime()
                                - startingTime.getTime())
                                / TimeUtil.MILLIS_PER_SECOND);
                    } else {
                        secondsCovered = (int) ((endingTime.getTime()
                                - startMillis) / TimeUtil.MILLIS_PER_SECOND);
                    }

                    total = endValue - startValue;
                } else if ((startMillis < startingTime.getTime())
                        && (endMillis > endingTime.getTime())) {
                    /*
                     * The two times are both outside (before and after) the
                     * desired time period, i.e. no reports were found within
                     * the time period. This value can only be used if it is
                     * zero since any non-zero precip may have occurred after
                     * the time period, and we must not allow double counting of
                     * precip to occur.
                     */
                    if ((endValue >= startValue)
                            && ((endValue - startValue) < 0.0001)) {
                        secondsCovered = (int) ((endingTime.getTime()
                                - startingTime.getTime())
                                / TimeUtil.MILLIS_PER_SECOND);
                        total = 0.0;
                    }
                } else if ((startMillis < startingTime.getTime())
                        && (endMillis <= endingTime.getTime())) {
                    /*
                     * The two times straddle the requested beginning time. This
                     * value may be used even if it is non-zero since we are
                     * assumming that the precip fell instantaneously at the end
                     * time and the end time is within the time period.
                     */
                    secondsCovered = (int) ((endMillis - startingTime.getTime())
                            / TimeUtil.MILLIS_PER_SECOND);
                    total = endValue - startValue;
                } else if ((startMillis >= startingTime.getTime())
                        && (endMillis > endingTime.getTime())) {
                    /*
                     * The two times straddle the requested ending time. This
                     * value can only be used if the value is 0.0 since the end
                     * time is outside the time period, and we are assuming the
                     * rain falls instantaneously at the ending report time.
                     */
                    if ((endValue >= startValue)
                            && ((endValue - startValue) < 0.0001)) {
                        if (alertStationFlag) {
                            secondsCovered = (int) ((endingTime.getTime()
                                    - startingTime.getTime())
                                    / TimeUtil.MILLIS_PER_SECOND);
                        } else {
                            secondsCovered = (int) ((endingTime.getTime()
                                    - startMillis)
                                    / TimeUtil.MILLIS_PER_SECOND);
                        }
                        total = 0.0;
                    }
                }

                /* Set the return values */
                precipTotal.setSecondsCovered(secondsCovered);
                precipTotal.setTotal(total);
            }
        } // add for if

        return precipTotal;
    }

    /**
     * Sum the data values for the given time period.
     * 
     * @param pcDataList
     *            List of PreciRecord data objects
     * @param startingTime
     *            The starting time of the period
     * @param endingTime
     *            The ending time of the period
     * 
     * @return PrecipTotal data object
     */
    private PrecipTotal sumRawPc(List<PrecipRecord> pcDataList,
            Date startingTime, Date endingTime) {
        PrecipTotal precipTotal = new PrecipTotal();
        double total = CommonHydroConstants.MISSING_VALUE;
        int secondsCovered = 0;
        long absoluteTimeDiff = 0;
        long endTimeDiff = Integer.MAX_VALUE;
        long beginTimeDiff = Integer.MAX_VALUE;
        boolean endValid = false;
        long startingMillis = 0;
        long endingMillis = 0;
        double beginValue;
        double endValue = CommonHydroConstants.MISSING_VALUE;
        int i = 0;

        if (pcDataList == null) {
            precipTotal.setTotal(CommonHydroConstants.MISSING_VALUE);
            precipTotal.setSecondsCovered(secondsCovered);

            return precipTotal;
        }

        /* Walk the list and get the sum */
        for (i = 0; i < pcDataList.size(); i++) {
            PrecipRecord pRec = pcDataList.get(i);
            Date obsTime = pRec.getDate();

            if (obsTime.after(startingTime)) {
                /* Check to make sure the value is valid. */
                if ((pRec.getValue() >= 0.0)
                        && (!pRec.getShefQualCode().startsWith("B"))
                        && (!pRec.getShefQualCode().startsWith("R"))) {
                    absoluteTimeDiff = Math
                            .abs(obsTime.getTime() - endingTime.getTime());

                    if (absoluteTimeDiff < endTimeDiff) {
                        endTimeDiff = absoluteTimeDiff;
                        endValue = pRec.getValue();
                        endingMillis = obsTime.getTime();
                        endValid = true;
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
            precipTotal.setTotal(CommonHydroConstants.MISSING_VALUE);
            precipTotal.setSecondsCovered(secondsCovered);

            return precipTotal;
        }

        /*
         * Walk through each PC report. If the PC report is valid, then compute
         * the precipitation total.
         */
        for (; i < pcDataList.size(); i++) {
            PrecipRecord pRec = pcDataList.get(i);
            Date obsTime = pRec.getDate();

            if (obsTime.getTime() >= startingTime.getTime()
                    - (12 * TimeUtil.MILLIS_PER_HOUR)) {
                if ((pRec.getValue() >= 0.0) && (pRec.getValue() <= endValue)
                        && !pRec.getShefQualCode().startsWith("B")
                        && !pRec.getShefQualCode().startsWith("R")) {
                    absoluteTimeDiff = Math
                            .abs(obsTime.getTime() - startingTime.getTime());

                    if (absoluteTimeDiff < beginTimeDiff) {
                        beginTimeDiff = absoluteTimeDiff;
                        beginValue = pRec.getValue();
                        startingMillis = obsTime.getTime();

                        if (total != CommonHydroConstants.MISSING_VALUE) {
                            total += endValue - beginValue;
                        } else {
                            total = endValue - beginValue;
                        }

                        /* Figure out the coverage. */
                        if (secondsCovered > 0) {
                            secondsCovered += (endingMillis - startingMillis)
                                    / TimeUtil.MILLIS_PER_SECOND;
                        } else {
                            secondsCovered = (int) ((endingMillis
                                    - startingMillis)
                                    / TimeUtil.MILLIS_PER_SECOND);
                        }
                    } else {
                        /* The best starting value has been found. */
                        break;
                    }
                }

                /* Set the end values to the current values. */
                endValue = pRec.getValue();
                endingMillis = obsTime.getTime();
            } else {
                break;
            }
        }

        /* Set the return data */
        precipTotal.setTotal(total);
        secondsCovered = (int) ((endingMillis - startingMillis)
                / TimeUtil.MILLIS_PER_SECOND);
        precipTotal.setSecondsCovered(secondsCovered);

        return precipTotal;
    }

    /**
     * Check to see if the given lid is an Alert station.
     * 
     * @param lid
     *            The location ID
     * @return true if the station is an alert station
     */
    private synchronized boolean isAlertStation(String lid) {
        boolean isAlert = false;

        if (alertStations == null) {
            alertStations = new HashSet<>();

            String query = "select lid from telem where type = 'ALERT'";
            QlServerRequest req = new QlServerRequest();
            req.setQuery(query);
            req.setLang(QueryLanguage.SQL);
            req.setDatabase(CommonHydroConstants.IHFS);

            try {
                Object result = RequestRouter.route(req);
                if (!(result instanceof ResponseMessageGeneric)) {
                    statusHandler.error("Received unexpected response of type: "
                            + result.getClass().getName() + ".");
                    return isAlert;
                }
                ResponseMessageGeneric response = (ResponseMessageGeneric) result;
                if (!(response.getContents() instanceof QueryResult)) {
                    statusHandler
                            .error("Received unexpected ResponseMessageGeneric contents of type: "
                                    + response.getContents().getClass()
                                            .getName()
                                    + ".");
                    return isAlert;
                }

                QueryResult queryResult = (QueryResult) response.getContents();
                for (QueryResultRow queryResultRow : queryResult.getRows()) {
                    alertStations.add((String) queryResultRow.getColumn(0));
                }
            } catch (Exception e) {
                statusHandler.error("Error getting alert stations", e);
            }
        }

        if (alertStations.contains(lid)) {
            isAlert = true;
        }

        return isAlert;
    }

    /**
     * Is the provided time near 7am local time.
     * 
     * @param endingTime
     *            The ending time of the period
     * @param obstime
     *            The obs time
     * @return true if near 7am local
     */
    private boolean isNear7amLocal(Date endingTime, Date obstime) {

        int local7amWindow;
        Calendar localTime = null;
        long diff;

        localTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        localTime.setTime(obstime);
        localTime.set(Calendar.HOUR, 7);
        localTime.set(Calendar.MINUTE, 0);
        localTime.set(Calendar.SECOND, 0);

        diff = Math.abs(endingTime.getTime() - localTime.getTimeInMillis());
        /* Convert difference from milliseconds to seconds */
        diff /= TimeUtil.MILLIS_PER_SECOND;

        /* Obtain time window in units of hours */
        local7amWindow = getLocal7amSearchWindow();

        /*
         * Convert time window from hours to seconds and compare with difference
         */
        if (diff <= (local7amWindow * TimeUtil.SECONDS_PER_HOUR)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Get a local 7am search window.
     * 
     * @return
     */
    private int getLocal7amSearchWindow() {
        int local7amWindow = CommonHydroConstants.LOCAL_5004_7AM_WINDOW;

        String amString = AppsDefaults.getInstance().getToken(
                CommonHydroConstants.AppsDefaults.PPP_PPD_LOCAL_7AM_WINDOW);
        if ((amString != null) && (amString.length() > 0)) {
            local7amWindow = Integer.parseInt(amString);
        }

        return local7amWindow;
    }

}
