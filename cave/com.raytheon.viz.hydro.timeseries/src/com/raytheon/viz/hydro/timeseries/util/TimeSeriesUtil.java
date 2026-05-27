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
package com.raytheon.viz.hydro.timeseries.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.common.collect.HashBiMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesPoint.MODE;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.util.QualityCodeUtil;

/**
 * Hydro Time Series Utilities
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Jul 29, 2008  1194        mpduff       Initial creation.
 * Oct 20, 2008  1520        mpduff       Added QC code.
 * Oct 29, 2009  2911        mpduff       Fixed DURATION_CHAR_CODES.substring
 *                                        code.
 * Jun 01, 2011  9499        djingtao     convertDurNameToValue()
 * Jul 12, 2011  9709        djingtao     modify tsAccumToInc3()
 * Aug 25, 2015  4794        mpduff       Added 5 min duration.
 * Jan 13, 2015  5243        tgurney      Fix convertDur2Short() handling of
 *                                        numeric characters
 * Nov 28, 2017  20465       jdeng        Fix timeseries plot error when dur=15
 *                                        or 6 min
 * Apr 12, 2018  6619        randerso     Code cleanup.
 * Apr 18, 2018  7268        mduff        Fixed issue with convertDur2Int
 *                                        method.
 * Jun 27, 2018  6748        randerso     Changed time series points from array
 *                                        to list to eliminate all the toArray
 *                                        calls. More code cleanup.
 *
 * </pre>
 *
 * @author mpduff
 */

public class TimeSeriesUtil {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TimeSeriesUtil.class);

    // TODO Move the QC code to HydroQC
    private static final int QC_PASSED = 101;

    private static final int QC_QUESTIONABLE = 102;

    private static final int QC_FAILED = 103;

    private static final int QC_NOT_PASSED = 104;

    private static final int QC_NOT_FAILED = 105;

    private static final int QC_DEFAULT = 110;

    private static final int SIGN_QC = 31;

    private static final int CERTAINTY_QC = 30;

    private static final int NOTQUEST_QC = 29;

    private static final int EXTERN_QC = 23;

    private static final int MANUAL_QC = 22;

    private static final int GROSSRANGE_QC = 21;

    private static final int EXTERN_NOTQUEST_QC = 20;

    private static final int REASONRANGE_QC = 19;

    private static final int ROC_QC = 18;

    private static final int OUTLIER_QC = 17;

    private static final int SCC_QC = 16;

    private static final int MSC_QC = 15;

    private static final int QC_GROSSRANGE_FAILED = 120;

    private static final int QC_MANUAL_PASSED = 121;

    private static final int QC_MANUAL_QUEST = 122;

    private static final int QC_MANUAL_FAILED = 123;

    private static final int QC_MANUAL_NEW = 124;

    private static final int QC_EXTERN_QUEST = 125;

    private static final int QC_EXTERN_FAILED = 126;

    private static final int QC_REASONRANGE_FAILED = 127;

    private static final int QC_ROC_PASSED = 128;

    private static final int QC_ROC_FAILED = 129;

    private static final int QC_OUTLIER_PASSED = 130;

    private static final int QC_OUTLIER_FAILED = 131;

    private static final int QC_SCC_PASSED = 132;

    private static final int QC_SCC_FAILED = 133;

    private static final int QC_MSC_PASSED = 134;

    private static final int QC_MSC_FAILED = 135;

    /**
     * Bit pattern 0110111111111111 1111111111111111 yields 1,879,048,191.
     */
    public static final int DEFAULT_QC_VALUE = 1_879_048_191;

    /**
     * Bit pattern 0110000000000000 0000000000000000 yields 1,610,612,736.
     */
    public static final int GOOD_QUESTIONABLE_THRESHOLD = 1_610_612_736;

    /**
     * Bit pattern 0100000000000000 0000000000000000 yields 1,073,741,824.
     */
    public static final int QUESTIONABLE_BAD_THRESHOLD = 1_073_741_824;

    /**
     * Bit pattern 0111111111111111 1111111111111111 yields 2,147,483,647. =
     * 2**31 - 1
     */
    public static final int ALL_ONES = 2_147_483_647;

    private static final HashBiMap<Integer, Character> durationMap = HashBiMap
            .create(23);

    static {
        durationMap.put(0, 'I');
        durationMap.put(1, 'U');
        durationMap.put(5, 'E');
        durationMap.put(10, 'G');
        durationMap.put(15, 'C');
        durationMap.put(30, 'J');
        durationMap.put(1001, 'H');
        durationMap.put(1002, 'B');
        durationMap.put(1003, 'T');
        durationMap.put(1004, 'F');
        durationMap.put(1006, 'Q');
        durationMap.put(1008, 'A');
        durationMap.put(1012, 'K');
        durationMap.put(1018, 'L');
        durationMap.put(2001, 'D');
        durationMap.put(2007, 'W');
        durationMap.put(3001, 'M');
        durationMap.put(4001, 'Y');
        durationMap.put(5000, 'Z');
        durationMap.put(5001, 'S');
        durationMap.put(5002, 'R');
        durationMap.put(5004, 'P');
        durationMap.put(5005, 'X');
    }

    /**
     * Convert duration character code to corresponding integer code
     *
     * @param dur
     * @return the integer code
     */
    public static int convertDur2Int(char dur) {
        dur = Character.toUpperCase(dur);
        if (!durationMap.containsValue(dur)) {
            return 0;
        }

        return durationMap.inverse().get(dur);
    }

    /**
     * Convert duration value to duration code
     *
     * @param durValue
     * @return the duration code or null if unable to convert
     */
    public static Character convertDur2Code(int durValue) {
        Character c = durationMap.get(durValue);
        if (c == null) {
            return null;
        }

        return c;
    }

    /**
     * Converts the duration value into readable text.
     *
     * @param dur
     *            The duration value
     * @return The readable text
     */
    public static String convertDur2Text(int dur) {
        int ivalue = dur / 1000;
        int jvalue = dur - (ivalue * 1000);
        String durtext = null;

        switch (ivalue) {
        case 0:
            if (dur == 0) {
                durtext = "";
            } else {
                durtext = dur + " min ";
            }
            break;
        case 1:
            durtext = jvalue + " hr ";
            break;
        case 2:
            durtext = jvalue + " day ";
            break;

        case 3:
            durtext = jvalue + " Mos ";
            break;
        case 4:
            durtext = jvalue + " Yr ";
            break;
        case 5:
            if (jvalue == 4) {
                durtext = "Since7AM ";
            } else if (jvalue == 1) {
                durtext = "Season ";
            } else if (jvalue == 2) {
                durtext = "PerRec ";
            } else {
                durtext = "Unk ";
            }
            break;
        default:
            durtext = "Unk ";
            break;
        }

        return durtext;
    }

    /**
     * This function takes the input time series and fills an output time
     * series, with regular intervals and interpolated values when necessary. It
     * does NOT change an accumulated series to an incremental one. It returns
     * the number of values in the output normalized time series that were
     * filled with non-missing data.
     *
     * @param td
     *            Original Trace
     * @param pcAsPpTraceData
     *            Derived PP Trace
     * @param interval
     *            Interval in seconds
     * @param interpMode
     *            Interpolation mode, true to interpolate
     * @param beginTime
     *            Beginning time of trace
     * @param endTime
     *            Ending time of trace
     * @return Number of points in the interpolated trace
     */
    public static int tsNormalize(TraceData td, TraceData pcAsPpTraceData,
            int interval, boolean interpMode, Date beginTime, Date endTime) {
        /* ********************************************************* */
        /* define an accumulated value for each period. */
        /* loop on the number of desired periods to fill. Always */
        /* fill a value with something, even if the value is missing. */
        /* ********************************************************* */
        List<TimeSeriesPoint> tsp = new ArrayList<>();
        for (int i = 0; beginTime.getTime() / TimeUtil.MILLIS_PER_SECOND
                + interval * i <= endTime.getTime()
                        / TimeUtil.MILLIS_PER_SECOND; i++) {
            TimeSeriesPoint point = new TimeSeriesPoint();
            Date d = new Date(beginTime.getTime()
                    + interval * TimeUtil.MILLIS_PER_SECOND * i);
            point.setX(d);
            point.setY(tsAssignValue(td, point.getX(), interval, interpMode));
            tsp.add(point);
        }

        pcAsPpTraceData.setTsData(tsp);
        return tsp.size();
    }

    private static double tsAssignValue(TraceData td, Date normTime,
            int interval, boolean interpMode) {
        /* initialize */
        double assignedValue = HydroConstants.MISSING_VALUE;
        boolean done = false;
        double bestDiff = Double.MAX_VALUE;
        double fraction = 0.0;
        double change = 0.0;
        long timeDiff;

        /*
         * check for the case where the desired time is before the earliest time
         * in the available data
         */
        List<TimeSeriesPoint> points = td.getTsData();
        if (normTime.before(points.get(0).getX())) {
            return assignedValue;
        }

        /* loop on the number of available points */
        for (int i = 0; i < td.getNpts() && !done; i++) {
            /* if there is an exact match, use it */

            if (points.get(i).getMode().equals(MODE.DELETE)
                    || points.get(i).getMode().equals(MODE.SETMISSING)) {
                continue;
            }

            if (points.get(i).getX().equals(normTime)) {
                assignedValue = points.get(i).getY();
                done = true;
            } else if (interpMode && i > 0
                    && points.get(i).getX().after(normTime)
                    && points.get(i - 1).getX().before(normTime)) {

                /*
                 * allow interpolation. if ever used, this algorithm should br
                 * reviewed. Comment straight from NWS code
                 */

                Date startTime = points.get(i - 1).getX();
                Date endTime = points.get(i).getX();

                timeDiff = endTime.getTime() - startTime.getTime();

                if (timeDiff == 0) {
                    fraction = 0.0;
                } else {
                    fraction = (normTime.getTime() - startTime.getTime())
                            / (double) timeDiff;
                }

                change = points.get(i).getY() - points.get(i - 1).getY();

                assignedValue = change * fraction + points.get(i - 1).getY();

                done = true;
            } else if (!interpMode) {
                /*
                 * If not interpolating, then allow use of values that are
                 * within a half-interval to be used, and use the one closest to
                 * the desired time.
                 */
                timeDiff = points.get(i).getX().getTime() - normTime.getTime();
                if (Math.abs(timeDiff) <= interval * 1000 / 2) {
                    if (Math.abs(timeDiff) < Math.abs(bestDiff)) {
                        assignedValue = points.get(i).getY();
                        bestDiff = timeDiff;
                    }

                    /*
                     * don't continue looping if the value being checked is at
                     * least a half-interval past the time being normalized
                     */
                    if (timeDiff >= interval / 2) {
                        done = true;
                    }
                }
            }
        }

        return assignedValue;
    }

    /**
     * this function receives an input time series that contains only valid
     * data. the output time series is returned in the same variable as the
     * input time series.
     *
     * @param td
     *            pcAsPp TraceData
     * @param distribMode
     *            Distribute Mode
     * @param dialog
     * @return Number of points in the trace
     */
    public static int tsAccumToInc3(TraceData td, boolean distribMode) {
        Date previousTime;
        double previousVal;
        long numHrsDiff = 0;
        int npts = 0;

        /* Make a copy of the object */
        TraceData tmpTrace = new TraceData(td);

        /* save the first value as the previous before beginning the loop */
        List<TimeSeriesPoint> points = td.getTsData();
        previousVal = points.get(0).getY();
        previousTime = points.get(0).getX();

        List<TimeSeriesPoint> tmpPoints = tmpTrace.getTsData();
        for (int i = 1; i < td.getNpts(); i++) {
            /* always set the time */
            tmpPoints.get(i - 1).setX(points.get(i).getX());

            /* set the value according to whether data available. */
            if (tmpPoints.get(i).getMode().equals(MODE.DELETE)) {
                tmpPoints.get(i).setY(HydroConstants.MISSING_VALUE);
            }

            if (tmpPoints.get(i).getY() != HydroConstants.MISSING_VALUE
                    && previousVal != HydroConstants.MISSING_VALUE) {
                /*
                 * if distributing the amount equally over the multiple periods,
                 * then find the value, then spread it to the previous hours
                 * that had unfilled data
                 */
                if (distribMode) {
                    numHrsDiff = (tmpPoints.get(i).getX().getTime()
                            - previousTime.getTime())
                            / TimeUtil.MILLIS_PER_HOUR;
                    double value = (tmpPoints.get(i).getY() - previousVal)
                            / numHrsDiff;

                    for (int n = 0; n < numHrsDiff; n++) {
                        tmpPoints.get(i - 1 - n).setY(value);
                    }
                } else {
                    tmpPoints.get(i - 1)
                            .setY(points.get(i).getY() - previousVal);
                }

                /*
                 * reject the value if a negative value results. if in distrib
                 * mode, then need to ensure that the previous time periods are
                 * also set to MISSING
                 */
                if (tmpPoints.get(i - 1).getY() < 0.0) {
                    tmpPoints.get(i - 1).setY(HydroConstants.MISSING_VALUE);

                    if (distribMode) {
                        for (int n = 0; n < numHrsDiff; n++) {
                            tmpPoints.get(i - 1 - (n + 1))
                                    .setY(tmpPoints.get(i - 1).getY());
                        }
                    }
                }

            } else {
                tmpPoints.get(i - 1).setY(HydroConstants.MISSING_VALUE);
            }

            /* save any good values for the next pass */
            if (tmpPoints.get(i).getY() != HydroConstants.MISSING_VALUE) {
                previousVal = tmpPoints.get(i).getY();
                previousTime = tmpPoints.get(i).getX();
            }
        }

        td = new TraceData(tmpTrace);

        if (td.getNpts() > 0) {
            npts = td.getNpts() - 1;
        } else {
            npts = 0;
        }

        return npts;
    }

    public static long setQcCode(int bitGroupDescription, long qualityCode) {
        switch (bitGroupDescription) {
        case QC_DEFAULT:
            /* Set the quality control code to the default value. */
            qualityCode = DEFAULT_QC_VALUE;
            break;
        case QC_GROSSRANGE_FAILED:
            /*
             * Set the quality control code so that the specific bit for gross
             * range check indicates it failed, and therefore set the summary
             * certainty bit to indicated the failure.
             */
            qualityCode = setQcBit(GROSSRANGE_QC, 0, qualityCode);
            qualityCode = setQcBit(CERTAINTY_QC, 0, qualityCode);
            break;
        case QC_REASONRANGE_FAILED:
            /*
             * Set the quality control code so that the specific bit for
             * reasonableness range check indicates failure, and therefore set
             * the summary questionable bit to indicate the failure.
             */
            qualityCode = setQcBit(REASONRANGE_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_ROC_FAILED:
            /*
             * Set the quality control code so that the specific bit for roc
             * check indicates it failed, and therefore set the summary
             * questionable bit to indicate the failure.
             */
            qualityCode = setQcBit(ROC_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_ROC_PASSED:
            /*
             * Set the quality control code so that the specific bit for roc
             * check indicates that it passed, and therefore set the summary
             * questionable bit to indicate the passing.
             */
            qualityCode = setQcBit(ROC_QC, 1, qualityCode);
            qualityCode = setQcNotQuestSummary(qualityCode);
            break;
        case QC_OUTLIER_FAILED:
            /*
             * Set the quality control code so that the specific bit for outlier
             * check indicates it failed, and therefore set the summary
             * questionable bit to indicate the failure.
             */
            qualityCode = setQcBit(OUTLIER_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_OUTLIER_PASSED:
            /*
             * Set the quality control code so that the specific bit for outlier
             * check indicates that it passed, and therefore set the summary
             * questionable bit to indicate the passing.
             */
            qualityCode = setQcBit(OUTLIER_QC, 1, qualityCode);
            qualityCode = setQcNotQuestSummary(qualityCode);
            break;
        case QC_SCC_FAILED:
            /*
             * Set the quality control code so that the specific bit for spatial
             * consistency check indicates that it failed, and therefore set the
             * summary questionable bit to indicate the failure.
             */

            qualityCode = setQcBit(SCC_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_SCC_PASSED:
            /*
             * Set the quality control code so that the specific bit for spatial
             * consistency check indicates that it passed, and therefore set the
             * summary questionable bit to indicate the passing.
             */
            qualityCode = setQcBit(SCC_QC, 1, qualityCode);
            qualityCode = setQcNotQuestSummary(qualityCode);
            break;
        case QC_MSC_FAILED:
            /*
             * Set the quality control code so that the specific bit for multi
             * sensor check indicates that it failed, and therefore set the
             * summary questionable bit to indicate the failure.
             */
            qualityCode = setQcBit(MSC_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_MSC_PASSED:
            /*
             * Set the quality control code so that the specific bit for multi
             * sensor check indicates that it passed, and therefore set the
             * summary questionable bit to indicate the passing.
             */

            qualityCode = setQcBit(MSC_QC, 1, qualityCode);
            qualityCode = setQcNotQuestSummary(qualityCode);
            break;
        case QC_EXTERN_FAILED:
            /*
             * Set the quality control code so that the specific bit for
             * external check indicates it failed, and therefore set the summary
             * certainty bit to indicate the failure.
             */
            qualityCode = setQcBit(EXTERN_QC, 0, qualityCode);
            qualityCode = setQcBit(CERTAINTY_QC, 0, qualityCode);
            break;
        case QC_EXTERN_QUEST:
            /*
             * Set the quality control code so that the specific bit for
             * external check indicates it is questioable, and therefore set the
             * summary questionable bit to indicate the failure.
             */
            qualityCode = setQcBit(EXTERN_NOTQUEST_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            break;
        case QC_MANUAL_PASSED:
            /*
             * When manually updating a value, assume that the value is set to a
             * good value, in which case we should set it to the default.
             */
            qualityCode = setQcCode(QC_DEFAULT, qualityCode);
            break;
        case QC_MANUAL_QUEST:
            /*
             * Set the quality control code so that the specific bit for manual
             * check indicates it is questionable, and therefore set the summary
             * certainty bit to indicate the questionable state.
             */
            qualityCode = setQcBit(MANUAL_QC, 0, qualityCode);
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
            qualityCode = setQcBit(CERTAINTY_QC, 1, qualityCode);
            break;
        case QC_MANUAL_FAILED:
            /*
             * Set the quality control code so that the specific bit for manual
             * check indicates it failed, and therefore set the summary
             * certainty bit to indicate the failure.
             */
            qualityCode = setQcBit(MANUAL_QC, 0, qualityCode);
            qualityCode = setQcBit(CERTAINTY_QC, 0, qualityCode);
            break;
        case QC_MANUAL_NEW:
            /*
             * When manually inserting a value, assume that the value is set to
             * a good value, in which case we should set it to the default.
             */
            qualityCode = setQcCode(QC_DEFAULT, qualityCode);
            break;
        default:
            /*
             * An invalid bit value was processed. The quality control code
             * remains unchanged.
             */
            statusHandler.error("Unknown bitGroupDescription received: "
                    + bitGroupDescription);
            break;
        }

        return qualityCode;
    }

    /**
     * Sets the specified bit of the quality code to the specified value.
     *
     * @param bitPosition
     *            The bit position
     * @param setting
     *            The setting
     * @param qualityCode
     *            The quality code
     * @return true if valid request, false if invalid request
     */
    public static long setQcBit(int bitPosition, int setting,
            long qualityCode) {

        /*
         * Variable used to set a specific bit in bit string to 1; initialized
         * as 0000000000000000 0000000000000001
         */
        int mask = 1;
        long bitwise_inclusive_OR_result;
        long bitwise_AND_result;

        /* Ensure that a valid bit position is requested. */
        if (bitPosition < SIGN_QC) {

            /* if setting the bit ON */
            if (setting == 1) {

                /*
                 * The mask is employed to set a specific bit to the value of 1
                 * in a 32-bit string while hiding the value of the other bits.
                 * The mask is leftwardly shifted to the bit position of the bit
                 * being referenced.
                 */
                mask = mask << bitPosition;

                /*
                 * The bitwise inclusive OR operation is used to set the
                 * specified bit. Upon completion, the bit is written to
                 * quality_code memory location.
                 */
                bitwise_inclusive_OR_result = qualityCode | mask;
                qualityCode = bitwise_inclusive_OR_result;
            } else {
                /*
                 * if setting the bit OFF. first build a mask that has all ones
                 * except for the bit in question, then AND the mask with the
                 * existing value to turn off the single bit.
                 */

                mask = ALL_ONES ^ mask << bitPosition;

                bitwise_AND_result = qualityCode & mask;
                qualityCode = bitwise_AND_result;
            }
        } else {
            statusHandler.error("Invalid bitPosition received: " + bitPosition
                    + ". Must be less than " + SIGN_QC);
        }

        return qualityCode;
    }

    /**
     * Set's the notquest_qc bit according to whether it passed the following
     * tests: External QC Not-QUESTIONABLE Indicator (BIT20) Reasonable Range
     * Test (BIT19) Rate-of-change Test (BIT18) Outlier Test (BIT17) Spatial
     * Consistency Test (BIT16) Multi Sensor Test (BIT15)
     *
     * @param qualityCode
     * @return the updated qualityCode
     */
    public static long setQcNotQuestSummary(long qualityCode) {

        if (!QualityCodeUtil.checkQcBit(EXTERN_NOTQUEST_QC, qualityCode)
                || !QualityCodeUtil.checkQcBit(REASONRANGE_QC, qualityCode)
                || !QualityCodeUtil.checkQcBit(ROC_QC, qualityCode)
                || !QualityCodeUtil.checkQcBit(OUTLIER_QC, qualityCode)
                || !QualityCodeUtil.checkQcBit(SCC_QC, qualityCode)
                || !QualityCodeUtil.checkQcBit(MSC_QC, qualityCode)) {
            qualityCode = setQcBit(NOTQUEST_QC, 0, qualityCode);
        } else {
            qualityCode = setQcBit(NOTQUEST_QC, 1, qualityCode);
        }

        return qualityCode;
    }

    /**
     * @param qualityCode
     * @return the QC symbol
     */
    public static String buildQcSymbol(long qualityCode) {
        String returnVal = "";
        if (checkQcCode(QC_PASSED, qualityCode)) {
            returnVal = "G";
        } else if (checkQcCode(QC_QUESTIONABLE, qualityCode)) {
            returnVal = "Q";
        } else if (checkQcCode(QC_FAILED, qualityCode)) {
            returnVal = "B";
        } else {
            returnVal = "?";
        }

        return returnVal;
    }

    public static boolean checkQcCode(int bitGroupDescription,
            long qualityCode) {
        boolean returnValue = false;
        switch (bitGroupDescription) {
        case QC_DEFAULT:
            if (qualityCode == DEFAULT_QC_VALUE) {
                returnValue = true;
            }
            break;
        case QC_PASSED:
            if (qualityCode > GOOD_QUESTIONABLE_THRESHOLD) {
                returnValue = true;
            }
            break;
        case QC_QUESTIONABLE:
            if (qualityCode >= QUESTIONABLE_BAD_THRESHOLD
                    && qualityCode < GOOD_QUESTIONABLE_THRESHOLD) {
                returnValue = true;
            }
            break;
        case QC_ROC_PASSED:
            returnValue = QualityCodeUtil.checkQcBit(ROC_QC, qualityCode);
            break;
        case QC_OUTLIER_PASSED:
            returnValue = QualityCodeUtil.checkQcBit(OUTLIER_QC, qualityCode);
            break;
        case QC_SCC_PASSED:
            returnValue = QualityCodeUtil.checkQcBit(SCC_QC, qualityCode);
            break;
        case QC_MSC_PASSED:
            returnValue = QualityCodeUtil.checkQcBit(MSC_QC, qualityCode);
            break;
        case QC_FAILED:
            if (qualityCode < QUESTIONABLE_BAD_THRESHOLD) {
                returnValue = true;
            }
            break;
        case QC_NOT_FAILED:
            if (qualityCode >= QUESTIONABLE_BAD_THRESHOLD) {
                returnValue = true;
            }
            break;
        case QC_NOT_PASSED:
            if (qualityCode <= GOOD_QUESTIONABLE_THRESHOLD) {
                returnValue = true;
            }
            break;
        default:
            statusHandler.error("Unknown bitGroupDescription received: "
                    + bitGroupDescription);
        }

        return returnValue;
    }

    /**
     * Round a double value to the specified number of decimal places
     *
     * @param value
     *            the value to be rounded
     * @param places
     *            the number of decimal places
     * @return the rounded value
     */
    public static double round(double value, int places) {
        double p = Math.pow(10, places);
        value *= p;
        double tmp = Math.round(value);
        return tmp / p;
    }

}
