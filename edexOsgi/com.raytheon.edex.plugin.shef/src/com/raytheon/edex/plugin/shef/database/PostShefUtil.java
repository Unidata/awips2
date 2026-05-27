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
package com.raytheon.edex.plugin.shef.database;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.edex.plugin.shef.util.BitUtils;
import com.raytheon.uf.common.dataplugin.shef.tables.Fcstheight;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Duration;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Utility methods for PostShef.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2018    6991    mduff       Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class PostShefUtil {
    private static final IUFStatusHandler log = UFStatus
            .getHandler(PostShefUtil.class);

    /**
     * Quality Control Code Enum
     */
    public static enum QualityControlCode {
        QC_DEFAULT,
        QC_GROSSRANGE_FAILED,
        QC_REASONRANGE_FAILED,
        QC_ROC_FAILED,
        QC_ROC_PASSED,
        QC_OUTLIER_FAILED,
        QC_OUTLIER_PASSED,
        QC_SCC_FAILED,
        QC_SCC_PASSED,
        QC_MSC_FAILED,
        QC_MSC_PASSED,
        QC_EXTERN_FAILED,
        QC_EXTERN_QUEST,
        QC_MANUAL_PASSED,
        QC_MANUAL_QUEST,
        QC_MANUAL_FAILED,
        QC_MANUAL_NEW,
        QC_PASSED,
        QC_QUESTIONABLE,
        QC_FAILED,
        QC_NOT_PASSED,
        QC_NOT_FAILED
    }

    /** Map of value to duration character */
    private static final Map<Integer, String> DURATION_MAP;

    static {
        DURATION_MAP = Collections.unmodifiableMap(buildDurationMap());
    }

    /**
     * Determine which items in the forecast time series to keep, as there may
     * be overlap due to multiple time_series.
     **/
    public boolean[] setFcstKeep(Object[] ulHead, Fcstheight[] fcstHead) {
        int fcstCount = fcstHead.length;
        int ulCount = ulHead.length;
        boolean[] doKeep = new boolean[fcstCount];
        int[] basisIndex = new int[fcstCount];
        int[] tsFirstChk = new int[ulCount];
        int MISSING = ShefConstants.SHEF_MISSING;
        Date[] startTime = new Date[ulCount];
        Date[] endTime = new Date[ulCount];
        Date[] basisTime = new Date[ulCount];
        Date fcstBasisTime = null;
        Date fcstValidTime = null;
        Date ulBasisTime = null;

        Date row = null;
        Date validTime = null;
        for (int i = 0; i < fcstCount; i++) {

            /* find out which basis time's time series this value belongs to */

            fcstBasisTime = new Date(
                    fcstHead[i].getId().getBasistime().getTime());

            basisIndex[i] = MISSING;

            for (int j = 0; ((j < ulCount)
                    && (basisIndex[i] == MISSING)); j++) {
                row = (Date) ulHead[j];
                ulBasisTime = row;

                if (ulBasisTime.compareTo(fcstBasisTime) == 0) {
                    basisIndex[i] = j;
                }
            }

            if (basisIndex[i] == MISSING) {
                log.info("Unexpected error assigning basis_index for " + i);
            }

            /*
             * check if the values constitute the start or end times for the
             * time series and record these times if they do
             */
            validTime = new Date(fcstHead[i].getId().getValidtime().getTime());

            if (tsFirstChk[basisIndex[i]] == 1) {
                if (validTime.before(startTime[basisIndex[i]])) {
                    startTime[basisIndex[i]] = validTime;
                } else if (validTime.after(endTime[basisIndex[i]])) {
                    endTime[basisIndex[i]] = validTime;
                }
            } else {
                startTime[basisIndex[i]] = validTime;
                endTime[basisIndex[i]] = validTime;
                tsFirstChk[basisIndex[i]] = 1;
            }
        }

        /*
         * for each of the unique basis times, assign the basis time in a
         * convenient array for use in the adjust_startend function.
         */
        for (int j = 0; j < ulCount; j++) {
            row = (Date) ulHead[j];
            basisTime[j] = row;
        }

        /*
         * knowing the actual start and end times for the multiple time series,
         * loop thru the time series and adjust the start and end times so that
         * they reflect the time span to use; i.e. there is no overlap. THIS IS
         * THE KEY STEP IN THE PROCESS OF DEFINING AN AGGREGATE VIRTUAL TIME
         * SERIES!!!
         */
        Object[] tmp = adjustStartEnd(ulCount, basisTime, startTime, endTime);
        startTime = (Date[]) tmp[0];
        endTime = (Date[]) tmp[1];

        /*
         * loop thru the complete retrieved time series and only keep the value
         * if it lies between the start and end time for this basis time
         */
        for (int i = 0; i < fcstCount; i++) {
            fcstValidTime = new Date(
                    fcstHead[i].getId().getValidtime().getTime());
            if ((fcstValidTime.compareTo(startTime[basisIndex[i]]) >= 0)
                    && (fcstValidTime.compareTo(endTime[basisIndex[i]]) <= 0)) {
                doKeep[i] = true;
            } else {
                doKeep[i] = false;
            }
        }
        return doKeep;
    }

    /**
     * This method uses the time series with the latest basis time first, and
     * uses it in its entirety. Then the time series with the next latest basis
     * time is used. If it overlaps portions of the already saved time series,
     * then only that portion which doesn't overlap is used. This process
     * continues until all time series have been considered. In essences, this
     * method adjoins adjacent time series.
     */
    private Object[] adjustStartEnd(int count, Date[] basisTime,
            Date[] startValidTime, Date[] endValidTime) {
        boolean found = false;
        int currentIndex = 0;
        int[] basisOrder = new int[count];
        Date fullStartValidTime = null;
        Date fullEndValidTime = null;
        Date tmpTime = null;
        Date zero = new Date(0);
        Object[] rval = new Object[2];

        Arrays.fill(basisOrder, -1);

        /*
         * find the order of the time series by their latest basis time. if two
         * time series have the same basis time, use the one that has the
         * earlier starting time. note that the order is such that the latest
         * basis time is last in the resulting order array.
         */
        for (int i = 0; i < count; i++) {
            tmpTime = zero;
            currentIndex = 0;

            for (int j = 0; j < count; j++) {
                /*
                 * only consider the time series if it hasn't been accounted for
                 * in the order array
                 */
                found = false;

                for (int k = 0; k < i; k++) {
                    if (j == basisOrder[k]) {
                        found = true;
                        break;
                    }
                }

                if (!found) {
                    if (basisTime[j].compareTo(tmpTime) > 0) {
                        currentIndex = j;
                        tmpTime = basisTime[j];
                    } else if (basisTime[j].compareTo(tmpTime) == 0) {
                        if (startValidTime[j]
                                .compareTo(startValidTime[currentIndex]) < 0) {
                            currentIndex = j;
                            tmpTime = basisTime[j];
                        }
                    }
                }
            }

            basisOrder[i] = currentIndex;
        }

        /*
         * do NOT adjust the start and end time of the time series with the
         * latest ending time. loop through all the other time series and adjust
         * their start and end times as necessary so that they do not overlap
         * the time limits of the being-built aggregate time series.
         */

        currentIndex = basisOrder[0];
        fullStartValidTime = startValidTime[currentIndex];
        fullEndValidTime = endValidTime[currentIndex];

        for (int i = 1; i < count; i++) {
            currentIndex = basisOrder[i];

            /*
             * each additional time series being considered is checked to see if
             * it falls outside the time window already encompassed by the
             * assembled time series. there are four cases that can occur; each
             * is handled below.
             */

            if ((startValidTime[currentIndex]
                    .compareTo(fullStartValidTime) >= 0)
                    && (endValidTime[currentIndex]
                            .compareTo(fullEndValidTime) <= 0)) {
                /*
                 * if the basis time series being considered is fully within the
                 * time of the already existing time series, then ignore it
                 * completely, and reset its times.
                 */
                startValidTime[currentIndex] = zero;
                endValidTime[currentIndex] = zero;
            } else if ((startValidTime[currentIndex]
                    .compareTo(fullStartValidTime) <= 0)
                    && (endValidTime[currentIndex]
                            .compareTo(fullEndValidTime) >= 0)) {
                /*
                 * if the basis time series being considered covers time both
                 * before and after the existing time series, use the portion of
                 * it that is before the time series. it is not desirable to use
                 * both the before and after portion (this results in a
                 * non-contiguous time-series that is weird), and given a choice
                 * it is better to use the forecast data early on than the later
                 * forecast data, so use the before portion
                 */
                endValidTime[currentIndex] = new Date(
                        fullStartValidTime.getTime() - 1000);
                fullStartValidTime = startValidTime[currentIndex];

            } else if ((startValidTime[currentIndex]
                    .compareTo(fullStartValidTime) <= 0)
                    && (endValidTime[currentIndex]
                            .compareTo(fullEndValidTime) <= 0)) {
                /*
                 * if the basis time series being considered straddles the
                 * beginning or is completely before the existing time series,
                 * then use the portion of it that is before the time series.
                 */
                endValidTime[currentIndex] = new Date(
                        fullStartValidTime.getTime() - 1000);
                fullStartValidTime = startValidTime[currentIndex];
            } else if ((startValidTime[currentIndex]
                    .compareTo(fullStartValidTime) >= 0)
                    && (endValidTime[currentIndex]
                            .compareTo(fullEndValidTime) >= 0)) {
                /*
                 * if the basis time series being considered straddles the end
                 * or is completely after the existing time series, then use the
                 * portion of it that is after the time series.
                 */
                startValidTime[currentIndex] = new Date(
                        fullEndValidTime.getTime() + 1000);
                fullEndValidTime = endValidTime[currentIndex];

            }
        }

        // Need to find a better way to do this
        rval[0] = startValidTime;
        rval[1] = endValidTime;
        return rval;
    }

    /**
     * Convert duration int to String character.
     *
     * @param dur
     *            The duration value
     */
    public void convertDur(short dur, ShefData data) {
        String value = null;
        String durationCode = null;
        Integer durInt = new Integer(dur);
        value = DURATION_MAP.get(durInt);
        if (value == null) {
            // Anything not in the DURATION_MAP is
            // probably a variable duration.
            value = "V";
            if (dur >= 7000) {
                durationCode = "S";
            } else if (dur < 1000) {
                durationCode = "N";
            } else if (dur < 2000) {
                durationCode = "H";
            } else if (dur < 3000) {
                durationCode = "D";
            } else if (dur < 4000) {
                durationCode = "M";
            } else if (dur < 5000) {
                durationCode = "Y";
            } else {
                // Not sure what value this would be.
                value = "Z";
            }
        }

        data.setDuration(Duration.getEnum(value));
        data.setDurationCodeVariable(durationCode);
        data.setDurationValue(dur);
    }

    private static Map<Integer, String> buildDurationMap() {
        Map<Integer, String> map = new HashMap<>();
        map.put(0, "I");
        map.put(1, "U");
        map.put(5, "E");
        map.put(10, "G");
        map.put(15, "C");
        map.put(30, "J");
        map.put(1001, "H");
        map.put(1002, "B");
        map.put(1003, "T");
        map.put(1004, "F");
        map.put(1006, "Q");
        map.put(1008, "A");
        map.put(1012, "K");
        map.put(1018, "L");
        map.put(2001, "D");
        map.put(2007, "W");
        map.put(3001, "M");
        map.put(4001, "Y");
        map.put(5004, "P");
        map.put(5000, "Z");
        map.put(5001, "S");
        map.put(5002, "R");
        map.put(5005, "X");
        return map;
    }

    /**
     * Check if the data value's time is within the given day-of-the-year
     * window.
     *
     * @param obsTime
     *            - Data time
     * @param monthDayStart
     *            - Valid range start day
     * @param monthDayEnd
     *            - Valid range end day
     * @return - true if the data time is within the range
     */
    public boolean checkRangeDate(Date obsTime, String monthDayStart,
            String monthDayEnd) {
        boolean valid = false;
        if ((obsTime != null) && (monthDayStart != null)
                && (monthDayEnd != null)) {
            if ((monthDayStart.length() == 5) && (monthDayEnd.length() == 5)) {

                int rangeStartDate = Integer
                        .parseInt(monthDayStart.substring(0, 2)) * 100;
                rangeStartDate += Integer.parseInt(monthDayStart.substring(3));

                int rangeEndDate = Integer.parseInt(monthDayEnd.substring(0, 2))
                        * 100;
                rangeEndDate += Integer.parseInt(monthDayEnd.substring(3));

                Calendar date = TimeUtil.newGmtCalendar(obsTime);

                int dataDate = (date.get(Calendar.MONTH) + 1) * 100;
                dataDate += date.get(Calendar.DAY_OF_MONTH);

                /* Compare the dates, don't check for straddling the year */
                valid = ((dataDate >= rangeStartDate)
                        && (dataDate <= rangeEndDate));
            }
        }
        return valid;
    }

    /**
     * Determine if the qualityCode passed in is of "Higher" quality than the
     * checkCode passed in
     *
     * @param checkCode
     *            - code to check against
     * @param qualityCode
     *            - code to check
     * @return true if the qualityCode is of "Higher" quality
     */
    public boolean checkQcCode(QualityControlCode checkCode, long qualityCode) {
        boolean returnValue = false;
        switch (checkCode) {
        case QC_DEFAULT:
            returnValue = (qualityCode == ShefConstants.DEFAULT_QC_VALUE);
            break;
        case QC_PASSED:
            returnValue = (qualityCode > ShefConstants.GOOD_QUESTIONABLE_THRESHOLD);
            break;
        case QC_QUESTIONABLE:
            returnValue = ((qualityCode >= ShefConstants.QUESTIONABLE_BAD_THRESHOLD)
                    && (qualityCode < ShefConstants.GOOD_QUESTIONABLE_THRESHOLD));
            break;
        case QC_ROC_PASSED:
            returnValue = BitUtils.checkQcBit(ShefConstants.ROC_QC,
                    qualityCode);
            break;
        case QC_OUTLIER_PASSED:
            returnValue = BitUtils.checkQcBit(ShefConstants.OUTLIER_QC,
                    qualityCode);
            break;
        case QC_SCC_PASSED:
            returnValue = BitUtils.checkQcBit(ShefConstants.SCC_QC,
                    qualityCode);
            break;
        case QC_MSC_PASSED:
            returnValue = BitUtils.checkQcBit(ShefConstants.MSC_QC,
                    qualityCode);
            break;
        case QC_FAILED:
            returnValue = qualityCode < ShefConstants.QUESTIONABLE_BAD_THRESHOLD;
            break;
        case QC_NOT_FAILED:
            returnValue = (qualityCode >= ShefConstants.QUESTIONABLE_BAD_THRESHOLD);
            break;
        case QC_NOT_PASSED:
            returnValue = (qualityCode <= ShefConstants.GOOD_QUESTIONABLE_THRESHOLD);
            break;
        default:
            log.error("Invalid request made in checkQcCode() method.");
            returnValue = false;
            break;
        }
        return returnValue;
    }
}
