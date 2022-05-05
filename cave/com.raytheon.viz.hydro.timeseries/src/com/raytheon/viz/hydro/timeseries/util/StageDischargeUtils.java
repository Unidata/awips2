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

import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Utility class for Stage/Discharge conversions.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 07, 2008  1194     mpduff    Initial creation
 * May 11,2011   9281     lbousaid  nothing get display in right y-axis when
 *                                  there is no rating curve
 * Jan 03,2013   15652    wkwock    Fix stage to discharge
 * Mar 12, 2013  1781     mpduff    Fix stage2discharge flag preventing multiple
 *                                  db queries.
 * May 30, 2018  6748     randerso  Eliminated redundant query of rating table.
 *                                  Code cleanup.
 * Jul 09, 2019  6748     randerso  Fixed and commented out status message in
 *                                  stage2discharge
 * 
 * </pre>
 *
 * @author mpduff
 */

public class StageDischargeUtils {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StageDischargeUtils.class);

    private static String previousLid = null;

    private static Rating ratingData = null;

    private static String RATING_SHIFT_QUERY = "select lid,date,shift_amount from ratingshift where lid = ':lid' and active='T' order by date desc";

    private static boolean needToFindShiftAmount = false;

    /**
     * Returns the corresponding stage value for the discharge value passed in.
     *
     * @param gd
     *            the Graph Data
     * @param stage
     *            the Discharge Value
     * @return the corresponding stage value
     */
    public static double getStageFromDischarge(GraphData gd, double stage) {
        double ystage = 0.0;
        for (int i = 0; i < gd.getNumTraces(); i++) {
            if (gd.getTraces().get(i).getPe().toUpperCase().startsWith("Q")) {
                ystage = discharge2stage(gd.getTraces().get(0).getLid(), stage);
                break;
            }
        }

        return ystage;
    }

    /**
     * @param lid
     * @return true if rating is valid
     */
    public static synchronized boolean checkRatingTable(String lid) {
        boolean retVal = false;
        try {
            if (ratingData == null
                    || !ratingData.getLid().equalsIgnoreCase(lid)) {
                ratingData = new Rating(lid);
                needToFindShiftAmount = true;
                /*
                 * Check the Rating object for data and return true if data are
                 * available
                 */
                if (ratingData.getStage().size() > 2) {
                    retVal = true;
                }
                /* return false if there is no data */
                else {
                    retVal = false;
                }
            } else {
                if (ratingData.getLid().equalsIgnoreCase(lid)) {
                    ratingData = new Rating(lid);
                    needToFindShiftAmount = true;
                    if (ratingData.getStage().size() > 2) {
                        retVal = true;
                    } else {
                        retVal = false;
                    }
                }
            }

        } catch (VizException e) {
            statusHandler.error("Error loading rating curve for " + lid, e);
        }

        return retVal;
    }

    /**
     * Convert the stage to discharge for the location and stage value passed
     * in.
     *
     * @param lid
     *            The Location ID
     * @param stage
     *            The Stage Value
     * @return the corresponding discharge
     */
    public static synchronized double stage2discharge(String lid,
            double stage) {
        /*
         * Check to determine if the stage value is missing. If it is then
         * return a flow value of missing.
         */
        if (stage == HydroConstants.MISSING_VALUE) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        double discharge = HydroConstants.MISSING_VALUE;

        /*
         * If the lid passed in is NOT the same as the previous lid then copy
         * lid passed in to previous and get the rating curve
         */
        if (!lid.equals(previousLid)) {
            previousLid = lid;

            try {
                if (ratingData == null
                        || !ratingData.getLid().equalsIgnoreCase(lid)) {
                    needToFindShiftAmount = true;
                    ratingData = new Rating(lid);
                }
            } catch (VizException e) {
                statusHandler.error("Error loading rating curve for " + lid, e);
            }
        }

        /*
         * if the pointer to the head is NULL then that means there is NO rating
         * curve for that location
         */
        if (ratingData == null) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        /*
         * If there are less than 2 points (ie. one) then that means there is NO
         * usable rating curve for that location
         */
        if (ratingData.getDischarge().size() < 2) {
            // Rating table has less than 2 points
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        List<Object[]> ratingShiftArray = null;
        double shiftAmount = 0;
        List<Double> stageRatingCurve = ratingData.getStage();

        /*
         * Determine if there is a shift factor for this rating curve. If there
         * is, then shift each point in the rating curve by this factor.
         */
        if (needToFindShiftAmount) {
            ratingShiftArray = queryRatingShift(lid);

            if (ratingShiftArray != null && !ratingShiftArray.isEmpty()) {
                Object[] shiftData = ratingShiftArray.get(0);
                shiftAmount = (Double) shiftData[2];
                double d;
                for (int i = 0; i < stageRatingCurve.size(); i++) {
                    d = stageRatingCurve.get(i);
                    d += shiftAmount;
                    stageRatingCurve.set(i, d);
                }
                ratingData.setStage(stageRatingCurve);
            }
            needToFindShiftAmount = false;
        }

        List<Double> dischargeList = ratingData.getDischarge();
        double minStage = stageRatingCurve.get(0);
        double maxStage = stageRatingCurve.get(stageRatingCurve.size() - 1);
        double minDischarge = dischargeList.get(0);
        double maxDischarge = dischargeList.get(dischargeList.size() - 1);

        /*
         * if the stage value passed in is less then the lowest stage in the
         * rating table then extrapolate the discharge
         */
        if (stage < minStage) {
            double nextStage = stageRatingCurve.get(1);
            double stageDifference = nextStage - stage;
            double dischargeDifference = dischargeList.get(1) - minDischarge;
            if (stageDifference == 0) {
                discharge = minDischarge;
            } else {
                discharge = minDischarge - dischargeDifference / stageDifference
                        * (minStage - stage);
            }
        }

        /*
         * if the stage value passed in is greater then the highest stage in the
         * rating table then extrapolate the discharge
         */
        if (stage > maxStage) {
            double prevStage = stageRatingCurve
                    .get(stageRatingCurve.size() - 2);
            double dischargeDifference = maxDischarge
                    - dischargeList.get(dischargeList.size() - 2);
            double stageDifference = maxStage - prevStage;
            if (stageDifference == 0) {
                discharge = maxDischarge;
            } else {
                discharge = maxDischarge + dischargeDifference / stageDifference
                        * (stage - maxStage);
            }
        }

        /*
         * if the stage value passed in is between the lowest and highest stage
         * in the rating table then interpolate the discharge
         */
        if (stage >= minStage && stage <= maxStage) {
            double lowerStage = minStage;
            double lowerDischarge = minDischarge;
            for (int i = 1; i < stageRatingCurve.size(); i++) {
                double nextStage = stageRatingCurve.get(i);
                double nextDischarge = dischargeList.get(i);
                if (stage >= lowerStage && stage <= nextStage) {
                    double dischargeDifference = nextDischarge - lowerDischarge;
                    double stageDifference = nextStage - lowerStage;
                    if (stageDifference == 0) {
                        discharge = lowerDischarge;
                    } else {
                        discharge = lowerDischarge + dischargeDifference
                                / stageDifference * (stage - lowerStage);
                    }
                    break;
                }
                lowerStage = nextStage;
                lowerDischarge = nextDischarge;
            }
        }

        /* If for some reason the discharge is < 0 then return missing */
        if (discharge < 0) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        return discharge;
    }

    /**
     * Convert the discharge to stage for the location and discharge value
     * passed in.
     *
     * @param lid
     *            The Location ID
     * @param discharge
     *            The Discharge Value
     * @return the corresponding discharge
     */
    public static synchronized double discharge2stage(String lid,
            double discharge) {
        /*
         * Check to see if the discharge value is bad, i.e. missing. If it is
         * bad, then return a stage value of missing.
         */
        if (discharge < 0) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }
        double stage = HydroConstants.MISSING_VALUE;
        boolean needToFindShiftAmount = false;

        /*
         * If the lid passed in is NOT the same as the previous lid then copy
         * lid passed in to previous and get the rating curve
         */
        if (!lid.equals(previousLid)) {
            previousLid = lid;
            needToFindShiftAmount = true;

            try {
                if (ratingData == null
                        || !ratingData.getLid().equalsIgnoreCase(lid)) {
                    ratingData = new Rating(lid);
                    needToFindShiftAmount = true;
                }
            } catch (VizException e) {
                statusHandler.error("Error loading rating curve for " + lid, e);
            }
        }

        /*
         * if the pointer to the head is NULL then that means there is NO rating
         * curve for that location
         */
        if (ratingData == null) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        /*
         * If there are less than 2 points (ie. one) then that means there is NO
         * usable rating curve for that location
         */
        if (ratingData.getDischarge().size() < 2) {
            statusHandler.error(
                    "Rating table has less than 2 points for LID=%s\n", lid);
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        List<Object[]> ratingShiftArray = null;
        double shiftAmount = 0;
        List<Double> dischargeRatingCurve = ratingData.getDischarge();

        /*
         * Determine if there is a shift factor for this rating curve. If there
         * is, then shift each point in the rating curve by this factor.
         */
        if (needToFindShiftAmount) {
            ratingShiftArray = queryRatingShift(lid);

            if (ratingShiftArray != null && !ratingShiftArray.isEmpty()) {
                Object[] shiftData = ratingShiftArray.get(0);
                shiftAmount = (Double) shiftData[2];
            }
        }

        List<Double> stageList = ratingData.getStage();
        double minDischarge = dischargeRatingCurve.get(0);
        double maxDischarge = dischargeRatingCurve
                .get(dischargeRatingCurve.size() - 1);
        double minStage = stageList.get(0);
        double maxStage = stageList.get(stageList.size() - 1);

        /*
         * if the discharge value passed in is less then the lowest discharge in
         * the rating table then extrapolate the stage
         */
        if (discharge < minDischarge) {
            double nextDischarge = dischargeRatingCurve.get(1);
            double dischargeDifference = nextDischarge - minDischarge;
            double stageDifference = stageList.get(1) - minStage;
            if (dischargeDifference == 0) {
                stage = minStage;
            } else {
                stage = minStage - stageDifference / dischargeDifference
                        * (minDischarge - discharge);
            }
        }

        /*
         * if the discharge value passed in is greater then the highest
         * discharge in the rating table then extrapolate the stage
         */
        if (discharge > maxDischarge) {
            double prevDischarge = dischargeRatingCurve
                    .get(dischargeRatingCurve.size() - 2);
            double dischargeDifference = maxDischarge - prevDischarge;
            double stageDifference = maxStage
                    - stageList.get(stageList.size() - 2);

            if (dischargeDifference == 0) {
                stage = maxStage;
            } else {
                stage = maxStage + stageDifference / dischargeDifference
                        * (discharge - maxDischarge);
            }
        }

        /*
         * if the discharge value passed in is between the lowest and highest
         * discharge in the rating table then interpolate the stage
         */
        if (discharge <= maxDischarge && discharge >= minDischarge) {
            double lowerStage = minStage;
            double lowerDischarge = minDischarge;

            for (int i = 1; i < dischargeRatingCurve.size(); i++) {
                double nextDischarge = dischargeRatingCurve.get(i);
                double nextStage = stageList.get(i);

                if (discharge >= lowerDischarge && discharge <= nextDischarge) {
                    double dischargeDifference = nextDischarge - lowerDischarge;
                    double stageDifference = nextStage - lowerStage;

                    if (dischargeDifference == 0) {
                        stage = lowerStage;
                    } else {
                        stage = lowerStage
                                + stageDifference / dischargeDifference
                                        * (discharge - lowerDischarge);
                    }
                    break;
                }
                lowerStage = nextStage;
                lowerDischarge = nextDischarge;
            }
        }

        stage += shiftAmount;
        return stage;
    }

    /**
     * Get the data from the IHFS RatingShift table for the specified Location
     * Id.
     *
     * @param lid
     *            The Location ID
     * @return The data from the ratingShift table in IHFS, null if no data
     *         available
     * @throws VizException
     */
    private static List<Object[]> queryRatingShift(String lid) {
        /* Query the ratingShift table */
        List<Object[]> results = null;
        try {
            results = DirectDbQuery.executeQuery(
                    RATING_SHIFT_QUERY.replace(":lid", lid),
                    HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.error("Error loading rating shift data for " + lid,
                    e);
        }
        return results;
    }
}
