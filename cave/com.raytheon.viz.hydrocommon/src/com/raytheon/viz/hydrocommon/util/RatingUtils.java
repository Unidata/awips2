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
package com.raytheon.viz.hydrocommon.util;

import java.util.ArrayList;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.Rating;
import com.raytheon.viz.hydrocommon.data.RatingShift;
import com.raytheon.viz.hydrocommon.datamanager.HydroCommonDataManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2008            mpduff     Initial creation
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RatingUtils {
    /**
     * Calculate discharge from stage.
     * 
     * @param lid
     *     The location id
     * @param stage
     *     The stage value
     * @return
     *     The discharge value
     */
    public static double stage2discharge(String lid, double stage) {
        HydroCommonDataManager dataManager = HydroCommonDataManager.getInstance();
        double discharge = 0;

        /*
         * Check to determine if the stage value passed into this routine is
         * missing. If it is, then return a flow value of missing.
         */
        if (stage == HydroConstants.MISSING_VALUE) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        Rating rating = dataManager.getRating(lid);

        if (rating == null) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        RatingShift ratingShift = dataManager.getRatingShift(lid);

        if (ratingShift != null) {
            /* rating shift exists */
            double shiftAmount = ratingShift.getShiftAmount();

            ArrayList<Double> stageList = rating.getStage();
            ArrayList<Double> dischargeList = rating.getDischarge();
            ArrayList<Double> tempList = new ArrayList<Double>();
            for (int i = 0; i < stageList.size(); i++) {
                tempList.add(stageList.get(i) + shiftAmount);
            }
            stageList = tempList;

            tempList = new ArrayList<Double>();
            for (int i = 0; i < dischargeList.size(); i++) {
                tempList.add(dischargeList.get(i) + shiftAmount);
            }
            dischargeList = tempList;

            rating.setDischarge(dischargeList);
            rating.setStage(stageList);
        }

        /*
         * if there is less than 2 points (ie. one) then that means there is NO
         * usable rating curve for that location
         */
        if ((rating.getStage() == null) || (rating.getStage().size() < 2)) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        ArrayList<Double> stageList = rating.getStage();
        ArrayList<Double> dischargeList = rating.getDischarge();

        double minStage = stageList.get(0);
        double maxStage = stageList.get(stageList.size() - 1);
        double minDischarge = dischargeList.get(0);
        double maxDischarge = dischargeList.get(dischargeList.size() - 1);

        /*
         * if the stage value passed in is less then the lowest stage in the
         * rating table then extrapolate the discharge
         */
        if (stage < minStage) {
            double nextStage = stageList.get(1);
            double stageDifference = nextStage - stage;
            double dischargeDifference = dischargeList.get(1) - minDischarge;
            if (stageDifference == 0) {
                discharge = minDischarge;
            } else {
                discharge = minDischarge
                        - ((dischargeDifference / stageDifference) * (minStage - stage));
            }
        }

        /*
         * if the stage value passed in is greater then the highest stage in the
         * rating table then extrapolate the discharge
         */
        if (stage > maxStage) {
            double prevStage = stageList.get(stageList.size() - 2);
            double dischargeDifference = maxDischarge
                    - dischargeList.get(dischargeList.size() - 2);
            double stageDifference = maxStage - prevStage;
            if (stageDifference == 0) {
                discharge = maxDischarge;
            } else {
                discharge = maxDischarge
                        + ((dischargeDifference / stageDifference) * (stage - maxStage));
            }
        }

        /*
         * if the stage value passed in is between the lowest and highest stage
         * in the rating table then interpolate the discharge
         */
        if ((stage >= minStage) && (stage <= (maxStage))) {
            double lowerStage = minStage;
            double lowerDischarge = minDischarge;
            for (int i = 1; i < stageList.size(); i++) {
                double nextStage = stageList.get(i);
                double nextDischarge = dischargeList.get(i);
                if ((stage >= lowerStage) && (stage <= nextStage)) {
                    double dischargeDifference = nextDischarge - lowerDischarge;
                    double stageDifference = nextStage - lowerStage;
                    if (stageDifference == 0) {
                        discharge = lowerDischarge;
                    } else {
                        discharge = lowerDischarge
                                + ((dischargeDifference / stageDifference) * (stage - lowerStage));
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
     * Calculate the stage from discharge.
     * 
     * @param lid
     *     The location Id
     * @param discharge
     *     The discharge
     * @return
     *     The stage
     */
    public static double discharge2stage(String lid, double discharge) {
        HydroCommonDataManager dataManager = HydroCommonDataManager.getInstance();
        double stage = 0;

        /*
         * Check to see if the discharge is a missing value. If it is then
         * return a missing value for the stage.
         */
        if (discharge < 0) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        Rating rating = dataManager.getRating(lid);

        if (rating == null) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        RatingShift ratingShift = dataManager.getRatingShift(lid);

        if (ratingShift != null) {
            /* rating shift exists */
            double shiftAmount = ratingShift.getShiftAmount();

            ArrayList<Double> stageList = rating.getStage();
            ArrayList<Double> dischargeList = rating.getDischarge();
            ArrayList<Double> tempList = new ArrayList<Double>();
            for (int i = 0; i < stageList.size(); i++) {
                tempList.add(stageList.get(i) + shiftAmount);
            }
            stageList = tempList;

            tempList = new ArrayList<Double>();
            for (int i = 0; i < dischargeList.size(); i++) {
                tempList.add(dischargeList.get(i) + shiftAmount);
            }
            dischargeList = tempList;

            rating.setDischarge(dischargeList);
            rating.setStage(stageList);
        }

        /*
         * if there is less than 2 points (ie. one) then that means there is NO
         * usable rating curve for that location
         */
        if (rating.getDischarge().size() < 2) {
            return HydroConstants.RATING_CONVERT_FAILED;
        }

        ArrayList<Double> stageList = rating.getStage();
        ArrayList<Double> dischargeList = rating.getDischarge();
        double minDischarge = dischargeList.get(0);
        double maxDischarge = dischargeList.get(dischargeList.size() - 1);
        double minStage = stageList.get(0);
        double maxStage = stageList.get(stageList.size() - 1);

        /*
         * if the discharge value passed in is less then the lowest discharge in
         * the rating table then extrapolate the stage
         */
        if (discharge < minDischarge) {
            double nextDischarge = dischargeList.get(1);
            double dischargeDifference = nextDischarge - minDischarge;
            double stageDifference = stageList.get(1) - minStage;
            if (dischargeDifference == 0) {
                stage = minStage;
            } else {
                stage = minStage
                        - ((stageDifference / dischargeDifference) * (minDischarge - discharge));
            }
        }

        /*
         * if the discharge value passed in is greater then the highest
         * discharge in the rating table then extrapolate the stage
         */
        if (discharge > maxDischarge) {
            double prevDischarge = dischargeList.get(dischargeList.size() - 2);
            double dischargeDifference = maxDischarge - prevDischarge;
            double stageDifference = maxStage
                    - stageList.get(stageList.size() - 2);

            if (dischargeDifference == 0) {
                stage = maxStage;
            } else {
                stage = maxStage
                        + ((stageDifference / dischargeDifference) * (discharge - maxDischarge));
            }
        }

        /*
         * if the discharge value passed in is between the lowest and highest
         * discharge in the rating table then interpolate the stage
         */
        if ((discharge <= maxDischarge) && (discharge >= minDischarge)) {
            double lowerStage = minStage;
            double lowerDischarge = minDischarge;

            for (int i = 1; i < dischargeList.size(); i++) {
                double nextDischarge = dischargeList.get(i);
                double nextStage = stageList.get(i);

                if ((discharge >= lowerDischarge)
                        && (discharge <= nextDischarge)) {
                    double dischargeDifference = nextDischarge - lowerDischarge;
                    double stageDifference = nextStage - lowerStage;

                    if (dischargeDifference == 0) {
                        stage = lowerStage;
                    } else {
                        stage = lowerStage
                                + ((stageDifference / dischargeDifference) * (discharge - lowerDischarge));
                    }
                    break;
                }
                lowerStage = nextStage;
                lowerDischarge = nextDischarge;
            }
        }

        return stage;
    }
}
