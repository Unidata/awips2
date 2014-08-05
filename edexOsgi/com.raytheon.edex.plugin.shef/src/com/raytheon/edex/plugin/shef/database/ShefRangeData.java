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

import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;

/**
 * Data object for holding SHEF range limits
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2014            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ShefRangeData {

    private double grossRangeMin = ShefConstants.SHEF_MISSING_INT;

    private double grossRangeMax = ShefConstants.SHEF_MISSING_INT;

    private double reasonRangeMin = ShefConstants.SHEF_MISSING_INT;

    private double reasonRangeMax = ShefConstants.SHEF_MISSING_INT;

    private double alertUpperLimit = ShefConstants.SHEF_MISSING_INT;

    private double alarmUpperLimit = ShefConstants.SHEF_MISSING_INT;

    private double alertLowerLimit = ShefConstants.SHEF_MISSING_INT;

    private double alarmLowerLimit = ShefConstants.SHEF_MISSING_INT;

    /**
     * @return the grossRangeMin
     */
    public double getGrossRangeMin() {
        return grossRangeMin;
    }

    /**
     * @param grossRangeMin
     *            the grossRangeMin to set
     */
    public void setGrossRangeMin(double grossRangeMin) {
        this.grossRangeMin = grossRangeMin;
    }

    /**
     * @return the grossRangeMax
     */
    public double getGrossRangeMax() {
        return grossRangeMax;
    }

    /**
     * @param grossRangeMax
     *            the grossRangeMax to set
     */
    public void setGrossRangeMax(double grossRangeMax) {
        this.grossRangeMax = grossRangeMax;
    }

    /**
     * @return the reasonRangeMin
     */
    public double getReasonRangeMin() {
        return reasonRangeMin;
    }

    /**
     * @param reasonRangeMin
     *            the reasonRangeMin to set
     */
    public void setReasonRangeMin(double reasonRangeMin) {
        this.reasonRangeMin = reasonRangeMin;
    }

    /**
     * @return the reasonRangeMax
     */
    public double getReasonRangeMax() {
        return reasonRangeMax;
    }

    /**
     * @param reasonRangeMax
     *            the reasonRangeMax to set
     */
    public void setReasonRangeMax(double reasonRangeMax) {
        this.reasonRangeMax = reasonRangeMax;
    }

    /**
     * @return the alertUpperLimit
     */
    public double getAlertUpperLimit() {
        return alertUpperLimit;
    }

    /**
     * @param alertUpperLimit
     *            the alertUpperLimit to set
     */
    public void setAlertUpperLimit(double alertUpperLimit) {
        this.alertUpperLimit = alertUpperLimit;
    }

    /**
     * @return the alarmUpperLimit
     */
    public double getAlarmUpperLimit() {
        return alarmUpperLimit;
    }

    /**
     * @param alarmUpperLimit
     *            the alarmUpperLimit to set
     */
    public void setAlarmUpperLimit(double alarmUpperLimit) {
        this.alarmUpperLimit = alarmUpperLimit;
    }

    /**
     * @return the alertLowerLimit
     */
    public double getAlertLowerLimit() {
        return alertLowerLimit;
    }

    /**
     * @param alertLowerLimit
     *            the alertLowerLimit to set
     */
    public void setAlertLowerLimit(double alertLowerLimit) {
        this.alertLowerLimit = alertLowerLimit;
    }

    /**
     * @return the alarmLowerLimit
     */
    public double getAlarmLowerLimit() {
        return alarmLowerLimit;
    }

    /**
     * @param alarmLowerLimit
     *            the alarmLowerLimit to set
     */
    public void setAlarmLowerLimit(double alarmLowerLimit) {
        this.alarmLowerLimit = alarmLowerLimit;
    }

}
