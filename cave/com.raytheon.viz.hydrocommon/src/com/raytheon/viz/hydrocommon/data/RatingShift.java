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
package com.raytheon.viz.hydrocommon.data;

import java.util.Date;

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Rating Shift data object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 23, 2008            mpduff     Initial creation
 * Sep 09, 2009 2259       mpduff     Refactored to HydroCommon
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RatingShift {
    private String lid = null;

    private Date date = null;

    private double shiftAmount = HydroConstants.MISSING_VALUE;

    private String active = null;

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the date
     */
    public Date getDate() {
        return date;
    }

    /**
     * @param date
     *            the date to set
     */
    public void setDate(Date date) {
        this.date = date;
    }

    /**
     * @return the shiftAmount
     */
    public double getShiftAmount() {
        return shiftAmount;
    }

    /**
     * @param shiftAmount
     *            the shiftAmount to set
     */
    public void setShiftAmount(double shiftAmount) {
        this.shiftAmount = shiftAmount;
    }

    /**
     * @return the active
     */
    public String getActive() {
        return active;
    }

    /**
     * @param active
     *            the active to set
     */
    public void setActive(String active) {
        this.active = active;
    }

}
