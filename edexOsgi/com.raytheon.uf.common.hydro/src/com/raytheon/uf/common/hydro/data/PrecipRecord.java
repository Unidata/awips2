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
package com.raytheon.uf.common.hydro.data;

import java.util.Date;

import com.raytheon.uf.common.hydro.CommonHydroConstants;

/**
 * Precipitation Record
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2010            mpduff     Initial creation
 * May 26, 2016 5571       skorolev   Relocated to a common plugin 
 *                                    for use in both EDEX/CAVE. Cleanup.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class PrecipRecord {
    /** Record date */
    private Date date = null;

    /** Record value */
    private double value = CommonHydroConstants.MISSING_VALUE;

    /** Type source */
    private String ts = null;

    /** Physical element */
    private String pe = null;

    /** Shef quality code */
    private String shefQualCode = null;

    /** location id */
    private String lid = null;

    /** data duration */
    private int duration;

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
     * @return the value
     */
    public double getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(double value) {
        this.value = value;
    }

    /**
     * @return the ts
     */
    public String getTs() {
        return ts;
    }

    /**
     * @param ts
     *            the ts to set
     */
    public void setTs(String ts) {
        this.ts = ts;
    }

    /**
     * @return the pe
     */
    public String getPe() {
        return pe;
    }

    /**
     * @param pe
     *            the pe to set
     */
    public void setPe(String pe) {
        this.pe = pe;
    }

    /**
     * @return the shefQualCode
     */
    public String getShefQualCode() {
        return shefQualCode;
    }

    /**
     * @param shefQualCode
     *            the shefQualCode to set
     */
    public void setShefQualCode(String shefQualCode) {
        this.shefQualCode = shefQualCode;
    }

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
     * @return the duration
     */
    public int getDuration() {
        return duration;
    }

    /**
     * @param duration
     *            the duration to set
     */
    public void setDuration(int duration) {
        this.duration = duration;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(lid);
        sb.append(" ").append(pe);
        sb.append(" ").append(ts);
        sb.append(" ").append(duration);
        sb.append(" ").append(shefQualCode);
        sb.append(" ").append(value);
        return sb.toString();
    }
}
