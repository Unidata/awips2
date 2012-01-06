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

import java.util.Date;

import com.raytheon.viz.hydrocommon.whfslib.PrecipUtil.DataErr;

/**
 * Total Precipitation Data Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2009 2257       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TotalPrecip {
    private String lid;

    private String PE;

    private String TS;

    private float value;

    private boolean summed_flag;

    private float hours_covered;

    private float percent_filled;

    private char value_indicator;

    private char qc;

    private DataErr err;

    private boolean reported_missing;

    private Date match_time;

    public TotalPrecip() {
        lid = "";
        PE = "";
        TS = "";
        value = PointPrecipConstants.MISSING_PRECIP;
        summed_flag = false;
        hours_covered = 0;
        percent_filled = 0;
        value_indicator = PointPrecipConstants.MISSING_CHAR;
        qc = 'Z';
        err = new DataErr();
        reported_missing = false;

    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the pE
     */
    public String getPE() {
        return PE;
    }

    /**
     * @param pe the pE to set
     */
    public void setPE(String pe) {
        PE = pe;
    }

    /**
     * @return the tS
     */
    public String getTS() {
        return TS;
    }

    /**
     * @param ts the tS to set
     */
    public void setTS(String ts) {
        TS = ts;
    }

    /**
     * @return the value
     */
    public float getValue() {
        return value;
    }

    /**
     * @param value the value to set
     */
    public void setValue(float value) {
        this.value = value;
    }

    /**
     * @return the summed_flag
     */
    public boolean isSummed_flag() {
        return summed_flag;
    }

    /**
     * @param summed_flag the summed_flag to set
     */
    public void setSummed_flag(boolean summed_flag) {
        this.summed_flag = summed_flag;
    }

    /**
     * @return the hours_covered
     */
    public float getHours_covered() {
        return hours_covered;
    }

    /**
     * @param hours_covered the hours_covered to set
     */
    public void setHours_covered(float hours_covered) {
        this.hours_covered = hours_covered;
    }

    /**
     * @return the percent_filled
     */
    public float getPercent_filled() {
        return percent_filled;
    }

    /**
     * @param percent_filled the percent_filled to set
     */
    public void setPercent_filled(float percent_filled) {
        this.percent_filled = percent_filled;
    }

    /**
     * @return the value_indicator
     */
    public char getValue_indicator() {
        return value_indicator;
    }

    /**
     * @param value_indicator the value_indicator to set
     */
    public void setValue_indicator(char value_indicator) {
        this.value_indicator = value_indicator;
    }

    /**
     * @return the qc
     */
    public char getQc() {
        return qc;
    }

    /**
     * @param qc the qc to set
     */
    public void setQc(char qc) {
        this.qc = qc;
    }

    /**
     * @return the err
     */
    public DataErr getErr() {
        return err;
    }

    /**
     * @param err the err to set
     */
    public void setErr(DataErr err) {
        this.err = err;
    }

    /**
     * @return the reported_missing
     */
    public boolean isReported_missing() {
        return reported_missing;
    }

    /**
     * @param reported_missing the reported_missing to set
     */
    public void setReported_missing(boolean reported_missing) {
        this.reported_missing = reported_missing;
    }

    /**
     * @return the match_time
     */
    public Date getMatch_time() {
        return match_time;
    }

    /**
     * @param match_time the match_time to set
     */
    public void setMatch_time(Date match_time) {
        this.match_time = match_time;
    }
}
